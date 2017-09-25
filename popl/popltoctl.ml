(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Past = Ast_popl
module Ast = Ast_cocci
module V = Visitor_ast
module CTL  = Ast_ctl

(* --------------------------------------------------------------------- *)
(* result type *)

type cocci_predicate = Lib_engine.predicate * Ast.meta_name Ast_ctl.modif
type formula =
    (cocci_predicate,Ast_cocci.meta_name, Wrapper_ctl.info) Ast_ctl.generic_ctl

(* --------------------------------------------------------------------- *)

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let mcode r (_,_,kind,_) =
    match kind with
      Ast.MINUS(_,_) -> true
    | Ast.PLUS -> failwith "not possible"
    | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING) in
  let do_nothing r k e = k e in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,rp) ->
      bind (mcode r ((),(),bef,[])) res
    | Ast.Decl(bef,_,decl) -> bind (mcode r ((),(),bef,[])) res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing
      do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

let ctl_exists v x keep_wit = CTL.Exists(v,x,keep_wit)

let predmaker guard term =
  let pos = ("","_p") in
  ctl_exists true pos
    (if guard && contains_modif term
    then
      let v = ("","_v") in
      ctl_exists true v
	(CTL.Pred (Lib_engine.Match(term),CTL.Modif v))
    else CTL.Pred (Lib_engine.Match(term),CTL.Control))

(* --------------------------------------------------------------------- *)

let is_true = function CTL.True -> true | _ -> false

let is_false = function CTL.False -> true | _ -> false

let ctl_true       = CTL.True

let ctl_false      = CTL.False

let ctl_and x y    =
  if is_true x then y
  else if is_true y then x else CTL.And(CTL.STRICT,x,y)

let ctl_or x y     =
  if is_false x then y
  else if is_false y then x else CTL.Or(x,y)

let ctl_seqor x y  = CTL.SeqOr(x,y)

let ctl_not x      = CTL.Not(x)

let ctl_ax x       =
  if is_true x then CTL.True
  else CTL.AX(CTL.FORWARD,CTL.STRICT,x)

let after          = CTL.Pred(Lib_engine.After, CTL.Control)
let exit           = CTL.Pred(Lib_engine.Exit, CTL.Control)
let truepred       = CTL.Pred(Lib_engine.TrueBranch, CTL.Control)
let retpred        = CTL.Pred(Lib_engine.Return, CTL.Control)

let string2var x = ("",x)

let labelctr = ref 0
let get_label_ctr _ =
  let cur = !labelctr in
  labelctr := cur + 1;
  string2var (Printf.sprintf "l%d" cur)

let ctl_au x seq_after y =
  let lv = get_label_ctr() in
  let labelpred = CTL.Pred(Lib_engine.Label lv,CTL.Control) in
  let preflabelpred = CTL.Pred(Lib_engine.PrefixLabel lv,CTL.Control) in
  let matchgoto = CTL.Pred(Lib_engine.Goto,CTL.Control) in
  let matchbreak =
    predmaker false
      (Ast.make_term
	 (Ast.Break(Ast.make_mcode "break",Ast.make_mcode ";"))) in
  let matchcontinue =
    predmaker false
      (Ast.make_term
	 (Ast.Continue(Ast.make_mcode "continue",Ast.make_mcode ";"))) in
  let stop_early =
    ctl_or after
      (ctl_and (ctl_and truepred labelpred)
	 (CTL.AU
	    (CTL.FORWARD,CTL.STRICT,preflabelpred,
	     ctl_and preflabelpred
	       (ctl_or retpred
		  (ctl_and (ctl_or (ctl_or matchgoto matchbreak) matchcontinue)
			(CTL.AG
			   (CTL.FORWARD,CTL.STRICT,
			    ctl_not seq_after))))))) in
  CTL.AU(CTL.FORWARD,CTL.STRICT,x,ctl_or y stop_early)

let ctl_uncheck x  = CTL.Uncheck(x)

(* --------------------------------------------------------------------- *)

let rec ctl_seq keep_wit a = function
    Past.Seq(elem,seq) ->
      ctl_element keep_wit (ctl_seq keep_wit a seq) elem
  | Past.Empty -> a
  | Past.SExists(var,seq) -> ctl_exists keep_wit var (ctl_seq keep_wit a seq)

and ctl_element keep_wit a = function
    Past.Term(term) -> ctl_and (predmaker keep_wit term) (ctl_ax a)
  | Past.Or(seq1,seq2) ->
      ctl_seqor (ctl_seq keep_wit a seq1) (ctl_seq keep_wit a seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      let shortest l =
	List.fold_left ctl_or ctl_false
	  (List.map (ctl_element false ctl_true) l) in
      let s = shortest (Common.union_set seq_bef seq_aft) in
      ctl_au (ctl_and (guard_ctl_dots keep_wit dots) (ctl_not s))
	(shortest seq_aft) a
  | Past.EExists(var,elem) ->
      ctl_exists keep_wit var (ctl_element keep_wit a elem)

(* --------------------------------------------------------------------- *)

and guard_ctl_seq keep_wit = function
    Past.Seq(elem,Past.Empty) -> guard_ctl_element keep_wit elem
  | Past.Seq(elem,seq) ->
      ctl_element keep_wit (guard_ctl_seq keep_wit seq) elem
  | Past.Empty -> ctl_true
  | Past.SExists(var,seq) ->
      ctl_exists keep_wit var (guard_ctl_seq keep_wit seq)

and guard_ctl_element keep_wit = function
    Past.Term(term) -> predmaker keep_wit term
  | Past.Or(seq1,seq2) ->
      ctl_seqor (guard_ctl_seq keep_wit seq1) (guard_ctl_seq keep_wit seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      let shortest l =
	List.fold_left ctl_or ctl_false
	  (List.map (ctl_element false ctl_true) l) in
      let s = shortest (Common.union_set seq_bef seq_aft) in
      let aft = ctl_or s exit in
      ctl_au (ctl_and (guard_ctl_dots keep_wit dots) (ctl_not s))
	(shortest seq_aft) aft
  | Past.EExists(var,elem) ->
      ctl_exists keep_wit var (guard_ctl_element keep_wit elem)

and guard_ctl_dots keep_wit = function
    Past.Dots -> ctl_true
  | Past.Nest(_) when not keep_wit -> ctl_true
  | Past.Nest(seq) ->
      ctl_or (guard_ctl_seq true seq) (ctl_not (guard_ctl_seq false seq))
  | Past.When(dots,seq) ->
      ctl_and
	(guard_ctl_dots keep_wit dots)
	(ctl_not (ctl_seq false ctl_true seq))
  | Past.DExists(var,dots) ->
      ctl_exists keep_wit var (guard_ctl_dots keep_wit dots)

(* --------------------------------------------------------------------- *)

let toctl sl = Asttoctl2.CODE (ctl_seq true ctl_true sl)
