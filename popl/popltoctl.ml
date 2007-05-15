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

let wrap n ctl = (ctl,n)

let ctl_true       = wrap 0 CTL.True

let ctl_false      = wrap 0 CTL.False

let ctl_and x y    = wrap 0 (CTL.And(CTL.STRICT,x,y))

let ctl_or x y     = wrap 0 (CTL.Or(x,y))

let ctl_seqor x y  = wrap 0 (CTL.SeqOr(x,y))

let ctl_not x      = wrap 0 (CTL.Not(x))

let ctl_ax x       = wrap 0 (CTL.AX(CTL.FORWARD,CTL.STRICT,x))

let after          = wrap 0 (CTL.Pred(Lib_engine.After, CTL.Control))

let ctl_au x y     = wrap 0 (CTL.AU(CTL.FORWARD,CTL.STRICT,x,ctl_or y after))

let ctl_exists v x = wrap 0 (CTL.Exists(v,x,true))

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let mcode r (_,_,kind) =
    match kind with
      Ast.MINUS(_,_) -> true
    | Ast.PLUS -> failwith "not possible"
    | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING) in
  let do_nothing r k e = k e in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,rp) ->
	bind (mcode r ((),(),bef)) res
    | Ast.Decl(bef,_,decl) -> bind (mcode r ((),(),bef)) res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

let predmaker guard term =
  if not guard && contains_modif term
  then
    let v = ("","_v") in
    ctl_exists v (wrap 0 (CTL.Pred (Lib_engine.Match(term),CTL.Modif v)))
  else wrap 0 (CTL.Pred (Lib_engine.Match(term),CTL.Control))

(* --------------------------------------------------------------------- *)

let rec ctl_seq guard a = function
    Past.Seq(elem,seq) ->
      ctl_element guard (ctl_seq guard a seq) elem
  | Past.Empty -> ctl_true
  | Past.SExists(var,seq) -> ctl_exists var (ctl_seq guard a seq)

and ctl_element guard a = function
    Past.Term(term) -> ctl_and (predmaker guard term) (ctl_ax a)
  | Past.Or(seq1,seq2) ->
      ctl_seqor (ctl_seq guard a seq1) (ctl_seq guard a seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      let shortest =
	List.fold_left ctl_or ctl_false
	  (List.map (ctl_element true ctl_true) (seq_bef@seq_aft)) in
      ctl_au (ctl_and (ctl_dots guard dots) (ctl_not shortest)) a
  | Past.EExists(var,elem) -> ctl_exists var (ctl_element guard a elem)

and ctl_dots guard = function
    Past.Dots -> ctl_true
  | Past.Nest(_) when guard -> ctl_true
  | Past.Nest(seq) ->
      ctl_or (ctl_seq false ctl_true seq) (ctl_not (ctl_seq true ctl_true seq))
  | Past.When(dots,seq) ->
      ctl_and (ctl_dots guard dots) (ctl_not (ctl_seq true ctl_true seq))
  | Past.DExists(var,dots) -> ctl_exists var (ctl_dots guard dots)

(* --------------------------------------------------------------------- *)

let toctl sl = ctl_seq false ctl_true sl
