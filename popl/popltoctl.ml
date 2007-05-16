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

let is_true c =
  match CTL.unwrap c with CTL.True -> true | _ -> false

let is_false c =
  match CTL.unwrap c with CTL.False -> true | _ -> false

let ctl_true       = wrap 0 CTL.True

let ctl_false      = wrap 0 CTL.False

let ctl_and x y    =
  if is_true x then y
  else if is_true y then x else wrap 0 (CTL.And(CTL.STRICT,x,y))

let ctl_or x y     =
  if is_false x then y
  else if is_false y then x else wrap 0 (CTL.Or(x,y))

let ctl_seqor x y  = wrap 0 (CTL.SeqOr(x,y))

let ctl_not x      = wrap 0 (CTL.Not(x))

let ctl_ax x       =
  if is_true x then wrap 0 CTL.True
  else wrap 0 (CTL.AX(CTL.FORWARD,CTL.STRICT,x))

let after          = wrap 0 (CTL.Pred(Lib_engine.After, CTL.Control))
let exit           = wrap 0 (CTL.Pred(Lib_engine.Exit, CTL.Control))

let ctl_au x y     = wrap 0 (CTL.AU(CTL.FORWARD,CTL.STRICT,x,ctl_or y after))

let ctl_exists v x = wrap 0 (CTL.Exists(v,x,true))

let ctl_uncheck x  = wrap 0 (CTL.Uncheck(x))

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

let rec ctl_seq unchecked a = function
    Past.Seq(elem,seq) ->
      ctl_element unchecked (ctl_seq unchecked a seq) elem
  | Past.Empty -> a
  | Past.SExists(var,seq) -> ctl_exists var (ctl_seq unchecked a seq)

and ctl_element unchecked a = function
    Past.Term(term) -> ctl_and (predmaker unchecked term) (ctl_ax a)
  | Past.Or(seq1,seq2) ->
      ctl_seqor (ctl_seq unchecked a seq1) (ctl_seq unchecked a seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      let shortest =
	List.fold_left ctl_or ctl_false
	  (List.map (ctl_element true ctl_true)
	     (Common.union_set seq_bef seq_aft)) in
      ctl_au (ctl_and (guard_ctl_dots unchecked dots) (ctl_not shortest)) a
  | Past.EExists(var,elem) -> ctl_exists var (ctl_element unchecked a elem)

(* --------------------------------------------------------------------- *)

and guard_ctl_seq unchecked = function
    Past.Seq(elem,Past.Empty) -> guard_ctl_element unchecked elem
  | Past.Seq(elem,seq) ->
      ctl_element unchecked (guard_ctl_seq unchecked seq) elem
  | Past.Empty -> ctl_true
  | Past.SExists(var,seq) -> ctl_exists var (guard_ctl_seq unchecked seq)

and guard_ctl_element unchecked = function
    Past.Term(term) -> predmaker unchecked term
  | Past.Or(seq1,seq2) ->
      ctl_seqor (guard_ctl_seq unchecked seq1) (guard_ctl_seq unchecked seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      let shortest =
	List.fold_left ctl_or ctl_false
	  (List.map (ctl_element true ctl_true)
	     (Common.union_set seq_bef seq_aft)) in
      let aft = ctl_or shortest exit in
      ctl_au (ctl_and (guard_ctl_dots unchecked dots) (ctl_not shortest)) aft
  | Past.EExists(var,elem) -> ctl_exists var (guard_ctl_element unchecked elem)

and guard_ctl_dots unchecked = function
    Past.Dots -> ctl_true
  | Past.Nest(_) when unchecked -> ctl_true
  | Past.Nest(seq) ->
      ctl_or (guard_ctl_seq false seq) (ctl_not (guard_ctl_seq true seq))
  | Past.When(dots,seq) ->
      ctl_and
	(guard_ctl_dots unchecked dots)
	(ctl_not (ctl_seq true ctl_true seq))
  | Past.DExists(var,dots) -> ctl_exists var (guard_ctl_dots unchecked dots)

(* --------------------------------------------------------------------- *)

let toctl sl = ctl_seq false ctl_true sl
