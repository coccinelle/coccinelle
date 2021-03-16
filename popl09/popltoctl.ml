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
  let bind x y = x || y in
  let option_default = false in
  let mcode r (_,_,kind,_) =
    match kind with
      Ast.MINUS(_,_,_,_) -> true
    | Ast.PLUS _ -> failwith "not possible"
    | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING) in
  let do_nothing r k e = k e in
  let annotated_decl decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,_,_) -> bef in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,va,rp) ->
      bind (mcode r ((),(),bef,[])) res
    | Ast.Decl decl -> bind (mcode r ((),(),annotated_decl decl,[])) res
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	bind (mcode r ((),(),annotated_decl decl,[])) res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing
      do_nothing do_nothing rule_elem do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

let ctl_exists keep_wit v x =
  CTL.Exists(!Flag_popl.keep_all_wits || keep_wit,v,x)

let predmaker keep_wit term =
  if (!Flag_popl.keep_all_wits || keep_wit) &&
     (!Flag_popl.mark_all || contains_modif term)
  then
    let v = ("","_v") in
    ctl_exists true v
      (CTL.Pred (Lib_engine.Match(term),CTL.Modif v))
  else CTL.Pred (Lib_engine.Match(term),CTL.Control)

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

let ctl_ex x       =
  if is_true x then CTL.True
  else CTL.EX(CTL.FORWARD,x)

let ctl_back_ex x  =
  if is_true x then CTL.True
  else CTL.EX(CTL.BACKWARD,x)

let after          = CTL.Pred(Lib_engine.After, CTL.Control)
let fall           = CTL.Pred(Lib_engine.FallThrough, CTL.Control)
let exit           = CTL.Pred(Lib_engine.Exit, CTL.Control)
let truepred       = CTL.Pred(Lib_engine.TrueBranch, CTL.Control)
let falsepred      = CTL.Pred(Lib_engine.FalseBranch, CTL.Control)
let retpred        = CTL.Pred(Lib_engine.Return, CTL.Control)

let string2var x = ("",x)

let labelctr = ref 0
let get_label_ctr _ =
  let cur = !labelctr in
  labelctr := cur + 1;
  string2var (Printf.sprintf "l%d" cur)

let ctl_au x y = CTL.AU(CTL.FORWARD,CTL.STRICT,x,y)

let ctl_uncheck x  = CTL.Uncheck(x)

let make_meta_rule_elem d cstr =
  let nm = "_S" in
  Ast.make_meta_rule_elem nm d cstr ([],[],[])

(* --------------------------------------------------------------------- *)

let rec ctl_seq keep_wit a = function
    Past.Seq(elem,seq) ->
      ctl_element keep_wit (ctl_seq keep_wit a seq) elem
  | Past.Empty -> a
  | Past.SExists(var,seq) -> ctl_exists keep_wit var (ctl_seq keep_wit a seq)

and ctl_term keep_wit a = function
    Past.Atomic(term) -> ctl_and (predmaker keep_wit term) (ctl_ax a)
  | Past.IfThen(test,thn,(_,_,_,aft)) -> ifthen keep_wit (Some a) test thn aft
  | Past.TExists(var,term) ->
      ctl_exists keep_wit var (ctl_term keep_wit a term)

and ctl_element keep_wit a = function
    Past.Term(term,ba) ->
      do_between_dots keep_wit ba (ctl_term keep_wit a term) a
  | Past.Or(seq1,seq2) ->
      ctl_seqor (ctl_seq keep_wit a seq1) (ctl_seq keep_wit a seq2)
  | Past.DInfo(dots) -> ctl_au (guard_ctl_dots keep_wit a dots) a
  | Past.EExists(var,elem) ->
      ctl_exists keep_wit var (ctl_element keep_wit a elem)

(* --------------------------------------------------------------------- *)

and guard_ctl_seq keep_wit a = function
    Past.Seq(elem,Past.Empty) -> guard_ctl_element keep_wit a elem
  | Past.Seq(elem,seq) ->
      ctl_element keep_wit (guard_ctl_seq keep_wit a seq) elem
  | Past.Empty -> ctl_true
  | Past.SExists(var,seq) ->
      ctl_exists keep_wit var (guard_ctl_seq keep_wit a seq)

and guard_ctl_term keep_wit = function
    Past.Atomic(term) -> predmaker keep_wit term
  | Past.IfThen(test,thn,(_,_,_,aft)) -> ifthen keep_wit None test thn aft
  | Past.TExists(var,term) ->
      ctl_exists keep_wit var (guard_ctl_term keep_wit term)

and guard_ctl_element keep_wit a = function
    Past.Term(term,_) -> guard_ctl_term keep_wit term
  | Past.Or(seq1,seq2) ->
      ctl_seqor
	(guard_ctl_seq keep_wit a seq1) (guard_ctl_seq keep_wit a seq2)
  | Past.DInfo(dots) -> ctl_au (guard_ctl_dots keep_wit a dots) a
  | Past.EExists(var,elem) ->
      ctl_exists keep_wit var (guard_ctl_element keep_wit a elem)

and guard_ctl_dots keep_wit a = function
    Past.Dots -> ctl_true
(* | Past.Nest(_) when not keep_wit -> ctl_true
   a possible optimization, but irrelevant to popl example *)
  | Past.Nest(seq) ->
      ctl_or
	(guard_ctl_seq true a seq)
	(ctl_not (guard_ctl_seq false a seq))
  | Past.When(dots,seq) ->
      ctl_and
	(guard_ctl_dots keep_wit a dots)
	(ctl_not (guard_ctl_seq false a seq))

(* --------------------------------------------------------------------- *)

and ifthen keep_wit a test thn aft =
(* "if (test) thn; after" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v (After & AXAX after))
             & EX After
    (* doesn't work for C code if (x) return 1; else return 2; *)
*)
  let end_code =
    match (aft,a) with
      (Ast.CONTEXT(_,Ast.NOTHING),None) -> ctl_true
    | (Ast.CONTEXT(_,Ast.NOTHING),Some a) -> ctl_ax (ctl_ax a)
    | (_,None) -> failwith "not possible"
    | (_,Some a) ->
	ctl_ax
	  (ctl_and
	     (predmaker keep_wit (make_meta_rule_elem aft Ast.CstrTrue))
	     (ctl_ax a)) in
  let body =
    ctl_or
      (ctl_and truepred
	 (ctl_ax
	    (guard_ctl_term keep_wit thn)))
      (ctl_or fall
	 (ctl_and after end_code)) in
  ctl_and (ctl_term keep_wit body test)
    (match a with Some CTL.True | None -> ctl_true | Some _ -> ctl_ex after)

and do_between_dots keep_wit ba term after =
    match ba with
      Past.AddingBetweenDots (brace_term,n)
    | Past.DroppingBetweenDots (brace_term,n) ->
	(* not sure at all what to do here for after... *)
	let match_brace = ctl_term keep_wit after brace_term in
	let v = Printf.sprintf "_r_%d" n in
	let case1 = ctl_and (CTL.Ref v) match_brace in
	let case2 = ctl_and (ctl_not (CTL.Ref v)) term in
	CTL.Let
	  (v,ctl_or
	     (ctl_back_ex truepred)
	     (ctl_back_ex (ctl_back_ex falsepred)),
	   ctl_or case1 case2)
    | Past.NoDots -> term

(* --------------------------------------------------------------------- *)

let toctl sl = Asttoctl2.CODE (ctl_seq true ctl_true sl)
