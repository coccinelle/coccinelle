(*
 * Copyright 2012-2015, Inria
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./iso_pattern.ml"
(* Potential problem: offset of mcode is not updated when an iso is
instantiated, implying that a term may end up with many mcodes with the
same offset.  On the other hand, at the moment offset only seems to be used
before this phase.  Furthermore add_dot_binding relies on the offset to
remain the same between matching an iso and instantiating it with bindings. *)

(* Consider whether ... in iso should match <... ...> in smpl? *)

(* --------------------------------------------------------------------- *)
(* match a SmPL expression against a SmPL abstract syntax tree,
either - or + *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

let current_rule = ref ""
let verbose_iso = ref true

(* --------------------------------------------------------------------- *)

type isomorphism =
    Ast_cocci.metavar list * Ast0_cocci.anything list list * string (* name *)

let strip_info =
  let mcode (term,_,_,_,_,_) =
    (term,Ast0.NONE,Ast0.default_info(),Ast0.PLUS Ast.ONE,
     ref [],-1) in
  let donothing r k e =
    let x = k e in
    {(Ast0.wrap (Ast0.unwrap x)) with
      Ast0.mcodekind = ref (Ast0.PLUS Ast.ONE);
      Ast0.true_if_test = x.Ast0.true_if_test} in
  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing

let anything_equal = function
    (Ast0.DotsExprTag(d1),Ast0.DotsExprTag(d2)) ->
      failwith "not a possible variable binding" (*not sure why these are pbs*)
  | (Ast0.DotsInitTag(d1),Ast0.DotsInitTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.DotsParamTag(d1),Ast0.DotsParamTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.DotsStmtTag(d1),Ast0.DotsStmtTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_statement_dots d1) =
      (strip_info.VT0.rebuilder_rec_statement_dots d2)
  | (Ast0.DotsDeclTag(d1),Ast0.DotsDeclTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.DotsCaseTag(d1),Ast0.DotsCaseTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.IdentTag(d1),Ast0.IdentTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_ident d1) =
      (strip_info.VT0.rebuilder_rec_ident d2)
  | (Ast0.ExprTag(d1),Ast0.ExprTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_expression d1) =
      (strip_info.VT0.rebuilder_rec_expression d2)
  | (Ast0.ArgExprTag(_),_) | (_,Ast0.ArgExprTag(_)) ->
      failwith "not possible - only in isos1"
  | (Ast0.TestExprTag(_),_) | (_,Ast0.TestExprTag(_)) ->
      failwith "not possible - only in isos1"
  | (Ast0.TypeCTag(d1),Ast0.TypeCTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_typeC d1) =
      (strip_info.VT0.rebuilder_rec_typeC d2)
  | (Ast0.InitTag(d1),Ast0.InitTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_initialiser d1) =
      (strip_info.VT0.rebuilder_rec_initialiser d2)
  | (Ast0.ParamTag(d1),Ast0.ParamTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_parameter d1) =
      (strip_info.VT0.rebuilder_rec_parameter d2)
  | (Ast0.DeclTag(d1),Ast0.DeclTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_declaration d1) =
      (strip_info.VT0.rebuilder_rec_declaration d2)
  | (Ast0.StmtTag(d1),Ast0.StmtTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_statement d1) =
      (strip_info.VT0.rebuilder_rec_statement d2)
  | (Ast0.CaseLineTag(d1),Ast0.CaseLineTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_case_line d1) =
      (strip_info.VT0.rebuilder_rec_case_line d2)
  | (Ast0.StringFragmentTag(d1),Ast0.StringFragmentTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_string_fragment d1) =
      (strip_info.VT0.rebuilder_rec_string_fragment d2)
  | (Ast0.TopTag(d1),Ast0.TopTag(d2)) ->
      (strip_info.VT0.rebuilder_rec_top_level d1) =
      (strip_info.VT0.rebuilder_rec_top_level d2)
  | (Ast0.IsoWhenTTag(_),_) | (_,Ast0.IsoWhenTTag(_)) ->
      failwith "only for isos within iso phase"
  | (Ast0.IsoWhenFTag(_),_) | (_,Ast0.IsoWhenFTag(_)) ->
      failwith "only for isos within iso phase"
  | (Ast0.IsoWhenTag(_),_) | (_,Ast0.IsoWhenTag(_)) ->
      failwith "only for isos within iso phase"
  | _ -> false

let term (var1,_,_,_,_,_) = var1
let dot_term (var1,_,info,_,_,_) =
  ("", var1 ^ (string_of_int info.Ast0.pos_info.Ast0.offset))


type reason =
    NotPure of Ast0.pure * Ast.meta_name * Ast0.anything
  | NotPureLength of Ast.meta_name
  | ContextRequired of Ast0.anything
  | NonMatch
  | Braces of Ast0.statement
  | Nest of Ast0.statement
  | Position of Ast.meta_name
  | TypeMatch of reason list

(* it would be nice if this would go to standard error *)
let rec interpret_reason name line reason printer =
  Printf.printf
    "warning: iso %s does not match the code below on line %d\n" name line;
  printer(); Format.print_newline();
  match reason with
    NotPure(Ast0.Pure,(_,var),nonpure) ->
      Printf.printf
	"pure metavariable %s is matched against the following nonpure code:\n"
	var;
      Unparse_ast0.unparse_anything nonpure
  | NotPure(Ast0.Context,(_,var),nonpure) ->
      Printf.printf
	"context metavariable %s is matched against the following\nnoncontext code:\n"
	var;
      Unparse_ast0.unparse_anything nonpure
  | NotPure(Ast0.PureContext,(_,var),nonpure) ->
      Printf.printf
	"pure context metavariable %s is matched against the following\nnonpure or noncontext code:\n"
	var;
      Unparse_ast0.unparse_anything nonpure
  | NotPureLength((_,var)) ->
      Printf.printf
	"pure metavariable %s is matched against too much or too little code\n"
	var;
  | ContextRequired(term) ->
      Printf.printf
	"the following code matched is not uniformly minus or context,\nor contains a disjunction:\n";
      Unparse_ast0.unparse_anything term
  | Braces(s) ->
      Printf.printf "braces must be all minus (plus code allowed) or all\ncontext (plus code not allowed in the body) to match:\n";
      Unparse_ast0.statement "" s;
      Format.print_newline()
  | Nest(s) ->
      Printf.printf "iso with nest doesn't match whencode (TODO):\n";
      Unparse_ast0.statement "" s;
      Format.print_newline()
  | Position(rule,name) ->
      Printf.printf "position variable %s.%s conflicts with an isomorphism\n"
	rule name
   | TypeMatch reason_list ->
      List.iter (function r -> interpret_reason name line r printer)
	reason_list
  | _ -> failwith "not possible"

type 'a either = OK of 'a | Fail of reason

let add_binding var exp bindings =
  let var = term var in
  let attempt bindings =
    try
      let cur = List.assoc var bindings in
      if anything_equal(exp,cur) then [bindings] else []
    with Not_found -> [((var,exp)::bindings)] in
  match List.concat(List.map attempt bindings) with
    [] -> Fail NonMatch
  | x -> OK x

let add_dot_binding var exp bindings =
  let var = dot_term var in
  let attempt bindings =
    try
      let cur = List.assoc var bindings in
      if anything_equal(exp,cur) then [bindings] else []
    with Not_found -> [((var,exp)::bindings)] in
  match List.concat(List.map attempt bindings) with
    [] -> Fail NonMatch
  | x -> OK x

(* multi-valued *)
let add_multi_dot_binding var exp bindings =
  let var = dot_term var in
  let attempt bindings = [((var,exp)::bindings)] in
  match List.concat(List.map attempt bindings) with
    [] -> Fail NonMatch
  | x -> OK x

let rec nub ls =
  match ls with
    [] -> []
  | (x::xs) when (List.mem x xs) -> nub xs
  | (x::xs) -> x::(nub xs)

(* --------------------------------------------------------------------- *)

let init_env = [[]]

let debug str m binding =
  let res = m binding in
  (match res with
    None -> Printf.printf "%s: failed\n" str
  | Some binding ->
      List.iter
	(function binding ->
	  Printf.printf "%s: %s\n" str
	    (String.concat " " (List.map (function (x,_) -> x) binding)))
	binding);
  res

let conjunct_bindings
    (m1 : 'binding -> 'binding either)
    (m2 : 'binding -> 'binding either)
    (binding : 'binding) : 'binding either =
  match m1 binding with Fail(reason) -> Fail(reason) | OK binding -> m2 binding

let rec conjunct_many_bindings = function
    [] -> failwith "not possible"
  | [x] -> x
  | x::xs -> conjunct_bindings x (conjunct_many_bindings xs)

let mcode_equal (x,_,_,_,_,_) (y,_,_,_,_,_) = x = y

let assignOp_equal op1 op2 = match (Ast0.unwrap op1, Ast0.unwrap op2) with
  | Ast0.SimpleAssign _, Ast0.SimpleAssign _ -> true
  | Ast0.OpAssign o1, Ast0.OpAssign o2 -> mcode_equal o1 o2
  | Ast0.MetaAssign (mv1, _, _), Ast0.MetaAssign (mv2, _, _) ->
    mcode_equal mv1 mv2
  | _, _ -> false

let binaryOp_equal op1 op2 = match (Ast0.unwrap op1, Ast0.unwrap op2) with
  | Ast0.Arith o1, Ast0.Arith o2 -> mcode_equal o1 o2
  | Ast0.Logical o1, Ast0.Logical o2 -> mcode_equal o1 o2
  | Ast0.MetaBinary (mv1, _, _), Ast0.MetaBinary (mv2, _, _) ->
    mcode_equal mv1 mv2
  | _, _ -> false

let return b binding = if b then OK binding else Fail NonMatch
let return_false reason binding = Fail reason

let match_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> return true
  | _ -> return false

let bool_match_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> true
  | _ -> false

(* context_required is for the example
   if (
+      (int * )
       x == NULL)
  where we can't change x == NULL to eg NULL == x.  So there can either be
  nothing attached to the root or the term has to be all removed.
  if would be nice if we knew more about the relationship between the - and +
  code, because in the case where the + code is a separate statement in a
  sequence, this is not a problem.  Perhaps something could be done in
  insert_plus

   The example seems strange.  Why isn't the cast attached to x?
 *)
let is_context e =
  !Flag.sgrep_mode2 || (* everything is context for sgrep *)
  (match Ast0.get_mcodekind e with
    Ast0.CONTEXT(cell) -> true
  | _ -> false)

(* needs a special case when there is a Disj or an empty DOTS
   the following stops at the statement level, and gives true if one
   statement is replaced by another *)
let rec is_pure_context s =
  !Flag.sgrep_mode2 || (* everything is context for sgrep *)
  (match Ast0.unwrap s with
    Ast0.Disj(starter,statement_dots_list,mids,ender) ->
      List.for_all
	(function x ->
	  match Ast0.undots x with
	    [s] -> is_pure_context s
	  | _ -> false (* could we do better? *))
	statement_dots_list
  | _ ->
      (match Ast0.get_mcodekind s with
	Ast0.CONTEXT(mc) ->
	  (match !mc with
	    (Ast.NOTHING,_,_) -> true
	  | _ -> false)
      | Ast0.MINUS(mc) ->
	  (match !mc with
 	(* do better for the common case of replacing a stmt by another one *)
	    (Ast.REPLACEMENT([[Ast.StatementTag(s)]],_),_) ->
	      (match Ast.unwrap s with
		Ast.IfThen(_,_,_) -> false (* potentially dangerous *)
	      | _ -> true)
	  | (_,_) -> false)
      | _ -> false))

let is_minus e =
  match Ast0.get_mcodekind e with Ast0.MINUS(cell) -> true | _ -> false

let match_list matcher is_list_matcher do_list_match la lb =
  let rec loop = function
      ([],[]) -> return true
    | ([x],lb) when is_list_matcher x -> do_list_match x lb
    | (x::xs,y::ys) -> conjunct_bindings (matcher x y) (loop (xs,ys))
    | _ -> return false in
  loop (la,lb)

let all_caps = Str.regexp "^[A-Z_][A-Z_0-9]*$"

let match_maker checks_needed context_required whencode_allowed =

  let check_mcode pmc (*pattern*) cmc (*code*) binding =
    if checks_needed
    then
      match Ast0.get_pos cmc with
	[] -> OK binding (* no hidden vars in smpl code, so nothing to do *)
      |	((a::_) as hidden_code) ->
	  let hidden_pattern =
	    List.filter (function Ast0.HiddenVarTag _ -> true | _ -> false)
	      (Ast0.get_pos pmc) in
	  (match hidden_pattern with
	    [Ast0.HiddenVarTag([Ast0.MetaPosTag(Ast0.MetaPos (name1,_,_))])] ->
	      add_binding name1 (Ast0.HiddenVarTag(hidden_code)) binding
	  | [] -> Fail(Position(Ast0.unwrap_mcode(Ast0.meta_pos_name a)))
	  | _ -> failwith "badly compiled iso - multiple hidden variable")
    else OK binding in

  let check_assignOp_mcode op1 op2 binding =
    match (Ast0.unwrap op1, Ast0.unwrap op2) with
      Ast0.SimpleAssign o1, Ast0.SimpleAssign o2 -> check_mcode o1 o2 binding
    | Ast0.OpAssign o1, Ast0.OpAssign o2 -> check_mcode o1 o2 binding
    | Ast0.MetaAssign(mv1,_,_), Ast0.MetaAssign(mv2,_,_) ->
      check_mcode mv1 mv2 binding
    | _ -> Fail(NonMatch) in

  let check_binaryOp_mcode op1 op2 binding =
    match (Ast0.unwrap op1, Ast0.unwrap op2) with
      Ast0.Arith o1, Ast0.Arith o2 -> check_mcode o1 o2 binding
    | Ast0.Logical o1, Ast0.Logical o2 -> check_mcode o1 o2 binding
    | Ast0.MetaBinary(mv1,_,_), Ast0.MetaBinary(mv2,_,_) ->
      check_mcode mv1 mv2 binding
    | _ -> Fail(NonMatch) in
  let match_dots matcher is_list_matcher do_list_match d1 d2 =
    match (Ast0.unwrap d1, Ast0.unwrap d2) with
      (Ast0.DOTS(la),Ast0.DOTS(lb))
    | (Ast0.CIRCLES(la),Ast0.CIRCLES(lb))
    | (Ast0.STARS(la),Ast0.STARS(lb)) ->
	match_list matcher is_list_matcher (do_list_match d2) la lb
    | _ -> return false in

  let is_elist_matcher el =
    match Ast0.unwrap el with Ast0.MetaExprList(_,_,_) -> true | _ -> false in

  let is_plist_matcher pl =
    match Ast0.unwrap pl with Ast0.MetaParamList(_,_,_) -> true | _ -> false in

  let is_slist_matcher pl =
    match Ast0.unwrap pl with Ast0.MetaStmtList(_,_) -> true | _ -> false in

  let is_strlist_matcher sl = false in

  let no_list _ = false in

  let build_dots pattern data =
    match Ast0.unwrap pattern with
      Ast0.DOTS(_) -> Ast0.rewrap pattern (Ast0.DOTS(data))
    | Ast0.CIRCLES(_) -> Ast0.rewrap pattern (Ast0.CIRCLES(data))
    | Ast0.STARS(_) -> Ast0.rewrap pattern (Ast0.STARS(data)) in

  let pure_sp_code =
    let bind = Ast0.lub_pure in
    let option_default = Ast0.Context in
    let pure_mcodekind mc =
      if !Flag.sgrep_mode2
      then Ast0.PureContext
      else
	match mc with
	  Ast0.CONTEXT(mc) ->
	    (match !mc with
	      (Ast.NOTHING,_,_) -> Ast0.PureContext
	    | _ -> Ast0.Context)
	| Ast0.MINUS(mc) ->
	    (match !mc with
	      (Ast.NOREPLACEMENT,_) -> Ast0.Pure
	    | _ ->  Ast0.Impure)
	| _ -> Ast0.Impure in
    let donothing r k e =
      bind (pure_mcodekind (Ast0.get_mcodekind e)) (k e) in

    let mcode m =
      Ast0.lub_pure
	(if Ast0.get_pos m = [] then Ast0.PureContext else Ast0.Impure)
	(pure_mcodekind (Ast0.get_mcode_mcodekind m)) in

    (* a case for everything that has a metavariable *)
    (* pure is supposed to match only unitary metavars, not anything that
       contains only unitary metavars *)
    let ident r k i =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind i)) (k i))
	(match Ast0.unwrap i with
	  Ast0.MetaId(name,_,_,pure) | Ast0.MetaFunc(name,_,pure)
	| Ast0.MetaLocalFunc(name,_,pure) -> pure
	| _ -> Ast0.Impure) in

    let expression r k e =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind e)) (k e))
	(match Ast0.unwrap e with
	  Ast0.MetaErr(name,_,pure)
	| Ast0.MetaExpr(name,_,_,_,pure) | Ast0.MetaExprList(name,_,pure) ->
	    pure
	| _ -> Ast0.Impure) in

    let assignOp r k e =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind e)) (k e))
	   (match Ast0.unwrap e with
              Ast0.MetaAssign(_, _, pure) -> pure
            | _ -> Ast0.Impure) in

    let binaryOp r k e =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind e)) (k e))
	   (match Ast0.unwrap e with
              Ast0.MetaBinary(_, _, pure) -> pure
            | _ -> Ast0.Impure) in

    let typeC r k t =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind t)) (k t))
	(match Ast0.unwrap t with
	  Ast0.MetaType(name,pure) -> pure
	| _ -> Ast0.Impure) in

    let init r k t =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind t)) (k t))
	(match Ast0.unwrap t with
	  Ast0.MetaInit(name,pure) | Ast0.MetaInitList(name,_,pure) -> pure
	| _ -> Ast0.Impure) in

    let param r k p =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind p)) (k p))
	(match Ast0.unwrap p with
	  Ast0.MetaParam(name,pure) | Ast0.MetaParamList(name,_,pure) -> pure
	| _ -> Ast0.Impure) in

    let decl r k d =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind d)) (k d))
	(match Ast0.unwrap d with
	  Ast0.MetaDecl(name,pure) | Ast0.MetaField(name,pure)
	| Ast0.MetaFieldList(name,_,pure) ->
	    pure
	| _ -> Ast0.Impure) in

    let stmt r k s =
      bind (bind (pure_mcodekind (Ast0.get_mcodekind s)) (k s))
	(match Ast0.unwrap s with
	  Ast0.MetaStmt(name,pure) | Ast0.MetaStmtList(name,pure) -> pure
	| _ -> Ast0.Impure) in

    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing
      ident expression assignOp binaryOp typeC init param decl stmt donothing donothing
      donothing donothing in

  let add_pure_list_binding name pure is_pure builder1 builder2 lst =
    match (checks_needed,pure) with
      (true,Ast0.Pure) | (true,Ast0.Context) | (true,Ast0.PureContext) ->
	(match lst with
	  [x] ->
	    if (Ast0.lub_pure (is_pure x) pure) = pure
	    then add_binding name (builder1 lst)
	    else return_false (NotPure (pure,term name,builder1 lst))
	| _ -> return_false (NotPureLength (term name)))
    | (false,_) | (_,Ast0.Impure) -> add_binding name (builder2 lst) in

  let add_pure_binding name pure is_pure builder x =
    match (checks_needed,pure) with
      (true,Ast0.Pure) | (true,Ast0.Context) | (true,Ast0.PureContext) ->
	if (Ast0.lub_pure (is_pure x) pure) = pure
	then add_binding name (builder x)
	else return_false (NotPure (pure,term name, builder x))
    | (false,_) | (_,Ast0.Impure) ->  add_binding name (builder x) in

  let do_elist_match builder el lst =
    match Ast0.unwrap el with
      Ast0.MetaExprList(name,lenname,pure) ->
        (*how to handle lenname? should it be an option type and always None?*)
	failwith "expr list pattern not supported in iso"
	(*add_pure_list_binding name pure
	  pure_sp_code.V0.combiner_expression
	  (function lst -> Ast0.ExprTag(List.hd lst))
	  (function lst -> Ast0.DotsExprTag(build_dots builder lst))
	  lst*)
    | _ -> failwith "not possible" in

  let do_plist_match builder pl lst =
    match Ast0.unwrap pl with
      Ast0.MetaParamList(name,lename,pure) ->
	failwith "param list pattern not supported in iso"
	(*add_pure_list_binding name pure
	  pure_sp_code.V0.combiner_parameter
	  (function lst -> Ast0.ParamTag(List.hd lst))
	  (function lst -> Ast0.DotsParamTag(build_dots builder lst))
	  lst*)
    | _ -> failwith "not possible" in

  let do_slist_match builder sl lst =
    match Ast0.unwrap sl with
      Ast0.MetaStmtList(name,pure) ->
	add_pure_list_binding name pure
	  pure_sp_code.VT0.combiner_rec_statement
	  (function lst -> Ast0.StmtTag(List.hd lst))
	  (function lst -> Ast0.DotsStmtTag(build_dots builder lst))
	  lst
    | _ -> failwith "not possible" in

  let do_nolist_match _ _ = failwith "not possible" in

  let rec match_ident pattern id =
    match Ast0.unwrap pattern with
      Ast0.MetaId(name,_,_,pure) ->
	(add_pure_binding name pure pure_sp_code.VT0.combiner_rec_ident
	  (function id -> Ast0.IdentTag id) id)
    | Ast0.MetaFunc(name,_,pure) -> failwith "metafunc not supported"
    | Ast0.MetaLocalFunc(name,_,pure) -> failwith "metalocalfunc not supported"
    | up ->
	if not(checks_needed) || not(context_required) || is_context id
	then
	  match (up,Ast0.unwrap id) with
	    (Ast0.Id(namea),Ast0.Id(nameb)) ->
	      if mcode_equal namea nameb
	      then check_mcode namea nameb
	      else return false
	  | (Ast0.DisjId(_,ids,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.OptIdent(ida),Ast0.OptIdent(idb))
	  | (Ast0.UniqueIdent(ida),Ast0.UniqueIdent(idb)) ->
	      match_ident ida idb
	  | (_,Ast0.OptIdent(idb))
	  | (_,Ast0.UniqueIdent(idb)) -> match_ident pattern idb
	  | _ -> return false
	else return_false (ContextRequired (Ast0.IdentTag id)) in

  (* should we do something about matching metavars against ...? *)
  let rec match_expr pattern expr =
    match Ast0.unwrap pattern with
      Ast0.MetaExpr(name,_,ty,form,pure) ->
	let form_ok =
	  match (form,expr) with
	    (Ast.ANY,_) -> true
	  | (Ast.CONST,e) ->
	      let rec matches e =
		match Ast0.unwrap e with
		  Ast0.Constant _ | Ast0.StringConstant _ -> true
		| Ast0.Ident(c) ->
		    (match Ast0.unwrap c with
		      Ast0.Id(nm) ->
			let nm = Ast0.unwrap_mcode nm in
			(* all caps is a const *)
			Str.string_match all_caps nm 0
		    | _ -> false)
		| Ast0.Cast(lp,ty,rp,e) -> matches e
		| Ast0.SizeOfExpr(se,exp) -> true
		| Ast0.SizeOfType(se,lp,ty,rp) -> true
		| Ast0.MetaExpr(nm,_,_,Ast.CONST,p) ->
		    (Ast0.lub_pure p pure) = pure
		| _ -> false in
	      matches e
	  | (Ast.ID,e) | (Ast.LocalID,e) | (Ast.GlobalID,e) ->
	      let rec matches e =
		match Ast0.unwrap e with
		  Ast0.Ident(c) -> true
		| Ast0.Cast(lp,ty,rp,e) -> matches e
		| Ast0.MetaExpr(nm,_,_,Ast.ID,p) ->
		    (Ast0.lub_pure p pure) = pure
		| _ -> false in
	      matches e in
	if form_ok
	then
	  match ty with
	    Some ts ->
	      if List.exists
		  (function Type_cocci.MetaType(_,_,_) -> true | _ -> false)
		  ts
	      then
		(match ts with
		  [Type_cocci.MetaType(tyname,_,_)] ->
		    let expty =
		      match (Ast0.unwrap expr,Ast0.get_type expr) with
		  (* easier than updating type inferencer to manage multiple
		     types *)
			(Ast0.MetaExpr(_,_,Some tts,_,_),_) -> Some tts
		      | (_,Some ty) -> Some [ty]
		      | _ -> None in
		    (match expty with
		      Some expty ->
			let tyname = Ast0.rewrap_mcode name tyname in
			conjunct_bindings
			  (add_pure_binding name pure
			     pure_sp_code.VT0.combiner_rec_expression
			     (function expr -> Ast0.ExprTag expr)
			     expr)
			  (function bindings ->
			    let attempts =
			      List.map
				(function expty ->
				  (try
				    add_pure_binding tyname Ast0.Impure
				      (function _ -> Ast0.Impure)
				      (function ty -> Ast0.TypeCTag ty)
				      (Ast0.rewrap expr
					 (Ast0.reverse_type expty))
				      bindings
				  with Ast0.TyConv ->
				    Printf.printf
				      "warning: unconvertible type";
				    return false bindings))
				expty in
			    if List.exists
				(function Fail _ -> false | OK x -> true)
				attempts
			    then
				(* not sure why this is ok. can there be more
				   than one OK? *)
			      OK (List.concat
				    (List.map
				       (function Fail _ -> [] | OK x -> x)
				       attempts))
			    else
			      Fail
				(TypeMatch
				   (List.map
				      (function
					  Fail r -> r
					| OK x -> failwith "not possible")
				      attempts)))
		    | _ ->
		  (*Printf.printf
		     "warning: type metavar can only match one type";*)
			return false)
		| _ ->
		    failwith
		      "mixture of metatype and other types not supported")
	      else
		let expty = Ast0.get_type expr in
		if List.exists (function t -> Type_cocci.compatible t expty) ts
		then
		  add_pure_binding name pure
		    pure_sp_code.VT0.combiner_rec_expression
		    (function expr -> Ast0.ExprTag expr)
		    expr
		else return false
	  | None ->
	      add_pure_binding name pure
		pure_sp_code.VT0.combiner_rec_expression
		(function expr -> Ast0.ExprTag expr)
		expr
	else return false
    | Ast0.MetaErr(namea,_,pure) -> failwith "metaerr not supported"
    | Ast0.MetaExprList(_,_,_) -> failwith "metaexprlist not supported"
    | up ->
	if not(checks_needed) || not(context_required) || is_context expr
	then
	  match (up,Ast0.unwrap expr) with
	    (Ast0.Ident(ida),Ast0.Ident(idb)) ->
	      match_ident ida idb
	  | (Ast0.Constant(consta),Ast0.Constant(constb)) ->
	      if mcode_equal consta constb
	      then check_mcode consta constb
	      else return false
	  | (Ast0.StringConstant(la,stra,ra),
	     Ast0.StringConstant(lb,strb,rb)) ->
	       conjunct_many_bindings
		 [check_mcode la lb; check_mcode rb rb;
		   match_dots match_frag is_strlist_matcher do_nolist_match
		     stra strb]
	  | (Ast0.FunCall(fna,lp1,argsa,rp1),Ast0.FunCall(fnb,lp,argsb,rp)) ->
	      conjunct_many_bindings
		[check_mcode lp1 lp; check_mcode rp1 rp; match_expr fna fnb;
		  match_dots match_expr is_elist_matcher do_elist_match
		    argsa argsb]
	  | (Ast0.Assignment(lefta,opa,righta,_),
	     Ast0.Assignment(leftb,opb,rightb,_)) ->
	       if assignOp_equal opa opb
	       then
		 conjunct_many_bindings
		   [check_assignOp_mcode opa opb; match_expr lefta leftb;
		     match_expr righta rightb]
	       else return false
	  | (Ast0.Sequence(lefta,opa,righta),
	     Ast0.Sequence(leftb,opb,rightb)) ->
	       if mcode_equal opa opb
	       then
		 conjunct_many_bindings
		   [check_mcode opa opb; match_expr lefta leftb;
		     match_expr righta rightb]
	       else return false
	  | (Ast0.CondExpr(exp1a,lp1,exp2a,rp1,exp3a),
	     Ast0.CondExpr(exp1b,lp,exp2b,rp,exp3b)) ->
	       conjunct_many_bindings
		 [check_mcode lp1 lp; check_mcode rp1 rp;
		   match_expr exp1a exp1b; match_option match_expr exp2a exp2b;
		   match_expr exp3a exp3b]
	  | (Ast0.Postfix(expa,opa),Ast0.Postfix(expb,opb)) ->
	      if mcode_equal opa opb
	      then
		conjunct_bindings (check_mcode opa opb) (match_expr expa expb)
	      else return false
	  | (Ast0.Infix(expa,opa),Ast0.Infix(expb,opb)) ->
	      if mcode_equal opa opb
	      then
		conjunct_bindings (check_mcode opa opb) (match_expr expa expb)
	      else return false
	  | (Ast0.Unary(expa,opa),Ast0.Unary(expb,opb)) ->
	      if mcode_equal opa opb
	      then
		conjunct_bindings (check_mcode opa opb) (match_expr expa expb)
	      else return false
	  | (Ast0.Binary(lefta,opa,righta),Ast0.Binary(leftb,opb,rightb)) ->
	      if binaryOp_equal opa opb
	      then
		conjunct_many_bindings
		  [check_binaryOp_mcode opa opb; match_expr lefta leftb;
		    match_expr righta rightb]
	      else return false
	  | (Ast0.Paren(lp1,expa,rp1),Ast0.Paren(lp,expb,rp)) ->
	      conjunct_many_bindings
		[check_mcode lp1 lp; check_mcode rp1 rp; match_expr expa expb]
	  | (Ast0.ArrayAccess(exp1a,lb1,exp2a,rb1),
	     Ast0.ArrayAccess(exp1b,lb,exp2b,rb)) ->
	       conjunct_many_bindings
		 [check_mcode lb1 lb; check_mcode rb1 rb;
		   match_expr exp1a exp1b; match_expr exp2a exp2b]
	  | (Ast0.RecordAccess(expa,opa,fielda),
	     Ast0.RecordAccess(expb,op,fieldb))
	  | (Ast0.RecordPtAccess(expa,opa,fielda),
	     Ast0.RecordPtAccess(expb,op,fieldb)) ->
	       conjunct_many_bindings
		 [check_mcode opa op; match_expr expa expb;
		   match_ident fielda fieldb]
	  | (Ast0.Cast(lp1,tya,rp1,expa),Ast0.Cast(lp,tyb,rp,expb)) ->
	      conjunct_many_bindings
		[check_mcode lp1 lp; check_mcode rp1 rp;
		  match_typeC tya tyb; match_expr expa expb]
	  | (Ast0.SizeOfExpr(szf1,expa),Ast0.SizeOfExpr(szf,expb)) ->
	      conjunct_bindings (check_mcode szf1 szf) (match_expr expa expb)
	  | (Ast0.SizeOfType(szf1,lp1,tya,rp1),
	     Ast0.SizeOfType(szf,lp,tyb,rp)) ->
	       conjunct_many_bindings
		 [check_mcode lp1 lp; check_mcode rp1 rp;
		   check_mcode szf1 szf; match_typeC tya tyb]
	  | (Ast0.Constructor(lp1,tya,rp1,inita),
	     Ast0.Constructor(lp,tyb,rp,initb)) ->
	       conjunct_many_bindings
		 [check_mcode lp1 lp; check_mcode rp1 rp;
		   match_typeC tya tyb; match_init inita initb]
	  | (Ast0.TypeExp(tya),Ast0.TypeExp(tyb)) ->
	      match_typeC tya tyb
	  | (Ast0.EComma(cm1),Ast0.EComma(cm)) -> check_mcode cm1 cm
	  | (Ast0.DisjExpr(_,expsa,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.NestExpr(_,exp_dotsa,_,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.Edots(d,None),Ast0.Edots(d1,None))
	  | (Ast0.Ecircles(d,None),Ast0.Ecircles(d1,None))
	  | (Ast0.Estars(d,None),Ast0.Estars(d1,None)) -> check_mcode d d1
	  | (Ast0.Edots(ed,None),Ast0.Edots(ed1,Some (wh,e,wc)))
	  | (Ast0.Ecircles(ed,None),Ast0.Ecircles(ed1,Some (wh,e,wc)))
	  | (Ast0.Estars(ed,None),Ast0.Estars(ed1,Some (wh,e,wc))) ->
	    (* hope that mcode of edots is unique somehow *)
	      conjunct_bindings (check_mcode ed ed1)
		(let (edots_whencode_allowed,_,_) = whencode_allowed in
		if edots_whencode_allowed
		then add_dot_binding ed
		  (Ast0.WhenTag(wh,Some e,Ast0.ExprTag wc))
		else
		  (Printf.printf
		     "warning: not applying iso because of whencode";
		   return false))
	  | (Ast0.Edots(_,Some _),_) | (Ast0.Ecircles(_,Some _),_)
	  | (Ast0.Estars(_,Some _),_) ->
	      failwith "whencode not allowed in a pattern1"
	  | (Ast0.OptExp(expa),Ast0.OptExp(expb))
	  | (Ast0.UniqueExp(expa),Ast0.UniqueExp(expb)) ->
	      match_expr expa expb
	  | (_,Ast0.OptExp(expb))
	  | (_,Ast0.UniqueExp(expb)) -> match_expr pattern expb
	  | _ -> return false
	else return_false (ContextRequired (Ast0.ExprTag expr))

  and match_frag e1 e2 = (* not an entry point, must be identical *)
    match (Ast0.unwrap e1,Ast0.unwrap e2) with
      (Ast0.ConstantFragment(str1),Ast0.ConstantFragment(str2)) ->
	if mcode_equal str1 str2
	then check_mcode str1 str2
	else return false
    | (Ast0.FormatFragment(pct1,fmt1),
       Ast0.FormatFragment(pct2,fmt2)) ->
	 conjunct_many_bindings [check_mcode pct1 pct2; match_format fmt1 fmt2]
    | (Ast0.Strdots(d1),Ast0.Strdots(d2)) -> check_mcode d1 d2
    | (Ast0.MetaFormatList(pct,name,lenname),_) ->
	failwith "not allowed in iso"
    | _ -> return false

  and match_format e1 e2 = (* not an entry point, must be identical *)
    match (Ast0.unwrap e1,Ast0.unwrap e2) with
      (Ast0.ConstantFormat(str1),Ast0.ConstantFormat(str2)) ->
	if mcode_equal str1 str2
	then check_mcode str1 str2
	else return false
    | (Ast0.MetaFormat(name,constraints),_) -> failwith "not allowed in iso"
    | _ -> return false

(* the special case for function types prevents the eg T X; -> T X = E; iso
   from applying, which doesn't seem very relevant, but it also avoids a
   mysterious bug that is obtained with eg int attach(...); *)
  and varargs_equal (comma1, ellipsis1) (comma2, ellipsis2) =
    let c1 = Ast0_cocci.unwrap_mcode comma1
    and e1 = Ast0_cocci.unwrap_mcode ellipsis1
    and c2 = Ast0_cocci.unwrap_mcode comma2
    and e2 = Ast0_cocci.unwrap_mcode ellipsis2
    in return (c1="," && e1="......" && c2=c1 && e2=e1)
  and match_typeC pattern t =
    match Ast0.unwrap pattern with
      Ast0.MetaType(name,pure) ->
	add_pure_binding name pure pure_sp_code.VT0.combiner_rec_typeC
	  (function ty -> Ast0.TypeCTag ty)
	  t
    | up ->
	if not(checks_needed) || not(context_required) || is_context t
	then
	  match (up,Ast0.unwrap t) with
	    (Ast0.ConstVol(cva,tya),Ast0.ConstVol(cvb,tyb)) ->
	      if mcode_equal cva cvb
	      then
		conjunct_bindings (check_mcode cva cvb) (match_typeC tya tyb)
	      else return false
	  | (Ast0.BaseType(tya,stringsa),Ast0.BaseType(tyb,stringsb)) ->
	      if tya = tyb
	      then
		match_list check_mcode
		  (function _ -> false) (function _ -> failwith "")
		  stringsa stringsb
	      else return false
	  | (Ast0.Signed(signa,tya),Ast0.Signed(signb,tyb)) ->
	      if mcode_equal signa signb
	      then
		conjunct_bindings (check_mcode signa signb)
		  (match_option match_typeC tya tyb)
	      else return false
	  | (Ast0.Pointer(tya,star1),Ast0.Pointer(tyb,star)) ->
	      conjunct_bindings (check_mcode star1 star) (match_typeC tya tyb)
	  | (Ast0.FunctionPointer(tya,lp1a,stara,rp1a,lp2a,paramsa,rp2a),
	     Ast0.FunctionPointer(tyb,lp1b,starb,rp1b,lp2b,paramsb,rp2b)) ->
	       conjunct_many_bindings
		 [check_mcode stara starb; check_mcode lp1a lp1b;
		   check_mcode rp1a rp1b; check_mcode lp2a lp2b;
		   check_mcode rp2a rp2b; match_typeC tya tyb;
		   match_dots match_param is_plist_matcher
		     do_plist_match paramsa paramsb]
	  | (Ast0.Array(tya,lb1,sizea,rb1),Ast0.Array(tyb,lb,sizeb,rb)) ->
	      conjunct_many_bindings
		[check_mcode lb1 lb; check_mcode rb1 rb;
		  match_typeC tya tyb; match_option match_expr sizea sizeb]
	  | (Ast0.Decimal(dec1,lp1,len1,comma1,prec_opt1,rp1),
	     Ast0.Decimal(dec2,lp2,len2,comma2,prec_opt2,rp2)) ->
	       conjunct_many_bindings
		 [check_mcode dec1 dec2; check_mcode lp1 lp2;
		   match_expr len1 len2;
		   match_option check_mcode comma1 comma2;
		   match_option match_expr prec_opt1 prec_opt2;
		   check_mcode rp1 rp2]
	  | (Ast0.EnumName(kinda,Some namea),
	     Ast0.EnumName(kindb,Some nameb)) ->
	       conjunct_bindings (check_mcode kinda kindb)
		 (match_ident namea nameb)
	  | (Ast0.EnumDef(tya,lb1,idsa,rb1),
	     Ast0.EnumDef(tyb,lb,idsb,rb)) ->
	       conjunct_many_bindings
		 [check_mcode lb1 lb; check_mcode rb1 rb;
		   match_typeC tya tyb;
		   match_dots match_expr no_list do_nolist_match idsa idsb]
	  | (Ast0.StructUnionName(kinda,Some namea),
	     Ast0.StructUnionName(kindb,Some nameb)) ->
	       if mcode_equal kinda kindb
	       then
		 conjunct_bindings (check_mcode kinda kindb)
		   (match_ident namea nameb)
	       else return false
	  | (Ast0.StructUnionDef(tya,lb1,declsa,rb1),
	     Ast0.StructUnionDef(tyb,lb,declsb,rb)) ->
	       conjunct_many_bindings
		 [check_mcode lb1 lb; check_mcode rb1 rb;
		   match_typeC tya tyb;
		   match_dots match_decl no_list do_nolist_match declsa declsb]
	  | (Ast0.TypeName(namea),Ast0.TypeName(nameb)) ->
	      if mcode_equal namea nameb
	      then check_mcode namea nameb
	      else return false
	  | (Ast0.DisjType(_,typesa,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.OptType(tya),Ast0.OptType(tyb))
	  | (Ast0.UniqueType(tya),Ast0.UniqueType(tyb)) -> match_typeC tya tyb
	  | (_,Ast0.OptType(tyb))
	  | (_,Ast0.UniqueType(tyb)) -> match_typeC pattern tyb
	  | _ -> return false
	else return_false (ContextRequired (Ast0.TypeCTag t))

  and match_decl pattern d =
    match Ast0.unwrap pattern with
      Ast0.MetaDecl(name,pure) ->
	add_pure_binding name pure pure_sp_code.VT0.combiner_rec_declaration
	  (function d -> Ast0.DeclTag d)
	  d
    | Ast0.MetaField(name,pure) ->
	add_pure_binding name pure pure_sp_code.VT0.combiner_rec_declaration
	  (function d -> Ast0.DeclTag d)
	  d
    | Ast0.MetaFieldList(name,_,pure) -> failwith "metafieldlist not supporte"
    | up ->
	if not(checks_needed) || not(context_required) || is_context d
	then
	  match (up,Ast0.unwrap d) with
	    (Ast0.Init(stga,tya,ida,eq1,inia,sc1),
	     Ast0.Init(stgb,tyb,idb,eq,inib,sc)) ->
	       if bool_match_option mcode_equal stga stgb
	       then
		 conjunct_many_bindings
		   [check_mcode eq1 eq; check_mcode sc1 sc;
		     match_option check_mcode stga stgb;
		     match_typeC tya tyb; match_ident ida idb;
		     match_init inia inib]
	       else return false
	  | (Ast0.UnInit(stga,tya,ida,sc1),Ast0.UnInit(stgb,tyb,idb,sc)) ->
	      if bool_match_option mcode_equal stga stgb
	      then
		conjunct_many_bindings
		  [check_mcode sc1 sc; match_option check_mcode stga stgb;
		    match_typeC tya tyb; match_ident ida idb]
	      else return false
	  | (Ast0.FunProto(fninfo1,name1,lp1,params1,va1a,rp1,sem1),
	     Ast0.FunProto(fninfo,name,lp,params,va1b,rp,sem)) ->
	       conjunct_many_bindings
		 [check_mcode lp1 lp; check_mcode rp1 rp; check_mcode sem1 sem;
		   match_fninfo fninfo1 fninfo; match_ident name1 name;
		   match_dots match_param is_plist_matcher do_plist_match
		     params1 params;
                   match_option varargs_equal va1a va1b
                 ]
	  | (Ast0.MacroDecl(stga,namea,lp1,argsa,rp1,sc1),
	     Ast0.MacroDecl(stgb,nameb,lp,argsb,rp,sc)) ->
	       if bool_match_option mcode_equal stga stgb
	       then
		 conjunct_many_bindings
		   [match_ident namea nameb;
		     check_mcode lp1 lp; check_mcode rp1 rp;
		     check_mcode sc1 sc;
		     match_dots match_expr is_elist_matcher do_elist_match
		       argsa argsb]
	       else return false
	  | (Ast0.MacroDeclInit(stga,namea,lp1,argsa,rp1,eq1,ini1,sc1),
	     Ast0.MacroDeclInit(stgb,nameb,lp,argsb,rp,eq,ini,sc)) ->
	       if bool_match_option mcode_equal stga stgb
	       then
		 conjunct_many_bindings
		   [match_ident namea nameb;
		     check_mcode lp1 lp; check_mcode rp1 rp;
		     check_mcode eq1 eq;
		     check_mcode sc1 sc;
		     match_dots match_expr is_elist_matcher do_elist_match
		       argsa argsb;
		     match_init ini1 ini]
	       else return false
	  | (Ast0.TyDecl(tya,sc1),Ast0.TyDecl(tyb,sc)) ->
	      conjunct_bindings (check_mcode sc1 sc) (match_typeC tya tyb)
	  | (Ast0.Typedef(stga,tya,ida,sc1),Ast0.Typedef(stgb,tyb,idb,sc)) ->
	      conjunct_bindings (check_mcode sc1 sc)
		(conjunct_bindings (match_typeC tya tyb) (match_typeC ida idb))
	  | (Ast0.DisjDecl(_,declsa,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.Ddots(d1,None),Ast0.Ddots(d,None)) -> check_mcode d1 d
	  |	(Ast0.Ddots(dd,None),Ast0.Ddots(d,Some (wh,ee,wc))) ->
	      conjunct_bindings (check_mcode dd d)
	    (* hope that mcode of ddots is unique somehow *)
		(let (ddots_whencode_allowed,_,_) = whencode_allowed in
		if ddots_whencode_allowed
		then add_dot_binding dd
		  (Ast0.WhenTag (wh,Some ee,Ast0.DeclTag wc))
		else
		  (Printf.printf "warning: not applying iso because of whencode";
		   return false))
	  | (Ast0.Ddots(_,Some _),_) ->
	      failwith "whencode not allowed in a pattern1"

	  | (Ast0.OptDecl(decla),Ast0.OptDecl(declb))
	  | (Ast0.UniqueDecl(decla),Ast0.UniqueDecl(declb)) ->
	      match_decl decla declb
	  | (_,Ast0.OptDecl(declb))
	  | (_,Ast0.UniqueDecl(declb)) ->
	      match_decl pattern declb
	  | _ -> return false
	else return_false (ContextRequired (Ast0.DeclTag d))

  and match_init pattern i =
    match Ast0.unwrap pattern with
      Ast0.MetaInit(name,pure) ->
	add_pure_binding name pure pure_sp_code.VT0.combiner_rec_initialiser
	  (function ini -> Ast0.InitTag ini)
	  i
    | up ->
	if not(checks_needed) || not(context_required) || is_context i
	then
	  match (up,Ast0.unwrap i) with
	    (Ast0.InitExpr(expa),Ast0.InitExpr(expb)) ->
	      match_expr expa expb
	  | (Ast0.InitList(lb1,initlista,rb1,oa),
	     Ast0.InitList(lb,initlistb,rb,ob))
	    when oa = ob ->
	      conjunct_many_bindings
		[check_mcode lb1 lb; check_mcode rb1 rb;
		  match_dots match_init no_list do_nolist_match
		    initlista initlistb]
	  | (Ast0.InitGccExt(designators1,e1,inia),
	     Ast0.InitGccExt(designators2,e2,inib)) ->
	       conjunct_many_bindings
		 [match_list match_designator
		     (function _ -> false) (function _ -> failwith "")
		     designators1 designators2;
		   check_mcode e1 e2;
		   match_init inia inib]
	  | (Ast0.InitGccName(namea,c1,inia),Ast0.InitGccName(nameb,c,inib)) ->
	      conjunct_many_bindings
		[check_mcode c1 c; match_ident namea nameb;
		  match_init inia inib]
	  | (Ast0.IComma(c1),Ast0.IComma(c)) -> check_mcode c1 c
	  | (Ast0.Idots(d1,None),Ast0.Idots(d,None)) -> check_mcode d1 d
	  | (Ast0.Idots(id,None),Ast0.Idots(d,Some (wh,e,wc))) ->
	      conjunct_bindings (check_mcode id d)
	  (* hope that mcode of edots is unique somehow *)
		(let (_,idots_whencode_allowed,_) = whencode_allowed in
		if idots_whencode_allowed
		then add_dot_binding id
		  (Ast0.WhenTag(wh,Some e,Ast0.InitTag wc))
		else
		  (Printf.printf
		     "warning: not applying iso because of whencode";
		   return false))
	  | (Ast0.Idots(_,Some _),_) ->
	      failwith "whencode not allowed in a pattern2"
	  | (Ast0.OptIni(ia),Ast0.OptIni(ib))
	  | (Ast0.UniqueIni(ia),Ast0.UniqueIni(ib)) -> match_init ia ib
	  | (_,Ast0.OptIni(ib))
	  | (_,Ast0.UniqueIni(ib)) -> match_init pattern ib
	  | _ -> return false
	else return_false (ContextRequired (Ast0.InitTag i))

  and match_designator pattern d =
    match (pattern,d) with
      (Ast0.DesignatorField(dota,ida),Ast0.DesignatorField(dotb,idb)) ->
	conjunct_bindings (check_mcode dota dotb) (match_ident ida idb)
    | (Ast0.DesignatorIndex(lba,expa,rba),
       Ast0.DesignatorIndex(lbb,expb,rbb)) ->
	 conjunct_many_bindings
	   [check_mcode lba lbb; match_expr expa expb;
	     check_mcode rba rbb]
    | (Ast0.DesignatorRange(lba,mina,dotsa,maxa,rba),
       Ast0.DesignatorRange(lbb,minb,dotsb,maxb,rbb)) ->
	 conjunct_many_bindings
	   [check_mcode lba lbb; match_expr mina minb;
	     check_mcode dotsa dotsb; match_expr maxa maxb;
	     check_mcode rba rbb]
    | _ -> return false

  and match_param pattern p =
    match Ast0.unwrap pattern with
      Ast0.MetaParam(name,pure) ->
	add_pure_binding name pure pure_sp_code.VT0.combiner_rec_parameter
	  (function p -> Ast0.ParamTag p)
	  p
    | Ast0.MetaParamList(name,_,pure) -> failwith "metaparamlist not supported"
    | up ->
	if not(checks_needed) || not(context_required) || is_context p
	then
	  match (up,Ast0.unwrap p) with
	    (Ast0.VoidParam(tya),Ast0.VoidParam(tyb)) -> match_typeC tya tyb
	  | (Ast0.Param(tya,ida),Ast0.Param(tyb,idb)) ->
	      conjunct_bindings (match_typeC tya tyb)
		(match_option match_ident ida idb)
	  | (Ast0.PComma(c1),Ast0.PComma(c)) -> check_mcode c1 c
	  | (Ast0.Pdots(d1),Ast0.Pdots(d))
	  | (Ast0.Pcircles(d1),Ast0.Pcircles(d)) -> check_mcode d1 d
	  | (Ast0.OptParam(parama),Ast0.OptParam(paramb))
	  | (Ast0.UniqueParam(parama),Ast0.UniqueParam(paramb)) ->
	      match_param parama paramb
	  | (_,Ast0.OptParam(paramb))
	  | (_,Ast0.UniqueParam(paramb)) -> match_param pattern paramb
	  | _ -> return false
	else return_false (ContextRequired (Ast0.ParamTag p))

  and match_statement pattern s =
    match Ast0.unwrap pattern with
      Ast0.MetaStmt(name,pure) ->
	(match Ast0.unwrap s with
	  Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) ->
	    return false (* ... is not a single statement *)
	| _ ->
	    add_pure_binding name pure pure_sp_code.VT0.combiner_rec_statement
	      (function ty -> Ast0.StmtTag ty)
	      s)
    | Ast0.MetaStmtList(name,pure) -> failwith "metastmtlist not supported"
    | up ->
	if not(checks_needed) || not(context_required) || is_context s
	then
	  match (up,Ast0.unwrap s) with
	    (Ast0.FunDecl(_,fninfoa,namea,lp1,paramsa,vaa,rp1,lb1,bodya,rb1,_),
	     Ast0.FunDecl(_,fninfob,nameb,lp,paramsb,vab,rp,lb,bodyb,rb,_)) ->
	       conjunct_many_bindings
		 [check_mcode lp1 lp; match_option varargs_equal vaa vab;
                   check_mcode rp1 rp;
		   check_mcode lb1 lb; check_mcode rb1 rb;
		   match_fninfo fninfoa fninfob; match_ident namea nameb;
		   match_dots match_param is_plist_matcher do_plist_match
		     paramsa paramsb;
		   match_dots match_statement is_slist_matcher do_slist_match
		     bodya bodyb]
	  | (Ast0.Decl(_,decla),Ast0.Decl(_,declb)) ->
	      match_decl decla declb
	  | (Ast0.Seq(lb1,bodya,rb1),Ast0.Seq(lb,bodyb,rb)) ->
	      (* seqs can only match if they are all minus (plus code
		 allowed) or all context (plus code not allowed in the body).
		 we could be more permissive if the expansions of the isos are
		 also all seqs, but this would be hard to check except at top
		 level, and perhaps not worth checking even in that case.
		 Overall, the issue is that braces are used where single
		 statements are required, and something not satisfying these
		 conditions can cause a single statement to become a
		 non-single statement after the transformation.

		 example: if { ... -foo(); ... }
		 if we let the sequence convert to just -foo();
		 then we produce invalid code.  For some reason,
		 single_statement can't deal with this case, perhaps because
		 it starts introducing too many braces?  don't remember the
		 exact problem...
              *)
	      conjunct_bindings (check_mcode lb1 lb)
		(conjunct_bindings (check_mcode rb1 rb)
		   (if not(checks_needed) || is_minus s ||
		     (is_context s &&
		      List.for_all is_pure_context (Ast0.undots bodyb))
		   then
		     match_dots match_statement is_slist_matcher do_slist_match
		       bodya bodyb
		   else return_false (Braces(s))))
	  | (Ast0.ExprStatement(expa,sc1),Ast0.ExprStatement(expb,sc)) ->
	      conjunct_bindings (check_mcode sc1 sc)
		(match_option match_expr expa expb)
	  | (Ast0.IfThen(if1,lp1,expa,rp1,branch1a,_),
	     Ast0.IfThen(if2,lp2,expb,rp2,branch1b,_)) ->
	       conjunct_many_bindings
		 [check_mcode if1 if2; check_mcode lp1 lp2;
		   check_mcode rp1 rp2;
		   match_expr expa expb;
		   match_statement branch1a branch1b]
	  | (Ast0.IfThenElse(if1,lp1,expa,rp1,branch1a,e1,branch2a,_),
	     Ast0.IfThenElse(if2,lp2,expb,rp2,branch1b,e2,branch2b,_)) ->
	       conjunct_many_bindings
		 [check_mcode if1 if2; check_mcode lp1 lp2;
		   check_mcode rp1 rp2; check_mcode e1 e2;
		   match_expr expa expb;
		   match_statement branch1a branch1b;
		   match_statement branch2a branch2b]
	  | (Ast0.While(w1,lp1,expa,rp1,bodya,_),
	     Ast0.While(w,lp,expb,rp,bodyb,_)) ->
	       conjunct_many_bindings
		 [check_mcode w1 w; check_mcode lp1 lp;
		   check_mcode rp1 rp; match_expr expa expb;
		   match_statement bodya bodyb]
	  | (Ast0.Do(d1,bodya,w1,lp1,expa,rp1,_),
	     Ast0.Do(d,bodyb,w,lp,expb,rp,_)) ->
	       conjunct_many_bindings
		 [check_mcode d1 d; check_mcode w1 w; check_mcode lp1 lp;
		   check_mcode rp1 rp; match_statement bodya bodyb;
		   match_expr expa expb]
	  | (Ast0.For(f1,lp1,firsta,e2a,sc2a,e3a,rp1,bodya,_),
	     Ast0.For(f,lp,firstb,e2b,sc2b,e3b,rp,bodyb,_)) ->
	       let first =
		 match (Ast0.unwrap firsta,Ast0.unwrap firstb) with
		   (Ast0.ForExp(e1a,sc1a),Ast0.ForExp(e1b,sc1b)) ->
		     conjunct_bindings
		       (check_mcode sc2a sc2b)
		       (match_option match_expr e1a e1b)
		 | (Ast0.ForDecl (_,decla),Ast0.ForDecl (_,declb)) ->
		     match_decl decla declb
		 | _ -> return false in
	       conjunct_many_bindings
		 [check_mcode f1 f; check_mcode lp1 lp; first;
		   check_mcode sc2a sc2b; check_mcode rp1 rp;
		   match_option match_expr e2a e2b;
		   match_option match_expr e3a e3b;
		   match_statement bodya bodyb]
	  | (Ast0.Iterator(nma,lp1,argsa,rp1,bodya,_),
	     Ast0.Iterator(nmb,lp,argsb,rp,bodyb,_)) ->
	       conjunct_many_bindings
		 [match_ident nma nmb;
		   check_mcode lp1 lp; check_mcode rp1 rp;
		   match_dots match_expr is_elist_matcher do_elist_match
		     argsa argsb;
		   match_statement bodya bodyb]
	  | (Ast0.Switch(s1,lp1,expa,rp1,lb1,declsa,casesa,rb1),
	     Ast0.Switch(s,lp,expb,rp,lb,declsb,casesb,rb)) ->
	       conjunct_many_bindings
		 [check_mcode s1 s; check_mcode lp1 lp; check_mcode rp1 rp;
		   check_mcode lb1 lb; check_mcode rb1 rb;
		   match_expr expa expb;
		   match_dots match_statement is_slist_matcher do_slist_match
		     declsa declsb;
		   match_dots match_case_line no_list do_nolist_match
		     casesa casesb]
	  | (Ast0.Break(b1,sc1),Ast0.Break(b,sc))
	  | (Ast0.Continue(b1,sc1),Ast0.Continue(b,sc)) ->
	      conjunct_bindings (check_mcode b1 b) (check_mcode sc1 sc)
	  | (Ast0.Label(l1,c1),Ast0.Label(l2,c)) ->
	      conjunct_bindings (match_ident l1 l2) (check_mcode c1 c)
	  | (Ast0.Goto(g1,l1,sc1),Ast0.Goto(g,l2,sc)) ->
	      conjunct_many_bindings
		[check_mcode g1 g; check_mcode sc1 sc; match_ident l1 l2]
	  | (Ast0.Return(r1,sc1),Ast0.Return(r,sc)) ->
	      conjunct_bindings (check_mcode r1 r) (check_mcode sc1 sc)
	  | (Ast0.ReturnExpr(r1,expa,sc1),Ast0.ReturnExpr(r,expb,sc)) ->
	      conjunct_many_bindings
		[check_mcode r1 r; check_mcode sc1 sc; match_expr expa expb]
	  | (Ast0.Exec(e1,l1,codea,sc1),Ast0.Exec(e2,l2,codeb,sc2)) ->
	      failwith "exec not supported in patterns"
	  | (Ast0.Disj(_,statement_dots_lista,_,_),_) ->
	      failwith "disj not supported in patterns"
	  | (Ast0.Nest(_,stmt_dotsa,_,[],multia),
	     Ast0.Nest(_,stmt_dotsb,_,wc,multib)) ->
	       if multia = multib
	       then
		 (match wc with
		   [] ->
		   (* not sure this is correct, perhaps too restrictive *)
		     if not(checks_needed) || is_minus s ||
		       (is_context s &&
			List.for_all is_pure_context (Ast0.undots stmt_dotsb))
		     then
		       match_dots match_statement
			 is_slist_matcher do_slist_match
			 stmt_dotsa stmt_dotsb
		     else return_false (Braces(s))
		 | _ -> return_false (Nest(s)))
	       else return false (* diff kind of nest *)
	  | (Ast0.Nest(_,stmt_dotsa,_,_,_),_) ->
	      failwith "nest with whencode not supported in patterns"
	  | (Ast0.Exp(expa),Ast0.Exp(expb)) -> match_expr expa expb
	  | (Ast0.TopExp(expa),Ast0.TopExp(expb)) -> match_expr expa expb
	  | (Ast0.Exp(expa),Ast0.TopExp(expb)) -> match_expr expa expb
	  | (Ast0.TopInit(inita),Ast0.TopInit(initb)) -> match_init inita initb
	  | (Ast0.Ty(tya),Ast0.Ty(tyb)) -> match_typeC tya tyb
	  | (Ast0.Dots(d,[]),Ast0.Dots(d1,wc))
	  | (Ast0.Circles(d,[]),Ast0.Circles(d1,wc))
	  | (Ast0.Stars(d,[]),Ast0.Stars(d1,wc)) ->
	      (match wc with
		[] -> check_mcode d d1
	      |	_ ->
		  let (_,_,dots_whencode_allowed) = whencode_allowed in
		  if dots_whencode_allowed
		  then
		    conjunct_bindings (check_mcode d d1)
		      (List.fold_left
			 (function prev ->
			   function
			     | Ast0.WhenNot (wh,e,wc) ->
				 conjunct_bindings prev
				 (add_multi_dot_binding d
				 (Ast0.WhenTag(wh,Some e,Ast0.DotsStmtTag wc)))
			     | Ast0.WhenAlways (wh,e,wc) ->
				 conjunct_bindings prev
				 (add_multi_dot_binding d
                                 (Ast0.WhenTag(wh,Some e,Ast0.StmtTag wc)))
			     | Ast0.WhenNotTrue (wh,e,wc) ->
				 conjunct_bindings prev
				 (add_multi_dot_binding d
				 (Ast0.WhenTag(wh,Some e,Ast0.IsoWhenTTag wc)))
			     | Ast0.WhenNotFalse (wh,e,wc) ->
				 conjunct_bindings prev
				 (add_multi_dot_binding d
				 (Ast0.WhenTag(wh,Some e,Ast0.IsoWhenFTag wc)))
			     | Ast0.WhenModifier(wh,x) ->
				 conjunct_bindings prev
				 (add_multi_dot_binding d
				 (Ast0.WhenTag(wh,None,Ast0.IsoWhenTag x))))
			 (return true) wc)
		  else
		    (Printf.printf
		       "warning: not applying iso because of whencode";
		     return false))
	  | (Ast0.Dots(_,_::_),_) | (Ast0.Circles(_,_::_),_)
	  | (Ast0.Stars(_,_::_),_) ->
	      failwith "whencode not allowed in a pattern3"
	  | (Ast0.OptStm(rea),Ast0.OptStm(reb))
	  | (Ast0.UniqueStm(rea),Ast0.UniqueStm(reb)) ->
	      match_statement rea reb
	  | (_,Ast0.OptStm(reb))
	  | (_,Ast0.UniqueStm(reb)) -> match_statement pattern reb
	  |	_ -> return false
	else return_false (ContextRequired (Ast0.StmtTag s))

  (* first should provide a subset of the information in the second *)
  and match_fninfo patterninfo cinfo =
    let patterninfo = List.sort compare patterninfo in
    let cinfo = List.sort compare cinfo in
    let rec loop = function
	(Ast0.FStorage(sta)::resta,Ast0.FStorage(stb)::restb) ->
	  if mcode_equal sta stb
	  then conjunct_bindings (check_mcode sta stb) (loop (resta,restb))
	  else return false
      |	(Ast0.FType(tya)::resta,Ast0.FType(tyb)::restb) ->
	  conjunct_bindings (match_typeC tya tyb) (loop (resta,restb))
      |	(Ast0.FInline(ia)::resta,Ast0.FInline(ib)::restb) ->
	  if mcode_equal ia ib
	  then conjunct_bindings (check_mcode ia ib) (loop (resta,restb))
	  else return false
      |	(Ast0.FAttr(ia)::resta,Ast0.FAttr(ib)::restb) ->
	  if mcode_equal ia ib
	  then conjunct_bindings (check_mcode ia ib) (loop (resta,restb))
	  else return false
      |	(x::resta,((y::_) as restb)) ->
	  (match compare x y with
	    -1 -> return false
	  | 1 -> loop (resta,restb)
	  | _ -> failwith "not possible")
      |	_ -> return false in
    loop (patterninfo,cinfo)

  and match_case_line pattern c =
    if not(checks_needed) || not(context_required) || is_context c
    then
      match (Ast0.unwrap pattern,Ast0.unwrap c) with
	(Ast0.Default(d1,c1,codea),Ast0.Default(d,c,codeb)) ->
	  conjunct_many_bindings
	    [check_mcode d1 d; check_mcode c1 c;
	      match_dots match_statement is_slist_matcher do_slist_match
		codea codeb]
      | (Ast0.Case(ca1,expa,c1,codea),Ast0.Case(ca,expb,c,codeb)) ->
	  conjunct_many_bindings
	    [check_mcode ca1 ca; check_mcode c1 c; match_expr expa expb;
	      match_dots match_statement is_slist_matcher do_slist_match
		codea codeb]
      | (Ast0.DisjCase(_,case_linesa,_,_),_) ->
	  failwith "not allowed in the pattern of an isomorphism"
      |	(Ast0.OptCase(ca),Ast0.OptCase(cb)) -> match_case_line ca cb
      |	(_,Ast0.OptCase(cb)) -> match_case_line pattern cb
      |	_ -> return false
    else return_false (ContextRequired (Ast0.CaseLineTag c)) in

  let match_statement_dots x y =
    match_dots match_statement is_slist_matcher do_slist_match x y in

  (match_expr, match_decl, match_statement, match_typeC,
   match_statement_dots)

let match_expr dochecks context_required whencode_allowed =
  let (fn,_,_,_,_) = match_maker dochecks context_required whencode_allowed in
  fn

let match_decl dochecks context_required whencode_allowed =
  let (_,fn,_,_,_) = match_maker dochecks context_required whencode_allowed in
  fn

let match_statement dochecks context_required whencode_allowed =
  let (_,_,fn,_,_) = match_maker dochecks context_required whencode_allowed in
  fn

let match_typeC dochecks context_required whencode_allowed =
  let (_,_,_,fn,_) = match_maker dochecks context_required whencode_allowed in
  fn

let match_statement_dots dochecks context_required whencode_allowed =
  let (_,_,_,_,fn) = match_maker dochecks context_required whencode_allowed in
  fn

(* --------------------------------------------------------------------- *)
(* make an entire tree MINUS *)

let make_minus =
  let mcode (term,arity,info,mcodekind,pos,adj) =
    let new_mcodekind =
     match mcodekind with
       Ast0.CONTEXT(mc) ->
	 (match !mc with
	   (Ast.NOTHING,_,_) ->
	     Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info))
	 | _ -> failwith "make_minus: unexpected befaft")
     | Ast0.MINUS(mc) -> mcodekind (* in the part copied from the src term *)
     | _ -> failwith "make_minus mcode: unexpected mcodekind" in
    (term,arity,info,new_mcodekind,pos,adj) in

  let update_mc mcodekind e =
    match !mcodekind with
      Ast0.CONTEXT(mc) ->
	(match !mc with
	  (Ast.NOTHING,_,_) ->
	    mcodekind :=
	      Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info))
	| _ -> failwith "make_minus: unexpected befaft")
    | Ast0.MINUS(_mc) -> () (* in the part copied from the src term *)
    | Ast0.PLUS _ -> failwith "make_minus donothing: unexpected plus mcodekind"
    | _ -> failwith "make_minus donothing: unexpected mcodekind" in

  let donothing r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    let e = k e in update_mc mcodekind e; e in

  (* special case for whencode, because it isn't processed by contextneg,
     since it doesn't appear in the + code *)
  (* cases for dots and nests *)
  let expression r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Edots(d,whencode) ->
       (*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Edots(mcode d,whencode))
    | Ast0.Ecircles(d,whencode) ->
       (*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Ecircles(mcode d,whencode))
    | Ast0.Estars(d,whencode) ->
       (*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Estars(mcode d,whencode))
    | Ast0.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	update_mc mcodekind e;
	Ast0.rewrap e
	  (Ast0.NestExpr(mcode starter,
			 r.VT0.rebuilder_rec_expression_dots expr_dots,
			 mcode ender,whencode,multi))
    | _ -> donothing r k e in

  let declaration r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Ddots(d,whencode) ->
       (*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Ddots(mcode d,whencode))
    | _ -> donothing r k e in

  let statement r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Dots(d,whencode) ->
       (*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Dots(mcode d,whencode))
    | Ast0.Circles(d,whencode) ->
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Circles(mcode d,whencode))
    | Ast0.Stars(d,whencode) ->
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Stars(mcode d,whencode))
    | Ast0.Nest(starter,stmt_dots,ender,whencode,multi) ->
	update_mc mcodekind e;
	Ast0.rewrap e
	  (Ast0.Nest
	     (mcode starter,r.VT0.rebuilder_rec_statement_dots stmt_dots,
	      mcode ender,whencode,multi))
    | _ -> donothing r k e in

  let initialiser r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Idots(d,whencode) ->
       (*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind e; Ast0.rewrap e (Ast0.Idots(mcode d,whencode))
    | _ -> donothing r k e in

  let dots r k e =
    let info = Ast0.get_info e in
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.DOTS([]) ->
	(* if context is - this should be - as well.  There are no tokens
	   here though, so the bottom-up minusifier in context_neg leaves it
	   as mixed (or context for sgrep2).  It would be better to fix
	   context_neg, but that would
	   require a special case for each term with a dots subterm. *)
	(match !mcodekind with
	  Ast0.MIXED(mc) | Ast0.CONTEXT(mc) ->
	    (match !mc with
	      (Ast.NOTHING,_,_) ->
		mcodekind :=
		  Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info));
		e
	    | _ -> failwith "make_minus: unexpected befaft")
	  (* code already processed by an enclosing iso *)
	| Ast0.MINUS(mc) -> e
	| _ ->
	    failwith
	      (Printf.sprintf
		 "%d: make_minus donothingxxx: unexpected mcodekind: %s"
		 info.Ast0.pos_info.Ast0.line_start (Dumper.dump e)))
    | _ -> donothing r k e in

  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    dots dots dots dots dots dots
    donothing expression donothing donothing donothing initialiser donothing declaration
    statement donothing donothing donothing donothing

(* --------------------------------------------------------------------- *)
(* rebuild mcode cells in an instantiated alt *)

(* mcodes will be side effected later with plus code, so we have to copy
   them on instantiating an isomorphism.  One could wonder whether it would
   be better not to use side-effects, but they are convenient for insert_plus
   where is it useful to manipulate a list of the mcodes but side-effect a
   tree *)
(* hmm... Insert_plus is called before Iso_pattern... *)
let rebuild_mcode start_line =
  let copy_mcodekind = function
      Ast0.CONTEXT(mc) -> Ast0.CONTEXT(ref (!mc))
    | Ast0.MINUS(mc) ->   Ast0.MINUS(ref (!mc))
    | Ast0.MIXED(mc) ->   Ast0.MIXED(ref (!mc))
    | Ast0.PLUS count ->
	(* this function is used elsewhere where we need to rebuild the
	   indices, and so we allow PLUS code as well *)
	Ast0.PLUS count in

  let mcode (term,arity,info,mcodekind,pos,adj) =
    let info =
      match start_line with
	Some x ->
	  let new_pos_info =
	    {info.Ast0.pos_info with
	      Ast0.line_start = x;
	      Ast0.line_end = x; } in
	  {info with Ast0.pos_info = new_pos_info}
      |	None -> info in
    (term,arity,info,copy_mcodekind mcodekind,pos,adj) in

  let copy_one x =
    let old_info = Ast0.get_info x in
    let info =
      match start_line with
	Some x ->
	  let new_pos_info =
	    {old_info.Ast0.pos_info with
	      Ast0.line_start = x;
	      Ast0.line_end = x; } in
	  {old_info with Ast0.pos_info = new_pos_info}
      |	None -> old_info in
    {x with Ast0.info = info; Ast0.index = ref(Ast0.get_index x);
      Ast0.mcodekind = ref (copy_mcodekind (Ast0.get_mcodekind x))} in

  let donothing r k e = copy_one (k e) in

  (* case for control operators (if, etc) *)
  let statement r k e =
    let s = k e in
    let res =
      copy_one
	(Ast0.rewrap s
	   (match Ast0.unwrap s with
	     Ast0.Decl((info,mc),decl) ->
	       Ast0.Decl((info,copy_mcodekind mc),decl)
	   | Ast0.IfThen(iff,lp,tst,rp,branch,(info,mc,adj)) ->
	       Ast0.IfThen(iff,lp,tst,rp,branch,(info,copy_mcodekind mc,adj))
	   | Ast0.IfThenElse(iff,lp,tst,rp,branch1,els,branch2,(info,mc,adj))->
	       Ast0.IfThenElse(iff,lp,tst,rp,branch1,els,branch2,
		 (info,copy_mcodekind mc,adj))
	   | Ast0.While(whl,lp,exp,rp,body,(info,mc,adj)) ->
	       Ast0.While(whl,lp,exp,rp,body,(info,copy_mcodekind mc,adj))
	   | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,(info,mc,adj)) ->
	       Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,
			(info,copy_mcodekind mc,adj))
	   | Ast0.Iterator(nm,lp,args,rp,body,(info,mc,adj)) ->
	       Ast0.Iterator(nm,lp,args,rp,body,(info,copy_mcodekind mc,adj))
	   | Ast0.FunDecl
	       ((info,mc),fninfo,name,lp,params,va,rp,lbrace,body,rbrace,
		(aftinfo,aftmc)) ->
		 Ast0.FunDecl
		   ((info,copy_mcodekind mc),
		    fninfo,name,lp,params,va,rp,lbrace,body,rbrace,
		    (aftinfo,copy_mcodekind aftmc))
	   | s -> s)) in
    Ast0.set_dots_bef_aft res
      (match Ast0.get_dots_bef_aft res with
	Ast0.NoDots -> Ast0.NoDots
      | Ast0.AddingBetweenDots s ->
	  Ast0.AddingBetweenDots(r.VT0.rebuilder_rec_statement s)
      | Ast0.DroppingBetweenDots s ->
	  Ast0.DroppingBetweenDots(r.VT0.rebuilder_rec_statement s)) in

  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing statement donothing donothing donothing donothing

(* --------------------------------------------------------------------- *)
(* The problem of whencode.  If an isomorphism contains dots in multiple
   rules, then the code that is matched cannot contain whencode, because we
   won't know which dots it goes with. Should worry about nests, but they
   aren't allowed in isomorphisms for the moment. *)

let count_edots =
  let option_default = 0 in
  let bind x y = x + y in
  let exprfn r k e =
    match Ast0.unwrap e with
      Ast0.Edots(_,_) | Ast0.Ecircles(_,_) | Ast0.Estars(_,_) -> 1
    | _ -> 0 in

  V0.combiner bind option_default
    {V0.combiner_functions with VT0.combiner_exprfn = exprfn}

let count_idots =
  let option_default = 0 in
  let bind x y = x + y in
  let initfn r k e =
    match Ast0.unwrap e with Ast0.Idots(_,_) -> 1 | _ -> 0 in

  V0.combiner bind option_default
    {V0.combiner_functions with VT0.combiner_initfn = initfn}

let count_dots =
  let option_default = 0 in
  let bind x y = x + y in
  let stmtfn r k e =
    match Ast0.unwrap e with
      Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> 1
    | _ -> 0 in

  V0.combiner bind option_default
    {V0.combiner_functions with VT0.combiner_stmtfn = stmtfn}

(* --------------------------------------------------------------------- *)

let lookup name bindings mv_bindings =
  try Common.Left (List.assoc (term name) bindings)
  with
    Not_found ->
      (* failure is not possible anymore *)
      Common.Right (List.assoc (term name) mv_bindings)

(* mv_bindings is for the fresh metavariables that are introduced by the
   isomorphism.  Model to know whether new code will be -. *)
let instantiate bindings mv_bindings model =
  let mcode x =
    let (hidden,others) =
      List.partition
	(function Ast0.HiddenVarTag _ -> true | _ -> false)
	(Ast0.get_pos x) in
    let new_names =
      match hidden with
	[Ast0.HiddenVarTag([Ast0.MetaPosTag(Ast0.MetaPos (name,_,_))])] ->
	  (try
	  (* not at all sure that this is good enough *)
	    match lookup name bindings mv_bindings with
	      Common.Left(Ast0.HiddenVarTag(ids)) -> ids
	    | _ -> failwith "not possible"
	  with Not_found ->
	     (*can't fail because checks_needed could be false?*)
	    [])
      |	[] -> [] (* no hidden metavars allowed *)
      | _ -> failwith "badly compiled mcode" in
    Ast0.set_pos (new_names@others) x in
  let donothing r k e = k e in

  (* cases where metavariables can occur *)
  let identfn r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaId(name,constraints,seed,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_ident
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.IdentTag(id)) -> id
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaId
		   (Ast0.set_mcode_data new_mv name,constraints,seed,pure)))
    | Ast0.MetaFunc(name,_,pure) -> failwith "metafunc not supported"
    | Ast0.MetaLocalFunc(name,_,pure) -> failwith "metalocalfunc not supported"
    | _ -> e in

  (* case for list metavariables *)
  let rec elist r same_dots = function
      [] -> []
    | [x] ->
	(match Ast0.unwrap x with
	  Ast0.MetaExprList(name,lenname,pure) ->
	    failwith "meta_expr_list in iso not supported"
	    (*match lookup name bindings mv_bindings with
	      Common.Left(Ast0.DotsExprTag(exp)) ->
		(match same_dots exp with
		  Some l -> l
		| None -> failwith "dots put in incompatible context")
	    | Common.Left(Ast0.ExprTag(exp)) -> [exp]
	    | Common.Left(_) -> failwith "not possible 1"
	    | Common.Right(new_mv) ->
		failwith "MetaExprList in SP not supported"*)
	| _ -> [r.VT0.rebuilder_rec_expression x])
    | x::xs -> (r.VT0.rebuilder_rec_expression x)::(elist r same_dots xs) in

  let rec plist r same_dots = function
      [] -> []
    | [x] ->
	(match Ast0.unwrap x with
	  Ast0.MetaParamList(name,lenname,pure) ->
	    failwith "meta_param_list in iso not supported"
	    (*match lookup name bindings mv_bindings with
		Common.Left(Ast0.DotsParamTag(param)) ->
		(match same_dots param with
		Some l -> l
		| None -> failwith "dots put in incompatible context")
		| Common.Left(Ast0.ParamTag(param)) -> [param]
		| Common.Left(_) -> failwith "not possible 1"
		| Common.Right(new_mv) ->
		failwith "MetaExprList in SP not supported"*)
	| _ -> [r.VT0.rebuilder_rec_parameter x])
    | x::xs -> (r.VT0.rebuilder_rec_parameter x)::(plist r same_dots xs) in

  let rec slist r same_dots = function
      [] -> []
    | [x] ->
	(match Ast0.unwrap x with
	  Ast0.MetaStmtList(name,pure) ->
	    (match lookup name bindings mv_bindings with
	      Common.Left(Ast0.DotsStmtTag(stm)) ->
		(match same_dots stm with
		  Some l -> l
		| None -> failwith "dots put in incompatible context")
	    | Common.Left(Ast0.StmtTag(stm)) -> [stm]
	    | Common.Left(_) -> failwith "not possible 1"
	    | Common.Right(new_mv) ->
		failwith "MetaExprList in SP not supported")
	| _ -> [r.VT0.rebuilder_rec_statement x])
    | x::xs -> (r.VT0.rebuilder_rec_statement x)::(slist r same_dots xs) in

  let same_dots d =
    match Ast0.unwrap d with Ast0.DOTS(l) -> Some l |_ -> None in
  let same_circles d =
    match Ast0.unwrap d with Ast0.CIRCLES(l) -> Some l |_ -> None in
  let same_stars d =
    match Ast0.unwrap d with Ast0.STARS(l) -> Some l |_ -> None in

  let dots list_fn r k d =
    Ast0.rewrap d
      (match Ast0.unwrap d with
	Ast0.DOTS(l) -> Ast0.DOTS(list_fn r same_dots l)
      | Ast0.CIRCLES(l) -> Ast0.CIRCLES(list_fn r same_circles l)
      | Ast0.STARS(l) -> Ast0.STARS(list_fn r same_stars l)) in

  let exprfn r k old_e = (* need to keep the original code for ! optim *)
    let e = k old_e in
    let e1 =
    match Ast0.unwrap e with
      Ast0.MetaExpr(name,constraints,x,form,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_expression
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.ExprTag(exp)) -> Ast0.clear_test_exp exp
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      let new_types =
		match x with
		  None -> None
		| Some types ->
		    let rec renamer = function
			Type_cocci.MetaType(name,keep,inherited) ->
			  (match
			    lookup (name,(),(),(),None,-1)
			      bindings mv_bindings
			  with
			    Common.Left(Ast0.TypeCTag(t)) ->
			      Ast0.ast0_type_to_type true t
			  | Common.Left(_) ->
			      failwith "iso pattern: unexpected type"
			  | Common.Right(new_mv) ->
			      Type_cocci.MetaType(new_mv,keep,inherited))
		      |	Type_cocci.ConstVol(cv,ty) ->
			  Type_cocci.ConstVol(cv,renamer ty)
		      | Type_cocci.Pointer(ty) ->
			  Type_cocci.Pointer(renamer ty)
		      | Type_cocci.FunctionPointer(ty) ->
			  Type_cocci.FunctionPointer(renamer ty)
		      | Type_cocci.Array(ty) ->
			  Type_cocci.Array(renamer ty)
		      | t -> t in
		    Some(List.map renamer types) in
	      Ast0.clear_test_exp
		(Ast0.rewrap e
		   (Ast0.MetaExpr
		      (Ast0.set_mcode_data new_mv name,constraints,
		       new_types,form,pure))))
    | Ast0.MetaErr(namea,_,pure) -> failwith "metaerr not supported"
    | Ast0.MetaExprList(namea,lenname,pure) ->
	failwith "metaexprlist not supported"
    | Ast0.Unary(exp,unop) ->
	(match Ast0.unwrap_mcode unop with
	  (* propagate negation only when the propagated and the encountered
	     negation have the same transformation, when there is nothing
	     added to the original one, and when there is nothing added to
	     the expression into which we are doing the propagation.  This
	     may be too conservative. *)
	  Ast.Not ->
	    let was_meta =
	      (* k e doesn't change the outer structure of the term,
		 only the metavars *)
	      match Ast0.unwrap old_e with
		Ast0.Unary(exp,_) ->
		  (match Ast0.unwrap exp with
		    Ast0.MetaExpr(name,constraints,x,form,pure) -> true
		  | _ -> false)
	      |	_ -> failwith "not possible" in
	    let nomodif = function
		Ast0.MINUS(x) ->
		  (match !x with
		    (Ast.NOREPLACEMENT,_) -> true
		  | _ -> false)
	      |	Ast0.CONTEXT(x) | Ast0.MIXED(x) ->
		  (match !x with
		    (Ast.NOTHING,_,_) -> true
		  | _ -> false)
	      |	_ -> failwith "plus not possible" in
	    let same_modif oldop =
	      (* only propagate ! if they, ie model and oldop, have the
		 same modification
		 and no + code on the old one (the new one from the iso
		 surely has no + code) *)
	      (* This is not ideal, because the model is the top level
		 term, and this is a subterm, so minus minus is only detected
		 when the whole matched term is minus.  Not sure what to do
		 in general, because not clear where the ! comes from.
		 Be more flexible for !, because it doesn't really matter *)
	      !Flag.sgrep_mode2 ||
	      match (Ast0.get_mcodekind model,oldop) with
		(Ast0.MINUS(x1),Ast0.MINUS(x2)) -> nomodif oldop
	      | (Ast0.CONTEXT(x1),Ast0.CONTEXT(x2)) -> nomodif oldop
	      | (Ast0.MIXED(x1),Ast0.MIXED(x2)) -> nomodif oldop
	      | _ -> false in
	    if was_meta
	    then
	      let idcont x = x in
              let get_binaryOp_mcodekind op =
                match Ast0.unwrap op with
                  Ast0.Logical o -> Ast0.get_mcode_mcodekind o
                | Ast0.Arith o -> Ast0.get_mcode_mcodekind o
                | Ast0.MetaBinary(mv,_,_) -> Ast0.get_mcode_mcodekind mv in
	      let get_op op = Ast0.unwrap_mcode op in
	      let rec negate e (*for rewrapping*) res (*code to process*) k =
		(* k accumulates parens, to keep negation outside if no
		   propagation is possible *)
		if nomodif (Ast0.get_mcodekind e)
		then
		  match Ast0.unwrap res with
		    Ast0.Unary(e1,op) when get_op op = Ast.Not &&
		      same_modif (Ast0.get_mcode_mcodekind op) ->
			  k e1
		  | Ast0.Edots(_,_) -> k (Ast0.rewrap e (Ast0.unwrap res))
		  | Ast0.Paren(lp,e1,rp) ->
		      negate e e1
			(function x ->
			  k (Ast0.rewrap res (Ast0.Paren(lp,x,rp))))
		  | Ast0.Binary(e1,op,e2) when
		      let v = same_modif (get_binaryOp_mcodekind op) in
		      v ->
			  let reb model nop =
			    let nop = Ast0.rewrap_mcode model nop in
			    Ast0.rewrap op (Ast0.Logical nop) in
			  let k1 x = k (Ast0.rewrap e x) in
			  (match Ast0.unwrap op with
			    Ast0.Logical op' when get_op op' = Ast.Inf ->
			      k1 (Ast0.Binary(e1,reb op' Ast.SupEq,e2))
			  | Ast0.Logical op' when get_op op' = Ast.Sup ->
			      k1 (Ast0.Binary(e1,reb op' Ast.InfEq,e2))
			  | Ast0.Logical op' when get_op op' = Ast.InfEq ->
			      k1 (Ast0.Binary(e1,reb op' Ast.Sup,e2))
			  | Ast0.Logical op' when get_op op' = Ast.SupEq ->
			      k1 (Ast0.Binary(e1,reb op' Ast.Inf,e2))
			  | Ast0.Logical op' when get_op op' = Ast.Eq ->
			      k1 (Ast0.Binary(e1,reb op' Ast.NotEq,e2))
			  | Ast0.Logical op' when get_op op' = Ast.NotEq ->
			      k1 (Ast0.Binary(e1,reb op' Ast.Eq,e2))
			  | Ast0.Logical op' when get_op op' = Ast.AndLog ->
			      k1 (Ast0.Binary(negate_reb e e1 idcont,
					      reb op' Ast.OrLog,
					      negate_reb e e2 idcont))
			  | Ast0.Logical op' when get_op op' = Ast.OrLog ->
			      k1 (Ast0.Binary(negate_reb e e1 idcont,
					      reb op' Ast.AndLog,
					      negate_reb e e2 idcont))
			  | _ ->
                             let rewrap_binaryOp_mcode op x =
                               match Ast0.unwrap op with
                                 Ast0.Arith o -> Ast0.rewrap_mcode o x
                               | Ast0.Logical o -> Ast0.rewrap_mcode o x
                               | Ast0.MetaBinary (mv,_,_) ->
				   Ast0.rewrap_mcode mv x in
			      Ast0.rewrap e
				(Ast0.Unary(k res,
					    rewrap_binaryOp_mcode op Ast.Not)))
		  | Ast0.DisjExpr(lp,exps,mids,rp) ->
		      (* use res because it is the transformed argument *)
		      let exps =
			List.map (function e1 -> negate_reb e e1 k) exps in
		      Ast0.rewrap res (Ast0.DisjExpr(lp,exps,mids,rp))
		  | _ ->
		      (*use e, because this might be the toplevel expression*)
		      Ast0.rewrap e
			(Ast0.Unary(k res,Ast0.rewrap_mcode unop Ast.Not))
		else
		  Ast0.rewrap e
		    (Ast0.Unary(k res,Ast0.rewrap_mcode unop Ast.Not))
	      and negate_reb e e1 k =
		(* used when ! is propagated to multiple places, to avoid
		   duplicating mcode cells *)
		let start_line =
		  Some ((Ast0.get_info e).Ast0.pos_info.Ast0.line_start) in
		(rebuild_mcode start_line).VT0.rebuilder_rec_expression
		  (negate e e1 k) in
	      negate e exp idcont
	    else e
	| _ -> e)
    | Ast0.Edots(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.WhenTag(wh,Some ee,Ast0.ExprTag exp) ->
              Ast0.rewrap e (Ast0.Edots(d,Some (wh,ee,exp)))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Ecircles(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.WhenTag(wh,Some ee,Ast0.ExprTag exp) ->
              Ast0.rewrap e (Ast0.Ecircles(d,Some (wh,ee,exp)))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Estars(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.WhenTag(wh,Some ee,Ast0.ExprTag(exp)) ->
              Ast0.rewrap e (Ast0.Estars(d,Some (wh,ee,exp)))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | _ -> e in
    if Ast0.get_test_exp old_e then Ast0.set_test_exp e1 else e1 in

  let tyfn r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaType(name,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_typeC
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.TypeCTag(ty)) -> ty
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaType(Ast0.set_mcode_data new_mv name,pure)))
    | _ -> e in

  let initfn r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaInit(name,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_initialiser
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.InitTag(ty)) -> ty
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaInit(Ast0.set_mcode_data new_mv name,pure)))
    | _ -> e in

  let declfn r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaDecl(name,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_declaration
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.DeclTag(d)) -> d
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaDecl(Ast0.set_mcode_data new_mv name, pure)))
    | Ast0.MetaField(name,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_declaration
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.DeclTag(d)) -> d
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaField(Ast0.set_mcode_data new_mv name, pure)))
    | Ast0.MetaFieldList(name,lenname,pure) ->
	failwith "metafieldlist not supported"
    | Ast0.Ddots(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.WhenTag(wh,Some ee,Ast0.DeclTag(exp)) ->
              Ast0.rewrap e (Ast0.Ddots(d,Some (wh,ee,exp)))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | _ -> e in

  let paramfn r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaParam(name,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_parameter
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.ParamTag(param)) -> param
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaParam(Ast0.set_mcode_data new_mv name, pure)))
    | Ast0.MetaParamList(name,lenname,pure) ->
	failwith "metaparamlist not supported"
    | _ -> e in

  let whenfn (_,v) =
    match v with
      Ast0.WhenTag(wh,Some ee,Ast0.DotsStmtTag(stms)) ->
	Ast0.WhenNot (wh,ee,stms)
    | Ast0.WhenTag(wh,Some ee,Ast0.StmtTag(stm)) ->
	Ast0.WhenAlways (wh,ee,stm)
    | Ast0.WhenTag(wh,Some ee,Ast0.IsoWhenTTag(stm)) ->
	Ast0.WhenNotTrue (wh,ee,stm)
    | Ast0.WhenTag(wh,Some ee,Ast0.IsoWhenFTag(stm)) ->
	Ast0.WhenNotFalse (wh,ee,stm)
    | Ast0.WhenTag(wh,None,Ast0.IsoWhenTag(x)) -> Ast0.WhenModifier(wh,x)
    | _ -> failwith "unexpected binding" in

  let stmtfn r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaStmt(name,pure) ->
	(rebuild_mcode None).VT0.rebuilder_rec_statement
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.StmtTag(stm)) -> stm
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaStmt(Ast0.set_mcode_data new_mv name,pure)))
    | Ast0.MetaStmtList(name,pure) -> failwith "metastmtlist not supported"
    | Ast0.Dots(d,_) ->
	Ast0.rewrap e
	  (Ast0.Dots
	     (d,
	      List.map whenfn
		(List.filter (function (x,v) -> x = (dot_term d)) bindings)))
    | Ast0.Circles(d,_) ->
	Ast0.rewrap e
	  (Ast0.Circles
	     (d,
	      List.map whenfn
		(List.filter (function (x,v) -> x = (dot_term d)) bindings)))
    | Ast0.Stars(d,_) ->
	Ast0.rewrap e
	  (Ast0.Stars
	     (d,
	      List.map whenfn
		(List.filter (function (x,v) -> x = (dot_term d)) bindings)))
    | _ -> e in

  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    (dots elist) donothing (dots plist) (dots slist) donothing donothing
    identfn exprfn donothing donothing tyfn initfn paramfn declfn stmtfn donothing donothing
    donothing donothing

(* --------------------------------------------------------------------- *)

let is_minus e =
  match Ast0.get_mcodekind e with Ast0.MINUS(cell) -> true | _ -> false

let context_required e = not(is_minus e) && not !Flag.sgrep_mode2

let disj_fail bindings e =
  match bindings with
    Some x -> Printf.fprintf stderr "no disj available at this type"; e
  | None -> e

(* isomorphism code is by default CONTEXT *)
let merge_plus model_mcode e_mcode =
  match model_mcode with
    Ast0.MINUS(mc) ->
      (* add the replacement information at the root *)
      (match e_mcode with
        Ast0.MINUS(emc) ->
          emc :=
            (match (!mc,!emc) with
              ((Ast.NOREPLACEMENT,_),(x,t))
            |  ((x,_),(Ast.NOREPLACEMENT,t)) -> (x,t)
            |  _ -> failwith "how can we combine minuses?")
      |	 _ -> failwith "not possible 6")
  | Ast0.CONTEXT(mc) ->
      (match e_mcode with
        Ast0.CONTEXT(emc) | Ast0.MIXED(emc) ->
          (* keep the logical line info as in the model *)
          let (mba,tb,ta) = !mc in
          let (eba,_,_) = !emc in
          (* merging may be required when a term is replaced by a subterm *)
          let merged =
            match (mba,eba) with
              (x,Ast.NOTHING) | (Ast.NOTHING,x) -> x
            | (Ast.BEFORE(b1,it1),Ast.BEFORE(b2,it2)) ->
                Ast.BEFORE(b1@b2,Ast.lub_count it1 it2)
            | (Ast.BEFORE(b,it1),Ast.AFTER(a,it2)) ->
                Ast.BEFOREAFTER(b,a,Ast.lub_count it1 it2)
            | (Ast.BEFORE(b1,it1),Ast.BEFOREAFTER(b2,a,it2)) ->
                Ast.BEFOREAFTER(b1@b2,a,Ast.lub_count it1 it2)
            | (Ast.AFTER(a,it1),Ast.BEFORE(b,it2)) ->
                Ast.BEFOREAFTER(b,a,Ast.lub_count it1 it2)
            | (Ast.AFTER(a1,it1),Ast.AFTER(a2,it2)) ->
                Ast.AFTER(a2@a1,Ast.lub_count it1 it2)
            | (Ast.AFTER(a1,it1),Ast.BEFOREAFTER(b,a2,it2)) ->
                Ast.BEFOREAFTER(b,a2@a1,Ast.lub_count it1 it2)
            | (Ast.BEFOREAFTER(b1,a,it1),Ast.BEFORE(b2,it2)) ->
                Ast.BEFOREAFTER(b1@b2,a,Ast.lub_count it1 it2)
            | (Ast.BEFOREAFTER(b,a1,it1),Ast.AFTER(a2,it2)) ->
                Ast.BEFOREAFTER(b,a2@a1,Ast.lub_count it1 it2)
            | (Ast.BEFOREAFTER(b1,a1,it1),Ast.BEFOREAFTER(b2,a2,it2)) ->
                Ast.BEFOREAFTER(b1@b2,a2@a1,Ast.lub_count it1 it2) in
          emc := (merged,tb,ta)
      |	Ast0.MINUS(emc) ->
          let (anything_bef_aft,_,_) = !mc in
          let (anythings,t) = !emc in
          (match (anything_bef_aft,anythings) with
            (Ast.BEFORE(b1,it1),Ast.NOREPLACEMENT) ->
              emc := (Ast.REPLACEMENT(b1,it1),t)
          | (Ast.AFTER(a1,it1),Ast.NOREPLACEMENT) ->
              emc := (Ast.REPLACEMENT(a1,it1),t)
          | (Ast.BEFOREAFTER(b1,a1,it1),Ast.NOREPLACEMENT) ->
              emc := (Ast.REPLACEMENT(b1@a1,it1),t)
          | (Ast.NOTHING,Ast.NOREPLACEMENT) ->
              emc := (Ast.NOREPLACEMENT,t)
          | (Ast.BEFORE(b1,it1),Ast.REPLACEMENT(a2,it2)) ->
              emc := (Ast.REPLACEMENT(b1@a2,Ast.lub_count it1 it2),t)
          | (Ast.AFTER(a1,it1),Ast.REPLACEMENT(a2,it2)) ->
              emc := (Ast.REPLACEMENT(a2@a1,Ast.lub_count it1 it2),t)
          | (Ast.BEFOREAFTER(b1,a1,it1),Ast.REPLACEMENT(a2,it2)) ->
              emc := (Ast.REPLACEMENT(b1@a2@a1,Ast.lub_count it1 it2),t)
          | (Ast.NOTHING,Ast.REPLACEMENT(a2,it2)) -> ()) (* no change *)
      (* Allowed because of possibility of metavar as model of macro
	 | Ast0.MIXED(_) -> failwith "how did this become mixed?" *)
      |	 _ -> failwith "not possible 7")
  | Ast0.MIXED(_) -> failwith "not possible 8"
  | Ast0.PLUS _ -> failwith "not possible 9"

let merge_plus_before model_mcode e_mcode =
  match model_mcode with
    Ast0.MINUS(mc) -> merge_plus model_mcode e_mcode
  | Ast0.CONTEXT(mc) ->
      let (mba,tb,ta) = !mc in
      (match mba with
	Ast.NOTHING | Ast.BEFORE _ -> merge_plus model_mcode e_mcode
      | Ast.AFTER(a,it1) ->
	  failwith "before cell should only contain before modifications"
      | Ast.BEFOREAFTER(b1,a,it1) ->
	  failwith "before cell should only contain before modifications")
  | Ast0.MIXED(_) -> failwith "not possible 8"
  | Ast0.PLUS _ -> failwith "not possible 9"

let merge_plus_after model_mcode e_mcode =
  match model_mcode with
    Ast0.MINUS(mc) -> merge_plus model_mcode e_mcode
  | Ast0.CONTEXT(mc) ->
      let (mba,tb,ta) = !mc in
      (match mba with
	Ast.NOTHING | Ast.AFTER _ ->  merge_plus model_mcode e_mcode
      | Ast.BEFORE(b1,it1) ->
	  failwith "after cell should only contain before modifications"
      | Ast.BEFOREAFTER(b1,a,it1) ->
	  failwith "after cell should only contain before modifications")
  | Ast0.MIXED(_) -> failwith "not possible 8"
  | Ast0.PLUS _ -> failwith "not possible 9"

let copy_plus printer minusify model e =
  if !Flag.sgrep_mode2
  then e (* no plus code, can cause a "not possible" error, so just avoid it *)
  else
    begin
      let e =
	match Ast0.get_mcodekind model with
          Ast0.MINUS(mc) -> minusify e
	| Ast0.CONTEXT(mc) -> e
	| _ -> failwith "not possible: copy_plus\n" in
      merge_plus (Ast0.get_mcodekind model) (Ast0.get_mcodekind e);
      e
    end

let copy_minus printer minusify model e =
  match Ast0.get_mcodekind model with
    Ast0.MINUS(mc) -> minusify e
  | Ast0.CONTEXT(mc) -> e
  | Ast0.MIXED(_) ->
      (* This is possible if the model of an isomorphism is a single
	 metavariable, and this metavariable matches mixed code.
	 Previously, this failed with impossible if not in sgrep mode. *)
      e
  | Ast0.PLUS _ -> failwith "not possible 9"

let whencode_allowed prev_ecount prev_icount prev_dcount
    ecount icount dcount rest =
  (* actually, if ecount or dcount is 0, the flag doesn't matter, because it
     won't be tested *)
  let other_ecount = (* number of edots *)
    List.fold_left (function rest -> function (_,ec,ic,dc) -> ec + rest)
      prev_ecount rest in
  let other_icount = (* number of dots *)
    List.fold_left (function rest -> function (_,ec,ic,dc) -> ic + rest)
      prev_icount rest in
  let other_dcount = (* number of dots *)
    List.fold_left (function rest -> function (_,ec,ic,dc) -> dc + rest)
      prev_dcount rest in
  (ecount = 0 || other_ecount = 0, icount = 0 || other_icount = 0,
   dcount = 0 || other_dcount = 0)

(* copy the befores and afters to the instantiated code *)
let extra_copy_stmt_plus model e =
  (if not !Flag.sgrep_mode2 (* sgrep has no plus code, so nothing to do *)
  then
    (match Ast0.unwrap model with
      Ast0.FunDecl((info,bef),_,_,_,_,_,_,_,_,_,(aftinfo,aft)) ->
	(match Ast0.unwrap e with
	  Ast0.FunDecl((info,bef1),_,_,_,_,_,_,_,_,_,(aftinfo,aft1)) ->
	    merge_plus_before bef bef1; merge_plus_after aft aft1
	| _ ->
	    let mc = Ast0.get_mcodekind e in
	    merge_plus_before bef mc;
	    merge_plus_after aft mc)
    | Ast0.Decl((info,bef),_) ->
	(match Ast0.unwrap e with
	  Ast0.Decl((info,bef1),_) ->
	    merge_plus_before bef bef1
	| _ ->  merge_plus_before bef (Ast0.get_mcodekind e))
    | Ast0.IfThen(_,_,_,_,_,(_,aft,_))
    | Ast0.IfThenElse(_,_,_,_,_,_,_,(_,aft,_))
    | Ast0.While(_,_,_,_,_,(_,aft,_))
    | Ast0.For(_,_,_,_,_,_,_,_,(_,aft,_))
    | Ast0.Iterator(_,_,_,_,_,(_,aft,_)) ->
	(match Ast0.unwrap e with
	  Ast0.IfThen(_,_,_,_,_,(_,aft1,_))
	| Ast0.IfThenElse(_,_,_,_,_,_,_,(_,aft1,_))
	| Ast0.While(_,_,_,_,_,(_,aft1,_))
	| Ast0.For(_,_,_,_,_,_,_,_,(_,aft1,_))
	| Ast0.Iterator(_,_,_,_,_,(_,aft1,_)) ->
	    merge_plus_after aft aft1
	| _ -> merge_plus_after aft (Ast0.get_mcodekind e))
    | _ -> ()));
  e

let extra_copy_other_plus model e = e

(* --------------------------------------------------------------------- *)

let mv_count = ref 0
let new_mv (_,s) =
  let ct = !mv_count in
  mv_count := !mv_count + 1;
  "_"^s^"_"^(string_of_int ct)

let get_name = function
    Ast.MetaMetaDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaMetaDecl(ar,nm))
  | Ast.MetaIdDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaIdDecl(ar,nm))
  | Ast.MetaFreshIdDecl(nm,seed) ->
      (nm,function nm -> Ast.MetaFreshIdDecl(nm,seed))
  | Ast.MetaTypeDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaTypeDecl(ar,nm))
  | Ast.MetaInitDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaInitDecl(ar,nm))
  | Ast.MetaInitListDecl(ar,nm,nm1) ->
      (nm,function nm -> Ast.MetaInitListDecl(ar,nm,nm1))
  | Ast.MetaListlenDecl(nm) ->
      failwith "should not be rebuilt"
  | Ast.MetaParamDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaParamDecl(ar,nm))
  | Ast.MetaParamListDecl(ar,nm,nm1) ->
      (nm,function nm -> Ast.MetaParamListDecl(ar,nm,nm1))
  | Ast.MetaBinaryOperatorDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaBinaryOperatorDecl(ar,nm))
  | Ast.MetaAssignmentOperatorDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaAssignmentOperatorDecl(ar,nm))
  | Ast.MetaConstDecl(ar,nm,ty) ->
      (nm,function nm -> Ast.MetaConstDecl(ar,nm,ty))
  | Ast.MetaErrDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaErrDecl(ar,nm))
  | Ast.MetaExpDecl(ar,nm,ty) ->
      (nm,function nm -> Ast.MetaExpDecl(ar,nm,ty))
  | Ast.MetaIdExpDecl(ar,nm,ty) ->
      (nm,function nm -> Ast.MetaIdExpDecl(ar,nm,ty))
  | Ast.MetaLocalIdExpDecl(ar,nm,ty) ->
      (nm,function nm -> Ast.MetaLocalIdExpDecl(ar,nm,ty))
  | Ast.MetaGlobalIdExpDecl(ar,nm,ty) ->
      (nm,function nm -> Ast.MetaGlobalIdExpDecl(ar,nm,ty))
  | Ast.MetaExpListDecl(ar,nm,nm1) ->
      (nm,function nm -> Ast.MetaExpListDecl(ar,nm,nm1))
  | Ast.MetaDeclDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaDeclDecl(ar,nm))
  | Ast.MetaFieldListDecl(ar,nm,nm1) ->
      (nm,function nm -> Ast.MetaFieldListDecl(ar,nm,nm1))
  | Ast.MetaFieldDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaFieldDecl(ar,nm))
  | Ast.MetaStmDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaStmDecl(ar,nm))
  | Ast.MetaStmListDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaStmListDecl(ar,nm))
  | Ast.MetaFuncDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaFuncDecl(ar,nm))
  | Ast.MetaLocalFuncDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaLocalFuncDecl(ar,nm))
  | Ast.MetaPosDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaPosDecl(ar,nm))
  | Ast.MetaFragListDecl(ar,nm,nm1) ->
      (nm,function nm -> Ast.MetaFragListDecl(ar,nm,nm1))
  | Ast.MetaFmtDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaFmtDecl(ar,nm))
  | Ast.MetaAnalysisDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaAnalysisDecl(ar,nm))
  | Ast.MetaDeclarerDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaDeclarerDecl(ar,nm))
  | Ast.MetaIteratorDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaIteratorDecl(ar,nm))
  | Ast.MetaScriptDecl(cell,nm) -> failwith "not relevant to isos"

let make_new_metavars metavars bindings =
  let new_metavars =
    List.filter
      (function mv ->
	let (s,_) = get_name mv in
	try let _ = List.assoc s bindings in false with Not_found -> true)
      metavars in
  List.split
    (List.map
       (function mv ->
	 let (s,rebuild) = get_name mv in
	 let new_s = (!current_rule,new_mv s) in
	 (rebuild new_s, (s,new_s)))
       new_metavars)

(* --------------------------------------------------------------------- *)

let do_nothing x = x

let mkdisj matcher metavars alts e instantiater mkiso disj_maker minusify
    rebuild_mcodes name printer extra_plus update_others has_context =
  let call_instantiate bindings mv_bindings alts pattern has_context =
    List.concat
      (List.map
	 (function (a,_,_,_) ->
	   nub
	   (* no need to create duplicates when the bindings have no effect *)
	     (List.map
		(function bindings ->
		  let instantiated =
		    instantiater bindings mv_bindings e (rebuild_mcodes a) in
		  let plus_added =
		    if has_context (* ie if pat is not just a metavara *)
		    then
		      copy_plus printer minusify e (extra_plus e instantiated)
		    else instantiated in
		  if pattern = a
		  then plus_added
		  else (* iso tracking *)
		  Ast0.set_iso plus_added
		    ((name,mkiso a)::(Ast0.get_iso e))) (* keep count, not U *)
		bindings))
	 alts) in
  let rec inner_loop all_alts prev_ecount prev_icount prev_dcount = function
      [] -> Common.Left (prev_ecount, prev_icount, prev_dcount)
    | ((pattern,ecount,icount,dcount)::rest) ->
	let wc =
	  whencode_allowed prev_ecount prev_icount prev_dcount
	    ecount dcount icount rest in
	(match matcher true (context_required e) wc pattern e init_env with
	  Fail(reason) ->
	    if reason = NonMatch || not !Flag_parsing_cocci.show_iso_failures
	    then ()
	    else
	      (match matcher false false wc pattern e init_env with
		OK _ when !verbose_iso ->
		  interpret_reason name (Ast0.get_line e) reason
		    (function () -> printer e)
	      | _ -> ());
	    inner_loop all_alts (prev_ecount + ecount) (prev_icount + icount)
	      (prev_dcount + dcount) rest
	| OK (bindings : ((Ast.meta_name * 'a) list list)) ->
	    let all_alts =
	      (* apply update_others to all patterns other than the matched
		 one.  This is used to desigate the others as test
		 expressions in the TestExpression case *)
	      (List.map
		 (function (x,e,i,d) as all ->
		   if x = pattern
		   then all
		   else (update_others x,e,i,d))
		 (List.hd all_alts)) ::
	      (List.map
		 (List.map (function (x,e,i,d) -> (update_others x,e,i,d)))
		 (List.tl all_alts)) in
	    (match List.concat all_alts with
	      [x] -> Common.Left (prev_ecount, prev_icount, prev_dcount)
	    | all_alts ->
		let (new_metavars,mv_bindings) =
		  make_new_metavars metavars (nub(List.concat bindings)) in
		Common.Right
		  (new_metavars,
		   call_instantiate bindings mv_bindings all_alts pattern
		     (has_context pattern)))) in
  let rec outer_loop prev_ecount prev_icount prev_dcount = function
      [] | [[_]] (*only one alternative*)  -> (0,[],e) (* nothing matched *)
    | (alts::rest) as all_alts ->
	match inner_loop all_alts prev_ecount prev_icount prev_dcount alts with
	  Common.Left(prev_ecount, prev_icount, prev_dcount) ->
	    outer_loop prev_ecount prev_icount prev_dcount rest
	| Common.Right (new_metavars,res) ->
	    (1,new_metavars,
	     copy_minus printer minusify e (disj_maker res)) in
  let (count,metavars,e) = outer_loop 0 0 0 alts in
  (count, metavars, e)

(* no one should ever look at the information stored in these mcodes *)
let disj_starter lst =
  let old_info = Ast0.get_info(List.hd lst) in
  let new_pos_info =
    { old_info.Ast0.pos_info with
      Ast0.line_end = old_info.Ast0.pos_info.Ast0.line_start;
      Ast0.logical_end = old_info.Ast0.pos_info.Ast0.logical_start; } in
  let info =
    { Ast0.pos_info = new_pos_info;
      Ast0.whitespace = "";
      Ast0.attachable_start = false; Ast0.attachable_end = false;
      Ast0.mcode_start = []; Ast0.mcode_end = [];
      Ast0.strings_before = []; Ast0.strings_after = [];
      Ast0.isSymbolIdent = false; } in
  Ast0.make_mcode_info "(" info

let disj_ender lst =
  let old_info = Ast0.get_info(List.hd lst) in
  let new_pos_info =
    { old_info.Ast0.pos_info with
      Ast0.line_start = old_info.Ast0.pos_info.Ast0.line_end;
      Ast0.logical_start = old_info.Ast0.pos_info.Ast0.logical_end; } in
  let info =
    { Ast0.pos_info = new_pos_info;
      Ast0.whitespace = "";
      Ast0.attachable_start = false; Ast0.attachable_end = false;
      Ast0.mcode_start = []; Ast0.mcode_end = [];
      Ast0.strings_before = []; Ast0.strings_after = [];
      Ast0.isSymbolIdent = false; } in
  Ast0.make_mcode_info ")" info

let disj_mid _ = Ast0.make_mcode "|"

let make_disj_type tl =
  let mids =
    match tl with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap (Ast0.DisjType(disj_starter tl,tl,mids,disj_ender tl))
let make_disj_stmt_list tl =
  let mids =
    match tl with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap (Ast0.Disj(disj_starter tl,tl,mids,disj_ender tl))
let make_disj_expr model el =
  let mids =
    match el with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  let update_arg x =
    if Ast0.get_arg_exp model then Ast0.set_arg_exp x else x in
  let update_test x =
    let x = if Ast0.get_test_pos model then Ast0.set_test_pos x else x in
    if Ast0.get_test_exp model then Ast0.set_test_exp x else x in
  let el = List.map update_arg (List.map update_test el) in
  Ast0.context_wrap (Ast0.DisjExpr(disj_starter el,el,mids,disj_ender el))
let make_disj_decl dl =
  let mids =
    match dl with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap (Ast0.DisjDecl(disj_starter dl,dl,mids,disj_ender dl))
let make_disj_stmt sl =
  let dotify x = Ast0.context_wrap (Ast0.DOTS[x]) in
  let mids =
    match sl with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap
    (Ast0.Disj(disj_starter sl,List.map dotify sl,mids,disj_ender sl))

let transform_type (metavars,alts,name) e =
  match alts with
    (Ast0.TypeCTag(_)::_)::_ ->
      (* start line is given to any leaves in the iso code *)
      let start_line =
	Some ((Ast0.get_info e).Ast0.pos_info.Ast0.line_start) in
      let alts =
	List.map
	  (List.map
	     (function
		 Ast0.TypeCTag(p) ->
		   (p,count_edots.VT0.combiner_rec_typeC p,
		    count_idots.VT0.combiner_rec_typeC p,
		    count_dots.VT0.combiner_rec_typeC p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_typeC metavars alts e
	(fun b mv_b model ->
	  (instantiate b mv_b model).VT0.rebuilder_rec_typeC)
	(function t -> Ast0.TypeCTag t)
	make_disj_type make_minus.VT0.rebuilder_rec_typeC
	(rebuild_mcode start_line).VT0.rebuilder_rec_typeC
	name Unparse_ast0.typeC extra_copy_other_plus do_nothing
	(function x ->
	  match Ast0.unwrap x with Ast0.MetaType _ -> false | _ -> true)
  | _ -> (0,[],e)


let transform_expr (metavars,alts,name) e =
  let process update_others =
      (* start line is given to any leaves in the iso code *)
    let start_line =
      Some ((Ast0.get_info e).Ast0.pos_info.Ast0.line_start) in
    let alts =
      List.map
	(List.map
	   (function
	       Ast0.ExprTag(p) | Ast0.ArgExprTag(p) | Ast0.TestExprTag(p) ->
		 (p,count_edots.VT0.combiner_rec_expression p,
		  count_idots.VT0.combiner_rec_expression p,
		  count_dots.VT0.combiner_rec_expression p)
	     | _ -> failwith "invalid alt"))
	alts in
    mkdisj match_expr metavars alts e
      (fun b mv_b model ->
	(instantiate b mv_b model).VT0.rebuilder_rec_expression)
      (function e -> Ast0.ExprTag e)
      (make_disj_expr e)
      make_minus.VT0.rebuilder_rec_expression
      (rebuild_mcode start_line).VT0.rebuilder_rec_expression
      name Unparse_ast0.expression extra_copy_other_plus update_others
      (function x ->
	 match Ast0.unwrap x with
	   Ast0.MetaExpr _ | Ast0.MetaExprList _ | Ast0.MetaErr _ -> false
	 | _ -> true)
  in
  match alts with
    (Ast0.ExprTag(_)::r)::rs ->
      (* hack to accomodate ToTestExpression case, where the first pattern is
	 a normal expression, but the others are test expressions *)
      let others = r @ (List.concat rs) in
      let is_test = function Ast0.TestExprTag(_) -> true | _ -> false in
      if List.for_all is_test others then process Ast0.set_test_exp
      else if List.exists is_test others then failwith "inconsistent iso"
      else process do_nothing
  | (Ast0.ArgExprTag(_)::_)::_ when Ast0.get_arg_exp e -> process do_nothing
  | (Ast0.TestExprTag(_)::_)::_ when Ast0.get_test_pos e ->
      process Ast0.set_test_exp
  | _ -> (0,[],e)

let transform_decl (metavars,alts,name) e =
  match alts with
    (Ast0.DeclTag(_)::_)::_ ->
      (* start line is given to any leaves in the iso code *)
      let start_line =
	Some (Ast0.get_info e).Ast0.pos_info.Ast0.line_start in
      let alts =
	List.map
	  (List.map
	     (function
		 Ast0.DeclTag(p) ->
		   (p,count_edots.VT0.combiner_rec_declaration p,
		    count_idots.VT0.combiner_rec_declaration p,
		    count_dots.VT0.combiner_rec_declaration p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_decl metavars alts e
	(fun b mv_b model ->
	  (instantiate b mv_b model).VT0.rebuilder_rec_declaration)
	(function d -> Ast0.DeclTag d)
	make_disj_decl
	make_minus.VT0.rebuilder_rec_declaration
	(rebuild_mcode start_line).VT0.rebuilder_rec_declaration
	name Unparse_ast0.declaration extra_copy_other_plus do_nothing
	(function _ -> true (* no metavars *))
  | _ -> (0,[],e)

let transform_stmt (metavars,alts,name) e =
  match alts with
    (Ast0.StmtTag(_)::_)::_ ->
      (* start line is given to any leaves in the iso code *)
      let start_line =
	Some (Ast0.get_info e).Ast0.pos_info.Ast0.line_start in
      let alts =
	List.map
	  (List.map
	     (function
		 Ast0.StmtTag(p) ->
		   (p,count_edots.VT0.combiner_rec_statement p,
		    count_idots.VT0.combiner_rec_statement p,
		    count_dots.VT0.combiner_rec_statement p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_statement metavars alts e
	(fun b mv_b model ->
	  (instantiate b mv_b model).VT0.rebuilder_rec_statement)
	(function s -> Ast0.StmtTag s)
	make_disj_stmt make_minus.VT0.rebuilder_rec_statement
	(rebuild_mcode start_line).VT0.rebuilder_rec_statement
	name (Unparse_ast0.statement "") extra_copy_stmt_plus do_nothing
	(function x ->
	  match Ast0.unwrap x with
	    Ast0.MetaStmt _ | Ast0.MetaStmtList _ -> false
	  | _ -> true)
  | _ -> (0,[],e)

(* sort of a hack, because there is no disj at top level *)
let transform_top (metavars,alts,name) e =
  match Ast0.unwrap e with
    Ast0.NONDECL(declstm) ->
      (try
	let strip alts =
	  List.map
	    (List.map
	       (function
		   Ast0.DotsStmtTag(d) ->
		     (match Ast0.unwrap d with
		       Ast0.DOTS([s]) -> Ast0.StmtTag(s)
		     | _ -> raise (Failure ""))
		 | _ -> raise (Failure "")))
	    alts in
	let (count,mv,s) = transform_stmt (metavars,strip alts,name) declstm in
	(count,mv,Ast0.rewrap e (Ast0.NONDECL(s)))
      with Failure _ -> (0,[],e))
  | Ast0.CODE(stmts) ->
      let (count,mv,res) =
	match alts with
	  (Ast0.DotsStmtTag(_)::_)::_ ->
	      (* start line is given to any leaves in the iso code *)
	    let start_line =
	      Some ((Ast0.get_info e).Ast0.pos_info.Ast0.line_start) in
	    let alts =
	      List.map
		(List.map
		   (function
		       Ast0.DotsStmtTag(p) ->
			 (p,count_edots.VT0.combiner_rec_statement_dots p,
			  count_idots.VT0.combiner_rec_statement_dots p,
			  count_dots.VT0.combiner_rec_statement_dots p)
		     | _ -> failwith "invalid alt"))
		alts in
	    mkdisj match_statement_dots metavars alts stmts
	      (fun b mv_b model ->
		(instantiate b mv_b model).VT0.rebuilder_rec_statement_dots)
	      (function s -> Ast0.DotsStmtTag s)
	      (function x ->
		Ast0.rewrap e (Ast0.DOTS([make_disj_stmt_list x])))
	      (function x ->
		make_minus.VT0.rebuilder_rec_statement_dots x)
	      (rebuild_mcode start_line).VT0.rebuilder_rec_statement_dots
	      name Unparse_ast0.statement_dots extra_copy_other_plus do_nothing
	      (function _ -> true)
	| _ -> (0,[],stmts) in
      (count,mv,Ast0.rewrap e (Ast0.CODE res))
  | _ -> (0,[],e)

(* --------------------------------------------------------------------- *)

let transform (alts : isomorphism) t =
  (* the following ugliness is because rebuilder only returns a new term *)
  let extra_meta_decls = ref ([] : Ast_cocci.metavar list) in
  let in_limit n = function
      None -> true
    | Some n1 ->
	n < n1 ||
	((if !Flag_parsing_cocci.show_iso_failures
	then Common.pr2_once "execeeded iso threshold, see -iso_limit option");
	 false) in
  let bind x y = x + y in
  let option_default = 0 in
  let exprfn r k e =
    let (e_count,e) = k e in
    if in_limit e_count !Flag_parsing_cocci.iso_limit
    then
      let (count,extra_meta,exp) = transform_expr alts e in
      extra_meta_decls := extra_meta @ !extra_meta_decls;
      (bind count e_count,exp)
    else (e_count,e) in

  let declfn r k e =
    let (e_count,e) = k e in
    if in_limit e_count !Flag_parsing_cocci.iso_limit
    then
      let (count,extra_meta,dec) = transform_decl alts e in
      extra_meta_decls := extra_meta @ !extra_meta_decls;
      (bind count e_count,dec)
    else (e_count,e) in

  let stmtfn r k e =
    let (e_count,e) = k e in
    if in_limit e_count !Flag_parsing_cocci.iso_limit
    then
      let (count,extra_meta,stm) = transform_stmt alts e in
      extra_meta_decls := extra_meta @ !extra_meta_decls;
      (bind count e_count,stm)
    else (e_count,e) in

  let typefn r k e =
    let (continue,e_count,e) =
      match Ast0.unwrap e with
	Ast0.Signed(signb,tyb) ->
       (* Hack!  How else to prevent iso from applying under an
	  unsigned??? *)
	  (true,0,e)
      | _ ->
	  let (e_count,e) = k e in
	  if in_limit e_count !Flag_parsing_cocci.iso_limit
	  then (true,e_count,e)
	  else (false,e_count,e) in
    if continue
    then
      let (count,extra_meta,ty) = transform_type alts e in
      extra_meta_decls := extra_meta @ !extra_meta_decls;
      (bind count e_count,ty)
    else (e_count,e) in

  let topfn r k e =
    let (e_count,e) = k e in
    if in_limit e_count !Flag_parsing_cocci.iso_limit
    then
      let (count,extra_meta,ty) = transform_top alts e in
      extra_meta_decls := extra_meta @ !extra_meta_decls;
      (bind count e_count,ty)
    else (e_count,e) in

  let res =
    V0.combiner_rebuilder bind option_default
      {V0.combiner_rebuilder_functions with
	VT0.combiner_rebuilder_exprfn = exprfn;
	VT0.combiner_rebuilder_tyfn = typefn;
	VT0.combiner_rebuilder_declfn = declfn;
	VT0.combiner_rebuilder_stmtfn = stmtfn;
	VT0.combiner_rebuilder_topfn = topfn} in
  let (_,res) = res.VT0.top_level t in
  (!extra_meta_decls,res)

(* --------------------------------------------------------------------- *)

(* should be done by functorizing the parser to use wrap or context_wrap *)
let rewrap =
  let mcode (x,a,i,mc,pos,adj) = (x,a,i,Ast0.context_befaft(),pos,adj) in
  let donothing r k e = Ast0.context_wrap(Ast0.unwrap(k e)) in
  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing

let rec rewrap_anything = function
    Ast0.DotsExprTag(d) ->
      Ast0.DotsExprTag(rewrap.VT0.rebuilder_rec_expression_dots d)
  | Ast0.DotsInitTag(d) ->
      Ast0.DotsInitTag(rewrap.VT0.rebuilder_rec_initialiser_list d)
  | Ast0.DotsParamTag(d) ->
      Ast0.DotsParamTag(rewrap.VT0.rebuilder_rec_parameter_list d)
  | Ast0.DotsStmtTag(d) ->
      Ast0.DotsStmtTag(rewrap.VT0.rebuilder_rec_statement_dots d)
  | Ast0.DotsDeclTag(d) ->
      Ast0.DotsDeclTag(rewrap.VT0.rebuilder_rec_declaration_dots d)
  | Ast0.DotsCaseTag(d) ->
      Ast0.DotsCaseTag(rewrap.VT0.rebuilder_rec_case_line_dots d)
  | Ast0.IdentTag(d) -> Ast0.IdentTag(rewrap.VT0.rebuilder_rec_ident d)
  | Ast0.ExprTag(d) -> Ast0.ExprTag(rewrap.VT0.rebuilder_rec_expression d)
  | Ast0.AssignOpTag(d) ->
      Ast0.AssignOpTag(rewrap.VT0.rebuilder_rec_assignOp d)
  | Ast0.BinaryOpTag(d) ->
      Ast0.BinaryOpTag(rewrap.VT0.rebuilder_rec_binaryOp d)
  | Ast0.ArgExprTag(d) ->
      Ast0.ArgExprTag(rewrap.VT0.rebuilder_rec_expression d)
  | Ast0.TestExprTag(d) ->
      Ast0.TestExprTag(rewrap.VT0.rebuilder_rec_expression d)
  | Ast0.TypeCTag(d) -> Ast0.TypeCTag(rewrap.VT0.rebuilder_rec_typeC d)
  | Ast0.InitTag(d) -> Ast0.InitTag(rewrap.VT0.rebuilder_rec_initialiser d)
  | Ast0.ParamTag(d) -> Ast0.ParamTag(rewrap.VT0.rebuilder_rec_parameter d)
  | Ast0.DeclTag(d) -> Ast0.DeclTag(rewrap.VT0.rebuilder_rec_declaration d)
  | Ast0.StmtTag(d) -> Ast0.StmtTag(rewrap.VT0.rebuilder_rec_statement d)
  | Ast0.ForInfoTag(d) -> Ast0.ForInfoTag(rewrap.VT0.rebuilder_rec_forinfo d)
  | Ast0.CaseLineTag(d) ->
      Ast0.CaseLineTag(rewrap.VT0.rebuilder_rec_case_line d)
  | Ast0.StringFragmentTag(d) ->
      Ast0.StringFragmentTag(rewrap.VT0.rebuilder_rec_string_fragment d)
  | Ast0.TopTag(d) -> Ast0.TopTag(rewrap.VT0.rebuilder_rec_top_level d)
  | Ast0.IsoWhenTag(_) | Ast0.IsoWhenTTag(_) | Ast0.IsoWhenFTag(_) ->
      failwith "only for isos within iso phase"
  | Ast0.MetaPosTag(p) -> Ast0.MetaPosTag(p)
  | Ast0.HiddenVarTag(p) -> Ast0.HiddenVarTag(p) (* not sure it is possible *)
  | Ast0.WhenTag(a,e,b) -> rewrap_anything b

(* --------------------------------------------------------------------- *)

let apply_isos isos rule rule_name =
  if isos = []
  then ([],rule)
  else
    begin
      current_rule := rule_name;
      let isos =
	List.map
	  (function (metavars,iso,name) ->
	    (metavars,List.map (List.map rewrap_anything) iso,name))
	  isos in
      let (extra_meta,rule) =
	List.split
	  (List.map
	     (function t ->
	       List.fold_left
		 (function (extra_meta,t) -> function iso ->
		   let (new_extra_meta,t) = transform iso t in
		   (new_extra_meta@extra_meta,t))
		 ([],t) isos)
	     rule) in
      (List.concat extra_meta, (Compute_lines.compute_lines true) rule)
    end
