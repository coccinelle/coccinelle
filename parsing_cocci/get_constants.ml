(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* get a list of all of the constants in the - slice of a SmPL file, to be
used to select which files to process *)

(* This could be made more efficient, by finding only the important things.
eg, if we have a function and its arguments, we could just pick the function.
And we could try to pick only the things annotated with -, and only pick
something else if there is no -.  In general, we only want the most important
constant, not all the constants. *)

module Ast = Ast_cocci
module V = Visitor_ast
module TC = Type_cocci

let keep_some_bind x y = match x with [] -> y | _ -> x
let or_bind x y = match x with [] -> [] | _ -> x
let keep_all_bind = Common.union_set

let get_minus_constants bind orbind =
  let donothing r k e = k e in
  let option_default = [] in
  let mcode _ _ = option_default in

  (* if one branch gives no information, then we have to take anything *)
  let disj_union_all l =
    if List.exists (function [] -> true | _ -> false) l
    then orbind [] (Common.union_all l)
    else Common.union_all l in

  (* need special cases for everything with a disj, because the bind above
     would throw away all but the first disj *)

  let ident r k e =
    match Ast.unwrap e with
      Ast.Id(name) ->
	(match Ast.unwrap_mcode name with
	  "NULL" -> [] (* special case, because this is too generic *)
	| nm -> [nm])
    | _ -> k e in

  let expression r k e =
    match Ast.unwrap e with
      Ast.RecordAccess(exp,_,fld) | Ast.RecordPtAccess(exp,_,fld) ->
	bind
	  (Common.union_all
	     (List.map (function id -> ["."^id;"->"^id])
		(r.V.combiner_ident fld)))
	  (r.V.combiner_expression exp)
    | Ast.QualifiedAccess(Some ty,_,fld) ->
    bind
	  (Common.union_all
         (List.map (function id -> ["::"^id])
		(r.V.combiner_ident fld)))
	  (r.V.combiner_fullType ty)
    | Ast.QualifiedACcess(None,_,fld) ->
	  Common.union_all
         (List.map (function id -> ["::"^id])
	     (r.V.combiner_ident fld))
    | Ast.SizeOfExpr(sizeof,_) | Ast.SizeOfType(sizeof,_,_,_) ->
	bind (k e) [Ast.unwrap_mcode sizeof]
    | Ast.Delete(delete,_) | Ast.DeleteArr(delete,_,_,_) ->
	bind (k e) [Ast.unwrap_mcode delete]
    | Ast.NewPar(nw,_,_,_,_,_,_) ->
	bind (k e) [Ast.unwrap_mcode new]
    | Ast.NewNoPar(nw,_,_,_,_,_,_) ->
	bind (k e) [Ast.unwrap_mcode new]
    | Ast.DisjExpr(exps) ->
	disj_union_all (List.map r.V.combiner_expression exps)
    | Ast.Edots(_,_) | Ast.Ecircles(_,_) | Ast.Estars(_,_) -> []
    | Ast.NestExpr(starter,expr_dots,ender,whencode,false) -> []
    | Ast.NestExpr(starter,expr_dots,ender,whencode,true) ->
	r.V.combiner_expression_dots expr_dots
    | _ -> k e in

  let typeC r k e =
    match Ast.unwrap e with
     Ast.TypeOfExpr(tf,_,_,_) | Ast.TypeOfType(tf,_,_,_) ->
	bind (k e) [Ast.unwrap_mcode tf]
    | Ast.NamedType(ty) ->
	if !Flag.sgrep_mode2
	then
	  match ty with
	    (_,_,Ast.MINUS(_,_,_,_),_) -> [Ast.unwrap_mcode ty]
	  | _ -> []
	else [Ast.unwrap_mcode ty]
    | _ -> k e in

  let fullType r k e =
    match Ast.unwrap e with
      Ast.DisjType(types) ->
	disj_union_all (List.map r.V.combiner_fullType types)
    | _ -> k e in

  let declaration r k e =
    match Ast.unwrap e with
      Ast.DisjDecl(decls) ->
	disj_union_all (List.map r.V.combiner_declaration decls)
    | Ast.Ddots(dots,whencode) -> []
    | _ -> k e in

  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.DisjRuleElem(res) ->
	disj_union_all (List.map r.V.combiner_rule_elem res)
    | _ -> k e in

  let statement r k e =
    match Ast.unwrap e with
      Ast.Disj(stmt_dots) ->
	disj_union_all (List.map r.V.combiner_statement_dots stmt_dots)
    | Ast.Dots(d,whn,_,_) | Ast.Circles(d,whn,_,_) | Ast.Stars(d,whn,_,_) -> []
    | Ast.Nest(starter,stmt_dots,ender,whn,false,_,_) -> []
    | Ast.Nest(starter,stmt_dots,ender,whn,true,_,_) ->
	r.V.combiner_statement_dots stmt_dots
    | _ -> k e in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    ident expression fullType typeC donothing donothing declaration
    rule_elem statement donothing donothing donothing

(* ------------------------------------------------------------------------ *)

let get_all_minus_constants =
  let donothing r k e = k e in
  let bind = Common.union_set in
  let option_default = [] in
  let mcode r (x,_,mcodekind,_) =
    match mcodekind with
      Ast.MINUS(_,_,_,_) -> [x]
    | _ -> [] in
  let other r (x,_,mcodekind,_) = [] in

  V.combiner bind option_default
    other mcode other other other other other other other other other other

    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing
(* ------------------------------------------------------------------------ *)

let get_plus_constants =
  let donothing r k e = k e in
  let bind = Common.union_set in
  let option_default = [] in
  let mcode r (_,_,mcodekind,_) =
    let recurse l =
      List.fold_left
	(List.fold_left
	   (function prev ->
	     function cur ->
	       let fn = get_minus_constants keep_all_bind keep_all_bind in
	       bind (fn.V.combiner_anything cur) prev))
	[] l in
    match mcodekind with
      Ast.MINUS(_,_,_,anythings) -> recurse anythings
    | Ast.CONTEXT(_,Ast.BEFORE(a,_)) -> recurse a
    | Ast.CONTEXT(_,Ast.AFTER(a,_)) -> recurse a
    | Ast.CONTEXT(_,Ast.BEFOREAFTER(a1,a2,_)) ->
	Common.union_set (recurse a1) (recurse a2)
    | _ -> [] in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

(* ------------------------------------------------------------------------ *)
(* see if there are any inherited variables that must be bound for this rule
to match *)

let check_inherited nm =
  let donothing r k e = k e in
  let option_default = false in
  let bind x y = x or y in
  let inherited (nm1,_) = not(nm = nm1) in
  let minherited mc = inherited (Ast.unwrap_mcode mc) in
  let mcode _ x =
    match Ast.get_pos_var x with
      Ast.MetaPos(name,constraints,_,keep,inh) -> minherited name
    | _ -> option_default in

  (* a case for everything for there is a metavariable, also disjunctions
     or optional things *)

  let strictident recursor k i =
    match Ast.unwrap i with
      Ast.MetaId(name,_,_,_) | Ast.MetaFunc(name,_,_,_)
    | Ast.MetaLocalFunc(name,_,_,_) -> bind (k i) (minherited name)
    | _ -> k i in

  let rec type_collect res = function
      TC.ConstVol(_,ty,_) | TC.Pointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.MetaType(tyname,_,_) ->
	inherited tyname
    | ty -> res in

  let strictexpr recursor k e =
    match Ast.unwrap e with
      Ast.MetaExpr(name,_,_,Some type_list,_,_) ->
	let types = List.fold_left type_collect option_default type_list in
	bind (minherited name) types
    | Ast.MetaErr(name,_,_,_) | Ast.MetaExpr(name,_,_,_,_,_) ->
	bind (k e) (minherited name)
    | Ast.MetaExprList(name,None,_,_) -> bind (k e) (minherited name)
    | Ast.MetaExprList(name,Some (lenname,_,_),_,_) ->
	bind (k e) (bind (minherited name) (minherited lenname))
    | Ast.DisjExpr(exps) ->
	(* could see if there are any variables that appear in all branches,
	   but perhaps not worth it *)
	option_default
    | _ -> k e in

  let strictdecls recursor k d =
    match Ast.unwrap d with
      Ast.MetaDecl(name,_,_) | Ast.MetaField(name,_,_) ->
	bind (k p) (minherited name)
    | Ast.DisjDecl(decls) -> option_default
    | _ -> k d in

  let strictfullType recursor k ty =
    match Ast.unwrap ty with
      Ast.DisjType(types) -> option_default
    | _ -> k ty in

  let stricttypeC recursor k ty =
    match Ast.unwrap ty with
      Ast.MetaType(name,_,_) -> bind (k ty) (minherited name)
    | _ -> k ty in

  let strictparam recursor k p =
    match Ast.unwrap p with
      Ast.MetaParam(name,_,_) -> bind (k p) (minherited name)
    | Ast.MetaParamList(name,None,_,_) -> bind (k p) (minherited name)
    | Ast.MetaParamList(name,Some(lenname,_,_),_,_) ->
	bind (k p) (bind (minherited name) (minherited lenname))
    | _ -> k p in

  let strictrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    match Ast.unwrap re with
      Ast.MetaRuleElem(name,_,_) | Ast.MetaStmt(name,_,_,_)
    | Ast.MetaStmtList(name,_,_) -> bind (k re) (minherited name)
    | _ -> k re in

  let strictattribute recursor k a =
    match Ast.unwrap a with
      Ast.MetaAttribute(name,_,_) -> bind (k a) (minherited name)
    | _ -> k a in

  let strictstatement recursor k s =
    match Ast.unwrap s with
      Ast.Disj(stms) -> option_default
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    strictident strictexpr strictfullType stricttypeC donothing strictparam
    strictdecls strictrule_elem strictstatement attributefn donothing donothing

(* ------------------------------------------------------------------------ *)

let rec dependent = function
    Ast.Dep s -> true
  | Ast.AntiDep s -> false
  | Ast.EverDep s -> true
  | Ast.NeverDep s -> false
  | Ast.AndDep (d1,d2) -> dependent d1 or dependent d2
  | Ast.OrDep (d1,d2) -> dependent d1 && dependent d2
  | Ast.NoDep -> false
  | Ast.FailDep -> true

(* ------------------------------------------------------------------------ *)

let rule_fn tls in_plus =
  List.fold_left
    (function (rest_info,in_plus) ->
      function cur ->
	let mfn = get_minus_constants keep_some_bind or_bind in
	let minuses = mfn.V.combiner_top_level cur in
	let all_minuses =
	  if !Flag.sgrep_mode2
	  then [] (* nothing removed for sgrep *)
	  else get_all_minus_constants.V.combiner_top_level cur in
	let plusses = get_plus_constants.V.combiner_top_level cur in
	(* the following is for eg -foo(2) +foo(x) then in another rule
	   -foo(10); don't want to consider that foo is guaranteed to be
	   created by the rule.  not sure this works completely: what if foo is
	   in both - and +, but in an or, so the cases aren't related?
	   not sure this whole thing is a good idea.  how do we know that
	   something that is only in plus is really freshly created? *)
	let plusses = Common.minus_set plusses all_minuses in
	let new_minuses = Common.minus_set minuses in_plus in
	let new_plusses = Common.union_set plusses in_plus in
	(Common.union_set new_minuses rest_info, new_plusses))
    ([],in_plus) tls

exception No_info

let get_constants rules =
  try
    let (info,_) =
      List.fold_left
        (function (rest_info,in_plus) ->
          function r ->
            match r with
              Ast.ScriptRule (_,_,_,_)
	    | Ast.InitialScriptRule (_,_,_) | Ast.FinalScriptRule (_,_,_) ->
		(rest_info, in_plus)
            | Ast.CocciRule (nm, (dep,_,_), cur, _, _) ->
                let (cur_info,cur_plus) = rule_fn cur in_plus in
	        let cur_info =
	          (* no dependencies if dependent on another rule; then we
	             need to find the constants of that rule *)
	          if dependent dep or
	            List.for_all (check_inherited nm).V.combiner_top_level cur
	          then []
	          else
		  if cur_info = [] then raise No_info else cur_info in
	        (Common.union_set [cur_info] rest_info,cur_plus))
        ([],[]) rules in
    List.rev info
  with No_info -> List.map (function _ -> []) rules
