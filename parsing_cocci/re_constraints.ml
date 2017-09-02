(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* if a variable affected by a script constraint is always referenced only
with all the other local variables that are mentioned bt that script
constraint, then we can push the script constraint down to the rule_element
level *)

(* Could be more optimal by checking that the params of the constraint
are not under a disj. Memoize to avoid rechecking for each constraint. *)

let disj_free_table = Hashtbl.create 101

let disj_free re =
  let bind = (&&) in
  let option_default = true in
  let mcode = true in
  let donothing r k e = k e in
  (* case for anything with a disj *)
  let ident r k e =
    match Ast.unwrap e with Ast.DisjId -> false | _ -> k e in
  let expr r k e =
    match Ast.unwrap e with Ast.DisjExpr -> false | _ -> k e in
  let ty r k e =
    match Ast.unwrap e with Ast.DisjType -> false | _ -> k e in
  let decl r k e =
    match Ast.unwrap e with Ast.DisjDecl -> false | _ -> k e in
  let field r k e =
    match Ast.unwrap e with Ast.DisjField -> false | _ -> k e in
  let rule_elem r k e =
    match Ast.unwrap e with Ast.DisjRuleElem -> false | _ -> k e in
  let statement r k e =
    match Ast.unwrap e with Ast.Disj -> false | _ -> k e in
  let v =
    V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing ident
    expr donothing donothing donothing donothing donothing ty
    donothing donothing donothing decl donothing field donothing
    rule_elem statement donothing donothing donothing in
  try Hashtbl.find disj_free_table re
  with Not_found ->
    let res = v.V.combiner_rule_elem re in
    Hashtbl.add disj_free_table re res;
    res

let ok_for_all_rule_elems constraint minirules =
  let bind = (&&) in
  let option_default = true in
  let mcode = true in
  let donothing r k e = k e in

  let (self,(_key,_lang,params,_pos,_code)) = constraint in
  let rule_elem r k re =
    let available = Ast.minus_free_vars re in
    if List.mem self available
    then
      (List.for_all (fun x -> List.mem x available) (List.map fst params)) &&
      (disj_free re)
    else true (* not relevant to this rule_elem *) in

  let v =
    V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    rule_elem donothing donothing donothing donothing in
  List.for_all (v.V.combiner_top_level) minirules

let update_for_all_rule_elems constraint rule =
  let mcode mc = mc in
  let donothing r k e = k e in

  let (self,(_key,_lang,params,_pos,_code)) = constraint in
  let rule_elem r k re =
    let re = k re in
    let available = Ast.minus_free_vars re in
    if List.mem self available
    then Ast.add_constraint constraint re
    else re in
    
  let v =
    V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing
      donothing donothing donothing rule_elem
      donothing donothing donothing donothing in
  match rule with
    Ast.CocciRule(rule_name,_,minirules,_,_) ->
      List.map (v.V.combiner_top_level) minirules
  | Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ ->
      rule

let remove ((nm,x) as cstr) =
  let cell = Hashtbl.find Data.non_local_script_constraints nm in
  let rest = List.filter (function y -> not(x = snd y)) !cell in
  cell := rest

let re_constraints rules =
  let constraints_by_rules = Hashtbl.create 101 in
  Hashtbl.iter
    (fun (rl,_) cstr ->
      List.iter (Common.hashadd constraints_by_rules rl) !cstr)
    Data.non_local_script_constraints;
  let rules =
    List.map
      (function rule ->
	match rule with
	  Ast.CocciRule(rule_name,info,minirules,b,ty) ->
	    let constraints =
	      try !(Hashtbl.find constraints_by_rules rule_name)
	      with Not_found -> [] in
	    let minirules =
	      List.fold_left
		(fun minirules constraint ->
		  let v = v constraint in
		  if ok_for_all_rule_elems constraint minirules
		  then
		    begin
		      remove constraint;
		      update_for_all_rule_elems constraint minirules
		    end
		  else minirules)
		minirules constraints in
	    Ast.CocciRule(rule_name,info,minirules,b,ty)
	| Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ ->
	    rule)
      rules in
  Hashtbl.clear disj_free_table;
  rules
  
