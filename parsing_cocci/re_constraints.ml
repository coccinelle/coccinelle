(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module V = Visitor_ast

(* if a variable affected by a script constraint is always referenced only
with all the other local variables that are mentioned by that script
constraint, then we can push the script constraint down to the rule_element
level *)

(* Could be more optimal by checking that the params of the constraint
are not under a disj. Memoize to avoid rechecking for each constraint. *)

let disj_free_table = Hashtbl.create 101

let disj_free re =
  let bind = (&&) in
  let option_default = true in
  let mcode _ _ = true in
  let donothing r k e = k e in
  (* case for anything with a disj *)
  let ident r k e =
    match Ast.unwrap e with Ast.DisjId _ -> false | _ -> k e in
  let expr r k e =
    match Ast.unwrap e with Ast.DisjExpr _ -> false | _ -> k e in
  let ty r k e =
    match Ast.unwrap e with Ast.DisjType _ -> false | _ -> k e in
  let decl r k e =
    match Ast.unwrap e with Ast.DisjDecl _ -> false | _ -> k e in
  let ann_field r k e =
    match Ast.unwrap e with Ast.DisjField _ -> false | _ -> k e in
  let rule_elem r k e =
    match Ast.unwrap e with Ast.DisjRuleElem _ -> false | _ -> k e in
  let statement r k e =
    match Ast.unwrap e with Ast.Disj _ -> false | _ -> k e in
  let v =
    V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing donothing ident
    expr donothing donothing donothing donothing ty donothing
    donothing donothing donothing decl donothing donothing ann_field donothing
    rule_elem statement donothing donothing donothing donothing donothing in
  try Hashtbl.find disj_free_table re
  with Not_found ->
    let res = v.V.combiner_rule_elem re in
    Hashtbl.add disj_free_table re res;
    res

let ok_for_all_rule_elems cstr minirules =
  let bind = (&&) in
  let option_default = true in
  let mcode _ _ = true in
  let donothing r k e = k e in

  let (self,(_key,_lang,params,_pos,_code)) = cstr in
  let rule_elem r k re =
    let available = Ast.get_minus_nc_fvs re in
    if List.mem self available
    then
      let params_available =
	List.for_all (fun x -> List.mem x available) (List.map fst params) in
      if params_available
      then
	let disj_free = disj_free re in
	if disj_free
	then true
	else
	  failwith
	    (Printf.sprintf
	       "%s: constraint on variable %s cannot be evaluated in line %d due to disjunction\n%s"
	       (fst self) (snd self) (Ast.get_line re)
	       (Pretty_print_cocci.rule_elem_to_string re))
      else
	failwith
	  (Printf.sprintf
	     "%s: constraint on variable %s cannot be evaluated in line %d. available: %s\nwanted: %s"
	     (fst self) (snd self) (Ast.get_line re) (Dumper.dump available)
	     (Dumper.dump (List.map fst params)))
    else true (* not relevant to this rule_elem *) in

  let v =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing rule_elem donothing donothing donothing donothing
      donothing donothing in
  List.for_all v.V.combiner_top_level minirules

let update_for_all_rule_elems cstr minirules =
  let mcode mc = mc in
  let donothing r k e = k e in

  let (self,((_key,_lang,params,_pos,_code) as sc)) = cstr in
  let rule_elem r k re =
    let re = k re in
    let available = Ast.get_minus_nc_fvs re in
    if List.mem self available
    then Ast.add_constraint re (self,Ast.CstrScript(true,sc))
    else re in
    
  let v =
    V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing
      donothing rule_elem donothing donothing donothing donothing
      donothing donothing in
  List.map v.V.rebuilder_top_level minirules

let remove rule_name ((nm,_) as x) =
  let cell = Hashtbl.find Data.non_local_script_constraints (rule_name,nm) in
  let rest = List.filter (function y -> not(x = y)) !cell in
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
		(fun minirules cstr ->
		  if ok_for_all_rule_elems cstr minirules
		  then
		    begin
		      remove rule_name cstr;
		      update_for_all_rule_elems cstr minirules
		    end
		  else minirules)
		minirules constraints in
	    Ast.CocciRule(rule_name,info,minirules,b,ty)
	| Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ ->
	    rule)
      rules in
  Hashtbl.clear disj_free_table;
  rules
  
