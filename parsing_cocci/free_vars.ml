(* For each rule return the list of variables that are used after it.
Also augment various parts of each rule with unitary, inherited, and freshness
informations *)

module Ast = Ast_cocci
module V = Visitor_ast
module TC = Type_cocci

let rec nub = function
    [] -> []
  | (x::xs) when (List.mem x xs) -> nub xs
  | (x::xs) -> x::(nub xs)

(* Collect all variable references in a minirule.  For a disj, we collect
the maximum number (2 is enough) of references in any branch. *)

let collect_unitary_nonunitary free_usage =
  let free_usage = List.sort compare free_usage in
  let rec loop1 todrop = function (* skips multiple occurrences *)
      [] -> []
    | (x::xs) as all -> if x = todrop then loop1 todrop xs else all in
  let rec loop2 = function
      [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::xs ->
	if x = y (* occurs more than once in free_usage *)
	then
	  let (unitary,non_unitary) = loop2(loop1 x xs) in
	  (unitary,x::non_unitary)
	else (* occurs only once in free_usage *)
	  let (unitary,non_unitary) = loop2 (y::xs) in
	  (x::unitary,non_unitary) in
  loop2 free_usage

let collect_all_refs =
  let bind x y = x @ y in
  let option_default = [] in

  let donothing recursor k e = k e in (* just combine in the normal way *)

  (* the following considers that anything that occurs non-unitarily in one
     branch occurs nonunitarily in all branches.  This is not optimal, but
     doing better seems to require a breadth-first traversal, which is
     perhaps better to avoid.  Also, unitarily is represented as occuring once,
     while nonunitarily is represented as twice - more is irrelevant *)
  (* cases for disjs and metavars *)
  let bind_disj refs_branches =
    let (unitary,nonunitary) =
      List.split (List.map collect_unitary_nonunitary refs_branches) in
    let unitary = nub (List.concat unitary) in
    let nonunitary = nub (List.concat nonunitary) in
    let unitary =
      List.filter (function x -> not (List.mem x nonunitary)) unitary in
    unitary@nonunitary@nonunitary in

  let metaid (x,_,_) = x in

  let astfvident recursor k i =
    match Ast.unwrap i with
      Ast.MetaId(name,_,_) | Ast.MetaFunc(name,_,_)
    | Ast.MetaLocalFunc(name,_,_) -> [metaid name]
    | _ -> k i in

  let rec type_collect res = function
      TC.ConstVol(_,ty) | TC.Pointer(ty) | TC.FunctionPointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.MetaType(tyname,_,_) -> bind [tyname] res
    | ty -> res in

  let astfvexpr recursor k e =
    match Ast.unwrap e with
      Ast.MetaExpr(name,_,Some type_list,_)
    | Ast.MetaConst(name,_,Some type_list,_) ->
	let types = List.fold_left type_collect option_default type_list in
	bind [metaid name] types
    | Ast.MetaConst(name,_,_,_) | Ast.MetaErr(name,_,_)
    | Ast.MetaExpr(name,_,_,_) | Ast.MetaExprList(name,_,_) -> [metaid name]
    | Ast.DisjExpr(exps) -> bind_disj (List.map k exps)
    | _ -> k e in

  let astfvdecls recursor k d =
    match Ast.unwrap d with
      Ast.DisjDecl(decls) -> bind_disj (List.map k decls)
    | _ -> k d in

  let astfvfullType recursor k ty =
    match Ast.unwrap ty with
      Ast.DisjType(types) -> bind_disj (List.map k types)
    | _ -> k ty in

  let astfvtypeC recursor k ty =
    match Ast.unwrap ty with
      Ast.MetaType(name,_,_) -> [metaid name]
    | _ -> k ty in

  let astfvparam recursor k p =
    match Ast.unwrap p with
      Ast.MetaParam(name,_,_) | Ast.MetaParamList(name,_,_) -> [metaid name]
    | _ -> k p in

  let astfvrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    nub
      (match Ast.unwrap re with
	Ast.MetaRuleElem(name,_,_) | Ast.MetaStmt(name,_,_,_)
      | Ast.MetaStmtList(name,_,_) -> [metaid name]
      | Ast.Define(_,id,_,db) ->
	  (match Ast.unwrap db with
	    Ast.DMetaId(name,_) -> bind (k re) [metaid name]
	  | _ -> k re)
      | _ -> k re) in

  let astfvstatement recursor k s =
    match Ast.unwrap s with
      Ast.Disj(stms) ->
	bind_disj (List.map recursor.V.combiner_statement_dots stms)
    | _ -> k s in

  let mcode r e = [] in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    astfvident astfvexpr astfvfullType astfvtypeC donothing astfvparam
    astfvdecls astfvrule_elem astfvstatement donothing donothing donothing

let collect_all_rule_refs minirules =
  List.fold_left (@) []
    (List.map collect_all_refs.V.combiner_top_level minirules)

let collect_all_minirule_refs = collect_all_refs.V.combiner_top_level

(* ---------------------------------------------------------------- *)

let collect_saved =
  let bind = Common.union_set in
  let option_default = [] in

  let donothing recursor k e = k e in (* just combine in the normal way *)

  let metaid (x,_,_) = x in

  (* cases for metavariables *)
  let astfvident recursor k i =
    match Ast.unwrap i with
      Ast.MetaId(name,TC.Saved,_) | Ast.MetaFunc(name,TC.Saved,_)
    | Ast.MetaLocalFunc(name,TC.Saved,_) -> [metaid name]
    | _ -> k i in

  let rec type_collect res = function
      TC.ConstVol(_,ty) | TC.Pointer(ty) | TC.FunctionPointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.MetaType(tyname,TC.Saved,_) -> bind [tyname] res
    | ty -> res in

  let astfvexpr recursor k e =
    let tymetas =
      match Ast.unwrap e with
	Ast.MetaConst(name,_,Some type_list,_)
      |	Ast.MetaExpr(name,_,Some type_list,_) ->
	  List.fold_left type_collect option_default type_list
      |	_ -> [] in
    let vars =
      match Ast.unwrap e with
	Ast.MetaConst(name,TC.Saved,_,_) | Ast.MetaErr(name,TC.Saved,_)
      | Ast.MetaExpr(name,TC.Saved,_,_) | Ast.MetaExprList(name,TC.Saved,_) ->
	  [metaid name]
      | _ -> k e in
    bind tymetas vars in

  let astfvtypeC recursor k ty =
    match Ast.unwrap ty with
      Ast.MetaType(name,TC.Saved,_) -> [metaid name]
    | _ -> k ty in

  let astfvparam recursor k p =
    match Ast.unwrap p with
      Ast.MetaParam(name,TC.Saved,_) | Ast.MetaParamList(name,TC.Saved,_) ->
	[metaid name]
    | _ -> k p in

  let astfvrule_elem recursor k re =
    nub (*within a rule_elem, pattern3 manages the coherence of the bindings*)
      (match Ast.unwrap re with
	Ast.MetaRuleElem(name,TC.Saved,_) | Ast.MetaStmt(name,TC.Saved,_,_)
      | Ast.MetaStmtList(name,TC.Saved,_) -> [metaid name]
      | Ast.Define(_,id,_,db) ->
	  (match Ast.unwrap db with
	    Ast.DMetaId(name,TC.Saved) -> bind (k re) [metaid name]
	  | _ -> k re)
      | _ -> k re) in

  let mcode r e = [] in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    astfvident astfvexpr donothing astfvtypeC donothing astfvparam
    donothing astfvrule_elem donothing donothing donothing donothing

(* ---------------------------------------------------------------- *)

(* For the rules under a given metavariable declaration, collect all of the
variables that occur in the plus code *)

let cip_mcodekind r mck =
  let process_anything_list_list anythings =
    let astfvs = collect_all_refs.V.combiner_anything in
    List.fold_left (@) []
      (List.map (function l -> List.fold_left (@) [] (List.map astfvs l))
	 anythings) in
  match mck with
    Ast.MINUS(_,anythings) -> process_anything_list_list anythings
  | Ast.CONTEXT(_,befaft) ->
      (match befaft with
	Ast.BEFORE(ll) -> process_anything_list_list ll
      | Ast.AFTER(ll) -> process_anything_list_list ll
      | Ast.BEFOREAFTER(llb,lla) ->
	  (process_anything_list_list lla) @
	  (process_anything_list_list llb)
      | Ast.NOTHING -> [])
  | Ast.PLUS -> []

let collect_in_plus_term =
  let bind x y = x @ y in
  let option_default = [] in
  let donothing r k e = k e in

  let mcode r (_,_,mck) = cip_mcodekind r mck in

  (* case for things with bef/aft mcode *)

  let astfvrule_elem recursor k re =
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fi,nm,_,params,_) ->
	let fi_metas =
	  List.concat
	    (List.map
	       (function
		   Ast.FType(ty) -> collect_all_refs.V.combiner_fullType ty
		 | _ -> [])
	       fi) in
	let nm_metas = collect_all_refs.V.combiner_ident nm in
	let param_metas =
	  match Ast.unwrap params with
	    Ast.DOTS(params) | Ast.CIRCLES(params) ->
	      List.concat
		(List.map
		   (function p ->
		     match Ast.unwrap p with
		       Ast.VoidParam(t) | Ast.Param(t,_) ->
			 collect_all_refs.V.combiner_fullType t
		     | _ -> [])
		   params)
	  | _ -> failwith "not allowed for params" in
	bind fi_metas
	  (bind nm_metas
	     (bind param_metas
		(bind (cip_mcodekind recursor bef) (k re))))
    | Ast.Decl(bef,_,_) ->
	bind (cip_mcodekind recursor bef) (k re)
    | _ -> k re in

  let astfvstatement recursor k s =
    match Ast.unwrap s with
      Ast.IfThen(_,_,(_,_,_,aft)) | Ast.IfThenElse(_,_,_,_,(_,_,_,aft))
    | Ast.While(_,_,(_,_,_,aft)) | Ast.For(_,_,(_,_,_,aft)) ->
	bind (k s) (cip_mcodekind recursor aft)
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing astfvrule_elem astfvstatement donothing donothing donothing

let collect_in_plus minirules =
  nub
    (List.concat
       (List.map collect_in_plus_term.V.combiner_top_level minirules))

(* ---------------------------------------------------------------- *)

(* For the rules under a given metavariable declaration, collect all of the
variables that occur only once and more than once in the minus code *)

let collect_all_multirefs minirules =
  let refs = List.map collect_all_refs.V.combiner_top_level minirules in
  collect_unitary_nonunitary (List.concat refs)

(* ---------------------------------------------------------------- *)

(* classify as unitary (no binding) or nonunitary (env binding) or saved
(witness binding) *)

let classify_variables metavars minirules used_after =
  let metavars = List.map Ast.get_meta_name metavars in
  let (unitary,nonunitary) = collect_all_multirefs minirules in
  let inplus = collect_in_plus minirules in
  
  let donothing r k e = k e in
  let mcode x = x in
  let check_unitary name inherited =
    if List.mem name inplus or List.mem name used_after
    then TC.Saved
    else if not inherited && List.mem name unitary
    then TC.Unitary
    else TC.Nonunitary in

  let get_option f = function Some x -> Some (f x) | None -> None in

  let classify (name,_,_) =
    let inherited = not (List.mem name metavars) in
    (check_unitary name inherited,inherited) in

  let ident r k e =
    match Ast.unwrap e with
      Ast.MetaId(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaId(name,unitary,inherited))
    | Ast.MetaFunc(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaFunc(name,unitary,inherited))
    | Ast.MetaLocalFunc(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaLocalFunc(name,unitary,inherited))
    | _ -> k e in

  let rec type_infos = function
      TC.ConstVol(cv,ty) -> TC.ConstVol(cv,type_infos ty)
    | TC.Pointer(ty) -> TC.Pointer(type_infos ty)
    | TC.FunctionPointer(ty) -> TC.FunctionPointer(type_infos ty)
    | TC.Array(ty) -> TC.Array(type_infos ty)
    | TC.MetaType(name,_,_) ->
	let (unitary,inherited) = classify (name,(),()) in
	Type_cocci.MetaType(name,unitary,inherited)
    | ty -> ty in

  let expression r k e =
    match Ast.unwrap e with
      Ast.MetaConst(name,_,ty,_) ->
	let (unitary,inherited) = classify name in
	let ty = get_option (List.map type_infos) ty in
	Ast.rewrap e (Ast.MetaConst(name,unitary,ty,inherited))
    | Ast.MetaErr(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaErr(name,unitary,inherited))
    | Ast.MetaExpr(name,_,ty,_) ->
	let (unitary,inherited) = classify name in
	let ty = get_option (List.map type_infos) ty in
	Ast.rewrap e (Ast.MetaExpr(name,unitary,ty,inherited))
    | Ast.MetaExprList(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaExprList(name,unitary,inherited))
    | _ -> k e in

  let typeC r k e =
    match Ast.unwrap e with
      Ast.MetaType(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaType(name,unitary,inherited))
    | _ -> k e in

  let param r k e =
    match Ast.unwrap e with
      Ast.MetaParam(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaParam(name,unitary,inherited))
    | Ast.MetaParamList(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaParamList(name,unitary,inherited))
    | _ -> k e in

  let define_body r b =
    match Ast.unwrap b with
      Ast.DMetaId(name,_) ->
	let (unitary,_) = classify name in
	Ast.rewrap b (Ast.DMetaId(name,unitary))
    | Ast.DStm(re) -> Ast.rewrap b (Ast.DStm(r.V.rebuilder_rule_elem re)) in
  
  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.MetaStmt(name,_,msi,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaStmt(name,unitary,msi,inherited))
    | Ast.MetaStmtList(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaStmtList(name,unitary,inherited))
    | Ast.Define(def,id,params,body) ->
	Ast.rewrap e (Ast.Define(def,id,params,define_body r body))
    | _ -> k e in

  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      donothing donothing donothing donothing
      ident expression donothing typeC donothing param donothing rule_elem
      donothing donothing donothing donothing in

  List.map fn.V.rebuilder_top_level minirules

(* ---------------------------------------------------------------- *)

(* For a minirule, collect the set of non-local (not in "bound") variables that
are referenced.  Store them in a hash table. *)

(* bound means the metavariable was declared previously, not locally *)

(* Highly inefficient, because we call collect_all_refs on nested code
multiple times.  But we get the advantage of not having too many variants
of the same functions. *)

let astfvs metavars bound =
  let fresh =
    List.fold_left
      (function prev ->
	function
	    Ast.MetaFreshIdDecl(_,_) as x -> (Ast.get_meta_name x)::prev
	  | _ -> prev)
      [] metavars in

  let collect_fresh = List.filter (function x -> List.mem x fresh) in

  (* cases for the elements of anything *)
  let astfvrule_elem recursor k re =
    let free =
      Common.union_set
	(nub (collect_all_refs.V.combiner_rule_elem re))
	(collect_in_plus_term.V.combiner_rule_elem re) in
    let (unbound,inherited) =
      List.partition (function x -> not(List.mem x bound)) free in
    let (re,l,_,_,_,_,d) = k re in
    (re,l,unbound,collect_fresh unbound,inherited,[],d) in

  let astfvstatement recursor k s =
    let free =
      Common.union_set
	(nub (collect_all_refs.V.combiner_statement s))
	(collect_in_plus_term.V.combiner_statement s) in
    let classify free =
      let (unbound,inherited) =
	List.partition (function x -> not(List.mem x bound)) free in
      (unbound,collect_fresh unbound,inherited) in
    let (s,l,_,_,_,_,d) = k s in
    let s =
      match s with
	Ast.IfThen(header,branch,(_,_,_,aft)) ->
	  let (unbound,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) in
	  Ast.IfThen(header,branch,(unbound,fresh,inherited,aft))
      | Ast.IfThenElse(header,branch1,els,branch2,(_,_,_,aft)) ->
	  let (unbound,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) in
	  Ast.IfThenElse(header,branch1,els,branch2,
			 (unbound,fresh,inherited,aft))
      | Ast.While(header,body,(_,_,_,aft)) ->
	  let (unbound,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) in
	  Ast.While(header,body,(unbound,fresh,inherited,aft))
      | Ast.For(header,body,(_,_,_,aft)) ->
	  let (unbound,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) in
	  Ast.For(header,body,(unbound,fresh,inherited,aft))
      |	_ -> s in
    let (unbound,fresh,inherited) = classify free in
    (s,l,unbound,collect_fresh unbound,inherited,[],d) in

  let astfvstatement_dots recursor k sd =
    let free =
      Common.union_set
	(nub (collect_all_refs.V.combiner_statement_dots sd))
	(collect_in_plus_term.V.combiner_statement_dots sd) in
    let (unbound,inherited) =
      List.partition (function x -> not(List.mem x bound)) free in
    let (sd,l,_,_,_,_,d) = k sd in
    (sd,l,unbound,collect_fresh unbound,inherited,[],d) in

  let astfvtoplevel recursor k tl =
    let saved = collect_saved.V.combiner_top_level tl in
    let (tl,l,unbound,fresh,inherited,_,d) = k tl in
    (tl,l,unbound,fresh,inherited,saved,d) in

  let mcode x = x in
  let donothing r k e = k e in

  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing astfvstatement_dots donothing
    donothing donothing donothing donothing donothing donothing donothing
    astfvrule_elem astfvstatement donothing astfvtoplevel donothing

let collect_astfvs rules =
  let rec loop bound = function
      [] -> []
    | (metavars,(nm,deps,minirules))::rules ->
	let bound =
	  Common.minus_set bound (List.map Ast.get_meta_name metavars) in
	(nm,deps,
	 (List.map (astfvs metavars bound).V.rebuilder_top_level minirules))::
	(loop ((List.map Ast.get_meta_name metavars)@bound) rules) in
  loop [] rules

(* ---------------------------------------------------------------- *)

(* collect used after lists, per minirule *)

(* defined is a list of variables that were declared in a previous metavar
declaration *)

(* Top-level used after: For each rule collect the set of variables that
are inherited, ie used but not defined.  These are accumulated back to
their point of definition. *)


let collect_top_level_used_after metavar_rule_list =
  let (used_after,used_after_lists) =
    List.fold_right
      (function (metavar_list,(name,dependencies,rule)) ->
	function (used_after,used_after_lists) ->
	  let locally_defined = List.map Ast.get_meta_name metavar_list in
	  let continue_propagation =
	    List.filter (function x -> not(List.mem x locally_defined))
	      used_after in
	  let free_vars =
	    Common.union_set (nub (collect_all_rule_refs rule))
	      (collect_in_plus rule) in
	  let inherited =
	    List.filter (function x -> not (List.mem x locally_defined))
	      free_vars in
	  (Common.union_set inherited continue_propagation,
	   used_after::used_after_lists))
      metavar_rule_list ([],[]) in
  match used_after with
    [] -> used_after_lists
  | _ ->
      failwith
	(Printf.sprintf "collect_top_level_used_after: unbound variables %s"
	   (String.concat " " (List.map (function (_,x) -> x) used_after)))
	
let collect_local_used_after metavars minirules used_after =
  let locally_defined = List.map Ast.get_meta_name metavars in
  let rec loop defined = function
      [] -> (used_after,[])
    | minirule::rest ->
	let local_free_vars =
	  List.filter (function x -> List.mem x locally_defined)
	    (Common.union_set
	       (nub (collect_all_minirule_refs minirule))
	       (collect_in_plus_term.V.combiner_top_level minirule)) in
	let new_defined = Common.union_set local_free_vars defined in
	let (mini_used_after,mini_used_after_lists) = loop new_defined rest in
	let local_used = Common.union_set local_free_vars mini_used_after in
	let (new_used_after,new_list) =
	  List.partition (function x -> List.mem x defined) mini_used_after in
	let new_used_after = Common.union_set local_used new_used_after in
	(new_used_after,new_list::mini_used_after_lists) in
  let (_,used_after_lists) = loop [] minirules in
  used_after_lists

let collect_used_after metavar_rule_list =
  let used_after_lists = collect_top_level_used_after metavar_rule_list in
  List.map2
    (function (metavars,(name,dependencies,minirules)) ->
      function used_after ->
	collect_local_used_after metavars minirules used_after)
    metavar_rule_list used_after_lists

(* ---------------------------------------------------------------- *)

(* entry point *)

let free_vars rules =
  let metavars = List.map (function (mv,rule) -> mv) rules in
  let used_after_lists = collect_used_after rules in
  let new_rules =
    List.map2
      (function (mv,(nm,dep,r)) ->
	function ua ->
	  (nm,dep,classify_variables mv r (List.concat ua)))
      rules used_after_lists in
  let new_rules = collect_astfvs (List.combine metavars new_rules) in
  (*List.iter
    (List.iter
       (function l -> Printf.printf "one rule: %s\n" (String.concat " " l)))
    used_after_lists;*)
  (new_rules,used_after_lists)
