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

let collect_refs include_constraints =
  let bind x y = x @ y in
  let option_default = [] in

  let donothing recursor k e = k e in (* just combine in the normal way *)

  let donothing_a recursor k e = (* anything is not wrapped *)
    k e in (* just combine in the normal way *)

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

  let metaid (x,_,_,_) = x in

  let astfvident recursor k i =
    bind (k i)
      (match Ast.unwrap i with
	Ast.MetaId(name,_,_,_) | Ast.MetaFunc(name,_,_,_)
      | Ast.MetaLocalFunc(name,_,_,_) -> [metaid name]
      | _ -> option_default) in

  let rec type_collect res = function
      TC.ConstVol(_,ty) | TC.Pointer(ty) | TC.FunctionPointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.MetaType(tyname,_,_) -> bind [tyname] res
    | TC.SignedT(_,Some ty) -> type_collect res ty
    | ty -> res in

  let astfvexpr recursor k e =
    bind (k e)
      (match Ast.unwrap e with
	Ast.MetaExpr(name,_,_,Some type_list,_,_) ->
	  let types = List.fold_left type_collect option_default type_list in
	  bind [metaid name] types
      | Ast.MetaErr(name,_,_,_) | Ast.MetaExpr(name,_,_,_,_,_) -> [metaid name]
      | Ast.MetaExprList(name,None,_,_) -> [metaid name]
      | Ast.MetaExprList(name,Some (lenname,_,_),_,_) ->
	  [metaid name;metaid lenname]
      | Ast.DisjExpr(exps) -> bind_disj (List.map k exps)
      | _ -> option_default) in

  let astfvdecls recursor k d =
    bind (k d)
      (match Ast.unwrap d with
	Ast.DisjDecl(decls) -> bind_disj (List.map k decls)
      | _ -> option_default) in

  let astfvfullType recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.DisjType(types) -> bind_disj (List.map k types)
      | _ -> option_default) in

  let astfvtypeC recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaType(name,_,_) -> [metaid name]
      | _ -> option_default) in

  let astfvinit recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaInit(name,_,_) -> [metaid name]
      | _ -> option_default) in

  let astfvparam recursor k p =
    bind (k p)
      (match Ast.unwrap p with
	Ast.MetaParam(name,_,_) -> [metaid name]
      | Ast.MetaParamList(name,None,_,_) -> [metaid name]
      | Ast.MetaParamList(name,Some(lenname,_,_),_,_) ->
	  [metaid name;metaid lenname]
      | _ -> option_default) in

  let astfvrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    bind (k re)
      (nub
	 (match Ast.unwrap re with
	   Ast.MetaRuleElem(name,_,_) | Ast.MetaStmt(name,_,_,_)
	 | Ast.MetaStmtList(name,_,_) -> [metaid name]
	 | _ -> option_default)) in

  let astfvstatement recursor k s =
    bind (k s)
      (match Ast.unwrap s with
	Ast.Disj(stms) ->
	  bind_disj (List.map recursor.V.combiner_statement_dots stms)
      | _ -> option_default) in

  let mcode r mc =
    if include_constraints
    then
      match Ast.get_pos_var mc with
	Ast.MetaPos(name,constraints,_,_,_) -> (metaid name)::constraints
      | _ -> option_default
    else option_default in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    astfvident astfvexpr astfvfullType astfvtypeC astfvinit astfvparam
    astfvdecls astfvrule_elem astfvstatement donothing donothing donothing_a

let collect_all_refs = collect_refs true
let collect_non_constraint_refs = collect_refs false

let collect_all_rule_refs minirules =
  List.fold_left (@) []
    (List.map collect_all_refs.V.combiner_top_level minirules)

let collect_all_minirule_refs = collect_all_refs.V.combiner_top_level

(* ---------------------------------------------------------------- *)

let collect_saved =
  let bind = Common.union_set in
  let option_default = [] in

  let donothing recursor k e = k e in (* just combine in the normal way *)

  let metaid (x,_,_,_) = x in

  (* cases for metavariables *)
  let astfvident recursor k i =
    bind (k i)
      (match Ast.unwrap i with
	Ast.MetaId(name,_,TC.Saved,_) | Ast.MetaFunc(name,_,TC.Saved,_)
      | Ast.MetaLocalFunc(name,_,TC.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let rec type_collect res = function
      TC.ConstVol(_,ty) | TC.Pointer(ty) | TC.FunctionPointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.MetaType(tyname,TC.Saved,_) -> bind [tyname] res
    | TC.SignedT(_,Some ty) -> type_collect res ty
    | ty -> res in

  let astfvexpr recursor k e =
    let tymetas =
      match Ast.unwrap e with
	Ast.MetaExpr(name,_,_,Some type_list,_,_) ->
	  List.fold_left type_collect option_default type_list
      |	_ -> [] in
    let vars =
      bind (k e)
	(match Ast.unwrap e with
	  Ast.MetaErr(name,_,TC.Saved,_) | Ast.MetaExpr(name,_,TC.Saved,_,_,_)
	| Ast.MetaExprList(name,None,TC.Saved,_) -> [metaid name]
	| Ast.MetaExprList(name,Some (lenname,ls,_),ns,_) ->
	    let namesaved =
	      match ns with TC.Saved -> [metaid name] | _ -> [] in
	    let lensaved =
	      match ls with TC.Saved -> [metaid lenname] | _ -> [] in
	    lensaved @ namesaved
	| _ -> option_default) in
    bind tymetas vars in

  let astfvtypeC recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaType(name,TC.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvinit recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaInit(name,TC.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvparam recursor k p =
    bind (k p)
      (match Ast.unwrap p with
	Ast.MetaParam(name,TC.Saved,_)
      | Ast.MetaParamList(name,None,_,_) -> [metaid name]
      | Ast.MetaParamList(name,Some (lenname,ls,_),ns,_) ->
	  let namesaved =
	    match ns with TC.Saved -> [metaid name] | _ -> [] in
	  let lensaved =
	    match ls with TC.Saved -> [metaid lenname] | _ -> [] in
	  lensaved @ namesaved
      | _ -> option_default) in

  let astfvrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    bind (k re)
      (nub
	 (match Ast.unwrap re with
	   Ast.MetaRuleElem(name,TC.Saved,_) | Ast.MetaStmt(name,TC.Saved,_,_)
	 | Ast.MetaStmtList(name,TC.Saved,_) -> [metaid name]
	 | _ -> option_default)) in

  let mcode r e =
    match Ast.get_pos_var e with
      Ast.MetaPos(name,_,_,TC.Saved,_) -> [metaid name]
    | _ -> option_default in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    astfvident astfvexpr donothing astfvtypeC astfvinit astfvparam
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

  (* no positions in the + code *)
  let mcode r (_,_,mck,_) = cip_mcodekind r mck in

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
    | Ast.While(_,_,(_,_,_,aft)) | Ast.For(_,_,(_,_,_,aft))
    | Ast.Iterator(_,_,(_,_,_,aft)) ->
	bind (k s) (cip_mcodekind recursor aft)
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
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
  let check_unitary name inherited =
    if List.mem name inplus or List.mem name used_after
    then TC.Saved
    else if not inherited && List.mem name unitary
    then TC.Unitary
    else TC.Nonunitary in

  let get_option f = function Some x -> Some (f x) | None -> None in

  let classify (name,_,_,_) =
    let inherited = not (List.mem name metavars) in
    (check_unitary name inherited,inherited) in

  let mcode mc =
    match Ast.get_pos_var mc with
      Ast.MetaPos(name,constraints,per,unitary,inherited) ->
	let (unitary,inherited) = classify name in
	Ast.set_pos_var (Ast.MetaPos(name,constraints,per,unitary,inherited))
	  mc
    | _ -> mc in

  let ident r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaId(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaId(name,constraints,unitary,inherited))
    | Ast.MetaFunc(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaFunc(name,constraints,unitary,inherited))
    | Ast.MetaLocalFunc(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaLocalFunc(name,constraints,unitary,inherited))
    | _ -> e in

  let rec type_infos = function
      TC.ConstVol(cv,ty) -> TC.ConstVol(cv,type_infos ty)
    | TC.Pointer(ty) -> TC.Pointer(type_infos ty)
    | TC.FunctionPointer(ty) -> TC.FunctionPointer(type_infos ty)
    | TC.Array(ty) -> TC.Array(type_infos ty)
    | TC.MetaType(name,_,_) ->
	let (unitary,inherited) = classify (name,(),(),Ast.NoMetaPos) in
	Type_cocci.MetaType(name,unitary,inherited)
    | TC.SignedT(sgn,Some ty) -> TC.SignedT(sgn,Some (type_infos ty))
    | ty -> ty in

  let expression r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaErr(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaErr(name,constraints,unitary,inherited))
    | Ast.MetaExpr(name,constraints,_,ty,form,_) ->
	let (unitary,inherited) = classify name in
	let ty = get_option (List.map type_infos) ty in
	Ast.rewrap e (Ast.MetaExpr(name,constraints,unitary,ty,form,inherited))
    | Ast.MetaExprList(name,None,_,_) ->
	(* lenname should have the same properties of being unitary or
	   inherited as name *)
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaExprList(name,None,unitary,inherited))
    | Ast.MetaExprList(name,Some(lenname,_,_),_,_) ->
	(* lenname should have the same properties of being unitary or
	   inherited as name *)
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaExprList
	     (name,Some(lenname,lenunitary,leninherited),unitary,inherited))
    | _ -> e in

  let typeC r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaType(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaType(name,unitary,inherited))
    | _ -> e in

  let init r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaInit(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaInit(name,unitary,inherited))
    | _ -> e in

  let param r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaParam(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaParam(name,unitary,inherited))
    | Ast.MetaParamList(name,None,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaParamList(name,None,unitary,inherited))
    | Ast.MetaParamList(name,Some (lenname,_,_),_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaParamList
	     (name,Some (lenname,lenunitary,leninherited),unitary,inherited))
    | _ -> e in

  let rule_elem r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaStmt(name,_,msi,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaStmt(name,unitary,msi,inherited))
    | Ast.MetaStmtList(name,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaStmtList(name,unitary,inherited))
    | _ -> e in

  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing
      ident expression donothing typeC init param donothing rule_elem
      donothing donothing donothing donothing in

  List.map fn.V.rebuilder_top_level minirules

(* ---------------------------------------------------------------- *)

(* For a minirule, collect the set of non-local (not in "bound") variables that
are referenced.  Store them in a hash table. *)

(* bound means the metavariable was declared previously, not locally *)

(* Highly inefficient, because we call collect_all_refs on nested code
multiple times.  But we get the advantage of not having too many variants
of the same functions. *)

(* Inherited doesn't include position constraints.  If they are not bound
then there is no constraint. *)

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
    let minus_free = nub (collect_all_refs.V.combiner_rule_elem re) in
    let minus_nc_free =
      nub (collect_non_constraint_refs.V.combiner_rule_elem re) in
    let plus_free = collect_in_plus_term.V.combiner_rule_elem re in
    let free = Common.union_set minus_free plus_free in
    let nc_free = Common.union_set minus_nc_free plus_free in
    let unbound =
      List.filter (function x -> not(List.mem x bound)) free in
    let inherited =
      List.filter (function x -> List.mem x bound) nc_free in
    let munbound =
      List.filter (function x -> not(List.mem x bound)) minus_free in
    {(k re) with
      Ast.free_vars = unbound;
      Ast.minus_free_vars = munbound;
      Ast.fresh_vars = collect_fresh unbound;
      Ast.inherited = inherited;
      Ast.saved_witness = []} in

  let astfvstatement recursor k s =
    let minus_free = nub (collect_all_refs.V.combiner_statement s) in
    let minus_nc_free =
      nub (collect_non_constraint_refs.V.combiner_statement s) in
    let plus_free = collect_in_plus_term.V.combiner_statement s in
    let free = Common.union_set minus_free plus_free in
    let nc_free = Common.union_set minus_nc_free plus_free in
    let classify free minus_free =
      let (unbound,inherited) =
	List.partition (function x -> not(List.mem x bound)) free in
      let munbound =
	List.filter (function x -> not(List.mem x bound)) minus_free in
      (unbound,munbound,collect_fresh unbound,inherited) in
    let res = k s in
    let s =
      match Ast.unwrap res with
	Ast.IfThen(header,branch,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) [] in
	  Ast.IfThen(header,branch,(unbound,fresh,inherited,aft))
      | Ast.IfThenElse(header,branch1,els,branch2,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) [] in
	  Ast.IfThenElse(header,branch1,els,branch2,
			 (unbound,fresh,inherited,aft))
      | Ast.While(header,body,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) [] in
	  Ast.While(header,body,(unbound,fresh,inherited,aft))
      | Ast.For(header,body,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) [] in
	  Ast.For(header,body,(unbound,fresh,inherited,aft))
      | Ast.Iterator(header,body,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) =
	    classify (cip_mcodekind collect_in_plus_term aft) [] in
	  Ast.Iterator(header,body,(unbound,fresh,inherited,aft))
      |	s -> s in

    let (unbound,munbound,fresh,_) = classify free minus_free in
    let inherited =
      List.filter (function x -> List.mem x bound) nc_free in
    {res with
      Ast.node = s;
      Ast.free_vars = unbound;
      Ast.minus_free_vars = munbound;
      Ast.fresh_vars = collect_fresh unbound;
      Ast.inherited = inherited;
      Ast.saved_witness = []} in

  let astfvstatement_dots recursor k sd =
    let minus_free = nub (collect_all_refs.V.combiner_statement_dots sd) in
    let minus_nc_free =
      nub (collect_non_constraint_refs.V.combiner_statement_dots sd) in
    let plus_free = collect_in_plus_term.V.combiner_statement_dots sd in
    let free = Common.union_set minus_free plus_free in
    let nc_free = Common.union_set minus_nc_free plus_free in
    let unbound =
      List.filter (function x -> not(List.mem x bound)) free in
    let inherited =
      List.filter (function x -> List.mem x bound) nc_free in
    let munbound =
      List.filter (function x -> not(List.mem x bound)) minus_free in
    {(k sd) with
      Ast.free_vars = unbound;
      Ast.minus_free_vars = munbound;
      Ast.fresh_vars = collect_fresh unbound;
      Ast.inherited = inherited;
      Ast.saved_witness = []} in

  let astfvtoplevel recursor k tl =
    let saved = collect_saved.V.combiner_top_level tl in
    {(k tl) with Ast.saved_witness = saved} in

  let mcode x = x in
  let donothing r k e = k e in

  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing astfvstatement_dots donothing
    donothing donothing donothing donothing donothing donothing donothing
    astfvrule_elem astfvstatement donothing astfvtoplevel donothing

(*
let collect_astfvs rules =
  let rec loop bound = function
      [] -> []
    | (metavars,(nm,rule_info,minirules))::rules ->
	let bound =
	  Common.minus_set bound (List.map Ast.get_meta_name metavars) in
	(nm,rule_info,
	 (List.map (astfvs metavars bound).V.rebuilder_top_level minirules))::
	(loop ((List.map Ast.get_meta_name metavars)@bound) rules) in
  loop [] rules
*)

let collect_astfvs rules =
  let rec loop bound = function
      [] -> []
    | (metavars, rule)::rules ->
        match rule with
          Ast.ScriptRule (_,_,_,_) ->
	    (* bound stays as is because script rules have no names, so no
	       inheritance is possible *)
	    rule::(loop bound rules)
        | Ast.CocciRule (nm, rule_info, minirules, isexp, ruletype) ->
          let bound =
            Common.minus_set bound (List.map Ast.get_meta_name metavars) in
          (Ast.CocciRule
	     (nm, rule_info,
	      (List.map (astfvs metavars bound).V.rebuilder_top_level
		 minirules),
	      isexp, ruletype))::
            (loop ((List.map Ast.get_meta_name metavars)@bound) rules) in
  loop [] rules

(* ---------------------------------------------------------------- *)
(* position variables that appear as a constraint on another position variable.
a position variable also cannot appear both positively and negatively in a
single rule. *)

let get_neg_pos_list (_,rule) used_after_list =
  let donothing r k e = k e in
  let bind (p1,np1) (p2,np2) =
    (Common.union_set p1 p2, Common.union_set np1 np2) in
  let option_default = ([],[]) in
  let metaid (x,_,_,_) = x in
  let mcode r mc =
    match Ast.get_pos_var mc with
      Ast.MetaPos(name,constraints,Ast.PER,_,_) ->
	([metaid name],constraints)
    | Ast.MetaPos(name,constraints,Ast.ALL,_,_) ->
	([],(metaid name)::constraints)
    | _ -> option_default in
  let v =
    V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing in
  match rule with
    Ast.CocciRule(_,_,minirules,_,_) ->
      List.map
	(function toplevel ->
	  let (positions,neg_positions) = v.V.combiner_top_level toplevel in
	  (if List.exists (function p -> List.mem p neg_positions) positions
	  then
	    failwith
	      "a variable cannot be used both as a position and a constraint");
	  neg_positions)
	minirules
  | Ast.ScriptRule _ -> [] (*no negated positions*)

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
      (function (metavar_list,r) ->
	function (used_after,used_after_lists) ->
	  let locally_defined = List.map Ast.get_meta_name metavar_list in
	  let continue_propagation =
	    List.filter (function x -> not(List.mem x locally_defined))
	      used_after in
	  let free_vars =
            match r with
              Ast.ScriptRule (_,_,mv,_) ->
                List.map (function (_,(r,v)) -> (r,v)) mv
            | Ast.CocciRule (_,_,rule,_,_) ->
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
      [] -> (used_after,[],[])
    | minirule::rest ->
	let free_vars =
	  Common.union_set
	    (nub (collect_all_minirule_refs minirule))
	    (collect_in_plus_term.V.combiner_top_level minirule) in
	let local_free_vars =
	  List.filter (function x -> List.mem x locally_defined) free_vars in
	let new_defined = Common.union_set local_free_vars defined in
	let (mini_used_after,fvs_lists,mini_used_after_lists) =
	  loop new_defined rest in
	let local_used = Common.union_set local_free_vars mini_used_after in
	let (new_used_after,new_list) =
	  List.partition (function x -> List.mem x defined) mini_used_after in
	let new_used_after = Common.union_set local_used new_used_after in
	(new_used_after,free_vars::fvs_lists,
	 new_list::mini_used_after_lists) in
  let (_,fvs_lists,used_after_lists) = loop [] minirules in
  (fvs_lists,used_after_lists)


let collect_used_after metavar_rule_list =
  let used_after_lists = collect_top_level_used_after metavar_rule_list in
  List.map2
    (function (metavars,r) ->
      function used_after ->
        match r with
          Ast.ScriptRule (_,_,mv,_) -> ([], [used_after])
        | Ast.CocciRule (name, rule_info, minirules, _,_) ->
          collect_local_used_after metavars minirules used_after
    )
    metavar_rule_list used_after_lists

(* ---------------------------------------------------------------- *)
(* entry point *)

let free_vars rules =
  let metavars = List.map (function (mv,rule) -> mv) rules in
  let (fvs_lists,used_after_lists) = List.split (collect_used_after rules) in
  let neg_pos_lists = List.map2 get_neg_pos_list rules used_after_lists in
  let positions_list = (* for all rules, assume all positions are used after *)
    List.map
      (function (mv, r) ->
         match r with
           Ast.ScriptRule _ -> []
         | Ast.CocciRule (_,_,rule,_,_) ->
           let positions =
             List.fold_left
               (function prev ->
                 function Ast.MetaPosDecl(_,nm) -> nm::prev | _ -> prev)
               [] mv in
           List.map (function _ -> positions) rule)
      rules in
  let new_rules =
    List.map2
      (function (mv,r) ->
	function ua ->
          match r with
            Ast.ScriptRule _ -> r
          | Ast.CocciRule (nm, rule_info, r, is_exp,ruletype) ->
	      Ast.CocciRule
		(nm, rule_info, classify_variables mv r (List.concat ua),
		 is_exp,ruletype))
      rules used_after_lists in
  let new_rules = collect_astfvs (List.combine metavars new_rules) in
  (metavars,new_rules,
   fvs_lists,neg_pos_lists,used_after_lists,positions_list)
