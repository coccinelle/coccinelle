(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* For each rule return the list of variables that are used after it.
Also augment various parts of each rule with unitary, inherited, and freshness
informations *)

(* metavar decls should be better integrated into computations of free
variables in plus code *)

module Ast = Ast_cocci
module V = Visitor_ast

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
     perhaps better to avoid.  Also, unitarily is represented as occurring once,
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
	Ast.MetaId(name,idconstraint,_,_) | Ast.MetaFunc(name,idconstraint,_,_)
      | Ast.MetaLocalFunc(name,idconstraint,_,_) ->
	  let metas =
	    if include_constraints
	    then Ast.cstr_meta_names idconstraint
	    else [] in
	  bind metas [metaid name]
      | Ast.DisjId(ids) -> bind_disj (List.map k ids)
      | _ -> option_default) in

  let type_collect res ty = bind res (Ast.meta_names_of_fullType ty) in

  let constraints cstr =
    if include_constraints then Ast.cstr_pos_meta_names cstr
    else [] in

  let listlen l =
    match l with
      Ast.MetaListLen(lenname,cstr,_,_) ->
	bind (constraints cstr) [metaid lenname]
    | _ -> [] in

  let listlen_option l =
    match l with
      None -> option_default
    | Some l -> listlen l in

  let astfvexpr recursor k e =
    bind (k e)
      (match Ast.unwrap e with
	Ast.MetaExpr(name,constraints,_,Some type_list,_,_,bitfield) ->
	  let types =
	    (* problem: if there are multiple types, then none in particular
	       is needed *)
	    if include_constraints || List.length type_list = 1
	    then List.fold_left type_collect option_default type_list
	    else [] in
	  let extra =
	    if include_constraints
	    then Ast.cstr_pos_meta_names constraints
	    else [] in
	  let bitfield' = listlen_option bitfield in
	  bind bitfield' (bind extra (bind [metaid name] types))
      | Ast.MetaErr(name,cstr,_,_) ->
	  bind (constraints cstr) [metaid name]
      | Ast.MetaExpr(name,cstr,_,_,_,_,bitfield) ->
	  let bitfield' = listlen_option bitfield in
	  bind bitfield' (bind (constraints cstr) [metaid name])
      | Ast.MetaExprList(name,len,cstr,_,_) ->
	  bind (constraints cstr) (bind (listlen len) [metaid name])
      | Ast.DisjExpr(exps) -> bind_disj (List.map k exps)
      | _ -> option_default) in

  let astfvfrag recursor k ft =
    bind (k ft)
      (match Ast.unwrap ft with
	Ast.MetaFormatList
	  (pct,name,len,cstr,_,_) ->
	  bind (constraints cstr) (bind (listlen len) [metaid name])
      | _ -> option_default) in

  let astfvfmt recursor k ft =
    bind (k ft)
      (match Ast.unwrap ft with
	Ast.MetaFormat(name,cstr,_,_) ->
	  bind (constraints cstr) [metaid name]
      | _ -> option_default) in

  let rec collect_assign_names aop =
    match Ast.unwrap aop with
      Ast.MetaAssign(name,cstr,_,_) ->
	bind (constraints cstr) [metaid name]
    | _ -> option_default in

  let astfvassignop recursor k bop =
    bind (k bop) (collect_assign_names bop) in

  let rec collect_binary_names bop =
    match Ast.unwrap bop with
      Ast.MetaBinary(name,cstr,_,_) ->
	bind (constraints cstr) [metaid name]
    | _ -> option_default in

  let astfvbinaryop recursor k bop =
    bind (k bop) (collect_binary_names bop) in

  let astfvdecls recursor k d =
    bind (k d)
      (match Ast.unwrap d with
	Ast.MetaDecl(name,cstr,_,_) ->
	  bind (constraints cstr) [metaid name]
      | Ast.DisjDecl(decls) -> bind_disj (List.map k decls)
      | _ -> option_default) in

  let astfvfields recursor k d =
    bind (k d)
      (match Ast.unwrap d with
	Ast.MetaField(name,cstr,_,_) ->
	  bind (constraints cstr) [metaid name]
      | Ast.MetaFieldList(name,len,cstr,_,_) ->
	  bind (constraints cstr) (bind (listlen len) [metaid name])
      | Ast.DisjField(decls) -> bind_disj (List.map k decls)
      | _ -> option_default) in

  let astfvfullType recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.DisjType(types) -> bind_disj (List.map k types)
      | _ -> option_default) in

  let astfvtypeC recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaType(name,cstr,_,_) ->
	  bind (constraints cstr) [metaid name]
      | _ -> option_default) in

  let astfvinit recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaInit(name,cstr,_,_) -> [metaid name]
      | Ast.MetaInitList(name,len,cstr,_,_) ->
	  bind (constraints cstr) (bind (listlen len) [metaid name])
      | _ -> option_default) in

  let astfvparam recursor k p =
    bind (k p)
      (match Ast.unwrap p with
	Ast.MetaParam(name,cstr,_,_) ->
	  bind (constraints cstr) [metaid name]
      | Ast.MetaParamList(name,len,cstr,_,_) ->
	  bind (constraints cstr) (bind (listlen len) [metaid name])
      | _ -> option_default) in

  let astfvdefine_param recursor k p =
    bind (k p)
      (match Ast.unwrap p with
	Ast.MetaDParamList(name,len,cstr,_,_) ->
	  bind (constraints cstr) (bind (listlen len) [metaid name])
      | _ -> option_default) in

  let astfvrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    bind (k re)
      (nub
	 (match Ast.unwrap re with
	   Ast.MetaRuleElem(name,cstr,_,_) | Ast.MetaStmt(name,cstr,_,_,_) ->
	     bind (constraints cstr) [metaid name]
	 | Ast.MetaStmtList(name,len,cstr,_,_) ->
	     bind (constraints cstr) (bind (listlen len) [metaid name])
	 | _ -> option_default)) in

  let astfvstatement recursor k s =
    bind (k s)
      (match Ast.unwrap s with
	Ast.Disj(stms) ->
	  bind_disj (List.map recursor.V.combiner_statement_dots stms)
      | _ -> option_default) in

  let mcode r mc = (*
    if include_constraints
    then *)
      List.fold_left bind option_default
	(List.map
	   (function Ast.MetaPos(name,constraints,_,_,_) ->
	     bind [metaid name]
	       (List.fold_left bind option_default
		  (if include_constraints
		  then
		    [Ast.cstr_meta_names constraints]
		  else [])))
	   (Ast.get_pos_var mc))
    (* else option_default *) in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    astfvident astfvexpr astfvfrag astfvfmt astfvassignop astfvbinaryop
    astfvfullType astfvtypeC astfvinit astfvparam astfvdefine_param
    astfvdecls donothing astfvfields donothing
    astfvrule_elem astfvstatement donothing donothing donothing_a

let collect_all_refs = collect_refs true
let collect_non_constraint_refs = collect_refs false

let collect_all_rule_refs minirules =
  List.fold_left (@) []
    (List.map collect_all_refs.V.combiner_top_level minirules)

let collect_all_minirule_refs = collect_all_refs.V.combiner_top_level

let collect_pos_positions =
  let bind x y = x @ y in
  let option_default = [] in
  let donothing recursor k e = k e in (* just combine in the normal way *)

  let metaid (x,_,_,_) = x in

  let mcode r mc =
      List.concat
	(List.map
	   (function Ast.MetaPos(name,constraints,_,_,_) -> [metaid name])
	   (Ast.get_pos_var mc)) in

  let cprule_elem recursor k re =
    match Ast.unwrap re with
      Ast.DisjRuleElem relist ->
	(*take the intersection of the results*)
	let subres = List.map k relist in
	List.fold_left Common.inter_set (List.hd subres) (List.tl subres)
    | _ -> k re in

  let cpstmt recursor k s =
    match Ast.unwrap s with
      Ast.Disj stmlist ->
	(*take the intersection of the results*)
	let subres =
	  List.map recursor.V.combiner_statement_dots stmlist in
	List.fold_left Common.inter_set (List.hd subres) (List.tl subres)
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing
    cprule_elem cpstmt donothing donothing donothing

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
        Ast.MetaId(name,_,Ast.Saved,_)
      | Ast.MetaFunc(name,_,Ast.Saved,_)
      | Ast.MetaLocalFunc(name,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let type_collect res ty =
    bind res (Common.set (Ast.meta_names_of_fullType ty)) in

  let fvbitfield bitfield =
    match bitfield with
      Some (Ast.MetaListLen (lenname,_,_,_)) -> [metaid lenname]
    | _ -> option_default in

  let astfvexpr recursor k e =
    let tymetas =
      match Ast.unwrap e with
	Ast.MetaExpr(name,_,_,Some type_list,_,_,bitfield) ->
	  List.fold_left type_collect (fvbitfield bitfield) type_list
      |	_ -> [] in
    let vars =
      bind (k e)
	(match Ast.unwrap e with
	  Ast.MetaErr(name,_,Ast.Saved,_) -> [metaid name]
	| Ast.MetaExpr(name,_,Ast.Saved,_,_,_,bitfield) ->
	    bind [metaid name] (fvbitfield bitfield)
	| Ast.MetaExprList(name,Ast.MetaListLen (lenname,_,ls,_),_,ns,_) ->
	    let namesaved =
	      match ns with Ast.Saved -> [metaid name] | _ -> [] in
	    let lensaved =
	      match ls with Ast.Saved -> [metaid lenname] | _ -> [] in
	    lensaved @ namesaved
	| Ast.MetaExprList(name,_,_,Ast.Saved,_) -> [metaid name]
	| _ -> option_default) in
    bind tymetas vars in

  let astfvfrag recursor k ft =
    bind (k ft)
      (match Ast.unwrap ft with
	Ast.MetaFormatList(pct,name,Ast.MetaListLen (lenname,_,_,_),_,
			   Ast.Saved,_) ->
	  [metaid name;metaid lenname]
      |	Ast.MetaFormatList(pct,name,_,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvfmt recursor k ft =
    bind (k ft)
      (match Ast.unwrap ft with
	Ast.MetaFormat(name,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvassign recursor k aop =
    bind (k aop)
      (match Ast.unwrap aop with
	Ast.MetaAssign(name,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvbinary recursor k bop =
    bind (k bop)
      (match Ast.unwrap bop with
	Ast.MetaBinary(name,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvtypeC recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaType(name,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvinit recursor k ty =
    bind (k ty)
      (match Ast.unwrap ty with
	Ast.MetaInit(name,_,Ast.Saved,_) -> [metaid name]
      |	Ast.MetaInitList(name,Ast.MetaListLen (lenname,_,ls,_),_,ns,_) ->
	  let namesaved =
	    match ns with Ast.Saved -> [metaid name] | _ -> [] in
	  let lensaved =
	    match ls with Ast.Saved -> [metaid lenname] | _ -> [] in
	  lensaved @ namesaved
      | Ast.MetaInitList(name,len,_,ns,_) ->
	  (match ns with Ast.Saved -> [metaid name] | _ -> [])
      | _ -> option_default) in

  let astfvparam recursor k p =
    bind (k p)
      (match Ast.unwrap p with
	Ast.MetaParam(name,_,Ast.Saved,_) -> [metaid name]
      | Ast.MetaParamList(name,Ast.MetaListLen (lenname,_,ls,_),_,ns,_) ->
	  let namesaved =
	    match ns with Ast.Saved -> [metaid name] | _ -> [] in
	  let lensaved =
	    match ls with Ast.Saved -> [metaid lenname] | _ -> [] in
	  lensaved @ namesaved
      | Ast.MetaParamList(name,_,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvdefine_param recursor k p =
    bind (k p)
      (match Ast.unwrap p with
	Ast.MetaDParamList(name,Ast.MetaListLen (lenname,_,ls,_),_,ns,_) ->
	  let namesaved =
	    match ns with Ast.Saved -> [metaid name] | _ -> [] in
	  let lensaved =
	    match ls with Ast.Saved -> [metaid lenname] | _ -> [] in
	  lensaved @ namesaved
      | Ast.MetaDParamList(name,_,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvdecls recursor k d =
    bind (k d)
      (match Ast.unwrap d with
	Ast.MetaDecl(name,_,Ast.Saved,_) ->
	  [metaid name]
      | _ -> option_default) in

  let astfvfields recursor k d =
    bind (k d)
      (match Ast.unwrap d with
	Ast.MetaField(name,_,Ast.Saved,_) ->
	  [metaid name]
      | Ast.MetaFieldList(name,Ast.MetaListLen (lenname,_,ls,_),_,ns,_) ->
	  let namesaved =
	    match ns with Ast.Saved -> [metaid name] | _ -> [] in
	  let lensaved =
	    match ls with Ast.Saved -> [metaid lenname] | _ -> [] in
	  lensaved @ namesaved
      | Ast.MetaFieldList(name,_,_,Ast.Saved,_) -> [metaid name]
      | _ -> option_default) in

  let astfvrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    bind (k re)
      (nub
	 (match Ast.unwrap re with
	   Ast.MetaRuleElem(name,_,Ast.Saved,_)
	 | Ast.MetaStmt(name,_,Ast.Saved,_,_) -> [metaid name]
	 | Ast.MetaStmtList(name,Ast.MetaListLen (lenname,_,ls,_),_,ns,_) ->
	     let namesaved =
	       match ns with Ast.Saved -> [metaid name] | _ -> [] in
	     let lensaved =
	       match ls with Ast.Saved -> [metaid lenname] | _ -> [] in
	     lensaved @ namesaved
	 | Ast.MetaStmtList(name,_,_,Ast.Saved,_) -> [metaid name]
	 | _ -> option_default)) in

  let mcode r e =
    List.fold_left
      (function acc ->
	function
	    Ast.MetaPos(name,_,_,Ast.Saved,_) -> (metaid name) :: acc
	  | _ -> acc)
      option_default (Ast.get_pos_var e) in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing
    astfvident astfvexpr astfvfrag astfvfmt astfvassign astfvbinary donothing
    astfvtypeC astfvinit astfvparam astfvdefine_param astfvdecls donothing
    astfvfields donothing astfvrule_elem donothing donothing donothing donothing

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
    Ast.MINUS(_,_,_,replacement) ->
      (match replacement with
	Ast.REPLACEMENT(anythings,_) -> process_anything_list_list anythings
      |	Ast.NOREPLACEMENT -> [])
  | Ast.CONTEXT(_,befaft) ->
      (match befaft with
	Ast.BEFORE(ll,_) -> process_anything_list_list ll
      | Ast.AFTER(ll,_) -> process_anything_list_list ll
      | Ast.BEFOREAFTER(llb,lla,_) ->
	  (process_anything_list_list lla) @
	  (process_anything_list_list llb)
      | Ast.NOTHING -> [])
  | Ast.PLUS _ -> []


let collect_fresh_seed_env metavars l =
  let fresh =
    List.fold_left
      (function prev ->
	function
	    Ast.MetaFreshIdDecl(_,seed) as x ->
	      ((Ast.get_meta_name x),seed)::prev
	  | _ -> prev)
      [] metavars in
  let (seed_env,seeds) =
    List.fold_left
      (function (seed_env,seeds) as prev ->
	function x ->
	  try
	    (let v = List.assoc x fresh in
	    match v with
	      Ast.ListSeed l ->
		let ids =
		  List.fold_left
		    (function prev ->
		      function
			  Ast.SeedId(id) -> id::prev
			| _ -> prev)
		    [] l in
		((x,ids)::seed_env,Common.union_set ids seeds)
	    | _ -> ((x,[])::seed_env,seeds))
	  with Not_found -> prev)
      ([],l) l in
  (List.rev seed_env,List.rev seeds)

let collect_fresh_seed metavars l =
  let (_,seeds) = collect_fresh_seed_env metavars l in seeds

let collect_in_plus_term =

  let bind x y = List.rev_append x y in
  let option_default = [] in
  let donothing r k e = k e in

  (* no positions in the + code *)
  let mcode r (_,_,mck,_) = cip_mcodekind r mck in

  (* case for things with bef/aft mcode *)

  let annotated_decl decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,_,_) -> bef in

  let astfvrule_elem recursor k re =
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fi,nm,_,params,_,_) ->
	bind (cip_mcodekind recursor bef) (k re)
	  (* no clue why this code is here *) (*
	let fi_metas =
	  List.concat
	    (List.map
	       (function
		   Ast.FType(ty) -> collect_all_refs.V.combiner_fullType ty
		 | _ -> [])
	       fi) in
	let nm_metas = collect_all_refs.V.combiner_ident nm in
	let param_metas =
	  List.concat
	    (List.map
	       (function p ->
	          match Ast.unwrap p with
	            Ast.VoidParam(t) | Ast.Param(t,_) ->
	              collect_all_refs.V.combiner_fullType t
		  | _ -> [])
	       (Ast.unwrap params)) in
	bind fi_metas
	  (bind nm_metas
	     (bind param_metas
		(bind (cip_mcodekind recursor bef) (k re)))) *)
    | Ast.Decl decl ->
	bind (cip_mcodekind recursor (annotated_decl decl)) (k re)
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	bind (cip_mcodekind recursor (annotated_decl decl)) (k re)
    | _ -> k re in

  let astfvstatement recursor k s =
    match Ast.unwrap s with
      Ast.IfThen(_,_,(_,_,_,aft)) | Ast.IfThenElse(_,_,_,_,(_,_,_,aft))
    | Ast.While(_,_,(_,_,_,aft)) | Ast.For(_,_,(_,_,_,aft))
    | Ast.Iterator(_,_,(_,_,_,aft)) | Ast.FunDecl(_,_,_,_,(_,_,_,aft)) ->
	bind (k s) (cip_mcodekind recursor aft)
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing astfvrule_elem
    astfvstatement donothing donothing donothing

let collect_in_plus metavars minirules =
  nub
    (collect_fresh_seed metavars
       (List.concat
	  (List.map collect_in_plus_term.V.combiner_top_level minirules)))

(* ---------------------------------------------------------------- *)

(* For the rules under a given metavariable declaration, collect all of the
variables that occur only once and more than once in the minus code *)

let collect_all_multirefs minirules =
  let refs = List.map collect_all_refs.V.combiner_top_level minirules in
  collect_unitary_nonunitary (List.concat refs)

(* ---------------------------------------------------------------- *)

(* classify as unitary (no binding) or nonunitary (env binding) or saved
(witness binding) *)

let classify_variables metavar_decls minirules used_after =
  let metavars = List.map Ast.get_meta_name metavar_decls in
  let (unitary,nonunitary) = collect_all_multirefs minirules in
  let inplus = collect_in_plus metavar_decls minirules in

  let donothing r k e = k e in
  let check_unitary name inherited =
    if List.mem name inplus || List.mem name used_after
    then Ast.Saved
    else if not inherited && List.mem name unitary
    then Ast.Unitary
    else Ast.Nonunitary in

  let get_option f = function Some x -> Some (f x) | None -> None in

  let classify (name,_,_,_) =
    let inherited = not (List.mem name metavars) in
    (check_unitary name inherited,inherited) in

  let mcode mc =
    let p =
      List.map
	(function Ast.MetaPos(name,constraints,per,unitary,inherited) ->
	  let (unitary,inherited) = classify name in
	  Ast.MetaPos(name,constraints,per,unitary,inherited))
	(Ast.get_pos_var mc) in
    Ast.set_pos_var p mc in

  let ident r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaId(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e
	  (Ast.MetaId(name,constraints,unitary,inherited))
    | Ast.MetaFunc(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaFunc(name,constraints,unitary,inherited))
    | Ast.MetaLocalFunc(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaLocalFunc(name,constraints,unitary,inherited))
    | _ -> e in

  let rec classify_ident ident =
    let new_ident =
      match Ast.unwrap ident with
        Ast.Id id -> Ast.Id id
      | Ast.MetaId (name, _, _, _) ->
          let (unitary, inherited) =
            classify (Ast.unwrap_mcode name, (), (), []) in
          Ast.MetaId (name, Ast.CstrTrue, unitary, inherited)
      | Ast.MetaFunc (name, _, _, _) ->
          let (unitary, inherited) =
            classify (Ast.unwrap_mcode name, (), (), []) in
          Ast.MetaFunc (name, Ast.CstrTrue, unitary, inherited)
      | Ast.MetaLocalFunc (name, _, _, _) ->
          let (unitary, inherited) =
            classify (Ast.unwrap_mcode name, (), (), []) in
          Ast.MetaLocalFunc (name, Ast.CstrTrue, unitary, inherited)
      | Ast.AsIdent (ident0, ident1) ->
          Ast.AsIdent (classify_ident ident0, classify_ident ident1)
      | Ast.DisjId list -> Ast.DisjId (List.map classify_ident list)
      | Ast.OptIdent ident' -> Ast.OptIdent (classify_ident ident') in
    Ast.rewrap ident new_ident in

  let type_infos ty =
    Ast.fullType_map { Ast.empty_transformer with
      Ast.decimal = Some (fun s0 s1 e1 s2 e2 s3 ->
          let e2mv e =
            match Ast.unwrap e with
              Ast.Ident ident -> Ast.rewrap e (Ast.Ident (classify_ident ident))
            | _ -> e in
          Ast.Decimal (s0, s1, e2mv e1, s2, Common.map_option e2mv e2, s3));
      enumName = Some (fun s0 ident ->
        let ident' = Common.map_option classify_ident ident in
        Ast.EnumName (s0, ident'));
      structUnionName = Some (fun su ident ->
        let ident' = Common.map_option classify_ident ident in
        Ast.StructUnionName (su, ident'));
      metaType = Some (fun name cstr _ _ ->
        let (unitary,inherited) = classify (Ast.unwrap_mcode name,(),(),[]) in
        Ast.MetaType (name,cstr,unitary,inherited))
    } ty in

  let expression r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaErr(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaErr(name,constraints,unitary,inherited))
    | Ast.MetaExpr(name,constraints,_,ty,form,_,bitfield) ->
	let (unitary,inherited) = classify name in
	let ty = get_option (List.map type_infos) ty in
	let bitfield' =
	  match bitfield with
	    Some (Ast.MetaListLen (lenname, cstr, _, _)) ->
	      let (lenunitary,leninherited) = classify lenname in
	      Some (Ast.MetaListLen (lenname, cstr, lenunitary, leninherited))
	  | _ -> bitfield in
	Ast.rewrap e
	  (Ast.MetaExpr(name,constraints,unitary,ty,form,inherited,bitfield'))
    | Ast.MetaExprList(name,Ast.MetaListLen(lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaExprList
	     (name,
	      Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaExprList(name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaExprList(name,lenname,cstr,unitary,inherited))
    | _ -> e in

  let string_fragment r k ft =
    let ft = k ft in
    match Ast.unwrap ft with
      Ast.MetaFormatList
	(pct,name,Ast.MetaListLen (lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap ft
	  (Ast.MetaFormatList
	     (pct,name,
	      Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaFormatList(pct,name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap ft
	  (Ast.MetaFormatList(pct,name,lenname,cstr,unitary,inherited))
    | _ -> ft in

  let string_format r k ft =
    let ft = k ft in
    match Ast.unwrap ft with
      Ast.MetaFormat(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap ft (Ast.MetaFormat(name,constraints,unitary,inherited))
    | _ -> ft in

  let assignop r k ft =
    let ft = k ft in
    match Ast.unwrap ft with
      Ast.MetaAssign(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap ft (Ast.MetaAssign(name,constraints,unitary,inherited))
    | _ -> ft in

  let binaryop r k ft =
    let ft = k ft in
    match Ast.unwrap ft with
      Ast.MetaBinary(name,constraints,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap ft (Ast.MetaBinary(name,constraints,unitary,inherited))
    | _ -> ft in

  let typeC r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaType(name,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaType(name,cstr,unitary,inherited))
    | _ -> e in

  let init r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaInit(name,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaInit(name,cstr,unitary,inherited))
    | Ast.MetaInitList(name,Ast.MetaListLen (lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaInitList
	     (name,Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaInitList(name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaInitList(name,lenname,cstr,unitary,inherited))
    | _ -> e in

  let param r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaParam(name,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaParam(name,cstr,unitary,inherited))
    | Ast.MetaParamList(name,Ast.MetaListLen (lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaParamList
	     (name,Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaParamList(name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaParamList(name,lenname,cstr,unitary,inherited))
    | _ -> e in

  let define_param r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaDParamList(name,Ast.MetaListLen (lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaDParamList
	     (name,Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaDParamList(name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaDParamList(name,lenname,cstr,unitary,inherited))
    | _ -> e in

  let decl r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaDecl(name,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaDecl(name,cstr,unitary,inherited))
    | _ -> e in

  let field r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaField(name,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaField(name,cstr,unitary,inherited))
    | Ast.MetaFieldList(name,Ast.MetaListLen (lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaFieldList
	     (name,Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaFieldList(name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaFieldList(name,lenname,cstr,unitary,inherited))
    | _ -> e in

  let rule_elem r k e =
    let e = k e in
    match Ast.unwrap e with
      Ast.MetaStmt(name,cstr,_,msi,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaStmt(name,cstr,unitary,msi,inherited))
    | Ast.MetaStmtList(name,Ast.MetaListLen (lenname,cstr,_,_),cstr',_,_) ->
	let (unitary,inherited) = classify name in
	let (lenunitary,leninherited) = classify lenname in
	Ast.rewrap e
	  (Ast.MetaStmtList
	     (name,Ast.MetaListLen(lenname,cstr,lenunitary,leninherited),
	      cstr',unitary,inherited))
    | Ast.MetaStmtList(name,lenname,cstr,_,_) ->
	let (unitary,inherited) = classify name in
	Ast.rewrap e (Ast.MetaStmtList(name,lenname,cstr,unitary,inherited))
    | _ -> e in

  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing
      ident expression string_fragment string_format assignop binaryop
      donothing typeC
      init param define_param decl donothing field donothing rule_elem
      donothing donothing donothing donothing in

  List.map fn.V.rebuilder_top_level minirules

(* ---------------------------------------------------------------- *)

(* For a minirule, collect the set of non-local (not in "bound") variables that
are referenced.  Store them in a hash table. *)

(* bound means the metavariable was declared previously, not locally *)

(* Highly inefficient, because we call collect_all_refs on nested code
multiple times.  But we get the advantage of not having too many variants
of the same functions. *)

(* Inherited doesn't include negative position constraints.  If they are
not bound then there is no constraint. *)

let astfvs metavars bound =
  let fresh =
    List.fold_left
      (function prev ->
	function
	    Ast.MetaFreshIdDecl(_,seed) as x ->
	      ((Ast.get_meta_name x),seed)::prev
	  | _ -> prev)
      [] metavars in

  let collect_fresh l =
    let (matched,freshvars) =
      List.fold_left
	(function (matched,freshvars) ->
	  function x ->
	    try let v = List.assoc x fresh in (matched,(x,v)::freshvars)
	    with Not_found -> (x::matched,freshvars))
	([],[]) l in
    (List.rev matched, List.rev freshvars) in

  let refront re =
    match Ast.unwrap re with
      Ast.FunHeader _ -> "FunHeader"
    | Ast.Decl _ -> "Decl"
    | Ast.SeqStart _ -> "SeqStart"
    | Ast.SeqEnd _ -> "SeqEnd"
    | Ast.ExprStatement _ -> "ExprStatement"
    | Ast.IfHeader _ -> "IfHeader"
    | Ast.Else _ -> "Else"
    | Ast.WhileHeader _ -> "WhileHeader"
    | Ast.DoHeader _ -> "DoHeader"
    | Ast.WhileTail _ -> "WhileTail"
    | Ast.ForHeader _ -> "ForHeader"
    | Ast.IteratorHeader _ -> "IteratorHeader"
    | Ast.SwitchHeader _ -> "SwitchHeader"
    | Ast.Break _ -> "Break"
    | Ast.Continue _ -> "Continue"
    | Ast.Label _ -> "Label"
    | Ast.Goto _ -> "Goto"
    | Ast.Return _ -> "Return"
    | Ast.ReturnExpr _ -> "ReturnExpr"
    | Ast.Exec _ -> "Exec"
    | Ast.MetaRuleElem _ -> "MetaRuleElem"
    | Ast.MetaStmt _ -> "MetaStmt"
    | Ast.MetaStmtList _ -> "MetaStmtList"
    | Ast.Exp _ -> "Exp"
    | Ast.TopExp _ -> "TopExp"
    | Ast.Ty _ -> "Ty"
    | Ast.TopId _ -> "TopId"
    | Ast.TopInit _ -> "TopInit"
    | Ast.Include _ -> "Include"
    | Ast.MetaInclude _ -> "MetaInclude"
    | Ast.Undef _ -> "Undef"
    | Ast.DefineHeader _ -> "DefineHeader"
    | Ast.Pragma _ -> "Pragma"
    | Ast.Case _ -> "Case"
    | Ast.Default _ -> "Default"
    | Ast.AsRe _ -> "AsRe"
    | Ast.DisjRuleElem _ -> "DisjRuleElem" in

  (* cases for the elements of anything *)
  let simple_setup refront getter k re =
    let minus_free = nub (getter collect_all_refs re) in
    let minus_nc_free =
      nub (getter collect_non_constraint_refs re) in
    let minus_pos_free =
      nub (getter collect_pos_positions re) in
    let plus_free =
      collect_fresh_seed metavars (getter collect_in_plus_term re) in
    let free = Common.union_set minus_free plus_free in
    let nc_free = Common.union_set minus_nc_free plus_free in
    let unbound =
      List.filter (function x -> not(List.mem x bound)) free in
    let inherited =
      List.filter (function x -> List.mem x bound) nc_free in
    let inherited_pos =
      List.filter (function x -> List.mem x bound) minus_pos_free in
    let munbound =
      List.filter (function x -> not(List.mem x bound)) minus_free in
    let (matched,fresh) = collect_fresh unbound in
    {(k re) with
      Ast.free_vars = matched;
      Ast.minus_free_vars = munbound;
      Ast.minus_nc_free_vars = minus_nc_free;
      Ast.fresh_vars = fresh;
      Ast.inherited = inherited;
      Ast.positive_inherited_positions = inherited_pos;
      Ast.saved_witness = []} in

  let astfvrule_elem recursor k re =
    simple_setup refront (function x -> x.V.combiner_rule_elem) k re in

  let astfvstatement recursor k s =
    let minus_free = nub (collect_all_refs.V.combiner_statement s) in
    let minus_nc_free =
      nub (collect_non_constraint_refs.V.combiner_statement s) in
    let plus_free =
      collect_fresh_seed metavars
	(collect_in_plus_term.V.combiner_statement s) in
    let free = Common.union_set minus_free plus_free in
    let nc_free = Common.union_set minus_nc_free plus_free in
    let classify free minus_free =
      let (unbound,inherited) =
	List.partition (function x -> not(List.mem x bound)) free in
      let munbound =
	List.filter (function x -> not(List.mem x bound)) minus_free in
      let (matched,fresh) = collect_fresh unbound in
      (matched,munbound,fresh,inherited) in
    let res = k s in
    let s =
      let cip_plus aft =
	collect_fresh_seed metavars
	  (cip_mcodekind collect_in_plus_term aft) in
      match Ast.unwrap res with
	Ast.IfThen(header,branch,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) = classify (cip_plus aft) [] in
	  Ast.IfThen(header,branch,(unbound,fresh,inherited,aft))
      | Ast.IfThenElse(header,branch1,els,branch2,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) = classify (cip_plus aft) [] in
	  Ast.IfThenElse(header,branch1,els,branch2,
			 (unbound,fresh,inherited,aft))
      | Ast.While(header,body,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) = classify (cip_plus aft) [] in
	  Ast.While(header,body,(unbound,fresh,inherited,aft))
      | Ast.For(header,body,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) = classify (cip_plus aft) [] in
	  Ast.For(header,body,(unbound,fresh,inherited,aft))
      | Ast.Iterator(header,body,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) = classify (cip_plus aft) [] in
	  Ast.Iterator(header,body,(unbound,fresh,inherited,aft))
      |	Ast.FunDecl(header,lbrace,body,rbrace,(_,_,_,aft)) ->
	  let (unbound,_,fresh,inherited) = classify (cip_plus aft) [] in
	  Ast.FunDecl(header,lbrace,body,rbrace,(unbound,fresh,inherited,aft))
      |	s -> s in

    let (matched,munbound,fresh,_) = classify free minus_free in
    let inherited =
      List.filter (function x -> List.mem x bound) nc_free in
    {res with
      Ast.node = s;
      Ast.free_vars = matched;
      Ast.minus_free_vars = munbound;
      Ast.fresh_vars = fresh;
      Ast.inherited = inherited;
      Ast.saved_witness = []} in

  let astfvstatement_dots recursor k sd =
    simple_setup (fun _ -> "statement")
      (function x -> x.V.combiner_statement_dots) k sd in

  let astfvcase_line recursor k cl =
    simple_setup (fun _ -> "case")
      (function x -> x.V.combiner_case_line) k cl in

  let astfvtoplevel recursor k tl =
    let saved = collect_saved.V.combiner_top_level tl in
    {(k tl) with Ast.saved_witness = saved} in

  let mcode x = x in
  let donothing r k e = k e in

  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing astfvstatement_dots donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing
    astfvrule_elem astfvstatement astfvcase_line astfvtoplevel donothing

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
          Ast.ScriptRule (_,_,_,_,script_vars,_,_) ->
	    (* why are metavars in rule, but outside for cocci rule??? *)
            let bound = script_vars @ bound in
	    rule::(loop bound rules)
        | Ast.InitialScriptRule (_,_,_,_,_,_)
	| Ast.FinalScriptRule (_,_,_,_,_,_) ->
	    (* bound stays as is because init/finalize provides no names, so
	       inheritance by others is not possible *)
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
  let bind (p1,np1,ap1) (p2,np2,ap2) =
    (Common.union_set p1 p2, Common.union_set np1 np2,
     Common.union_set ap1 ap2) in
  let option_default = ([],[],[]) in
  let metaid (x,_,_,_) = x in
  let get_neg_pos_constraints =
    Ast.cstr_fold_sign
      { Ast.empty_cstr_transformer with
	Ast.cstr_meta_name = Some (fun mn (pos, neg) -> (mn :: pos, neg)) }
      { Ast.empty_cstr_transformer with
	Ast.cstr_meta_name = Some (fun mn (pos, neg) -> (pos, mn :: neg)) } in
  let mcode r mc =
    List.fold_left
      (function (pos_vars,neg_vars,all_vars) ->
	(* pos_vars are position variables that are used in a positive way
	   neg_vars are position variables that are used in a negative way
	   all_vars are position variables with the annotation ALL.
	   These sets can overlap. *)
	(function
	    Ast.MetaPos(name,constraints,collect,_,_) ->
	      let name' = metaid name in
	      let pos_vars', neg_vars' =
		get_neg_pos_constraints constraints (pos_vars, neg_vars) in
	      let (pos_vars'', all_vars') =
		match collect with
		  Ast.PER -> (name' :: pos_vars', all_vars)
		| Ast.ALL -> (pos_vars', name' :: all_vars) in
	      (pos_vars'', neg_vars',all_vars')))
      option_default (Ast.get_pos_var mc) in
  let v =
    V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing in
  match rule with
    Ast.CocciRule(rule_name,_,minirules,_,_) ->
      List.map
	(function toplevel ->
	  let (positions,neg_positions,all_positions) =
	    v.V.combiner_top_level toplevel in
	  (try
	    let (badr,badnm) =
	      List.find (function p -> List.mem p neg_positions) positions in
	    failwith
	      (Printf.sprintf
		 "Variable %s in %s cannot be used as both a position and a constraint"
		 (if badr = rule_name
		 then badnm
		 else Printf.sprintf "%s.%s" badr badnm)
		 rule_name)
	      with Not_found -> ());
	  (neg_positions, all_positions))
	minirules
  | Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ ->
      (*no negated positions*) []

(* ---------------------------------------------------------------- *)

(* collect used after lists, per minirule *)

(* defined is a list of variables that were declared in a previous metavar
declaration *)

(* Top-level used after: For each rule collect the set of variables that
are inherited, ie used but not defined.  These are accumulated back to
their point of definition. *)


let collect_top_level_used_after metavar_rule_list dropped_rules =
  let drop_virt = List.filter (function ("virtual",_) -> false | _ -> true) in
  let (used_after,used_after_lists) =
    List.fold_right
      (function (metavar_list,r) ->
	function (used_after,used_after_lists) ->
	  let locally_defined =
            match r with
              Ast.ScriptRule (_,_,_,_,free_vars,_,_) -> free_vars
	    | _ -> List.map Ast.get_meta_name metavar_list in
	  let continue_propagation =
	    List.filter (function x -> not(List.mem x locally_defined))
	      used_after in
	  let free_vars =
            match r with
              Ast.ScriptRule (_,_,_,mv,_,_,_) ->
                drop_virt(List.map (function (_,(r,v),_,_) -> (r,v)) mv)
            | Ast.InitialScriptRule (_,_,_,mv,_,_)
	    | Ast.FinalScriptRule (_,_,_,mv,_,_) ->
		(* only virtual identifiers *)
		[]
            | Ast.CocciRule (_,_,rule,_,_) ->
		drop_virt
	          (Common.union_set (nub (collect_all_rule_refs rule))
	             (collect_in_plus metavar_list rule)) in
	  let inherited =
	    List.filter (function x -> not (List.mem x locally_defined))
	      free_vars in
	  (Common.union_set inherited continue_propagation,
	   used_after::used_after_lists))
      metavar_rule_list ([],[]) in
  let used_after =
    List.filter
      (function (rule,_) -> not(List.mem rule dropped_rules))
      used_after in
  match used_after with
    [] -> used_after_lists
  | _ ->
      failwith
	(Printf.sprintf "collect_top_level_used_after: unbound variables %s"
	   (String.concat " " (List.map (function (_,x) -> x) used_after)))

let collect_local_used_after metavars minirules used_after =
  let locally_defined = List.map Ast.get_meta_name metavars in
  let rec loop = function
      [] -> (used_after,[],[],[],[])
    | minirule::rest ->
	(* In a rule there are three kinds of local variables:
	   1. Variables referenced in the minus or context code.
	   These get a value by matching.  This value can be used in
	   subsequent rules.
	   2. Fresh variables referenced in the plus code.
	   3. Variables referenced in the seeds of the fresh variables.
	   There are also non-local variables. These may either be variables
	   referenced in the minus, context, or plus code, or they may be
	   variables referenced in the seeds of the fresh variables. *)
	(* Step 1: collect all references in minus/context, plus, seed
	   code *)
	let variables_referenced_in_minus_context_code =
	  nub (collect_all_minirule_refs minirule) in
	let variables_referenced_in_plus_code =
	  collect_in_plus_term.V.combiner_top_level minirule in
	let (env_of_fresh_seeds,seeds_and_plus) =
	  collect_fresh_seed_env
	    metavars variables_referenced_in_plus_code in
	let all_free_vars =
	  Common.union_set variables_referenced_in_minus_context_code
	    seeds_and_plus in
	(* Step 2: identify locally defined ones *)
	let local_fresh = List.map fst env_of_fresh_seeds in
	let is_local =
	  List.partition (function x -> List.mem x locally_defined) in
	let local_env_of_fresh_seeds =
	  (* these have to be restricted to only one value if the associated
	     fresh variable is used after *)
	  List.map (function (f,ss) -> (f,is_local ss)) env_of_fresh_seeds in
	let (local_all_free_vars,nonlocal_all_free_vars) =
	  is_local all_free_vars in
	(* Step 3, recurse on the rest of the rules, making available whatever
	   has been defined in this one *)
	let (mini_used_after,fvs_lists,mini_used_after_lists,
	     mini_fresh_used_after_lists,mini_fresh_used_after_seeds) =
	  loop rest in
	(* Step 4: collect the results.  These are:
	   1. All of the variables used non-locally in the rules starting
	   with this one
	   2. All of the free variables to the end of the semantic patch
	   3. The variables that are used afterwards and defined here by
	   matching (minus or context code)
	   4. The variables that are used afterwards and are defined here as
	   fresh
	   5. The variables that are used as seeds in computing the bindings
	   of the variables collected in part 4. *)
	let (local_used_after, nonlocal_used_after) =
	  is_local mini_used_after in
	let (fresh_local_used_after(*4*),matched_local_used_after) =
	  List.partition (function x -> List.mem x local_fresh)
	    local_used_after in
	let matched_local_used_after(*3*) =
	  Common.union_set matched_local_used_after nonlocal_used_after in
	let new_used_after = (*1*)
	  Common.union_set nonlocal_all_free_vars nonlocal_used_after in
	let fresh_local_used_after_seeds =
	  List.filter
	    (* no point to keep variables that already are gtd to have only
	       one value *)
	    (function x -> not (List.mem x matched_local_used_after))
	    (List.fold_left (function p -> function c -> Common.union_set c p)
	       []
	       (List.map
		  (function fua ->
		    fst (List.assoc fua local_env_of_fresh_seeds))
		  fresh_local_used_after)) in
	(new_used_after,all_free_vars::fvs_lists(*2*),
	 matched_local_used_after::mini_used_after_lists,
	 fresh_local_used_after::mini_fresh_used_after_lists,
	 fresh_local_used_after_seeds::mini_fresh_used_after_seeds) in
  let (_,fvs_lists,used_after_lists(*ua*),
       fresh_used_after_lists(*fua*),fresh_used_after_lists_seeds(*fuas*)) =
    loop minirules in
  (fvs_lists,used_after_lists,
   fresh_used_after_lists,fresh_used_after_lists_seeds)



let collect_used_after metavar_rule_list dropped_rules =
  let used_after_lists =
    collect_top_level_used_after metavar_rule_list dropped_rules in
  List.map2
    (function (metavars,r) ->
      function used_after ->
        match r with
          Ast.ScriptRule (_,_,_,_,_,_,_) (* no minirules, so nothing to do? *)
	| Ast.InitialScriptRule (_,_,_,_,_,_)
	| Ast.FinalScriptRule (_,_,_,_,_,_) ->
	    ([], [used_after], [[]], [])
        | Ast.CocciRule (name, rule_info, minirules, _,_) ->
          collect_local_used_after metavars minirules used_after
    )
    metavar_rule_list used_after_lists

let rec split4 = function
    [] -> ([],[],[],[])
  | (a,b,c,d)::l -> let (a1,b1,c1,d1) = split4 l in (a::a1,b::b1,c::c1,d::d1)

(* ---------------------------------------------------------------- *)
(* entry point *)

let free_vars rules dropped_rules =
  let metavars = List.map (function (mv,rule) -> mv) rules in
  let (fvs_lists,used_after_matched_lists,
       fresh_used_after_lists,fresh_used_after_lists_seeds) =
    split4 (collect_used_after rules dropped_rules) in
  let neg_pos_lists =
    List.map2 get_neg_pos_list rules used_after_matched_lists in
  let positions_list = (* for all rules, assume all positions are used after *)
    List.map
      (function (mv, r) ->
         match r with
           Ast.ScriptRule _ (* doesn't declare position variables *)
	 | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> []
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
	function (ua,fua) ->
          match r with
            Ast.ScriptRule _
	  | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> r
          | Ast.CocciRule (nm, rule_info, r, is_exp,ruletype) ->
	      Ast.CocciRule
		(nm, rule_info,
		 classify_variables mv r
		   ((List.concat ua) @ (List.concat fua)),
		 is_exp,ruletype))
      rules (List.combine used_after_matched_lists fresh_used_after_lists) in
  let new_rules = collect_astfvs (List.combine metavars new_rules) in
  (metavars,new_rules,
   fvs_lists,neg_pos_lists,
   (used_after_matched_lists,
    fresh_used_after_lists,fresh_used_after_lists_seeds),
   positions_list)
