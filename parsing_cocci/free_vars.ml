(* for each rule return:
1. Hash table of all of the terms in the rule with their free variables
2. The list of metavariables that are declared in this one and used without
being redeclared in subsequent ones *)

module Ast = Ast_cocci
module V = Visitor_ast

let set_minus s minus = List.filter (function n -> not (List.mem n minus)) s
let set_intersect s1 s2 = List.filter (function n -> List.mem n s2) s1

let rec split3 = function
    [] -> ([],[],[])
  | (a,b,c)::rest -> let (a1,b1,c1) = split3 rest in (a::a1,b::b1,c::c1)

let rec nub = function
    [] -> []
  | (x::xs) when (List.mem x xs) -> nub xs
  | (x::xs) -> x::(nub xs)

(* --------------------------------------------------------------------- *)

(* a variable that occurs only once in free_usage and does not occur in used
after is unitary *)
let collect_unitary_nonunitary free_usage used_after =
  let free_usage = set_minus free_usage used_after in
  let free_usage = List.sort compare free_usage in
  let rec loop1 todrop = function
      [] -> []
    | (x::xs) as all -> if x = todrop then loop1 todrop xs else all in
  let rec loop2 = function
      [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::xs ->
	if x = y
	then
	  let (unitary,non_unitary) = loop2(loop1 x xs) in
	  (unitary,x::non_unitary)
	else
	  let (unitary,non_unitary) = loop2 (y::xs) in
	  (x::unitary,non_unitary) in
  loop2 free_usage

let collect_unitary_variables free_usage used_after =
  let (unitary,non_unitary) =
    collect_unitary_nonunitary free_usage used_after in
  unitary

(* --------------------------------------------------------------------- *)
(* Computing free variables *)

type anything =
    Rule_elem        of Ast.rule_elem
  | Statement        of Ast.statement
  | StatementDots    of Ast.statement Ast.dots

type free_table =
    (anything,(string list(*unbound*)*string list(*inherited*))) Hashtbl.t

(* Note that we would really rather attach + code to - or context code that
shares the same variables, if there is such.  If we attach it to something
else, the we increase the scope of the variable, and may allow less
variation.  Perhaps this is never a problem, because multiple control-flow
paths are only possible when there are dots, and + code can't attach to
dots.  If there are two options for attaching the + code, then both options
necessarily occur the same number of times in the matched code, so it
doesn't matter where the quantifier goes. *)

(* unbound is free and not in the argument bound.  free is as though the
argument bound were [].  only unbound is hashed, since that is what asttoctl
needs. *)

(* bound means the metavariable was declared previously, not locally *)

let astfvs bound =
  let free_table = (Hashtbl.create(50) : free_table) in

  let metaid (x,_,_) = x in

  let bind (unbound1,free1,refs1) (unbound2,free2,refs2) =
    (Common.union_set unbound1 unbound2, Common.union_set free1 free2,
     refs1 @ refs2) in
  (* used with fold_left, so the second argument is typically smaller *)
  let bind2 (unbound1,free1,refs1) (unbound2,free2,refs2) =
    (Common.union_set unbound2 unbound1, Common.union_set free2 free1,
     (* dup refs because so that nothing in + code is considered unitary *)
     refs2 @ refs2 @ refs1) in
  let option_default = ([],[],[]) in

  let mcodekind r mck =
    let process_anything_list_list anythings =
      let astfvs = r.V.combiner_anything in
      List.fold_left bind ([],[],[])
	(List.map
	   (function l ->
	     List.fold_left bind2 ([],[],[]) (List.map astfvs l))
	   anythings) in
    match mck with
      Ast.MINUS(_,anythings) -> process_anything_list_list anythings
    | Ast.CONTEXT(_,befaft) ->
	(match befaft with
	  Ast.BEFORE(ll) -> process_anything_list_list ll
	| Ast.AFTER(ll) -> process_anything_list_list ll
	| Ast.BEFOREAFTER(llb,lla) ->
	    bind
	      (process_anything_list_list lla)
	      (process_anything_list_list llb)
	| Ast.NOTHING -> option_default)
    | Ast.PLUS -> option_default in

  let mcode r (_,_,mck) = mcodekind r mck in

  let donothing recursor k e = k e in (* just combine in the normal way *)

  (* the following considers that anything that occurs non-unitarily in one
     branch occurs nonunitarily in all branches.  This is not optimal, but
     doing better seems to require a breadth-first traversal, which is
     perhaps better to avoid.  Also, unitarily is represented as occuring once,
     while nonunitarily is represented as twice - more is irrelevant *)
  let bind_disj infos =
    let (unbound,free,refs_branches) = split3 infos in
    let (unitary,nonunitary) =
      List.split
	(List.map (function x -> collect_unitary_nonunitary x [])
	   refs_branches) in
    let unbound = nub (List.concat unbound) in
    let free = nub (List.concat free) in
    let unitary = nub (List.concat unitary) in
    let nonunitary = nub (List.concat nonunitary) in
    let unitary =
      List.filter (function x -> not (List.mem x nonunitary)) unitary in
    (unbound,free,unitary@nonunitary@nonunitary) in

  let astfvident recursor k i =
    match Ast.unwrap i with
      Ast.MetaId(name,true,_) | Ast.MetaFunc(name,true,_)
    | Ast.MetaLocalFunc(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id],[]) (mcode recursor name)
	else bind ([id],[id],[id]) (mcode recursor name)
    | _ -> k i in

  let astfvexpr recursor k e =
    match Ast.unwrap e with
      Ast.MetaConst(name,true,_,_) | Ast.MetaErr(name,true,_)
    | Ast.MetaExpr(name,true,_,_) | Ast.MetaExprList(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id],[]) (mcode recursor name)
	else bind ([id],[id],[id]) (mcode recursor name)
    | Ast.DisjExpr(exps) -> bind_disj (List.map k exps)
    | _ -> k e in

  let astfvdecls recursor k d =
    match Ast.unwrap d with
      Ast.DisjDecl(decls) -> bind_disj (List.map k decls)
    | _ -> k d in

  let astfvtypeC recursor k ty =
    match Ast.unwrap ty with
      Ast.MetaType(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id],[]) (mcode recursor name)
	else bind ([id],[id],[id]) (mcode recursor name)
    | _ -> k ty in

  let astfvparam recursor k p =
    match Ast.unwrap p with
      Ast.MetaParam(name,true,_) | Ast.MetaParamList(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id],[]) (mcode recursor name)
	else bind ([id],[id],[id]) (mcode recursor name)
    | _ -> k p in

  let astfvrule_elem recursor k re =
    let (unbound,free,_) as res =
      match Ast.unwrap re with
	Ast.MetaRuleElem(name,true,_) | Ast.MetaStmt(name,true,_,_)
      | Ast.MetaStmtList(name,true,_) ->
	  let id = metaid name in
	  if List.mem id bound
	  then bind ([],[id],[]) (mcode recursor name)
	  else bind ([id],[id],[id]) (mcode recursor name)
      |	Ast.Define(_,_,db) ->
	  (match Ast.unwrap db with
	    Ast.DMetaId(name,true) ->
	      let id = metaid name in
	      if List.mem id bound
	      then bind ([],[id],[]) (mcode recursor name)
	      else bind ([id],[id],[id]) (mcode recursor name)
	  | _ -> k re)
      |	Ast.FunHeader(bef,_,_,_,_,_,_,_) | Ast.Decl(bef,_) ->
	  bind (mcodekind recursor bef) (k re)
      | _ -> k re in
    Hashtbl.add free_table (Rule_elem re)
      (unbound, Common.minus_set free unbound);
    res in

  let astfvstatement recursor k s =
    let (unbound,free,_) as res =
      match Ast.unwrap s with
	Ast.Disj(stms) ->
	  bind_disj (List.map recursor.V.combiner_statement_dots stms)
      | Ast.IfThen(_,_,aft) | Ast.IfThenElse(_,_,_,_,aft)
      | Ast.While(_,_,aft) | Ast.For(_,_,aft) ->
	  bind (k s) (mcodekind recursor aft)
      |	_ -> k s in
    Hashtbl.add free_table (Statement s)
      (unbound, Common.minus_set free unbound);
    res in

  let astfvstatement_dots recursor k s = 
    let (unbound,free,_) as res = k s in
    Hashtbl.add free_table (StatementDots s) 
      (unbound, Common.minus_set free unbound);
    res in

  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing astfvstatement_dots
      astfvident astfvexpr donothing astfvtypeC donothing astfvparam astfvdecls
      astfvrule_elem astfvstatement donothing donothing donothing in

  (* all is the information for each rule.  the second component is a
  summary of the information for all of the rules that share a single
  metavariable declaration.  in each case, the first component is the set
  of non-local variables that are referenced and the second component is
  the complete set of variables that are referenced. *)
  let rule l =
    let all = List.map recursor.V.combiner_top_level l in
    (all,List.fold_left bind option_default all) in

  (function l -> (rule l,free_table))

let get_names = function
    Ast.MetaIdDecl(ar,nm) -> nm
  | Ast.MetaFreshIdDecl(ar,nm) -> nm
  | Ast.MetaTypeDecl(ar,nm) -> nm
  | Ast.MetaParamDecl(ar,nm) -> nm
  | Ast.MetaParamListDecl(ar,nm) -> nm
  | Ast.MetaConstDecl(ar,nm) -> nm
  | Ast.MetaErrDecl(ar,nm) -> nm
  | Ast.MetaExpDecl(ar,nm) -> nm
  | Ast.MetaExpListDecl(ar,nm) -> nm
  | Ast.MetaStmDecl(ar,nm) -> nm
  | Ast.MetaStmListDecl(ar,nm) -> nm
  | Ast.MetaFuncDecl(ar,nm) -> nm
  | Ast.MetaLocalFuncDecl(ar,nm) -> nm
  | Ast.MetaTextDecl(ar,nm) -> nm

let update table metavars unitary_variables =
  let fresh =
    List.fold_left
      (function prev ->
	function Ast.MetaFreshIdDecl(arity,name) -> name::prev | _ -> prev)
      [] metavars in

  let collect_fresh = List.filter (function x -> List.mem x fresh) in

  let statement r k s =
    let (fvs,inherited) = Hashtbl.find table (Statement s) in
    let fvs = set_minus fvs unitary_variables in
    let (s,l,_,_,_,d) = k s in
    (s,l,fvs,collect_fresh fvs,inherited,d) in

  let statement_dots r k s =
    let (fvs,inherited) = Hashtbl.find table (StatementDots s) in
    let fvs = set_minus fvs unitary_variables in
    let (s,l,_,_,_,d) = k s in
    (s,l,fvs,collect_fresh fvs,inherited,d) in

  let rule_elem r k s =
    let (fvs,inherited) = Hashtbl.find table (Rule_elem s) in
    let fvs = set_minus fvs unitary_variables in
    let (s,l,_,_,_,d) = k s in
    (s,l,fvs,collect_fresh fvs,inherited,d) in

  let mcode x = x in
  let donothing r k e = k e in

  (V.rebuilder
     mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
     donothing donothing statement_dots
     donothing donothing donothing donothing donothing donothing donothing
     rule_elem statement donothing donothing donothing).V.rebuilder_top_level

let inner_non_locally_used l =
  let rec loop bound = function
      [] -> []
    | x::xs ->
	let all = Common.union_set x bound in
	let x = List.filter (function x -> List.exists (List.mem x) xs) all in
	x::(loop all xs) in
  loop [] l

(* --------------------------------------------------------------------- *)
(* detection and updating of unitary variables *)

let drop_unitary_variables unitary_variables =
  Printf.printf "unitary variables %s\n" (String.concat " " unitary_variables);
  let donothing r k e = k e in
  let mcode x = x in
  let not_unitary (name,_,mc) = not(List.mem name unitary_variables) in

  let ident r k e =
    match Ast.unwrap e with
      Ast.MetaId(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaId(name,not_unitary name,inherited))
    | Ast.MetaFunc(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaFunc(name,not_unitary name,inherited))
    | Ast.MetaLocalFunc(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaLocalFunc(name,not_unitary name,inherited))
    | _ -> k e in

  let expression r k e =
    match Ast.unwrap e with
      Ast.MetaConst(name,_,ty,inherited) ->
	Ast.rewrap e (Ast.MetaConst(name,not_unitary name,ty,inherited))
    | Ast.MetaErr(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaErr(name,not_unitary name,inherited))
    | Ast.MetaExpr(name,_,ty,inherited) ->
	Ast.rewrap e (Ast.MetaExpr(name,not_unitary name,ty,inherited))
    | Ast.MetaExprList(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaExprList(name,not_unitary name,inherited))
    | _ -> k e in

  let typeC r k e =
    match Ast.unwrap e with
      Ast.MetaType(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaType(name,not_unitary name,inherited))
    | _ -> k e in

  let param r k e =
    match Ast.unwrap e with
      Ast.MetaParam(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaParam(name,not_unitary name,inherited))
    | Ast.MetaParamList(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaParamList(name,not_unitary name,inherited))
    | _ -> k e in

  let define_body b =
    match Ast.unwrap b with
      Ast.DMetaId(name,_) ->
	Ast.rewrap b (Ast.DMetaId(name,not_unitary name))
    | _ -> b in
  
  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.MetaStmt(name,_,msi,inherited) ->
	Ast.rewrap e (Ast.MetaStmt(name,not_unitary name,msi,inherited))
    | Ast.MetaStmtList(name,_,inherited) ->
	Ast.rewrap e (Ast.MetaStmtList(name,not_unitary name,inherited))
    | Ast.Define(def,id,body) ->
	Ast.rewrap e (Ast.Define(def,id,define_body body))
    | _ -> k e in

  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing
      ident expression donothing typeC donothing param donothing rule_elem
      donothing donothing donothing donothing in

  fn.V.rebuilder_top_level

(* --------------------------------------------------------------------- *)

let rec loop defined = function
    [] -> ([],[],[])
  | (metavar_list,rule)::rest ->
      let locally_defined = List.map get_names metavar_list in
      let not_rebound = set_minus defined locally_defined in
      let ((all_info,(_,locally_free,free_usage)),table) =
	astfvs not_rebound rule in
      let (_,all_locally_frees,all_free_usage) = split3 all_info in
      let (later_free,later_nonlocally_used,later_rules) =
	loop (Common.union_set defined locally_defined) rest in
      let local_used_after =
	List.map (function x -> Common.union_set x later_free)
	  (inner_non_locally_used all_locally_frees) in
      let unitary_variables =
	List.map2 collect_unitary_variables all_free_usage local_used_after in
      let rule_with_fvs =
	List.map2 (update table metavar_list) unitary_variables rule in
      let rule_with_fvs =
	List.map2 drop_unitary_variables unitary_variables rule_with_fvs in
      (set_minus (Common.union_set locally_free later_free) locally_defined,
       local_used_after::later_nonlocally_used,
       rule_with_fvs::later_rules)

(* --------------------------------------------------------------------- *)
(* determine for each metavar whether it is declared in the current rule or
previously *)
(* this fills in the inherited field *)

(* special case for metavars *)
let update_metavars previous_metavars =
  let donothing r k e = k e in
  let mcode x = x in
  let free_mv (name,_,_) = List.mem name previous_metavars in

  let ident r k e =
    match Ast.unwrap e with
      Ast.MetaId(name,keep,_) ->
	Ast.rewrap e (Ast.MetaId(name,keep,free_mv name))
    | Ast.MetaFunc(name,keep,_) ->
	Ast.rewrap e (Ast.MetaFunc(name,keep,free_mv name))
    | Ast.MetaLocalFunc(name,keep,_) ->
	Ast.rewrap e (Ast.MetaLocalFunc(name,keep,free_mv name))
    | _ -> k e in

  let expression r k e =
    match Ast.unwrap e with
      Ast.MetaConst(name,keep,ty,_) ->
	Ast.rewrap e (Ast.MetaConst(name,keep,ty,free_mv name))
    | Ast.MetaErr(name,keep,_) ->
	Ast.rewrap e (Ast.MetaErr(name,keep,free_mv name))
    | Ast.MetaExpr(name,keep,ty,_) ->
	Ast.rewrap e (Ast.MetaExpr(name,keep,ty,free_mv name))
    | Ast.MetaExprList(name,keep,_) ->
	Ast.rewrap e (Ast.MetaExprList(name,keep,free_mv name))
    | _ -> k e in

  let typeC r k e =
    match Ast.unwrap e with
      Ast.MetaType(name,keep,_) ->
	Ast.rewrap e (Ast.MetaType(name,keep,free_mv name))
    | _ -> k e in

  let param r k e =
    match Ast.unwrap e with
      Ast.MetaParam(name,keep,_) ->
	Ast.rewrap e (Ast.MetaParam(name,keep,free_mv name))
    | Ast.MetaParamList(name,keep,_) ->
	Ast.rewrap e (Ast.MetaParamList(name,keep,free_mv name))
    | _ -> k e in

  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.MetaStmt(name,keep,msi,_) ->
	Ast.rewrap e (Ast.MetaStmt(name,keep,msi,free_mv name))
    | Ast.MetaStmtList(name,keep,_) ->
	Ast.rewrap e (Ast.MetaStmtList(name,keep,free_mv name))
    | _ -> k e in

  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing
      ident expression donothing typeC donothing param donothing rule_elem
      donothing donothing donothing donothing in

  fn.V.rebuilder_top_level

let update_loop nonlocally_used rules =
  let rec inner_loop = function
      ([],nonlocally_used) -> ([],nonlocally_used)
    | (x::xs,nlu::nonlocally_used) ->
	let (xs,rest_nonlocally_used) = inner_loop (xs, nonlocally_used) in
	(update_metavars nlu x::xs,rest_nonlocally_used)
    | _ -> failwith "not possible" in
  let rec outer_loop nonlocally_used = function
      [] -> []
    | x::xs ->
	let (x,nonlocally_used) = inner_loop (x,nonlocally_used) in
	x::(outer_loop nonlocally_used xs) in
  outer_loop ([]::List.concat nonlocally_used) rules

(* --------------------------------------------------------------------- *)

let free_vars rules =
  let (_,nonlocally_used,rules) = loop [] rules in
  (update_loop nonlocally_used rules,nonlocally_used)
