(* for each rule return:
1. Hash table of all of the terms in the rule with their free variables
2. The list of metavariables that are declared in this one and used without
being redeclared in subsequent ones *)

module Ast = Ast_cocci
module V = Visitor_ast
  
(* --------------------------------------------------------------------- *)
(* Computing free variables *)

type anything =
    Rule_elem        of Ast.rule_elem
  | Statement        of Ast.statement
  | StatementDots    of Ast.statement Ast.dots

type free_table = (anything,string list) Hashtbl.t

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

let astfvs bound =
  let free_table = (Hashtbl.create(50) : free_table) in

  let metaid (x,_,_) = x in

  let bind (unbound1,free1) (unbound2,free2) =
    (Common.union_set unbound1 unbound2, Common.union_set free1 free2) in
  let option_default = ([],[]) in

  let mcode r (_,_,mcodekind) =
    let process_anything_list_list anythings =
      let astfvs = r.V.combiner_anything in
      List.fold_left bind ([],[])
	(List.map
	   (function l ->
	     List.fold_left bind ([],[]) (List.map astfvs l))
	   anythings) in
    match mcodekind with
      Ast.MINUS(anythings) -> process_anything_list_list anythings
    | Ast.CONTEXT(befaft) ->
	(match befaft with
	  Ast.BEFORE(ll) -> process_anything_list_list ll
	| Ast.AFTER(ll) -> process_anything_list_list ll
	| Ast.BEFOREAFTER(llb,lla) ->
	    bind
	      (process_anything_list_list lla)
	      (process_anything_list_list llb)
	| Ast.NOTHING -> option_default)
    | Ast.PLUS -> option_default in

  let donothing recursor k e = k e in (* just combine in the normal way *)

  let astfvident recursor k i =
    match Ast.unwrap i with
      Ast.MetaId(name,true,_) | Ast.MetaFunc(name,true,_)
    | Ast.MetaLocalFunc(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id]) (mcode recursor name)
	else bind ([id],[id]) (mcode recursor name)
    | _ -> k i in

  let astfvexpr recursor k e =
    match Ast.unwrap e with
      Ast.MetaConst(name,true,_,_) | Ast.MetaErr(name,true,_)
    | Ast.MetaExpr(name,true,_,_) | Ast.MetaExprList(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id]) (mcode recursor name)
	else bind ([id],[id]) (mcode recursor name)
    | _ -> k e in

  let astfvtypeC recursor k ty =
    match Ast.unwrap ty with
      Ast.MetaType(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id]) (mcode recursor name)
	else bind ([id],[id]) (mcode recursor name)
    | _ -> k ty in

  let astfvparam recursor k p =
    match Ast.unwrap p with
      Ast.MetaParam(name,true,_) | Ast.MetaParamList(name,true,_) ->
	let id = metaid name in
	if List.mem id bound
	then bind ([],[id]) (mcode recursor name)
	else bind ([id],[id]) (mcode recursor name)
    | _ -> k p in

  let astfvrule_elem recursor k re =
    let (unbound,_) as res =
      match Ast.unwrap re with
	Ast.MetaRuleElem(name,true,_) | Ast.MetaStmt(name,true,_,_)
      | Ast.MetaStmtList(name,true,_) ->
	  let id = metaid name in
	  if List.mem id bound
	  then bind ([],[id]) (mcode recursor name)
	  else bind ([id],[id]) (mcode recursor name)
      | _ -> k re in
    Hashtbl.add free_table (Rule_elem re) unbound;
    res in

  let astfvstatement recursor k s =
    let (unbound,_) as res = k s in
    Hashtbl.add free_table (Statement s) unbound;
    res in

  let astfvstatement_dots recursor k s = 
    let (unbound,_) as res = k s in
    Hashtbl.add free_table (StatementDots s) unbound; res in

  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing astfvstatement_dots
      astfvident astfvexpr donothing astfvtypeC donothing astfvparam donothing
      astfvrule_elem astfvstatement donothing donothing donothing in

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

let update table =
  let statement r k s =
    let fvs = Hashtbl.find table (Statement s) in
    let (s,l,_,d) = k s in
    (s,l,fvs,d) in

  let statement_dots r k s =
    let fvs = Hashtbl.find table (StatementDots s) in
    let (s,l,_,d) = k s in
    (s,l,fvs,d) in

  let rule_elem r k s =
    let fvs = Hashtbl.find table (Rule_elem s) in
    let (s,l,_,d) = k s in
    (s,l,fvs,d) in

  let mcode x = x in
  let donothing r k e = k e in

  (V.rebuilder
     mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
     donothing donothing donothing statement_dots
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

let set_minus s minus = List.filter (function n -> not (List.mem n minus)) s
let set_intersect s1 s2 = List.filter (function n -> List.mem n s2) s1
let rec loop defined = function
    [] -> ([],[],[])
  | (metavar_list,rule)::rest ->
      let locally_defined = List.map get_names metavar_list in
      let not_rebound = set_minus defined locally_defined in
      let ((all_info,(_,locally_free)),table) = astfvs not_rebound rule in
      let (_,all_locally_frees) = List.split all_info in
      let (later_free,later_nonlocally_used,later_rules) =
	loop (Common.union_set defined locally_defined) rest in
      (set_minus (Common.union_set locally_free later_free) locally_defined,
       (List.map (function x -> Common.union_set x later_free)
	  (inner_non_locally_used all_locally_frees))
       ::later_nonlocally_used,
       (List.map (update table) rule)::later_rules)

(* --------------------------------------------------------------------- *)
(* determine for each metavar whether it is declared in the current rule or
previously *)

(* special case for metavars *)
let update_metavars previous_metavars =
  let donothing r k e = k e in
  let mcode x = x in
  let free_mv (name,_,_) = List.mem name previous_metavars in

  let ident r k e =
    match Ast.unwrap e with
      Ast.MetaId(name,true,_) ->
	Ast.rewrap e (Ast.MetaId(name,true,free_mv name))
    | Ast.MetaFunc(name,true,_) ->
	Ast.rewrap e (Ast.MetaFunc(name,true,free_mv name))
    | Ast.MetaLocalFunc(name,true,_) ->
	Ast.rewrap e (Ast.MetaLocalFunc(name,true,free_mv name))
    | _ -> k e in

  let expression r k e =
    match Ast.unwrap e with
      Ast.MetaConst(name,true,ty,_) ->
	Ast.rewrap e (Ast.MetaConst(name,true,ty,free_mv name))
    | Ast.MetaErr(name,true,_) ->
	Ast.rewrap e (Ast.MetaErr(name,true,free_mv name))
    | Ast.MetaExpr(name,true,ty,_) ->
	Ast.rewrap e (Ast.MetaExpr(name,true,ty,free_mv name))
    | Ast.MetaExprList(name,true,_) ->
	Ast.rewrap e (Ast.MetaExprList(name,true,free_mv name))
    | _ -> k e in

  let typeC r k e =
    match Ast.unwrap e with
      Ast.MetaType(name,true,_) ->
	Ast.rewrap e (Ast.MetaType(name,true,free_mv name))
    | _ -> k e in

  let param r k e =
    match Ast.unwrap e with
      Ast.MetaParam(name,true,_) ->
	Ast.rewrap e (Ast.MetaParam(name,true,free_mv name))
    | Ast.MetaParamList(name,true,_) ->
	Ast.rewrap e (Ast.MetaParamList(name,true,free_mv name))
    | _ -> k e in

  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.MetaStmt(name,true,msi,_) ->
	Ast.rewrap e (Ast.MetaStmt(name,true,msi,free_mv name))
    | Ast.MetaStmtList(name,true,_) ->
	Ast.rewrap e (Ast.MetaStmtList(name,true,free_mv name))
    | _ -> k e in

  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing
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
