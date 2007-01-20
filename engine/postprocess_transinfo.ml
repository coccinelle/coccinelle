(* two goals: first drop from the environments things that are not used,
   and second prompt for the names of fresh variables that are used *)

module Ast = Ast_cocci

let get_vars = function
    Lib_engine.Match(re) -> (Ast.get_fvs re, Ast.get_fresh re)
  | _ -> ([],[])

let string2val str = Lib_engine.NormalMetaVal(Ast_c.MetaIdVal(str))

let process_tree l =
  let (all_fresh,local_freshs,new_triples) =
    List.fold_left
      (function (all_fresh,local_freshs,new_triples) ->
	function (node,env,pred) ->
	  let (other,fresh) = get_vars pred in
	  let env = List.filter (function (x,_) -> List.mem x other) env in
	  (Common.union_set fresh all_fresh,
	   fresh::local_freshs,
	   (node,env,pred)::new_triples))
      ([],[],[]) l in
  let local_freshs = List.rev local_freshs in
  let new_triples = List.rev new_triples in
  let fresh_env =
    List.map
      (function fresh ->
	Printf.printf "name for %s: " fresh; (* not debugging code!!! *)
	(fresh,string2val(read_line())))
      all_fresh in
  let (_,res) =
    List.split
      (List.fold_left
	 (function freshs_node_env_preds ->
	   function (fresh,_) as elem ->
	     List.map
	       (function (freshs,((node,env,pred) as cur)) ->
		 if List.mem fresh freshs
		 then (freshs,(node,elem::env,pred))
		 else (freshs,cur))
	       freshs_node_env_preds)
	 (List.combine local_freshs new_triples)
	 fresh_env) in
  List.rev res
    
    
let process l =
  Common.uniq(List.concat (List.map process_tree l))
