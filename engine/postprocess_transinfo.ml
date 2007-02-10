(* two goals: first drop from the environments things that are not used,
   and second prompt for the names of fresh variables that are used *)

(* have to add in the whole inherited env because inherited variables are
not returned by get_fvs.  It would be better if that were the case, but
since at the moment I think we can only inherit one value per variable,
perhaps it doesn't matter - these bindings will always be the same no matter
how we reached a particular match *)

module Ast = Ast_cocci

let get_vars = function
    Lib_engine.Match(re) -> (Ast.get_fvs re, Ast.get_fresh re)
  | _ -> ([],[])

let string2val str = Lib_engine.NormalMetaVal(Ast_c.MetaIdVal(str))

let process_tree inherited_env l =
  let (all_fresh,local_freshs,new_triples) =
    List.fold_left
      (function (all_fresh,local_freshs,new_triples) ->
	function (node,env,pred) ->
	  let (other,fresh) = get_vars pred in
	  let env = List.filter (function (x,_) -> List.mem x other) env in
	  (Common.union_set fresh all_fresh,
	   fresh::local_freshs,
	   (node,env@inherited_env,pred)::new_triples))
      ([],[],[]) l in
  let local_freshs = List.rev local_freshs in
  let new_triples = List.rev new_triples in
  let fresh_env =
    List.map
      (function fresh ->
	Printf.printf "name for %s: " fresh; (* not debugging code!!! *)
	flush stdout;
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
  (List.rev res, fresh_env)

(* needs to follow the same strategy as the function with the same name in
wrapper_ctl.ml *)
let collect_used_after used_after envs =
  let print_var var = Printf.printf "%s" var in
  List.concat
    (List.map
       (function used_after_var ->
	 let vl =
	   List.fold_left
	     (function rest ->
	       function env ->
		 try
		   let vl = List.assoc used_after_var env in
		   match rest with
		     None -> Some vl
		   | Some old_vl when vl = old_vl -> rest
		   | Some old_vl -> print_var used_after_var;
		       Format.print_newline();
		       Pretty_print_engine.pp_binding_kind2 old_vl;
		       Format.print_newline();
		       Pretty_print_engine.pp_binding_kind2 vl;
		       Format.print_newline();
		       failwith "incompatible values"
		 with Not_found -> rest)
	     None envs in
	 match vl with
	   None -> [] (*raise (INCOMPLETE_BINDINGS used_after_var)*)
	 | Some vl -> [(used_after_var, vl)])
       used_after)
    
    
    
let process used_after inherited_env l =
  let (trees, fresh_envs) =
    List.split (List.map (process_tree inherited_env) l) in
  (Common.uniq(List.concat trees), collect_used_after used_after fresh_envs)
