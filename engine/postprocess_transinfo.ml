(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* two goals: first drop from the environments things that are not used,
   and second prompt for the names of fresh variables that are used *)

(* have to add in the whole inherited env because inherited variables are
not returned by get_fvs.  It would be better if that were the case, but
since at the moment I think we can only inherit one value per variable,
perhaps it doesn't matter - these bindings will always be the same no matter
how we reached a particular match *)

module Ast = Ast_cocci

module ParamsType =
  struct
    type t = Ast_c.metavars_binding
    let compare = Stdcompat.Stdlib.compare
  end

module ParamsMap = Map.Make(ParamsType)

let extra_counter = ref 0

let reset_fresh_counter () = extra_counter := 0

let get_extra _ =
  let ctr = !extra_counter in
  extra_counter := !extra_counter + 1;
  "__extra_counter__"^(string_of_int ctr)

let get_seeded seed =
  let ctr = !extra_counter in
  extra_counter := !extra_counter + 1;
  seed^(string_of_int ctr)

let read_fresh_id _ =
  try
    let s = read_line () in
    match Parse_c.tokens_of_string s None with
      [Parser_c.TIdent _; Parser_c.EOF _] -> s
    | [Parser_c.EOF _] -> get_extra()
    | _ -> failwith ("wrong fresh id: " ^ s)
  with End_of_file -> get_extra()

let get_vars = function
    Lib_engine.Match(re) -> (Ast.get_fvs re, Ast.get_fresh re)
  | _ -> ([],[])

let string2val str = Lib_engine.NormalMetaVal(Ast_c.MetaIdVal str)

(* ----------------------------------------------------------------------- *)
(* Get values for fresh variables *)

let paramsmap_find_opt key m =
  try Some(ParamsMap.find key m) with Not_found -> None

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
      (function
	  ((r,n) as fresh,Ast.NoVal) ->
	    Printf.printf "%s: name for %s: " r n; (* not debugging code!!! *)
	    flush stdout;
	    (fresh,let v = string2val(read_fresh_id()) in function _ -> v)
	| ((r,n) as fresh,Ast.StringSeed seed) ->
	    (fresh,let v = string2val(get_seeded seed) in function _ -> v)
	| ((r,n) as fresh,Ast.ListSeed seed) ->
	    (fresh,
	     function env ->
	       let strings =
		 List.map
		   (function
		       Ast.SeedString s -> s
		     | Ast.SeedId id ->
			 try
			   (match List.assoc id env with
			     Lib_engine.NormalMetaVal(Ast_c.MetaIdVal str) ->
			       str
			   | _ -> failwith "bad id value")
			 with
			   Not_found ->
			     failwith
			       ("fresh: no binding for meta "^(Dumper.dump id)))
		   seed in
	       string2val(String.concat "" strings))
        | ((r, n) as fresh, Ast.ScriptSeed(name, lang, params, pos, body)) ->
            let res = ref ParamsMap.empty in
            let make_fresh_id env =
              let args =
                List.map
                  (fun (((rule, name) as meta_name), _) ->
                    try match List.assoc meta_name env with
                    | Lib_engine.NormalMetaVal v -> (meta_name, v)
                    | _ ->
                        failwith
                          (Printf.sprintf
                             "Undesired metavar_binding in line %d"
                             (snd pos))
                    with
                    | Not_found ->
                        let get_meta_names l =
                          List.map
                            (fun (mn, _) -> Ast.string_of_meta_name mn)
                            l in
                        let string_of_list l =
                          "[" ^ String.concat "; " l ^ "]" in
                        failwith
                          (Printf.sprintf
                             "%s: script on variable %s cannot be evaluated in line %d. available: %s\nwanted: %s"
                             r n (snd pos)
                             (string_of_list (get_meta_names env))
                             (string_of_list (get_meta_names params)))
                          )
                  params in
              match paramsmap_find_opt args !res with
		Some x -> x
	      | None ->
		  let fresh_id =
                    match lang with
                    | "ocaml" ->
			Run_ocamlcocci.run_fresh_id name (List.map snd args)
                    | "python" -> Pycocci.run_fresh_id args pos body
                    | _ ->
			failwith
			  "languages other than ocaml or python not supported" in
		  let r = string2val fresh_id in
                  res := ParamsMap.add args r !res;
		  r in
            (fresh, make_fresh_id)
      )
      all_fresh in
  let (_,res) =
    List.split
      (List.fold_left
	 (function freshs_node_env_preds ->
	   function (fresh,fn) ->
	     List.map
	       (function (freshs,((node,env,pred) as cur)) ->
		 try
		   let _ = List.assoc fresh freshs in
		   (freshs,(node,(fresh,fn env)::env,pred))
		 with Not_found -> (freshs,cur))
	       freshs_node_env_preds)
	 (List.combine local_freshs new_triples)
	 fresh_env) in
  (List.rev res, fresh_env)

(* ----------------------------------------------------------------------- *)
(* Create the environment to be used afterwards *)
(* anything that a used after fresh variable refers to has to have a unique
value, by virtue of the placement of the quantifier.  thus we augment
inherited env with the first element of l, if any *)

let collect_used_after used_after envs l inherited_env =
  List.map2
    (function env -> function l ->
      let inherited_env =
	match l with
	  [] -> inherited_env
	| (_,fse,_)::_ ->
	    (* l represents the result from a single tree. fse is a complete
	       environment in that tree.  for a fresh seed, the environments
	       for all leaves contain the same information *)
	    fse@inherited_env  in
      List.map
	(function (v,vl) -> (v,vl inherited_env))
	(List.filter (function (v,vl) -> List.mem v used_after) env))
    envs l

(* ----------------------------------------------------------------------- *)
(* distinguish between distinct witness trees, each gets an index n *)
(* index should be global, so that it can extend over environments *)

let index = ref (-1)

let fold_left_with_index f acc =
  let rec fold_lwi_aux acc = function
    | [] -> acc
    | x::xs ->
	let n = !index in
	index := !index + 1;
	fold_lwi_aux (f acc x n) xs
  in fold_lwi_aux acc

let numberify same_env trees =
  let same_env (a1,env1,b1) (a2,env2,b2) =
    a1 = a2 && b1 = b2 && same_env env1 env2 in
  let trees =
    fold_left_with_index
      (function acc -> function xs -> function n ->
	(List.map (function x -> (n,x)) xs) @ acc)
      [] trees in
  List.fold_left
    (function res ->
      function (n,x) ->
	let (same,diff) = List.partition (function (ns,xs) -> same_env x xs) res in
	match same with
	  [(ns,xs)] -> (n::ns,xs)::diff
	| _ -> ([n],x)::res)
    [] trees

(* ----------------------------------------------------------------------- *)
(* entry point *)

let process used_after inherited_env same_env l =
  let (trees, fresh_envs) =
    List.split (List.map (process_tree inherited_env) l) in
  let trees = numberify same_env trees in
  (trees, collect_used_after used_after fresh_envs l inherited_env)
