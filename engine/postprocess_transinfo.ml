(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
* This file is part of Coccinelle.
* 
* Coccinelle is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, according to version 2 of the License.
* 
* Coccinelle is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
* 
* The authors reserve the right to distribute this or future versions of
* Coccinelle under other licenses.
*)


(* two goals: first drop from the environments things that are not used,
   and second prompt for the names of fresh variables that are used *)

(* have to add in the whole inherited env because inherited variables are
not returned by get_fvs.  It would be better if that were the case, but
since at the moment I think we can only inherit one value per variable,
perhaps it doesn't matter - these bindings will always be the same no matter
how we reached a particular match *)

module Ast = Ast_cocci

let extra_counter = ref 0
let get_extra _ =
  let ctr = !extra_counter in
  extra_counter := !extra_counter + 1;
  "__extra_counter__"^(string_of_int ctr)

let read_fresh_id () =
  try 
    let s = read_line () in
    match Parse_c.tokens_of_string s with
      [Parser_c.TIdent _; Parser_c.EOF _] -> s
    | _ -> failwith ("wrong fresh id: " ^ s)
  with End_of_file -> get_extra()

let get_vars = function
    Lib_engine.Match(re) -> (Ast.get_fvs re, Ast.get_fresh re)
  | _ -> ([],[])

let string2val str = Lib_engine.NormalMetaVal(Ast_c.MetaIdVal(str))

(* ----------------------------------------------------------------------- *)
(* Get values for fresh variables *)

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
      (function ((r,n) as fresh) ->
	Printf.printf "%s: name for %s: " r n; (* not debugging code!!! *)
	flush stdout;
	(fresh,string2val(read_fresh_id())))
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

(* ----------------------------------------------------------------------- *)
(* Create the environment to be used afterwards *)

let collect_used_after used_after envs =
  List.map (List.filter (function (v,vl) -> List.mem v used_after)) envs

(* ----------------------------------------------------------------------- *)
(* entry point *)

let process used_after inherited_env l =
  extra_counter := 0;
  let (trees, fresh_envs) =
    List.split (List.map (process_tree inherited_env) l) in
  (Common.uniq(List.concat trees), collect_used_after used_after fresh_envs)
