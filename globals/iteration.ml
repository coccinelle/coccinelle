(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type init_info = (string (* language *) * string (* rule name *)) *
      (string list (* defined virtual rules *) *
	 (string * string) list (* defined virtual env *))

let initialization_stack = ref ([] : init_info list)

(* ----------------------------------------------------------------------- *)

let base_file_list = ref ([] : string list)
let parsed_virtual_rules = ref ([] : string list)
let parsed_virtual_identifiers = ref ([] : string list)

(* ----------------------------------------------------------------------- *)

type pending_info = string list (* files to treat *) *
      string list * (* defined virtual rules *)
      (string * string) list (* virtual identifiers *)

let pending_instances_file = ref ([] : pending_info list)
let pending_instances_dir = ref ([] : pending_info list)

let reset _ =
  initialization_stack := [];
  base_file_list := [];
  parsed_virtual_rules := [];
  parsed_virtual_identifiers := [];
  pending_instances_file := [];
  pending_instances_dir := []

let add_pending_instance (files,vrules,vids,extend_vids) =
  let vids =
    if extend_vids
    then
      let newdefs = List.map fst vids in
      let olddefs =
	List.filter
	  (function (id,_) -> not (List.mem id newdefs))
	  !Flag.defined_virtual_env in
      vids @ olddefs
    else vids in
  match files with
    None ->
      pending_instances_dir :=
	(!base_file_list,vrules,vids) :: !pending_instances_dir
  | Some f ->
      (* if one specifies a file, it is assumed to be the current one *)
      (* put at the end of the list of information about this file *)
      let rec loop = function
	  [] -> [(f,vrules,vids)]
	| ((f1,vr1,vi1) as front)::rest ->
	    if f = f1
	    then front :: (loop rest)
	    else (f,vrules,vids) :: front :: rest in
      pending_instances_file := loop !pending_instances_file

let get_pending_instance _ =
  (if (List.length !pending_instances_file) > 0 ||
    (List.length !pending_instances_dir) > 0
  then
    Common.pr2
      (Printf.sprintf
	 "%d pending new file instances\n%d pending original file instances\n"
	 (List.length !pending_instances_file)
	 (List.length !pending_instances_dir)));
  match !pending_instances_file with
    [] ->
      (match !pending_instances_dir with
	[] -> None
      | x::xs ->
	  pending_instances_dir := xs;
	  Some x)
  | x::xs ->
      pending_instances_file := xs;
      Some x

let clear_pending_instance _ =
  pending_instances_file := [];
  pending_instances_dir := []
