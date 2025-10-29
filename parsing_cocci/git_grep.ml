(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

exception Failed

let interpret dir query suffixes =
  let collect query =
    let cmd =
      Printf.sprintf "cd %s > /dev/null; git grep -l -w %s -- %s" dir query suffixes in
    let (res,code) = Common.cmd_to_list_and_status cmd in
    if code = Unix.WEXITED 0 || code = Unix.WEXITED 1
    then
      List.rev
	(List.fold_left
	   (fun prev f ->
	     let len = String.length f in
	     if len < 5
	     then f :: prev
	     else
	       let trailing = String.sub f (len - 5) 5 in
	       if List.mem trailing [".in.c";".in.h"]
	       then prev
	       else f :: prev)
	   [] res)
    else raise Failed in
  try
    match List.map collect query with
      [] -> failwith "git grep: no files: not possible"
    | x::xs ->
	let res = List.fold_left Common.inter_set x xs in
	Some(List.map (function x -> dir^"/"^x) res)
  with Failed -> None
