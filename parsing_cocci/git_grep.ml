(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

exception Failed

let interpret dir query suffixes =
  let collect query =
    let cmd =
      Printf.sprintf "cd %s > /dev/null; git grep -l -w %s -- %s" dir query suffixes in
    let (res,code) = Common.cmd_to_list_and_status cmd in
    if code = Unix.WEXITED 0 || code = Unix.WEXITED 1
    then res
    else raise Failed in
  try
    match List.map collect query with
      [] -> failwith "git grep: no files: not possible"
    | x::xs ->
	let res = List.fold_left Common.inter_set x xs in
	Some(List.map (function x -> dir^"/"^x) res)
  with Failed -> None
