(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

let cache_threshold = 500 (** caching of header file information *)

let elem_threshold = 10

let include_headers_for_types = ref false

type include_options =
    I_UNSPECIFIED | I_NO_INCLUDES | I_NORMAL_INCLUDES
  | I_ALL_INCLUDES | I_REALLY_ALL_INCLUDES

let include_options = ref I_UNSPECIFIED

let include_path = ref ([] : string list)
(* if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

let relax_include_path = ref false

let extra_includes = ref ([] : string list)

(* finding among the #include the one that we need to parse
 * because they may contain useful type definition or because
 * we may have to modify them
 *
 * For the moment we base in part our heuristic on the name of the file, e.g.
 * serio.c is related we think to #include <linux/serio.h>
 *)
let include_table = ("include_table", ref 0, Hashtbl.create(101))
let find_table = ("find_table", ref 0, Hashtbl.create(101))

let cache_find (_,_,cache) k =
  let (ct,res) = Hashtbl.find cache k in
  ct := !ct + 1;
  res

let cache_add (nm,ct,cache) k v =
  ct := !ct + 1;
  (if !ct > cache_threshold
  then
    begin
      Hashtbl.iter
	(fun k (vct,v) ->
	  if !vct < elem_threshold
	  then
	    begin
	      Hashtbl.remove cache k;
	      ct := !ct - 1
	    end
	  else vct := 0)
	cache
    end);
  Hashtbl.add cache k (ref 1, v)

let interpret_include_path relpath =
  let maxdepth = List.length relpath in
  let unique_file_exists dir f =
    let cmd =
      Printf.sprintf "find %s -maxdepth %d -mindepth %d -path \"*/%s\""
	dir maxdepth maxdepth f in
    try cache_find find_table cmd
    with Not_found ->
      let res =
	match Common.cmd_to_list cmd with
	  [x] -> Some x
	| _ -> None in
      cache_add find_table cmd res;
      res in
  let native_file_exists dir f =
    let f = Filename.concat dir f in
    if Sys.file_exists f
    then Some f
    else None in
  let rec search_include_path exists searchlist relpath =
    match searchlist with
      []       -> None
    | hd::tail ->
	(match exists hd relpath with
	  Some x -> Some x
	| None -> search_include_path exists tail relpath) in
  let rec search_path exists searchlist = function
      [] ->
	let res = String.concat "/" relpath in
	cache_add include_table (searchlist,relpath) res;
	Some res
    | (hd::tail) as relpath1 ->
	let relpath1 = String.concat "/" relpath1 in
	(match search_include_path exists searchlist relpath1 with
	  None -> search_path unique_file_exists searchlist tail
	| Some f ->
	    cache_add include_table (searchlist,relpath) f;
	    Some f) in
  let searchlist =
    match !include_path with
      [] -> ["include"]
    | x -> List.rev x in
  try Some(cache_find include_table (searchlist,relpath))
  with Not_found -> search_path native_file_exists searchlist relpath
