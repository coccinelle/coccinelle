(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

let include_headers_for_types = ref false

let is_header filename =
  Filename.check_suffix filename ".h" ||
  Filename.check_suffix filename ".h.res" (* for okfailed testing *)

type parsing_style =
  | Parse_no_includes
  | Parse_local_includes
  | Parse_all_includes
  | Parse_really_all_includes

let string_of_parsing_style = function
  | Parse_no_includes -> "Parse_no_includes"
  | Parse_local_includes -> "Parse_local_includes"
  | Parse_all_includes -> "Parse_all_includes"
  | Parse_really_all_includes -> "Parse_really_all_includes"

let _parsing_style_set = ref false
let _parsing_style = ref Parse_local_includes

let get_parsing_style () = !_parsing_style
let set_parsing_style ps =
  _parsing_style := ps;
  _parsing_style_set := true

let is_parsing_style_set () = !_parsing_style_set

let parse_all_includes parsing_style =
  (parsing_style = Parse_all_includes) ||
    (parsing_style = Parse_really_all_includes)

let include_path = ref ([] : string list)

let relax_include_path = ref false
let for_tests = ref false
(* if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

let extra_includes = ref ([] : string list)

(* finding among the #include the one that we need to parse
 * because they may contain useful type definition or because
 * we may have to modify them
 *
 * For the moment we base in part our heuristic on the name of the file, e.g.
 * serio.c is related we think to #include <linux/serio.h>
 *)

let unique_file_table = ref []

let include_table = Common.create_bounded_cache 500 (([],[]),None)

let interpret_include_path relpath =
  let unique_file_exists dir f =
    try
      let info = List.assoc dir !unique_file_table in
      try Some (Hashtbl.find info f)
      with Not_found -> None
    with Not_found -> None in
  let rec native_file_exists dir f =
    let f = Filename.concat dir f in
    if Sys.file_exists f
    then Some f
    else None in
  let rec search_include_path exists searchlist relpath =
    match searchlist with
      [] -> None
    | hd::tail ->
	(match exists hd relpath with
	  Some x -> Some x
	| None -> search_include_path exists tail relpath) in
  let rec search_path exists searchlist = function
      [] -> None
    | (hd::tail) as relpath1 ->
	let relpath1 = String.concat "/" relpath1 in
	(match search_include_path exists searchlist relpath1 with
	  None -> search_path exists searchlist tail
	| (Some _) as res -> res) in
  let searchlist =
    match !include_path with
      [] ->
	(try if Sys.is_directory "include" then ["include"] else []
	with Sys_error _ -> [])
    | x -> List.rev x in
  try Common.find_bounded_cache include_table (searchlist,relpath)
  with Not_found ->
    (match search_path native_file_exists searchlist relpath with
      None ->
	let res = search_path unique_file_exists searchlist relpath in
	Common.extend_bounded_cache include_table (searchlist,relpath) res;
	(if res = None && !Flag_parsing_c.verbose_includes
	then
	  Common.pr2
	    (Printf.sprintf "failed on %s" (String.concat "/" relpath)));
	res
    | (Some _) as res ->
	Common.extend_bounded_cache include_table (searchlist,relpath) res;
	res)

let should_parse parsing_style filename incl = match parsing_style with
  | Parse_no_includes -> false
  | Parse_local_includes when is_header filename -> false
  | Parse_local_includes ->
    (match incl with
    | Ast_c.Local _ -> true
    | Ast_c.Weird _ -> false
    | Ast_c.NonLocal _ -> false
    )
  | Parse_all_includes -> not (is_header filename)
  | Parse_really_all_includes -> true

let resolve filename parsingstyle x =
  let all_includes = parse_all_includes parsingstyle in
  let dir = Filename.dirname filename in
  let clean s =
    if Str.string_match (Str.regexp_string "./") s 0
    then String.sub s 2 (String.length s - 2)
    else s in
  match x with
    | Ast_c.Local include_path ->
      let relpath = String.concat "/" include_path in
      let f = Filename.concat dir (clean relpath) in
      if (Sys.file_exists f)
      then Some f
      else if !relax_include_path
      (* for our tests, all the files are flat in the current dir *)
      then
        let attempt2 = Filename.concat dir (Common.last include_path) in
        if all_includes && not (Sys.file_exists attempt2)
        then interpret_include_path include_path
        else Some attempt2
      else if !for_tests
      then interpret_include_path [Common.last include_path]
      else if all_includes
      then interpret_include_path include_path
      else None
    | Ast_c.NonLocal include_path ->
      if all_includes ||
         Common.fileprefix (Common.last include_path) =
           Common.fileprefix filename
      then interpret_include_path include_path
      else None
    | Ast_c.Weird _ -> None

(* ------------------------------------------------------------------------ *)

let setup_unique_search cores searchlist =
  let searchlist = List.filter (function f -> Sys.file_exists f) searchlist in
  let cores =
    match cores with
      None -> 1
    | Some x -> x in
  let looper f l =
    if cores > 1
    then Parmap.parmap ~ncores:cores f (Parmap.L l)
    else List.map f l in
  let process dir =
    let lines =
      let cmd = Printf.sprintf "find %s -name \"*h\"" dir in
      Common.cmd_to_list cmd in
    let lines =
      List.fold_left
	(fun prev cur ->
	  let last = Filename.basename cur in
	  let two_last =
	    Filename.concat (Filename.basename (Filename.dirname cur)) last in
	  (last,cur) :: (two_last,cur) :: prev)
	[] lines in
    let lines = List.sort compare lines in
    let rec loop good bad = function
	[] -> good
      | [(x,xp)] ->
	  if List.mem x bad
	  then good
	  else  ((x,xp)::good)
      | (x,xp)::(((y,yp)::rest) as arest) ->
	  if List.mem x bad
	  then loop good bad arest
	  else if x = y
	  then loop good (x::bad) rest
	  else loop ((x,xp)::good) bad arest in
    (dir,loop [] [] lines) in
  let res = looper process searchlist in
  unique_file_table :=
    List.map
      (function (dir,lst) ->
	let tbl = Hashtbl.create 101 in
	List.iter (function (fl,src) -> Hashtbl.add tbl fl src) lst;
	(dir,tbl))
      res
