(* adjust as convenient *)
let prefix = "/tmp/"
let prefix = ""

type dir = Minus | Plus | Context | ChangeLog

let space = Str.regexp " "

let matches pattern line =
  try let _ = Str.search_forward pattern line 0 in true
  with Not_found -> false

let res = ref []

let scan dir pattern i =
  let rec loop skipping cl git =
    let line = input_line i in
    match Str.split space line with
      ["commit";git] -> loop false true git
    | "diff"::_ -> loop skipping false git
    | _ ->
	if String.length line > 0 && not skipping &&
	  ((String.get line 0 = '-' && dir = Minus) or
	   (String.get line 0 = '+' && dir = Plus) or
	   (cl && dir = ChangeLog) or
	   (not (String.get line 0 = '-') && not (String.get line 0 = '+') &&
	    dir = Context)) &&
	  matches pattern line
	then (res := git::!res; loop true cl git)
	else loop skipping cl git in
  loop false false ""

let dot = Str.regexp "\\."

let open_git file =
  let tmp = prefix^file in
  if Sys.file_exists tmp
  then open_in tmp
  else
    match List.rev (Str.split dot file) with
      last::rest ->
	let last_int = int_of_string last in
	if last_int = 0
	then
	  failwith
	    "can't go back one version from 0; make the log file by hand";
	let prev =
	  String.concat "." (List.rev ((string_of_int (last_int-1))::rest)) in
	let _ =
	  Sys.command
	    (Printf.sprintf "git log -p v%s..v%s > %s" prev file tmp) in
	open_in tmp
    | _ -> open_in file

let rec split_args = function
    [] -> []
  | "-"::pattern::rest -> (Minus,Str.regexp pattern) :: split_args rest
  | "+"::pattern::rest -> (Plus,Str.regexp pattern) :: split_args rest
  | "@"::pattern::rest -> (Context,Str.regexp pattern) :: split_args rest
  | "C"::pattern::rest -> (ChangeLog,Str.regexp pattern) :: split_args rest
  | _ -> failwith "bad argument list"

let process_one (dir,pattern) version =
  res := [];
  let i = open_git version in
  try scan dir pattern i
  with End_of_file -> (close_in i; List.rev !res)

let inter l1 l2 = List.filter (function x -> List.mem x l1) l2

let _ =
  if Array.length Sys.argv < 4
  then failwith "arguments: -/+/@ pattern -/+/@ pattern ... version";
  let args = List.tl(Array.to_list Sys.argv) in
  let version = List.hd(List.rev args) in
  let pairs = List.rev(List.tl(List.rev args)) in
  let requirements = split_args pairs in
  let res =
    List.fold_left
      (function all ->
	function pattern ->
	  inter (process_one pattern version) all)
      (process_one (List.hd requirements) version)
      (List.tl requirements) in
  List.iter (function name -> Printf.printf "%s\n" name) res
