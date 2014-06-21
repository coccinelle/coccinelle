(* adjust as convenient *)
let prefix = "/tmp/"
let prefix = ""

(* The -grouped option means that all - and + code must appear in a
single contiguous block of - + code.  This option has no effect on the
other kinds of patterns, ie Changelog (C) or Context (@) *)

(* example: gitgrep -grouped -maxlen 25 - "[A-Z][A-Z]+" + "[A-Z][A-Z]+"
usb_21_22 *)

type dir = Minus | Plus | Context | ChangeLog

type res = Git of string | Block of int * string

let grouped = ref false
let maxlen = ref None

let space = Str.regexp " "

let matches pattern line =
  try let _ = Str.search_forward pattern line 0 in true
  with Not_found -> false

let res = ref []

let changed = ref 0
let badgits = ref []
let add x = function (y::_) as a -> if x = y then a else x::a | _ -> [x]
let too_many_changed = function Some n -> !changed > n | None -> false

(*let scan dir pattern i maxlen =
  let rec loop skipping cl git =
    let line = input_line i in
    match Str.split space line with
      ["commit";newgit] ->
	(if too_many_changed maxlen then badgits := add git !badgits);
	changed := 0;
	loop false true newgit
    | "diff"::_ ->
	(if too_many_changed maxlen then badgits := add git !badgits);
	changed := 0;
	loop skipping false git
    | _ ->
	if String.length line = 0
	then loop skipping cl git
	else
	  begin
	    let start = String.get line 0 in
	    (if start = '-' or start = '+' then changed := !changed + 1);
	    if not skipping &&
	      ((start = '-' && dir = Minus) or (start = '+' && dir = Plus) or
	       (cl && dir = ChangeLog) or
	       (not (start = '-') && not (start = '+') && dir = Context)) &&
	      matches pattern line
	    then (res := Git(git)::!res; loop true cl git)
	    else loop skipping cl git
	  end in
  loop false false ""*)

let scan allpatterns i maxlen =
  let rec loop cl git patterns =
    let line = input_line i in
    match Str.split space line with
      ["commit";newgit] ->
	(if too_many_changed maxlen
	then badgits := add git !badgits
	else if patterns = [] then res := Git(git)::!res);
	changed := 0;
	loop true newgit allpatterns
    | "diff"::_ ->
	(if too_many_changed maxlen then badgits := add git !badgits);
	changed := 0;
	loop false git patterns
    | _ ->
	if String.length line = 0
	then loop cl git patterns
	else
	  begin
	    let start = String.get line 0 in
	    (if start = '-' or start = '+' then changed := !changed + 1);
	    let remaining_patterns =
	      List.filter
		(function (dir,pattern) ->
		  not (* argument is true if match succeeds *)
		    (if ((start = '-' && dir = Minus) or
			 (start = '+' && dir = Plus) or
			 (cl && dir = ChangeLog) or
			 (not (start = '-') && not (start = '+') &&
			  dir = Context))
		    then matches pattern line
		    else false))
		patterns in
	    loop cl git remaining_patterns
	  end in
  loop false "" allpatterns

(* for Minus and Plus directions only *)
let scan_grouped allpatterns i maxlen =
  let block = ref 0 in
  (* mp = true in minus-plus region *)
  let rec loop mp git patterns =
    let line = input_line i in
    match Str.split space line with
      ["commit";newgit] ->
	(if too_many_changed maxlen
	then badgits := add git !badgits
	else if patterns = []
	then res := Block(!block,git) :: !res);
	changed := 0;
	block := 0;
	loop false newgit allpatterns
    | "diff"::_ ->
	(if too_many_changed maxlen
	then badgits := add git !badgits
	else if patterns = []
	then res := Block(!block,git) :: !res);
	changed := 0;
	loop false git patterns
    | _ ->
	if String.length line > 0
	then
	    let first_char = String.get line 0 in
	    (if first_char = '-' or first_char = '+'
	    then changed := !changed + 1);
	    let (new_mp,patterns) =
	      match first_char with
		'-' | '+' ->
		  if not mp
		  then
		    begin
		      (if patterns = []
		      then res := Block(!block,git) :: !res);
		      block := !block + 1;
		      (true,allpatterns)
		    end
		  else (true,patterns)
	      |	_ -> (false,patterns) in
	    let remaining_patterns =
	      List.filter
		(function (dir,pattern) ->
		  not (* argument is true if the pattern matches *)
		    (match (first_char,dir) with
		      ('-',Minus) | ('+',Plus) -> matches pattern line
		    | _ -> false))
		patterns in
	    loop new_mp git remaining_patterns
	else loop mp git patterns in
  loop false "" allpatterns

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

let version = ref None

let make_pattern s = Str.regexp (Printf.sprintf "\\b%s\\b" s)

let rec split_args = function
    [] -> []
  | "-grouped"::rest   -> grouped := true; split_args rest
  | "-maxlen"::len::rest -> maxlen := Some (int_of_string len); split_args rest
  | "-version"::v::rest -> version := Some v; split_args rest
  | "-"::pattern::rest -> (Minus,make_pattern pattern) :: split_args rest
  | "+"::pattern::rest -> (Plus,make_pattern pattern) :: split_args rest
  | "@"::pattern::rest -> (Context,make_pattern pattern) :: split_args rest
  | "C"::pattern::rest -> (ChangeLog,make_pattern pattern) :: split_args rest
  | _ -> failwith "bad argument list"

let process patterns version maxlen =
  res := [];
  let i =
    match version with Some version -> open_git version | None -> stdin in
  try
    if !grouped
    then scan_grouped patterns i maxlen
    else scan patterns i maxlen
  with End_of_file ->
   ((match version with Some _ -> close_in i | None -> ()); List.rev !res)

let _ =
  if Array.length Sys.argv < 3
  then failwith "arguments: -/+/@/C pattern -/+/@/C pattern ... version";
  let args = List.tl(Array.to_list Sys.argv) in
  let requirements = split_args args in
  let version = !version in
  (if !grouped
  then
    if List.exists (function (dir,_) -> not (dir = Minus or dir = Plus))
	requirements
    then
      failwith "only minus and plus requirements allowed in the grouped case");
  let res =
    List.map (function Git x -> x | Block (_,x) -> x)
      (process requirements version !maxlen) in
  let res =
    if !grouped
    then
      List.rev
	(List.fold_left
	   (function prev ->
	     function x -> if List.mem x prev then prev else x::prev)
	   [] res)
    else res in
  let res = List.filter (function x -> not(List.mem x !badgits)) res in
  List.iter (function name -> Printf.printf "%s\n" name) res
