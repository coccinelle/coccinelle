(* adjust as convenient *)
let prefix = "/tmp/"
let prefix = ""

type dir = Minus | Plus

let space = Str.regexp " "

let scan pattern dir i =
  let rec loop skipping git =
    let line = input_line i in
    match Str.split space line with
      ["commit";git] -> loop false git
    | "-"::_ | "---"::_ when not skipping && dir = Minus ->
	(try
	  let _ = Str.search_forward pattern line 0 in
	  Printf.printf "%s\n" git;
	  loop true git
	with Not_found -> loop skipping git)
    | "+"::_ | "+++"::_ when not skipping && dir = Plus ->
	(try
	  let _ = Str.search_forward pattern line 0 in
	  Printf.printf "%s\n" git;
	  loop true git
	with Not_found -> loop skipping git)
    | _ -> loop skipping git in
  loop false ""

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

let _ =
  if not(Array.length Sys.argv = 4)
  then failwith "arguments: -/+ text version";
  let dir =
    match Array.get Sys.argv 1 with
      "-" -> Minus
    | "+" -> Plus
    | _ -> failwith "bad direction" in
  let pattern = Str.regexp (Array.get Sys.argv 2) in
  let i = open_git (Array.get Sys.argv 3) in
  try scan pattern dir i
  with End_of_file -> close_in i
