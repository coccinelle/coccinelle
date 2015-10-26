(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* This reads Coccinelle command line arguments from a configuration file.
The configuration file is named .cocci.  The only format allowed at the
moment is

[spatch]
  options = --opt1 --opt2
  options = --opt3 --opt4
  ...

Perhaps entries for other tools may be added later.  Multiple options lines
are permitted only for readability.  Later lines extend and may override
earlier ones.

.cocci files can be placed in the user's home directory, the directory from
which spatch is called, and the directory provided with the --dir option.
The .cocci file in the user's home directory is processed first, the .cocci
file in the directory from which spatch is called is processed next, and
the .cocci file in the directory provided with the --dir option is
processed last.  In each case, the read options extend/override the
previously read ones.  In all cases, the user can extend/override the
options found in the .cocci files on the command line.

Newlines, even with \, are not tolerated in attribute values *)

let get_home () = Sys.getenv "HOME"

let split_at_spaces s = Str.split (Str.regexp "[ \t]+") s

(* not very robust - may just skip some garbage lines *)
let rec read_to_header i l =
  if l = "" || String.get l 0 = '#' (* comment character *)
  then read_to_header i (input_line i)
  else
    match Str.split_delim (Str.regexp_string "[") l with
      [before;after] ->
	(match Str.split_delim (Str.regexp_string "]") after with
	  [spatch;after] -> spatch
	| _ -> failwith ("unexpected entry: "^l))
    | _ -> read_to_header i (input_line i)

let parse_file file =
  let i = open_in file in
  let options = ref [] in
  let rec loop l =
    let header = read_to_header i l in
    match header with
      "spatch" ->
	let rec iloop _ =
	  let l = input_line i in
	  if l = "" || String.get l 0 = '#' (* comment character *)
	  then iloop()
	  else
	  (* bounded split doesn't split at = in value part *)
	    match Str.bounded_split (Str.regexp "[ \t]*=[ \t]*") l 2 with
	      [opts;new_options] ->
		(match split_at_spaces opts with
		  ["options"] ->
		    options := split_at_spaces new_options :: !options;
		    iloop()
		| [other] ->
		    failwith
		      (Printf.sprintf "expected options, found %s" other)
		| xs ->
		    failwith
		      ("options is the only supported attribute: "^l))
	    | _ -> loop l in
	iloop()
    | _ -> failwith "only spatch supported as a header in a .cocci file" in
  try loop (input_line i)
  with End_of_file -> List.concat (List.rev !options)

(* ------------------------------------------------------------------------ *)

let process_arglist strings = function
    spatch::rest ->
      let before = [spatch] in
      let after = rest in
      let rec loop = function
	  x::opt::xs when List.mem opt strings -> loop xs
	| ""::xs -> loop xs (* not sure why it would arise *)
	| x::xs ->
	    if String.get x 0 = '-'
	    then loop xs
	    else
	      if Filename.check_suffix x ".cocci"
	      then loop xs
	      else
		if Sys.file_exists x && Common.is_directory x
		then Some x
		else loop xs
	| [] -> None in
      (before,after,loop (List.rev rest))
  | [] -> failwith "arglist should always contain the command"

(* ------------------------------------------------------------------------ *)

let check_one file =
  if Sys.file_exists file
  then Some (parse_file file)
  else None

let unoption = function None -> [] | Some l -> l

let read_options strings arglist =
  let hd = get_home() in
  let cwd = Sys.getcwd() in
  let home_dir_options = check_one (Printf.sprintf "%s/.cocciconfig" hd) in
  let cwd_options = check_one ".cocciconfig" in
  let (before,after,dir) = process_arglist strings arglist in
  let dir_options =
    let rec loop dir =
      if dir = "/" || dir = "." || dir = hd || dir = cwd
      then None
      else
	match check_one (Printf.sprintf "%s/.cocciconfig" dir) with
	  None -> loop (Filename.dirname dir)
	| Some l -> Some l in
    match dir with
      None -> None
    | Some dir -> loop dir in
  before @
  (unoption home_dir_options) @ (unoption cwd_options) @ (unoption dir_options)
  @ after
