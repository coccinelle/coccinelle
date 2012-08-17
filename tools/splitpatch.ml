(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
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


# 0 "./splitpatch.ml"
(* split patch per file *)

(* ------------------------------------------------------------------------ *)
(* The following are a reminder of what this information should look like.
These values are not used.  See the README file for information on how to
create a .splitpatch file in your home directory. *)

let from = ref "email@xyz.org"
let git_tree = ref "/var/linuxes/linux-next"
let git_options = ref "--cc=kernel-janitors@vger.kernel.org --suppress-cc=self"
let not_linux = ref "--suppress-cc=self"
let prefix_before = ref (Some "/var/linuxes/linux-next")
let prefix_after = ref (Some "/var/julia/linuxcopy")

(* ------------------------------------------------------------------------ *)
(* misc *)

let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)
let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l
let process_output_to_list = cmd_to_list
let cmd_to_list_and_status = process_output_to_list2

let safe_chop_extension s = try Filename.chop_extension s with _ -> s

let safe_get_extension s =
  match List.rev (Str.split (Str.regexp_string ".") s) with
    ext::_::rest -> Some (String.concat "." (List.rev rest))
  | _ -> None

(* ------------------------------------------------------------------------ *)
(* set configuration variables *)

let from_from_template template =
  let signed_offs =
    cmd_to_list (Printf.sprintf "grep Signed-off-by: %s" template) in
  match signed_offs with
    x::xs -> String.concat " " (Str.split (Str.regexp "[ \t]+") x)
  | _ -> failwith "No Signed-off-by in template file"

let from_from_gitconfig path =
  let config = path^"/.git/config" in
  if Sys.file_exists config
  then
    let i = open_in config in
    let rec inner_loop _ =
      let l = input_line i in
      match Str.split (Str.regexp "[ \t]+") l with
	"from"::"="::f -> from := String.concat " " f
      |	_ ->
	  if String.length l >= 1 && String.get l 0 = '['
	  then ()
	  else inner_loop() in
    let rec outer_loop _ =
      let l = input_line i in
      if l = "[sendemail]"
      then inner_loop()
      else outer_loop() in
    (try outer_loop() with Not_found -> ());
    close_in i

let read_configs template =
  let temporary_git_tree = ref None in
  git_options := "";
  prefix_before := None;
  prefix_after := None;
  (* get information in message template, lowest priority *)
  from := from_from_template template;
  (* get information in git config *)
  let rec loop = function
      "/" -> ()
    | path ->
	if Sys.file_exists ".git"
	then
	  begin temporary_git_tree := Some path; from_from_gitconfig path end
	else loop (Filename.dirname path) in
  loop (Sys.getcwd());
  (* get information from .splitpatch *)
  let home = List.hd(cmd_to_list "ls -d ~") in
  let config = home^"/.splitpatch" in
  (if Sys.file_exists config
  then
    let i = open_in config in
    let rec loop _ =
      let l = input_line i in
      (* bounded split doesn't split at = in value part *)
      (match Str.bounded_split (Str.regexp "[ \t]*=[ \t]*") l 2 with
	["from";s] -> from := s
      | ["git_tree";s] -> temporary_git_tree := Some s
      | ["git_options";s] -> git_options := s; not_linux := s
      | ["prefix_before";s] -> prefix_before := Some s
      | ["prefix_after";s] -> prefix_after := Some s
      | _ -> Printf.fprintf stderr "unknown line: %s\n" l);
      loop() in
    try loop() with End_of_file -> close_in i);
  match !temporary_git_tree with
    None -> failwith "Unable to find Linux source tree"
  | Some g -> git_tree := g

(* ------------------------------------------------------------------------ *)

let maintainer_command file =
  Printf.sprintf
    "cd %s; scripts/get_maintainer.pl --nokeywords --separator , --nogit-fallback --norolestats -f %s"
    !git_tree file

let subsystem_command file =
  Printf.sprintf
    "cd %s; scripts/get_maintainer.pl --nokeywords --nogit-fallback --subsystem --norolestats -f %s | grep -v @"
    !git_tree file

let checkpatch_command file =
  Printf.sprintf "cd %s; scripts/checkpatch.pl %s" !git_tree file

let default_string = "THE REST" (* split by file *)

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)
(* Template file processing *)

let read_up_to_dashes i =
  let lines = ref [] in
  let rec loop _ =
    let l = input_line i in
    if l = "---"
    then ()
    else begin lines := l :: !lines; loop() end in
  (try loop() with End_of_file -> ());
  let lines =
    match !lines with
      ""::lines -> List.rev lines (* drop last line if blank *)
    | lines -> List.rev lines in
  match lines with
    ""::lines -> lines (* drop first line if blank *)
  | _ -> lines

let get_template_information file =
  let i = open_in file in
  (* subject *)
  let subject = read_up_to_dashes i in
  match subject with
    [subject] ->
      let cover = read_up_to_dashes i in
      let message = read_up_to_dashes i in
      if message = []
      then (subject,None,cover)
      else (subject,Some cover,message)
  | _ ->
      failwith
	("Subject must be exactly one line "^
	 (string_of_int (List.length subject)))

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)
(* Patch processing *)

let spaces = Str.regexp "[ \t]+"

let fix_before_after l prefix = function
    Some old_prefix ->
      (match Str.split spaces l with
	("diff"|"+++"|"---")::_ ->
	  (match Str.split (Str.regexp old_prefix) l with
	    [a;b] ->
	      (match Str.split_delim (Str.regexp ("[ \t]"^prefix)) a with
		[_;""] -> a^b (* prefix is already there *)
	      |	_ -> a^prefix^b)
	  | _ -> l)
      |	_ -> l)
  | _ -> l

let fix_date l =
  match Str.split spaces l with
    (("+++"|"---") as a)::path::rest -> Printf.sprintf "%s %s" a path
  | _ -> l

(* ------------------------------------------------------------------------ *)

let is_diff = Str.regexp "diff "
let split_patch i =
  let patches = ref [] in
  let cur = ref [] in
  let get_size l =
    match Str.split_delim (Str.regexp ",") l with
      [_;size] -> int_of_string size
    | [_] -> 1
    | _ -> failwith ("bad size: "^l) in
  let rec read_diff_or_atat _ =
    let l = input_line i in
    let l = fix_date(fix_before_after l "a" !prefix_before) in
    let l = fix_date(fix_before_after l "b" !prefix_after) in
    match Str.split spaces l with
      "diff"::_ ->
	(if List.length !cur > 0
	then patches := List.rev !cur :: !patches);
	cur := [l];
	read_diff()
    | "@@"::min::pl::"@@"::rest ->
	let msize = get_size min in
	let psize = get_size pl in
	cur := l :: !cur;
	read_hunk msize psize
    | "\\"::_ -> cur := l :: !cur; read_diff_or_atat()
    | _ ->
	failwith
	  "expected diff or @@ (diffstat information should not be present)"
  and read_diff _ =
    let l = input_line i in
    let l = fix_date(fix_before_after l "a" !prefix_before) in
    let l = fix_date(fix_before_after l "b" !prefix_after) in
    cur := l :: !cur;
    match Str.split spaces l with
      "+++"::_ -> read_diff_or_atat()
    | _ -> read_diff()
  and read_hunk msize psize =
    if msize = 0 && psize = 0
    then read_diff_or_atat()
    else
      let l = input_line i in
      cur := l :: !cur;
      match String.get l 0 with
	'-' -> read_hunk (msize - 1) psize
      |	'+' -> read_hunk msize (psize - 1)
      |	_ -> read_hunk (msize - 1) (psize - 1) in
  try read_diff_or_atat()
  with End_of_file -> List.rev ((List.rev !cur)::!patches)

(* ------------------------------------------------------------------------ *)

let uctr = ref 0

let found_a_maintainer = ref false

let common_prefix l1 l2 =
  let rec loop = function
      ([],_) | (_,[]) -> []
    | (x::xs,y::ys) when x = y -> x :: (loop (xs,ys))
    | _ -> [] in
  match loop (l1,l2) with
    [] -> None
  | res -> Some (String.concat "/" res)

let find_common_path file cell =
  let fs = Str.split (Str.regexp "/") file in
  let rec loop = function
      [] ->
	let c1 = ref [] in
	cell := ((ref file),c1)::!cell;
	c1
    | (f,c1)::xs ->
	(match common_prefix fs (Str.split (Str.regexp "/") !f) with
	  None -> loop xs
	| Some cp -> f := cp; c1) in
  loop !cell

let resolve_maintainers patches =
  let maintainer_table = Hashtbl.create (List.length patches) in
  List.iter
    (function
	diff_line::rest ->
	  (match Str.split (Str.regexp " a/") diff_line with
	    [before;after] ->
	      (match Str.split spaces after with
		file::_ ->
		  let maintainers =
		    match (cmd_to_list (maintainer_command file)) with
		      m::_ -> found_a_maintainer := true; m
		    | [] ->
			(* maybe the file is new? *)
			(match
			  (cmd_to_list
			     (maintainer_command (Filename.dirname file)))
			with
			  m::_ -> found_a_maintainer := true; m
			| [] ->
			    uctr := !uctr + 1;
			    "unknown"^(string_of_int !uctr)) in
		  let subsystems =
		    cmd_to_list (subsystem_command file) in
		  let info = (subsystems,maintainers) in
		  let cell =
		    try Hashtbl.find maintainer_table info
		    with Not_found ->
		      let cell = ref [] in
		      Hashtbl.add maintainer_table info cell;
		      cell in
		  let cell1 = find_common_path file cell in
		  cell1 := (file,(diff_line :: rest)) :: !cell1
	      |	_ -> failwith "filename not found")
	  | _ ->
	      failwith (Printf.sprintf "prefix a/ not found in %s" diff_line))
      |	_ -> failwith "bad diff line")
    patches;
  maintainer_table

(* ------------------------------------------------------------------------ *)

let print_all o l =
  List.iter (function x -> Printf.fprintf o "%s\n" x) l

let make_mail_header o date maintainers ctr number subject =
  Printf.fprintf o "From nobody %s\n" date;
  Printf.fprintf o "From: %s\n" !from;
  (match Str.split (Str.regexp_string ",") maintainers with
    [x] -> Printf.fprintf o "To: %s\n" x
  | x::xs ->
      Printf.fprintf o "To: %s\n" x;
      Printf.fprintf o "Cc: %s\n" (String.concat "," xs)
  | _ -> failwith "no maintainers");
  if number = 1
  then Printf.fprintf o "Subject: [PATCH] %s\n\n" subject
  else Printf.fprintf o "Subject: [PATCH %d/%d] %s\n\n" ctr number subject

let make_message_files subject cover message date maintainer_table
    patch front add_ext nomerge =
  let ctr = ref 0 in
  let elements =
    Hashtbl.fold
      (function (services,maintainers) ->
	function diffs ->
	  function rest ->
	    if services=[default_string] or nomerge
	    then
	      (* if no maintainer, then one file per diff *)
	      let diffs =
		List.concat
		  (List.map (function (common,diffs) -> !diffs) !diffs) in
	      (List.map
		 (function (file,diff) ->
		   ctr := !ctr + 1;
		   (file,(!ctr,true,maintainers,[file],[diff])))
		 (List.rev diffs)) @
	      rest
	    else
	      (List.map
		 (function (common,diffs) ->
		   ctr := !ctr + 1;
		   let (files,diffs) = List.split (List.rev !diffs) in
		   (!common,(!ctr,false,maintainers,files,diffs)))
		 !diffs) @
	      rest)
      maintainer_table [] in
  let number = List.length elements in
  let generated =
    List.map
      (function (common,(ctr,the_rest,maintainers,files,diffs)) ->
	let output_file = add_ext(Printf.sprintf "%s%d" front ctr) in
	let o = open_out output_file in
	make_mail_header o date maintainers ctr number
	  (Printf.sprintf "%s: %s" common subject);
	print_all o message;
	Printf.fprintf o "\n---\n";
	let (nm,o1) = Filename.open_temp_file "patch" "patch" in
	List.iter (print_all o1) (List.rev diffs);
	close_out o1;
	let diffstat =
	  cmd_to_list
	    (Printf.sprintf "diffstat -p1 < %s ; /bin/rm %s" nm nm) in
	List.iter (print_all o) [diffstat];
	Printf.fprintf o "\n";
	List.iter (print_all o) diffs;
	Printf.fprintf o "\n";
	close_out o;
	let (info,stat) =
	  cmd_to_list_and_status
	    (checkpatch_command ((Sys.getcwd())^"/"^output_file)) in
	(if not(stat = Unix.WEXITED 0)
	then (print_all stderr info; Printf.fprintf stderr "\n"));
	output_file)
      (List.rev elements) in
  let later = add_ext(Printf.sprintf "%s%d" front (number+1)) in
  if Sys.file_exists later
  then Printf.fprintf stderr "Warning: %s and other files may be left over from a previous run\n" later;
  generated

let make_cover_file n subject cover front date maintainer_table =
  match cover with
    None -> ()
  | Some cover ->
      let common_maintainers =
	let intersect l1 l2 =
	  List.rev
	    (List.fold_left
	       (function i -> function cur ->
		 if List.mem cur l2 then cur :: i else i)
	       [] l1) in
	let start = ref true in
	String.concat ","
	  (Hashtbl.fold
	     (function (services,maintainers) ->
	       function diffs ->
		 function rest ->
		   let cur = Str.split (Str.regexp_string ",") maintainers in
		   if !start
		   then begin start := false; cur end
		   else intersect cur rest)
	     maintainer_table []) in
      let output_file = Printf.sprintf "%s.cover" front in
      let o = open_out output_file in
      make_mail_header o date common_maintainers 0 n subject;
      print_all o cover;
      Printf.fprintf o "\n";
      close_out o

let mail_sender = "git send-email" (* use this when it works *)
let mail_sender = "cocci-send-email.perl"

let generate_command front cover generated =
  let output_file = front^".cmd" in
  let o = open_out output_file in
  (match cover with
    None ->
      Printf.fprintf o
	"%s --auto-to --no-thread --from=\"%s\" %s $* %s\n"
	mail_sender !from !git_options
	(String.concat " " generated)
  | Some cover ->
      Printf.fprintf o
	"%s --auto-to --thread --from=\"%s\" %s $* %s\n"
	mail_sender !from !git_options
	(String.concat " " ((front^".cover") :: generated)));
  close_out o

let make_output_files subject cover message maintainer_table patch nomerge =
  let date = List.hd (cmd_to_list "date") in
  let front = safe_chop_extension patch in
  let add_ext =
    match safe_get_extension patch with
      Some ext -> (function s -> s ^ "." ^ ext)
    | None -> (function s -> s) in
  let generated =
    make_message_files subject cover message date maintainer_table
      patch front add_ext nomerge in
  make_cover_file (List.length generated) subject cover front date
    maintainer_table;
  generate_command front cover generated

(* ------------------------------------------------------------------------ *)

let nomerge = ref false

let parse_args l =
  let (other_args,files) =
    List.partition (function a -> String.length a > 1 && String.get a 0 = '-')
      l in
  let (nomergep,other_args) =
    List.partition (function a -> a = "-nomerge") other_args in
  (if not(nomergep = []) then nomerge := true);
  match files with
    [file] -> (file,String.concat " " other_args)
  | _ -> failwith "Only one file allowed"

let _ =
  let (file,git_args) = parse_args (List.tl (Array.to_list Sys.argv)) in
  let message_file = (safe_chop_extension file)^".msg" in
  (* set up environment *)
  read_configs message_file;
  (* get message information *)
  let (subject,cover,message) = get_template_information message_file in
  (* split patch *)
  let i = open_in file in
  let patches = split_patch i in
  close_in i;
  let maintainer_table = resolve_maintainers patches in
  (if !found_a_maintainer = false then git_options := !not_linux);
  (if not (git_args = "") then git_options := !git_options^" "^git_args);
  make_output_files subject cover message maintainer_table file !nomerge
