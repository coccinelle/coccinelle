(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(**
 * This script adds a legal header to various kinds of files.
 * To apply it to a directory, change to this directory and do:
 * ocaml str.cma path/to/licensify-new.ml
 *)

let lines =
["This file is part of Coccinelle, licensed under the terms of the GPL v2.";
"See copyright.txt in the Coccinelle source code for more information.";
"The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website";
]


let comment_lines =
  List.map (function x -> if x <> "" then " * "^x else " *") lines

let cpp_lines = "/*" :: comment_lines @ [" */"]

let ml_lines = "(*" :: comment_lines @ [" *)"]

let make_lines = (List.map (function x -> if x <> "" then "# "^x else "#") lines)
let c_lines = (List.map (function x -> if x <> "" then "// "^x else "//") lines)

let do_one file =
  let lines =
    if Filename.check_suffix file ".cocci" then c_lines else
    if Filename.check_suffix file ".mly"   then cpp_lines else
    if Filename.check_suffix file ".ml"    then ml_lines else
    if Filename.check_suffix file ".mli"   then ml_lines else
    if Filename.check_suffix file ".mll"   then ml_lines else
    if Filename.check_suffix file ".pl"    then make_lines else
    if Filename.basename file = "Makefile" then make_lines else
    failwith (Printf.sprintf "unknown file type: %s" file) in
  let tmpfl = Filename.temp_file "cocci_licence" "orig" in
  let _     = Sys.command (Printf.sprintf "cp %s %s" file tmpfl) in
  let o     = open_out file in
  List.iter (function l -> Printf.fprintf o "%s\n" l) lines;
  Printf.fprintf o "\n";
  close_out o;
  let _ = Sys.command (Printf.sprintf "cat %s >> %s" tmpfl file) in
  Sys.remove tmpfl

(* pad's modif *)
let (+>) o f = f o
let cat file =
  let chan = open_in file in
  let rec cat_aux acc ()  =
      (* can't do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () +> List.rev +> (fun x -> close_in chan; x)


let rec process dir =
  let files =
    try
      List.map (function fl -> dir^"/"^fl)
	(Array.to_list(Sys.readdir dir))
    with Sys_error _ -> [] in
  List.iter (function file ->
    try
      let xs = cat file in
      if List.exists (fun s ->
        s = "* This file is part of Coccinelle."
        ||
        s = "# This file is part of Coccinelle."
        ||
        s = "// This file is part of Coccinelle."
        ||
        Str.string_match (Str.regexp_string "Copyright") s 0
      ) xs
      then print_string ("already processed: " ^ file ^ "\n")
      else begin
        do_one file;
        print_string ("processed: " ^ file ^ "\n");
      end
    with _ ->
      print_string ("skipped: " ^ file ^ "\n");
      ()
  ) files;
  (* pad: no recursive call in directory List.iter process files *)
  ()

let _ = process "."
