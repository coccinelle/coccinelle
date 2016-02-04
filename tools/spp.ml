(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

exception WrongArguments

(* could do via a List.filter because cpp flags are simple as it's
 * "-I/usr/include" not ["-I";"/usr/include"] like in ocaml so no
 * need to look multiple args.
 *)
let rec cpp_flags_filter xs =
  match xs with
  | [] -> []
  | x::xs ->
      (match x with
      | s  when x =~ "-D.*" ->
          s::cpp_flags_filter xs
      | s  when s =~ "-I.*" ->
          s::cpp_flags_filter xs
      | _ ->
          cpp_flags_filter xs
      )

let is_compile_command xs =
  List.mem "-c" xs

let source_file xs =
  xs +> List.filter (fun s -> s =~ ".*\\.c$")

let rec fix_args args file =
  match args with
      [] -> []
    | hd::tail ->
	if hd = file then
	  (hd^".i") :: tail
	else
	  hd::fix_args tail file

let rec get_outputfile args =
  match args with
      [] -> ([],"")
    | hd::tail ->
	if hd = "-o" then
	  let (hd',tail') = match tail with
	      hd'::tail' -> (hd',tail')
	    | _          -> raise WrongArguments
	  in
	    (tail', hd')
	else
	  let (ntail, out) = get_outputfile tail in
	    (hd::ntail, out)

let main () =
  let args = List.tl (Array.to_list Sys.argv) in
  (*args +> List.iter pr2;*)
  if is_compile_command args
  then begin
    let file = source_file args in
      (match file with
	 | [file] ->
             let cpp_flags = cpp_flags_filter args in
             let cmd2 =
               (spf "cpp %s %s > %s.i"
                  (String.concat " " cpp_flags)
                  file
                  file)
             in
               pr2 cmd2;
               let ret2 = Sys.command cmd2 in
		 if ret2 > 0 then exit ret2;
		 let sp_args = fix_args args file in
		 let cmd = "spatch " ^ (String.concat " " sp_args) in
		   pr2 cmd;
		   let ret = Sys.command cmd in
		     exit ret

	 | [] -> failwith "could not find name of source file"
	 | x::y::xs -> failwith "multiple source files"
      );
  end
  else
    begin
      let (nargs, outfile) = get_outputfile args in
      let cmd2 =
        (spf "cat %s > %s"
                  (String.concat " " nargs)
                  outfile)
      in
        pr2 cmd2;
        Sys.command cmd2
    end

let _ = main ()
