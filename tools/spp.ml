(*
 * Copyright 2012-2015, Inria
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


# 0 "./spp.ml"
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
                  (Common.join " " cpp_flags)
                  file
                  file)
             in
               pr2 cmd2;
               let ret2 = Sys.command cmd2 in
		 if ret2 > 0 then exit ret2;
		 let sp_args = fix_args args file in
		 let cmd = "spatch " ^ (Common.join " " sp_args) in
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
                  (Common.join " " nargs)
                  outfile)
      in
        pr2 cmd2;
        Sys.command cmd2
    end

let _ = main ()
