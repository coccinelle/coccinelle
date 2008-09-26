let lines =
["Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen";
"Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller";
"This file is part of Coccinelle.";
"";
"Coccinelle is free software: you can redistribute it and/or modify";
"it under the terms of the GNU General Public License as published by";
"the Free Software Foundation, according to version 3 of the License.";
"";
"Coccinelle is distributed in the hope that it will be useful,";
"but WITHOUT ANY WARRANTY; without even the implied warranty of";
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the";
"GNU General Public License for more details.";
"";
"You should have received a copy of the GNU General Public License";
"along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.";
"The authors reserve the right to distribute this or future versions of";
"Coccinelle under other licenses"
]

let c_lines = "/*" :: (List.map (function x -> "* "^x) lines) @ ["*/"]

let ml_lines = "(*" :: (List.map (function x -> "* "^x) lines) @ ["*)"]

let make_lines = (List.map (function x -> "# "^x) lines)

let do_one file =
  let lines =
    if Filename.check_suffix file ".mly" then c_lines else
    if Filename.check_suffix file ".ml" then ml_lines else
    if Filename.check_suffix file ".mll" then ml_lines else
    if Filename.basename file = "Makefile" then make_lines else
    failwith (Printf.sprintf "unknown file type: %s" file) in
  let _ = Sys.command (Printf.sprintf "cp %s /tmp/tmpfl" file) in
  let o = open_out file in
  List.iter (function l -> Printf.fprintf o "%s\n" l) lines;
  Printf.fprintf o "\n";
  Printf.fprintf o "\n";
  close_out o;
  let _ = Sys.command (Printf.sprintf "cat /tmp/tmpfl >> %s" file) in
  ()

let rec process dir =
  let files =
    try
      List.map (function fl -> dir^"/"^fl)
	(Array.to_list(Sys.readdir dir))
    with Sys_error _ -> [] in
  List.iter (function file -> try do_one file with _ -> ()) files;
  List.iter process files

let _ = process "."
