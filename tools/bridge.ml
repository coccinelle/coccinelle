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


# 0 "./bridge.ml"
let drop_spaces s =
  String.concat "" (Str.split (Str.regexp "[ ]+") s)

let parse_line fp l n =
  if List.mem l fp
  then None
  else
    if Str.string_match (Str.regexp "#") l 0
    then None (* comment line *)
    else
      let top_split = Str.split (Str.regexp ":") l in
      match top_split with
	cocci::first::others ->
	  let rec loop tag = function
	      [x] ->
		let x =
		  String.concat "\\ " (Str.split (Str.regexp "[ ]+") x) in
		[(tag,x)]
	    | first::rest ->
		let splitted = Str.split (Str.regexp "[ ]+") first in
		(match List.rev splitted with
		  new_tag::info ->
		    let rest = loop new_tag rest in
		    (tag,String.concat "\\ " (List.rev info))::rest
		|	_ -> failwith "bad element")
	    | _ -> failwith "no data" in
	  Some (cocci,loop (drop_spaces first) others)
      | _ -> failwith (Printf.sprintf "bad line: %s" l)

let collect_lines fp i =
  let lines = ref [] in
  let ln = ref 0 in
  let rec loop _ =
    ln := !ln + 1;
    (match parse_line fp (input_line i) !ln with
      Some l ->
	if List.mem l !lines
	then ()
	else lines := l::!lines
    | None -> ());
    loop() in
  try loop() with End_of_file -> !lines

(* --------------------------------------------------------------------- *)

let process_fp fl =
  let i = open_in fl in
  let lines = ref ([] : string list) in
  let rec loop _ =
    let l = input_line i in
    (if not(Str.string_match (Str.regexp "#") l 0)
    then lines := l :: !lines);
    loop() in
  (try loop() with End_of_file -> ());
  close_in i;
  !lines

(* --------------------------------------------------------------------- *)
(* same info, different categories *)

let discard_ambiguous lines =
  let rec loop = function
      [] -> []
    | (cocci,tags)::rest ->
	let (same,others) =
	  List.partition
	    (function (cocci2,tags2) -> tags = tags2 && not(cocci = cocci2))
	    rest in
	match same with
	  [] -> (cocci,tags)::loop rest
	| _ ->
	    Printf.printf "ignoring ambiguity:\n";
	    List.iter
	      (function (cocci,tags) ->
		Printf.printf "%s: %s\n" cocci
		  (String.concat ", "
		     (List.map
			(function (tag,tagval) ->
			  Printf.sprintf "%s: %s" tag tagval)
			tags)))
	      ((cocci,tags)::same);
	    loop others in
  loop lines

(* --------------------------------------------------------------------- *)
(* only actually collects the rightmost element into ors *)

let split_or (cocci,line) =
  let rev = List.rev line in
  (cocci,List.rev(List.tl rev), List.hd rev)

let collect_ors fp lines =
  let rec loop = function
      [] -> failwith "no lines"
    | [line] ->
	let (c,k,v) = split_or line in
	((c,k,[v]),[])
    | line::xs ->
	let (c,k,v) = split_or line in
	let ((c1,k1,v1),rest) = loop xs in
	if c = c1 && k = k1 && not (k = [])
	then
	  if List.mem v v1
	  then ((c1,k1,v1),rest)
	  else ((c1,k1,v::v1),rest)
	else ((c,k,[v]),((c1,k1,v1)::rest)) in
  let ((c,k,v),rest) = loop lines in
  let res = (c,k,v)::rest in
  List.fold_left
    (function prev ->
      function (c,k,v) ->
	match v with
	  [] -> failwith "not possible"
	| [x] -> (c,k@v) :: prev
	| (tag,_)::_ ->
	    (*let vs =
	      Printf.sprintf "%s:(%s)" tag
		(String.concat "|"
		   (List.sort compare
		      (List.map (function (_,vl) -> vl) v))) in
	    let attempt =
	      Printf.sprintf "%s: %s %s" c
		(String.concat " " (List.map (function (k,v) -> k^":"^v) k))
		vs in*)
	    if true (*List.mem attempt fp*)
	    then
	      let vs =
		Printf.sprintf "\\\\\\\\\\(%s\\\\\\\\\\)"
		  (String.concat "\\\\\\\\\\|"
		     (List.sort compare
			(List.map (function (_,vl) -> vl) v))) in
	      (c,k@[(tag,vs)]) :: prev
	    else (List.map (function vi -> (c,k@[vi])) v) @ prev)
    [] res

(* --------------------------------------------------------------------- *)

let command s =
  let _ = Sys.command s in
  ()

let created = ref ([] : (string * (string list ref * out_channel)) list)

let mktag n = Printf.sprintf "x%d" n

let created_files = ref ([] : (string * int ref) list)

let process_line env (cocci,tags) =
  let files = List.filter (function (c,f) -> c = cocci) env in
  List.iter
    (function (_,cocci_file) ->
      let resdir = Filename.chop_extension cocci_file in
      (if not(Sys.file_exists cocci_file)
      then failwith "no cocci file");
      let (n,o) =
	try List.assoc resdir !created
	with Not_found ->
	  begin
	    if Sys.file_exists resdir
	    then
	      command
		(Printf.sprintf
		   "test %s -nt %s && /bin/rm -r -f %s && mkdir %s"
		   cocci_file resdir resdir resdir)
	    else command (Printf.sprintf "mkdir %s" resdir);
	    let files = Printf.sprintf "%s/files" resdir in
	    let o = open_out files in
	    Printf.fprintf o "all: real_all\n\n";
	    let cell = ((ref []),o) in
	    created := (resdir,cell) :: !created;
	    cell
	  end in
      let temp_file = Filename.temp_file cocci ".cocci" in
      command (Printf.sprintf "cp %s %s" cocci_file temp_file);
      let first_tag_val =
	match tags with
	  [] -> failwith "no tags"
	| (_,first_tag_val)::_ ->
	    let cell =
	      try List.assoc first_tag_val !created_files
	      with Not_found ->
		let c = ref (-1) in
		created_files := (first_tag_val,c)::!created_files;
		c in
	    cell := !cell + 1;
	    if !cell = 0
	    then first_tag_val
	    else Printf.sprintf "%s%d" first_tag_val !cell in
      List.iter
	(function (tag,tagval) ->
	  command
	    (Printf.sprintf "sed s+%s+%s+ %s > %s_out; cp %s_out %s"
	       tag tagval temp_file temp_file temp_file temp_file))
	tags;
      command
	(Printf.sprintf "mv %s %s/%s.cocci" temp_file resdir first_tag_val);
      Printf.fprintf o "%s.out:\n\tmono_spatch_linux %s.cocci ${ARGS}\n\n"
	first_tag_val first_tag_val;
      n := (first_tag_val^".out") :: !n)
    files

(* --------------------------------------------------------------------- *)

let rec mkenv = function
    [] -> []
  | [_] -> failwith "required arguments: file (category x cocci file)*"
  | category::cocci::rest ->
      if Filename.check_suffix cocci ".cocci"
      then (category,cocci)::mkenv rest
      else failwith "required arguments: file (category x cocci file)*"

let rec upto = function
    0 -> []
  | n -> (mktag (n-1)) :: (upto (n-1))

let _ =
  let (no_ors,args) =
    List.partition (function "-no_ors" -> true | _ -> false)
      (Array.to_list Sys.argv) in
  let (file,fp,env) =
    match List.tl args with
      file::env ->
	let rec loop prev = function
	    [] ->
	      if prev = ""
	      then ([],[])
	      else ([prev],[])
	  | x::xs ->
	      try
		let _ = Str.search_forward (Str.regexp ".cocci") x 0 in
		if prev = ""
		then ([],x::xs)
		else ([],prev::x::xs)
	      with Not_found ->
		let (fp,env) = loop x xs in
		if prev = ""
		then (fp,env)
		else (prev::fp,env) in
	let (fp,env) = loop "" env in
	(file,fp,mkenv env)
    | _ -> failwith "one argument expected" in
  let fp = List.fold_left (@) [] (List.map process_fp fp) in
  let i = open_in file in
  let lines = collect_lines fp i in
  let lines = if no_ors = [] then collect_ors fp lines else lines in
  close_in i;
  let lines = discard_ambiguous lines in
  List.iter (process_line env) lines;
  List.iter
    (function (resdir,(n,o)) ->
      Printf.fprintf o "real_all: %s\n"
	(String.concat " " (List.rev !n));
      Printf.fprintf o "\tcat %s > completed\n"
	(String.concat " " (List.rev !n));
      close_out o)
    !created
