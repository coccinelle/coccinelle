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


# 0 "./gitgrep.ml"
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
	then (res := Git(git)::!res; loop true cl git)
	else loop skipping cl git in
  loop false false ""

(* for Minus and Plus directions only *)
let scan_grouped dir pattern i =
  let block = ref 0 in
  (* mp = true in minus-plus region *)
  let rec loop mp git =
    let line = input_line i in
    match Str.split space line with
      ["commit";git] -> loop false git
    | "diff"::_ -> loop false git
    | _ ->
	if String.length line > 0
	then
	    let first_char = String.get line 0 in
	    let new_mp =
	      match first_char with
		'-' | '+' -> (if not mp then block := !block + 1; true)
	      |	_ -> false in
	    match (first_char,dir) with
	      ('-',Minus) | ('+',Plus) ->
		let info = Block(!block,git) in
		(if matches pattern line && not (List.mem info !res)
		then res := info::!res);
		loop new_mp git
	    | _ -> loop new_mp git
	else loop mp git in
  loop false ""

let scan_line max i =
  let rec loop skipping num git =
    let line = input_line i in
    match Str.split space line with
      ["commit";git1] ->
	loop false (-1) git1
    | "diff"::_ ->
	if num > max && not skipping
	then (res:=Git(git)::!res;loop true (num+1) git)
	else loop skipping (if num = (-1) then 1 else num+1) git
    | _ ->
	if num > max && not skipping
	then (res:=Git(git)::!res;loop true (num+1) git)
	else loop skipping (if num = (-1) then num else num+1) git in
  loop false (-1) ""

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
  | "-grouped"::rest   -> grouped := true; split_args rest
  | "-maxlen"::len::rest -> maxlen := Some (int_of_string len); split_args rest
  | "-"::pattern::rest -> (Minus,Str.regexp pattern) :: split_args rest
  | "+"::pattern::rest -> (Plus,Str.regexp pattern) :: split_args rest
  | "@"::pattern::rest -> (Context,Str.regexp pattern) :: split_args rest
  | "C"::pattern::rest -> (ChangeLog,Str.regexp pattern) :: split_args rest
  | _ -> failwith "bad argument list"

let process_one (dir,pattern) version =
  res := [];
  let i = open_git version in
  try
    if !grouped && (dir = Minus or dir = Plus)
    then scan_grouped dir pattern i
    else scan dir pattern i
  with End_of_file -> (close_in i; List.rev !res)

let process_len max version =
  res := [];
  let i = open_git version in
  try scan_line max i
  with End_of_file -> (close_in i; List.rev !res)

let inter l1 l2 =
  List.rev
    (List.fold_left
       (function prev ->
	 function
	     (Git(git)) as x ->
	       let rec loop = function
		   [] -> prev
		 | Git(git1)::rest when git = git1 -> x::prev
		 | Block(b1,git1)::rest when git = git1 -> Block(b1,git1)::prev
		 | _::rest -> loop rest in
	       loop l2
	   | (Block(block,git)) as x ->
	       let rec loop = function
		   [] -> prev
		 | Git(git1)::rest when git = git1 -> x::prev
		 | Block(b1,git1)::rest when block = b1 && git = git1 ->
		     Block(b1,git1)::prev
		 | _::rest -> loop rest in
	       loop l2)
       [] l1)

let _ =
  if Array.length Sys.argv < 4
  then failwith "arguments: -/+/@/C pattern -/+/@/C pattern ... version";
  let args = List.tl(Array.to_list Sys.argv) in
  let version = List.hd(List.rev args) in
  let pairs = List.rev(List.tl(List.rev args)) in
  let requirements = split_args pairs in
  let res =
    List.map (function Git x -> x | Block (_,x) -> x)
      (List.fold_left
	 (function all ->
	   function pattern ->
	     inter (process_one pattern version) all)
	 (process_one (List.hd requirements) version)
	 (List.tl requirements)) in
  let res =
    if !grouped
    then
      List.rev
	(List.fold_left
	   (function prev ->
	     function x -> if List.mem x prev then prev else x::prev)
	   [] res)
    else res in
  let res =
    match !maxlen with
      None -> res
    | Some max ->
	let badgits = process_len max version in
	List.filter (function x -> not(List.mem (Git(x)) badgits)) res in
  List.iter (function name -> Printf.printf "%s\n" name) res
