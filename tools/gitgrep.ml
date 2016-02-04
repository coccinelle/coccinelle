(*
 * Copyright 2012-2014, INRIA
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
usb_21_22
maxlen is per file, regardless of whether the pattern is found in that file.
 *)

type dir = Minus | Plus | Context | ChangeLog
type orientation = Pos | Neg

type res = Git of string | Block of int * string

let grouped = ref false
let maxlen = ref None

let space = Str.regexp " "

let matches pattern line =
  try let _ = Str.search_forward pattern line 0 in true
  with Not_found -> false

let match_one start dir cl pattern line =
  if ((start = '-' && dir = Minus) or
      (start = '+' && dir = Plus) or
      (cl && dir = ChangeLog) or
      (not (start = '-') && not (start = '+') && dir = Context))
  then matches pattern line
  else false

let res = ref []

let changed = ref 0
let badgits = ref []
let add x = function (y::_) as a -> if x = y then a else x::a | _ -> [x]
let too_many_changed = function Some n -> !changed > n | None -> false

let scan allpatterns i maxlen =
  let allpospatterns =
    List.filter (function (_,Pos,_) -> true | _ -> false) allpatterns in
  let allnegpatterns =
    List.filter (function (_,Neg,_) -> true | _ -> false) allpatterns in
  let git = ref "" in
  let pospatterns = ref allpospatterns in
  let negpatterns = ref allnegpatterns in
  let clear_patterns _ = pospatterns := []; negpatterns := [] in
  let ender isalldone =
    (if too_many_changed maxlen
    then badgits := add !git !badgits
    else
      if isalldone
      then
	  if !pospatterns = [] && !negpatterns = allnegpatterns
	  then res := Git(!git)::!res);
    if isalldone
    then
      (pospatterns := allpospatterns;
       negpatterns := allnegpatterns) in
  let rec loop cl =
    let line = input_line i in
    match Str.split space line with
      ["commit";newgit] ->
	ender true;
	changed := 0;
	git := newgit;
	loop true
    | "diff"::_ ->
	ender false;
	changed := 0;
	loop false
    | _ ->
	if String.length line = 0
	then loop cl
	else
	  begin
	    let start = String.get line 0 in
	    (if start = '-' or start = '+' then changed := !changed + 1);
	    let fails =
	      List.exists
		(function (dir,ok,pattern) ->
		  match_one start dir cl pattern line)
		!negpatterns in
	    if fails
	    then
	      begin
		clear_patterns();
		loop cl
	      end
	    else
	      begin
		let remaining_patterns =
		  List.filter
		    (function (dir,ok,pattern) ->
		      not (* argument is true if match succeeds *)
			(match_one start dir cl pattern line))
		    !pospatterns in
		pospatterns := remaining_patterns;
		loop cl
	      end
	  end in
  try loop false
  with End_of_file -> ender true

(* for Minus and Plus directions only *)
let scan_grouped allpatterns i maxlen =
  let block = ref 0 in
  let git = ref "" in
  let patterns = ref allpatterns in
  let ender isdone =
    if too_many_changed maxlen
    then badgits := add !git !badgits
    else if isdone
    then
      begin
	(if !patterns = [] then res := Block(!block,!git)::!res);
	patterns := []
      end in
  (* mp = true in minus-plus region *)
  let rec loop mp =
    let line = input_line i in
    match Str.split space line with
      ["commit";newgit] ->
	ender true;
	patterns := allpatterns;
	changed := 0;
	block := 0;
	git := newgit;
	loop false
    | "diff"::_ ->
	ender false;
	changed := 0;
	loop false
    | _ ->
	if String.length line > 0
	then
	    let first_char = String.get line 0 in
	    (if first_char = '-' or first_char = '+'
	    then changed := !changed + 1);
	    let new_mp =
	      match first_char with
		'-' | '+' ->
		  if not mp
		  then
		    begin
		      ender true;
		      block := !block + 1;
		      true
		    end
		  else true
	      |	_ -> false in
	    let remaining_patterns =
	      List.filter
		(function (dir,ok,pattern) ->
		  not (* argument is true if the pattern matches *)
		    (match (first_char,dir) with
		      ('-',Minus) | ('+',Plus) -> matches pattern line
		    | _ -> false))
		!patterns in
	    patterns := remaining_patterns;
	    loop new_mp
	else loop mp in
  try loop false
  with End_of_file -> ender true

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
  | key::pattern::rest ->
      let pattern = make_pattern pattern in
      let rest = split_args rest in
      (match key with
	"-" -> (Minus,Pos,pattern) :: rest
      | "+" -> (Plus,Pos,pattern) :: rest
      | "@" -> (Context,Pos,pattern) :: rest
      | "C" -> (ChangeLog,Pos,pattern) :: rest
      | "no-" -> (Minus,Neg,pattern) :: rest
      | "no+" -> (Plus,Neg,pattern) :: rest
      | "no@" -> (Context,Neg,pattern) :: rest
      | "noC" -> (ChangeLog,Neg,pattern) :: rest
      | _ -> failwith "bad argument list")
  | _ -> failwith "bad argument list"

let process patterns version maxlen =
  res := [];
  let i =
    match version with Some version -> open_git version | None -> stdin in
  (if !grouped
  then scan_grouped patterns i maxlen
  else scan patterns i maxlen);
  ((match version with Some _ -> close_in i | None -> ()); List.rev !res)

let _ =
  if Array.length Sys.argv < 3
  then failwith "arguments: -/+/@/C pattern -/+/@/C pattern ... version";
  let args = List.tl(Array.to_list Sys.argv) in
  let requirements = split_args args in
  let version = !version in
  (if !grouped
  then
    if List.exists
	(function
	    (_,Neg,_) -> true
	  | (dir,_,_) -> not (dir = Minus or dir = Plus))
	requirements
    then
      failwith
	"only minus and plus requirements, and no negated requirements, allowed in the grouped case");
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
