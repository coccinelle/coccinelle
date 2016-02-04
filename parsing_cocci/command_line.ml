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


# 0 "./command_line.ml"
(* ---------------------------------------------------------------------- *)
(* useful functions *)

let starts_with c s =
  if String.length s > 0 && String.get s 0 = c
  then Some (String.sub s 1 ((String.length s) - 1))
  else None

let ends_with c s =
  if String.length s > 0 && String.get s ((String.length s) - 1) = c
  then Some (String.sub s 0 ((String.length s) - 1))
  else None

let split_when fn l =
  let rec loop acc = function
  | []    -> raise Not_found
  | x::xs ->
      (match fn x with
	Some x -> List.rev acc, x, xs
      |	None -> loop (x :: acc) xs) in
  loop [] l

(* ---------------------------------------------------------------------- *)
(* make a semantic patch from a string *)

let find_metavariables tokens =
  let rec loop env = function
      [] -> (env,[])
    | x :: xs ->
	(* single upper case letter is a metavariable *)
	let (x,xs,env) =
(*
  Testing for uppercase and length is not enough as "+" is
  a single character identical in upper/lower case.
*)
	  (*
		The ":" delimiter could not be used two times
		1) Str.split
		2) split_when (ends_with ...)

		Otherwise split_when will raise a Not_found exception.
	  *)
	  match Str.bounded_split (Str.regexp ":") x 2 with
	    [before;after] ->
	      let (ty,endty,afterty) = split_when (ends_with ':') (after::xs) in
	      let decl = 
		Printf.sprintf "%s %s;\n"
		  (String.concat "" (ty@[endty]))
		  before in
	      (try
		if decl = List.assoc before env
		then (before,afterty,env)
		else failwith (before^" already declared with another type")
	      with Not_found ->
		let env = (before, decl) :: env in
		(before,afterty,env))
	  | _ ->
	      if Str.string_match (Str.regexp "[A-Z]") x 0
	      then
		begin
		  try let _ = Some(List.assoc x env) in (x,xs,env)
		  with Not_found ->
		    let env =
		      (x,(Printf.sprintf "metavariable %s;\n" x)) :: env in
		    (x,xs,env)
		end
	      else (x,xs,env) in
	let (env,sp) = loop env xs in
	(env,x::sp) in
  loop [] tokens

let find_when_dots tokens =
  let rec loop = function
      [] -> []
    | "when !=" :: e :: rest ->
	"when != " :: e :: "\n" :: (loop rest)
    | "when ==" :: e :: rest ->
	"when == " :: e :: "\n" :: (loop rest)
    | "when" :: " " :: e :: rest ->
	"when" :: " " :: e :: "\n" :: (loop rest)
    | "..." :: "when" :: rest -> "\n" :: "..." :: (loop ("when" :: rest))
    | "..." :: rest -> "\n" :: "..." :: "\n" :: (loop rest)
    | x::xs -> x::(loop xs) in
  loop tokens

let add_stars tokens =
  let rec loop = function
      [] -> []
    | "." :: "." :: "." :: rest -> "..." :: skip rest
    | "<" :: "." :: "." :: "." :: rest -> "<..." :: skip rest
    | "<" :: "+" :: "." :: "." :: "." :: rest -> "<+..." :: skip rest
    | "\n" :: rest -> "\n" :: loop rest
    | x :: xs -> ("* " ^ x) :: (skip xs)
  and skip = function
      [] -> []
    | "\n" :: rest -> "\n" :: loop rest
    | x :: xs -> x :: skip xs in
  loop tokens

let rec add_spaces = function
    [] -> []
  | x :: "\n" :: rest -> x :: "\n" :: (add_spaces rest)
  | "\n" :: rest -> "\n" :: (add_spaces rest)
  | x :: rest -> x :: " " :: (add_spaces rest)

let reparse tokens =
  let (env,code) = find_metavariables tokens in
  let env = String.concat "" (List.map snd env) in
  let code = find_when_dots code in
  let code = add_stars code in
  let code = String.concat "" code in
  let res = "@@\n"^env^"@@\n"^code in
  Printf.printf "%s\n\n" res;
  let out = Common.new_temp_file "sp" ".cocci" in
  let o = open_out out in
  Printf.fprintf o "%s\n" res;
  close_out o;
  out

let tokenize first =
  let lexbuf = Lexing.from_string first in
  let rec loop b =
    let tok = Lexer_cli.token b in
    if not (tok = Lexer_cli.EOF) then
      let s = Lexer_cli.pretty_print tok in
      s :: loop b
    else
      []
  in loop lexbuf

(* ---------------------------------------------------------------------- *)
(* entry point *)

let command_line args =
  let info =
    try Some (Common.split_when (function x -> List.mem x ["-sp";"--sp"]) args)
    with Not_found -> None in
  match info with
    None -> args
  | Some(pre_args,sp,post_args) ->
      (match post_args with
	first::post_args ->
	  pre_args @ "--sp-file" ::
		     (reparse (tokenize first)) ::
		     post_args
      | [] -> failwith "--sp needs an argument")
