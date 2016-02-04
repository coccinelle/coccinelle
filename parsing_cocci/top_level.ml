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


# 0 "./top_level.ml"
(* Reorganize the top level of a rule to be a list of either top-level
declarations or code dots.  A function declaration is always considered top
level.  A statement is always considered code dots.  A variable declaration
is ambiguous.  We use the heuristic that if there are code dots somewhere
else, then the variable declaration is at top level, otherwise it applies
both at top level and to all code. *)

(* This is assumed to be done before compute_lines, and thus the info on a
complex term is assumed to be Ast0.default_info *)

module Ast0 = Ast0_cocci

let top_dots l =
  let circle x =
    match Ast0.unwrap x with Ast0.Circles(_) -> true | _ -> false in
  let star x =
    match Ast0.unwrap x with Ast0.Stars(_) -> true | _ -> false in
  if List.exists circle l
  then Ast0.wrap (Ast0.CIRCLES(l))
  else if List.exists star l
  then Ast0.wrap (Ast0.STARS(l))
  else Ast0.wrap (Ast0.DOTS(l))

let rec is_decl s =
  match Ast0.unwrap s with
    Ast0.Decl(_,e) -> true
  | _ -> false

let isonly f l = match Ast0.undots l with [s] -> f s | _ -> false
let isall f l = List.for_all (isonly f) l

let rec is_toplevel s =
  match Ast0.unwrap s with
    Ast0.Decl(_,e) -> true
  | Ast0.FunDecl(_,_,_,_,_,_,_,_,_,_,_) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_toplevel stmts
  | Ast0.ExprStatement(Some fc,_) -> false
  | Ast0.Include(_,_) -> true
  | Ast0.Undef(_,_) -> true
  | Ast0.Define(_,_,_,_) -> true
  | Ast0.Pragma(_,_,_) -> true
  | _ -> false

let scan_code must_be_code l =
  let rec loop = function
      [] -> ([],[])
    | (x::xs) as all ->
        (match Ast0.unwrap x with
          (Ast0.OTHER(code)) ->
            let (front,rest) = loop xs in
            (code::front,rest)
        | _ -> ([],all)) in
  match loop l with
    ([],_) as res -> res
  | (code,rest) ->
      (match code with
      | [x] when is_decl x && must_be_code ->
	  ([Ast0.wrap(Ast0.NONDECL x)],rest)
      | _ when List.for_all is_toplevel code ->
	  ([Ast0.wrap(Ast0.TOPCODE(top_dots code))],rest)
      | _ ->
	  ([Ast0.wrap(Ast0.CODE(top_dots code))],rest))

let rec scan_top_decl = function
    [] -> ([],[])
  | ((topdecl::rest) as all) ->
      (match Ast0.unwrap topdecl with
	Ast0.OTHER(_) -> ([],all)
      | _ ->
	  let (front,rest) = scan_top_decl rest
	  in (topdecl::front,rest))

(* for debugging *)
let l2c l =
  match Ast0.unwrap l with
    Ast0.NONDECL(_) -> "decl"
  | Ast0.CODE(_) -> "code"
  | Ast0.TOPCODE(_) -> "code"
  | Ast0.FILEINFO(_,_) -> "fileinfo"
  | Ast0.ERRORWORDS(_) -> "errorwords"
  | Ast0.OTHER(_) -> "other"

let rec top_level must_be_code l =
  match scan_code must_be_code l with
    (code,[]) -> code
  | (code,rest) ->
      (match scan_top_decl rest with
	(top_decls,[]) -> code@top_decls
      |	(top_decls,rest) -> code @ top_decls @ (top_level must_be_code rest))

let clean l =
  List.map
    (function tl ->
      match Ast0.unwrap tl with
	Ast0.TOPCODE x -> Ast0.rewrap tl (Ast0.CODE x)
      |	_ -> tl)
    l
