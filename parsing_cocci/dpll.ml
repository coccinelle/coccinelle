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


# 0 "./dpll.ml"
(* A simple implementation of the DPLL algorithm:
http://en.wikipedia.org/wiki/DPLL_algorithm

A formula is in cnf. *)

type positive_literal = string
type negative_literal = string
type clause = positive_literal * negative_literal (*represents a disjunction*)
type formula = clause list (* represents a conjunction *)

(* The formula ((a v !b) & (c v !d v !e)) would be
[[([a],[b])];[([c],[d;e])]] *)

(* ----------------------------------------------------------------------- *)
(* evaluation *)

let rec evaluate var vl = function
    [] -> []
  | (pos,neg)::rest ->
      match vl with
	true ->
	  if List.mem var pos
	  then evaluate var vl rest
	  else (pos,Common.minus_set neg [var]) :: evaluate var vl rest
      |	false ->
	  if List.mem var neg
	  then evaluate var vl rest
	  else (Common.minus_set pos [var],neg) :: evaluate var vl rest

(* ----------------------------------------------------------------------- *)
(* Unit propagation *)

let unit_propagation formula =
  let rec loop changed prev = function
      [] -> (changed,prev)
    | ([x],[])::rest ->
	loop true (evaluate x true prev) (evaluate x true rest)
    | ([],[x])::rest ->
	loop true (evaluate x false prev) (evaluate x false rest)
    | cl::rest -> loop changed (cl::prev) rest in
  let rec oloop formula =
    let (changed,formula) = loop false [] formula in
    if changed
    then oloop formula
    else formula in
  oloop formula

(* ----------------------------------------------------------------------- *)
(* Pure literal elimination *)

let pure_literals formula =
  let rec loop pos neg = function
      [] -> (pos,neg)
    | (cpos,cneg)::rest ->
	let pos = Common.minus_set pos cneg in
	let neg = Common.minus_set neg cpos in
	let cpos = Common.minus_set cpos neg in
	let cneg = Common.minus_set cneg pos in
	let pos = Common.union_set pos cpos in
	let neg = Common.union_set neg cneg in
	loop pos neg rest in
  let (pos,neg) = loop [] [] formula in
  let formula =
    List.fold_left (fun formula pos -> evaluate pos true formula)
      formula pos in
  let formula =
    List.fold_left (fun formula neg -> evaluate neg false formula)
      formula neg in
  formula

(* ----------------------------------------------------------------------- *)
(* Main loop *)

let rec dpll formula =
  let formula = unit_propagation formula in
  let formula = pure_literals formula in
  match formula with
    [] -> true
  | ((pos,neg) :: _) as formula ->
      if List.mem ([],[]) formula
      then false
      else
	match pos@neg with
	  x::_ ->
	    (dpll (evaluate x true formula)) ||
	    (dpll (evaluate x false formula))
	| _ -> failwith "at least one of pos and neg must be nonempty"
