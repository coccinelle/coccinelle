(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* A simple implementation of the DPLL algorithm:
http://en.wikipedia.org/wiki/DPLL_algorithm

A formula is in cnf.

This is not currently used, but please don't get rid of it. *)

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
