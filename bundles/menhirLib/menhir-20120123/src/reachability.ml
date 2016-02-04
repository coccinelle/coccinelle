(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  Fran�ois Pottier, INRIA Rocquencourt                                  *)
(*  Yann R�gis-Gianas, PPS, Universit� Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

open UnparameterizedSyntax
open Syntax

let rec visit grammar visited symbol =
  try
    let rule = StringMap.find symbol grammar.rules in
    if not (StringSet.mem symbol visited) then
      let visited = StringSet.add symbol visited in
      List.fold_left (visitb grammar) visited rule.branches
    else
      visited
  with Not_found ->
    (* This is a terminal symbol. *)
    assert (symbol = "error" || StringMap.mem symbol grammar.tokens);
    visited

and visitb grammar visited { producers = symbols } =
  List.fold_left (visits grammar) visited symbols

and visits grammar visited (symbol, _) =
  visit grammar visited symbol

let trim grammar =
  if StringSet.cardinal grammar.start_symbols = 0 then
    Error.error [] "no start symbol has been declared."
  else
    let reachable =
      StringSet.fold (fun symbol visited ->
	visit grammar visited symbol
      ) grammar.start_symbols StringSet.empty 
    in
    StringMap.iter (fun symbol rule ->
      if not (StringSet.mem symbol reachable) then
	Error.grammar_warning
	  rule.positions
	  (Printf.sprintf
	     "symbol %s is unreachable from any of the start symbol(s)."
	       symbol)
    ) grammar.rules;
    { grammar with rules = StringMap.restrict reachable grammar.rules }

