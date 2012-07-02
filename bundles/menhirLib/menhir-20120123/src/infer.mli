(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* [ntvar symbol] is the name of the type variable associated with a
   nonterminal symbol. *)

val ntvar: string -> string

(* [infer grammar] analyzes the grammar [grammar] and returns a new
   grammar, augmented with a [%type] declaration for every nonterminal
   symbol. The [ocamlc] compiler is used to infer types. *)

val infer: UnparameterizedSyntax.grammar -> UnparameterizedSyntax.grammar

(* [depend grammar] prints (on the standard output channel) the
   Objective Caml dependencies induced by the semantic actions.
   Then, it exits the program. *)

val depend: UnparameterizedSyntax.grammar -> 'a

