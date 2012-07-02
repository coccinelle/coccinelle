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

(* This module drives the first half of the front-end. It opens and
   parses the input files, which yields a number of partial
   grammars. It joins these grammars, expands them to get rid of
   parameterized nonterminals, and performs reachability
   analysis. This yields a single unified grammar.

   More transformations over this grammar are performed in the second
   half of the front-end, which is driven by [Front]. The modules
   [PreFront] and [Front] are separated because it is convenient to
   insert auxiliary modules, such as [TokenType] and [Infer], in
   between the two.  *)

val grammar: UnparameterizedSyntax.grammar

