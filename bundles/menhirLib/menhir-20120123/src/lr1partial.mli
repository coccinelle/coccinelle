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

open Grammar

module Run (X : sig

  (* A restricted set of tokens of interest. *)

  val tokens: TerminalSet.t

  (* A state of the (merged) LR(1) automaton that we're trying to
     simulate. *)

  val goal: Lr1.node

end) : sig

  (* What we are after is a path, in the canonical LR(1) automaton,
     that leads from some entry node to a node [N] such that (i)
     [N] has a conflict involving one of the tokens of interest
     and (ii) [N] corresponds to the goal node, that is, the path
     that leads to [N] in the canonical LR(1) automaton leads to
     the goal node in the merged LR(1) automaton. *)

  val source: Item.t

  val path: Symbol.t array

  val goal: Lr0.concretelr1state

  (* An (arbitrarily chosen) conflict token in the goal state. *)

  val token: Terminal.t

end

