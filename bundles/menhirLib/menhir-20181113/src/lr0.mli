(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(* This module builds the LR(0) automaton associated with the grammar,
   then provides access to it. It also provides facilities for
   efficiently performing LR(1) constructions. *)

(* ------------------------------------------------------------------------ *)
(* The LR(0) automaton. *)

(* The nodes of the LR(0) automaton are numbered. *)

type node =
    int

(* This is the number of nodes in the LR(0) automaton. *)

val n: int

(* These are the automaton's entry states, indexed by the start productions. *)

val entry: node ProductionMap.t

(* A node can be converted to the underlying LR(0) set of items. This set is
   not closed. *)

val items: node -> Item.Set.t

(* The incoming symbol of an LR(0) node is the symbol carried by all of the
   edges that enter this node. A node has zero incoming edges (and, thus, no
   incoming symbol) if and only if it is a start node.. *)

val incoming_symbol: node -> Symbol.t option

(* ------------------------------------------------------------------------ *)
(* Help for building the LR(1) automaton. *)

(* An LR(1) state is internally represented as a pair of an LR(0)
   state number and an array of concrete lookahead sets (whose length
   depends on the LR(0) state). *)

type lr1state

(* An encoded LR(1) state can be turned into a concrete representation,
   that is, a mapping of items to concrete lookahead sets. *)

type concretelr1state =
    TerminalSet.t Item.Map.t

val export: lr1state -> concretelr1state

(* One can take the closure of a concrete LR(1) state. *)

val closure: concretelr1state -> concretelr1state

(* The core of an LR(1) state is the underlying LR(0) state. *)

val core: lr1state -> node

(* One can create an LR(1) start state out of an LR(0) start
   node. *)

val start: node -> lr1state

(* Information about the transitions and reductions at a state. *)

val transitions: lr1state -> lr1state SymbolMap.t
val outgoing_symbols: node -> Symbol.t list
val transition: Symbol.t -> lr1state -> lr1state

val reductions: lr1state -> (TerminalSet.t * Production.index) list

(* Equality of states. The two states must have the same core. Then,
   they are equal if and only if their lookahead sets are pointwise
   equal. *)

val equal: lr1state -> lr1state -> bool

(* Subsumption between states. The two states must have the same
   core. Then, one subsumes the other if and only if their lookahead
   sets are (pointwise) in the subset relation. *)

val subsume: lr1state -> lr1state -> bool

(* A slightly modified version of Pager's weak compatibility
   criterion. The two states must have the same core. *)

val compatible: lr1state -> lr1state -> bool

(* This function determines whether two (core-equivalent) states can
   be merged without creating an end-of-stream conflict. *)

val eos_compatible: lr1state -> lr1state -> bool

(* This function determines whether two (core-equivalent) states can
   be merged without creating spurious reductions on the [error]
   token. *)

val error_compatible: lr1state -> lr1state -> bool

(* Union of two states. The two states must have the same core. The
   new state is obtained by pointwise union of the lookahead sets. *)

val union: lr1state -> lr1state -> lr1state

(* Restriction of a state to a set of tokens of interest. Every
   lookahead set is intersected with that set. *)

val restrict: TerminalSet.t -> lr1state -> lr1state

(* The following functions display: 1- a concrete state; 2- a state
   (only the kernel, not the closure); 3- the closure of a state.
   The first parameter is a fixed string that is added at the
   beginning of every line. *)

val print_concrete: string -> concretelr1state -> string
val print:          string ->         lr1state -> string
val print_closure:  string ->         lr1state -> string

