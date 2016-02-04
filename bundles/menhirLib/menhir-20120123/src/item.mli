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

open Grammar

(* An LR(0) item encodes a pair of integers, namely the index of the
   production and the index of the bullet in the production's
   right-hand side. *)

type t
val import: Production.index * int -> t
val export: t -> Production.index * int

(* Comparison. *)

val equal: t -> t -> bool

(* [def item] looks up the production associated with this item in the
   grammar and returns [prod, nt, rhs, pos, length], where [prod] is
   the production's index, [nt] and [rhs] represent the production,
   [pos] is the position of the bullet in the item, and [length] is
   the length of the production's right-hand side. *)

val def: t -> Production.index * Nonterminal.t * Symbol.t array * int * int

(* If [item] is a start item, [startnt item] returns the start
   nonterminal that corresponds to [item]. *)

val startnt: t -> Nonterminal.t

(* Printing. *)

val print: t -> string

(* Classifying items as shift or reduce items. A shift item is one
   where the bullet can still advance. A reduce item is one where the
   bullet has reached the end of the right-hand side. *)

type kind =
  | Shift of Symbol.t * t
  | Reduce of Production.index

val classify: t -> kind

(* Sets of items and maps over items. Hashing these data structures is
   specifically allowed. *)

module Set : GSet.S with type element = t
module Map : GMap.S with type key = t
                     and type Domain.t = Set.t

(* This functor performs precomputation that helps efficiently compute
   the closure of an LR(0) or LR(1) state. The precomputation requires
   time linear in the size of the grammar. The nature of the lookahead
   sets remains abstract. *)

module Closure (L : Lookahead.S) : sig

  (* A state maps items to lookahead information. *)

  type state = L.t Map.t

  (* This takes the closure of a state through all epsilon transitions. *)

  val closure: state -> state

end

