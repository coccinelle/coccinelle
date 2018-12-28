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

module type MEMOIZER = sig
  (* A type of keys. *)
  type key
  (* A memoization combinator for this type. *)
  val memoize: (key -> 'a) -> (key -> 'a)
end

module type IMPERATIVE_MAP = sig
  (* A type of keys. *)
  type key
  (* A type of imperative maps. *)
  type 'a t
  (* Creation, insertion, lookup. *)
  val create: int -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
end

module Make (M : IMPERATIVE_MAP) : MEMOIZER with type key = M.key

module MakeViaMap (O : Map.OrderedType) : MEMOIZER with type key = O.t

module MakeViaHashtbl (H : Hashtbl.HashedType) : MEMOIZER with type key = H.t

module Int : MEMOIZER with type key = int
