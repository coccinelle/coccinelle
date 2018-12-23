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
  (* A fixed type of keys. *)
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

module Make (M : IMPERATIVE_MAP) = struct
  type key = M.key
  let memoize (f : key -> 'a) =
    let table = M.create 127 in
    fun x ->
      try
	M.find table x
      with Not_found ->
	let y = f x in
	M.add table x y;
	y
end

module MakeViaMap (O : Map.OrderedType) =
  Make(struct
    module M = Map.Make(O)
    type key = O.t
    type 'a t = 'a M.t ref
    let create _ = ref M.empty
    let add table key data = table := M.add key data !table
    let find table key = M.find key !table
  end)

module MakeViaHashtbl (H : Hashtbl.HashedType) =
  Make(Hashtbl.Make(H))

module Int =
  MakeViaHashtbl(struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end)
