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

(* TEMPORARY tidy up, comment, remove dead code *)

(* Converting an option to a string, with [None] converted
   to the empty string. *)

val o2s: 'a option -> ('a -> string) -> string

(* Projection out of a singleton list. *)

val single: 'a list -> 'a

(* A variant of [List.map] where [f] returns a pair of elements,
   to be flattened into the new list. *)

val mapd: ('a -> 'b * 'b) -> 'a list -> 'b list

(* Tabulating a function using an internal array. [tabulateb n f]
   returns a function that is extensionally equal to [f], but relies
   on an internal array. Arguments to [f] are of type [int] and are
   supposed to lie in the range [0..n). The result type of [f] is
   assumed to be of type [bool]. [tabulateb] also returns the number
   of points where [f] is [true]. *)

val tabulateb: int -> (int -> bool) -> (int -> bool) * int

(* [tabulateo number fold n f] returns a function that is
   extensionally equal to [f], but relies on an internal
   array. Arguments to [f] are of type ['a] and are mapped by [number]
   into the range [0..n). [fold] allows folding over the domain of
   [f]. The result type of [f] is an option type, and [tabulateo] also
   returns the number of points where [f] is [Some _]. *)

val tabulateo: ('a -> int) -> ((unit -> 'a -> unit) -> unit -> unit) -> int -> ('a -> 'b option) -> ('a -> 'b option) * int

(* Truncature of a list. *)

val truncate: int -> 'a list -> 'a list

(* Reverse function application. *)

val ( $$ ) : 'a -> ('a -> 'b) -> 'b

(* Sets of strings and maps over strings. *)

module IntSet    : Set.S with type elt = int

(* [separated_list_to_string printer sep l] converts [l] into a string
   representation built using [printer] on each element and [sep] as 
   a separator. *)

val separated_list_to_string: ('a -> string) -> string -> 'a list -> string

(* [index_map f] returns a triple (indexed_f, domain_indexation, domain_array).
   [indexed_f] is a mapping from [0..n-1] to the elements of the map [f] 
   ([n] being the size of the image of [f]). 
   [domain_indexation] is a mapping from the domain of the map [f] to indexes. 
   [domain_array] is a mapping from the indexes to the domain of [f]. 
   The indexation implements [f] ie:
   - forall x in domain(m), indexed_f (domain_indexation x) = f (x).
   - forall x in domain(m), domain_array (domain_indexation x) = x. *)

val index_map 
  : 'a StringMap.t -> (int -> 'a) * (string -> int) * (int -> string)

(* [support_assoc l x] returns the second component of the first couple
   in [l] whose first component is [x]. If it does not exist, it returns
   [x]. *)

val support_assoc : ('a * 'a) list -> 'a -> 'a

(* [index] indexes a list of (distinct) strings, that is, assigns an
   integer index to each string and builds mappings both ways between
   strings and indices. *)

val index: string list -> int * string array * int StringMap.t

(* Turning an implicit list, stored using pointers through a hash
   table, into an explicit list. The head of the implicit list is
   not included in the explicit list. *)

val materialize: ('a, 'a option) Hashtbl.t -> 'a -> 'a list

(* [iteri] implements a [for] loop over integers, from 0 to
   [n-1]. *)

val iteri: int -> (int -> unit) -> unit

(* [foldi] implements a [for] loop over integers, from 0 to [n-1],
   with an accumulator. [foldij] implements a [for] loop over
   integers, from [start] to [n-1], with an accumulator. *)

val foldi: int -> (int -> 'a -> 'a) -> 'a -> 'a
val foldij: int -> int -> (int -> 'a -> 'a) -> 'a -> 'a

(* [mapi n f] produces the list [ f 0; ... f (n-1) ]. *)

val mapi: int -> (int -> 'a) -> 'a list

(* [qfold f accu q] repeatedly takes an element [x] off the queue [q]
   and applies [f] to the accumulator and to [x], until [q] becomes
   empty. Of course, [f] can add elements to [q] as a side-effect. *)

val qfold: ('a -> 'b -> 'a) -> 'a -> 'b Queue.t -> 'a

(* [qiter f q] repeatedly takes an element [x] off the queue [q] and
   applies [f] to [x], until [q] becomes empty. Of course, [f] can add
   elements to [q] as a side-effect. *)

val qiter: ('b -> unit) -> 'b Queue.t -> unit

(* [smap] has the same semantics as [List.map], but attempts to
   physically return the input list when [f] is the identity. *)

val smap: ('a -> 'a) -> 'a list -> 'a list

(* [smapa] is a variant of [smap] that maintains an accumulator. *)

val smapa: ('b -> 'a -> 'b * 'a) -> 'b -> 'a list -> 'b * 'a list

(* [normalize s] returns a copy of [s] where parentheses and commas
   are replaced with underscores. *)

val normalize: string -> string

(* [postincrement r] increments [r] and returns its original value. *)

val postincrement: int ref -> int

(* [gcp] returns the greatest common prefix of two strings. *)

val gcp: string -> string -> string

(* [gcps] returns the greatest common prefix of a nonempty list of strings. *)

val gcps : string list -> string

(* [array_forall p a] computes the conjunction of the predicate [p] over all
   elements of the array [a]. *)

val array_forall: ('a -> bool) -> 'a array -> bool

