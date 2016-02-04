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

(* $Id: unionFind.ml,v 1.5 2005/12/01 16:20:07 regisgia Exp $ *)

(** This module implements a simple and efficient union/find algorithm.
    See Robert E. Tarjan, ``Efficiency of a Good But Not Linear Set
    Union Algorithm'', JACM 22(2), 1975. *)

(** The abstraction defined by this module is a set of points,
    partitioned into equivalence classes. With each equivalence class,
    a piece of information, of abstract type ['a], is associated; we
    call it a descriptor.

    A point is implemented as a cell, whose (mutable) contents consist
    of a single link to either information about the equivalence class,
    or another point. Thus, points form a graph, which must be acyclic,
    and whose connected components are the equivalence classes. In
    every equivalence class, exactly one point has no outgoing edge,
    and carries information about the class instead. It is the class's
    representative element.

    Information about a class consists of an integer weight (the number
    of elements in the class) and of the class's descriptor. *)
type 'a point = {
    mutable link: 'a link
  } 

and 'a link =
  | Info of 'a info
  | Link of 'a point

and 'a info = {
    mutable weight: int;
    mutable descriptor: 'a
  } 

(** [fresh desc] creates a fresh point and returns it. It forms an
    equivalence class of its own, whose descriptor is [desc]. *)
let fresh desc = {
  link = Info { weight = 1; descriptor = desc }
} 

(** [repr point] returns the representative element of [point]'s
    equivalence class. It is found by starting at [point] and following
    the links. For efficiency, the function performs path compression
    at the same time. *)
let rec repr point =
  match point.link with
  | Link point' ->
      let point'' = repr point' in
      if point'' != point' then

	(* [point''] is [point']'s representative element. Because we
	   just invoked [repr point'], [point'.link] must be [Link
	   point'']. We write this value into [point.link], thus
	   performing path compression. Note that this function never
	   performs memory allocation. *)

	point.link <- point'.link;
      point''
  | Info _ ->
      point

(** [find point] returns the descriptor associated with [point]'s
    equivalence class. *)
let rec find point =

  (* By not calling [repr] immediately, we optimize the common cases
     where the path starting at [point] has length 0 or 1, at the
     expense of the general case. *)

  match point.link with
  | Info info
  | Link { link = Info info } ->
      info.descriptor
  | Link { link = Link _ } ->
      find (repr point)

let rec change point v = 
  match point.link with
  | Info info
  | Link { link = Info info } ->
      info.descriptor <- v
  | Link { link = Link _ } ->
      change (repr point) v

(** [union point1 point2] merges the equivalence classes associated
    with [point1] and [point2] (which must be distinct) into a single
    class whose descriptor is that originally associated with [point2].

    The fact that [point1] and [point2] do not originally belong to the
    same class guarantees that we do not create a cycle in the graph.

    The weights are used to determine whether [point1] should be made
    to point to [point2], or vice-versa. By making the representative
    of the smaller class point to that of the larger class, we
    guarantee that paths remain of logarithmic length (not accounting
    for path compression, which makes them yet smaller). *)
let union point1 point2 =
  let point1 = repr point1
  and point2 = repr point2 in
  assert (point1 != point2);
  match point1.link, point2.link with
  | Info info1, Info info2 ->
      let weight1 = info1.weight
      and weight2 = info2.weight in
      if weight1 >= weight2 then begin
	point2.link <- Link point1;
	info1.weight <- weight1 + weight2;
	info1.descriptor <- info2.descriptor
      end
      else begin
	point1.link <- Link point2;
	info2.weight <- weight1 + weight2
      end
  | _, _ ->
      assert false (* [repr] guarantees that [link] matches [Info _]. *)

(** [equivalent point1 point2] tells whether [point1] and [point2]
    belong to the same equivalence class. *)
let equivalent point1 point2 =
  repr point1 == repr point2

(** [eunion point1 point2] is identical to [union], except it does
    nothing if [point1] and [point2] are already equivalent. *)
let eunion point1 point2 =
  if not (equivalent point1 point2) then
    union point1 point2

(** [redundant] maps all members of an equivalence class, but one, to
    [true]. *)
let redundant = function
  | { link = Link _ } ->
      true
  | { link = Info _ } ->
      false

