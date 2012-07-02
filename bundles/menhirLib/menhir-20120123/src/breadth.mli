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

(* This module implements generic breadth-first search
   over a graph with labeled edges. *)

module Make (G : sig

  (* This is the type of graph vertices. *)

  type vertex

  (* This is the type of graph labels. *)

  type label

  (* These allow marking a vertex and checking whether
     it is marked. *)

  val set_mark: vertex -> Mark.t -> unit
  val get_mark: vertex -> Mark.t

  (* This is an iterator over the graph's entry vertices. *)

  val entry: (vertex -> unit) -> unit

  (* This provides access to a vertex' successors. *)

  val successors: (label -> vertex -> unit) -> vertex -> unit

end) : sig

  (* [search f] invokes [f discovery v label v'] once for every edge
     from vertex [v] to vertex [v'] carrying label [label]. Vertices
     [v'] are presented breadth-first. The flag [discovery] tells
     whether the edge is a discovery edge, that is, whether it belongs
     to the spanning forest of shortest paths that is being built. *)

  val search: (bool -> G.vertex -> G.label -> G.vertex -> unit) -> unit

end

