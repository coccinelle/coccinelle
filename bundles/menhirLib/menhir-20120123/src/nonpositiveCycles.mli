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

(* This module uses Floyd and Warshall's algorithm to detect whether a graph
   with integer-weighted edges contains a simple cycle of negative weight. *)

(* The algorithm runs in cubic time in the number of vertices. It may be
   worthwhile to first use Tarjan's algorithm to obtain the graph's strongly
   connected components, and use Floyd and Warshall's algorithm only on each
   component. *)

module Run (G : sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Iterating over a node's immediate successors. Edges are weighted. *)

  val successors: (int -> node -> unit) -> node -> unit

  (* Iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end) : sig

  val graph_has_nonpositive_simple_cycle : bool

end

