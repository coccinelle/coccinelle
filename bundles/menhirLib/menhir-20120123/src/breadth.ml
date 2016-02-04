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

end) = struct

  let search f =

    let queue : G.vertex Queue.t =
      Queue.create ()

    and mark =
      Mark.fresh()

    in

    let visited vertex =
      Mark.same mark (G.get_mark vertex)

    and visit vertex =
      G.set_mark vertex mark;
      Queue.add vertex queue

    in

    G.entry visit;
    Misc.qiter (fun vertex ->
      G.successors (fun label son ->
	if not (visited son) then begin
	  visit son;
	  f true vertex label son
	end
	else
	  f false vertex label son
      ) vertex
    ) queue

end

