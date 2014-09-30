(*
 * Copyright 2012-2014, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./ctl_engine.mli"
open Ast_ctl

module type SUBST =
  sig
    type value
    type mvar
    val eq_mvar : mvar -> mvar -> bool
    val eq_val : value -> value -> bool
    val merge_val : value -> value -> value
    val print_mvar : mvar -> unit
    val print_value : value -> unit
  end

module type GRAPH =
  sig
    type node
    type cfg
    val predecessors:     cfg -> node -> node list
    val successors:       cfg -> node -> node list
    val extract_is_loop : cfg -> node -> bool
    val print_node :      node -> unit
    val size :            cfg -> int
    val print_graph :     cfg -> string option ->
      (node * string) list -> (node * string) list -> string -> unit
  end

module OGRAPHEXT_GRAPH :
  sig
    type node = int
    type cfg = (string, unit) Ograph_extended.ograph_mutable
    val predecessors :
      < predecessors : 'a -> < tolist : ('b * 'c) list; .. >; .. > ->
      'a -> 'b list
    val print_node : node -> unit
  end

module type PREDICATE =
sig
  type t
  val print_predicate : t -> unit
end

module CTL_ENGINE :
  functor (SUB : SUBST) ->
    functor (G : GRAPH) ->
      functor (P : PREDICATE) ->
      sig

	type substitution = (SUB.mvar, SUB.value) Ast_ctl.generic_subst list

	type ('pred,'anno) witness =
	    (G.node, substitution,
	     ('pred, SUB.mvar, 'anno) Ast_ctl.generic_ctl list)
	      Ast_ctl.generic_witnesstree

	type ('pred,'anno) triples =
	    (G.node * substitution * ('pred,'anno) witness list) list

  val sat : G.cfg * (P.t -> (P.t,'anno) triples) * G.node list ->
            (P.t, SUB.mvar, 'c) Ast_ctl.generic_ctl ->
            (P.t list list (* optional and required things *)) ->
            (P.t,'anno) triples

	val print_bench : unit -> unit
      end

val get_graph_files : unit -> string list
val get_graph_comp_files : string -> string list

