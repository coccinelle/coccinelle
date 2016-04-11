(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

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
    module G : Ograph_extended.S
    type cfg = string G.ograph_mutable
    val predecessors : cfg -> node -> node list
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

	val sat :
	    G.cfg * (P.t -> (P.t,'anno) triples) * (P.t -> bool) *
	    G.node list ->
              (P.t, SUB.mvar, 'c) Ast_ctl.generic_ctl ->
		(P.t list list (* optional and required things *)) ->
		  (P.t,'anno) triples

	val print_bench : unit -> unit
      end

val get_graph_files : unit -> string list
val get_graph_comp_files : string -> string list
