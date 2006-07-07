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
    val predecessors : cfg -> node -> node list 
    val print_node: node -> unit
  end

module OGRAPHEXT_GRAPH :
  sig
    type node = int
    type cfg = (string, unit) Ograph_extended.ograph_extended
    val predecessors :
      < predecessors : 'a -> < tolist : ('b * 'c) list; .. >; .. > ->
      'a -> 'b list
    val print_node : node -> unit
  end

exception TODO_CTL
exception NEVER_CTL

module CTL_ENGINE :
  functor (SUB : SUBST) ->
    functor (G : GRAPH) ->
      sig

	type substitution = (SUB.mvar, SUB.value) Ast_ctl.generic_subst list

	type ('pred,'anno) witnesses =
	    (G.node, substitution,
	     ('pred, SUB.mvar, 'anno) Ast_ctl.generic_ctl list)
	      Ast_ctl.generic_witness list

	type ('pred,'anno) triples =
	    (G.node * substitution * ('pred,'anno) witnesses) list

        val sat :
          G.cfg * ('a -> ('pred,'anno) triples) * G.node list ->
            ('a, SUB.mvar, 'c) Ast_ctl.generic_ctl ->
	    (('a, SUB.mvar, 'c) Ast_ctl.generic_ctl ->
	      ('pred,'anno) triples ->
	      ('pred,'anno) triples ->
	      ('pred,'anno) triples -> unit) -> (* check_conjunction *)
	    ('pred,'anno) triples
      end
