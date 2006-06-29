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

        val sat :
          G.cfg *
          ('a ->
           (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
            (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
             'b list)
            Ast_ctl.generic_witness list)
           list) *
          G.node list ->
          ('a, SUB.mvar, 'c) Ast_ctl.generic_ctl ->
          (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
           (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
            'b list)
           Ast_ctl.generic_witness list)
          list

        val sat_verbose :
          (int ->
           ('a, SUB.mvar, 'b) Ast_ctl.generic_ctl ->
           (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
            (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
             'c list)
            Ast_ctl.generic_witness list)
           list -> 'd list -> 'd) ->
          int ->
          int ->
          G.cfg *
          ('a ->
           (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
            (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
             'c list)
            Ast_ctl.generic_witness list)
           list) *
          G.node list ->
          ('a, SUB.mvar, 'b) Ast_ctl.generic_ctl ->
          'd *
          (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
           (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
            'c list)
           Ast_ctl.generic_witness list)
          list

        type 'a witAnnoTree = WitAnno of ('a * 'a witAnnoTree list)
        val sat_annotree :
          (int ->
           ('a, SUB.mvar, 'b) Ast_ctl.generic_ctl ->
           (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
            (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
             'c list)
            Ast_ctl.generic_witness list)
           list -> 'd) ->
          G.cfg *
          ('a ->
           (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
            (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
             'c list)
            Ast_ctl.generic_witness list)
           list) *
          G.node list ->
          ('a, SUB.mvar, 'b) Ast_ctl.generic_ctl ->
          'd witAnnoTree *
          (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_subst list *
           (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_subst list,
            'c list)
           Ast_ctl.generic_witness list)
          list
      end
