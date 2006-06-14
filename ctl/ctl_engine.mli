module type SUBST =
  sig
    type value
    type mvar
    val eq_mvar : mvar -> mvar -> bool
    val eq_val : value -> value -> bool
    val merge_val : value -> value -> value
  end

module type GRAPH =
  sig 
    type node 
    type cfg 
    val predecessors : cfg -> node -> node list 
end



module SIMPLE_ENV :
  sig
    type value = string
    type mvar = string
    val eq_mvar : 'a -> 'a -> bool
    val eq_val : 'a -> 'a -> bool
    val merge_val : 'a -> 'b -> 'a
  end
module EXAMPLE_ENV :
  sig
    type value = PredVal of string | CodeVal of string | OtherVal of string
    type mvar = Modif of string | UnModif of string | Control
    val eq_mvar : 'a -> 'a -> bool
    val eq_val : 'a -> 'a -> bool
    val merge_val : 'a -> 'b -> 'a
  end

module SIMPLE_CFG :
  sig
    type node = int
    type cfg = (string, unit) Ograph_extended.ograph_extended
    val predecessors :
      < predecessors : 'a -> < tolist : ('b * 'c) list; .. >; .. > ->
      'a -> 'b list
  end

exception TODO_CTL
exception NEVER_CTL


module CTL_ENGINE :
  functor (SUB : SUBST) ->
    functor (G : GRAPH) ->
      sig
        type ('a, 'b) generic_ctl =
            False
          | True
          | Pred of 'a * 'b modif
          | Not of ('a, 'b) generic_ctl
          | Exists of 'b * ('a, 'b) generic_ctl
          | And of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
          | Or of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
          | Implies of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
          | AF of ('a, 'b) generic_ctl
          | AX of ('a, 'b) generic_ctl
          | AG of ('a, 'b) generic_ctl
          | AU of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
          | EF of ('a, 'b) generic_ctl
          | EX of ('a, 'b) generic_ctl
          | EG of ('a, 'b) generic_ctl
          | EU of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
        and 'mvar modif = Modif of 'mvar | UnModif of 'mvar | Control

        type ('a, 'b) generic_subst = 
            Subst of 'a * 'b 
          | NegSubst of 'a * 'b

        type ('a, 'b) generic_substitution = ('a, 'b) generic_subst list

        type ('a, 'b, 'c) generic_witness =
            Wit of 'a * 'b * 'c * ('a, 'b, 'c) generic_witness list
          | NegWit of 'a * 'b * 'c * ('a, 'b, 'c) generic_witness list


        val sat :
          G.cfg *
          ('a ->
           (G.node * (SUB.mvar, SUB.value) generic_subst list *
            (G.node, (SUB.mvar, SUB.value) generic_subst list, 'b list)
            generic_witness list)
           list) *
          G.node list ->
          ('a, SUB.mvar) generic_ctl ->
          (G.node * (SUB.mvar, SUB.value) generic_subst list *
           (G.node, (SUB.mvar, SUB.value) generic_subst list, 'b list)
           generic_witness list)
          list
      end
