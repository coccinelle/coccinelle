(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type info = int

type ('pred, 'mvar) wrapped_ctl =
    ('pred * 'mvar Ast_ctl.modif,  'mvar, info) Ast_ctl.generic_ctl

type ('a, 'b) wrapped_binding =
    ClassicVal of 'a
  | PredVal of 'b Ast_ctl.modif

type ('pred,'state,'mvar,'value) labelfunc =
    'pred ->
      ('state * ('pred * ('mvar, 'value) Ast_ctl.generic_substitution))  list

type 'pred preprocfunc =
    'pred -> bool

module CTL_ENGINE_BIS :
  functor (SUB : Ctl_engine.SUBST) ->
    functor (G : Ctl_engine.GRAPH) ->
      functor(P : Ctl_engine.PREDICATE) ->
  sig

    type predicate = P.t
    module WRAPPER_ENV :
        sig
          type mvar = SUB.mvar
          type value = (SUB.value, predicate) wrapped_binding
        end
    module WRAPPER_PRED :
        sig
          type t = P.t * SUB.mvar Ast_ctl.modif
        end
    module WRAPPER_ENGINE :
        sig
          type substitution =
              (WRAPPER_ENV.mvar, WRAPPER_ENV.value) Ast_ctl.generic_subst list
          type ('a, 'b) witness =
              (G.node, substitution,
               ('a, WRAPPER_ENV.mvar, 'b) Ast_ctl.generic_ctl list)
                Ast_ctl.generic_witnesstree
          type ('a, 'b) triples =
              (G.node * substitution * ('a, 'b) witness list) list
	end


    val satbis_noclean :
	G.cfg *
	(predicate, G.node, WRAPPER_ENV.mvar, SUB.value) labelfunc *
	predicate preprocfunc *
	G.node list ->
	  ((WRAPPER_PRED.t, WRAPPER_ENV.mvar, int) Ast_ctl.generic_ctl *
	     (WRAPPER_PRED.t list list)) ->
               (WRAPPER_PRED.t, 'a) WRAPPER_ENGINE.triples

    val satbis :
	G.cfg *
	 (predicate,G.node,SUB.mvar,SUB.value) labelfunc *
	 predicate preprocfunc *
         G.node list ->
	   ((predicate,SUB.mvar) wrapped_ctl *
	      (WRAPPER_PRED.t list list)) ->
		(WRAPPER_ENV.mvar list * (SUB.mvar * SUB.value) list) ->
		  ((WRAPPER_PRED.t, 'a) WRAPPER_ENGINE.triples *
		     ((G.node * (SUB.mvar * SUB.value) list * predicate)
			list list *
			bool *
			(WRAPPER_ENV.mvar * SUB.value) list list))

    val print_bench : unit -> unit
end
