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


# 0 "./wrapper_ctl.mli"
type info = int

type ('pred, 'mvar) wrapped_ctl =
    ('pred * 'mvar Ast_ctl.modif,  'mvar, info) Ast_ctl.generic_ctl

type ('a, 'b) wrapped_binding =
    ClassicVal of 'a
  | PredVal of 'b Ast_ctl.modif

type ('pred,'state,'mvar,'value) labelfunc =
    'pred ->
      ('state * ('pred * ('mvar, 'value) Ast_ctl.generic_substitution))  list

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
	G.node list ->
	  ((WRAPPER_PRED.t, WRAPPER_ENV.mvar, int) Ast_ctl.generic_ctl *
	     (WRAPPER_PRED.t list list)) ->
               (WRAPPER_PRED.t, 'a) WRAPPER_ENGINE.triples

    val satbis :
	G.cfg *
	 (predicate,G.node,SUB.mvar,SUB.value) labelfunc *
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
