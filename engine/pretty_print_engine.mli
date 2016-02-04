(*
 * Copyright 2012-2015, Inria
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


# 0 "./pretty_print_engine.mli"
(* could be in pretty_print_c because dependent of ast_c but metavars
 * are in ast_c for "bad" reason, so better put the pretty_print
 * of metavars here
 *)

val pp_binding_kind : Ast_c.metavar_binding_kind -> unit
val pp_binding : Ast_c.metavars_binding -> unit

val pp_binding_kind2 : Lib_engine.metavar_binding_kind2 -> unit
val pp_binding2_ctlsubst :
    (Lib_engine.mvar, Lib_engine.metavar_binding_kind2)
    Ast_ctl.generic_substitution ->
    unit
val pp_predicate : Lib_engine.predicate -> unit
val predicate_to_string : Lib_engine.predicate -> string


val pp_ctlcocci :
  bool (* show_plus *) -> bool (* inline_let *) -> Lib_engine.ctlcocci -> unit

