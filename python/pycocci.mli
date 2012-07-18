(*
 * Copyright 2012, INRIA
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


# 0 "./pycocci.mli"
val build_classes : Ast_cocci.meta_name list -> unit
val construct_variables :
    (string * Ast_cocci.meta_name * Ast_cocci.metavar) list
  -> Ast_c.metavars_binding (*virts*) -> unit
val construct_script_variables : Ast_cocci.meta_name list -> unit
val pyrun_simplestring : string -> int
val inc_match : bool ref
val exited : bool ref
val retrieve_script_variables : Ast_cocci.meta_name list -> string list
exception Pycocciexception 
val set_coccifile : string -> unit
val python_support : bool
val initialised : bool ref
val py_isinitialized : unit -> int
val py_finalize : unit -> unit 
