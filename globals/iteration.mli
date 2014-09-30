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


# 0 "./iteration.mli"
type init_info = (string (* language *) * string (* rule name *)) *
      (string list (* defined virtual rules *) *
	 (string * string) list (* defined virtual env *))

val initialization_stack : init_info list ref

(* ----------------------------------------------------------------------- *)

val base_file_list : string list ref
val parsed_virtual_rules : string list ref
val parsed_virtual_identifiers : string list ref

(* ----------------------------------------------------------------------- *)

type pending_info = string list (* files to treat *) *
      string list * (* defined virtual rules *)
      (string * string) list (* virtual identifiers *)

val add_pending_instance :
    (* input is like pending_info, but with an extra option on files and bool
    for environment extension *)
    (string list option * string list * (string * string) list * bool) ->
    unit

val get_pending_instance : unit -> pending_info option

(* ----------------------------------------------------------------------- *)

val check_virtual_rule : string -> unit
val check_virtual_ident : string -> unit
