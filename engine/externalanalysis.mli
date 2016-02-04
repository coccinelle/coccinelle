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


# 0 "./externalanalysis.mli"
module Int64Set : Set.S with type elt = int64

type bound = int64 option

type result =
    IntSet of Int64Set.t
  | IntBounds of bound * bound
  | Other of string

val show_bound : bound -> string
val show_result : result -> string

val load_external_results : string -> unit
val find_results : Common.filename -> Ast_c.posl -> Ast_c.posl -> result list

val intersect_results : result -> result -> result option

val satisfy :
  (result list -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val satisfy1 :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool

val has_any_result : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val for_all :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val for_all1 :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val exists :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool

val single_int : int64 -> result -> bool
val contains_int : int64 -> result -> bool
val has_only_nul : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val has_also_nul : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val has_also_int : int64 -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
