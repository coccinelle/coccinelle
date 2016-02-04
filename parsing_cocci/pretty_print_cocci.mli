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


# 0 "./pretty_print_cocci.mli"
val unparse : Ast_cocci.rule -> unit
val unparse_to_string : Ast_cocci.rule -> string
val expression : Ast_cocci.expression -> unit
val ident : Ast_cocci.ident -> unit
val ident_to_string : Ast_cocci.ident -> string
val declaration : Ast_cocci.declaration -> unit
val statement : string -> Ast_cocci.statement -> unit
val statement_dots : Ast_cocci.statement Ast_cocci.dots -> unit
val rule_elem : string -> Ast_cocci.rule_elem -> unit
val rule_elem_to_string : Ast_cocci.rule_elem -> string

val print_mcodekind : Ast_cocci.mcodekind -> unit

val constant : Ast_cocci.constant -> unit
val assignOp : Ast_cocci.assignOp -> unit
val simpleAssignOp : Ast_cocci.simpleAssignOp -> unit
val opAssignOp : Ast_cocci.arithOp -> unit
val fixOp : Ast_cocci.fixOp -> unit
val unaryOp : Ast_cocci.unaryOp -> unit
val binaryOp : Ast_cocci.binaryOp -> unit
val arithOp : Ast_cocci.arithOp -> unit
val logicalOp : Ast_cocci.logicalOp -> unit
val const_vol : Ast_cocci.const_vol -> unit
val sign : Ast_cocci.sign -> unit
val structUnion : Ast_cocci.structUnion -> unit
val storage : Ast_cocci.storage -> unit
val baseType : Ast_cocci.baseType -> unit
val fullType : Ast_cocci.fullType -> unit
val typeC : Ast_cocci.typeC -> unit
val inc_file : Ast_cocci.inc_file -> unit

val print_around :
    ('a -> unit) -> 'a -> Ast_cocci.anything Ast_cocci.befaft -> unit
val print_anything : string -> Ast_cocci.anything list list -> unit
val pp_print_anything : Ast_cocci.anything -> unit

val print_plus_flag : bool ref
val print_minus_flag : bool ref

val print_rule_elem : Ast_cocci.rule_elem -> unit
val print_when_modif : Ast_cocci.when_modifier -> unit

val dep : bool -> Ast_cocci.dependency -> unit
