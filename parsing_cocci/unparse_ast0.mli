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


# 0 "./unparse_ast0.mli"
val expression_dots : Ast0_cocci.expression Ast0_cocci.dots -> unit
val parameter_list : Ast0_cocci.parameterTypeDef Ast0_cocci.dots -> unit
val statement_dots : Ast0_cocci.statement Ast0_cocci.dots -> unit
val ident : Ast0_cocci.ident -> unit
val expression : Ast0_cocci.expression -> unit
val typeC : Ast0_cocci.typeC -> unit
val parameterTypeDef : Ast0_cocci.parameterTypeDef -> unit
val declaration : Ast0_cocci.declaration -> unit
val statement : string -> Ast0_cocci.statement -> unit
val top_level : Ast0_cocci.top_level -> unit

val unparse : Ast0_cocci.rule -> unit
val unparse_anything : Ast0_cocci.anything -> unit

val show_cocci_parse_tree : string -> Ast0_cocci.top_level -> unit
