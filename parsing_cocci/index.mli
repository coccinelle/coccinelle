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


# 0 "./index.mli"
val expression_dots : Ast0_cocci.expression Ast0_cocci.dots -> int list
val initialiser_dots : Ast0_cocci.initialiser Ast0_cocci.dots -> int list
val parameter_dots : Ast0_cocci.parameterTypeDef Ast0_cocci.dots -> int list
val statement_dots : Ast0_cocci.statement Ast0_cocci.dots -> int list
val declaration_dots : Ast0_cocci.declaration Ast0_cocci.dots -> int list
val case_line_dots : Ast0_cocci.case_line Ast0_cocci.dots -> int list
val ident : Ast0_cocci.ident -> int list
val expression : Ast0_cocci.expression -> int list
val typeC : Ast0_cocci.typeC -> int list
val declaration : Ast0_cocci.declaration -> int list
val initialiser : Ast0_cocci.initialiser -> int list
val parameterTypeDef : Ast0_cocci.parameterTypeDef -> int list
val statement : Ast0_cocci.statement -> int list
val forinfo : Ast0_cocci.forinfo -> int list
val pragmainfo : Ast0_cocci.pragmainfo -> int list
val case_line : Ast0_cocci.case_line -> int list
val top_level : Ast0_cocci.top_level -> int list
