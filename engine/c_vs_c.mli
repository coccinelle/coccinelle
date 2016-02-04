(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val eq_type : Ast_c.fullType -> Ast_c.fullType -> bool
val merge_type : Ast_c.fullType -> Ast_c.fullType -> Ast_c.fullType

val subexpression_of_expression : Ast_c.expression -> Ast_c.expression -> bool
