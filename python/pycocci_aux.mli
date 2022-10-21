(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val exprlistrep : Ast_c.argument Ast_c.wrap2 list -> string * string list
val paramlistrep : Ast_c.parameterType Ast_c.wrap2 list -> string * string list
val initlistrep : Ast_c.newlines * Ast_c.initialiser Ast_c.wrap2 list -> string * string list
val fieldlistrep : Ast_c.field list -> string * string list
val stringrep : Ast_c.metavar_binding_kind -> string
