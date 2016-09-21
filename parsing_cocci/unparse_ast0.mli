(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

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
val unparse_x_to_string : ('a -> unit) -> 'a -> string

val show_cocci_parse_tree : string -> Ast0_cocci.top_level -> unit
