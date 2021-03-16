(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val expression_dots : Ast0_cocci.expression Ast0_cocci.dots -> int list
val initialiser_dots : Ast0_cocci.initialiser Ast0_cocci.dots -> int list
val parameter_dots : Ast0_cocci.parameterTypeDef Ast0_cocci.dots -> int list
val statement_dots : Ast0_cocci.statement Ast0_cocci.dots -> int list
val declaration_dots : Ast0_cocci.declaration Ast0_cocci.dots -> int list
val field_dots : Ast0_cocci.field Ast0_cocci.dots -> int list
val enum_decl_dots : Ast0_cocci.enum_decl Ast0_cocci.dots -> int list
val case_line_dots : Ast0_cocci.case_line Ast0_cocci.dots -> int list
val define_param_dots : Ast0_cocci.define_param Ast0_cocci.dots -> int list
val ident : Ast0_cocci.ident -> int list
val expression : Ast0_cocci.expression -> int list
val assignOp : Ast0_cocci.assignOp -> int list
val binaryOp : Ast0_cocci.binaryOp -> int list
val typeC : Ast0_cocci.typeC -> int list
val declaration : Ast0_cocci.declaration -> int list
val field : Ast0_cocci.field -> int list
val enum_decl : Ast0_cocci.enum_decl -> int list
val initialiser : Ast0_cocci.initialiser -> int list
val parameterTypeDef : Ast0_cocci.parameterTypeDef -> int list
val statement : Ast0_cocci.statement -> int list
val forinfo : Ast0_cocci.forinfo -> int list
val pragmainfo : Ast0_cocci.pragmainfo -> int list
val case_line : Ast0_cocci.case_line -> int list
val string_fragment : Ast0_cocci.string_fragment -> int list
val attribute : Ast0_cocci.attr -> int list
val attr_arg : Ast0_cocci.attr_arg -> int list
val top_level : Ast0_cocci.top_level -> int list
