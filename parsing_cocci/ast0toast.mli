(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val ast0toast :
    string -> Ast_cocci.dependency -> string list (* dropped isos *) ->
      Ast_cocci.exists -> Ast0_cocci.rule -> bool list -> Ast_cocci.ruletype ->
	Ast_cocci.rule
val ast0toast_toplevel : Ast0_cocci.top_level -> Ast_cocci.top_level

val ident : Ast0_cocci.ident -> Ast_cocci.ident
val expression : Ast0_cocci.expression -> Ast_cocci.expression
val assignOp : Ast0_cocci.assignOp -> Ast_cocci.assignOp
val binaryOp : Ast0_cocci.binaryOp -> Ast_cocci.binaryOp
val expression_dots :
    Ast0_cocci.expression Ast0_cocci.dots ->
      Ast_cocci.expression Ast_cocci.dots
val initialiser : Ast0_cocci.initialiser -> Ast_cocci.initialiser
val statement : Ast0_cocci.statement -> Ast_cocci.statement
val forinfo : Ast0_cocci.forinfo -> Ast_cocci.forinfo
val statement_dots :
    Ast0_cocci.statement Ast0_cocci.dots -> Ast_cocci.statement Ast_cocci.dots
val declaration_dots :
    Ast0_cocci.declaration Ast0_cocci.dots ->
      Ast_cocci.annotated_decl Ast_cocci.dots
val field_dots :
    Ast0_cocci.field Ast0_cocci.dots ->
      Ast_cocci.annotated_field Ast_cocci.dots
val enum_decl_dots :
    Ast0_cocci.enum_decl Ast0_cocci.dots ->
      Ast_cocci.enum_decl Ast_cocci.dots
val define_param_dots :
    Ast0_cocci.define_param Ast0_cocci.dots ->
      Ast_cocci.define_param Ast_cocci.dots
val case_line : Ast0_cocci.case_line -> Ast_cocci.case_line
val string_fragment : Ast0_cocci.string_fragment -> Ast_cocci.string_fragment
val attribute : Ast0_cocci.attr -> Ast_cocci.attr
val attr_arg : Ast0_cocci.attr_arg -> Ast_cocci.attr_arg
val typeC : bool (*allminus*) -> Ast0_cocci.typeC -> Ast_cocci.fullType
val declaration : Ast0_cocci.declaration -> Ast_cocci.declaration
val field : Ast0_cocci.field -> Ast_cocci.field
val enum_decl : Ast0_cocci.enum_decl -> Ast_cocci.enum_decl
val parameterTypeDef :
    Ast0_cocci.parameterTypeDef -> Ast_cocci.parameterTypeDef
val parameter_list : Ast0_cocci.parameter_list -> Ast_cocci.parameter_list
val top_level : Ast0_cocci.top_level -> Ast_cocci.top_level
val convert_info : Ast0_cocci.info -> Ast_cocci.info
val constraints : Ast0_cocci.constraints -> Ast_cocci.constraints
