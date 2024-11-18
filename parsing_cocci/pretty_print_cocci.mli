(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

val unparse : Ast_cocci.metavar list -> Ast_cocci.rule -> unit
val unparse_to_string : Ast_cocci.rule -> string
val expression : Ast_cocci.expression -> unit
val expression_to_string : Ast_cocci.expression -> string
val ident : Ast_cocci.ident -> unit
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
    ('a -> unit) -> 'a -> Ast_cocci.befaft -> unit
val print_anything : string -> Ast_cocci.anything list list -> unit
val pp_print_anything : Ast_cocci.anything -> unit

val print_plus_flag : bool ref
val print_minus_flag : bool ref

val print_rule_elem : Ast_cocci.rule_elem -> unit
val print_when_modif : Ast_cocci.when_modifier -> unit

val dependency : Ast_cocci.dependency -> unit
