val ast0toast : Ast0_cocci.rule -> Ast_cocci.rule

val ident : Ast0_cocci.ident -> Ast_cocci.ident
val expression : Ast0_cocci.expression -> Ast_cocci.expression
val typeC : Ast0_cocci.typeC -> Ast_cocci.typeC
val declaration : Ast0_cocci.declaration -> Ast_cocci.declaration
val parameterTypeDef :
    Ast0_cocci.parameterTypeDef -> Ast_cocci.parameterTypeDef
val parameter_list : Ast0_cocci.parameter_list -> Ast_cocci.parameter_list
val mcode : 'a Ast0_cocci.mcode -> 'a Ast_cocci.mcode
