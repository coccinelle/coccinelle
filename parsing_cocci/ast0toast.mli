val ast0toast : Ast0_cocci.rule -> Ast_cocci.rule

val ident : Ast0_cocci.ident -> Ast_cocci.ident
val expression : Ast0_cocci.expression -> Ast_cocci.expression
val statement : Ast0_cocci.statement -> Ast_cocci.statement
val statement_dots :
    Ast0_cocci.statement Ast0_cocci.dots -> Ast_cocci.statement Ast_cocci.dots
val fullType : Ast0_cocci.fullType -> Ast_cocci.fullType
val declaration : Ast0_cocci.declaration -> Ast_cocci.declaration
val parameterTypeDef :
    Ast0_cocci.parameterTypeDef -> Ast_cocci.parameterTypeDef
val parameter_list : Ast0_cocci.parameter_list -> Ast_cocci.parameter_list
val mcode : 'a Ast0_cocci.mcode -> 'a Ast_cocci.mcode
