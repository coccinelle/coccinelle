val compute_lines : Ast0_cocci.rule -> Ast0_cocci.rule

val expression_dots :
    Ast0_cocci.expression Ast0_cocci.dots ->
      Ast0_cocci.expression Ast0_cocci.dots
val parameter_dots :
    Ast0_cocci.parameterTypeDef Ast0_cocci.dots ->
      Ast0_cocci.parameterTypeDef Ast0_cocci.dots
val initialiser_dots :
    Ast0_cocci.initialiser Ast0_cocci.dots ->
      Ast0_cocci.initialiser Ast0_cocci.dots
val statement_dots :
    Ast0_cocci.statement Ast0_cocci.dots ->
      Ast0_cocci.statement Ast0_cocci.dots

val ident : Ast0_cocci.ident -> Ast0_cocci.ident
val expression : Ast0_cocci.expression -> Ast0_cocci.expression
val typeC : Ast0_cocci.typeC -> Ast0_cocci.typeC
val declaration : Ast0_cocci.declaration -> Ast0_cocci.declaration
val parameterTypeDef :
    Ast0_cocci.parameterTypeDef -> Ast0_cocci.parameterTypeDef
val statement : Ast0_cocci.statement -> Ast0_cocci.statement
val top_level : Ast0_cocci.top_level -> Ast0_cocci.top_level
