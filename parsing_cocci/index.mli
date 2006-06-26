val expression_dots :
    Ast0_cocci.expression Ast0_cocci.base_dots Ast0_cocci.wrap -> int list
val parameter_dots :
    Ast0_cocci.parameterTypeDef Ast0_cocci.base_dots Ast0_cocci.wrap ->
      int list
val statement_dots :
    Ast0_cocci.statement Ast0_cocci.base_dots Ast0_cocci.wrap -> int list
val ident : Ast0_cocci.base_ident Ast0_cocci.wrap -> int list
val expression : Ast0_cocci.base_expression Ast0_cocci.wrap -> int list
val typeC : Ast0_cocci.base_typeC Ast0_cocci.wrap -> int list
val declaration : Ast0_cocci.base_declaration Ast0_cocci.wrap -> int list
val parameterTypeDef :
    Ast0_cocci.base_parameterTypeDef Ast0_cocci.wrap -> int list
val statement : Ast0_cocci.base_statement Ast0_cocci.wrap -> int list
val top_level : Ast0_cocci.base_top_level Ast0_cocci.wrap -> int list
