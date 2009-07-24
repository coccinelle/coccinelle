val string_of_toplevel: Ast_c.toplevel -> string
val string_of_expression: Ast_c.expression -> string

val string_of_program: Ast_c.program -> string

val show_info: bool ref
val show_qualifier: bool ref
val show_expr_info: bool ref
