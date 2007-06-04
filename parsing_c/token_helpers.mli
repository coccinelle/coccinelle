
val is_comment             : Parser_c.token -> bool
val is_real_comment        : Parser_c.token -> bool
val is_not_comment         : Parser_c.token -> bool
val is_cpp_instruction     : Parser_c.token -> bool
val is_eof                 : Parser_c.token -> bool
val is_statement_token     : Parser_c.token -> bool
val is_start_of_something  : Parser_c.token -> bool
val is_binary_operator     : Parser_c.token -> bool
val is_stuff_taking_parenthized : Parser_c.token -> bool

val info_from_token : Parser_c.token -> Ast_c.info

val visitor_info_from_token : 
  (Ast_c.info -> Ast_c.info) -> Parser_c.token -> Parser_c.token

val linecol_of_tok : Parser_c.token -> int * int
val col_of_tok  : Parser_c.token -> int
val line_of_tok : Parser_c.token -> int

val pos_of_token : Parser_c.token -> int

val str_of_token : Parser_c.token -> string

