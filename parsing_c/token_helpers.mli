
val info_from_token : Parser_c.token -> Ast_c.info
val visitor_info_from_token : 
  (Ast_c.info -> Ast_c.info) -> Parser_c.token -> Parser_c.token

val is_comment             : Parser_c.token -> bool
val is_not_comment         : Parser_c.token -> bool
val is_cpp_instruction     : Parser_c.token -> bool

val linecol_of_tok : Parser_c.token -> int * int
