(* the result is reversed, as that is what is useful for the caller *)
val parse_string : (string * Ast_c.isWchar) -> Ast_c.info ->
  Parser_c.token list
