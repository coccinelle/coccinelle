
val fix_tokens_cpp : Parser_c.token list -> Parser_c.token list

(* next stream tokens -> passed stream tokens -> final next token *)
val lookahead : Parser_c.token list -> Parser_c.token list -> Parser_c.token
