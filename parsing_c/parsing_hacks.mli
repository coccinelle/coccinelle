open Common 

type define_body = (unit,string list) either * Parser_c.token list

val _defs : (string, define_body) Hashtbl.t ref


val fix_tokens_define : Parser_c.token list -> Parser_c.token list

val extract_cpp_define : Parser_c.token list -> (string, define_body) assoc




val fix_tokens_cpp : Parser_c.token list -> Parser_c.token list

(* next stream tokens -> passed stream tokens -> final next token *)
val lookahead : Parser_c.token list -> Parser_c.token list -> Parser_c.token

