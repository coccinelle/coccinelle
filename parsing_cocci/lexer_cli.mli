exception Lexical of string
val tok : Lexing.lexbuf -> string
type cli_tok = Id of string | NotEq | EqEq | Other of string | EOF
val pretty_print : cli_tok -> string
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> cli_tok
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> cli_tok
