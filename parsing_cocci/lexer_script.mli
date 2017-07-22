module D = Data
module Ast = Ast_cocci
exception Lexical of string
val tok : Lexing.lexbuf -> string
val language : string ref
val inc_line : 'a -> unit
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Parser_cocci_menhir.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Parser_cocci_menhir.token
val string : Lexing.lexbuf -> string
val __ocaml_lex_string_rec : Lexing.lexbuf -> int -> string
val cstring : Lexing.lexbuf -> string
val __ocaml_lex_cstring_rec : Lexing.lexbuf -> int -> string
