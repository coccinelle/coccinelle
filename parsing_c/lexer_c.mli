val pr2 : string -> unit
val pr2_once : string -> unit
exception Lexical of string
val tok : Lexing.lexbuf -> string
val eoltok : Lexing.lexbuf -> string
val tokinfo : Lexing.lexbuf -> Ast_c.info
val eoltokinfo : Lexing.lexbuf -> Ast_c.info
val eoftokinfo : Lexing.lexbuf -> Parser_c.token
val no_ifdef_mark : unit -> (int * int) option ref
val tok_add_s : string -> Ast_c.info -> Ast_c.info
val function_cpp_eat_until_nl :
  ('a -> string) ->
  ('a -> string) -> ('a -> string) -> string -> 'a -> string
val keyword_table : (string, Ast_c.info -> Parser_c.token) Hashtbl.t
val cpp_keyword_table : (string, Ast_c.info -> Parser_c.token) Hashtbl.t
val ibm_keyword_table : (string, Ast_c.info -> Parser_c.token) Hashtbl.t
val error_radix : string -> string
val is_long_dec : string -> 'a -> 'b -> 'a -> 'a -> 'a
val is_long_ho :
  string -> 'a -> 'a -> 'a -> 'a -> int -> int -> (int -> int) -> 'a
val is_long_oct : string -> 'a -> 'a -> 'a -> 'a -> 'a
val is_long_hex : string -> 'a -> 'a -> 'a -> 'a -> 'a
val sint : Ast_c.sign * Ast_c.base
val uint : Ast_c.sign * Ast_c.base
val slong : Ast_c.sign * Ast_c.base
val ulong : Ast_c.sign * Ast_c.base
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Parser_c.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Parser_c.token
val char : Lexing.lexbuf -> string
val __ocaml_lex_char_rec : Lexing.lexbuf -> int -> string
val restchars : Lexing.lexbuf -> string
val __ocaml_lex_restchars_rec : Lexing.lexbuf -> int -> string
val string : Lexing.lexbuf -> string
val __ocaml_lex_string_rec : Lexing.lexbuf -> int -> string
val comment : Lexing.lexbuf -> string
val __ocaml_lex_comment_rec : Lexing.lexbuf -> int -> string
val parse_newline : Lexing.lexbuf -> string
val __ocaml_lex_parse_newline_rec : Lexing.lexbuf -> int -> string
val cpp_in_comment_eat_until_nl : Lexing.lexbuf -> string
val __ocaml_lex_cpp_in_comment_eat_until_nl_rec :
  Lexing.lexbuf -> int -> string
val cpp_eat_until_nl : Lexing.lexbuf -> string
val __ocaml_lex_cpp_eat_until_nl_rec : Lexing.lexbuf -> int -> string
