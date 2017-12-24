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
val token : Lexing.lexbuf -> Parser_c.token
val char : Lexing.lexbuf -> string
val restchars : Lexing.lexbuf -> string
val string : Lexing.lexbuf -> string
val comment : Lexing.lexbuf -> string
val parse_newline : Lexing.lexbuf -> string
val cpp_in_comment_eat_until_nl : Lexing.lexbuf -> string
val cpp_eat_until_nl : Lexing.lexbuf -> string
