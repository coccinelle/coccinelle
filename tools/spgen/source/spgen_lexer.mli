type attribute = Org of string * string list | Report of string * string list
type part =
    Description of string
  | Limitations of string list
  | Keywords of string
  | Confidence of string
  | Comments of string
  | Options of string
  | Authors of string list
  | Url of string
  | Rule of (string * string option) * attribute list
val table : (int * int) array ref
exception Eof
val get_start_line : Lexing.lexbuf -> int
val get_column : Lexing.lexbuf -> int
val get_position_str : Lexing.lexbuf -> string
val error : string -> Lexing.lexbuf -> 'a
val illegal : Lexing.lexbuf -> 'a
val split_list : string -> string -> string list
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> part
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> part
val comment : Lexing.lexbuf -> part
val __ocaml_lex_comment_rec : Lexing.lexbuf -> int -> part
val cocci_rule : Lexing.lexbuf -> attribute list
val __ocaml_lex_cocci_rule_rec : Lexing.lexbuf -> int -> attribute list
val format_string : Lexing.lexbuf -> string * string list
val __ocaml_lex_format_string_rec :
  Lexing.lexbuf -> int -> string * string list
