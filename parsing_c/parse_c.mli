open Common

val tokens:      filename -> Parser_c.token list
val tokens_string: string -> Parser_c.token list

val parse:                        filename -> Ast_c.program
val parse_print_error:            filename -> Ast_c.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_c.token) -> Lexing.lexbuf -> 'a) -> string -> 'a


type parsing_stat = {
    filename: filename;
    mutable passing_through_lines: int;
    mutable have_timeout: bool;
    mutable correct: int;
    mutable bad: int;
  } 
(* this is the main function *)
val parse_print_error_heuristic:  filename -> (Ast_c.program2 * parsing_stat)



(* also used in unparser *)
val info_from_token: Parser_c.token -> Ast_c.info

val print_parsing_stat_list: parsing_stat list -> unit

