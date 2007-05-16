open Common

val tokens:      filename -> Parser_c.token list
val tokens_string: string -> Parser_c.token list

val parse:                        filename -> Ast_c.program
val parse_print_error:            filename -> Ast_c.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_c.token) -> Lexing.lexbuf -> 'a) -> string -> 'a

(* easy way to build complex Ast elements from simple strings *)
val type_of_string      : string -> Ast_c.fullType
val statement_of_string : string -> Ast_c.statement



(* the token list contain also the comment-tokens  *)
type info_item = (string * Parser_c.token list)

type program2 = toplevel2 list
     and toplevel2 = Ast_c.toplevel * info_item

type parsing_stat = {
    filename: filename;
    mutable have_timeout: bool;
    mutable correct: int;
    mutable bad: int;
  } 


(* this is the main function *)
val parse_print_error_heuristic:  filename -> (program2 * parsing_stat)


val print_parsing_stat_list: parsing_stat list -> unit

