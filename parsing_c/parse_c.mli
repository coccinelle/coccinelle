open Common

type parsing_stat = {
    filename: filename;
    mutable passing_through_lines: int;
    mutable have_timeout: bool;
    mutable correct: int;
    mutable bad: int;
  } 

type info_item = (string * Parser_c.token list)

type program2 = programElement2 list
     and programElement2 = Ast_c.programElement * info_item




val tokens:      filename -> Parser_c.token list
val tokens_string: string -> Parser_c.token list

val parse:                        filename -> Ast_c.program
val parse_print_error:            filename -> Ast_c.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_c.token) -> Lexing.lexbuf -> 'a) -> string -> 'a

(* this is the main function *)
val parse_print_error_heuristic:  filename -> (program2 * parsing_stat)




val print_parsing_stat_list: parsing_stat list -> unit

(* also used in unparser *)
val info_from_token: Parser_c.token -> Ast_c.info

(* easy way to build complex Ast elements from simple strings *)
val type_of_string : string -> Ast_c.fullType
val statement_of_string : string -> Ast_c.statement
