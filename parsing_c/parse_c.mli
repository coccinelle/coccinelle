open Common

(* The main function is parse_c_and_cpp. It uses globals in Lexer_Parser and 
 * Parsing_hacks. Especially Parsing_hacks._defs which often comes
 * from a standard.h macro file. Cf also init_defs below.
 *)

(* ---------------------------------------------------------------------- *)
type program2 = toplevel2 list
   and toplevel2 = Ast_c.toplevel * info_item

    (* the token list contains now also the comment-tokens *)
    and info_item = (string * Parser_c.token list)

(* ---------------------------------------------------------------------- *)
(* a few globals *)
val parse_cpp_define_file : 
  filename -> (string, Cpp_token_c.define_def) assoc

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Cpp_token_c.define_def) Hashtbl.t ref
(* usually correspond to what is inside your standard.h *)
val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref

val init_defs : filename -> unit
val init_defs_builtins : filename -> unit


(* ---------------------------------------------------------------------- *)
(* This is the main function *)
val parse_print_error_heuristic:  
  filename (*cfile*) -> (program2 * Parsing_stat.parsing_stat)
(* alias of previous func *)
val parse_c_and_cpp : 
  filename (*cfile*) -> (program2 * Parsing_stat.parsing_stat)

(* use some .ast_raw memoized version, and take care if obsolete *)
val parse_cache:
  filename (*cfile*) -> (program2 * Parsing_stat.parsing_stat)

(* ---------------------------------------------------------------------- *)
(* used also for the standard.h file *)
val tokens:      ?profile:bool -> filename -> Parser_c.token list
val tokens_of_string: string -> Parser_c.token list

val parse:                        filename -> Ast_c.program
val parse_print_error:            filename -> Ast_c.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_c.token) -> Lexing.lexbuf -> 'a) -> string -> 'a



(* ---------------------------------------------------------------------- *)
(* Easy way to build complex Ast elements from simple strings.
 * Can also be useful when called from the ocaml toplevel to test. 
 *)
val type_of_string      : string -> Ast_c.fullType
val statement_of_string : string -> Ast_c.statement

(* similar but use parse_c_and_cpp and a /tmp/__cocci.c and extract the part *)
val cstatement_of_string  : string -> Ast_c.statement
val cexpression_of_string : string -> Ast_c.expression




(* ---------------------------------------------------------------------- *)
(* a few helpers *)
val print_commentized       : Parser_c.token list -> unit

val program_of_program2 : program2 -> Ast_c.program
val with_program2: (Ast_c.program -> Ast_c.program) -> program2 -> program2



