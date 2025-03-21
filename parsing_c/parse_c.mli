(* The main function is parse_c_and_cpp. It uses globals in Lexer_Parser and
 * and also _defs below which often comes from a standard.h macro file.
 * cf also init_defs_xxx below.
*)

type line_restriction = Included of int * int | Excluded of int * int

type program2 = toplevel2 list
   and extended_program2 = toplevel2 list *
      (string, Lexer_parser.identkind) Common.scoped_h_env (* type defs *) *
      (string, Cpp_token_c.define_def) Hashtbl.t (* macro defs *)
   and toplevel2 =
    Ast_c.toplevel * info_item
     (* the token list contains now also the comment-tokens *)
       and info_item = (string * Parser_c.token list)

type 'a generic_parse_info = {
  filename : string;
  ranges : line_restriction list option;
  parse_trees : 'a; (* program2 or extended_program2 *)
  statistics : Parsing_stat.parsing_stat;
}

type parse_info = program2 generic_parse_info

type extended_parse_info = extended_program2 generic_parse_info

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Cpp_token_c.define_def) Hashtbl.t ref
(* usually correspond to what is inside your standard.h *)
val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref

val init_defs_macros : Common.filename -> unit
val init_defs_builtins : Common.filename -> unit


(* This is the main function *)
val parse_c_and_cpp :
    bool (* true if format characters need to be parsed *) ->
    bool (* true if parsing results should be cached *) ->
      Common.filename (*cfile*) ->
	(program2 * Parsing_stat.parsing_stat)
val parse_c_and_cpp_keep_typedefs :
    (string, Lexer_parser.identkind) Common.scoped_h_env option (*typedefs*) ->
      (string, Cpp_token_c.define_def) Hashtbl.t option (* macro defs *) ->
      bool (* true if format characters need to be parsed *) ->
      bool (* true if parsing results should be cached *) ->
	Common.filename (*cfile*) ->
	  extended_parse_info * extended_parse_info list

(* use some .ast_raw memoized version, and take care if obsolete *)
val parse_cache:
    (string, Lexer_parser.identkind) Common.scoped_h_env option (* typedefs *) ->
    bool (* true if format characters need to be parsed *) ->
    bool (* true if parsing results should be cached *) ->
    Common.filename (*cfile*) -> bool (* true if no changes *) ->
    extended_parse_info * extended_parse_info list


(* ---------------------------------------------------------------------- *)
(* used to extract macros from standard.h, but also now from regular C files
 * in -extract_macros to later feed an automatically build standard.h *)
val extract_macros :
  Common.filename -> (string, Cpp_token_c.define_def) Common.assoc


(* ---------------------------------------------------------------------- *)
(* used also for the standard.h file *)
val tokens:      ?profile:bool -> Common.filename -> Parser_c.token list
val tokens_of_string: string -> Lexing.position option -> Parser_c.token list

val parse:                        Common.filename -> Ast_c.program

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

(* ---------------------------------------------------------------------- *)
(* custom error reporting function *)
type parse_error_function = int -> Parser_c.token list -> (int * int) ->
    string array -> int -> unit
