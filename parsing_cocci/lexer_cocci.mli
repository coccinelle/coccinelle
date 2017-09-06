module D = Data
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module P = Parse_aux
module FC = Flag_parsing_cocci
exception Lexical of string
val tok : Lexing.lexbuf -> string
val line : int ref
val file : string ref
val logical_line : int ref
val current_line_type : (D.line_type * int * int) ref
val prev_plus : bool ref
val line_start : int ref
val get_current_line_type :
  Lexing.lexbuf ->
  D.line_type * int * int * int * int * int * 'a list * 'b list * 'c list *
  string
val current_line_started : bool ref
val col_zero : bool ref
val contextify :
  'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j ->
  D.line_type * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j
val reset_line : Lexing.lexbuf -> unit
val started_line : int ref
val start_line : bool -> unit
val pass_zero : 'a -> unit
val lexerr : string -> string -> 'a
val opt_reverse_token : D.line_type -> D.line_type
val add_current_line_type : D.line_type -> unit
val check_minus_context_linetype : string -> unit
val check_context_linetype : string -> unit
val check_plus_linetype : string -> unit
val check_arity_context_linetype : string -> unit
val check_comment : string -> unit
val process_include : int -> int -> string -> string
type pm = PATCH | MATCH | UNKNOWN
val pm : pm ref
val patch_or_match : pm -> unit
val metavariables : (string, D.clt -> Parser_cocci_menhir.token) Hashtbl.t
val all_metavariables :
  (string, (string * (D.clt -> Parser_cocci_menhir.token)) list) Hashtbl.t
val type_names : (string, D.clt -> Parser_cocci_menhir.token) Hashtbl.t
val attr_names : (string, D.clt -> Parser_cocci_menhir.token) Hashtbl.t
val declarer_names : (string, D.clt -> Parser_cocci_menhir.token) Hashtbl.t
val iterator_names : (string, D.clt -> Parser_cocci_menhir.token) Hashtbl.t
val symbol_names : (string, D.clt -> Parser_cocci_menhir.token) Hashtbl.t
val rule_names : (string, unit) Hashtbl.t
val check_var : string -> D.clt -> Parser_cocci_menhir.token
val id_tokens : Lexing.lexbuf -> Parser_cocci_menhir.token
val mkassign :
  Ast_cocci.arithOp -> Lexing.lexbuf -> Parser_cocci_menhir.token
val init : 'a -> unit
val reinit : 'a -> unit
val spinit: unit -> unit
val include_init : 'a -> unit
val drop_spaces : string -> string
val token : Lexing.lexbuf -> Parser_cocci_menhir.token
val metavariable_decl_token : Lexing.lexbuf -> Parser_cocci_menhir.token
val char : Lexing.lexbuf -> string
val restchars : Lexing.lexbuf -> string
val string : Lexing.lexbuf -> string
val comment : (string -> unit) -> Lexing.lexbuf -> string
