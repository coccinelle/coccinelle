module Ast_c = Ast_c
module Parse_c = Parse_c
module Parser_c = Parser_c
module Lexer_c = Lexer_c
module Pretty_print_c = Pretty_print_c
module Lib_parsing_c = Lib_parsing_c
module Visitor_c = Visitor_c
module Regexp = Regexp
module Config = Config
module Flag = Flag
module Flag_parsing_c = Flag_parsing_c
module Iteration = Iteration
module Commands = Commands
module Common = Common
module Ast_cocci = Ast_cocci
module Ast0_cocci = Ast0_cocci
module Dumper = Dumper
type pos = {
  current_element : string;
  current_element_line : int;
  current_element_col : int;
  current_element_line_end : int;
  current_element_col_end : int;
  file : string;
  line : int;
  col : int;
  line_end : int;
  col_end : int;
}
type param_type =
    Pos of pos list
  | AssignOp of Ast_c.assignOp
  | BinaryOp of Ast_c.binaryOp
  | Str of string
  | Type of Ast_c.fullType
  | Init of Ast_c.initialiser
  | InitList of Ast_c.initialiser Ast_c.wrap2 list
  | Int of int
  | Param of Ast_c.parameterType
  | ParamList of Ast_c.parameterType Ast_c.wrap2 list
  | DParamList of string Ast_c.wrap Ast_c.wrap2 list
  | Expr of Ast_c.expression
  | ExprList of Ast_c.argument Ast_c.wrap2 list
  | Decl of Ast_c.declaration
  | Field of Ast_c.field
  | FieldList of Ast_c.field list
  | FragList of Ast_c.string_fragment list
  | Fmt of Ast_c.string_format
  | Stmt of Ast_c.statement
  | StmtList of Ast_c.statement_sequencable list
val fcts :
  (string, param_type list -> Ast_c.metavar_binding_kind ref list -> unit)
  Hashtbl.t
val bool_fcts : (string, param_type list -> bool) Hashtbl.t
val variables_to_merge : (unit -> string array) ref
val merged_variables : string list array option ref
val no_format : string -> bool
val cstatement_of_string : string -> string -> Ast_c.statement
val cexpression_of_string : string -> string -> Ast_c.expression
val parse_failure : string -> string -> (string -> 'a) -> 'a
val make_ident : string -> Ast_c.metavar_binding_kind
val make_expr : string -> Ast_c.metavar_binding_kind
val make_expr_with_env : string -> string -> Ast_c.metavar_binding_kind
val make_stmt : string -> Ast_c.metavar_binding_kind
val make_stmt_with_env : string -> string -> Ast_c.metavar_binding_kind
val make_type : string -> Ast_c.metavar_binding_kind
val make_listlen : int -> Ast_c.metavar_binding_kind
val make_full_position :
  Common.filename ->
  string ->
  int ->
  int -> int -> int -> int -> int -> int -> int -> Ast_c.metavar_binding_kind
val make_position :
  Common.filename ->
  string -> int -> int -> int -> int -> Ast_c.metavar_binding_kind
val inc_match : bool ref
val include_match : bool -> unit
val exited : bool ref
val exit : unit -> unit
val dir : unit -> string
val files : unit -> string list
val cocci_version : unit -> string
val build_link : pos -> string -> string -> string
val print_todo : ?color:string -> ?msg:string -> pos -> unit
val print_link : ?color:string -> ?msg:string -> pos -> unit
val print_safe_todo : ?color:string -> ?msg:string -> pos -> unit
val print_safe_link : ?color:string -> ?msg:string -> pos -> unit
val print_main : ?color:string -> string -> pos list -> unit
val print_sec : ?color:string -> string -> pos list -> unit
val print_secs : ?color:string -> string -> pos list -> unit
val basename_pos : pos -> pos
module Ana :
  sig
    type result = Externalanalysis.result
    type bound = Externalanalysis.bound
    val show_bound : Externalanalysis.bound -> string
    val show_result : Externalanalysis.result -> string
    val load_results : string -> unit
    val find : pos -> Externalanalysis.result list
    val inter :
      Externalanalysis.result ->
      Externalanalysis.result -> Externalanalysis.result option
    val satisfy : (Externalanalysis.result list -> bool) -> pos -> bool
    val satisfy1 : (Externalanalysis.result -> bool) -> pos -> bool
    val has_any : pos -> bool
    val for_all : (Externalanalysis.result -> bool) -> pos -> bool
    val for_all1 : (Externalanalysis.result -> bool) -> pos -> bool
    val exists : (Externalanalysis.result -> bool) -> pos -> bool
    val single_int : int64 -> Externalanalysis.result -> bool
    val contains_int : int64 -> Externalanalysis.result -> bool
    val has_only_nul : pos -> bool
    val has_also_nul : pos -> bool
    val has_also_int : int64 -> pos -> bool
  end
