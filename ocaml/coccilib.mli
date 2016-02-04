(** Library of functions for use with Coccinelle OCaml script code
*)

(** A value of type {b pos} describes a position in a source file.
*)
type pos = {
  current_element : string;
  (** {b current_element} is the name of the function containing the
      matched position *)
  file :string ;
  (** {b file} is the name of the file containing the matched
      position *)
  line : int;
  (** {b line} is the number of the line containing the first
      character of the matched position *)
  col : int;
  (** {b col} is the column containing the first character of the
      matched position *)
  line_end : int;
  (** {b line_end} is the number of the line containing the last
      character of the matched position *)
  col_end : int;
  (** {b col_end} is the column containing the last character of the
       matched position. *)
}

(**
   Types describing the metavariables
*)
type param_type =
    Pos of pos list
  | Str of string
  | Type of Ast_c.fullType
  | Init of Ast_c.initialiser
  | InitList of Ast_c.initialiser Ast_c.wrap2 list
  | Int of int
  | Param of Ast_c.parameterType
  | ParamList of Ast_c.parameterType Ast_c.wrap2 list
  | Expr of Ast_c.expression
  | ExprList of Ast_c.argument Ast_c.wrap2 list
  | Decl of Ast_c.declaration
  | Field of Ast_c.field
  | FieldList of Ast_c.field list
  | Stmt of Ast_c.statement

(* ---------------------------------------------------------------------- *)
(* Match management *)

(** If the argument is true, retain the environment with respect to
    which the ocaml script code is being executed for use in subsequent
    rules.  If the argument is false, discard this environment.  By
    default, the environment is retained.
*)
val include_match : bool -> unit

(**
   See include_match
*)
val inc_match : bool ref

(** If called, aborts the treatment of the current file.  All previous
    changes take effect.
*)
val exit : unit -> unit

(**
   See exit
*)
val exited : bool ref

(** Returns the directory on which spatch was launched.*)
val dir : unit -> string

(**/**)

(**
   For internal use only
*)
val fcts : (string, param_type list -> string ref list -> unit) Hashtbl.t

(* org mode *)

val print_main : ?color:string -> string -> pos list -> unit
val print_sec  : ?color:string -> string -> pos list -> unit
val print_secs : ?color:string -> string -> pos list -> unit


(* ---------------------------------------------------------------------- *)
(* Pos transformations *)

(** convert the filename of a pos to its basename *)
val basename_pos : pos -> pos


(* ---------------------------------------------------------------------- *)
(* External analysis *)

(** external analysis integration. Note: do not use after transformations. *)
module Ana :
  sig
    (** the type of analysis results: currently only integer ranges. *)
    type result = Externalanalysis.result
    type bound  = Externalanalysis.bound

    (** convert a result value to a string for showing. *)
    val show_result : result -> string

    (** convert a bound to a string for showing. *)
    val show_bound : bound -> string

    (** loads some analysis results from the given file. *)
    val load_results : string -> unit

    (** finds the analysis results for a given position. *)
    val find : pos -> result list

    (** computes the intersection of analysis results, if possible. *)
    val inter :
      result -> result -> result option

    (** predicate over a list of analysis results of a given position. *)
    val satisfy : (result list -> bool) -> pos -> bool

    (** predicate over the intersection of analysis results. *)
    val satisfy1 : (result -> bool) -> pos -> bool

    (** true if an analysis result exists for the given position. *)
    val has_any : pos -> bool

    (** predicate over all analysis results of a given position. *)
    val for_all : (result -> bool) -> pos -> bool

    (** predicate over all analysis results (at least one) of a given position. *)
    val for_all1 : (result -> bool) -> pos -> bool

    (** true if the predicate is satisfied for at least one result
        of a given position. *)
    val exists : (result -> bool) -> pos -> bool

    (** true if the result contains only a single integer as range *)
    val single_int : int64 -> result -> bool

    (** true if the result range contains the given integer. *)
    val contains_int : int64 -> result -> bool

    (** analysis result of the position has only the zero value. *)
    val has_only_nul : pos -> bool

    (** analysis result of the position contains also the zero value. *)
    val has_also_nul : pos -> bool

    (** analysis result of the position contains also the given integer. *)
    val has_also_int : int64 -> pos -> bool
  end
