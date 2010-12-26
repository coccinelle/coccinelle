
type pos = { current_element : string;
	     file :string ;
	     line : int;
	     col : int;
	     line_end : int;
	     col_end : int; }

type param_type =
    Pos of pos list
  | Str of string
  | Type of Ast_c.fullType
  | Init of Ast_c.initialiser
  | Int of int
  | Param of Ast_c.parameterType
  | ParamList of Ast_c.parameterType Ast_c.wrap2 list
  | Expr of Ast_c.expression
  | ExprList of Ast_c.argument Ast_c.wrap2 list
  | Decl of Ast_c.declaration
  | Field of Ast_c.field
  | Stmt of Ast_c.statement

val fcts : (string, param_type list -> string ref list -> unit) Hashtbl.t

(* ---------------------------------------------------------------------- *)
(* Match management *)

val inc_match : bool ref
val include_match : bool -> unit
val dir : unit -> string

(* ---------------------------------------------------------------------- *)
(* Iteration management *)

class iteration : unit ->
    object
      val mutable files : string list
      val mutable virtual_identifiers : (string * string) list
      val mutable virtual_rules : string list
      method set_files : string list -> unit
      method add_virtual_rule : string -> unit
      method add_virtual_identifier : string -> string -> unit
      method register : unit -> unit
    end
