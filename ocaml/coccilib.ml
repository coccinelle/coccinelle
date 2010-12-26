(* Function table management *)

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

let fcts : (string, param_type list -> string ref list -> unit) Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

(* ---------------------------------------------------------------------- *)
(* Match management *)

let inc_match = ref true
let include_match x = inc_match := x
let dir () = !Flag.dir

(* ---------------------------------------------------------------------- *)
(* Iteration management *)

class iteration () =
  object
    val mutable files = !Iteration.base_file_list
    val mutable virtual_rules = ([] : string list)
    val mutable virtual_identifiers = ([] : (string * string) list)
    method set_files f = files <- f
    method add_virtual_rule r =
      Iteration.check_virtual_rule r;
      virtual_rules <- Common.union_set [r] virtual_rules
    method add_virtual_identifier i v =
      Iteration.check_virtual_ident i;
      try
	let v1 = List.assoc i virtual_identifiers in
	if not (v = v1)
	then failwith ("multiple values specified for "^i)
      with Not_found ->
	virtual_identifiers <- (i,v) :: virtual_identifiers
    method register () =
      Iteration.add_pending_instance (files,virtual_rules,virtual_identifiers)
  end
