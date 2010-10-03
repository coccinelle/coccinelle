type namedef = 
  | VarOrFunc    of string * Ast_c.exp_type
  | EnumConstant of string * string option

  | TypeDef      of string * Ast_c.fullType
  | StructUnionNameDef of string *
      (Ast_c.structUnion * Ast_c.structType) Ast_c.wrap

  | Macro of string * (Ast_c.define_kind * Ast_c.define_val)

(* have nested scope, so nested list*)
type environment = namedef list list 

(* can be set with init_env *)
val initial_env : environment ref
(* ex: config/envos/environment_unix.h *)
val init_env : Common.filename -> unit



val annotate_type_and_localvar : 
  environment -> Ast_c.toplevel list -> 
  (Ast_c.toplevel * environment Common.pair) list

(* julia: cocci *)
val annotate_test_expressions : 
  Ast_c.toplevel list -> unit



(* !!Annotate via side effects!!. Fill in the type  
 * information that was put to None during parsing.
 *)
val annotate_program : 
  environment -> Ast_c.toplevel list -> 
  (Ast_c.toplevel * environment Common.pair) list

