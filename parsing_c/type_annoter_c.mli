
type namedef = 
  | VarOrFunc of string * Ast_c.fullType
  | TypeDef   of string * Ast_c.fullType
  | StructUnionNameDef of 
      string * (Ast_c.structUnion * Ast_c.structType) Ast_c.wrap

type environment = namedef list list (* cos have nested scope, so nested list*)

val initial_env : environment

(* In fact do via side effects. Fill in the type information that was put
 * to None during parsing 
 *)
val annotate_program : 
  environment -> Ast_c.toplevel list -> 
  (Ast_c.toplevel * environment Common.pair) list
