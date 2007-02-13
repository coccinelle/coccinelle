
type namedef = 
  | VarOrFunc of string * Ast_c.fullType
  | TypeDef   of string * Ast_c.fullType
  | StructUnionNameDef of string * Ast_c.structType Ast_c.wrap

type environment = namedef list list (* cos have nested scope, so nested list*)

val initial_env : environment

(* Fill in the type information that was put to None during parsing *)
val annotate_program : 
  environment -> Ast_c.programElement list -> 
  (Ast_c.programElement * environment Common.pair) list
