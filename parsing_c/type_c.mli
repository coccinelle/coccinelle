
type finalType = Ast_c.fullType

(* lookup *)
val type_field: 
  string -> (Ast_c.structUnion * Ast_c.structType) -> Ast_c.fullType

(* typing rules *)
val lub: 
  finalType option -> finalType option -> Ast_c.exp_info

(* helpers *)
val structdef_to_struct_name: 
  finalType -> finalType
val fake_function_type: 
  finalType option -> Ast_c.argument Ast_c.wrap2 list -> finalType option

(* return normalize types ? *)
val type_of_function: 
  Ast_c.definition -> finalType
val type_of_decl:
  Ast_c.declaration -> finalType
val structdef_of_decl:
  Ast_c.declaration -> Ast_c.structUnion * Ast_c.structType


(* builders *)
val make_info_def: finalType -> Ast_c.exp_info
val make_info: Ast_c.exp_type -> Ast_c.exp_info

val noTypeHere: Ast_c.exp_info

val do_with_type: 
  (finalType -> Ast_c.exp_info) -> Ast_c.exp_info -> Ast_c.exp_info
val get_opt_type: 
  Ast_c.expression -> finalType option
