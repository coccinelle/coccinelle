
type finalType = Ast_c.fullType

(* completed NamedType, removed ParenType, use StructUnionName when can *)
type completed_and_simplified = Ast_c.fullType

type completed_typedef = Ast_c.fullType
type removed_typedef = Ast_c.fullType

(* lookup *)
val type_field:
  string -> (Ast_c.structUnion * Ast_c.structType) -> Ast_c.fullType

(* typing rules *)
val lub:
  Ast_c.arithOp -> finalType option -> finalType option -> Ast_c.exp_info

(* helpers *)
val structdef_to_struct_name:
  finalType -> finalType
val fake_function_type:
  finalType option -> Ast_c.argument Ast_c.wrap2 list -> finalType option

(* builders *)
val make_info_def: finalType -> Ast_c.exp_info
val make_info: Ast_c.exp_type -> Ast_c.exp_info

val noTypeHere: Ast_c.exp_info

val do_with_type:
  (finalType -> Ast_c.exp_info) -> Ast_c.exp_info -> Ast_c.exp_info
val get_opt_type:
  Ast_c.expression -> finalType option
