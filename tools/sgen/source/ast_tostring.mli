(* Turns Ast_cocci base types into strings. *)

(* ------------------------------------------------------------------------- *)
(* TOSTRING FUNCTIONS *)

val meta_tostring : Ast_cocci.meta_name -> string
val constant_tostring : Ast_cocci.constant -> string
val struct_union_tostring : Ast_cocci.structUnion -> string
val sign_tostring : Ast_cocci.sign -> string
val const_vol_tostring : Ast_cocci.const_vol -> string
val storage_tostring : Ast_cocci.storage -> string
val inc_elem_tostring : Ast_cocci.inc_elem -> string
val inc_file_tostring : Ast_cocci.inc_file -> string
val fix_tostring : Ast_cocci.fixOp -> string
val arith_tostring : Ast_cocci.arithOp -> string
val logic_tostring : Ast_cocci.logicalOp -> string
val unary_tostring : Ast_cocci.unaryOp -> string
val assign_tostring : Ast_cocci.assignOp -> string
val  binary_tostring : Ast_cocci.binaryOp -> string
val type_tostring : Ast_cocci.baseType -> string
val whenmodifier_tostring : Ast_cocci.when_modifier -> string
