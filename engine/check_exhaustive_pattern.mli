module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c
val dumb_astcocci_rule_elem : A.base_rule_elem -> unit
val dumb_astcocci_decl : A.base_declaration -> unit
val dumb_astcocci_initialiser : A.base_declaration -> unit
val dumb_astcocci_field : A.base_field -> unit
val dumb_astcocci_expr : A.base_expression -> unit
val dumb_astcocci_fulltype : A.base_fullType -> unit
val dumb_astcocci_type : A.base_typeC -> unit
