
type 'a distributer =
    (Ast_c.info -> Ast_c.info) * 
    (Ast_c.info -> Ast_c.info) *
    (Ast_c.info -> Ast_c.info) -> 
    'a -> 'a

val distribute_mck :
  Ast_cocci.mcodekind -> 'a distributer -> 'a -> Ast_c.metavars_binding -> 'a

val distribute_mck_e    : Ast_c.expression  distributer
val distribute_mck_decl : Ast_c.declaration distributer
val distribute_mck_stat : Ast_c.statement   distributer
val distribute_mck_type : Ast_c.fullType    distributer

val distribute_mck_node : Control_flow_c.node2 distributer
