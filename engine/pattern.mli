type sequence_processing_style = Ordered | Unordered

type semantic_info_ident = 
  | Function 
  | LocalFunction
  | DontKnow

type ('a, 'b) matcher =
    'a -> 'b -> Lib_engine.metavars_binding -> Lib_engine.metavars_binding list


val match_re_node : (Ast_cocci.rule_elem, Control_flow_c.node) matcher
val match_re_st :   (Ast_cocci.rule_elem, Ast_c.statement)     matcher
val match_re_decl : (Ast_cocci.declaration, Ast_c.declaration) matcher
val match_e_e :     (Ast_cocci.expression, Ast_c.expression)   matcher
val match_arguments :
    sequence_processing_style ->
      (Ast_cocci.expression list, Ast_c.expression list) matcher
val match_params :
    sequence_processing_style ->
    (Ast_cocci.parameterTypeDef list, (Ast_c.parameterTypeDef * Ast_c.il) list) 
    matcher
val match_ft_ft : (Ast_cocci.fullType, Ast_c.fullType) matcher
val match_t_t :   (Ast_cocci.typeC,    Ast_c.fullType) matcher
val match_ident : semantic_info_ident -> (Ast_cocci.ident, string) matcher
val match_opt :
    (Ast_cocci.expression, Ast_c.expression) matcher ->
     Ast_cocci.expression option ->
     Ast_c.exprStatement -> 
     Lib_engine.metavars_binding -> 
     Lib_engine.metavars_binding list


val equal_unaryOp     : Ast_cocci.unaryOp     -> Ast_c.unaryOp -> bool
val equal_assignOp    : Ast_cocci.assignOp    -> Ast_c.assignOp -> bool
val equal_fixOp       : Ast_cocci.fixOp       -> Ast_c.fixOp -> bool
val equal_binaryOp    : Ast_cocci.binaryOp    -> Ast_c.binaryOp -> bool
val equal_arithOp     : Ast_cocci.arithOp     -> Ast_c.arithOp -> bool
val equal_logicalOp   : Ast_cocci.logicalOp   -> Ast_c.logicalOp -> bool
val equal_structUnion : Ast_cocci.structUnion -> Ast_c.structUnion -> bool
val equal_sign        : Ast_cocci.sign        -> Ast_c.sign -> bool
