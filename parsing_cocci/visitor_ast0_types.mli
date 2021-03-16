type ('a, 'n) inout = 'a -> 'n * 'a
type 'n all_functions = {
  meta_mcode : (Ast_cocci.meta_name Ast0_cocci.mcode, 'n) inout;
  ident : (Ast0_cocci.ident, 'n) inout;
  expression : (Ast0_cocci.expression, 'n) inout;
  assignOp : (Ast0_cocci.assignOp, 'n) inout;
  binaryOp : (Ast0_cocci.binaryOp, 'n) inout;
  typeC : (Ast0_cocci.typeC, 'n) inout;
  declaration : (Ast0_cocci.declaration, 'n) inout;
  field : (Ast0_cocci.field, 'n) inout;
  enum_decl : (Ast0_cocci.enum_decl,'n) inout;
  initialiser : (Ast0_cocci.initialiser, 'n) inout;
  initialiser_list : (Ast0_cocci.initialiser_list, 'n) inout;
  parameter : (Ast0_cocci.parameterTypeDef, 'n) inout;
  parameter_list : (Ast0_cocci.parameter_list, 'n) inout;
  statement : (Ast0_cocci.statement, 'n) inout;
  forinfo : (Ast0_cocci.forinfo, 'n) inout;
  case_line : (Ast0_cocci.case_line, 'n) inout;
  define_param : (Ast0_cocci.define_param, 'n) inout;
  string_fragment : (Ast0_cocci.string_fragment, 'n) inout;
  attribute : (Ast0_cocci.attr, 'n) inout;
  attr_arg : (Ast0_cocci.attr_arg, 'n) inout;
  top_level : (Ast0_cocci.top_level, 'n) inout;
  expression_dots : (Ast0_cocci.expression Ast0_cocci.dots, 'n) inout;
  statement_dots : (Ast0_cocci.statement Ast0_cocci.dots, 'n) inout;
  declaration_dots : (Ast0_cocci.declaration Ast0_cocci.dots, 'n) inout;
  field_dots : (Ast0_cocci.field Ast0_cocci.dots, 'n) inout;
  enum_decl_dots : (Ast0_cocci.enum_decl Ast0_cocci.dots, 'n) inout;
  case_line_dots : (Ast0_cocci.case_line Ast0_cocci.dots, 'n) inout;
  define_param_dots : (Ast0_cocci.define_param Ast0_cocci.dots, 'n) inout;
  anything : (Ast0_cocci.anything, 'n) inout;
}
type ('a, 'n) combiner_inout = 'a -> 'n
type 'n combiner_rec_functions = {
  combiner_rec_meta_mcode : (Ast_cocci.meta_name Ast0_cocci.mcode, 'n) combiner_inout;
  combiner_rec_ident : (Ast0_cocci.ident, 'n) combiner_inout;
  combiner_rec_expression : (Ast0_cocci.expression, 'n) combiner_inout;
  combiner_rec_assignOp : (Ast0_cocci.assignOp, 'n) combiner_inout;
  combiner_rec_binaryOp : (Ast0_cocci.binaryOp, 'n) combiner_inout;
  combiner_rec_typeC : (Ast0_cocci.typeC, 'n) combiner_inout;
  combiner_rec_declaration : (Ast0_cocci.declaration, 'n) combiner_inout;
  combiner_rec_field : (Ast0_cocci.field, 'n) combiner_inout;
  combiner_rec_enumdecl : (Ast0_cocci.enum_decl,'n) combiner_inout;
  combiner_rec_initialiser : (Ast0_cocci.initialiser, 'n) combiner_inout;
  combiner_rec_initialiser_list : (Ast0_cocci.initialiser_list, 'n) combiner_inout;
  combiner_rec_parameter : (Ast0_cocci.parameterTypeDef, 'n) combiner_inout;
  combiner_rec_parameter_list : (Ast0_cocci.parameter_list, 'n) combiner_inout;
  combiner_rec_statement : (Ast0_cocci.statement, 'n) combiner_inout;
  combiner_rec_forinfo : (Ast0_cocci.forinfo, 'n) combiner_inout;
  combiner_rec_case_line : (Ast0_cocci.case_line, 'n) combiner_inout;
  combiner_rec_define_param : (Ast0_cocci.define_param, 'n) combiner_inout;
  combiner_rec_string_fragment : (Ast0_cocci.string_fragment, 'n) combiner_inout;
  combiner_rec_attribute : (Ast0_cocci.attr, 'n) combiner_inout;
  combiner_rec_attr_arg : (Ast0_cocci.attr_arg, 'n) combiner_inout;
  combiner_rec_top_level : (Ast0_cocci.top_level, 'n) combiner_inout;
  combiner_rec_expression_dots :
    (Ast0_cocci.expression Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_statement_dots : (Ast0_cocci.statement Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_declaration_dots :
    (Ast0_cocci.declaration Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_field_dots : (Ast0_cocci.field Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_enum_decl_dots : (Ast0_cocci.enum_decl Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_case_line_dots : (Ast0_cocci.case_line Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_define_param_dots :
    (Ast0_cocci.define_param Ast0_cocci.dots, 'n) combiner_inout;
  combiner_rec_anything : (Ast0_cocci.anything, 'n) combiner_inout;
}
type ('mc, 'n) cmcode = 'n -> 'mc Ast0_cocci.mcode -> 'n
type ('mc, 'n) flat_cmcode = 'mc Ast0_cocci.mcode -> 'n
type ('cd, 'n) ccode = 'n combiner_rec_functions -> ('cd -> 'n) -> 'cd -> 'n
type 'n combiner_functions = {
  combiner_meta_mcode : (Ast_cocci.meta_name, 'n) cmcode;
  combiner_string_mcode : (string, 'n) cmcode;
  combiner_const_mcode : (Ast_cocci.constant, 'n) cmcode;
  combiner_simpleAssign_mcode : (Ast0_cocci.simpleAssignOp, 'n) cmcode;
  combiner_opAssign_mcode : (Ast_cocci.arithOp, 'n) cmcode;
  combiner_fix_mcode : (Ast_cocci.fixOp, 'n) cmcode;
  combiner_unary_mcode : (Ast_cocci.unaryOp, 'n) cmcode;
  combiner_arithOp_mcode : (Ast_cocci.arithOp, 'n) cmcode;
  combiner_logicalOp_mcode : (Ast_cocci.logicalOp, 'n) cmcode;
  combiner_cv_mcode : (Ast_cocci.const_vol, 'n) cmcode;
  combiner_sign_mcode : (Ast_cocci.sign, 'n) cmcode;
  combiner_struct_mcode : (Ast_cocci.structUnion, 'n) cmcode;
  combiner_storage_mcode : (Ast_cocci.storage, 'n) cmcode;
  combiner_inc_mcode : (Ast_cocci.inc_file, 'n) cmcode;
  combiner_dotsexprfn : (Ast0_cocci.expression Ast0_cocci.dots, 'n) ccode;
  combiner_dotsinitfn : (Ast0_cocci.initialiser Ast0_cocci.dots, 'n) ccode;
  combiner_dotsparamfn : (Ast0_cocci.parameterTypeDef Ast0_cocci.dots, 'n) ccode;
  combiner_dotsstmtfn : (Ast0_cocci.statement Ast0_cocci.dots, 'n) ccode;
  combiner_dotsdeclfn : (Ast0_cocci.declaration Ast0_cocci.dots, 'n) ccode;
  combiner_dotsfieldfn : (Ast0_cocci.field Ast0_cocci.dots, 'n) ccode;
  combiner_dotsenumdeclfn : (Ast0_cocci.enum_decl Ast0_cocci.dots, 'n) ccode;
  combiner_dotscasefn : (Ast0_cocci.case_line Ast0_cocci.dots, 'n) ccode;
  combiner_dotsdefparfn : (Ast0_cocci.define_param Ast0_cocci.dots, 'n) ccode;
  combiner_identfn : (Ast0_cocci.ident, 'n) ccode;
  combiner_exprfn : (Ast0_cocci.expression, 'n) ccode;
  combiner_assignOpfn : (Ast0_cocci.assignOp, 'n) ccode;
  combiner_binaryOpfn : (Ast0_cocci.binaryOp, 'n) ccode;
  combiner_tyfn : (Ast0_cocci.typeC, 'n) ccode;
  combiner_initfn : (Ast0_cocci.initialiser, 'n) ccode;
  combiner_paramfn : (Ast0_cocci.parameterTypeDef, 'n) ccode;
  combiner_declfn : (Ast0_cocci.declaration, 'n) ccode;
  combiner_fieldfn : (Ast0_cocci.field, 'n) ccode;
  combiner_enumdeclfn : (Ast0_cocci.enum_decl,'n) ccode;
  combiner_stmtfn : (Ast0_cocci.statement, 'n) ccode;
  combiner_forinfofn : (Ast0_cocci.forinfo, 'n) ccode;
  combiner_casefn : (Ast0_cocci.case_line, 'n) ccode;
  combiner_string_fragmentfn : (Ast0_cocci.string_fragment, 'n) ccode;
  combiner_attributefn : (Ast0_cocci.attr, 'n) ccode;
  combiner_attr_argfn : (Ast0_cocci.attr_arg, 'n) ccode;
  combiner_topfn : (Ast0_cocci.top_level, 'n) ccode;
}
type 'a rebuilder_inout = 'a -> 'a
type rebuilder_rec_functions = {
  rebuilder_rec_meta_mcode : Ast_cocci.meta_name Ast0_cocci.mcode rebuilder_inout;
  rebuilder_rec_ident : Ast0_cocci.ident rebuilder_inout;
  rebuilder_rec_expression : Ast0_cocci.expression rebuilder_inout;
  rebuilder_rec_assignOp : Ast0_cocci.assignOp rebuilder_inout;
  rebuilder_rec_binaryOp : Ast0_cocci.binaryOp rebuilder_inout;
  rebuilder_rec_typeC : Ast0_cocci.typeC rebuilder_inout;
  rebuilder_rec_declaration : Ast0_cocci.declaration rebuilder_inout;
  rebuilder_rec_field : Ast0_cocci.field rebuilder_inout;
  rebuilder_rec_enumdecl : Ast0_cocci.enum_decl rebuilder_inout;
  rebuilder_rec_initialiser : Ast0_cocci.initialiser rebuilder_inout;
  rebuilder_rec_initialiser_list : Ast0_cocci.initialiser_list rebuilder_inout;
  rebuilder_rec_parameter : Ast0_cocci.parameterTypeDef rebuilder_inout;
  rebuilder_rec_parameter_list : Ast0_cocci.parameter_list rebuilder_inout;
  rebuilder_rec_statement : Ast0_cocci.statement rebuilder_inout;
  rebuilder_rec_forinfo : Ast0_cocci.forinfo rebuilder_inout;
  rebuilder_rec_case_line : Ast0_cocci.case_line rebuilder_inout;
  rebuilder_rec_string_fragment : Ast0_cocci.string_fragment rebuilder_inout;
  rebuilder_rec_attribute : Ast0_cocci.attr rebuilder_inout;
  rebuilder_rec_attr_arg : Ast0_cocci.attr_arg rebuilder_inout;
  rebuilder_rec_top_level : Ast0_cocci.top_level rebuilder_inout;
  rebuilder_rec_expression_dots : Ast0_cocci.expression Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_statement_dots : Ast0_cocci.statement Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_declaration_dots : Ast0_cocci.declaration Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_field_dots : Ast0_cocci.field Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_enum_decl_dots : Ast0_cocci.enum_decl Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_case_line_dots : Ast0_cocci.case_line Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_define_param_dots :
    Ast0_cocci.define_param Ast0_cocci.dots rebuilder_inout;
  rebuilder_rec_anything : Ast0_cocci.anything rebuilder_inout;
}
type 'mc rmcode = 'mc Ast0_cocci.mcode rebuilder_inout
type 'cd rcode =
    rebuilder_rec_functions -> 'cd rebuilder_inout -> 'cd rebuilder_inout
type rebuilder_functions = {
  rebuilder_meta_mcode : Ast_cocci.meta_name rmcode;
  rebuilder_string_mcode : string rmcode;
  rebuilder_const_mcode : Ast_cocci.constant rmcode;
  rebuilder_simpleAssign_mcode : Ast0_cocci.simpleAssignOp rmcode;
  rebuilder_opAssign_mcode : Ast_cocci.arithOp rmcode;
  rebuilder_fix_mcode : Ast_cocci.fixOp rmcode;
  rebuilder_unary_mcode : Ast_cocci.unaryOp rmcode;
  rebuilder_arithOp_mcode : Ast_cocci.arithOp rmcode;
  rebuilder_logicalOp_mcode : Ast_cocci.logicalOp rmcode;
  rebuilder_cv_mcode : Ast_cocci.const_vol rmcode;
  rebuilder_sign_mcode : Ast_cocci.sign rmcode;
  rebuilder_struct_mcode : Ast_cocci.structUnion rmcode;
  rebuilder_storage_mcode : Ast_cocci.storage rmcode;
  rebuilder_inc_mcode : Ast_cocci.inc_file rmcode;
  rebuilder_dotsexprfn : Ast0_cocci.expression Ast0_cocci.dots rcode;
  rebuilder_dotsinitfn : Ast0_cocci.initialiser Ast0_cocci.dots rcode;
  rebuilder_dotsparamfn : Ast0_cocci.parameterTypeDef Ast0_cocci.dots rcode;
  rebuilder_dotsstmtfn : Ast0_cocci.statement Ast0_cocci.dots rcode;
  rebuilder_dotsdeclfn : Ast0_cocci.declaration Ast0_cocci.dots rcode;
  rebuilder_dotsfieldfn : Ast0_cocci.field Ast0_cocci.dots rcode;
  rebuilder_dotsenumdeclfn : Ast0_cocci.enum_decl Ast0_cocci.dots rcode;
  rebuilder_dotscasefn : Ast0_cocci.case_line Ast0_cocci.dots rcode;
  rebuilder_dotsdefparfn : Ast0_cocci.define_param Ast0_cocci.dots rcode;
  rebuilder_identfn : Ast0_cocci.ident rcode;
  rebuilder_exprfn : Ast0_cocci.expression rcode;
  rebuilder_assignOpfn : Ast0_cocci.assignOp rcode;
  rebuilder_binaryOpfn : Ast0_cocci.binaryOp rcode;
  rebuilder_tyfn : Ast0_cocci.typeC rcode;
  rebuilder_initfn : Ast0_cocci.initialiser rcode;
  rebuilder_paramfn : Ast0_cocci.parameterTypeDef rcode;
  rebuilder_declfn : Ast0_cocci.declaration rcode;
  rebuilder_fieldfn : Ast0_cocci.field rcode;
  rebuilder_enumdeclfn : Ast0_cocci.enum_decl rcode;
  rebuilder_stmtfn : Ast0_cocci.statement rcode;
  rebuilder_forinfofn : Ast0_cocci.forinfo rcode;
  rebuilder_casefn : Ast0_cocci.case_line rcode;
  rebuilder_string_fragmentfn : Ast0_cocci.string_fragment rcode;
  rebuilder_attributefn : Ast0_cocci.attr rcode;
  rebuilder_attr_argfn : Ast0_cocci.attr_arg rcode;
  rebuilder_topfn : Ast0_cocci.top_level rcode;
}
type ('mc, 'a) rcmcode = 'a -> 'mc Ast0_cocci.mcode -> 'a * 'mc Ast0_cocci.mcode
type ('cd, 'a) rccode =
    'a all_functions -> ('cd -> 'a * 'cd) -> 'cd -> 'a * 'cd
type 'n combiner_rebuilder_functions = {
  combiner_rebuilder_meta_mcode : (Ast_cocci.meta_name, 'n) rcmcode;
  combiner_rebuilder_string_mcode : (string, 'n) rcmcode;
  combiner_rebuilder_const_mcode : (Ast_cocci.constant, 'n) rcmcode;
  combiner_rebuilder_simpleAssign_mcode : (Ast0_cocci.simpleAssignOp, 'n) rcmcode;
  combiner_rebuilder_opAssign_mcode : (Ast_cocci.arithOp, 'n) rcmcode;
  combiner_rebuilder_fix_mcode : (Ast_cocci.fixOp, 'n) rcmcode;
  combiner_rebuilder_unary_mcode : (Ast_cocci.unaryOp, 'n) rcmcode;
  combiner_rebuilder_arithOp_mcode : (Ast_cocci.arithOp, 'n) rcmcode;
  combiner_rebuilder_logicalOp_mcode : (Ast_cocci.logicalOp, 'n) rcmcode;
  combiner_rebuilder_cv_mcode : (Ast_cocci.const_vol, 'n) rcmcode;
  combiner_rebuilder_sign_mcode : (Ast_cocci.sign, 'n) rcmcode;
  combiner_rebuilder_struct_mcode : (Ast_cocci.structUnion, 'n) rcmcode;
  combiner_rebuilder_storage_mcode : (Ast_cocci.storage, 'n) rcmcode;
  combiner_rebuilder_inc_mcode : (Ast_cocci.inc_file, 'n) rcmcode;
  combiner_rebuilder_dotsexprfn : (Ast0_cocci.expression Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsinitfn : (Ast0_cocci.initialiser Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsparamfn :
    (Ast0_cocci.parameterTypeDef Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsstmtfn : (Ast0_cocci.statement Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsdeclfn : (Ast0_cocci.declaration Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsfieldfn : (Ast0_cocci.field Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsenumdeclfn : (Ast0_cocci.enum_decl Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotscasefn : (Ast0_cocci.case_line Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_dotsdefparfn : (Ast0_cocci.define_param Ast0_cocci.dots, 'n) rccode;
  combiner_rebuilder_identfn : (Ast0_cocci.ident, 'n) rccode;
  combiner_rebuilder_exprfn : (Ast0_cocci.expression, 'n) rccode;
  combiner_rebuilder_assignOpfn : (Ast0_cocci.assignOp, 'n) rccode;
  combiner_rebuilder_binaryOpfn : (Ast0_cocci.binaryOp, 'n) rccode;
  combiner_rebuilder_tyfn : (Ast0_cocci.typeC, 'n) rccode;
  combiner_rebuilder_initfn : (Ast0_cocci.initialiser, 'n) rccode;
  combiner_rebuilder_paramfn : (Ast0_cocci.parameterTypeDef, 'n) rccode;
  combiner_rebuilder_declfn : (Ast0_cocci.declaration, 'n) rccode;
  combiner_rebuilder_fieldfn : (Ast0_cocci.field, 'n) rccode;
  combiner_rebuilder_enumdeclfn : (Ast0_cocci.enum_decl,'n) rccode;
  combiner_rebuilder_stmtfn : (Ast0_cocci.statement, 'n) rccode;
  combiner_rebuilder_forinfofn : (Ast0_cocci.forinfo, 'n) rccode;
  combiner_rebuilder_casefn : (Ast0_cocci.case_line, 'n) rccode;
  combiner_rebuilder_string_fragmentfn : (Ast0_cocci.string_fragment, 'n) rccode;
  combiner_rebuilder_attributefn : (Ast0_cocci.attr, 'n) rccode;
  combiner_rebuilder_attr_argfn : (Ast0_cocci.attr_arg, 'n) rccode;
  combiner_rebuilder_topfn : (Ast0_cocci.top_level, 'n) rccode;
}
