(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

type ('a,'n) inout = 'a -> ('n * 'a)

type 'n all_functions =
    {meta_mcode : (Ast.meta_name Ast0.mcode,'n) inout;
      ident : (Ast0.ident,'n) inout;
      expression : (Ast0.expression,'n) inout;
      assignOp : (Ast0.assignOp,'n) inout;
      binaryOp : (Ast0.binaryOp,'n) inout;
      typeC : (Ast0.typeC,'n) inout;
      declaration : (Ast0.declaration,'n) inout;
      field : (Ast0.field,'n) inout;
      enum_decl : (Ast0.enum_decl,'n) inout;
      initialiser : (Ast0.initialiser,'n) inout;
      initialiser_list : (Ast0.initialiser_list,'n) inout;
      parameter : (Ast0.parameterTypeDef,'n) inout;
      parameter_list : (Ast0.parameter_list,'n) inout;
      statement : (Ast0.statement,'n) inout;
      forinfo : (Ast0.forinfo,'n) inout;
      case_line : (Ast0.case_line,'n) inout;
      define_param : (Ast0.define_param,'n) inout;
      string_fragment : (Ast0.string_fragment,'n) inout;
      attribute : (Ast0.attr,'n) inout;
      attr_arg : (Ast0.attr_arg,'n) inout;
      top_level : (Ast0.top_level,'n) inout;
      expression_dots : (Ast0.expression Ast0.dots,'n) inout;
      statement_dots : (Ast0.statement Ast0.dots,'n) inout;
      declaration_dots : (Ast0.declaration Ast0.dots,'n) inout;
      field_dots : (Ast0.field Ast0.dots,'n) inout;
      enum_decl_dots : (Ast0.enum_decl Ast0.dots,'n) inout;
      case_line_dots : (Ast0.case_line Ast0.dots,'n) inout;
      define_param_dots : (Ast0.define_param Ast0.dots,'n) inout;
      anything : (Ast0.anything,'n) inout}

(* ----------------------------------------------------------------------- *)
(* combiner *)

type ('a,'n) combiner_inout = 'a -> 'n

type 'n combiner_rec_functions =
    {combiner_rec_meta_mcode : (Ast.meta_name Ast0.mcode,'n) combiner_inout;
      combiner_rec_ident : (Ast0.ident,'n) combiner_inout;
      combiner_rec_expression : (Ast0.expression,'n) combiner_inout;
      combiner_rec_assignOp : (Ast0.assignOp,'n) combiner_inout;
      combiner_rec_binaryOp : (Ast0.binaryOp,'n) combiner_inout;
      combiner_rec_typeC : (Ast0.typeC,'n) combiner_inout;
      combiner_rec_declaration : (Ast0.declaration,'n) combiner_inout;
      combiner_rec_field : (Ast0.field,'n) combiner_inout;
      combiner_rec_enumdecl : (Ast0.enum_decl,'n) combiner_inout;
      combiner_rec_initialiser : (Ast0.initialiser,'n) combiner_inout;
      combiner_rec_initialiser_list :
	(Ast0.initialiser_list,'n) combiner_inout;
      combiner_rec_parameter : (Ast0.parameterTypeDef,'n) combiner_inout;
      combiner_rec_parameter_list : (Ast0.parameter_list,'n) combiner_inout;
      combiner_rec_statement : (Ast0.statement,'n) combiner_inout;
      combiner_rec_forinfo   : (Ast0.forinfo,'n) combiner_inout;
      combiner_rec_case_line : (Ast0.case_line,'n) combiner_inout;
      combiner_rec_define_param : (Ast0.define_param,'n) combiner_inout;
      combiner_rec_string_fragment : (Ast0.string_fragment,'n) combiner_inout;
      combiner_rec_attribute : (Ast0.attr,'n) combiner_inout;
      combiner_rec_attr_arg : (Ast0.attr_arg,'n) combiner_inout;
      combiner_rec_top_level : (Ast0.top_level,'n) combiner_inout;
      combiner_rec_expression_dots :
	(Ast0.expression Ast0.dots,'n) combiner_inout;
      combiner_rec_statement_dots :
	(Ast0.statement Ast0.dots,'n) combiner_inout;
      combiner_rec_declaration_dots :
	(Ast0.declaration Ast0.dots,'n) combiner_inout;
      combiner_rec_field_dots :
	(Ast0.field Ast0.dots,'n) combiner_inout;
      combiner_rec_enum_decl_dots :
	(Ast0.enum_decl Ast0.dots,'n) combiner_inout;
      combiner_rec_case_line_dots :
	(Ast0.case_line Ast0.dots,'n) combiner_inout;
      combiner_rec_define_param_dots :
	(Ast0.define_param Ast0.dots,'n) combiner_inout;
      combiner_rec_anything : (Ast0.anything,'n) combiner_inout}

type ('mc,'n) cmcode = 'n -> 'mc Ast0.mcode -> 'n
type ('mc,'n) flat_cmcode = 'mc Ast0.mcode -> 'n
type ('cd,'n) ccode = 'n combiner_rec_functions -> ('cd -> 'n) -> 'cd -> 'n

type 'n combiner_functions =
  {combiner_meta_mcode : (Ast.meta_name,'n) cmcode;
   combiner_string_mcode : (string,'n) cmcode;
   combiner_const_mcode : (Ast.constant,'n) cmcode;
   combiner_simpleAssign_mcode : (Ast0.simpleAssignOp,'n) cmcode;
   combiner_opAssign_mcode : (Ast.arithOp,'n) cmcode;
   combiner_fix_mcode : (Ast.fixOp,'n) cmcode;
   combiner_unary_mcode : (Ast.unaryOp,'n) cmcode;
   combiner_arithOp_mcode : (Ast.arithOp,'n) cmcode;
   combiner_logicalOp_mcode : (Ast.logicalOp,'n) cmcode;
   combiner_cv_mcode : (Ast.const_vol,'n) cmcode;
   combiner_sign_mcode : (Ast.sign,'n) cmcode;
   combiner_struct_mcode : (Ast.structUnion,'n) cmcode;
   combiner_storage_mcode : (Ast.storage,'n) cmcode;
   combiner_inc_mcode : (Ast.inc_file,'n) cmcode;
   combiner_dotsexprfn : (Ast0.expression Ast0.dots,'n) ccode;
   combiner_dotsinitfn : (Ast0.initialiser Ast0.dots,'n) ccode;
   combiner_dotsparamfn : (Ast0.parameterTypeDef Ast0.dots,'n) ccode;
   combiner_dotsstmtfn : (Ast0.statement Ast0.dots,'n) ccode;
   combiner_dotsdeclfn : (Ast0.declaration Ast0.dots,'n) ccode;
   combiner_dotsfieldfn : (Ast0.field Ast0.dots,'n) ccode;
   combiner_dotsenumdeclfn : (Ast0.enum_decl Ast0.dots,'n) ccode;
   combiner_dotscasefn : (Ast0.case_line Ast0.dots,'n) ccode;
   combiner_dotsdefparfn : (Ast0.define_param Ast0.dots,'n) ccode;
   combiner_identfn : (Ast0.ident,'n) ccode;
   combiner_exprfn : (Ast0.expression,'n) ccode;
   combiner_assignOpfn : (Ast0.assignOp,'n) ccode;
   combiner_binaryOpfn : (Ast0.binaryOp,'n) ccode;
   combiner_tyfn : (Ast0.typeC,'n) ccode;
   combiner_initfn : (Ast0.initialiser,'n) ccode;
   combiner_paramfn : (Ast0.parameterTypeDef,'n) ccode;
   combiner_declfn : (Ast0.declaration,'n) ccode;
   combiner_fieldfn : (Ast0.field,'n) ccode;
   combiner_enumdeclfn : (Ast0.enum_decl,'n) ccode;
   combiner_stmtfn : (Ast0.statement,'n) ccode;
   combiner_forinfofn : (Ast0.forinfo,'n) ccode;
   combiner_casefn : (Ast0.case_line,'n) ccode;
   combiner_string_fragmentfn : (Ast0.string_fragment,'n) ccode;
   combiner_attributefn : (Ast0.attr,'n) ccode;
   combiner_attr_argfn : (Ast0.attr_arg,'n) ccode;
   combiner_topfn : (Ast0.top_level,'n) ccode}

(* ----------------------------------------------------------------------- *)
(* rebuilder *)

type 'a rebuilder_inout = 'a -> 'a

type rebuilder_rec_functions =
    {rebuilder_rec_meta_mcode : Ast.meta_name Ast0.mcode rebuilder_inout;
      rebuilder_rec_ident : Ast0.ident rebuilder_inout;
      rebuilder_rec_expression : Ast0.expression rebuilder_inout;
      rebuilder_rec_assignOp : Ast0.assignOp rebuilder_inout;
      rebuilder_rec_binaryOp : Ast0.binaryOp rebuilder_inout;
      rebuilder_rec_typeC : Ast0.typeC rebuilder_inout;
      rebuilder_rec_declaration : Ast0.declaration rebuilder_inout;
      rebuilder_rec_field : Ast0.field rebuilder_inout;
      rebuilder_rec_enumdecl : Ast0.enum_decl rebuilder_inout;
      rebuilder_rec_initialiser : Ast0.initialiser rebuilder_inout;
      rebuilder_rec_initialiser_list :
	Ast0.initialiser_list rebuilder_inout;
      rebuilder_rec_parameter : Ast0.parameterTypeDef rebuilder_inout;
      rebuilder_rec_parameter_list : Ast0.parameter_list rebuilder_inout;
      rebuilder_rec_statement : Ast0.statement rebuilder_inout;
      rebuilder_rec_forinfo : Ast0.forinfo rebuilder_inout;
      rebuilder_rec_case_line : Ast0.case_line rebuilder_inout;
      rebuilder_rec_string_fragment : Ast0.string_fragment rebuilder_inout;
      rebuilder_rec_attribute : Ast0.attr rebuilder_inout;
      rebuilder_rec_attr_arg : Ast0.attr_arg rebuilder_inout;
      rebuilder_rec_top_level : Ast0.top_level rebuilder_inout;
      rebuilder_rec_expression_dots :
	Ast0.expression Ast0.dots rebuilder_inout;
      rebuilder_rec_statement_dots :
	Ast0.statement Ast0.dots rebuilder_inout;
      rebuilder_rec_declaration_dots :
	Ast0.declaration Ast0.dots rebuilder_inout;
      rebuilder_rec_field_dots :
	Ast0.field Ast0.dots rebuilder_inout;
      rebuilder_rec_enum_decl_dots :
	Ast0.enum_decl Ast0.dots rebuilder_inout;
      rebuilder_rec_case_line_dots :
	Ast0.case_line Ast0.dots rebuilder_inout;
      rebuilder_rec_define_param_dots :
	Ast0.define_param Ast0.dots rebuilder_inout;
      rebuilder_rec_anything : Ast0.anything rebuilder_inout}

type 'mc rmcode = 'mc Ast0.mcode rebuilder_inout
type 'cd rcode =
    rebuilder_rec_functions -> ('cd rebuilder_inout) -> 'cd rebuilder_inout

type rebuilder_functions =
  {rebuilder_meta_mcode : Ast_cocci.meta_name rmcode;
   rebuilder_string_mcode : string rmcode;
   rebuilder_const_mcode : Ast.constant rmcode;
   rebuilder_simpleAssign_mcode : Ast0.simpleAssignOp rmcode;
   rebuilder_opAssign_mcode : Ast.arithOp rmcode;
   rebuilder_fix_mcode : Ast.fixOp rmcode;
   rebuilder_unary_mcode : Ast.unaryOp rmcode;
   rebuilder_arithOp_mcode : Ast.arithOp rmcode;
   rebuilder_logicalOp_mcode : Ast.logicalOp rmcode;
   rebuilder_cv_mcode : Ast.const_vol rmcode;
   rebuilder_sign_mcode : Ast.sign rmcode;
   rebuilder_struct_mcode : Ast.structUnion rmcode;
   rebuilder_storage_mcode : Ast.storage rmcode;
   rebuilder_inc_mcode : Ast.inc_file rmcode;
   rebuilder_dotsexprfn : Ast0.expression Ast0.dots rcode;
   rebuilder_dotsinitfn : Ast0.initialiser Ast0.dots rcode;
   rebuilder_dotsparamfn : Ast0.parameterTypeDef Ast0.dots rcode;
   rebuilder_dotsstmtfn : Ast0.statement Ast0.dots rcode;
   rebuilder_dotsdeclfn : Ast0.declaration Ast0.dots rcode;
   rebuilder_dotsfieldfn : Ast0.field Ast0.dots rcode;
   rebuilder_dotsenumdeclfn : Ast0.enum_decl Ast0.dots rcode;
   rebuilder_dotscasefn : Ast0.case_line Ast0.dots rcode;
   rebuilder_dotsdefparfn : Ast0.define_param Ast0.dots rcode;
   rebuilder_identfn : Ast0.ident rcode;
   rebuilder_exprfn : Ast0.expression rcode;
   rebuilder_assignOpfn : Ast0.assignOp rcode;
   rebuilder_binaryOpfn : Ast0.binaryOp rcode;

   rebuilder_tyfn : Ast0.typeC rcode;
   rebuilder_initfn : Ast0.initialiser rcode;
   rebuilder_paramfn : Ast0.parameterTypeDef rcode;
   rebuilder_declfn : Ast0.declaration rcode;
   rebuilder_fieldfn : Ast0.field rcode;
   rebuilder_enumdeclfn : Ast0.enum_decl rcode;
   rebuilder_stmtfn : Ast0.statement rcode;
   rebuilder_forinfofn : Ast0.forinfo rcode;
   rebuilder_casefn : Ast0.case_line rcode;
   rebuilder_string_fragmentfn : Ast0.string_fragment rcode;
   rebuilder_attributefn : Ast0.attr rcode;
   rebuilder_attr_argfn : Ast0.attr_arg rcode;
   rebuilder_topfn : Ast0.top_level rcode}

(* ----------------------------------------------------------------------- *)
(* combiner_rebuilder *)

type ('mc,'a) rcmcode = 'a -> 'mc Ast0.mcode -> ('a * 'mc Ast0.mcode)
type ('cd,'a) rccode =
    'a all_functions -> ('cd -> ('a * 'cd)) -> 'cd -> ('a * 'cd)

type 'n combiner_rebuilder_functions =
  {combiner_rebuilder_meta_mcode : (Ast_cocci.meta_name,'n) rcmcode;
   combiner_rebuilder_string_mcode : (string,'n) rcmcode;
   combiner_rebuilder_const_mcode : (Ast.constant,'n) rcmcode;
   combiner_rebuilder_simpleAssign_mcode : (Ast0.simpleAssignOp,'n) rcmcode;
   combiner_rebuilder_opAssign_mcode : (Ast.arithOp,'n) rcmcode;
   combiner_rebuilder_fix_mcode : (Ast.fixOp,'n) rcmcode;
   combiner_rebuilder_unary_mcode : (Ast.unaryOp,'n) rcmcode;
   combiner_rebuilder_arithOp_mcode : (Ast.arithOp,'n) rcmcode;
   combiner_rebuilder_logicalOp_mcode : (Ast.logicalOp,'n) rcmcode;
   combiner_rebuilder_cv_mcode : (Ast.const_vol,'n) rcmcode;
   combiner_rebuilder_sign_mcode : (Ast.sign,'n) rcmcode;
   combiner_rebuilder_struct_mcode : (Ast.structUnion,'n) rcmcode;
   combiner_rebuilder_storage_mcode : (Ast.storage,'n) rcmcode;
   combiner_rebuilder_inc_mcode : (Ast.inc_file,'n) rcmcode;
   combiner_rebuilder_dotsexprfn : (Ast0.expression Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsinitfn : (Ast0.initialiser Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsparamfn :
      (Ast0.parameterTypeDef Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsstmtfn : (Ast0.statement Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsdeclfn : (Ast0.declaration Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsfieldfn : (Ast0.field Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsenumdeclfn : (Ast0.enum_decl Ast0.dots,'n) rccode;
   combiner_rebuilder_dotscasefn : (Ast0.case_line Ast0.dots,'n) rccode;
   combiner_rebuilder_dotsdefparfn : (Ast0.define_param Ast0.dots,'n) rccode;
   combiner_rebuilder_identfn : (Ast0.ident,'n) rccode;
   combiner_rebuilder_exprfn : (Ast0.expression,'n) rccode;
   combiner_rebuilder_assignOpfn : (Ast0.assignOp,'n) rccode;
   combiner_rebuilder_binaryOpfn : (Ast0.binaryOp,'n) rccode;
   combiner_rebuilder_tyfn : (Ast0.typeC,'n) rccode;
   combiner_rebuilder_initfn : (Ast0.initialiser,'n) rccode;
   combiner_rebuilder_paramfn : (Ast0.parameterTypeDef,'n) rccode;
   combiner_rebuilder_declfn : (Ast0.declaration,'n) rccode;
   combiner_rebuilder_fieldfn : (Ast0.field,'n) rccode;
   combiner_rebuilder_enumdeclfn : (Ast0.enum_decl,'n) rccode;
   combiner_rebuilder_stmtfn : (Ast0.statement,'n) rccode;
   combiner_rebuilder_forinfofn : (Ast0.forinfo,'n) rccode;
   combiner_rebuilder_casefn : (Ast0.case_line,'n) rccode;
   combiner_rebuilder_string_fragmentfn : (Ast0.string_fragment,'n) rccode;
   combiner_rebuilder_attributefn : (Ast0.attr,'n) rccode;
   combiner_rebuilder_attr_argfn : (Ast0.attr_arg,'n) rccode;
   combiner_rebuilder_topfn : (Ast0.top_level,'n) rccode}
