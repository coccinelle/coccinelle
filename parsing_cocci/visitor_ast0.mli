(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type 'b cmcodefn = { cmcode: 'a. 'a Ast0_cocci.mcode -> 'b }
type 'b cdonothingfn =
    { cdonothing: 'a. 'b Visitor_ast0_types.combiner_rec_functions ->
      ('a Ast0_cocci.wrap -> 'b) -> 'a Ast0_cocci.wrap -> 'b }

val combiner :
    ('a -> 'a -> 'a) -> 'a -> 'a cmcodefn -> 'a cdonothingfn ->
    ?meta_mcode:((Ast_cocci.meta_name,'a) Visitor_ast0_types.flat_cmcode) ->
    ?string_mcode:((string,'a) Visitor_ast0_types.flat_cmcode) ->
    ?const_mcode:((Ast_cocci.constant,'a) Visitor_ast0_types.flat_cmcode) ->
    ?simpleAssign_mcode:((Ast0_cocci.simpleAssignOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?opAssign_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?fixOp_mcode:((Ast_cocci.fixOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?unaryOp_mcode:((Ast_cocci.unaryOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?arithOp_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?logicalOp_mcode:((Ast_cocci.logicalOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?cv_mcode:((Ast_cocci.const_vol,'a) Visitor_ast0_types.flat_cmcode) ->
    ?sign_mcode:((Ast_cocci.sign,'a) Visitor_ast0_types.flat_cmcode) ->
    ?struct_mcode:((Ast_cocci.structUnion,'a) Visitor_ast0_types.flat_cmcode) ->
    ?storage_mcode:((Ast_cocci.storage,'a) Visitor_ast0_types.flat_cmcode) ->
    ?inc_mcode:((Ast_cocci.inc_file,'a) Visitor_ast0_types.flat_cmcode) ->

    ?dotsexpr:((Ast0_cocci.expression Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsinit:((Ast0_cocci.initialiser Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsparam:((Ast0_cocci.parameterTypeDef Ast0_cocci.dots,'a)
       Visitor_ast0_types.ccode) ->
    ?dotstemplateparam:((Ast0_cocci.templateParameterTypeDef Ast0_cocci.dots,'a)
       Visitor_ast0_types.ccode) ->
    ?dotsstmt:((Ast0_cocci.statement Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsdecl:((Ast0_cocci.declaration Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsfield:((Ast0_cocci.field Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsenumdecl:((Ast0_cocci.enum_decl Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotscase:((Ast0_cocci.case_line Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsdefpar:((Ast0_cocci.define_param Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?ident:((Ast0_cocci.ident,'a) Visitor_ast0_types.ccode) ->
    ?expr:((Ast0_cocci.expression,'a) Visitor_ast0_types.ccode) ->
    ?assignOp:((Ast0_cocci.assignOp,'a) Visitor_ast0_types.ccode) ->
    ?binaryOp:((Ast0_cocci.binaryOp,'a) Visitor_ast0_types.ccode) ->
    ?ty:((Ast0_cocci.typeC,'a) Visitor_ast0_types.ccode) ->
    ?init:((Ast0_cocci.initialiser,'a) Visitor_ast0_types.ccode) ->
    ?param:((Ast0_cocci.parameterTypeDef,'a) Visitor_ast0_types.ccode) ->
    ?template_param:((Ast0_cocci.templateParameterTypeDef,'a) Visitor_ast0_types.ccode) ->
    ?decl:((Ast0_cocci.declaration,'a) Visitor_ast0_types.ccode) ->
    ?field:((Ast0_cocci.field,'a) Visitor_ast0_types.ccode) ->
    ?enumdecl:((Ast0_cocci.enum_decl,'a) Visitor_ast0_types.ccode) ->
    ?stmt:((Ast0_cocci.statement,'a) Visitor_ast0_types.ccode) ->
    ?forinfo:((Ast0_cocci.forinfo,'a) Visitor_ast0_types.ccode) ->
    ?case:((Ast0_cocci.case_line,'a) Visitor_ast0_types.ccode) ->
    ?string_fragment:((Ast0_cocci.string_fragment,'a) Visitor_ast0_types.ccode) ->
    ?attribute:((Ast0_cocci.attr,'a) Visitor_ast0_types.ccode) ->
    ?attr_arg:((Ast0_cocci.attr_arg,'a) Visitor_ast0_types.ccode) ->
    ?top:((Ast0_cocci.top_level,'a) Visitor_ast0_types.ccode) -> unit ->
    'a Visitor_ast0_types.combiner_rec_functions

val combiner_default :
    ('a -> 'a -> 'a) -> 'a ->
    ?meta_mcode:((Ast_cocci.meta_name,'a) Visitor_ast0_types.flat_cmcode) ->
    ?string_mcode:((string,'a) Visitor_ast0_types.flat_cmcode) ->
    ?const_mcode:((Ast_cocci.constant,'a) Visitor_ast0_types.flat_cmcode) ->
    ?simpleAssign_mcode:((Ast0_cocci.simpleAssignOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?opAssign_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?fixOp_mcode:((Ast_cocci.fixOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?unaryOp_mcode:((Ast_cocci.unaryOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?arithOp_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?logicalOp_mcode:((Ast_cocci.logicalOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ?cv_mcode:((Ast_cocci.const_vol,'a) Visitor_ast0_types.flat_cmcode) ->
    ?sign_mcode:((Ast_cocci.sign,'a) Visitor_ast0_types.flat_cmcode) ->
    ?struct_mcode:((Ast_cocci.structUnion,'a) Visitor_ast0_types.flat_cmcode) ->
    ?storage_mcode:((Ast_cocci.storage,'a) Visitor_ast0_types.flat_cmcode) ->
    ?inc_mcode:((Ast_cocci.inc_file,'a) Visitor_ast0_types.flat_cmcode) ->

    ?dotsexpr:((Ast0_cocci.expression Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsinit:((Ast0_cocci.initialiser Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsparam:((Ast0_cocci.parameterTypeDef Ast0_cocci.dots,'a)
       Visitor_ast0_types.ccode) ->
    ?dotstemplateparam:((Ast0_cocci.templateParameterTypeDef Ast0_cocci.dots,'a)
       Visitor_ast0_types.ccode) ->
    ?dotsstmt:((Ast0_cocci.statement Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsdecl:((Ast0_cocci.declaration Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsfield:((Ast0_cocci.field Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsenumdecl:((Ast0_cocci.enum_decl Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotscase:((Ast0_cocci.case_line Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?dotsdefpar:((Ast0_cocci.define_param Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ?ident:((Ast0_cocci.ident,'a) Visitor_ast0_types.ccode) ->
    ?expr:((Ast0_cocci.expression,'a) Visitor_ast0_types.ccode) ->
    ?assignOp:((Ast0_cocci.assignOp,'a) Visitor_ast0_types.ccode) ->
    ?binaryOp:((Ast0_cocci.binaryOp,'a) Visitor_ast0_types.ccode) ->
    ?ty:((Ast0_cocci.typeC,'a) Visitor_ast0_types.ccode) ->
    ?init:((Ast0_cocci.initialiser,'a) Visitor_ast0_types.ccode) ->
    ?param:((Ast0_cocci.parameterTypeDef,'a) Visitor_ast0_types.ccode) ->
    ?template_param:((Ast0_cocci.templateParameterTypeDef,'a) Visitor_ast0_types.ccode) ->
    ?decl:((Ast0_cocci.declaration,'a) Visitor_ast0_types.ccode) ->
    ?field:((Ast0_cocci.field,'a) Visitor_ast0_types.ccode) ->
    ?enumdecl:((Ast0_cocci.enum_decl,'a) Visitor_ast0_types.ccode) ->
    ?stmt:((Ast0_cocci.statement,'a) Visitor_ast0_types.ccode) ->
    ?forinfo:((Ast0_cocci.forinfo,'a) Visitor_ast0_types.ccode) ->
    ?case:((Ast0_cocci.case_line,'a) Visitor_ast0_types.ccode) ->
    ?string_fragment:((Ast0_cocci.string_fragment,'a) Visitor_ast0_types.ccode) ->
    ?attribute:((Ast0_cocci.attr,'a) Visitor_ast0_types.ccode) ->
    ?attr_arg:((Ast0_cocci.attr_arg,'a) Visitor_ast0_types.ccode) ->
    ?top:((Ast0_cocci.top_level,'a) Visitor_ast0_types.ccode) -> unit ->
    'a Visitor_ast0_types.combiner_rec_functions


type rmcodefn = { rmcode: 'a. 'a Ast0_cocci.mcode -> 'a Ast0_cocci.mcode }
type rdonothingfn =
    { rdonothing: 'a. Visitor_ast0_types.rebuilder_rec_functions ->
      ('a Ast0_cocci.wrap -> 'a Ast0_cocci.wrap) -> 'a Ast0_cocci.wrap ->
	'a Ast0_cocci.wrap }

val rebuilder : rmcodefn -> rdonothingfn ->
    ?meta_mcode:(Ast_cocci.meta_name Visitor_ast0_types.rmcode) ->
    ?string_mcode:(string Visitor_ast0_types.rmcode) ->
    ?const_mcode:(Ast_cocci.constant Visitor_ast0_types.rmcode) ->
    ?simpleAssign_mcode:(Ast0_cocci.simpleAssignOp Visitor_ast0_types.rmcode) ->
    ?opAssign_mcode:(Ast_cocci.arithOp Visitor_ast0_types.rmcode) ->
    ?fixOp_mcode:(Ast_cocci.fixOp Visitor_ast0_types.rmcode) ->
    ?unaryOp_mcode:(Ast_cocci.unaryOp Visitor_ast0_types.rmcode) ->
    ?arithOp_mcode:(Ast_cocci.arithOp Visitor_ast0_types.rmcode) ->
    ?logicalOp_mcode:(Ast_cocci.logicalOp Visitor_ast0_types.rmcode) ->
    ?cv_mcode:(Ast_cocci.const_vol Visitor_ast0_types.rmcode) ->
    ?sign_mcode:(Ast_cocci.sign Visitor_ast0_types.rmcode) ->
    ?struct_mcode:(Ast_cocci.structUnion Visitor_ast0_types.rmcode) ->
    ?storage_mcode:(Ast_cocci.storage Visitor_ast0_types.rmcode) ->
    ?inc_mcode:(Ast_cocci.inc_file Visitor_ast0_types.rmcode) ->

    ?dotsexpr: (Ast0_cocci.expression Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsinit: (Ast0_cocci.initialiser Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsparam: (Ast0_cocci.parameterTypeDef Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotstemplateparam: (Ast0_cocci.templateParameterTypeDef Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsstmt: (Ast0_cocci.statement Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsdecl: (Ast0_cocci.declaration Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsfield: (Ast0_cocci.field Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsenumdecl: (Ast0_cocci.enum_decl Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotscase: (Ast0_cocci.case_line Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsdefpar: (Ast0_cocci.define_param Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?ident:(Ast0_cocci.ident Visitor_ast0_types.rcode) ->
    ?expr:(Ast0_cocci.expression Visitor_ast0_types.rcode) ->
    ?assignOp:(Ast0_cocci.assignOp Visitor_ast0_types.rcode) ->
    ?binaryOp:(Ast0_cocci.binaryOp Visitor_ast0_types.rcode) ->
    ?ty:(Ast0_cocci.typeC Visitor_ast0_types.rcode) ->
    ?init:(Ast0_cocci.initialiser Visitor_ast0_types.rcode) ->
    ?param:(Ast0_cocci.parameterTypeDef Visitor_ast0_types.rcode) ->
    ?template_param:(Ast0_cocci.templateParameterTypeDef Visitor_ast0_types.rcode) ->
    ?decl:(Ast0_cocci.declaration Visitor_ast0_types.rcode) ->
    ?field:(Ast0_cocci.field Visitor_ast0_types.rcode) ->
    ?enumdecl:(Ast0_cocci.enum_decl Visitor_ast0_types.rcode) ->
    ?stmt:(Ast0_cocci.statement Visitor_ast0_types.rcode) ->
    ?forinfo:(Ast0_cocci.forinfo Visitor_ast0_types.rcode) ->
    ?case:(Ast0_cocci.case_line Visitor_ast0_types.rcode) ->
    ?string_fragment:(Ast0_cocci.string_fragment Visitor_ast0_types.rcode) ->
    ?attribute:(Ast0_cocci.attr Visitor_ast0_types.rcode) ->
    ?attr_arg:(Ast0_cocci.attr_arg Visitor_ast0_types.rcode) ->
    ?top:(Ast0_cocci.top_level Visitor_ast0_types.rcode) -> unit ->
      Visitor_ast0_types.rebuilder_rec_functions

val rebuilder_default :
    ?meta_mcode:(Ast_cocci.meta_name Visitor_ast0_types.rmcode) ->
    ?string_mcode:(string Visitor_ast0_types.rmcode) ->
    ?const_mcode:(Ast_cocci.constant Visitor_ast0_types.rmcode) ->
    ?simpleAssign_mcode:(Ast0_cocci.simpleAssignOp Visitor_ast0_types.rmcode) ->
    ?opAssign_mcode:(Ast_cocci.arithOp Visitor_ast0_types.rmcode) ->
    ?fixOp_mcode:(Ast_cocci.fixOp Visitor_ast0_types.rmcode) ->
    ?unaryOp_mcode:(Ast_cocci.unaryOp Visitor_ast0_types.rmcode) ->
    ?arithOp_mcode:(Ast_cocci.arithOp Visitor_ast0_types.rmcode) ->
    ?logicalOp_mcode:(Ast_cocci.logicalOp Visitor_ast0_types.rmcode) ->
    ?cv_mcode:(Ast_cocci.const_vol Visitor_ast0_types.rmcode) ->
    ?sign_mcode:(Ast_cocci.sign Visitor_ast0_types.rmcode) ->
    ?struct_mcode:(Ast_cocci.structUnion Visitor_ast0_types.rmcode) ->
    ?storage_mcode:(Ast_cocci.storage Visitor_ast0_types.rmcode) ->
    ?inc_mcode:(Ast_cocci.inc_file Visitor_ast0_types.rmcode) ->

    ?dotsexpr: (Ast0_cocci.expression Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsinit: (Ast0_cocci.initialiser Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsparam: (Ast0_cocci.parameterTypeDef Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotstemplateparam: (Ast0_cocci.templateParameterTypeDef Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsstmt: (Ast0_cocci.statement Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsdecl: (Ast0_cocci.declaration Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsfield: (Ast0_cocci.field Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsenumdecl: (Ast0_cocci.enum_decl Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotscase: (Ast0_cocci.case_line Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?dotsdefpar: (Ast0_cocci.define_param Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    ?ident:(Ast0_cocci.ident Visitor_ast0_types.rcode) ->
    ?expr:(Ast0_cocci.expression Visitor_ast0_types.rcode) ->
    ?assignOp:(Ast0_cocci.assignOp Visitor_ast0_types.rcode) ->
    ?binaryOp:(Ast0_cocci.binaryOp Visitor_ast0_types.rcode) ->
    ?ty:(Ast0_cocci.typeC Visitor_ast0_types.rcode) ->
    ?init:(Ast0_cocci.initialiser Visitor_ast0_types.rcode) ->
    ?param:(Ast0_cocci.parameterTypeDef Visitor_ast0_types.rcode) ->
    ?template_param:(Ast0_cocci.templateParameterTypeDef Visitor_ast0_types.rcode) ->
    ?decl:(Ast0_cocci.declaration Visitor_ast0_types.rcode) ->
    ?field:(Ast0_cocci.field Visitor_ast0_types.rcode) ->
    ?enumdecl:(Ast0_cocci.enum_decl Visitor_ast0_types.rcode) ->
    ?stmt:(Ast0_cocci.statement Visitor_ast0_types.rcode) ->
    ?forinfo:(Ast0_cocci.forinfo Visitor_ast0_types.rcode) ->
    ?case:(Ast0_cocci.case_line Visitor_ast0_types.rcode) ->
    ?string_fragment:(Ast0_cocci.string_fragment Visitor_ast0_types.rcode) ->
    ?attribute:(Ast0_cocci.attr Visitor_ast0_types.rcode) ->
    ?attr_arg:(Ast0_cocci.attr_arg Visitor_ast0_types.rcode) ->
    ?top:(Ast0_cocci.top_level Visitor_ast0_types.rcode) -> unit ->
      Visitor_ast0_types.rebuilder_rec_functions

type 'b crmcodefn = { crmcode: 'a. 'a Ast0_cocci.mcode -> 'b * 'a Ast0_cocci.mcode }
type 'b crdonothingfn =
    { crdonothing: 'a. 'b Visitor_ast0_types.all_functions ->
      ('a Ast0_cocci.wrap -> 'b * 'a Ast0_cocci.wrap) -> 'a Ast0_cocci.wrap ->
	'b * 'a Ast0_cocci.wrap }

val combiner_rebuilder :
  ('a -> 'a -> 'a) -> 'a -> 'a crmcodefn -> 'a crdonothingfn ->
    ?meta_mcode:((Ast_cocci.meta_name,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?string_mcode:((string,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?const_mcode:((Ast_cocci.constant,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?simpleAssign_mcode:((Ast0_cocci.simpleAssignOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?opAssign_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?fixOp_mcode:((Ast_cocci.fixOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?unaryOp_mcode:((Ast_cocci.unaryOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?arithOp_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?logicalOp_mcode:((Ast_cocci.logicalOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?cv_mcode:((Ast_cocci.const_vol,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?sign_mcode:((Ast_cocci.sign,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?struct_mcode:((Ast_cocci.structUnion,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?storage_mcode:((Ast_cocci.storage,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?inc_mcode:((Ast_cocci.inc_file,'a) Visitor_ast0_types.flat_rcmcode) ->

    ?dotsexpr: ((Ast0_cocci.expression Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsinit: ((Ast0_cocci.initialiser Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsparam: ((Ast0_cocci.parameterTypeDef Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotstemplateparam: ((Ast0_cocci.templateParameterTypeDef Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsstmt: ((Ast0_cocci.statement Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsdecl: ((Ast0_cocci.declaration Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsfield: ((Ast0_cocci.field Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsenumdecl: ((Ast0_cocci.enum_decl Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotscase: ((Ast0_cocci.case_line Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsdefpar: ((Ast0_cocci.define_param Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?ident:((Ast0_cocci.ident,'a) Visitor_ast0_types.rccode) ->
    ?expr:((Ast0_cocci.expression,'a) Visitor_ast0_types.rccode) ->
    ?assignOp:((Ast0_cocci.assignOp,'a) Visitor_ast0_types.rccode) ->
    ?binaryOp:((Ast0_cocci.binaryOp,'a) Visitor_ast0_types.rccode) ->
    ?ty:((Ast0_cocci.typeC,'a) Visitor_ast0_types.rccode) ->
    ?init:((Ast0_cocci.initialiser,'a) Visitor_ast0_types.rccode) ->
    ?param:((Ast0_cocci.parameterTypeDef,'a) Visitor_ast0_types.rccode) ->
    ?template_param:((Ast0_cocci.templateParameterTypeDef,'a) Visitor_ast0_types.rccode) ->
    ?decl:((Ast0_cocci.declaration,'a) Visitor_ast0_types.rccode) ->
    ?field:((Ast0_cocci.field,'a) Visitor_ast0_types.rccode) ->
    ?enumdecl:((Ast0_cocci.enum_decl,'a) Visitor_ast0_types.rccode) ->
    ?stmt:((Ast0_cocci.statement,'a) Visitor_ast0_types.rccode) ->
    ?forinfo:((Ast0_cocci.forinfo,'a) Visitor_ast0_types.rccode) ->
    ?case:((Ast0_cocci.case_line,'a) Visitor_ast0_types.rccode) ->
    ?string_fragment:((Ast0_cocci.string_fragment,'a) Visitor_ast0_types.rccode) ->
    ?attribute:((Ast0_cocci.attr,'a) Visitor_ast0_types.rccode) ->
    ?attr_arg:((Ast0_cocci.attr_arg,'a) Visitor_ast0_types.rccode) ->
    ?top:((Ast0_cocci.top_level,'a) Visitor_ast0_types.rccode) -> unit ->
      'a Visitor_ast0_types.all_functions

val combiner_rebuilder_default :
  ('a -> 'a -> 'a) -> 'a ->
    ?meta_mcode:((Ast_cocci.meta_name,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?string_mcode:((string,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?const_mcode:((Ast_cocci.constant,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?simpleAssign_mcode:((Ast0_cocci.simpleAssignOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?opAssign_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?fixOp_mcode:((Ast_cocci.fixOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?unaryOp_mcode:((Ast_cocci.unaryOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?arithOp_mcode:((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?logicalOp_mcode:((Ast_cocci.logicalOp,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?cv_mcode:((Ast_cocci.const_vol,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?sign_mcode:((Ast_cocci.sign,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?struct_mcode:((Ast_cocci.structUnion,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?storage_mcode:((Ast_cocci.storage,'a) Visitor_ast0_types.flat_rcmcode) ->
    ?inc_mcode:((Ast_cocci.inc_file,'a) Visitor_ast0_types.flat_rcmcode) ->

    ?dotsexpr: ((Ast0_cocci.expression Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsinit: ((Ast0_cocci.initialiser Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsparam: ((Ast0_cocci.parameterTypeDef Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotstemplateparam: ((Ast0_cocci.templateParameterTypeDef Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsstmt: ((Ast0_cocci.statement Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsdecl: ((Ast0_cocci.declaration Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsfield: ((Ast0_cocci.field Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsenumdecl: ((Ast0_cocci.enum_decl Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotscase: ((Ast0_cocci.case_line Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?dotsdefpar: ((Ast0_cocci.define_param Ast0_cocci.dots,'a) Visitor_ast0_types.rccode) ->
    ?ident:((Ast0_cocci.ident,'a) Visitor_ast0_types.rccode) ->
    ?expr:((Ast0_cocci.expression,'a) Visitor_ast0_types.rccode) ->
    ?assignOp:((Ast0_cocci.assignOp,'a) Visitor_ast0_types.rccode) ->
    ?binaryOp:((Ast0_cocci.binaryOp,'a) Visitor_ast0_types.rccode) ->
    ?ty:((Ast0_cocci.typeC,'a) Visitor_ast0_types.rccode) ->
    ?init:((Ast0_cocci.initialiser,'a) Visitor_ast0_types.rccode) ->
    ?param:((Ast0_cocci.parameterTypeDef,'a) Visitor_ast0_types.rccode) ->
    ?template_param:((Ast0_cocci.templateParameterTypeDef,'a) Visitor_ast0_types.rccode) ->
    ?decl:((Ast0_cocci.declaration,'a) Visitor_ast0_types.rccode) ->
    ?field:((Ast0_cocci.field,'a) Visitor_ast0_types.rccode) ->
    ?enumdecl:((Ast0_cocci.enum_decl,'a) Visitor_ast0_types.rccode) ->
    ?stmt:((Ast0_cocci.statement,'a) Visitor_ast0_types.rccode) ->
    ?forinfo:((Ast0_cocci.forinfo,'a) Visitor_ast0_types.rccode) ->
    ?case:((Ast0_cocci.case_line,'a) Visitor_ast0_types.rccode) ->
    ?string_fragment:((Ast0_cocci.string_fragment,'a) Visitor_ast0_types.rccode) ->
    ?attribute:((Ast0_cocci.attr,'a) Visitor_ast0_types.rccode) ->
    ?attr_arg:((Ast0_cocci.attr_arg,'a) Visitor_ast0_types.rccode) ->
    ?top:((Ast0_cocci.top_level,'a) Visitor_ast0_types.rccode) -> unit ->
      'a Visitor_ast0_types.all_functions
