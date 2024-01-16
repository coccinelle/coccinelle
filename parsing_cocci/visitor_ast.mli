(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type 'a combiner =
    {combiner_ident : Ast_cocci.ident -> 'a;
     combiner_expression : Ast_cocci.expression -> 'a;
     combiner_fragment : Ast_cocci.string_fragment -> 'a;
     combiner_format : Ast_cocci.string_format -> 'a;
     combiner_assignOp : Ast_cocci.assignOp -> 'a;
     combiner_binaryOp : Ast_cocci.binaryOp -> 'a;
     combiner_fullType : Ast_cocci.fullType -> 'a;
     combiner_typeC : Ast_cocci.typeC -> 'a;
     combiner_declaration : Ast_cocci.declaration -> 'a;
     combiner_field : Ast_cocci.field -> 'a;
     combiner_ann_field : Ast_cocci.annotated_field -> 'a;
     combiner_enumdecl : Ast_cocci.enum_decl -> 'a;
     combiner_initialiser : Ast_cocci.initialiser -> 'a;
     combiner_parameter : Ast_cocci.parameterTypeDef -> 'a;
     combiner_template_parameter : Ast_cocci.templateParameterTypeDef -> 'a;
     combiner_parameter_list : Ast_cocci.parameter_list -> 'a;
     combiner_rule_elem : Ast_cocci.rule_elem -> 'a;
     combiner_statement : Ast_cocci.statement -> 'a;
     combiner_case_line : Ast_cocci.case_line -> 'a;
     combiner_attribute : Ast_cocci.attr -> 'a;
     combiner_attr_arg : Ast_cocci.attr_arg -> 'a;
     combiner_top_level : Ast_cocci.top_level -> 'a;
     combiner_anything : Ast_cocci.anything  -> 'a;
     combiner_expression_dots : Ast_cocci.expression Ast_cocci.dots -> 'a;
     combiner_statement_dots : Ast_cocci.statement Ast_cocci.dots -> 'a;
     combiner_anndecl_dots : Ast_cocci.annotated_decl Ast_cocci.dots -> 'a;
     combiner_annfield_dots : Ast_cocci.annotated_field Ast_cocci.dots -> 'a;
     combiner_enumdecl_dots : Ast_cocci.enum_decl Ast_cocci.dots -> 'a;
     combiner_initialiser_dots : Ast_cocci.initialiser Ast_cocci.dots -> 'a}

type ('mc,'a) cmcode = 'a combiner -> 'mc Ast_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a

type 'b cmcodefn = { cmcode: 'a. 'b combiner -> 'a Ast_cocci.mcode -> 'b }
type 'b cdonothingfn =
    { cdonothing: 'a. 'b combiner -> ('a Ast_cocci.wrap -> 'b) -> 'a Ast_cocci.wrap -> 'b }

val combiner :
    ('a -> 'a -> 'a) -> 'a -> 'a cmcodefn -> 'a cdonothingfn ->
    ?meta_mcode:((Ast_cocci.meta_name,'a) cmcode) ->
    ?string_mcode:((string,'a) cmcode) ->
    ?const_mcode:((Ast_cocci.constant,'a) cmcode) ->
    ?simpleAssign_mcode:((Ast_cocci.simpleAssignOp,'a) cmcode) ->
    ?opAssign_mcode:((Ast_cocci.arithOp,'a) cmcode) ->
    ?fixOp_mcode:((Ast_cocci.fixOp,'a) cmcode) ->
    ?unaryOp_mcode:((Ast_cocci.unaryOp,'a) cmcode) ->
    ?arithOp_mcode:((Ast_cocci.arithOp,'a) cmcode) ->
    ?logicalOp_mcode:((Ast_cocci.logicalOp,'a) cmcode) ->
    ?cv_mcode:((Ast_cocci.const_vol,'a) cmcode) ->
    ?sign_mcode:((Ast_cocci.sign,'a) cmcode) ->
    ?struct_mcode:((Ast_cocci.structUnion,'a) cmcode) ->
    ?storage_mcode:((Ast_cocci.storage,'a) cmcode) ->
    ?inc_mcode:((Ast_cocci.inc_file,'a) cmcode) ->
    ?dotsexpr:((Ast_cocci.expression Ast_cocci.dots,'a) ccode) ->
    ?dotsinit:((Ast_cocci.initialiser Ast_cocci.dots,'a) ccode) ->
    ?dotsparam:((Ast_cocci.parameterTypeDef Ast_cocci.dots,'a) ccode) ->
    ?dotstemplateparam:((Ast_cocci.templateParameterTypeDef Ast_cocci.dots,'a) ccode) ->
    ?dotsstmt:((Ast_cocci.statement Ast_cocci.dots,'a) ccode) ->
    ?dotsanndecl:((Ast_cocci.annotated_decl Ast_cocci.dots,'a) ccode) ->
    ?dotsannfield:((Ast_cocci.annotated_field Ast_cocci.dots,'a) ccode) ->
    ?dotsenumdecl:((Ast_cocci.enum_decl Ast_cocci.dots,'a) ccode) ->
    ?dotsdefpar:((Ast_cocci.define_param Ast_cocci.dots,'a) ccode) ->
    ?ident:((Ast_cocci.ident,'a) ccode) ->
    ?expr:((Ast_cocci.expression,'a) ccode) ->
    ?assignOp:((Ast_cocci.assignOp,'a) ccode) ->
    ?binaryOp:((Ast_cocci.binaryOp,'a) ccode) ->
    ?ty:((Ast_cocci.typeC,'a) ccode) ->
    ?ft:((Ast_cocci.fullType,'a) ccode) ->
    ?init:((Ast_cocci.initialiser,'a) ccode) ->
    ?param:((Ast_cocci.parameterTypeDef,'a) ccode) ->
    ?template_param:((Ast_cocci.templateParameterTypeDef,'a) ccode) ->
    ?define_param:((Ast_cocci.define_param,'a) ccode) ->
    ?decl:((Ast_cocci.declaration,'a) ccode) ->
    ?annotated_decl:((Ast_cocci.annotated_decl,'a) ccode) ->
    ?field:((Ast_cocci.field,'a) ccode) ->
    ?annotated_field:((Ast_cocci.annotated_field,'a) ccode) ->
    ?enumdecl:((Ast_cocci.enum_decl,'a) ccode) ->
    ?stmt:((Ast_cocci.statement,'a) ccode) ->
    ?rule:((Ast_cocci.rule_elem,'a) ccode) ->
    ?case:((Ast_cocci.case_line,'a) ccode) ->
    ?string_fragment:((Ast_cocci.string_fragment,'a) ccode) ->
    ?fmt:((Ast_cocci.string_format,'a) ccode) ->
    ?attribute:((Ast_cocci.attr,'a) ccode) ->
    ?attr_arg:((Ast_cocci.attr_arg,'a) ccode) ->
    ?pragma_info:((Ast_cocci.pragmainfo,'a) ccode) ->
    ?directive:((Ast_cocci.directive,'a) ccode) ->
    ?top:((Ast_cocci.top_level,'a) ccode) ->
    ((Ast_cocci.anything,'a) ccode) ->
      'a combiner

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast_cocci.ident inout;
      rebuilder_expression : Ast_cocci.expression inout;
      rebuilder_fragment : Ast_cocci.string_fragment inout;
      rebuilder_format : Ast_cocci.string_format inout;
      rebuilder_assignOp : Ast_cocci.assignOp inout;
      rebuilder_binaryOp : Ast_cocci.binaryOp inout;
      rebuilder_fullType : Ast_cocci.fullType inout;
      rebuilder_typeC : Ast_cocci.typeC inout;
      rebuilder_declaration : Ast_cocci.declaration inout;
      rebuilder_field : Ast_cocci.field inout;
      rebuilder_ann_field : Ast_cocci.annotated_field inout;
      rebuilder_enumdecl : Ast_cocci.enum_decl inout;
      rebuilder_initialiser : Ast_cocci.initialiser inout;
      rebuilder_parameter : Ast_cocci.parameterTypeDef inout;
      rebuilder_template_parameter : Ast_cocci.templateParameterTypeDef inout;
      rebuilder_parameter_list : Ast_cocci.parameter_list inout;
      rebuilder_statement : Ast_cocci.statement inout;
      rebuilder_case_line : Ast_cocci.case_line inout;
      rebuilder_attribute : Ast_cocci.attr inout;
      rebuilder_attr_arg : Ast_cocci.attr_arg inout;
      rebuilder_rule_elem : Ast_cocci.rule_elem inout;
      rebuilder_top_level : Ast_cocci.top_level inout;
      rebuilder_expression_dots : Ast_cocci.expression Ast_cocci.dots inout;
      rebuilder_statement_dots : Ast_cocci.statement Ast_cocci.dots inout;
      rebuilder_anndecl_dots : Ast_cocci.annotated_decl Ast_cocci.dots inout;
      rebuilder_annfield_dots : Ast_cocci.annotated_field Ast_cocci.dots inout;
      rebuilder_enumdecl_dots : Ast_cocci.enum_decl Ast_cocci.dots inout;
      rebuilder_initialiser_dots : Ast_cocci.initialiser Ast_cocci.dots inout;
      rebuilder_define_param_dots: Ast_cocci.define_param Ast_cocci.dots inout;
      rebuilder_define_param : Ast_cocci.define_param inout;
      rebuilder_define_parameters : Ast_cocci.define_parameters inout;
      rebuilder_anything : Ast_cocci.anything inout}

type 'mc rmcode = 'mc Ast_cocci.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout

type rmcodefn = { rmcode: 'a. 'a Ast_cocci.mcode -> 'a Ast_cocci.mcode }
type rdonothingfn =
    { rdonothing: 'a. rebuilder -> ('a Ast_cocci.wrap -> 'a Ast_cocci.wrap) ->
      'a Ast_cocci.wrap -> 'a Ast_cocci.wrap }

val rebuilder :
    rmcodefn -> rdonothingfn ->
    ?meta_mcode:(Ast_cocci.meta_name rmcode) ->
    ?string_mcode:(string rmcode) ->
    ?const_mcode:(Ast_cocci.constant rmcode) ->
    ?simpleAssign_mcode:(Ast_cocci.simpleAssignOp rmcode) ->
    ?opAssign_mcode:(Ast_cocci.arithOp rmcode) ->
    ?fixOp_mcode:(Ast_cocci.fixOp rmcode) ->
    ?unaryOp_mcode:(Ast_cocci.unaryOp rmcode) ->
    ?arithOp_mcode:(Ast_cocci.arithOp rmcode) ->
    ?logicalOp_mcode:(Ast_cocci.logicalOp rmcode) ->
    ?cv_mcode:(Ast_cocci.const_vol rmcode) ->
    ?sign_mcode:(Ast_cocci.sign rmcode) ->
    ?struct_mcode:(Ast_cocci.structUnion rmcode) ->
    ?storage_mcode:(Ast_cocci.storage rmcode) ->
    ?inc_mcode:(Ast_cocci.inc_file rmcode) ->
    ?dotsexpr:(Ast_cocci.expression Ast_cocci.dots rcode) ->
    ?dotsinit:(Ast_cocci.initialiser Ast_cocci.dots rcode) ->
    ?dotsparam:(Ast_cocci.parameterTypeDef Ast_cocci.dots rcode) ->
    ?dotstemplateparam:(Ast_cocci.templateParameterTypeDef Ast_cocci.dots rcode) ->
    ?dotsstmt:(Ast_cocci.statement Ast_cocci.dots rcode) ->
    ?dotsanndecl:(Ast_cocci.annotated_decl Ast_cocci.dots rcode) ->
    ?dotsannfield:(Ast_cocci.annotated_field Ast_cocci.dots rcode) ->
    ?dotsenumdecl:(Ast_cocci.enum_decl Ast_cocci.dots rcode) ->
    ?dotsdefpar:(Ast_cocci.define_param Ast_cocci.dots rcode) ->
    ?ident:(Ast_cocci.ident rcode) ->
    ?expr:(Ast_cocci.expression rcode) ->
    ?assignOp:(Ast_cocci.assignOp rcode) ->
    ?binaryOp:(Ast_cocci.binaryOp rcode) ->
    ?ty:(Ast_cocci.typeC rcode) ->
    ?ft:(Ast_cocci.fullType rcode) ->
    ?init:(Ast_cocci.initialiser rcode) ->
    ?param:(Ast_cocci.parameterTypeDef rcode) ->
    ?template_param:(Ast_cocci.templateParameterTypeDef rcode) ->
    ?define_param:(Ast_cocci.define_param rcode) ->
    ?decl:(Ast_cocci.declaration rcode) ->
    ?annotated_decl:(Ast_cocci.annotated_decl rcode) ->
    ?field:(Ast_cocci.field rcode) ->
    ?annotated_field:(Ast_cocci.annotated_field rcode) ->
    ?enumdecl:(Ast_cocci.enum_decl rcode) ->
    ?stmt:(Ast_cocci.statement rcode) ->
    ?rule:(Ast_cocci.rule_elem rcode) ->
    ?case:(Ast_cocci.case_line rcode) ->
    ?string_fragment:(Ast_cocci.string_fragment rcode) ->
    ?fmt:(Ast_cocci.string_format rcode) ->
    ?attribute:(Ast_cocci.attr rcode) ->
    ?attr_arg:(Ast_cocci.attr_arg rcode) ->
    ?pragma_info:(Ast_cocci.pragmainfo rcode) ->
    ?directive:(Ast_cocci.directive rcode) ->
    ?top:(Ast_cocci.top_level rcode) ->
    (Ast_cocci.anything rcode) ->
    rebuilder
