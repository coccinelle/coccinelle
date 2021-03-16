(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val combiner_functions : 'a Visitor_ast0_types.combiner_functions
val combiner :
  ('a -> 'a -> 'a) ->
  'a -> 'a Visitor_ast0_types.combiner_functions ->
    'a Visitor_ast0_types.combiner_rec_functions

val flat_combiner :
    ('a -> 'a -> 'a) -> 'a ->
    ((Ast_cocci.meta_name,'a) Visitor_ast0_types.flat_cmcode) ->
    ((string,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.constant,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast0_cocci.simpleAssignOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.fixOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.unaryOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.arithOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.logicalOp,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.const_vol,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.sign,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.structUnion,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.storage,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast_cocci.inc_file,'a) Visitor_ast0_types.flat_cmcode) ->
    ((Ast0_cocci.expression Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.initialiser Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.parameterTypeDef Ast0_cocci.dots,'a)
       Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.statement Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.declaration Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.field Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.enum_decl Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.case_line Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.define_param Ast0_cocci.dots,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.ident,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.expression,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.assignOp,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.binaryOp,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.typeC,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.initialiser,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.parameterTypeDef,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.declaration,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.field,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.enum_decl,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.statement,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.forinfo,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.case_line,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.string_fragment,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.attr,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.attr_arg,'a) Visitor_ast0_types.ccode) ->
    ((Ast0_cocci.top_level,'a) Visitor_ast0_types.ccode) ->
    'a Visitor_ast0_types.combiner_rec_functions

val rebuilder_functions : Visitor_ast0_types.rebuilder_functions
val rebuilder : Visitor_ast0_types.rebuilder_functions ->
  Visitor_ast0_types.rebuilder_rec_functions

val flat_rebuilder :
    (Ast_cocci.meta_name Visitor_ast0_types.rmcode) ->
    (string Visitor_ast0_types.rmcode) ->
    (Ast_cocci.constant Visitor_ast0_types.rmcode) ->
    (Ast0_cocci.simpleAssignOp Visitor_ast0_types.rmcode) ->
    (Ast_cocci.arithOp Visitor_ast0_types.rmcode) ->
    (Ast_cocci.fixOp Visitor_ast0_types.rmcode) ->
    (Ast_cocci.unaryOp Visitor_ast0_types.rmcode) ->
    (Ast_cocci.arithOp Visitor_ast0_types.rmcode) ->
    (Ast_cocci.logicalOp Visitor_ast0_types.rmcode) ->
    (Ast_cocci.const_vol Visitor_ast0_types.rmcode) ->
    (Ast_cocci.sign Visitor_ast0_types.rmcode) ->
    (Ast_cocci.structUnion Visitor_ast0_types.rmcode) ->
    (Ast_cocci.storage Visitor_ast0_types.rmcode) ->
    (Ast_cocci.inc_file Visitor_ast0_types.rmcode) ->
    (Ast0_cocci.expression Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.initialiser Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.parameterTypeDef Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.statement Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.declaration Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.field Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.enum_decl Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.case_line Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.define_param Ast0_cocci.dots Visitor_ast0_types.rcode) ->
    (Ast0_cocci.ident Visitor_ast0_types.rcode) ->
    (Ast0_cocci.expression Visitor_ast0_types.rcode) ->
    (Ast0_cocci.assignOp Visitor_ast0_types.rcode) ->
    (Ast0_cocci.binaryOp Visitor_ast0_types.rcode) ->
    (Ast0_cocci.typeC Visitor_ast0_types.rcode) ->
    (Ast0_cocci.initialiser Visitor_ast0_types.rcode) ->
    (Ast0_cocci.parameterTypeDef Visitor_ast0_types.rcode) ->
    (Ast0_cocci.declaration Visitor_ast0_types.rcode) ->
    (Ast0_cocci.field Visitor_ast0_types.rcode) ->
    (Ast0_cocci.enum_decl Visitor_ast0_types.rcode) ->
    (Ast0_cocci.statement Visitor_ast0_types.rcode) ->
    (Ast0_cocci.forinfo Visitor_ast0_types.rcode) ->
    (Ast0_cocci.case_line Visitor_ast0_types.rcode) ->
    (Ast0_cocci.string_fragment Visitor_ast0_types.rcode) ->
    (Ast0_cocci.attr Visitor_ast0_types.rcode) ->
    (Ast0_cocci.attr_arg Visitor_ast0_types.rcode) ->
    (Ast0_cocci.top_level Visitor_ast0_types.rcode) ->
      Visitor_ast0_types.rebuilder_rec_functions

val combiner_rebuilder_functions :
    'a Visitor_ast0_types.combiner_rebuilder_functions
val combiner_rebuilder :
  ('a -> 'a -> 'a) -> 'a ->
    'a Visitor_ast0_types.combiner_rebuilder_functions ->
      'a Visitor_ast0_types.all_functions
