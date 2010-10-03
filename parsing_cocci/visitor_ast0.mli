
(* --------------------------------------------------------------------- *)

type 'a combiner =
    {combiner_ident : Ast0_cocci.ident -> 'a;
      combiner_expression : Ast0_cocci.expression -> 'a;
      combiner_typeC : Ast0_cocci.typeC -> 'a;
      combiner_declaration : Ast0_cocci.declaration -> 'a;
      combiner_initialiser : Ast0_cocci.initialiser -> 'a;
      combiner_initialiser_list : Ast0_cocci.initialiser_list -> 'a;
      combiner_parameter : Ast0_cocci.parameterTypeDef -> 'a;
      combiner_parameter_list : Ast0_cocci.parameter_list -> 'a;
      combiner_statement : Ast0_cocci.statement -> 'a;
      combiner_case_line : Ast0_cocci.case_line -> 'a;
      combiner_top_level : Ast0_cocci.top_level -> 'a;
      combiner_expression_dots :
	  Ast0_cocci.expression Ast0_cocci.dots -> 'a;
      combiner_statement_dots :
	      Ast0_cocci.statement Ast0_cocci.dots -> 'a;
      combiner_declaration_dots :
		  Ast0_cocci.declaration Ast0_cocci.dots -> 'a;
      combiner_case_line_dots :
		  Ast0_cocci.case_line Ast0_cocci.dots -> 'a;
      combiner_anything : Ast0_cocci.anything -> 'a}

type ('mc,'a) cmcode = 'mc Ast0_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a

val combiner :
    ('a -> 'a -> 'a) -> 'a ->
    ((string*string,'a) cmcode) ->
    ((string,'a) cmcode) ->
    ((Ast_cocci.constant,'a) cmcode) ->
    ((Ast_cocci.assignOp,'a) cmcode) ->
    ((Ast_cocci.fixOp,'a) cmcode) ->
    ((Ast_cocci.unaryOp,'a) cmcode) ->
    ((Ast_cocci.binaryOp,'a) cmcode) ->
    ((Ast_cocci.const_vol,'a) cmcode) ->
    ((Ast_cocci.baseType,'a) cmcode) ->
    ((Ast_cocci.sign,'a) cmcode) ->
    ((Ast_cocci.structUnion,'a) cmcode) ->
    ((Ast_cocci.storage,'a) cmcode) ->
    ((Ast_cocci.inc_file,'a) cmcode) ->
    ((Ast0_cocci.expression Ast0_cocci.dots,'a) ccode) ->
    ((Ast0_cocci.initialiser Ast0_cocci.dots,'a) ccode) ->
    ((Ast0_cocci.parameterTypeDef Ast0_cocci.dots,'a) ccode) ->
    ((Ast0_cocci.statement Ast0_cocci.dots,'a) ccode) ->
    ((Ast0_cocci.declaration Ast0_cocci.dots,'a) ccode) ->
    ((Ast0_cocci.case_line Ast0_cocci.dots,'a) ccode) ->
    ((Ast0_cocci.ident,'a) ccode) ->
    ((Ast0_cocci.expression,'a) ccode) ->
    ((Ast0_cocci.typeC,'a) ccode) ->
    ((Ast0_cocci.initialiser,'a) ccode) ->
    ((Ast0_cocci.parameterTypeDef,'a) ccode) ->
    ((Ast0_cocci.declaration,'a) ccode) ->
    ((Ast0_cocci.statement,'a) ccode) ->
    ((Ast0_cocci.case_line,'a) ccode) ->
    ((Ast0_cocci.top_level,'a) ccode) ->
    'a combiner

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast0_cocci.ident inout;
      rebuilder_expression : Ast0_cocci.expression inout;
      rebuilder_typeC : Ast0_cocci.typeC inout;
      rebuilder_declaration : Ast0_cocci.declaration inout;
      rebuilder_initialiser : Ast0_cocci.initialiser inout;
      rebuilder_initialiser_list : Ast0_cocci.initialiser_list inout;
      rebuilder_parameter : Ast0_cocci.parameterTypeDef inout;
      rebuilder_parameter_list : Ast0_cocci.parameter_list inout;
      rebuilder_statement : Ast0_cocci.statement inout;
      rebuilder_case_line : Ast0_cocci.case_line inout;
      rebuilder_top_level : Ast0_cocci.top_level inout;
      rebuilder_expression_dots :
	Ast0_cocci.expression Ast0_cocci.dots ->
	  Ast0_cocci.expression Ast0_cocci.dots;
	  rebuilder_statement_dots :
	    Ast0_cocci.statement Ast0_cocci.dots ->
	      Ast0_cocci.statement Ast0_cocci.dots;
	  rebuilder_declaration_dots :
	    Ast0_cocci.declaration Ast0_cocci.dots ->
	      Ast0_cocci.declaration Ast0_cocci.dots;
	  rebuilder_case_line_dots :
	    Ast0_cocci.case_line Ast0_cocci.dots ->
	      Ast0_cocci.case_line Ast0_cocci.dots;
          rebuilder_anything : Ast0_cocci.anything -> Ast0_cocci.anything}

type 'mc rmcode = 'mc Ast0_cocci.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout

val rebuilder :
    ((string*string) rmcode) ->
    (string rmcode) ->
    (Ast_cocci.constant rmcode) ->
    (Ast_cocci.assignOp rmcode) ->
    (Ast_cocci.fixOp rmcode) ->
    (Ast_cocci.unaryOp rmcode) ->
    (Ast_cocci.binaryOp rmcode) ->
    (Ast_cocci.const_vol rmcode) ->
    (Ast_cocci.baseType rmcode) ->
    (Ast_cocci.sign rmcode) ->
    (Ast_cocci.structUnion rmcode) ->
    (Ast_cocci.storage rmcode) ->
    (Ast_cocci.inc_file rmcode) ->
    (Ast0_cocci.expression Ast0_cocci.dots rcode) ->
    (Ast0_cocci.initialiser Ast0_cocci.dots rcode) ->
    (Ast0_cocci.parameterTypeDef Ast0_cocci.dots rcode) ->
    (Ast0_cocci.statement Ast0_cocci.dots rcode) ->
    (Ast0_cocci.declaration Ast0_cocci.dots rcode) ->
    (Ast0_cocci.case_line Ast0_cocci.dots rcode) ->
    (Ast0_cocci.ident rcode) ->
    (Ast0_cocci.expression rcode) ->
    (Ast0_cocci.typeC rcode) ->
    (Ast0_cocci.initialiser rcode) ->
    (Ast0_cocci.parameterTypeDef rcode) ->
    (Ast0_cocci.declaration rcode) ->
    (Ast0_cocci.statement rcode) ->
    (Ast0_cocci.case_line rcode) ->
    (Ast0_cocci.top_level rcode) ->
    rebuilder
