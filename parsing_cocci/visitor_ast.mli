type 'a combiner =
    {combiner_ident : Ast_cocci.ident -> 'a;
      combiner_expression : Ast_cocci.expression -> 'a;
	combiner_fullType : Ast_cocci.fullType -> 'a;
	  combiner_typeC : Ast_cocci.typeC -> 'a;
	    combiner_declaration : Ast_cocci.declaration -> 'a;
	      combiner_parameter_list : Ast_cocci.parameter_list -> 'a;
		combiner_rule_elem : Ast_cocci.rule_elem -> 'a;
		  combiner_statement : Ast_cocci.statement -> 'a;
		    combiner_top_level : Ast_cocci.top_level -> 'a;
		      combiner_anything : Ast_cocci.anything  -> 'a;
			combiner_expression_dots :
			  Ast_cocci.expression Ast_cocci.dots -> 'a;
			    combiner_statement_dots :
			      Ast_cocci.statement Ast_cocci.dots -> 'a}

type ('mc,'a) cmcode = 'mc Ast_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a

val combiner :
    ('a -> 'a -> 'a) -> 'a ->
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
			    ((Ast_cocci.expression Ast_cocci.dots,'a) ccode) ->
			      ((Ast_cocci.parameterTypeDef Ast_cocci.dots,'a) ccode) ->
				((Ast_cocci.statement Ast_cocci.dots,'a) ccode) ->
				  ((Ast_cocci.ident,'a) ccode) ->
				    ((Ast_cocci.expression,'a) ccode) ->
				      ((Ast_cocci.fullType,'a) ccode) ->
					((Ast_cocci.typeC,'a) ccode) ->
					  ((Ast_cocci.parameterTypeDef,'a) ccode) ->
					    ((Ast_cocci.declaration,'a) ccode) ->
					      ((Ast_cocci.rule_elem,'a) ccode) ->
						((Ast_cocci.statement,'a) ccode) ->
						  ((Ast_cocci.top_level,'a) ccode) ->
						    ((Ast_cocci.anything,'a) ccode) ->
						      'a combiner
