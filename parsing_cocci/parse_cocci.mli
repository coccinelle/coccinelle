val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
	(Ast_cocci.rule list) *
	  (((Ast_cocci.meta_name list) list) list) (*fvs of the rule*) *
	  (((Ast_cocci.meta_name list) list) list) (*negated pos vars*) *
	  (((Ast_cocci.meta_name list) list) list) (*used after list*) *
	  (((Ast_cocci.meta_name list) list) list) (*positions list*) *
	  string list list (* non metavars in - code, for grep *) *
	  string option (* non metavars in - code, for glimpse *)
