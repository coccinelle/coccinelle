val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
	(Ast_cocci.rule list) *
	  (((Ast_cocci.meta_name list) list) list) (*fvs of the rule*) *
	  (((Ast_cocci.meta_name list) list) list) (*used after list*) *
	  string list (* non metavars in - code *)
