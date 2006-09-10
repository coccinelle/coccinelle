val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
	(Ast_cocci.rule list) * ((string list) list) *
	  string list (* non metavars in - code *)
