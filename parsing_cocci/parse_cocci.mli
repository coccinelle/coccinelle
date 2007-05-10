val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
	string list (* all names *) * string list list (* all dependencies *) *
	(Ast_cocci.rule list) * (((Ast_cocci.meta_name list) list) list) *
	  string list (* non metavars in - code *)
