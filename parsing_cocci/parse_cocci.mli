exception Bad_virt of string

val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
	(Ast_cocci.metavar list list) * (Ast_cocci.rule list) *
	  Ast_cocci.meta_name list list list (*fvs of the rule*) *
	  Ast_cocci.meta_name list list list (*negated pos vars*) *
	  (Ast_cocci.meta_name list list list (*used after list*) *
	     (*fresh used after list*)
	     Ast_cocci.meta_name list list list *
	     (*fresh used after list seeds*)
	     Ast_cocci.meta_name list list list) *
	  Ast_cocci.meta_name list list list (*positions list*) *
	  (string list option (*non metavars in - code, for grep*) *
	     string list option (*non metavars in - code, for glimpse/google*) *
	     (*non metavars in - code, for other tools*)
	     Get_constants2.combine option)
