val process : string (* name *) -> Ast_cocci.metavar list ->
  string list (* dropped isos *) ->
    Ast0_cocci.rule -> Ast0_cocci.rule -> Ast_cocci.ruletype ->
      ((Ast_cocci.metavar list * Ast0_cocci.rule) *
	 Ast_cocci.rule_with_metavars option)
