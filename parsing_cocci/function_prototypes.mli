val process : Ast0_cocci.rule -> Ast0_cocci.rule ->
  string (*current rule name*) ->
    (Ast_cocci.metavar list * Ast_cocci.rule) option
