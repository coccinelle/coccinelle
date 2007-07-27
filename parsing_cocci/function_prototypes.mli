val process : string (* name *) -> string list (* dropped isos *) ->
  Ast0_cocci.rule -> Ast0_cocci.rule -> Ast_cocci.rule_with_metavars option
