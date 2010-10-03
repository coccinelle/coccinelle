
val match_re_node :
  string list (* dropped isos *) ->
  Ast_cocci.rule_elem -> Control_flow_c.node ->
  Lib_engine.metavars_binding -> 
  (Ast_cocci.rule_elem * Lib_engine.metavars_binding) list
