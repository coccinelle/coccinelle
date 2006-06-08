val c : Common.filename -> Ast_c.program2 * Parse_c.parsing_stat
val sp : string -> (Ast_cocci.rule * Ast0_cocci.rule) list
val spbis : string -> Ast_cocci.rule_with_metavars list
val ctls :
  string -> (Ast0toctl.predicate, string) Ast_ctl.generic_ctl list list
val one_ctl : string -> (Ast0toctl.predicate, string) Ast_ctl.generic_ctl
val flows :
  Common.filename ->
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended
  list
val one_flow :
  Common.filename ->
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended
val print_flow : Common.filename -> unit
val test_cocci :
  Common.filename -> Common.filename ->
  (Ograph_extended.nodei *
   (Ctlcocci_integration.mvar, Ctlcocci_integration.metavar_binding_kind2)
   Ast_ctl.generic_substitution *
   (Ograph_extended.nodei,
    (Ctlcocci_integration.mvar, Ctlcocci_integration.metavar_binding_kind2)
    Ast_ctl.generic_substitution, 'a list)
   Ast_ctl.generic_witness list)
  list
