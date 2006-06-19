val cprogram_from_file : Common.filename -> Ast_c.program2 * Parse_c.parsing_stat

val cstatement_from_string : string -> Ast_c.statement
val cexpression_from_string : string -> Ast_c.expression

val sp_from_file : Common.filename -> Ast_cocci.rule list
val spbis_from_file : Common.filename -> Ast_cocci.rule_with_metavars list
val rule_elem_from_string : string -> Ast_cocci.rule_elem


val flows : Ast_c.program2 * Parse_c.parsing_stat ->
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended list
val one_flow :
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended list
  -> (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended
val print_flow : (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended -> unit


val ctls : Ast_cocci.rule list -> 
  (Lib_engine.full_predicate, string) Ast_ctl.generic_ctl list list
val one_ctl :
    (Lib_engine.full_predicate, string) Ast_ctl.generic_ctl list list ->
      (Lib_engine.full_predicate, string) Ast_ctl.generic_ctl


val test_unparser : Common.filename -> string list
val test_pattern : string -> string -> Ast_c.metavars_binding list

(*
val test_cocci :
  Common.filename -> Common.filename ->
    'a

  (Ograph_extended.nodei *
   (Ast_ctl.mvar, Ast_ctl.metavar_binding_kind2)
   Ast_ctl.generic_substitution *
   (Ograph_extended.nodei,
    (Ast_ctl.mvar, Ast_ctl.metavar_binding_kind2)
    Ast_ctl.generic_substitution, 'a list)
   Ast_ctl.generic_witness list)
  list
*)
