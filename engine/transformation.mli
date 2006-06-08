open Ograph_extended

type transformation_info = (nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list

val transform: transformation_info ->
       (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
        (Control_flow_c.node, Control_flow_c.edge) ograph_extended
