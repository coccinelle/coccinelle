val transform: 
 Lib_engine.transformation_info -> Control_flow_c.cflow -> Control_flow_c.cflow


val transform_re_node: 
   Ast_cocci.rule_elem -> Control_flow_c.node -> Lib_engine.metavars_binding ->
   Control_flow_c.node

exception NoMatch 
