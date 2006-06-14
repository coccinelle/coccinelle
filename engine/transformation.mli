open Ograph_extended

val transform: Lib_engine.transformation_info ->
       (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
        (Control_flow_c.node, Control_flow_c.edge) ograph_extended
