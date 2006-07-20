open Common
open Ograph_extended 


val labels_for_ctl :
  (nodei * Control_flow_c.node) list -> Lib_engine.label_ctlcocci


val fix_flow_ctl : 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended

val model_for_ctl :
  (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
  (Control_flow_c.node, Control_flow_c.edge) ograph_extended *
   Lib_engine.label_ctlcocci *
   nodei list


val mysat :
  ((Control_flow_c.node, Control_flow_c.edge) ograph_extended *
   Lib_engine.label_ctlcocci *
   nodei list) -> 
  Lib_engine.ctlcocci -> 
  (nodei * 
   (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *
    Lib_engine.predicate) 
  list



val satbis_to_trans_info :  
  (nodei * 
   (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *  
   Lib_engine.predicate) 
  list -> 
  (nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list 

