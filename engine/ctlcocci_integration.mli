open Common
open Ograph_extended 


val labels_for_ctl :
  (nodei * Control_flow_c.node) list -> Lib_engine.metavars_binding -> 
  Lib_engine.label_ctlcocci


val fix_flow_ctl : 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended

val model_for_ctl :
  (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
  Lib_engine.metavars_binding ->
  (Control_flow_c.node, Control_flow_c.edge) ograph_extended *
   Lib_engine.label_ctlcocci *
   nodei list


val mysat :
  ((Control_flow_c.node, Control_flow_c.edge) ograph_extended *
   Lib_engine.label_ctlcocci *
   nodei list) -> 
  Lib_engine.ctlcocci -> 
  (Lib_engine.mvar list * Lib_engine.metavars_binding2) -> 
  ((nodei * Lib_engine.metavars_binding2 * Lib_engine.predicate) list *
     Lib_engine.metavars_binding2) option



val satbis_to_trans_info :  
  (nodei * Lib_engine.metavars_binding2 * Lib_engine.predicate) list -> 
  (nodei * Lib_engine.metavars_binding  * Ast_cocci.rule_elem)  list 

val metavars_binding2_to_binding : 
    Lib_engine.metavars_binding2 -> Lib_engine.metavars_binding

val metavars_binding_to_binding2 : 
    Lib_engine.metavars_binding -> Lib_engine.metavars_binding2
