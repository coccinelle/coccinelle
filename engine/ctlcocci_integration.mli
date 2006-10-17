open Common
open Ograph_extended 


val labels_for_ctl :
  (nodei * Control_flow_c.node) list -> Lib_engine.metavars_binding -> 
  Lib_engine.label_ctlcocci


val fix_flow_ctl : Control_flow_c.cflow -> Control_flow_c.cflow

val model_for_ctl :
  Control_flow_c.cflow -> Lib_engine.metavars_binding ->
  Control_flow_c.cflow * Lib_engine.label_ctlcocci * nodei list

type pred = Lib_engine.predicate * string Ast_ctl.modif

val mysat :
  (Control_flow_c.cflow * Lib_engine.label_ctlcocci * nodei list) -> 
  (Lib_engine.ctlcocci * (pred list * pred list)) -> 
  (Lib_engine.mvar list * Lib_engine.metavars_binding2) -> 
  ((nodei * Lib_engine.metavars_binding2 * Lib_engine.predicate) list *
     bool *
     Lib_engine.metavars_binding2,
   string) either



val satbis_to_trans_info :  
  (nodei * Lib_engine.metavars_binding2 * Lib_engine.predicate) list -> 
  (nodei * Lib_engine.metavars_binding  * Ast_cocci.rule_elem)  list 

val metavars_binding2_to_binding : 
    Lib_engine.metavars_binding2 -> Lib_engine.metavars_binding

val metavars_binding_to_binding2 : 
    Lib_engine.metavars_binding -> Lib_engine.metavars_binding2

val print_bench : unit -> unit
