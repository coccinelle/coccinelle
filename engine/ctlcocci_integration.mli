open Ograph_extended 


val labels_for_ctl :
  (nodei * Control_flow_c.node) list -> Lib_engine.metavars_binding -> 
  Lib_engine.label_ctlcocci


val fix_flow_ctl : Control_flow_c.cflow -> Control_flow_c.cflow

val model_for_ctl :
  Control_flow_c.cflow -> Lib_engine.metavars_binding -> Lib_engine.model


type pred = Lib_engine.predicate * string Ast_ctl.modif

val mysat :
  Lib_engine.model -> 
  (Lib_engine.ctlcocci * (pred list * pred list)) -> 
  (Lib_engine.mvar list * Lib_engine.metavars_binding) -> 
  (Lib_engine.transformation_info *  bool * Lib_engine.metavars_binding,
   string) 
  Common.either


val print_bench : unit -> unit
