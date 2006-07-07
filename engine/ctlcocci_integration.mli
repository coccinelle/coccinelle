open Common
open Ograph_extended 


val labels_for_ctl :
  (nodei * Control_flow_c.node) list ->
  (Lib_engine.predicate ->
   (nodei * 
    (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution)
   list)

val model_for_ctl :
  (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended *
   (Lib_engine.predicate ->
    (nodei * (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution)
    list) *
   nodei list


val mysat :
       ((Control_flow_c.node, Control_flow_c.edge) ograph_extended *
        (Lib_engine.predicate ->
           (nodei * (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution)
            list) *
         nodei list)
         -> (Lib_engine.predicate, Lib_engine.mvar) Wrapper_ctl.wrapped_ctl
           -> (Ograph_extended.nodei * 
                 (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *
                 Lib_engine.predicate) list


val satbis_to_trans_info :  
  (nodei * (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *  Lib_engine.predicate) list
     -> (nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list



(*val mysat_noclean :
       ((Control_flow_c.node, Control_flow_c.edge) ograph_extended *
        (Lib_engine.predicate ->
           (nodei * (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution)
            list) *
         nodei list)
         -> (Lib_engine.predicate, Lib_engine.mvar) Wrapper_ctl.wrapped_ctl
          -> (nodei *
                (Lib_engine.mvar, (Lib_engine.metavar_binding_kind2, Lib_engine.predicate) Wrapper_ctl.wrapped_binding
                ) Ast_ctl.generic_subst list *
                (nodei, (Lib_engine.mvar, (Lib_engine.metavar_binding_kind2, Lib_engine.predicate) Wrapper_ctl.wrapped_binding
                  )  Ast_ctl.generic_subst list, 'a list)
                Ast_ctl.generic_witness list)
              list
*)
