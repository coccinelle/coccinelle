open Common
open Ograph_extended 

type mvar = string

type metavar_binding_kind2 =
    NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string

type substitution = (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution

type ctl_cocci = (Ast0toctl.predicate, mvar) Ast_ctl.generic_ctl

val labels_for_ctl :
  (nodei * Control_flow_c.node) list ->  Ast0toctl.predicate list ->
  (Ast0toctl.predicate,
   (nodei * 
    (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution * 'a list)
   list)
  assoc

val model_for_ctl :
  (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> ctl_cocci ->
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended *
   (Ast0toctl.predicate ->
    (nodei * (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution * 'a list)
    list) *
   nodei list




(* ------------------------------------------------------------------------------ *)
(* normally not needed, but can be useful for debugging purpose *)
(* ------------------------------------------------------------------------------ *)
val ctl_get_all_predicates : ctl_cocci -> Ast0toctl.predicate set
