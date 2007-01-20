open Ograph_extended

(*****************************************************************************)
(* the different ctl formula related types *)
(*****************************************************************************)
type mvar = string

type predicate =
    TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | FallThrough
  | Return (* any exit from the current function *)
  | Exit (* the "exit" node of the current function *)
  | ErrorExit
  | Paren of string
  | Match of Ast_cocci.rule_elem
  | Label of string
  | PrefixLabel of string

type ctlcocci = (predicate, string) Wrapper_ctl.wrapped_ctl


(*****************************************************************************)
(* the different binding types *)
(*****************************************************************************)
type metavars_binding = Ast_c.metavars_binding

(* used in ctlcocci_integration *)
type metavar_binding_kind2 = 
  | NormalMetaVal of Ast_c.metavar_binding_kind
  | ParenVal of string
  | LabelVal of int list

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc



(*****************************************************************************)
(* the CTL model related types *)
(*****************************************************************************)
type label_ctlcocci = 
 predicate -> 
 (nodei * (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution) list

type model = Control_flow_c.cflow * label_ctlcocci * nodei list




type transformation_info = 
 (nodei * metavars_binding * Ast_cocci.rule_elem) list
