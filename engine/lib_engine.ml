type mvar = string

type predicate =
  | TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | FallThrough
  | Return (* any exit from the current function *)
  | Exit (* the "exit" node of the current function *)
  | ErrorExit
  | Paren of string
  | Match of Ast_cocci.rule_elem
  | Label of string
  | PrefixLabel of string
  | Include of string Ast_cocci.mcode * string Ast_cocci.mcode

type ctlcocci = (predicate, string) Wrapper_ctl.wrapped_ctl


type metavars_binding = Ast_c.metavars_binding

(* used in ctlcocci_integration *)
type metavar_binding_kind2 = 
  | NormalMetaVal of Ast_c.metavar_binding_kind
  | ParenVal of string
  | LabelVal of int list

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc


type label_ctlcocci = 
 predicate -> 
   (Ograph_extended.nodei * 
    (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution)
   list


type transformation_info = 
 (Ograph_extended.nodei * metavars_binding * Ast_cocci.rule_elem) list
