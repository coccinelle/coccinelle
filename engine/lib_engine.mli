type mvar = string

type predicate =
  | TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | Return (* any exit from the current function *)
  | Paren of string
  | Match of Ast_cocci.rule_elem
  | Label of string
  | PrefixLabel of string


type ctlcocci = (predicate, string) Wrapper_ctl.wrapped_ctl



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
    (Ograph_extended.nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list

val pp_binding_kind2 : metavar_binding_kind2 -> unit
val pp_binding2_ctlsubst : 
    (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution -> unit
val pp_predicate : predicate -> unit
val predicate_to_string : predicate -> string


val pp_ctlcocci_no_mcodekind : ctlcocci -> unit
