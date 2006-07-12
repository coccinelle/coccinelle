type mvar = string

type metavar_binding_kind2 = 
  | NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string
  | LabelValue of int list

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc

type predicate =
  | TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | Return (* any exit from the current function *)
  | Paren of string
  | Match of Ast_cocci.rule_elem
  | Label of string
  | PrefixLabel of string



type transformation_info = 
    (Ograph_extended.nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list

val pp_binding_kind2 : metavar_binding_kind2 -> unit
val pp_predicate : predicate -> unit
val predicate_to_string : predicate -> string
