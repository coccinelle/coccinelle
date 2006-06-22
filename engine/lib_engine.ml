
type mvar = string

type metavar_binding_kind2 = 
  | NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string

type predicate =
    TrueBranch | FalseBranch | After
  | Paren of string
  | Match of Ast_cocci.rule_elem


type transformation_info = (Ograph_extended.nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list


