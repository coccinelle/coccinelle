type predicate =
    TrueBranch | FalseBranch | After
  | Paren of string
  | Match of Ast_cocci.rule_elem * string
  | MatchModif of Ast_cocci.rule_elem * string

val pred2c : predicate -> string

val ast0toctl :
    Ast0_cocci.top_level list -> (predicate,string) Ast_ctl.generic_ctl list
