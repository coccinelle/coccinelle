type predicate =
    State of string
  | TrueBranch | FalseBranch | After
  | Paren of string
  | Match of Ast_cocci.rule_elem

val pred2c : predicate -> string

val ast0toctl :
    Ast0_cocci.top_level list -> (predicate,string) Ast_ctl.generic_ctl list
