type state =
    True of Ast.pos
  | False of Ast.pos
type entry = string * state list * Ast.pos
type matrix = int list * entry list
