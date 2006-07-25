type anything =
    Rule_elem        of Ast_cocci.rule_elem
  | Statement        of Ast_cocci.statement
  | StatementDots    of Ast_cocci.statement Ast_cocci.dots

type free_table = (anything,string list) Hashtbl.t

val free_vars : Ast_cocci.rule_with_metavars list ->
  (free_table list) * ((string list) list) *
    ((Ast_cocci.statement -> string list) list)
