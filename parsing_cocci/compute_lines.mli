val compute_lines : bool -> Ast0_cocci.rule -> Ast0_cocci.rule

val compute_statement_dots_lines : bool ->
    Ast0_cocci.statement Ast0_cocci.dots ->
      Ast0_cocci.statement Ast0_cocci.dots

val compute_statement_lines :
    bool -> Ast0_cocci.statement -> Ast0_cocci.statement
