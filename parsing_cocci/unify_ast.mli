type res = NO | MAYBE

val unify_statement_dots :
    Ast_cocci.statement Ast_cocci.dots ->
      Ast_cocci.statement Ast_cocci.dots -> res
