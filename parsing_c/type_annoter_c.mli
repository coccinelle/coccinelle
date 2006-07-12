type environment = (string, Ast_c.fullType) Common.assoc

type context = Ast_c.fullType option

val annotate_expr :
  environment -> context -> Ast_c.expression -> Ast_c.expression

val annotate_program : environment -> Ast_c.program -> Ast_c.program
