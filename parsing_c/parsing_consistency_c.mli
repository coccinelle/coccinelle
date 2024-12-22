(* check consistency and possibly change some Ident expression into
 * NamedType, especially in argument to functions. *)
val consistency_checking:
  Ast_c.program -> Ast_c.program
