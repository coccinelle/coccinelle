(* check consistency and possibly change some Ident expression into
 * TypeName, especially in argument to functions. *)
val consistency_checking:
  Ast_c.program -> Ast_c.program
