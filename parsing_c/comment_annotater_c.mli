(* !!Annotate via side effects!!. Fill in the comments_around
 * information that was put to empty during parsing.
 *)
val annotate_program :
  Parser_c.token list -> Ast_c.toplevel list -> Ast_c.toplevel list
