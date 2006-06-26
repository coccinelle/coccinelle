type anything =
    DotsExpr of Ast0_cocci.expression Ast0_cocci.dots
  | DotsParam of Ast0_cocci.parameterTypeDef Ast0_cocci.dots
  | DotsStmt of Ast0_cocci.statement Ast0_cocci.dots
  | Ident of Ast0_cocci.ident
  | Expr of Ast0_cocci.expression
  | TypeC of Ast0_cocci.typeC
  | Param of Ast0_cocci.parameterTypeDef
  | Decl of Ast0_cocci.declaration
  | Stmt of Ast0_cocci.statement
  | Top of Ast0_cocci.top_level

val context_neg :
    Ast0_cocci.rule -> Ast0_cocci.rule ->
      (Ast0_cocci.top_level * Ast0_cocci.top_level) list

val minus_table : (int list, anything * int list) Hashtbl.t
val plus_table : (int list, anything * int list) Hashtbl.t
