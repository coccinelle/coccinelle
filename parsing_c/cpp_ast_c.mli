type cpp_option = 
  | I of Common.filename
  | D of string * string option

val cpp_option_of_cmdline: 
  Common.dirname list (* -I *) * string list (* -D *) -> cpp_option list

val cpp_expand_include: 
  cpp_option list -> Common.dirname (* start point for relative paths *) -> 
  Ast_c.program -> Ast_c.program


val cpp_ifdef_statementize: Ast_c.program -> Ast_c.program

