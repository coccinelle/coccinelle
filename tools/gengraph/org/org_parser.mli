exception Error

type token = 
  | TVIEW
  | TVERT
  | TTODO
  | TSTODO
  | TSTAR
  | TSLASH
  | TRAB
  | TPLUS
  | TORG
  | TLINB
  | TLAB
  | TInt of (int)
  | TId of (string)
  | TFACE
  | TEQUAL
  | TDASH
  | TCONFIG
  | TCOLON
  | TCOLE
  | TCOLB
  | EOL
  | EOF


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast_org.orgs)