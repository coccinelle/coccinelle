open Common

(* program -> output filename (often "/tmp/output.c") -> unit *) 
val pp_rule : 
    Ast_cocci.metavar list (* local metavars only *) ->
      Ast_cocci.rule -> Ast_c.metavars_binding -> filename -> unit

