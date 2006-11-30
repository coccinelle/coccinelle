open Common

type ppmethod = PPviatok of Ast_c.info list | PPnormal

(* I need the filename because my algorithm need to redo a lexical analysis.
 * We work on a list of programElement, a list of translation_unit.
 * First arg is infile, second arg is outfile (often "/tmp/output.c").
*) 

val pp_program : 
 (Ast_c.programElement * ppmethod) list -> filename -> filename -> unit

