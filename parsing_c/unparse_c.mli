open Common

type ppmethod = PPviatok of Ast_c.info list | PPnormal

val pp_program : 
(* I need the filename because my algorithm need to redo a lexical analysis, so need the file *) 
    filename ->  
    (* we work on a list of programElement, a list of translation_unit *)
      (Ast_c.programElement * ppmethod) list -> 
        unit

