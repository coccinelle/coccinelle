open Common

type ppmethod = PPviatok | PPnormal

val pp_program :  
    filename ->  (* I need the filename because my algorithm need to redo
                    a lexical analysis, so need the file 
                 *)
      ((Ast_c.programElement * ppmethod) * (* the Ast of one translation_unit, and the method *)
         ('a * ('b * 'c) * 'd *            (* some info on position in the file for this translation_unit (not used) *)
          (Ast_c.info list))               (* the flat list of tokens involved in the Ast *)
      ) list -> (* we work on a list of programElement, a list of translation_unit *)
    unit

