open Common

type ppmethod = PPviatok | PPnormal

(* program -> output filename (often "/tmp/output.c") -> unit *) 
val pp_program : (Parse_c.programElement2 * ppmethod) list -> filename -> unit

