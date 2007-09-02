open Common

(* program -> output filename (often "/tmp/output.c") -> unit *) 
val pp_program : 
  (Parse_c.toplevel2 * Unparse_c.ppmethod) list -> filename -> unit

