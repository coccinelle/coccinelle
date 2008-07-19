open Common

(* program -> output filename (often "/tmp/output.c") -> unit *) 
val pp_program : 
  Parse_c.toplevel2 -> filename -> filename ->
    bool (* true if res is an exp *) -> unit

