val context_neg :
    Ast0_cocci.rule -> Ast0_cocci.rule ->
      (Ast0_cocci.top_level * Ast0_cocci.top_level) list

val minus_table :
    (int list, Ast0_cocci.anything * int Common.set list) Hashtbl.t
val plus_table :
    (int list, Ast0_cocci.anything * int Common.set list) Hashtbl.t
