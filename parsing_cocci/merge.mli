val do_merge :
    Ast0_cocci.rule ->
      (Ast_cocci.anything * int * int * int * int) list list list ->
      unit (* updates Ast0_cocci.rule argument *)
