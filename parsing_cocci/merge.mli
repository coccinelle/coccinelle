val do_merge :
    Ast_cocci.rule ->
      (Ast_cocci.anything * int * int * int * int) list list list ->
      unit (* updates Ast_cocci.rule argument *)
