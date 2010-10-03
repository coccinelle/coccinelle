(* 'iso is the return type of parse_iso, which currently is
(Ast_cocci.metavar list * Ast0_cocci.anything list list) list *)

(*
val do_unitary :
    (Ast0_cocci.rule * Ast_cocci.metavar list * 'iso) list ->
      (Ast0_cocci.rule * Ast_cocci.metavar list) list ->
	(Ast0_cocci.rule * Ast_cocci.metavar list * 'iso) list
*)

val do_unitary :
  Ast0_cocci.parsed_rule list -> Ast0_cocci.parsed_rule list
