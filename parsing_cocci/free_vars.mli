(* Used after things can only have one binding.  Positions can have many
bindings.  These are combined in ctlcocciintegration, ie after the CTL
generation. *)

val free_vars : Ast_cocci.rule_with_metavars list ->
  (Ast_cocci.metavar list list) * (Ast_cocci.rule list) *
    (((Ast_cocci.meta_name list) list) list) (*fvs of the rule*) *
    (((Ast_cocci.meta_name list) list) list) (*negated position vars*) *
    (((Ast_cocci.meta_name list) list) list) (*used after list*) *
    (((Ast_cocci.meta_name list) list) list) (*positions list*)
