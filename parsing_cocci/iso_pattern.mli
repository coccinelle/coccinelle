type isomorphism = Ast0_cocci.anything list list

val apply_isos :
    isomorphism list -> Ast0_cocci.rule -> Ast0_cocci.rule
