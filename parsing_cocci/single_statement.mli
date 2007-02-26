val single_statement : Ast0_cocci.rule -> Ast0_cocci.rule

(* probably should be defined in some generic place, but it's not clear
where *)
val contains_only_minus : bool Visitor_ast0.combiner
