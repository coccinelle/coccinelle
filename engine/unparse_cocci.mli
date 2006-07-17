exception CantBeInPlus

val pp_list_list_any :
  Ast_c.metavars_binding * string (* current_tabbing *) * (string -> unit) (* pr *) *
  Pretty_print_c.pr_elem_func -> Ast_cocci.anything list list -> unit
