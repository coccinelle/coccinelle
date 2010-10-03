exception CantBeInPlus

type pos = Before | After | InPlace

val pp_list_list_any :
  Ast_c.metavars_binding * 
  (string -> unit) (* pr cocci *) * Pretty_print_c.pr_elem_func (* pr c *) *
    (unit -> unit) (* pr space *) *
    (unit -> unit) (* indent *) * (unit -> unit) (* unindent *) -> 
  bool (*true if generating*) -> Ast_cocci.anything list list -> pos ->
  unit
