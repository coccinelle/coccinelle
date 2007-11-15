exception CantBeInPlus

type pos = Before | After | InPlace

val pp_list_list_any :
  Lib_engine.metavars_binding * 
  (string -> unit) (* pr cocci *) * Pretty_print_c.pr_elem_func (* pr c *) *
    (unit -> unit) (* pr space *) -> 
  Ast_cocci.anything list list -> pos ->
  unit
