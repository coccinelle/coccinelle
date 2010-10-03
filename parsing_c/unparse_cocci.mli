exception CantBeInPlus

type pos = Before | After | InPlace

val pp_list_list_any :
  Ast_c.metavars_binding list *
  (* pr cocci *)
  (string -> int (*line*) -> int (*lcol*) -> int (*rcol*) -> unit) *
    (Ast_c.info -> unit) (* pr c *) *
    (unit -> unit) (* pr C space *) *
    (unit -> unit) (* pr space *) *
    (string -> unit) (* pr arity *) *
    (int (*line*) -> int (*lcol*) -> unit) (* pr barrier *) *
    (unit -> unit) (* indent *) * (unit -> unit) (* unindent *) -> 
  bool (*true if generating*) -> Ast_cocci.anything list list -> pos ->
  unit
