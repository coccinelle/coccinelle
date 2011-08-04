exception CantBeInPlus

type pos = Before | After | InPlace
type nlhint = StartBox | EndBox | SpaceOrNewline of string ref

val pp_list_list_any :
  Ast_c.metavars_binding list *
  (* pr cocci *)
  (string -> int (*line*) -> int (*lcol*) -> int (*rcol*) -> nlhint option
    -> unit) *
    (Ast_c.info -> unit) (* pr c *) *
    (unit -> unit) (* pr C space *) *
    (unit -> unit) (* pr space *) *
    (string -> unit) (* pr arity *) *
    (int (*line*) -> int (*lcol*) -> unit) (* pr barrier *) *
    (unit -> unit) (* indent *) * (bool -> unit) (* unindent *) *
    (unit -> unit) (* eat_space *)->
  bool (*true if generating*) -> Ast_cocci.anything list list -> pos ->
  unit
