type result =
    IntSet of int64 list
  | IntBounds of int64 option * int64 option
  | Other of string

val load_external_results : string -> unit
val find_results : Common.filename -> Ast_c.posl -> Ast_c.posl -> result list
