module Int64Set : Set.S with type elt = int64

type result =
    IntSet of Int64Set.t
  | IntBounds of int64 option * int64 option
  | Other of string

val load_external_results : string -> unit
val find_results : Common.filename -> Ast_c.posl -> Ast_c.posl -> result list

val intersect_results : result -> result -> result option

val satisfy :
  (result list -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val satisfy1 :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool

val has_any_result : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val for_all :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val exists :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool

val single_int : int64 -> result -> bool
val contains_int : int64 -> result -> bool
val has_only_nul : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val has_also_nul : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val has_also_int : int64 -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
