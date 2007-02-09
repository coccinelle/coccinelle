open Common

val print_diff_expected_res_and_exit : filename -> filename -> bool -> unit

val testone : 
  string -> bool (* compare_with_expected *) -> string (* iso *) ->unit
val testall : string (* iso *) -> unit
