open Common.BasicType

val get_files : string -> string list

val test_compare_c : filename -> filename -> unit (* result is in unix code *)

val actions: unit -> Common.cmdline_actions
