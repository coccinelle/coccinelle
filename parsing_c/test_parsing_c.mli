open Common.BasicType

val test_tokens_c : filename -> unit

(* parse and handle some regression information when called with dirmode *)
val test_parse_c  : filename list -> unit
val test_parse_h  : filename list -> unit
val test_parse_ch : filename list -> unit

val test_parse_unparse : filename -> unit

val test_cfg : filename (* foo.c or foo.c:main *) -> unit
val test_type_c : filename -> unit

val test_compare_c : filename -> filename -> unit (* result is in unix code *)
val test_compare_c_hardcoded : unit -> unit


val test_xxx : string list ->  unit


val actions: unit -> Common.cmdline_actions
