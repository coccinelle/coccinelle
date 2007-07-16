open Common

(*****************************************************************************)
(* work with tests/ *)
(*****************************************************************************)
val testone : 
  string (*test*) -> filename (*iso*) -> bool (*compare_expected*) -> unit
val testall : 
  filename (*iso*) -> unit

(*****************************************************************************)
(* works with tests-big/. The .res, .ok, .failed, .var *)
(*****************************************************************************)
val test_okfailed : 
  (filename (*cocci*) * filename (*iso*)) -> filename (*c*) list -> unit
val test_regression_okfailed : 
  unit -> unit



(*****************************************************************************)
(* the parameter is the result of Cocci.full_engine *)
(*****************************************************************************)
val compare_with_expected : (filename * filename option) list -> unit 


(*****************************************************************************)
(* to test/debug the coccinelle subsystems *)
(*****************************************************************************)

val test_tokens_c : filename -> unit
(* parse and handle some regression information when called with dirmode *)
val test_parse_c  : filename list -> bool (* dirmode *) -> unit
val test_parse_h  : filename list -> bool (* dirmode *) -> unit
val test_parse_ch : filename list -> bool (* dirmode *) -> unit

val test_parse_unparse : filename -> unit

val test_cfg : filename (* foo.c or foo.c:main *) -> unit
val test_typeur : filename -> unit

val test_compare_c : filename -> filename -> unit (* result is in unix code *)
val test_compare_c_hardcoded : unit -> unit

val test_parse_cocci : filename -> filename (* iso *) -> unit

val test_xxx : unit -> unit
