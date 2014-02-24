(* uses E rather than A and adds comments indicating the start and end of
each matched term *)

let sgrep_mode = ref false (* no longer supported, subsumed by sgrep2 *)

let show_SP = ref false
let show_iso_failures = ref true

(* interpret the patch in an inverted way *)
let interpret_inverted = ref false

let iso_limit = ref (None : int option) (*(Some 3)*)
let disabled_isos = ref ([] : string list)

(* Used to debug embedded ML scripts *)
let keep_ml_script = ref false

let id_utils_index = ref ".id-utils.index" (* default id-utils value *)

let in_minus = ref false

let debug_parse_cocci = ref false
