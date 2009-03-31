(* uses E rather than A and adds comments indicating the start and end of
each matched term *)

let sgrep_mode = ref false (* no longer supported, subsumed by sgrep2 *)

let show_SP = ref false
let show_iso_failures = ref true

let iso_limit = ref (None : int option) (*(Some 3)*)
