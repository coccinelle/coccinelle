(* uses E rather than A and adds comments indicating the start and end of
each matched term *)

let sgrep_mode = ref false (* no longer supported, subsumed by sgrep2 *)

let show_SP = ref false
let show_iso_failures = ref true

let iso_limit = ref (None : int option) (*(Some 3)*)

let defined_virtual_rules = ref []
let undefined_virtual_rules = ref []

let set_defined_virtual_rules s =
  (if List.mem s !undefined_virtual_rules
  then
    failwith
      (Printf.sprintf "virtual method %s cannot be both defined and undefined"
	 s));
  defined_virtual_rules := s :: !defined_virtual_rules

let set_undefined_virtual_rules s =
  (if List.mem s !defined_virtual_rules
  then
    failwith
      (Printf.sprintf "virtual method %s cannot be both defined and undefined"
	 s));
  undefined_virtual_rules := s :: !undefined_virtual_rules
