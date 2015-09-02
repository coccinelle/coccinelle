let sgrep_mode2 = ref false

let show_misc = ref true

let show_transinfo = ref false

let show_trying = ref false

let track_iso_usage = ref false

let worth_trying_opt = ref true

type scanner = IdUtils | Glimpse | CocciGrep | GitGrep | NoScanner
let scanner = ref NoScanner

let pyoutput = ref "coccilib.output.Console"

let ocamlc = ref Commands.ocamlc_cmd
let ocamlopt = ref Commands.ocamlopt_cmd
let ocamldep = ref Commands.ocamldep_cmd
let ocamlfind = ref Commands.ocamlfind_cmd

(*"Some" value is the path with respect to which the patch should be created*)
let patch = ref (None : string option)

let make_hrule = ref (None : string (*dir*) option)
let hrule_per_file = ref true (* if false, then a rule per function *)

let currentfile = ref (None : string option)

let current_element = ref ""
let dir = ref ""

let defined_virtual_rules = ref ([] : string list)
let defined_virtual_env = ref ([] : (string*string) list)

let set_defined_virtual_rules s =
  match Str.split_delim (Str.regexp "=") s with
    [_] -> defined_virtual_rules := s :: !defined_virtual_rules
  | name::vl ->
      let vl = String.concat "=" vl in
      defined_virtual_env := (name,vl) :: !defined_virtual_env
  | _ -> failwith "nothing defined"

let c_plus_plus = ref false
let ibm = ref false

(* was in main *)
let include_headers = ref false

exception UnreadableFile of string
