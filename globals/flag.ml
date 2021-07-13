(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

let sgrep_mode2 = ref false

let show_misc = ref true

let show_transinfo = ref false

let show_trying = ref false

let track_iso_usage = ref false

let worth_trying_opt = ref true

type scanner = IdUtils | Glimpse | CocciGrep | GitGrep | PatchDiff | NoScanner
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

let currentfile = ref (None : string option) (* file of current code *)
let currentfiles = ref ([] : string list) (* starting files of this run *)

let current_element = ref ""
let current_element_pos = ref(lazy((0,0),(0,0)))
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

type c_plus_plus = Off | On of int option (* release year *)
let c_plus_plus = ref Off

let set_c_plus_plus version =
  let version =
    match version with
      None -> None
    | Some "98" -> Some 1998
    | Some "03" -> Some 2003
    | Some "0x" | Some "11" -> Some 2011
    | Some "1y" | Some "14" -> Some 2014
    | Some "1z" | Some "17" -> Some 2017
    | Some "2a" | Some "20" -> Some 2020
    | Some version -> failwith ("Invalid C++ version: " ^ version) in
  c_plus_plus := On version

let ibm = ref false

(* was in main *)
let include_headers = ref false
let no_include_cache = ref false

let parmap_cores      = ref (None : int option)
let parmap_chunk_size = ref (None : int option)

exception UnreadableFile of string

let cocci_attribute_names = ref ([] : string list)
let add_cocci_attribute_names s =
  if not (List.mem s !cocci_attribute_names)
  then cocci_attribute_names := s :: !cocci_attribute_names
