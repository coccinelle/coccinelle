(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* the inputs *)
let show_c = ref false
let show_cocci = ref false

(* the output *)
let show_diff = ref true
let force_diff = ref false (*show diff even if there are only space changes*)

(* the derived inputs *)
let show_flow = ref false
let show_before_fixed_flow = ref false

let show_ctl_tex =  ref false
let show_ctl_text = ref false

let inline_let_ctl = ref false
let show_mcodekind_in_ctl = ref false

(* the "underived" outputs *)
let show_binding_in_out = ref false
let show_dependencies =   ref false

let inplace_modif = ref false

let verbose_cocci = ref true

let windows = ref false

let popl = ref false

let timeout = ref (None : int option)

let selected_only = ref false (* just print files that would be treated *)

let use_saved_typedefs = ref true (* hack! *)
