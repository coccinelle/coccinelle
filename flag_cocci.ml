(* the inputs *)
let show_c = ref false
let show_cocci = ref false

(* the output *)
let show_diff = ref true

(* the derived inputs *)
let show_flow = ref false
let show_before_fixed_flow = ref false

let show_ctl_tex = ref false
let show_ctl_text = ref true

let inline_let_ctl = ref false
let show_mcodekind_in_ctl = ref false

(* the "underived" outputs *)
let show_transinfo = ref true
let show_binding_in_out = ref false


let unparse_c2 = ref true

let windows = ref false

let popl = ref false

let all_includes = ref false
let include_path = ref "include"
(* if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)
let relax_include_path = ref false 

let timeout = ref (None : int option)

let worth_trying_opt = ref true

(*"Some" value is the path with respect to which the patch should be created*)
let patch = ref (None : string option)
