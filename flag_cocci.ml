(* the inputs *)
let show_c = ref false
let show_cocci = ref false

(* the output *)
let show_diff = ref true

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

let verbose_cocci = ref true

let windows = ref false

let popl = ref false

let ifdef_to_if = ref true(*false*)

type include_options =
    I_UNSPECIFIED | I_NO_INCLUDES | I_NORMAL_INCLUDES | I_ALL_INCLUDES
let include_options = ref I_UNSPECIFIED

let include_path = ref (None : string option)
(* if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

let relax_include_path = ref false 

let timeout = ref (None : int option)

let worth_trying_opt = ref true
