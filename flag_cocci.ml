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

let verbose_cocci = ref true

let windows = ref false

let popl = ref false

let include_headers_for_types = ref false

type include_options =
    I_UNSPECIFIED | I_NO_INCLUDES | I_NORMAL_INCLUDES
  | I_ALL_INCLUDES | I_REALLY_ALL_INCLUDES
let include_options = ref I_UNSPECIFIED

let include_path = ref ([] : string list)
(* if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

let relax_include_path = ref false

let extra_includes = ref ([] : string list)

let timeout = ref (None : int option)

let selected_only = ref false (* just print files that would be treated *)

let use_saved_typedefs = ref true (* hack! *)

(* caching of header file information *)
let cache_threshold = 500
let elem_threshold = 10
