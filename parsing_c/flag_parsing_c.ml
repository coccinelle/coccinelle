let verbose_parsing = ref true
let verbose_type    = ref true

let debug_lexer   = ref false
let debug_etdt    = ref false
let debug_typedef = ref false
let debug_cpp     = ref false

let debug_cfg = ref false

let filter_msg = ref false
let filter_define_error = ref false
let filter_classic_passed = ref false

let pretty_print_type_info = ref false

let show_flow_labels = ref true




let label_strategy_2 = ref false

let ifdef_to_if = ref false

let if0_passing = ref true

let add_typedef_root = ref false


let diff_lines = ref (None : string option) (* number of lines of context *)

let use_cache = ref false
