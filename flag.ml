let verbose_parsing = ref true

let debug_lexer = ref false
let debug_etdt = ref false



let action = ref "parse_c"

let dir = ref false


let logfile = "/tmp/error_coccic"
let _chan_logfile = open_out logfile 


let cocci_file = ref ""
let cocci_error_words = ref ""

let classic_patch_file = ref ""
