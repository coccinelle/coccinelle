(*****************************************************************************)
(* convenient globals to pass to parse_c.init_defs *)
(*****************************************************************************)
let path = Filename.concat (Sys.getenv "HOME") "coccinelle"
let std_h   = ref (Filename.concat path "standard.h")

let cmdline_flags_macrofile () = 
  [
    "-macro_file", Arg.Set_string std_h,
    " <file> (default=" ^ !std_h ^ ")";
    "-D",   Arg.Set_string std_h,     
    " short option of -macro_file";
  ]

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_lexing = ref true
let verbose_parsing = ref true
let verbose_type    = ref true

let filter_msg = ref false
let filter_define_error = ref false
let filter_classic_passed = ref false

let pretty_print_type_info = ref false

(* cocci specific *)
let show_flow_labels = ref true


let cmdline_flags_verbose () = 
  [
    "-no_parse_error_msg", Arg.Clear verbose_parsing, " ";
    "-no_verbose_parsing", Arg.Clear verbose_parsing , "  ";
    "-no_verbose_lexing", Arg.Clear verbose_lexing , "  ";
    "-no_type_error_msg",  Arg.Clear verbose_type, " ";
    
    
    "-filter_msg",      Arg.Set  filter_msg , 
    "  filter some cpp message when the macro is a \"known\" cpp construct";
    "-filter_define_error",Arg.Set filter_define_error,"  ";
    "-filter_classic_passed",Arg.Set filter_classic_passed,"  ";
  ]


(*****************************************************************************)
(* debugging *)
(*****************************************************************************)

let debug_lexer   = ref false
let debug_etdt    = ref false
let debug_typedef = ref false
let debug_cpp     = ref false

let debug_unparsing = ref false

let debug_cfg = ref false

(*   "debug C parsing/unparsing", "" *)
let cmdline_flags_debugging () = 
  [
  "-debug_cpp",          Arg.Set  debug_cpp, " ";
  "-debug_lexer",        Arg.Set  debug_lexer , " ";
  "-debug_etdt",         Arg.Set  debug_etdt , "  ";
  "-debug_typedef",      Arg.Set  debug_typedef, "  ";

  "-debug_cfg",          Arg.Set debug_cfg , "  ";
  "-debug_unparsing",      Arg.Set  debug_unparsing, "  ";
  ]

(*****************************************************************************)
(* change algo *)
(*****************************************************************************)

let ifdef_to_if = ref false
let if0_passing = ref true
let add_typedef_root = ref true

(* cocci specific *)
let label_strategy_2 = ref false

let cmdline_flags_algos () =
  [
    "-ifdef",              Arg.Set ifdef_to_if, 
    "   convert ifdef to if (buggy!)";
    "-noif0_passing",   Arg.Clear if0_passing, 
    " ";
    "-noadd_typedef_root",   Arg.Clear add_typedef_root, " ";

    "-l1",                Arg.Clear label_strategy_2, " ";
  ]

(*****************************************************************************)
(* other *)
(*****************************************************************************)

(* for compare_c *)
let diff_lines = ref (None : string option) (* number of lines of context *)

(* for parse_c *)
let use_cache = ref false

let cmdline_flags_other () = 
  [
    "-U", Arg.Int (fun n -> diff_lines := Some (Common.i_to_s n)), 
    "  set number of diff context lines";
    
    "-use_cache", Arg.Set use_cache, 
    "   use .ast_raw pre-parsed cached C file";
  ]


(*****************************************************************************)

