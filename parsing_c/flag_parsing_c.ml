(*****************************************************************************)
(* convenient globals to pass to parse_c.init_defs *)
(*****************************************************************************)
let path = ref 
  (try (Sys.getenv "YACFE_HOME")
    with Not_found-> "/home/pad/c-yacfe"
  )
let std_h   = ref (Filename.concat !path "data/standard.h")
let common_h   = ref (Filename.concat !path "data/common_macros.h")


let cmdline_flags_macrofile () = 
  [
    "-macro_file", Arg.Set_string std_h,
    " <file> (default=" ^ !std_h ^ ")";
  ]


(*****************************************************************************)
(* used only by cpp_ast_c, not by the parser *)
(*****************************************************************************)
let cpp_i_opts = ref []
let cpp_d_opts = ref []

let cmdline_flags_cpp () = [
    "-D",   Arg.String (fun s -> Common.push2 s cpp_d_opts),
    " <x=y>";
    "-I", Arg.String (fun s -> Common.push2 s cpp_i_opts),
    " <dir>"
  ]

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_lexing = ref true
let verbose_parsing = ref true
let verbose_type    = ref true

let filter_msg = ref false
let filter_msg_define_error = ref false

let filter_define_error = ref false

let filter_passed_level = ref 0

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
    "-filter_msg_define_error",Arg.Set filter_msg_define_error,
    "  filter the error msg";

    "-filter_define_error",Arg.Set filter_define_error,
    "  filter the error, which will not be added in the stat";
    "-filter_passed_level",Arg.Set_int filter_passed_level,"  ";
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

(* cocci specific *)
let label_strategy_2 = ref false

let cmdline_flags_algos () =
  [
    "-l1",                Arg.Clear label_strategy_2, " ";
  ]

(*****************************************************************************)
(* Disable parsing feature (for CC09 and also to see if useful) *)
(*****************************************************************************)

let cpp_directive_passing = ref false
let ifdef_directive_passing = ref false 

let disable_two_pass = ref false
let disable_add_typedef = ref false

let if0_passing = ref true
let add_typedef_root = ref true

let cmdline_flags_parsing_algos () = [

    "-directive_passing",              Arg.Set cpp_directive_passing, 
    "   pass most cpp directives, especially when inside function";
    "-ifdef_passing",              Arg.Set ifdef_directive_passing, 
    "   pass ifdef directives ";

    "-noif0_passing",   Arg.Clear if0_passing, 
    " ";
    "-noadd_typedef_root",   Arg.Clear add_typedef_root, " ";
    "-noadd_typedef",   Arg.Set disable_add_typedef, " ";

    "-disable_two_pass", Arg.Set disable_two_pass, " ";
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

