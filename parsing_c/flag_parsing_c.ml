(*****************************************************************************)
(* convenient globals. *)
(*****************************************************************************)
let path = ref
  (try (Sys.getenv "YACFE_HOME")
    with Not_found-> "/home/pad/c-yacfe"
  )

(*****************************************************************************)
(* macros *)
(*****************************************************************************)

let macro_dir = "config/macros/"
let mk_macro_path ~cocci_path file =
  Filename.concat cocci_path (macro_dir ^ file)


(* to pass to parse_c.init_defs *)
let std_h   = ref (mk_macro_path ~cocci_path:!path "standard.h")
let common_h   = ref (mk_macro_path ~cocci_path:!path "common_macros.h")


let cmdline_flags_macrofile () =
  [
    "--macro-file-builtins", Arg.Set_string std_h,
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
(* types *)
(*****************************************************************************)
let std_envir = ref (Filename.concat !path "config/envos/environment_splint.h")

let cmdline_flags_envfile () =
  [
    "--env-file", Arg.Set_string std_envir,
    " <file> (default=" ^ !std_envir ^ ")";
  ]

(*****************************************************************************)
(* show *)
(*****************************************************************************)

let show_parsing_error = ref true

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_lexing = ref true
let verbose_parsing = ref true
let verbose_type    = ref true
let verbose_cfg    = ref true
let verbose_annotater = ref true
let verbose_unparsing = ref true
let verbose_visit = ref true
let verbose_cpp_ast = ref true

let filter_msg = ref false
let filter_msg_define_error = ref false

let filter_define_error = ref false

let filter_passed_level = ref 0

let pretty_print_type_info = ref false
let pretty_print_comment_info = ref false
let pretty_print_typedef_value = ref false

(* cocci specific *)
let show_flow_labels = ref true


let cmdline_flags_verbose () =
  [
    "--no-verbose-parsing", Arg.Clear verbose_parsing , "  ";
    "--no-verbose-lexing", Arg.Clear verbose_lexing , "  ";
    "--no-verbose-annotater", Arg.Clear verbose_annotater , "  ";

    "--no-parse-error-msg", Arg.Clear verbose_parsing, " ";
    "--no-type-error-msg",  Arg.Clear verbose_type, " ";


    "--filter-msg",      Arg.Set  filter_msg ,
    "  filter some cpp message when the macro is a \"known\" cpp construct";
    "--filter-msg-define-error",Arg.Set filter_msg_define_error,
    "  filter the error msg";

    "--filter-define-error",Arg.Set filter_define_error,
    "  filter the error, which will not be added in the stat";
    "--filter-passed-level",Arg.Set_int filter_passed_level,"  ";
  ]


(*****************************************************************************)
(* debugging *)
(*****************************************************************************)

let debug_lexer   = ref false
let debug_etdt    = ref false
let debug_typedef = ref false
let debug_cpp     = ref false

let debug_cpp_ast  = ref false

let debug_unparsing = ref false

let debug_cfg = ref false

(*   "debug C parsing/unparsing", "" *)
let cmdline_flags_debugging () =
  [
  "--debug-cpp",          Arg.Set  debug_cpp, " ";
  "--debug-lexer",        Arg.Set  debug_lexer , " ";
  "--debug-etdt",         Arg.Set  debug_etdt , "  ";
  "--debug-typedef",      Arg.Set  debug_typedef, "  ";

  "--debug-cfg",          Arg.Set debug_cfg , "  ";
  "--debug-unparsing",      Arg.Set  debug_unparsing, "  ";
  ]

(*****************************************************************************)
(* checks *)
(*****************************************************************************)

let check_annotater = ref true
let cmdline_flags_checks () =
  [
  "--disable-check-annotater",          Arg.Clear  check_annotater, " ";
  "--enable-check-annotater",          Arg.Set  check_annotater, " ";
  ]

(*****************************************************************************)
(* change algorithm *)
(*****************************************************************************)

(* cocci specific *)
let label_strategy_2 = ref false

let cmdline_flags_algos () =
  [
    "--l1",                Arg.Clear label_strategy_2, " ";
  ]

(*****************************************************************************)
(* Disable parsing feature (for CC09 and also to see if useful) *)
(*****************************************************************************)

let cpp_directive_passing = ref false
let ifdef_directive_passing = ref false
let ifdef_to_if = ref true(*false*)

let disable_multi_pass = ref false
let disable_add_typedef = ref false

let if0_passing = ref true
let add_typedef_root = ref true
let exts_ITU = ref false (* ITU.dk extensions *)

(* defined and undefined constants *)
let add c s = c := (Str.split (Str.regexp ",") s) @ !c
let defined = ref ([] : string list)
let undefined = ref ([] : string list)

let cmdline_flags_parsing_algos () = [

    "--directive-passing",              Arg.Set cpp_directive_passing,
    "   pass most cpp directives, especially when inside function";
    "--ifdef-passing",              Arg.Set ifdef_directive_passing,
    "   pass ifdef directives ";

    "--noif0-passing",   Arg.Clear if0_passing,
    " ";
    "--noadd-typedef-root",   Arg.Clear add_typedef_root, " ";
    "--noadd-typedef",   Arg.Set disable_add_typedef, " ";

    "--disable-multi-pass", Arg.Set disable_multi_pass, " ";
]

(*****************************************************************************)
(* other *)
(*****************************************************************************)

(* for compare_c *)
let diff_lines = ref (None : string option) (* number of lines of context *)

(* for parse_c *)
let use_cache = ref false
let cache_prefix = ref (None : string option)
let cache_limit = ref (None : int option)

let cmdline_flags_other () =
  [
    "-U", Arg.Int (fun n -> diff_lines := Some (Common.i_to_s n)),
    "  set number of diff context lines";

    "--use-cache", Arg.Set use_cache,
    "   use .ast_raw pre-parsed cached C file";
  ]

(*****************************************************************************)
(* for lexing of integer constants *)
(*****************************************************************************)

let int_thresholds =
  ref (None :
	 (int (*int_sz*) * int (*long_sz*) *
	    Big_int.big_int (*uint threshold*) *
	    Big_int.big_int (*long threshold*) *
	    Big_int.big_int (*ulong threshold*)) option)

let set_int_bits n =
  match !int_thresholds with
    None ->
      (*assume long is 2*int; this can be corrected by a subsequent long_bits*)
      let uint_threshold  = Big_int.power_int_positive_int 2 (n-1) in
      let long_threshold  = Big_int.power_int_positive_int 2 n in
      let ulong_threshold = Big_int.power_int_positive_int 2 ((2*n)-1) in
      int_thresholds :=
	Some (n,2*n,uint_threshold,long_threshold,ulong_threshold)
  | Some(int_sz,long_sz,uint_threshold,long_threshold,ulong_threshold) ->
      let uint_threshold = Big_int.power_int_positive_int 2 (n-1) in
      let long_threshold = Big_int.power_int_positive_int 2 n in
      int_thresholds :=
	Some (n,long_sz,uint_threshold,long_threshold,ulong_threshold)

let set_long_bits n =
  match !int_thresholds with
    None ->
      (*assume int is 1/2*int; this can be corrected by a subsequent int_bits*)
      set_int_bits (n/2)
  | Some(int_sz,long_sz,uint_threshold,long_threshold,ulong_threshold) ->
      let ulong_threshold = Big_int.power_int_positive_int 2 (n-1) in
      int_thresholds :=
	Some (int_sz,n,uint_threshold,long_threshold,ulong_threshold)

(*****************************************************************************)
(* unparsing strategy *)
(*****************************************************************************)

type spacing = LINUX | SMPL
let spacing = ref LINUX

let set_linux_spacing _ = spacing := LINUX (*follow the conventions of Linux*)
let set_smpl_spacing _ = spacing := SMPL   (*use spacing from the SP*)

let max_width = 78

(*****************************************************************************)

(* drop back edges made by proper loop constructs -
   unsafe but more efficient *)
let no_loops = ref false
let no_gotos = ref false

let keep_comments = ref false (* unparsing *)

let parsing_header_for_types = ref false
