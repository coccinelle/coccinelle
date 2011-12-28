open Common
module FC = Flag_cocci

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in "the spatch options" section below), the
 * spatch program also depends on external files, described in
 * globals/config.ml, mainly a standard.h and standard.iso file *)

let cocci_file = ref ""

let output_file = ref ""
let inplace_modif = ref false  (* but keeps nothing *)
let backup_suffix =
  ref (None : string option) (* suffix for backup if one is desired *)
let outplace_modif = ref false (* generates a .cocci_res  *)
let preprocess = ref false     (* run the C preprocessor before cocci *)
let compat_mode = ref false
let ignore_unknown_opt = ref false

(* somehow obsolete now *)
let dir = ref false

let include_headers = ref false
let kbuild_info = ref ""

let macro_file = ref ""

(* test mode *)
let test_mode = ref false
let test_all = ref false
let test_okfailed = ref false
let test_regression_okfailed = ref false
let expected_score_file = ref ""


(* action mode *)
let action = ref ""

(* works with -test but also in "normal" spatch mode *)
let compare_with_expected = ref false

let distrib_index = ref (None : int option)
let distrib_max   = ref (None : int option)
let mod_distrib   = ref false

(*****************************************************************************)
(* Profiles *)
(*****************************************************************************)

(* pair of  (list of flags to set true, list of flags to set false *)
let very_quiet_profile = (
  [
  ],
  [
    (* FC.show_diff;   just leave this as it is *)

    Common.print_to_stderr;
    Flag.show_misc;
    Flag.show_trying;
    Flag.show_transinfo;

    FC.show_c;
    FC.show_cocci;
    FC.show_flow;
    FC.show_before_fixed_flow;
    FC.show_ctl_tex;
    FC.show_ctl_text;
    FC.show_binding_in_out;

    FC.verbose_cocci;

    Flag_parsing_c.show_parsing_error;

    Flag_parsing_c.verbose_lexing;
    Flag_parsing_c.verbose_parsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_cfg;
    Flag_parsing_c.verbose_unparsing;
    Flag_parsing_c.verbose_visit;
    Flag_parsing_c.verbose_cpp_ast;

    Flag_matcher.verbose_matcher;
    Flag_matcher.debug_engine;

    Flag_parsing_c.debug_unparsing;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;

    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;


  ])

let quiet_profile = (
  [
    Common.print_to_stderr
  ],
  [
    (* FC.show_diff;   just leave this as it is *)

    Flag.show_misc;
    Flag.show_trying;
    Flag.show_transinfo;

    FC.show_c;
    FC.show_cocci;
    FC.show_flow;
    FC.show_before_fixed_flow;
    FC.show_ctl_tex;
    FC.show_ctl_text;
    FC.show_binding_in_out;

    FC.verbose_cocci;

    Flag_parsing_c.show_parsing_error;

    Flag_parsing_c.verbose_lexing;
    Flag_parsing_c.verbose_parsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_cfg;
    Flag_parsing_c.verbose_unparsing;
    Flag_parsing_c.verbose_visit;
    Flag_parsing_c.verbose_cpp_ast;

    Flag_matcher.verbose_matcher;
    Flag_matcher.debug_engine;

    Flag_parsing_c.debug_unparsing;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;

    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;


  ])

(* some information that is useful in seeing why a semantic patch doesn't
work properly *)
let debug_profile = (
  [
    Common.print_to_stderr;
    Flag.show_misc;
    Flag.show_transinfo;

    FC.show_diff;
    FC.show_cocci;
    FC.show_binding_in_out;
    FC.show_dependencies;

    Flag_parsing_cocci.keep_ml_script;
    Flag_parsing_cocci.show_iso_failures;

    FC.verbose_cocci;

    Flag_parsing_c.verbose_cfg;
    Flag_parsing_c.verbose_unparsing;
    Flag_parsing_c.verbose_visit;

    Flag_matcher.verbose_matcher;

    Flag_parsing_c.show_parsing_error;
  ],
  [

    Flag.show_misc;

    FC.show_c;
    FC.show_flow;
    FC.show_before_fixed_flow;
    FC.show_ctl_tex;
    FC.show_ctl_text;

    Flag_parsing_cocci.show_SP;
    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;
    Flag_matcher.debug_engine;
    Flag_parsing_c.debug_unparsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_parsing;
  ])

let pad_profile = (
  [
    FC.show_diff;
    Common.print_to_stderr;
  ],
  [

    Flag.show_misc;
    Flag.show_transinfo;

    FC.show_c;
    FC.show_cocci;
    FC.show_flow;
    FC.show_before_fixed_flow;
    FC.show_ctl_tex;
    FC.show_ctl_text;
    FC.show_binding_in_out;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;
    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;
    Flag_matcher.debug_engine;
    Flag_parsing_c.debug_unparsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_parsing;
  ])

let run_profile p =
  let (set_to_true, set_to_false) = p in
  List.iter (fun x -> x := false) set_to_false;
  List.iter (fun x -> x := true) set_to_true

(*****************************************************************************)
(* The spatch options *)
(*****************************************************************************)

let usage_msg =
  "Usage: " ^ basename Sys.argv.(0) ^
    " -sp_file <SP> <infile> [-o <outfile>] [-iso_file <iso>] [options]" ^
    "\n" ^ "Options are:"

(* forward reference trick *)
let short_usage_func = ref (fun () -> ())
let long_usage_func  = ref (fun () -> ())


(* The short_options are user-oriented. The other options are for
 * the developers of coccinelle or advanced-users that know
 * quite well the underlying semantics of coccinelle.
 *)


(* will be printed when use only ./spatch. For the rest you have to
 * use -longhelp to see them.
 *)
let short_options = [
  "-sp_file",  Arg.Set_string cocci_file,
  " <file> the semantic patch file";

  "-o", Arg.Set_string output_file,
  "   <file> the output file";
  "-in_place", Arg.Set inplace_modif,
  "   do the modification on the file directly";
  "-backup_suffix", Arg.String (function s -> backup_suffix := Some s),
  "   suffix to use when making a backup for inplace";
  "-out_place", Arg.Set outplace_modif,
  "   store modifications in a .cocci_res file";

  "-U", Arg.Int (fun n -> Flag_parsing_c.diff_lines := Some (i_to_s n)),
  "  set number of diff context lines";
  "-partial_match",        Arg.Set Flag_ctl.partial_match,
  "    report partial matches of the SP on the C file";

  "-iso_file", Arg.Set_string Config.std_iso,
  " <file> (default=" ^ !Config.std_iso ^")";
  "-macro_file", Arg.Set_string macro_file,
  " <file>";
  "-macro_file_builtins", Arg.Set_string Config.std_h,
  " <file> (default=" ^ !Config.std_h ^ ")";

  "-recursive_includes",
  Arg.Unit (function _ -> FC.include_options := FC.I_REALLY_ALL_INCLUDES),
  "  causes all available include files, both those included in the C file(s) and those included in header files, to be used";
  "-all_includes",
  Arg.Unit (function _ -> FC.include_options := FC.I_ALL_INCLUDES),
  "  causes all available include files included in the C file(s) to be used";
  "-no_includes",
  Arg.Unit (function _ -> FC.include_options := FC.I_NO_INCLUDES),
  "  causes not even local include files to be used";
  "-local_includes",
  Arg.Unit (function _ -> FC.include_options := FC.I_NORMAL_INCLUDES),
  "  causes local include files to be used";
  "-ignore_unknown_options", Arg.Set ignore_unknown_opt,
  "    For integration in a toolchain (must be set before the first unknown option)";
  "-include_headers", Arg.Set include_headers,
  "    process header files independently";
  "-I",   Arg.String (fun x ->
			FC.include_path:= x::!FC.include_path
		     ),
  "  <dir> containing the header files (optional)";

  "-preprocess", Arg.Set preprocess,
  " run the C preprocessor before applying the semantic match";

  "-c", Arg.Set compat_mode, " gcc/cpp compatibility mode";

  "-dir", Arg.Set dir,
  "    <dir> process all files in directory recursively";

  "-use_glimpse", Arg.Unit (function _ -> Flag.scanner := Flag.Glimpse),
  "    works with -dir, use info generated by glimpseindex";
  "-use_google", Arg.String (function s -> Flag.scanner := Flag.Google s),
  "    find relevant files using google code search";
  "-use_idutils", Arg.Unit (function s -> Flag.scanner := Flag.IdUtils),
  "    find relevant files using id-utils";
  "-patch",
    Arg.String (function s -> Flag.patch := Some (Cocci.normalize_path s)),
  ("    <dir> path name with respect to which a patch should be created\n"^
   "    \"\" for a file in the current directory");
  "-kbuild_info", Arg.Set_string kbuild_info,
  "    <file> improve -dir by grouping related c files";
  "-pyoutput", Arg.Set_string Flag.pyoutput,
  "    Sets output routine: Standard values: <coccilib.output.Gtk|coccilib.output.Console>";


  "-version",   Arg.Unit (fun () ->
    let withpython = if Pycocci.python_support then "with" else "without" in
    let whichregexp =
      if !Regexp.pcre_support then "with PCRE support"
      else "with Str regexp support "
    in
    pr2 (spf "spatch version %s %s Python support and %s" Config.version withpython whichregexp);
    exit 0;
  ),
    "  guess what";

  "-date",   Arg.Unit (fun () ->
    pr2 "version: $Date$";
    raise (Common.UnixExit 0)
    ),
  "   guess what";

  "-shorthelp", Arg.Unit (fun () ->
    !short_usage_func();
    raise (Common.UnixExit 0)
  ),
  "    see short list of options";
  "-longhelp", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
    ),
  "    see all the available options in different categories";
  "-help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
  ),
  " ";
  "--help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
  ),
  " ";
]

(* the format is a list of triples:
 *  (title of section * (optional) explanation of sections * option list)
 *)
let other_options = [
  "aliases and obsolete options",
  "",
  [
    "-sp", Arg.Set_string cocci_file,     " short option of -sp_file";
    "-iso", Arg.Set_string Config.std_iso,   " short option of -iso_file";

    "-cocci_file", Arg.Set_string cocci_file,
    "   <file> the semantic patch file";
(*     "-c", Arg.Set_string cocci_file,     " short option of -sp_file"; *)
  ];

  "most useful show options",
  "",
  [
    "-show_diff"           , Arg.Set FC.show_diff, " ";
    "-no_show_diff"           , Arg.Clear FC.show_diff, " ";
    "-force_diff"           , Arg.Set FC.force_diff,
       "show diff even if only spacing changes";
    "-show_flow"              , Arg.Set FC.show_flow,        " ";
    (* works in conjunction with -show_ctl_text *)
    "-ctl_inline_let",
    Arg.Unit
    (function _ -> FC.show_ctl_text := true; FC.inline_let_ctl := true), " ";
    "-ctl_show_mcodekind",
    Arg.Unit
    (function _ -> FC.show_ctl_text := true; FC.show_mcodekind_in_ctl := true),
    " ";
    "-show_bindings",        Arg.Set FC.show_binding_in_out, " ";
    "-show_transinfo",    Arg.Set Flag.show_transinfo, " ";
    "-show_misc",         Arg.Set Flag.show_misc, " ";
    "-show_trying",          Arg.Set Flag.show_trying,
    " show the name of each function being processed";
    "-show_dependencies",
    Arg.Unit (function _ -> FC.show_dependencies := true;
      FC.show_binding_in_out := true),
    " show the dependencies related to each rule";
  ];

  "verbose subsystems options",
  "",
  [
    "-verbose_ctl_engine",
    Arg.Unit (function _ ->
      Flag_ctl.verbose_ctl_engine := true; FC.show_ctl_text := true) , " ";
    "-verbose_match",        Arg.Set Flag_ctl.verbose_match, " ";
    "-verbose_engine",       Arg.Set Flag_matcher.debug_engine,    " ";
    "-graphical_trace",      Arg.Set Flag_ctl.graphical_trace, "  generate a pdf file representing the matching process";
    "-gt_without_label",
     Arg.Unit (function _ ->
       Flag_ctl.graphical_trace := true; Flag_ctl.gt_without_label := true),
       "  remove graph label (requires option -graphical_trace)";

    "-parse_error_msg", Arg.Set Flag_parsing_c.show_parsing_error, " ";
    "-verbose_parsing",
       Arg.Unit (fun _ -> Flag_parsing_c.verbose_parsing := true;
	 Flag_parsing_c.show_parsing_error := true), " ";
    "-type_error_msg",  Arg.Set Flag_parsing_c.verbose_type, " ";
    (* could also use Flag_parsing_c.options_verbose *)
  ];

  "other show options",
  "",
  [
    "-show_c"                 , Arg.Set FC.show_c,           " ";
    "-show_cocci"             , Arg.Set FC.show_cocci,       " ";
    "-show_before_fixed_flow" , Arg.Set FC.show_before_fixed_flow,  " ";
    "-show_ctl_tex"           , Arg.Set FC.show_ctl_tex,     " ";
    "-show_ctl_text"          , Arg.Set FC.show_ctl_text,     " ";
    "-show_SP"             ,    Arg.Set Flag_parsing_cocci.show_SP,  " ";
  ];


  "debug C parsing/unparsing",
  "",
  [
    "-debug_cpp",          Arg.Set  Flag_parsing_c.debug_cpp, " ";
    "-debug_lexer",        Arg.Set  Flag_parsing_c.debug_lexer , " ";
    "-debug_etdt",         Arg.Set  Flag_parsing_c.debug_etdt , "  ";
    "-debug_typedef",      Arg.Set  Flag_parsing_c.debug_typedef, "  ";

    "-filter_msg",         Arg.Set  Flag_parsing_c.filter_msg ,
    "  filter some cpp message when the macro is a \"known\" cpp construct";
    "-filter_define_error",     Arg.Set Flag_parsing_c.filter_define_error,"  ";
    "-filter_msg_define_error", Arg.Set Flag_parsing_c.filter_msg_define_error,
    "  filter the error msg";
    "-filter_passed_level",     Arg.Set_int Flag_parsing_c.filter_passed_level,"  ";
(*  debug cfg doesn't seem to have any effect, so drop it as an option *)
(*  "-debug_cfg",          Arg.Set Flag_parsing_c.debug_cfg , "  "; *)
    "-debug_unparsing",         Arg.Set  Flag_parsing_c.debug_unparsing, "  ";

  ];
  (* could use Flag_parsing_c.options_debug_with_title instead *)


  "shortcut for enabling/disabling a set of debugging options at once",
  "",
  [
    (* todo: other profile ? *)
    "-quiet",   Arg.Unit (fun () -> run_profile quiet_profile), " ";
    "-very_quiet",   Arg.Unit (fun () -> run_profile very_quiet_profile), " ";
    "-debug",   Arg.Unit (fun () -> run_profile debug_profile), " ";
    "-pad",     Arg.Unit (fun () -> run_profile pad_profile),   " ";

  ];

  "bench options",
  "",
  [
    "-profile", Arg.Unit (function () -> Common.profile := Common.PALL) ,
    "   gather timing information about the main coccinelle functions";
    "-bench", Arg.Int (function x -> Flag_ctl.bench := x),
    "   <level> for profiling the CTL engine";
    "-timeout", Arg.Int (fun x -> FC.timeout := Some x),
    "   <sec> timeout in seconds";
    "-steps", Arg.Int (fun x -> Flag_ctl.steps := Some x),
    "   max number of model checking steps per code unit";
    "-iso_limit", Arg.Int (fun x -> Flag_parsing_cocci.iso_limit := Some x),
    "   max depth of iso application";
    "-no_iso_limit", Arg.Unit (fun _ -> Flag_parsing_cocci.iso_limit := None),
    "   disable limit on max depth of iso application";
    "-track_iso", Arg.Set Flag.track_iso_usage,
    "   gather information about isomorphism usage";
    "-disable_iso",
    Arg.String
    (fun s -> Flag_parsing_cocci.disabled_isos :=
      s :: !Flag_parsing_cocci.disabled_isos),
    "   disable a specific isomorphism";
    "-profile_iso",
    Arg.Unit
    (function () ->
      Common.profile :=
	(*post_engine not included, because it doesn't use isos*)
	PSOME ["parse cocci";"mysat";"asttoctl2";"pre_engine";"full_engine"]),
    "   gather information about the cost of isomorphism usage"
  ];



  "change of algorithm options",
  "",
  [
    "-popl", Arg.Set FC.popl,
    "    simplified SmPL, for the popl paper";

    "-popl_mark_all",
    Arg.Unit
    (function _ -> FC.popl := true; Flag_popl.mark_all := true),
    "    simplified SmPL, for the popl paper";

    "-popl_keep_all_wits",
    Arg.Unit
    (function _ -> FC.popl := true; Flag_popl.keep_all_wits := true),
    "    simplified SmPL, for the popl paper";

    "-hrule", Arg.String
    (function s ->
      Flag.make_hrule := Some s; FC.include_options := FC.I_NO_INCLUDES),
    "    semantic patch generation";

    "-keep_comments", Arg.Set Flag_parsing_c.keep_comments,
    "   keep comments around removed code";

    "-loop",              Arg.Set Flag_ctl.loop_in_src_code,    " ";
    "-no_loops",          Arg.Set Flag_parsing_c.no_loops,
    "   drop all back edges derived from looping constructs - unsafe";
    "-no_gotos",          Arg.Set Flag_parsing_c.no_gotos,
    "   drop all jumps derived from gotos - unsafe";
    "-no_saved_typedefs", Arg.Clear Flag_cocci.use_saved_typedefs,
    "   drop all inferred typedefs from one parse of some code to the next";

    "-ocaml_regexps", Arg.Clear Regexp.pcre_support,
    "   use OCaml Str regular expressions for constraints";

    "-l1",                Arg.Clear Flag_parsing_c.label_strategy_2, " ";
    "-ifdef_to_if",       Arg.Set FC.ifdef_to_if,
    "   convert ifdef to if (experimental)";
    "-no_ifdef_to_if",              Arg.Clear FC.ifdef_to_if,
    "   convert ifdef to if (experimental)";

    "-disable_multi_pass", Arg.Set Flag_parsing_c.disable_multi_pass, " ";

    "-noif0_passing",      Arg.Clear Flag_parsing_c.if0_passing,
    " ";
    "-noadd_typedef_root", Arg.Clear Flag_parsing_c.add_typedef_root, " ";
    (* could use Flag_parsing_c.options_algo instead *)


    "-disallow_nested_exps", Arg.Set Flag_matcher.disallow_nested_exps,
       " disallow an expresion pattern from matching a term and its subterm";
    "-disable_worth_trying_opt", Arg.Clear FC.worth_trying_opt,
    "  ";
    "-only_return_is_error_exit",
    Arg.Set Flag_matcher.only_return_is_error_exit,
    "if this flag is not set, then break and continue are also error exits";
    (* the following is a hack to make it easier to add code in sgrep-like
       code, essentially to compensate for the fact that we don't have
       any way of printing things out *)
    "-allow_inconsistent_paths",
    Arg.Set Flag_matcher.allow_inconsistent_paths,
    "   if this flag is set don't check for inconsistent paths; dangerous";
    "-no_safe_expressions",
    Arg.Set Flag_matcher.no_safe_expressions,
    "  make an expression disjunction not prioritise the topmost disjunct";
    "-int_bits", Arg.Int Flag_parsing_c.set_int_bits,
    "  the number of bits in an unsigned int";
    "-long_bits", Arg.Int Flag_parsing_c.set_long_bits,
    "  the number of bits in an unsigned long";
    "-linux_spacing", Arg.Unit Flag_parsing_c.set_linux_spacing,
    "  spacing of + code follows the conventions of Linux";
    "-smpl_spacing", Arg.Unit Flag_parsing_c.set_smpl_spacing,
    "  spacing of + code follows the semantic patch";
    "-D", Arg.String Flag.set_defined_virtual_rules,
    "  indicate that a virtual rule should be considered to be matched";
    "-c++", Arg.Set Flag.c_plus_plus,
    "  make a small attempt to parse C++ files"
  ];

  "misc options",
  "",
  [
    "-debugger",           Arg.Set Common.debugger,
    "   option to set if launch spatch in ocamldebug";
    "-disable_once",       Arg.Set Common.disable_pr2_once,
    "   to print more messages";
    "-show_trace_profile", Arg.Set Common.show_trace_profile,
    "   show trace";
    "-save_tmp_files",     Arg.Set Common.save_tmp_files,   " ";
  ];

  "concurrency",
  "",
  [
    "-index",       Arg.Int (function x -> distrib_index := Some x) ,
    "   the processor to use for this run of spatch";
    "-max",         Arg.Int (function x -> distrib_max := Some x) ,
    "   the number of processors available";
    "-mod_distrib", Arg.Set mod_distrib,
    "   use mod to distribute files among the processors";
  ];

  "pad options",
  "",
  [
    "-use_cache", Arg.Set Flag_parsing_c.use_cache,
    "   use .ast_raw pre-parsed cached C file";
    "-cache_prefix",
    Arg.String (function s ->
      Flag_parsing_c.cache_prefix := Some s;
      Flag_parsing_c.use_cache := true),
    "   directory of cached ASTs, sets -use_cache";
    (* could use Flag_parsing_c.options_pad instead *)
    "-cache_limit",
      Arg.Int (function n ->
	Flag_parsing_c.cache_limit := Some n),
    "   maximum number of cached ASTs, sets -use_cache";
  ];



  "test mode and test options (works with tests/ or .ok files)",
  "The test options don't work with the -sp_file and so on.",
  [
    "-test",    Arg.Set test_mode,
    "   <file> launch spatch on tests/file.[c,cocci]";
    "-testall", Arg.Set test_all,
    "   launch spatch on all files in tests/ having a .res";
    "-test_okfailed", Arg.Set test_okfailed,
    "    generates .{ok,failed,spatch_ok} files using .res files";
    "-test_regression_okfailed", Arg.Set test_regression_okfailed,
    "    process the .{ok,failed,spatch_ok} files in current dir";

    "-compare_with_expected", Arg.Set compare_with_expected,
    "   use also file.res";
    "-expected_score_file", Arg.Set_string expected_score_file,
    "   which score file to compare with in -testall";
    "-relax_include_path", Arg.Set FC.relax_include_path,
    " ";
  ];

  "action mode",
  ("The action options don't work with the -sp_file and so on." ^ "\n" ^
   "It's for the other (internal) uses of the spatch program."
  ),

    (* -token_c, -parse_c, etc  *)
  ((Common.options_of_actions action (Test_parsing_c.actions())) ++
    [
    (let s = "-parse_cocci"  in s, Arg.Unit (fun () -> action := s),
    "   <file>");
    (let s = "-compare_c"  in s, Arg.Unit (fun () -> action := s),
    "   <file1> <file2>");
    ]);
]


let all_options =
  short_options ++ List.concat (List.map Common.thd3 other_options)

(* I don't want the -help and --help that are appended by Arg.align *)
let arg_align2 xs =
  Arg.align xs +> List.rev +> Common.drop 2 +> List.rev

(*
  Ignore unknown option

  This simplifies the integration of Coccinelle in toolchain.  For
  instance, spatch can then be used as a checker in the Linux build
  system.

*)
let check_include_path () =
  let opt = Array.get Sys.argv !Arg.current in
  let is_include_re = Str.regexp "-I\\(.*\\)" in
  if Str.string_match is_include_re opt 0 then
    let path = Str.matched_group 1 opt in
	FC.include_path:= path::!FC.include_path
  else ()

let rec arg_parse_no_fail l f msg =
  try
    check_include_path ();
    Arg.parse_argv Sys.argv l f msg;
  with
    | Arg.Bad emsg ->
	arg_parse_no_fail l f msg
    | Arg.Help msg -> (* printf "%s" msg; exit 0; *)
	raise Impossible  (* -help is specified in speclist *)

(* copy paste of Arg.parse. Don't want the default -help msg *)
let arg_parse2 l f msg argv =
  (try
    Arg.parse_argv argv l f msg;
  with
  | Arg.Bad emsg -> (* eprintf "%s" msg; exit 2; *)
      if not !ignore_unknown_opt then
	begin
	  let xs = Common.lines emsg in
	    (* take only head, it's where the error msg is *)
	    pr2 (List.hd xs);
	    !short_usage_func();
	    raise (Common.UnixExit (2))
	end
      else
	arg_parse_no_fail l f msg;
  | Arg.Help msg -> (* printf "%s" msg; exit 0; *)
      raise Impossible  (* -help is specified in speclist *)
  )


let short_usage () =
 begin
  Common.short_usage usage_msg short_options;
  pr2 "";
  pr2 "Example of use:";
  pr2 "  ./spatch -sp_file foo.cocci foo.c -o /tmp/newfoo.c";
  pr2 "";
 end


let long_usage () =
  Common.long_usage usage_msg short_options other_options

let _ = short_usage_func := short_usage
let _ = long_usage_func := long_usage

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for fresh identifier information *)
let adjust_stdin cfiles k =
  match cfiles with
    [] -> failwith "not possible"
  | cfile::_ ->
      let newin =
	try
          let (dir, base, ext) = Common.dbe_of_filename cfile in
          let varfile = Common.filename_of_dbe (dir, base, "var") in
          if ext =$= "c" && Common.lfile_exists varfile
          then Some varfile
          else None
	with Invalid_argument("Filename.chop_extension") -> None in
      Common.redirect_stdin_opt newin k

let glimpse_filter (coccifile, isofile) dir =
  let (_metavars,astcocci,_free_var_lists,_negated_positions,
       _used_after_lists,_positions_lists,(_,query,_)) =
    Cocci.sp_of_file coccifile (Some isofile) in
  match query with
    None -> pr2 "no inferred glimpse keywords"; None
  | Some queries ->
      let suffixes = if !include_headers then ["c";"h"] else ["c"] in
      let rec loop = function
	  [] -> None (* error, eg due to pattern too big *)
	| query::queries ->
	    Printf.fprintf stderr "%s\n" ("glimpse request = " ^ query);
	    let command = spf "glimpse -y -H %s -N -W -w '%s'" dir query in
	    let (glimpse_res,stat) = Common.cmd_to_list_and_status command in
	    match stat with
	      Unix.WEXITED(0) | Unix.WEXITED(1) ->
      Printf.fprintf stderr "got files\n"; flush stderr;
		Some
		  (glimpse_res +>
		   List.filter
		     (fun file -> List.mem (Common.filesuffix file) suffixes))
	    |	_ -> loop queries (* error, eg due to pattern too big *) in
      loop queries

let idutils_filter (coccifile, isofile) dir =
  let (_metavars,astcocci,_free_var_lists,_negated_positions,
       _used_after_lists,_positions_lists,(_,_,query)) =
    Cocci.sp_of_file coccifile (Some isofile) in
  match query with
    None -> pr2 "no inferred idutils keywords"; None
  | Some query ->
      let suffixes = if !include_headers then ["c";"h"] else ["c"] in
      let files = Id_utils.interpret dir query in
      Printf.fprintf stderr "got files\n"; flush stderr;
      Some
	(files +>
	 List.filter (fun file -> List.mem (Common.filesuffix file) suffixes))

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let get_files path =
  let ch =
    Common.cmd_to_list (* same as "true, "", _" case *)
      (if !include_headers
			  (* FIXME : Could we remove xs ?
			     -use_glimpse requires a singleton.
			     This is checked some lines before.
			     then ("find "^(join " " (x::xs))^" -name \"*.[ch]\"")
			     else ("find "^(join " " (x::xs))^" -name \"*.c\"")
			  *)
      then ("find "^ path ^" -name \"*.[ch]\"")
      else ("find "^ path ^" -name \"*.c\"")) in
  let cpp =
    if !Flag.c_plus_plus
    then Common.cmd_to_list ("find "^ path ^" -name \"*.cpp\"")
    else [] in
  cpp @ ch

let rec main_action xs =
  let (cocci_files,xs) =
    List.partition (function nm -> Filename.check_suffix nm ".cocci") xs in
  (match (!cocci_file,cocci_files) with
    "",[fl] -> cocci_file := fl
  | _,[] -> ()
  | _ -> failwith "only one .cocci file allowed");
  Iteration.base_file_list := xs;
  let rec toploop = function
      [] -> raise Impossible
    | x::xs ->
      (* a more general solution would be to use
       * Common.files_of_dir_or_files (x::xs)
       * as some elements in xs may also be directories, or individual
       * files.
       *)
	  dir := (Common.is_directory x);
	
          if !cocci_file =$= ""
          then failwith "I need a cocci file,  use -sp_file <file>";
	  
	  if !dir && !Flag.patch =*= None
	  then
	    (match xs with
	    | [] -> Flag.patch := Some (Cocci.normalize_path x)
	    | _ ->
		pr2
		  ("warning: patch output can only be created when only one\n"^
		   "directory is specified or when the -patch flag is used")
		  );
	  Flag.dir := x;
	  
          let infiles =
            Common.profile_code "Main.infiles computation" (fun () ->
	      match !dir, !kbuild_info, !Flag.scanner with
            (* glimpse *)
              | false, _, (Flag.Glimpse|Flag.IdUtils) -> [x::xs]
              | true, s, (Flag.Glimpse|Flag.IdUtils) when s <> "" ->
                  failwith
		    "-use_glimpse or -id_utils does not work with -kbuild"
              | true, "", Flag.Glimpse ->
                  (if not (null xs)
                  then failwith "-use_glimpse can accept only one dir");
		  
                  let files =
		    match glimpse_filter (!cocci_file, !Config.std_iso) x with
		      None -> get_files x
		    | Some files -> files in
                  files +> List.map (fun x -> [x])
              | true, "", Flag.IdUtils ->
                  (if not (null xs)
                  then failwith "-id_utils can accept only one dir");
		  
                  let files =
		    match idutils_filter (!cocci_file, !Config.std_iso) x with
		      None -> get_files x
		    | Some files -> files in
                  files +> List.map (fun x -> [x])
                  (* normal *)
	      | false, _, _ -> [x::xs]
	      | true, "", _ ->
		  get_files (join " " (x::xs)) +> List.map (fun x -> [x])
		    
            (* kbuild *)
	      | true, kbuild_info_file,_ ->
		  let dirs =
                    Common.cmd_to_list ("find "^(join " " (x::xs))^" -type d")
                  in
		  let info = Kbuild.parse_kbuild_info kbuild_info_file in
		  let groups = Kbuild.files_in_dirs dirs info in
		  
		  groups +> List.map (function Kbuild.Group xs -> xs)
		    )
          in

	  (* make cache unique in parallel case *)
	  (match (!distrib_index,!Flag_parsing_c.cache_prefix) with
	    (Some index,Some str) ->
	      Flag_parsing_c.cache_prefix :=
		Some (Printf.sprintf "%s/d%d" str index)
	  | _ -> ());

	  let infiles =
	    match (!distrib_index,!distrib_max) with
	      (None,None) -> infiles
	    | (Some index,Some max) ->
		(if index >= max
		then
		  failwith "index starts at 0, and so must be less than max");
		if !mod_distrib
		then
		  let rec loop ct = function
		      [] -> []
		    | x::xs ->
			if (ct mod max) =|= index
			then x::(loop (ct+1) xs)
			else loop (ct+1) xs in
		  loop 0 infiles
		else
		  begin
		    let all_files = List.length infiles in
		    let regions = (all_files + (max - 1)) / max in
		    let this_min = index * regions in
		    let this_max = (index+1) * regions in
		    let rec loop ct = function
			[] -> []
		      | x::xs ->
			  if this_min <= ct && ct < this_max
			  then x::(loop (ct+1) xs)
			  else loop (ct+1) xs in
		    loop 0 infiles
		  end
	    | _ -> failwith "inconsistent distribution information" in
	  
          let (cocci_infos,outfiles) =
            Common.profile_code "Main.outfiles computation" (fun () ->
	      let cocci_infos =
		Cocci.pre_engine (!cocci_file, !Config.std_iso) in
	      let res =
		infiles +> List.map (fun cfiles ->
		  pr2 ("HANDLING: " ^ (join " " cfiles));
		  Common.timeout_function_opt !FC.timeout (fun () ->
  	            Common.report_if_take_time 10 (join " " cfiles) (fun () ->
                      try
			let optfile =
			  if !output_file <> "" && !compat_mode then
			    Some !output_file
			  else
			    None
			in
			adjust_stdin cfiles (fun () ->
			  Common.redirect_stdout_opt optfile (fun () ->
			  (* this is the main call *)
			    Cocci.full_engine cocci_infos cfiles
			))
		      with
		      | Common.UnixExit x -> raise (Common.UnixExit x)
		      |	Pycocci.Pycocciexception ->
			  raise Pycocci.Pycocciexception
		      | e ->
			  if !dir
			  then begin
			    pr2 ("EXN:" ^ Printexc.to_string e);
			    [] (* *)
			  end
			  else raise e))) in
	      (cocci_infos,res)) in
	  let outfiles = List.concat outfiles in
	  (match Iteration.get_pending_instance() with
	    None ->
	      (x,xs,cocci_infos,outfiles)
	  | Some (files,virt_rules,virt_ids) ->
	      if outfiles = [] or outfiles = [] or not !FC.show_diff
		  or !inplace_modif
	      then
		begin
		  (if !inplace_modif then generate_outfiles outfiles x xs);
		  Flag.defined_virtual_rules := virt_rules;
		  Flag.defined_virtual_env := virt_ids;
		  Common.erase_temp_files();
		  Common.clear_pr2_once();
		  distrib_index := None;
		  distrib_max := None;
		  toploop files
		end
	      else
		begin
		  Common.pr2
		    "Out of place transformation not compatible with iteration. Aborting.\n consider using -no_show_diff or -in_place";
		  (x,xs,cocci_infos,outfiles)
		end) in
      let (x,xs,cocci_infos,outfiles) = toploop xs in

      Cocci.post_engine cocci_infos;
      Common.profile_code "Main.result analysis" (fun () ->
	Ctlcocci_integration.print_bench();
	generate_outfiles outfiles x xs;
        if !compare_with_expected
        then Testing.compare_with_expected outfiles)

and generate_outfiles outfiles x (* front file *) xs (* other files *) =
  let outfiles = Cocci.check_duplicate_modif outfiles in
  outfiles +> List.iter (fun (infile, outopt) ->
    outopt +> Common.do_option (fun outfile ->
      if !inplace_modif
      then begin
	(match !backup_suffix with
	  Some backup_suffix ->
	    Common.command2 ("cp "^infile^" "^infile^backup_suffix)
	| None -> ());
        Common.command2 ("cp "^outfile^" "^infile);
      end;
      
      if !outplace_modif
      then Common.command2 ("cp "^outfile^" "^infile^".cocci_res")
      
	      (* potential source of security pb if the /tmp/ file is
		 * a symlink, so simpler to not produce any regular file
		 * (files created by Common.new_temp_file are still ok)
		 * anymore in /tmp.
              *)
              (*
	         if !output_file =$= ""
	         then begin
                 let tmpfile = "/tmp/"^Common.basename infile in
                 pr2 (spf "One file modified. Result is here: %s" tmpfile);
                 Common.command2 ("cp "^outfile^" "^tmpfile);
	         end
              *)
	    ));
  if !output_file <> "" && not !compat_mode then
    (match outfiles with
    | [infile, Some outfile] when infile =$= x && null xs ->
        Common.command2 ("cp " ^outfile^ " " ^ !output_file)
    | [infile, None] when infile =$= x && null xs ->
        Common.command2 ("cp " ^infile^ " " ^ !output_file)
    | _ ->
        failwith
          ("-o can not be applied because there are multiple " ^
           "modified files"))

(*****************************************************************************)
(* The coccinelle main entry point *)
(*****************************************************************************)
let main () =
  begin
    let arglist = Array.to_list Sys.argv in
    let arglist = Command_line.command_line arglist in
    
    if not (null (Common.inter_set arglist
	            ["-cocci_file";"-sp_file";"-sp";"-test";"-testall";
                      "-test_okfailed";"-test_regression_okfailed"]))
    then run_profile quiet_profile;
    
    let args = ref [] in
    
    (* Gc.set {(Gc.get ()) with Gc.stack_limit = 1024 * 1024};*)
    
    (* this call can set up many global flag variables via the cmd line *)
    arg_parse2 (Arg.align all_options) (fun x -> args := x::!args) usage_msg
      (Array.of_list arglist);
    
    (* julia hack so that one can override directories specified on
       * the command line. *)
    (if !dir
    then
      let chosen_dir =
	if List.length !args > 1
	then
	  begin
	    let chosen = List.hd !args in
	    Flag.dir := chosen;
	    pr2 ("ignoring all but the last specified directory: "^chosen);
	    args := [chosen];
	    chosen
	  end
	else List.hd !args in
      if !FC.include_path =*= []
      then FC.include_path := [Filename.concat chosen_dir "include"]);

    args := List.rev !args;

    if !cocci_file <> "" && (not (!cocci_file =~ ".*\\.\\(sgrep\\|spatch\\)$"))
    then cocci_file := Common.adjust_ext_if_needed !cocci_file ".cocci";

    if !Config.std_iso <> ""
    then Config.std_iso := Common.adjust_ext_if_needed !Config.std_iso ".iso";
    if !Config.std_h <> ""
    then Config.std_h := Common.adjust_ext_if_needed !Config.std_h ".h";

    if !Config.std_h <> ""
    then Parse_c.init_defs_builtins !Config.std_h;

    if !macro_file <> ""
    then Parse_c.init_defs_macros !macro_file;


    (* must be done after Arg.parse, because Common.profile is set by it *)
    Common.profile_code "Main total" (fun () ->


    let all_actions = Test_parsing_c.actions() in

    (match (!args) with

    (* --------------------------------------------------------- *)
    (* The test framework. Works with tests/ or .ok and .failed  *)
    (* --------------------------------------------------------- *)
    | [x] when !test_mode    ->
	begin
	  let prefix = "tests/" in
	  let testfile = x ^ ".cocci" in
	    if Sys.file_exists (prefix ^ testfile) then
	      begin
		(if !FC.include_path = []
		then FC.include_path := [prefix^"include"]);
		Testing.testone prefix x !compare_with_expected
	      end
	    else
	      if Sys.file_exists testfile then
		begin
		  (if !FC.include_path = []
		  then FC.include_path := ["include"]);
		  Testing.testone "" x !compare_with_expected
		end
	      else
		pr2 (spf "ERROR: File %s does not exist" testfile)
	end

    | []  when !test_all ->
        (if !FC.include_path = []
	then FC.include_path := ["tests/include"]);
        if !expected_score_file <> ""
        then Testing.testall ~expected_score_file:!expected_score_file ()
        else Testing.testall ()

    | [] when !test_regression_okfailed ->
        Testing.test_regression_okfailed ()

    | ((x::xs) as cfiles) when !test_okfailed ->
        (* do its own timeout on FC.timeout internally *)
        FC.relax_include_path := true;
	adjust_stdin cfiles (fun () ->
          Testing.test_okfailed !cocci_file cfiles
        )

    (* --------------------------------------------------------- *)
    (* Actions, useful to debug subpart of coccinelle *)
    (* --------------------------------------------------------- *)

    | xs when List.mem !action (Common.action_list all_actions) ->
        Common.do_action !action xs all_actions

    | [file] when !action =$= "-parse_cocci" ->
	Testing.test_parse_cocci file

     (* I think this is used by some scripts in some Makefile for our
      * big-tests. So dont remove.
      *)
    | [file1;file2] when !action =$= "-compare_c" ->
       Test_parsing_c.test_compare_c file1 file2 (* result = unix code *)

    (* could add the Test_parsing_c.test_actions such as -parse_c & co *)


    (* --------------------------------------------------------- *)
    (* This is the main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> short_usage()
  ));
    if !Pycocci.initialised && (Pycocci.py_isinitialized ()) != 0 then begin
      ignore(Pycocci.pyrun_simplestring "cocci.finalise()");
      if !Flag.show_misc
      then Common.pr2 "Finalizing python\n";
      Pycocci.py_finalize ();
    end
  end


let main_with_better_error_report () =
  if !Common.debugger then main ()
  else
    try
      main ()
    with
    | Unix.Unix_error (e, "stat", filename) ->
        pr2
	  (spf "ERROR: File %s does not exist: %s"
	     filename (Unix.error_message e));
        raise (UnixExit (-1))
    | Parse_cocci.Bad_virt s ->
	Common.pr2 (Printf.sprintf "virtual rule %s not supported" s);
        raise (UnixExit (-1))

(*****************************************************************************)
let start =
  Common.main_boilerplate (fun () ->
    main_with_better_error_report ();
    Ctlcocci_integration.print_bench();
  )
