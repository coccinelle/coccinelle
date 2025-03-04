(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

open Common
module FC = Flag_cocci
module Inc = Includes

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in "the spatch options" section below), the
 * spatch program also depends on external files, described in
 * globals/cocciconfig.ml, mainly a standard.h and standard.iso file *)

let cocci_file = ref ""
let opt_c_files = ref []
let output_file = ref "" (* resulting code *)
let tmp_dir = ref "" (* temporary files for parallelism *)
let aux_file_suffix =
  ref (None : string option) (* suffix for backup if one is desired *)
let outplace_modif = ref false (* generates a .cocci_res  *)
let preprocess = ref false     (* run the C preprocessor before cocci *)
let compat_mode = ref false
let ignore_unknown_opt = ref false
let profile_per_file = ref false

let dir = ref false
let ignore = ref []
let file_groups = ref false
let kbuild_info = ref ""

let macro_file = ref ""

(* test mode *)
let test_mode = ref false
let c_test_all = ref false
let cpp_test_all = ref false
let test_spacing = ref false
let test_okfailed = ref false
let test_regression_okfailed = ref false

(* action mode *)
let action = ref ""

(* works with --test but also in "normal" spatch mode *)
let compare_with_expected = ref (None : string option)

let distrib_index = ref (None : int option)
let distrib_max   = ref (None : int option)
let mod_distrib   = ref false

let previous_merges = ref (([], []) : Cocci.merge_vars)

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
    Flag_parsing_c.verbose_includes;

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
    Flag_parsing_c.verbose_includes;

    Flag_matcher.verbose_matcher;
    Flag_matcher.debug_engine;

    Flag_parsing_c.debug_unparsing;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;

    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;


  ])

let testing_profile = (
  [
    Common.print_to_stderr;
    FC.show_diff;
  ],
  [

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
    Flag_parsing_c.verbose_includes;

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
    Flag_parsing_c.verbose_includes;
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
    Flag_parsing_c.verbose_includes;
  ])

let run_profile p =
  let (set_to_true, set_to_false) = p in
  List.iter (fun x -> x := false) set_to_false;
  List.iter (fun x -> x := true) set_to_true

(*****************************************************************************)
(* The spatch options *)
(*****************************************************************************)

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " --sp-file <SP> <infile> [-o <outfile>] [--iso-file <iso>] [options]" ^
    "\n" ^ "Options are:"

(* forward reference trick *)
let short_usage_func = ref (fun () -> ())
let long_usage_func  = ref (fun () -> ())


(* The short_options are user-oriented. The other options are for
 * the developers of coccinelle or advanced-users that know
 * quite well the underlying semantics of coccinelle.
 *)


(* will be printed when use only ./spatch. For the rest you have to
 * use --longhelp to see them.
 *)

let print_version () =
  let yes_or_no_of_bool b = if b then "yes" else "no" in
  let withocaml = yes_or_no_of_bool Prepare_ocamlcocci.ocaml_support in
  let withpython = yes_or_no_of_bool Pycocci.python_support in
  let whichregexp =
    if !Regexp.pcre_support then "PCRE"
    else "Str" in
  let flags =
    if Cocciconfig.configure_flags<>"" then Cocciconfig.configure_flags
    else "[none]" in
  (* let mode = if Dynlink.is_native then "native" else "byte-code" in *)
  Printf.printf "spatch version %s compiled with OCaml version %s\n"
    (* mode *) Cocciconfig.version Cocciconfig.ocaml_version;
  Printf.printf "Flags passed to the configure script: %s\n" flags;
  Printf.printf "OCaml scripting support: %s\n" withocaml;
  Printf.printf "Python scripting support: %s\n" withpython;
  Printf.printf "Syntax of regular expressions: %s\n" whichregexp;
  exit 0

let short_options = [
  "--sp-file",  Arg.Set_string cocci_file,
  " <file> the semantic patch file";

  "--opt-c",
  Arg.String (fun filename ->
    if Sys.file_exists filename
    then
      if Sys.is_directory filename
      then
        failwith (filename ^ " is a directory but a c source file was expected")
      else
        opt_c_files := filename::!opt_c_files),
  " <file> a c file to process if it exists";

  "-o", Arg.Set_string output_file,
  "   <file> the output file";
  "--in-place", Arg.Set Flag_cocci.inplace_modif,
  "   do the modification on the file directly (backup file name specified with --suffix)";
  "--suffix", Arg.String (function s -> aux_file_suffix := Some s),
  "   suffix to use when making a backup for inplace or for result of --out-place";
  "--out-place", Arg.Set outplace_modif,
  "   store modifications in a .cocci_res file (renamable via --suffix)";
  "--reverse", Arg.Set Flag_parsing_cocci.interpret_inverted,
  "  invert the semantic patch before applying it";

  "-U", Arg.Int (fun n -> Flag_parsing_c.diff_lines := Some (string_of_int n)),
  "  set number of diff context lines";
  "--partial-match",        Arg.Set Flag_ctl.partial_match,
  "    report partial matches of the SP on the C file";

  "--iso-file", Arg.Set_string Cocciconfig.std_iso,
  " <file> (default=" ^ !Cocciconfig.std_iso ^")";
  "--macro-file", Arg.Set_string macro_file,
  " <file>";
  "--macro-file-builtins", Arg.Set_string Cocciconfig.std_h,
  " <file> (default=" ^ !Cocciconfig.std_h ^ ")";

  "--recursive-includes",
  Arg.Unit (function _ -> Inc.set_parsing_style Inc.Parse_really_all_includes),
  ("  causes all available include files, both those included in the"^
   " C file(s) and those included in header files, to be used");
  "--all-includes",
  Arg.Unit (function _ -> Inc.set_parsing_style Inc.Parse_all_includes),
  "  causes all available include files included in the C file(s) to be used";
  "--no-includes",
  Arg.Unit (function _ -> Inc.set_parsing_style Inc.Parse_no_includes),
  "  causes not even local include files to be used";
  "--local-includes",
  Arg.Unit (function _ -> Inc.set_parsing_style Inc.Parse_local_includes),
  "  causes local include files to be used";
  "--include-headers-for-types", Arg.Set Inc.include_headers_for_types,
  "    use only type information from header files";
  "--no-include-cache", Arg.Set Flag.no_include_cache,
  "  don't cache parsed include files";
  "--ignore-unknown-options", Arg.Set ignore_unknown_opt,
  ("    For integration in a toolchain (must be set before the first unknown"^
   " option)");
  "--include-headers", Arg.Set Flag.include_headers,
  "    process header files independently";
  "-I",   Arg.String (fun x -> Inc.include_path:= x::!Inc.include_path),
  "  <dir> containing the header files";
  "--include", Arg.String (fun x -> Inc.extra_includes:=x::!Inc.extra_includes),
  "  file to consider as being included";

  "--preprocess", Arg.Set preprocess,
  " run the C preprocessor before applying the semantic match";

  "-c", Arg.Set compat_mode, " gcc/cpp compatibility mode";

  "--dir", Arg.Set dir,
  "    <dir> process all files in directory recursively";
  "--ignore", Arg.String (fun s -> ignore := s :: !ignore),
  "    <string> specify a file name prefix to ignore";
  "--file-groups", Arg.Set file_groups,
  "    <file> process the file groups listed in the file";

  "--no-scanner", Arg.Unit (function _ -> Flag.scanner := Flag.NoScanner),
  "    no indexing";
  "--use-gitgrep", Arg.Unit (function _ -> Flag.scanner := Flag.GitGrep),
  "    works with --dir, works on git tree subdirectories";
  "--use-glimpse", Arg.Unit (function _ -> Flag.scanner := Flag.Glimpse),
  "    works with --dir, use info generated by glimpseindex";
  "--use-idutils",
  Arg.String (function s -> Flag_parsing_cocci.id_utils_index := s;
    Flag.scanner := Flag.IdUtils),
  "    find relevant files using id-utils";
  "--use-coccigrep",
  Arg.Unit (function _ -> Flag.scanner := Flag.CocciGrep),
  "    find relevant files using cocci grep";
  "--use-diff",
  Arg.Unit (function _ -> Flag.scanner := Flag.PatchDiff),
  "    process files in the diff for a directory";
  "--use-patch-diff",
  Arg.String
    (function s ->
      let (startid,endid) = Patch_diff.split s in
      Flag.scanner := Flag.PatchDiffRange(startid,endid)),
  "    process files in the diff for a directory";
  "--patch",
    Arg.String (function s -> Flag.patch := Some (Cocci.normalize_path s)),
  ("    <dir> path name with respect to which a patch should be created\n"^
   "    \"\" for a file in the current directory");
  "--kbuild-info", Arg.Set_string kbuild_info,
  "    <file> improve --dir by grouping related c files";
  "--python", Arg.Set_string Cocciconfig.python_interpreter,
  "    Sets the path to the python interpreter";
  "--pyoutput", Arg.Set_string Flag.pyoutput,
  "    Sets output routine: Default value: coccilib.output.Console";
  "--parse-handler",
  Arg.String (fun f ->
    let f' = Prepare_ocamlcocci.prepare_simple f in
    Prepare_ocamlcocci.load_file f'),
  "    <file> Loads the file containing the OCaml code in charge of parse errors reporting";

  "--print-options-only", Arg.Unit (fun () -> ()),
  "   print selected options and exit";

  "--version",   Arg.Unit print_version, "  print version and build info";

  "--date",   Arg.Unit (fun () ->
    Printf.printf "version: $Date$";
    raise (Common.UnixExit 0)
    ),
  "   guess what";

  "--shorthelp", Arg.Unit (fun () ->
    !short_usage_func();
    raise (Common.UnixExit 0)
  ),
  "    see short list of options";
  "--longhelp", Arg.Unit (fun () ->
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
    "--sp", Arg.Unit (function _ -> failwith "impossible"),
    " command line semantic patch";
    "--iso", Arg.Set_string Cocciconfig.std_iso,   " short option of --iso-file";

    "--cocci-file", Arg.Set_string cocci_file,
    "   <file> the semantic patch file";
(*     "-c", Arg.Set_string cocci_file,     " short option of --sp-file"; *)
  ];

  "most useful show options",
  "",
  [
    "--show-diff"           , Arg.Set FC.show_diff, " ";
    "--no-show-diff"           , Arg.Clear FC.show_diff, " ";
    "--force-diff"           , Arg.Set FC.force_diff,
       " show diff even if only spacing changes";
    "--show-flow"              , Arg.Set FC.show_flow,        " ";
    (* works in conjunction with --show_ctl_text *)
    "--ctl-inline-let",
    Arg.Unit
    (function _ -> FC.show_ctl_text := true; FC.inline_let_ctl := true), " ";
    "--ctl-show-mcodekind",
    Arg.Unit
    (function _ -> FC.show_ctl_text := true; FC.show_mcodekind_in_ctl := true),
    " ";
    "--show-bindings",        Arg.Set FC.show_binding_in_out, " ";
    "--show-transinfo",    Arg.Set Flag.show_transinfo, " ";
    "--show-misc",         Arg.Set Flag.show_misc, " ";
    "--show-trying",          Arg.Set Flag.show_trying,
    " show the name of each function being processed";
    "--show-dependencies",
    Arg.Unit (function _ -> FC.show_dependencies := true;
      FC.show_binding_in_out := true),
    " show the dependencies related to each rule";
  ];

  "verbose subsystems options",
  "",
  [
    "--verbose-ctl-engine",
    Arg.Unit (function _ ->
      Flag_ctl.verbose_ctl_engine := true; FC.show_ctl_text := true) , " ";
    "--verbose-match",        Arg.Set Flag_ctl.verbose_match, " ";
    "--verbose-engine",       Arg.Set Flag_matcher.debug_engine,    " ";
    "--graphical-trace",      Arg.Set Flag_ctl.graphical_trace, "  generate a pdf file representing the matching process";
    "--gt-without-label",
     Arg.Unit (function _ ->
       Flag_ctl.graphical_trace := true; Flag_ctl.gt_without_label := true),
       "  remove graph label (requires option --graphical-trace)";

    "--parse-error-msg", Arg.Set Flag_parsing_c.show_parsing_error, " ";
    "--verbose-parsing",
       Arg.Unit (fun _ -> Flag_parsing_c.verbose_parsing := true;
	 Flag_parsing_c.show_parsing_error := true), " ";
    "--verbose-includes", Arg.Set Flag_parsing_c.verbose_includes,
    "  show on stderr which files are chosen for inclusion";
    "--type-error-msg",  Arg.Set Flag_parsing_c.verbose_type, " ";
    (* could also use Flag_parsing_c.options_verbose *)
  ];

  "other show options",
  "",
  [
    "--show-c"                 , Arg.Set FC.show_c,           " ";
    "--show-cocci"             , Arg.Set FC.show_cocci,       " ";
    "--show-before-fixed-flow" , Arg.Set FC.show_before_fixed_flow,  " ";
    "--show-ctl-tex"           , Arg.Set FC.show_ctl_tex,     " ";
    "--show-ctl-text"          , Arg.Set FC.show_ctl_text,     " ";
    "--show-SP"             ,    Arg.Set Flag_parsing_cocci.show_SP,  " ";
  ];


  "debug C parsing/unparsing",
  "",
  [
    "--debug-cpp",          Arg.Set  Flag_parsing_c.debug_cpp, " ";
    "--debug-lexer",        Arg.Set  Flag_parsing_c.debug_lexer , " ";
    "--debug-etdt",         Arg.Set  Flag_parsing_c.debug_etdt , "  ";
    "--debug-typedef",      Arg.Set  Flag_parsing_c.debug_typedef, "  ";

    "--filter-msg",         Arg.Set  Flag_parsing_c.filter_msg ,
    "  filter some cpp message when the macro is a \"known\" cpp construct";
    "--filter-define-error",     Arg.Set Flag_parsing_c.filter_define_error,"  ";
    "--filter-msg-define-error", Arg.Set Flag_parsing_c.filter_msg_define_error,
    "  filter the error msg";
    "--filter-passed-level",     Arg.Set_int Flag_parsing_c.filter_passed_level,"  ";
(*  debug cfg doesn't seem to have any effect, so drop it as an option *)
(*  "--debug_cfg",          Arg.Set Flag_parsing_c.debug_cfg , "  "; *)
    "--debug-unparsing",    Arg.Set Flag_parsing_c.debug_unparsing, "  ";
    "--debug-parse-cocci",  Arg.Set Flag_parsing_cocci.debug_parse_cocci, "  ";

  ];
  (* could use Flag_parsing_c.options_debug_with_title instead *)


  "shortcut for enabling/disabling a set of debugging options at once",
  "",
  [
    (* todo: other profile ? *)
    "--quiet",   Arg.Unit (fun () -> run_profile quiet_profile), " ";
    "--very-quiet",   Arg.Unit (fun () -> run_profile very_quiet_profile), " ";
    "--debug",   Arg.Unit (fun () -> run_profile debug_profile), " ";
    "--pad",     Arg.Unit (fun () -> run_profile pad_profile),   " ";

  ];

  "bench options",
  "",
  [
    "--profile", Arg.Unit (function () -> Common.profile := Common.PALL) ,
    "   gather timing information about the main coccinelle functions";
    "--profile-per-file",
    Arg.Unit
    (function () -> profile_per_file :=true; Common.profile := Common.PALL),
    "   gather timing information for each file (implies --profile)";
    "--bench", Arg.Int (function x -> Flag_ctl.bench := x),
    "   <level> for profiling the CTL engine";
    "--timeout",
    Arg.Int (fun x -> FC.timeout := (if x = 0 then None else Some x)),
    "   <sec> timeout in seconds, 0 for no timeout";
    "--steps", Arg.Int (fun x -> Flag_ctl.steps := Some x),
    "   max number of model checking steps per code unit";
    "--iso-limit", Arg.Int (fun x -> Flag_parsing_cocci.iso_limit := Some x),
    "   max depth of iso application";
    "--no-iso-limit", Arg.Unit (fun _ -> Flag_parsing_cocci.iso_limit := None),
    "   disable limit on max depth of iso application";
    "--track-iso", Arg.Set Flag.track_iso_usage,
    "   gather information about isomorphism usage";
    "--disable-iso",
    Arg.String
    (fun s -> Flag_parsing_cocci.disabled_isos :=
      s :: !Flag_parsing_cocci.disabled_isos),
    "   disable a specific isomorphism";
    "--profile-iso",
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
(*    "--popl", Arg.Set FC.popl,
    "    simplified SmPL, for the popl paper";

    "--popl_mark_all",
    Arg.Unit
    (function _ -> FC.popl := true; Flag_popl.mark_all := true),
    "    simplified SmPL, for the popl paper";

    "--popl_keep_all_wits",
    Arg.Unit
    (function _ -> FC.popl := true; Flag_popl.keep_all_wits := true),
    "    simplified SmPL, for the popl paper";

    "--hrule", Arg.String
    (function s ->
      Flag.make_hrule := Some s; Inc.include_options := Inc.Parse_no_includes),
    "    semantic patch generation";
*)
    "--keep-comments", Arg.Set Flag_parsing_c.keep_comments,
    "   keep comments around removed code";

    "--loop",              Arg.Set Flag_ctl.loop_in_src_code,    " ";
    "--no-loops",          Arg.Set Flag_parsing_c.no_loops,
    "   drop all back edges derived from looping constructs - unsafe";
    "--no-gotos",          Arg.Set Flag_parsing_c.no_gotos,
    "   drop all jumps derived from gotos - unsafe";
    "--no-saved-typedefs", Arg.Clear Flag_cocci.use_saved_typedefs,
    "   drop all inferred typedefs from one parse of some code to the next";

    "--ocaml-regexps", Arg.Clear Regexp.pcre_support,
    "   use OCaml Str regular expressions for constraints";

    "--l1",                Arg.Clear Flag_parsing_c.label_strategy_2, " ";
    "--ifdef-to-if",       Arg.Set Flag_parsing_c.ifdef_to_if,
    "   convert ifdef to if (experimental)";
    "--no-ifdef-to-if",              Arg.Clear Flag_parsing_c.ifdef_to_if,
    "   convert ifdef to if (experimental)";

    "--disable-multi-pass", Arg.Set Flag_parsing_c.disable_multi_pass, " ";

    "--noif0-passing",      Arg.Clear Flag_parsing_c.if0_passing, " ";
    "--itu",      Arg.Set Flag_parsing_c.exts_ITU,
    "   Experimental extensions for handling #ifdef developed at ITU.dk";
    "--defined", Arg.String (Flag_parsing_c.add Flag_parsing_c.defined),
    "   <symbol> treat cpp symbol as defined in #ifdef";
    "--undefined", Arg.String
        (Flag_parsing_c.add Flag_parsing_c.undefined),
    "   <symbol> treat cpp symbol as undefined in #ifdef";
    "--noadd-typedef-root", Arg.Clear Flag_parsing_c.add_typedef_root, " ";
    (* could use Flag_parsing_c.options_algo instead *)


    "--disallow-nested-exps", Arg.Set Flag_matcher.disallow_nested_exps,
       " disallow an expression pattern from matching a term and its subterm";
    "--disallow-nested-types", Arg.Set Flag_matcher.disallow_nested_types,
       " disallow an type pattern from matching a term and its subterm";
    "--disable-worth-trying-opt", Arg.Clear Flag.worth_trying_opt,
    "   run the semantic patch even if the C file contains no relevant tokens";
    "--selected-only", Arg.Set FC.selected_only, "  only show selected files";
    "--only-return-is-error-exit",
    Arg.Set Flag_matcher.only_return_is_error_exit,
    "   if this flag is not set, then break and continue are also error exits";
    (* the following is a hack to make it easier to add code in sgrep-like
       code, essentially to compensate for the fact that we don't have
       any way of printing things out *)
    "--allow-inconsistent-paths",
    Arg.Set Flag_matcher.allow_inconsistent_paths,
    "   if this flag is set don't check for inconsistent paths; dangerous";
    "--no-safe-expressions",
    Arg.Set Flag_matcher.no_safe_expressions,
    "  make an expression disjunction not prioritise the topmost disjunct";
    "--int-bits", Arg.Int Flag_parsing_c.set_int_bits,
    "  the number of bits in an unsigned int";
    "--long-bits", Arg.Int Flag_parsing_c.set_long_bits,
    "  the number of bits in an unsigned long";
    "--linux-spacing", Arg.Unit Flag_parsing_c.set_linux_spacing,
    "  spacing of + code follows the conventions of Linux";
    "--smpl-spacing", Arg.Unit Flag_parsing_c.set_smpl_spacing,
    "  spacing of + code follows the semantic patch";
    "--indent", Arg.Set_int Flag_parsing_c.indent,
    "  default indent, in spaces (no tabs)";
    "--max-width", Arg.Set_int Flag_parsing_c.max_width,
    "  column limit for generated code";
    "-D", Arg.String Flag.set_defined_virtual_rules,
    "  indicate that a virtual rule should be considered to be matched";
    "--c++", Arg.Unit (fun () -> Flag.set_c_plus_plus None),
    "  make a small attempt to parse C++ files";
    (* Special case, for when a user gives a specific C++ version *)
    "--c++=",
    Arg.Symbol (["98"; "03"; "0x"; "11"; "1y"; "14"; "1z"; "17"; "2a"; "20"; "23"],
                (fun i -> Flag.set_c_plus_plus (Some i))),
    "  <int> same as --c++ but allows to specify a version (e.g. --c++=11)";
    "--ibm", Arg.Set Flag.ibm,
    "  make a small attempt to parse IBM C files";
    "--force-kr", Arg.Set Flag_parsing_c.force_kr,
    "  despite the presence of non-K&R code, keep looking for K&R code";
    "--prevent-kr", Arg.Set Flag_parsing_c.prevent_kr,
    "  never make an identifier parameter alone into a K&R parameter";
  ];

  "misc options",
  "",
  [
    "--debugger",           Arg.Set Common.debugger,
    "   option to set if launch spatch in ocamldebug";
    "--disable-once",       Arg.Set Common.disable_pr2_once,
    "   to print more messages";
    "--show-trace-profile", Arg.Set Common.show_trace_profile,
    "   show trace";
    "--save-tmp-files",     Arg.Set Common.save_tmp_files,   " ";
    "--external-analysis-file", Arg.String
      (Externalanalysis.load_external_results),
    "  import results from an external analysis";
    "--ocaml-lib-search-path",
    Arg.String (function s ->
      Flag.ocaml_lib_search_path := s :: !Flag.ocaml_lib_search_path),
    "  add a path when searching for dynamically loadable modules";
  ];

  "concurrency",
  "",
  [
    "--index",       Arg.Int (function x -> distrib_index := Some x) ,
    "   the processor to use for this run of spatch";
    "--max",         Arg.Int (function x -> distrib_max := Some x) ,
    "   the number of processors available";
    "--mod-distrib", Arg.Set mod_distrib,
    "   use mod to distribute files among the processors";
    "--jobs",  Arg.Int (function x -> Flag.parmap_cores := Some x),
    "   the number of processes to be used";
    "-j",  Arg.Int (function x -> Flag.parmap_cores := Some x),
    "   the number of processes to be used";
    "--chunksize", Arg.Int (function x -> Flag.parmap_chunk_size := Some x),
    "   the size of work chunks for parallelism";
    "--tmp-dir", Arg.Set_string tmp_dir,
    "   prefix of temporary directories for parallelism";
    "--temp-files", Arg.Set_string Common.temp_files,
    "   prefix of temporary directory for cocci files";
  ];

  "pad options",
  "",
  [
    "--use-cache", Arg.Set Flag_parsing_c.use_cache,
    "   use .ast_raw pre-parsed cached C file";
    "--cache-prefix",
    Arg.String (function s ->
      Flag_parsing_c.cache_prefix := Some s;
      Flag_parsing_c.use_cache := true),
    "   directory of cached ASTs, sets --use-cache";
    (* could use Flag_parsing_c.options_pad instead *)
    "--cache-limit",
      Arg.Int (function n ->
	Flag_parsing_c.cache_limit := Some n;
	Flag_parsing_c.use_cache := true),
    "   maximum number of cached ASTs, sets --use-cache";
  ];



  "test mode and test options (works with tests/ or .ok files)",
  "The test options don't work with the --sp-file and so on.",
  [
    "--test",    Arg.Set test_mode,
    "   <file> launch spatch on tests/file.[c,cocci]";
    "--ctestall", Arg.Set c_test_all,
    "   launch spatch on all files in tests/ having a .res";
    "--cpptestall", Arg.Set cpp_test_all,
    "   launch spatch on all files in cpptests/ (C++) having a .res";
    "--test-spacing", Arg.Set test_spacing,
    "    check that the result matches the .res file exactly";
    "--test-okfailed", Arg.Set test_okfailed,
    "    generates .{ok,failed,spatch_ok} files using .res files";
    "--test-regression-okfailed", Arg.Set test_regression_okfailed,
    "    process the .{ok,failed,spatch_ok} files in current dir";

    "--compare-with-expected",
    Arg.Unit (fun _ -> compare_with_expected := Some ".res"),
    "   use also file.res";
    "--expected-extension",
    Arg.String (fun x -> compare_with_expected := Some x),
    "   extension for --compare-with-expected; implicitly sets --compare-with-expected";
    "--relax-include-path", Arg.Set Inc.relax_include_path,
    " ";
    "--batch_mode", Arg.Set _batch_mode,
    " no interactivity"
  ];

  "action mode",
  ("The action options don't work with the --sp-file and so on." ^ "\n" ^
   "It's for the other (internal) uses of the spatch program."
  ),

    (* --token_c, --parse_c, etc  *)
  ((Common.options_of_actions action (Test_parsing_c.actions())) @
    [
    (let s = "--parse-cocci"  in s, Arg.Unit (fun () -> action := s),
    "   <file>");
    (let s = "--rule-dependencies"  in s, Arg.Unit (fun () -> action := s),
    "   <file>");
    (let s = "--compare-c"  in s, Arg.Unit (fun () -> action := s),
    "   <file1> <file2>");
    ]);
]


let all_options =
  short_options @ List.concat (List.map Common.thd3 other_options)

let all_string_option_names =
  List.fold_left
    (function prev ->
      function
	  (_,Arg.Unit _,_) -> prev
	| (nm,_,_) -> nm :: prev)
    [] all_options

(*
  Ignore unknown option

  This simplifies the integration of Coccinelle in toolchain.  For
  instance, spatch can then be used as a checker in the Linux build
  system.

*)
let check_include_path () =
  let opt = Array.get Sys.argv !Arg.current in
  let is_include_re = Str.regexp "-I\\(.*\\)" in
  if Str.string_match is_include_re opt 0
  then
    let path = Str.matched_group 1 opt in
    Inc.include_path:= path::!Inc.include_path

let rec arg_parse_no_fail ~current l f msg argv =
  try
    check_include_path ();
    Arg.parse_argv ~current argv l f msg;
  with
    | Arg.Bad emsg ->
	arg_parse_no_fail ~current l f msg argv
    | Arg.Help msg -> (* printf "%s" msg; exit 0; *)
	raise (Impossible 165)  (* --help is specified in speclist *)

(* copy paste of Arg.parse. Don't want the default --help msg *)
let arg_parse2 ~current l f msg argv argv_location =
  (try
    Arg.parse_argv ~current argv l f msg;
  with
  | Arg.Bad emsg -> (* eprintf "%s" msg; exit 2; *)
      if not !ignore_unknown_opt then
	begin
	  let xs = Common.lines emsg in
	    (* take only head, it's where the error msg is *)
	    (* was pr2, but that doesn't always get generated *)
	    Printf.eprintf "%s (found %s)\n%!" (List.hd xs) argv_location;
	    !short_usage_func();
	    raise (Common.UnixExit (2))
	end
      else
	arg_parse_no_fail ~current l f msg argv;
  | Arg.Help msg -> (* printf "%s" msg; exit 0; *)
      raise (Impossible 166)  (* --help is specified in speclist *)
  )


let short_usage () =
 begin
  Common.short_usage usage_msg ~short_opt:short_options;
  pr "";
  pr "Example of use:";
  pr "  ./spatch --sp-file foo.cocci foo.c -o /tmp/newfoo.c";
  pr "";
 end


let long_usage () =
  Common.long_usage usage_msg ~short_opt:short_options ~long_opt:other_options

let _ = short_usage_func := short_usage
let _ = long_usage_func := long_usage

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for fresh identifier information *)
let adjust_stdin cfiles k =
  match cfiles with
    [] -> failwith "no files: not possible"
  | cfile::_ ->
      let newin =
	try
          let (dir, base, ext) = Common.dbe_of_filename (fst cfile) in
          let varfile = Common.filename_of_dbe (dir, base, "var") in
          if ext = "c" && Common.lfile_exists varfile
          then Some varfile
          else None
	with Invalid_argument _ -> None in
      Common.redirect_stdin_opt newin k

let glimpse_filter2 (_,query,_,_) dir =
  match query with
    None -> pr2 "no inferred glimpse keywords"; None
  | Some queries ->
      let suffixes = if !Flag.include_headers then ["c";"h"] else ["c"] in
      let rec loop = function
	  [] -> None (* error, eg due to pattern too big *)
	| query::queries ->
	    Printf.eprintf "%s\n" ("glimpse request = " ^ query);
	    let command = spf "glimpse -y -H %s -N -W -w '%s'" dir query in
	    let (glimpse_res,stat) = Common.cmd_to_list_and_status command in
	    match stat with
	      Unix.WEXITED(0) | Unix.WEXITED(1) ->
		Some
		  (let filelist = glimpse_res +>
		   List.filter
		     (fun file ->
		       List.mem (Common.filesuffix file) suffixes) in
		   if filelist <> [] then
		     begin
		       let firstfile = List.hd filelist in
		       if Filename.is_relative firstfile ||
		          Filename.is_implicit firstfile
		       then
			 List.map (fun file -> dir ^ Filename.dir_sep ^ file)
			   filelist
		       else filelist
		     end
		   else []
		  )
	    |	_ -> loop queries (* error, eg due to pattern too big *) in
      loop queries

let glimpse_filter a b  =
  Common.profile_code "glimpse_filter" (fun () -> glimpse_filter2 a b)

let coccigrep_filter (_,_,query,_) dir =
  match query with
    None -> pr2 "no inferred keywords"; None
  | Some (q1,q2,_) ->
      let res =
	Test_parsing_c.get_files dir +>
	List.filter (Cocci_grep.interpret (q1,q2)) in
      Printf.eprintf "%d files match\n" (List.length res);
      Some res

let gitgrep_filter ((_,_,query,_) as x) dir =
  match query with
    None -> pr2 "no inferred keywords"; None
  | Some (_,_,query) ->
      let suffixes = if !Flag.include_headers then "'*.[ch]'" else "'*.c'" in
      match Git_grep.interpret dir query suffixes with
	Some res ->
	  Printf.eprintf "%d files match\n" (List.length res);
	  Some res
      |	None -> coccigrep_filter x dir

let idutils_filter (_,_,_,query) dir =
  match Id_utils.interpret dir query with
    None -> None
  | Some files ->
      let suffixes = if !Flag.include_headers then ["c";"h"] else ["c"] in
      Printf.eprintf "%d files match\n" (List.length files);
      Some
	(files +>
	 List.filter
     (fun file -> List.mem (Common.filesuffix file) suffixes))

let diff_filter _ dir =
  let struc = Patch_diff.getdiff dir in
  Some (List.map (function x -> x.Patch_diff.file_name) struc)

let patchdiff_filter startid endid _ dir =
  let struc = Patch_diff.getpatchdiff dir startid endid in
  Some (List.map (function x -> x.Patch_diff.file_name) struc)

let scanner_to_interpreter = function
    Flag.Glimpse -> glimpse_filter
  | Flag.IdUtils -> idutils_filter
  | Flag.CocciGrep -> coccigrep_filter
  | Flag.GitGrep -> gitgrep_filter
  | Flag.PatchDiff -> diff_filter
  | Flag.PatchDiffRange(startid,endid) ->
      patchdiff_filter startid endid
  | _ -> failwith "impossible"

(*****************************************************************************)
(* File groups *)
(*****************************************************************************)

(* Groups are separated by blank lines. Single-line comments beginning
with // are tolerated *)
let read_file_groups x =
  let i = open_in x in
  let file_groups = ref [] in
  let res = ref [] in
  let dump _ =
    (match !res with
      [] -> ()
    | l -> file_groups := (List.rev l) :: !file_groups);
    res := [] in
  let empty_line = Str.regexp " *$" in
  let com = Str.regexp " *//" in
  let parse_line s =
    match Str.bounded_split (Str.regexp ":") s 2 with
      [fl;lns] ->
	let pieces = Str.split (Str.regexp ",") lns in
	let lines =
	  List.map
	    (function piece ->
	      match Str.split_delim (Str.regexp_string "-") piece with
		["";from;upto] ->
		  let from = int_of_string(Stdcompat.String.trim from) in
		  let upto = int_of_string(Stdcompat.String.trim upto) in
		  Parse_c.Excluded(from,upto)
	      | ["";from] ->
		  let from = int_of_string(Stdcompat.String.trim from) in
		  Parse_c.Excluded(from,from)
	      | [from;upto] ->
		  let from = int_of_string(Stdcompat.String.trim from) in
		  let upto = int_of_string(Stdcompat.String.trim upto) in
		  Parse_c.Included(from,upto)
	      | [from] ->
		  let from = int_of_string(Stdcompat.String.trim from) in
		  Parse_c.Included(from,from)
	      | _ -> failwith (Printf.sprintf "bad spec in %s" x))
	    pieces in
	(fl,Some lines)
    | _ -> (s,None) in
  let rec in_files _ =
    let l = input_line i in
    if Str.string_match empty_line l 0
    then begin dump(); in_space() end
    else if Str.string_match com l 0
    then in_files()
    else begin res := parse_line l :: !res; in_files() end
  and in_space _ =
    let l = input_line i in
    if Str.string_match empty_line l 0
    then in_space()
    else if Str.string_match com l 0
    then in_space()
    else begin res := parse_line l :: !res; in_files() end in
  try in_files() with End_of_file -> begin dump(); List.rev !file_groups end

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let rec main_action xs =
  let (cocci_files,xs) =
    List.partition (function nm -> Filename.check_suffix nm ".cocci") xs in
  (match (!cocci_file,cocci_files) with
    "",[fl] -> cocci_file := fl
  | _,[] -> ()
  | _ -> failwith "only one .cocci file allowed");
  Iteration.base_file_list := xs;
  previous_merges := ([], []);
  let rec toploop patching_failed = function
      [] -> failwith "no C files provided"
    | x::xs ->
      (* a more general solution would be to use
       * Common.files_of_dir_or_files (x::xs)
       * as some elements in xs may also be directories, or individual
       * files.
       *)
          if !cocci_file = ""
          then failwith "I need a cocci file,  use --sp-file <file>";

	  if !dir && !Flag.patch = None
	  then
	    begin
	      (match xs with
	      | [] -> Flag.patch := Some (Cocci.normalize_path x)
	      | _ ->
		  pr2
		    ("warning: patch output can only be created when only one\n"
		     ^"directory is specified or when the --patch flag is used")
		    );
	      Flag.dir := x
	    end;

	(if List.mem (Inc.get_parsing_style())
	    [Inc.Parse_all_includes;Inc.Parse_really_all_includes]
	then
	  Common.profile_code "setup_unique_search" (fun _ ->
	    Inc.setup_unique_search !Flag.parmap_cores !Inc.include_path));

	let (cocci_infos,constants) =
	  Cocci.pre_engine (!cocci_file, !Cocciconfig.std_iso) in

        let infiles =
            Common.profile_code "Main.infiles computation" (fun () ->
	      match !file_groups, !dir, !kbuild_info, !Flag.scanner, xs with
            (* glimpse *)
	      | true, false, "", scanner, [] ->
		  let fg = read_file_groups x in
		  (match scanner with
		    Flag.NoScanner -> fg
		  | _ ->
		      let interpreter = scanner_to_interpreter !Flag.scanner in
		      List.filter
			(function files ->
			  List.exists
			    (function (fl,_) ->
			      match interpreter constants fl with
				Some [] -> false (* no file matches *)
			      | _ -> true)
			    files)
			fg)
	      | true, _, _, _, _ ->
		  failwith
		    ("file groups not compatible with --dir, --kbuild-info,"^
		     " or multiple files")
              | _, false, _, _, _ -> [List.map (fun x -> (x,None)) (x::xs)]
	      |	_, true, "",
		  (Flag.Glimpse|Flag.IdUtils|Flag.CocciGrep|Flag.GitGrep|
		  Flag.PatchDiff|Flag.PatchDiffRange _),
		  [] ->
		    let interpreter = scanner_to_interpreter !Flag.scanner in
		    let files =
		      match interpreter constants x with
			None -> Test_parsing_c.get_files x
		      | Some files -> files in
                    files +> List.map (fun x -> [(x,None)])
              | _, true, s,
		  (Flag.Glimpse|Flag.IdUtils|Flag.CocciGrep|Flag.GitGrep|
		  Flag.PatchDiff|Flag.PatchDiffRange _), _
		when s <> "" ->
                  failwith "--use-xxx filters do not work with --kbuild"
                  (* normal *)
	      | _, true, "", _, _ ->
		  Test_parsing_c.get_files (String.concat " " (x::xs)) +>
		  List.map (fun x -> [(x,None)])

            (* kbuild *)
	      | _, true, kbuild_info_file,_,_ ->
		  let dirs =
                    Common.cmd_to_list
		      ("find "^(String.concat " " (x::xs))^" -type d") in
		  let info = Kbuild.parse_kbuild_info kbuild_info_file in
		  let groups = Kbuild.files_in_dirs dirs info in

		  groups +>
		  List.map
		    (function Kbuild.Group xs ->
		      List.map (fun x -> (x,None)) xs)
		    )
          in

	  (* make cache unique in parallel case *)
	  (match (!distrib_index,!Flag_parsing_c.cache_prefix) with
	    (Some index,Some str) ->
	      Flag_parsing_c.cache_prefix :=
		Some (Printf.sprintf "%s/d%d" str index)
	  | _ -> ());

	let ncores =
	  match !Flag.parmap_cores with
	  | Some x when x <= 0 -> succ (Parmap.get_default_ncores ())
	  | Some x -> x
	  | None -> 0 in
	let chunksize =
	  match !Flag.parmap_chunk_size with
	  | Some x when x > 0 -> x
	  | Some _ | None -> 1 in
	let infiles =
	  if !ignore = []
	  then infiles
	  else
	    let regexps = List.map Str.regexp !ignore in
	    let inacceptable x =
	      List.for_all (* all files in a group must fail *)
		(fun (x,_) ->
		  List.exists (fun re -> Str.string_match re x 0) regexps)
		x in
	    if ncores = 0
	    then List.filter (function x -> not (inacceptable x)) infiles
	    else
	      Parmap.parfold ~ncores
		(fun x rest -> if inacceptable x then rest else x :: rest)
		(Parmap.L infiles) [] (@) in
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
			if (ct mod max) = index
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

	  let seq_fold merge op z l =
	    List.fold_left op z l in
	  let par_fold merge op z l =
            let prefix =
              let prefix =
		let tm = Unix.localtime(Unix.time()) in
		Printf.sprintf "%s_%d_%d_%d:%d:%d:%d_%d"
		  (Filename.chop_extension (Filename.basename !cocci_file))
		  tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900)
		  tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
		  (Unix.getpid()) in
	      if !tmp_dir = ""
	      then prefix
	      else Printf.sprintf "%s_%s" !tmp_dir prefix in
	    (if Sys.file_exists prefix
	    then
	      failwith
		(Printf.sprintf
		   "Directory %s used for temporary files already exists and should be removed." prefix));
	    let clean _ =
	      let files = Array.to_list(Sys.readdir prefix) in
	      let (stdouts,stderrs) =
		List.partition
		  (function x -> Str.string_match (Str.regexp "stdout") x 0)
		  files in
	      List.iter (function x -> Common.file_to_stdout (prefix^"/"^x))
		stdouts;
	      List.iter (function x -> Common.file_to_stderr (prefix^"/"^x))
		stderrs;
	      let _ = Sys.command (Printf.sprintf "rm -rf %s" prefix) in
	      () in
	    let op y (x, temp_files) =
	      (* The subprocess has a different address space, so its
		 collected temporary files aren't seen by the main spatch
		 process *)
	      Common._temp_files_created := temp_files;
	      let res = op x y in
	      (res, !Common._temp_files_created) in
	    let merge (x, temp_files) (y, temp_files') =
	      (merge x y, List.rev_append temp_files temp_files') in
	    Pycocci.flush_stdout_and_stderr ();
	    let (res, tmps) =
	      try
		Parmap.parfold
		  ~init:(fun id -> Parmap.redirect ~path:prefix ~id)
		  ~finalize:(fun () ->
                    Pycocci.flush_stdout_and_stderr ();
                    flush_all ())
		  ~ncores
		  ~chunksize
		  op (Parmap.L l) (z, !Common._temp_files_created) merge
	      with e ->
		(Printf.eprintf "exception on %s: %s\n" prefix (Dumper.dump e);
		 clean(); raise e) in
	    Common._temp_files_created := tmps;
	    clean();
	    res
	  in
	  let actual_fold =
	    if ncores <= 1
	    then seq_fold
	    else par_fold in

          let (outfiles, merges, patching_failed) =
            Common.profile_code "Main.outfiles computation" (fun () ->
	      let res =
		match infiles with
		  [] ->
		    (* parmap case does a lot of work that is not needed
		       if there is nothing to do *)
		    [], [], false
		| _ ->
(* merge_vars associates one merge variable assignment to each core.
The following code relies on the fact that parfold never
passes the accumulator from one core to another before merging the
results. Therefore, the fold operator may just keep the last value in a
singleton list (forgetting the former accumulator value) and these
singleton lists are then just appended to each other during the merge. *)
		    let merge (outfiles, merge_vars, fld) (outfiles', merge_vars', fld') =
		      let all_outfiles = List.rev_append outfiles outfiles' in
		      let all_merge_vars =
			List.rev_append merge_vars merge_vars' in
		      all_outfiles, all_merge_vars, fld || fld' in
		    infiles +> actual_fold merge
		      (fun ((prev_files, prev_merges, fld) as prev) cfiles ->
			if (not !Flag.worth_trying_opt) ||
			Cocci.worth_trying cfiles constants
			then
			  begin
			    let all_cfiles =
			      String.concat " " (List.map fst cfiles) in
			    pr2 ("HANDLING: " ^ all_cfiles);
			    flush stderr;

			    Common.timeout_function_opt all_cfiles !FC.timeout
			      (fun () ->
				let res =
				  Common.report_if_take_time 10 all_cfiles
				    (fun () ->
				      try
					let optfile =
					  if !output_file <> "" && !compat_mode
					  then Some !output_file
					  else None in
					let files, merges =
					  adjust_stdin cfiles (fun () ->
					    Common.redirect_stdout_opt optfile
					      (fun () ->
					      (* this is the main call *)
						Cocci.full_engine cocci_infos
						  cfiles
						  )) in
					List.rev_append files prev_files,
					[merges], fld
				      with
				      | Common.UnixExit x ->
					  raise (Common.UnixExit x)
				      | Pycocci.Pycocciexception ->
					  raise Pycocci.Pycocciexception
				      | e ->
					  if !dir || !file_groups
					  then begin
					  (* not hidden by --very-quiet *)
					    Printf.eprintf "EXN: %s in %s\n"
					      (Printexc.to_string e)
					      all_cfiles;
					    flush stderr;
					    (prev_files, prev_merges, true)
					  end
					  else raise e) in
				(if !dir && !profile_per_file
				then Common.reset_profile());
				res)
			  end
			else prev)
		      ([], [], patching_failed) in res) in
	  let outfiles = List.rev outfiles in
	  let merges =
            List.fold_left Cocci.union_merge_vars !previous_merges merges in
	  let pending_instance =
	    match Iteration.get_pending_instance() with
	      None ->
		previous_merges := ([], []);
		Cocci.post_engine cocci_infos merges;
		Iteration.get_pending_instance()
	    | pending_instance ->
		previous_merges := merges;
		pending_instance in
	  (match pending_instance with
	    None ->
	      (x,xs,cocci_infos,outfiles,patching_failed)
	  | Some (files,virt_rules,virt_ids) ->
	      if outfiles = [] || outfiles = [] || not !FC.show_diff ||
	      !Flag_cocci.inplace_modif
	      then
		begin
		  (if !Flag_cocci.inplace_modif
		  then generate_outfiles outfiles x xs);
		  debug_restart virt_rules virt_ids;
		  Flag.defined_virtual_rules := virt_rules;
		  Flag.defined_virtual_env := virt_ids;
		  Common.erase_temp_files();
		  Common.clear_pr2_once();
		  distrib_index := None;
		  distrib_max := None;
		  dir := List.exists Sys.is_directory files;
		  toploop patching_failed files
		end
	      else
		begin
		  Printf.eprintf
		    "Out of place transformation not compatible with iteration. Aborting.\n consider using --no-show-diff or --in-place";
		  (x,xs,cocci_infos,outfiles,true)
		end) in
      let (x,xs,cocci_infos,outfiles,patching_failed) = toploop false xs in
      Common.profile_code "Main.result analysis" (fun () ->
	Ctlcocci_integration.print_bench();
	(if not patching_failed
	then generate_outfiles outfiles x xs);
        match !compare_with_expected with
	  None -> ()
        | Some extension -> Testing.compare_with_expected outfiles extension);
      if patching_failed
      then
        Printf.eprintf
	  "An error occurred when attempting to transform some files.\n";

and debug_restart virt_rules virt_ids =
  if !Flag_parsing_cocci.debug_parse_cocci
  then
    begin
      Printf.eprintf
	"Starting a new iteration with:\nVirtual rules: %s\n"
	(String.concat " " virt_rules);
      Printf.eprintf
	"Virtual identifiers: %s\n\n"
	(String.concat ", "
	   (List.map
	      (function (a,b) -> Printf.sprintf "%s: %s" a b)
	      virt_ids))
    end

and generate_outfiles outfiles x (* front file *) xs (* other files *) =
  let outfiles = Cocci.check_duplicate_modif outfiles in
  outfiles +> List.iter (fun (infile, outopt) ->
    outopt +> Common.do_option (fun outfile ->
      if !Flag_cocci.inplace_modif
      then begin
	(match !aux_file_suffix with
	  Some backup_suffix ->
	    Common.command2 ("cp "^infile^" "^infile^backup_suffix)
	| None -> ());
        Common.command2 ("cp "^outfile^" "^infile);
      end;

      if !outplace_modif
      then
	let suffix =
	  match !aux_file_suffix with
	    Some suffix -> suffix
	  | None -> ".cocci_res" in
	Common.command2 ("cp "^outfile^" "^infile^suffix)

	      (* potential source of security pb if the /tmp/ file is
		 * a symlink, so simpler to not produce any regular file
		 * (files created by Common.new_temp_file are still ok)
		 * anymore in /tmp.
              *)
              (*
	         if !output_file = ""
	         then begin
                 let tmpfile = "/tmp/"^Filename.basename infile in
                 pr2 (spf "One file modified. Result is here: %s" tmpfile);
                 Common.command2 ("cp "^outfile^" "^tmpfile);
	         end
              *)
	    ));
  if !output_file <> "" && not !compat_mode
  then
    let relevant_files =
      List.filter
	(function (fl,modif) -> fl = x || not(modif = None))
	outfiles in
    (match relevant_files with
    | [infile, Some outfile] when infile = x && xs=[] ->
        Common.command2 ("cp " ^outfile^ " " ^ !output_file)
    | [infile, None] when infile = x && xs=[] ->
        Common.command2 ("cp " ^infile^ " " ^ !output_file)
    | [] ->
        failwith
          ("-o can not be applied because there are no " ^
           "modified files")
    | _ ->
        failwith
          ("-o can not be applied because there are multiple " ^
           "modified files"))

let fix_chars s =
  if (String.length s) > 2 && String.get s 0 = '-'
      && not (String.get s 1 = '-')
  then "-"^(String.concat "-" (Str.split (Str.regexp_string "_") s))
  else s

let rec fix_idutils = function
    [] -> []
  | ["--use-idutils"] -> ["--use-idutils";".id-utils.index"]
  | "--use-idutils"::second::rest
    when String.length second > 0 && String.get second 0 = '-' ->
       "--use-idutils"::".id-utils.index" :: (fix_idutils (second :: rest))
  | x :: rest ->
      let fail _ = x :: (fix_idutils rest) in
      let len = String.length x in
      if len > 3 && String.get x 0 = '-' && String.get x 1 = '-'
      then
	let c1 = String.get x 2 in
	let s = String.sub x 3 (len-3) in
	if c1 = 'j'
	then
	  try let _ = int_of_string s in "-j"::s::(fix_idutils rest)
	  with _ -> fail()
	else if c1 = 'I'
	then "--I"::s::(fix_idutils rest)
	else fail()
      else fail()

(*****************************************************************************)
(* The coccinelle main entry point *)
(*****************************************************************************)
let main arglist =
  begin
    let (spatch_bin_name, cl_args) =
      match arglist with
      | [] -> failwith "The binary name ($0) is missing"
      | hd::tl -> (hd, tl) in
    let normalize_args args =
      Command_line.command_line args
      +> List.map fix_chars
      +> fix_idutils in
    let cl_args = normalize_args cl_args in
    let cocci_args =
      Cocci_args.read_args cl_args
      +> normalize_args in
    let config_args =
      Read_options.read_options all_string_option_names (cocci_args @ cl_args)
      +> normalize_args in
    let arglist = spatch_bin_name :: config_args @ cocci_args @ cl_args in

    (if List.mem "--print-options-only" arglist
    then
      begin
	Printf.eprintf "options: '%s'\n" (String.concat "' '" arglist);
	raise (UnixExit 0)
      end);

    let contains_cocci =
      (* rather a hack... don't want to think about all possible options *)
      List.exists (function x -> Filename.check_suffix x ".cocci") arglist
        && not (List.mem "--parse-cocci" arglist)
	&& not (List.mem "--rule-dependencies" arglist) in
    if (Common.inter_set arglist
	            ["--cocci-file";"--sp-file";"--sp";"--test";"--ctestall";"--cpptestall";
                      "--test-okfailed";"--test-regression-okfailed"]) <> []
         || contains_cocci
    then run_profile quiet_profile;

    let args = ref [] in

    let arg_parse =
      let speclist = Arg.align all_options in
      let anon_fun = (fun x -> args := x::!args) in
      fun ?(anon_fun=anon_fun) some_args args_location ->
        let rec split_c_plus_plus_equal = function
          (* hack to separate '--c++=<version>' as '--c++=' '<version>',
           * otherwise, '<version>' would be considered to be an argument for
           * '--c++' (which does not expect an argument) instead of '--c++='
           *)
          | [] -> []
          | s::l ->
              let len = String.length s in
              if len > 6 && String.sub s 0 6 = "--c++=" then
                let version_arg = String.sub s 6 (len - 6) in
                "--c++="::version_arg::(split_c_plus_plus_equal l)
              else
                s::split_c_plus_plus_equal l in
        let some_args = split_c_plus_plus_equal some_args in
        arg_parse2 ~current:(ref 0) speclist anon_fun usage_msg
                   (Array.of_list (spatch_bin_name::some_args))
                   args_location in
    arg_parse config_args "in a cocciconfig file";
    if List.mem "--parse-cocci" cl_args
    then arg_parse ~anon_fun:(fun _ -> ()) cocci_args "in the cocci file"
    else arg_parse cocci_args "in the cocci file";
    arg_parse cl_args "on the command line";
    args := List.filter (function arg ->
              if Filename.check_suffix arg ".cocci"
              then
                begin
                  (if !cocci_file = ""
                  then cocci_file := arg
                  else failwith "only one .cocci file allowed");
                  false
                end
              else true)
              !args;
    args := !args @ !opt_c_files;
    (match (!Flag_parsing_c.cache_prefix,!distrib_index) with
      (Some cp,Some n) ->
        Flag_parsing_c.cache_prefix := Some (Printf.sprintf "%s_%d" cp n)
      | _ -> ());

    (* julia hack so that one can override directories specified on
     * the command line. *)
    dir :=
      (if !args = [] || !test_mode
      then false
      else Common.is_directory (List.hd !args));
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
        else List.hd !args
      in if !Inc.include_path = []
      then
	let i = Filename.concat chosen_dir "include" in
	try
	  if Sys.is_directory i
	  then Inc.include_path := [i]
	with Sys_error _ -> ());
    (* The same thing for file groups *)
    (if !file_groups
    then
      if List.length !args > 1
      then
        begin
          let chosen = List.hd !args in
          pr2 ("ignoring all but the last specified file: "^chosen);
          args := [chosen]
        end);

    args := List.rev !args;

    if !cocci_file <> "" && (not (!cocci_file =~ ".*\\.\\(sgrep\\|spatch\\)$"))
    then cocci_file := Common.adjust_ext_if_needed !cocci_file ".cocci";

    if !Cocciconfig.std_iso <> ""
    then Cocciconfig.std_iso := Common.adjust_ext_if_needed !Cocciconfig.std_iso ".iso";
    if !Cocciconfig.std_h <> ""
    then Cocciconfig.std_h := Common.adjust_ext_if_needed !Cocciconfig.std_h ".h";

    if !Cocciconfig.std_h <> ""
    then Parse_c.init_defs_builtins !Cocciconfig.std_h;

    if !macro_file <> ""
    then Parse_c.init_defs_macros !macro_file;

    let uses_distribution =
      (!distrib_index <> None) || (!distrib_max <> None) || !mod_distrib in
    let uses_parmap =
      (!Flag.parmap_cores <> None) || (!Flag.parmap_chunk_size <> None) in
    if uses_distribution && uses_parmap then begin
      pr2 "error: distribution and parallelism are not compatible";
      exit 1
    end;

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
		(if !Inc.include_path = []
		then Inc.include_path := [prefix^"include"]);
		Testing.testone prefix x !compare_with_expected
	      end
	    else
	      if Sys.file_exists testfile then
	      begin
		(if !Inc.include_path = []
		then Inc.include_path := ["include"]);
		Testing.testone "" x !compare_with_expected
	      end
	      else
	        Printf.eprintf
		  "ERROR: File %s does not exist\n" testfile
	end

    | []  when !c_test_all ->
        (if !Inc.include_path = []
         then Inc.include_path := ["tests/include"]);
        Testing.ctestall
	  (fun file ->
	    run_profile testing_profile;
	    let cocci_args =
	      Cocci_args.read_args [file] +> normalize_args in
	    arg_parse cocci_args "in the cocci file")

    | []  when !cpp_test_all ->
        (if !Inc.include_path = []
         then Inc.include_path := ["cpptests/include"]);
        Testing.cpptestall
	  (fun file ->
	    run_profile testing_profile;
	    let cocci_args =
	      Cocci_args.read_args [file] +> normalize_args in
	    arg_parse cocci_args "in the cocci file")

    | []  when !test_spacing ->
        (if !Inc.include_path = []
         then Inc.include_path := ["tests/include"]);
        Testing.test_spacing
	  (fun file ->
	    run_profile testing_profile;
	    let cocci_args =
	      Cocci_args.read_args [file] +> normalize_args in
	    arg_parse cocci_args "in the cocci file")

    | [] when !test_regression_okfailed ->
        Testing.test_regression_okfailed ()

    | ((x::xs) as cfiles) when !test_okfailed ->
        (* do its own timeout on FC.timeout internally *)
        Inc.for_tests := true;
        adjust_stdin (List.map (fun x -> (x,None)) cfiles) (fun () ->
          Testing.test_okfailed !cocci_file cfiles
          )

    (* --------------------------------------------------------- *)
    (* Actions, useful to debug subpart of coccinelle *)
    (* --------------------------------------------------------- *)

    | xs when List.mem !action (Common.action_list all_actions) ->
        Common.do_action !action xs all_actions

    | [] when !action = "--parse-cocci" ->
	Iso_pattern.verbose_iso := true;
        Testing.test_parse_cocci !cocci_file

    | [] when !action = "--rule-dependencies" ->
        Testing.test_rule_dependencies !cocci_file

     (* I think this is used by some scripts in some Makefile for our
      * big-tests. So don't remove.
      *)
    | [file1;file2] when !action = "--compare-c" ->
       Test_parsing_c.test_compare_c file1 file2 (* result = unix code *)

    (* could add the Test_parsing_c.test_actions such as --parse_c & co *)


    (* --------------------------------------------------------- *)
    (* This is the main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> short_usage()
    ));
    if !Pycocci.initialised && Pycocci.py_isinitialized () then begin
      Pycocci.pyrun_simplestring "cocci.finalise()";
      if !Flag.show_misc
      then Common.pr2 "Finalizing python\n";
      Pycocci.py_finalize ();
    end
  end


let main_with_better_error_report arglist =
  if !Common.debugger then main arglist
  else
    try
      main arglist
    with
    | Unix.Unix_error (e, "stat", filename) ->
        Printf.eprintf "ERROR: File %s does not exist: %s\n"
	  filename (Unix.error_message e);
        raise (UnixExit (-1))
    | Parse_cocci.Bad_virt s ->
	Printf.eprintf "virtual rule %s not supported\n" s;
        raise (UnixExit (-1))
    | Parse_cocci.SMPLParseError error_message
    | Failure error_message ->
	Printf.eprintf "%s\n" error_message;
	Printexc.print_backtrace stderr;
	raise (UnixExit (-1))

(*****************************************************************************)

let __init_cocci_file = !cocci_file
let __init_output_file = !output_file
let __init_tmp_dir = !tmp_dir
let __init_aux_file_suffix = !aux_file_suffix
let __init_outplace_modif = !outplace_modif
let __init_preprocess = !preprocess
let __init_compat_mode = !compat_mode
let __init_ignore_unknown_opt = !ignore_unknown_opt
let __init_profile_per_file = !profile_per_file
let __init_dir = !dir
let __init_ignore = !ignore
let __init_file_groups = !file_groups
let __init_kbuild_info = !kbuild_info
let __init_macro_file = !macro_file
let __init_test_mode = !test_mode
let __init_test_all = !c_test_all
let __init_test_spacing = !test_spacing
let __init_test_okfailed = !test_okfailed
let __init_test_regression_okfailed = !test_regression_okfailed
let __init_action = !action
let __init_compare_with_expected = !compare_with_expected
let __init_distrib_index = !distrib_index
let __init_distrib_max = !distrib_max
let __init_mod_distrib = !mod_distrib
let __init_previous_merges = !previous_merges
let __init_parmap_cores = !Flag.parmap_cores
let __init_parmap_chunk_size = !Flag.parmap_chunk_size
let __init_short_usage_func = !short_usage_func
let __init_long_usage_func = !long_usage_func

let reinitialize _ = (* clean start for invocation from OCaml *)
  cocci_file := __init_cocci_file;
  output_file := __init_output_file;
  tmp_dir := __init_tmp_dir;
  aux_file_suffix := __init_aux_file_suffix;
  outplace_modif := __init_outplace_modif;
  preprocess := __init_preprocess;
  compat_mode := __init_compat_mode;
  ignore_unknown_opt := __init_ignore_unknown_opt;
  profile_per_file := __init_profile_per_file;
  dir := __init_dir;
  ignore := __init_ignore;
  file_groups := __init_file_groups;
  kbuild_info := __init_kbuild_info;
  macro_file := __init_macro_file;
  test_mode := __init_test_mode;
  c_test_all := __init_test_all;
  test_spacing := __init_test_spacing;
  test_okfailed := __init_test_okfailed;
  test_regression_okfailed := __init_test_regression_okfailed;
  action := __init_action;
  compare_with_expected := __init_compare_with_expected;
  distrib_index := __init_distrib_index;
  distrib_max := __init_distrib_max;
  mod_distrib := __init_mod_distrib;
  previous_merges := __init_previous_merges;
  Flag.parmap_cores := __init_parmap_cores;
  Flag.parmap_chunk_size := __init_parmap_chunk_size;
  short_usage_func := __init_short_usage_func;
  long_usage_func := __init_long_usage_func

