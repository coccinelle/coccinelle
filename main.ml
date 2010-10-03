(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
* This file is part of Coccinelle.
* 
* Coccinelle is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, according to version 2 of the License.
* 
* Coccinelle is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
* 
* The authors reserve the right to distribute this or future versions of
* Coccinelle under other licenses.
*)


open Common

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in "the spatch options" section below), the
 * spatch program also depends on external files, described in
 * globals/config.ml, mainly a standard.h and standard.iso file *)

let cocci_file = ref ""

let output_file = ref ""
let inplace_modif = ref false  (* but keeps a .cocci_orig *)
let outplace_modif = ref false (* generates a .cocci_res  *)

(* could be avoided by using Common.files_of_dir_or_files instead *)
let dir = ref false 

let include_headers = ref false
let kbuild_info = ref ""

(* test mode *)
let test_mode = ref false
let test_all = ref false
let test_okfailed = ref false
let test_regression_okfailed = ref false


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
let quiet_profile = (
  [
  ],
  [
    (* Flag_cocci.show_diff;   just leave this as it is *)

    Flag.show_misc;
    Flag.show_trying;

    Flag_cocci.show_c;
    Flag_cocci.show_cocci;
    Flag_cocci.show_flow;
    Flag_cocci.show_before_fixed_flow;
    Flag_cocci.show_ctl_tex;
    Flag_cocci.show_ctl_text;
    Flag_cocci.show_transinfo;
    Flag_cocci.show_binding_in_out;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;
    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;
    Flag_engine.debug_engine;
    Flag_parsing_c.debug_unparsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_parsing;
  ])

let pad_profile = (
  [
    Flag_cocci.show_diff;
  ],
  [

    Flag.show_misc;

    Flag_cocci.show_c;
    Flag_cocci.show_cocci;
    Flag_cocci.show_flow;
    Flag_cocci.show_before_fixed_flow;
    Flag_cocci.show_ctl_tex;
    Flag_cocci.show_ctl_text;
    Flag_cocci.show_transinfo;
    Flag_cocci.show_binding_in_out;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;
    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;
    Flag_engine.debug_engine;
    Flag_parsing_c.debug_unparsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_parsing;
  ])

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
  "-inplace", Arg.Set inplace_modif,
  "   do the modification on the file directly";
  "-outplace", Arg.Set outplace_modif,
  "   store modifications in a .cocci_res file";

  "-U", Arg.Int (fun n -> Flag_parsing_c.diff_lines := Some (i_to_s n)), 
  "  set number of diff context lines";
  "-partial_match",        Arg.Set Flag_ctl.partial_match, 
  "    report partial matches of the SP on the C file";

  "-iso_file", Arg.Set_string Config.std_iso,   
  " <file> (default=" ^ !Config.std_iso ^")";
  "-macro_file", Arg.Set_string Config.std_h,
  " <file> (default=" ^ !Config.std_h ^ ")";

  "-all_includes",   Arg.Set Flag_cocci.all_includes,
  "  causes all available include files to be used";
  "-no_includes",   Arg.Set Flag_cocci.no_includes,
  "  causes not even local include files to be used";
  "-I",   Arg.Set_string Flag_cocci.include_path,
  "  <dir> containing the Linux headers (optional)";


  "-dir", Arg.Set dir,
  "    <dir> process all files in directory recursively";

  "-include_headers", Arg.Set include_headers,
  "    process header files independently";
  "-use_glimpse", Arg.Set Flag.use_glimpse,
  "    works with -dir, use info generated by glimpseindex";
  "-patch", Arg.String (function s -> Flag.patch := Some s),
  ("    <dir> path name with respect to which a patch should be created\n"^
   "    \"\" for a file in the current directory");
  "-kbuild_info", Arg.Set_string kbuild_info, 
  "    <file> improve -dir by grouping related c files";
  "-pyoutput", Arg.Set_string Flag.pyoutput,
  "    Sets output routine: Standard values: <coccilib.output.Gtk|coccilib.output.Console>";


  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "spatch version: %s" Config.version);
    exit 0;
  ), 
    "  guess what";

  "-date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2008/10/11 16:22:38 $";
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
    "-cocci_file", Arg.Set_string cocci_file, 
    "   <file> the semantic patch file";
    "-c", Arg.Set_string cocci_file,     " short option of -cocci_file";
    "-iso", Arg.Set_string Config.std_iso,   " short option of -iso_file";
    "-D",   Arg.Set_string Config.std_h,     " short option of -macro_file";
  ];

  "most useful show options", 
  "",
  [
    "-show_diff"           , Arg.Set Flag_cocci.show_diff, " ";
    "-no_show_diff"           , Arg.Clear Flag_cocci.show_diff, " ";
    "-show_flow"              , Arg.Set Flag_cocci.show_flow,        " ";
    "-no_show_ctl_text"       , Arg.Clear Flag_cocci.show_ctl_text,  " ";
    (* works in conjunction with -show_ctl *)
    "-ctl_inline_let",       Arg.Set Flag_cocci.inline_let_ctl, " ";
    "-ctl_show_mcodekind",   Arg.Set Flag_cocci.show_mcodekind_in_ctl, " ";
    "-show_binding_in_out",  Arg.Set Flag_cocci.show_binding_in_out, " ";
    "-no_show_transinfo",    Arg.Clear Flag_cocci.show_transinfo, " ";
    "-no_show_misc",         Arg.Clear Flag.show_misc, " ";
    "-show_trying",          Arg.Set Flag.show_trying,
    " show the name of each function being processed";
    "-show_dependencies",
    Arg.Unit (function _ -> Flag_cocci.show_dependencies := true;
      Flag_cocci.show_binding_in_out := true),
    " show the dependencies related to each rule";
  ];

  "verbose subsystems options",  
  "",
  [
    "-verbose_ctl_engine",   Arg.Set Flag_ctl.verbose_ctl_engine, " ";
    "-verbose_match",   Arg.Set Flag_ctl.verbose_match, " ";
    "-verbose_engine",       Arg.Set Flag_engine.debug_engine,    " ";

    "-no_parse_error_msg", Arg.Clear Flag_parsing_c.verbose_parsing, " ";
    "-no_type_error_msg",  Arg.Clear Flag_parsing_c.verbose_type, " ";
    (* could also use Flag_parsing_c.options_verbose *)
  ];

  "other show options",
  "",
  [
    "-show_c"                 , Arg.Set Flag_cocci.show_c,           " ";
    "-show_cocci"             , Arg.Set Flag_cocci.show_cocci,       " ";
    "-show_before_fixed_flow" , Arg.Set Flag_cocci.show_before_fixed_flow,  " ";
    "-show_ctl_tex"           , Arg.Set Flag_cocci.show_ctl_tex,     " ";
    "-show_SP_julia"       ,    Arg.Set Flag_parsing_cocci.show_SP,  " ";
  ];


  "debug C parsing/unparsing",
  "",
  [
    "-debug_cpp",          Arg.Set  Flag_parsing_c.debug_cpp, " ";
    "-debug_lexer",        Arg.Set  Flag_parsing_c.debug_lexer , " ";
    "-debug_etdt",         Arg.Set  Flag_parsing_c.debug_etdt , "  ";
    "-debug_typedef",      Arg.Set  Flag_parsing_c.debug_typedef, "  ";

    "-filter_msg",      Arg.Set  Flag_parsing_c.filter_msg , 
    "  filter some cpp message when the macro is a \"known\" cpp construct";
    "-filter_define_error",Arg.Set Flag_parsing_c.filter_define_error,"  ";
    "-filter_classic_passed",Arg.Set Flag_parsing_c.filter_classic_passed,"  ";

    "-debug_cfg",          Arg.Set Flag_parsing_c.debug_cfg , "  ";
    "-debug_unparsing",      Arg.Set  Flag_parsing_c.debug_unparsing, "  ";

  ];
  (* could use Flag_parsing_c.options_debug_with_title instead *)


  "shortcut for enabling/disabling a set of debugging options at once",
  "",
  [
    (* todo: other profile ? *)
    "-quiet",   Arg.Unit (fun () -> 
      let (set_to_true, set_to_false) = quiet_profile in
      List.iter (fun x -> x := false) set_to_false;
      List.iter (fun x -> x := true) set_to_true;
    ), " ";

    "-pad",   Arg.Unit (fun () -> 
      let (set_to_true, set_to_false) = pad_profile in
      List.iter (fun x -> x := false) set_to_false;
      List.iter (fun x -> x := true) set_to_true;
    ), " ";

  ];

  "bench options",
  "",
  [
    "-profile", Arg.Unit (function () -> Common.profile := Common.PALL) , 
    "   gather timing information about the main coccinelle functions";
    "-bench", Arg.Int (function x -> Flag_ctl.bench := x), 
    "   <level> for profiling the CTL engine";
    "-timeout", Arg.Int (fun x -> Flag_cocci.timeout := Some x), 
    "   <sec> timeout in seconds";
    "-steps", Arg.Int (fun x -> Flag_ctl.steps := Some x), 
    "   max number of model checking steps per code unit";
    "-track_iso", Arg.Set Flag.track_iso_usage,
    "   gather information about isomorphism usage";
    "-profile_iso",
    Arg.Unit
    (function () ->
      Common.profile:=PSOME ["parse cocci";"mysat";"asttoctl2";"full_engine"]),
    "   gather information about the cost of isomorphism usage"
  ];



  "change of algorithm options",
  "", 
  [  
    "-popl", Arg.Set Flag_cocci.popl, 
    "    simplified SmPL, for the popl paper";

    "-popl_mark_all",
    Arg.Unit
    (function _ -> Flag_cocci.popl := true; Flag_popl.mark_all := true), 
    "    simplified SmPL, for the popl paper";

    "-popl_keep_all_wits",
    Arg.Unit
    (function _ -> Flag_cocci.popl := true; Flag_popl.keep_all_wits := true), 
    "    simplified SmPL, for the popl paper";

    "-hrule", Arg.String (function s -> Flag.make_hrule := Some s),
    "    semantic patch generation";

    "-loop",              Arg.Set Flag_ctl.loop_in_src_code,    " ";

    "-l1",                Arg.Clear Flag_parsing_c.label_strategy_2, " ";
    "-ifdef",              Arg.Set Flag_parsing_c.ifdef_to_if, 
    "   convert ifdef to if (buggy!)";
    "-noif0_passing",   Arg.Clear Flag_parsing_c.if0_passing, 
    " ";
    "-noadd_typedef_root",   Arg.Clear Flag_parsing_c.add_typedef_root, " ";
    (* could use Flag_parsing_c.options_algo instead *)


    "-disallow_nested_exps", Arg.Set Flag_engine.disallow_nested_exps,
       "disallow an expresion pattern from matching a term and its subterm";
    "-disable_worth_trying_opt", Arg.Clear Flag_cocci.worth_trying_opt,
    "  ";
    "-only_return_is_error_exit",
    Arg.Set Flag_engine.only_return_is_error_exit,
    "if this flag is not set, then break and continue are also error exits";
    (* the following is a hack to make it easier to add code in sgrep-like
       code, essentially to compensate for the fact that we don't have
       any way of printing things out *)
    "-allow_inconsistent_paths",
    Arg.Set Flag_engine.allow_inconsistent_paths,
    "if this flag is set don't check for inconsistent paths; dangerous";    
  ];

  "misc options",
  "",
  [
    "-debugger",         Arg.Set Common.debugger , 
    "   option to set if launch spatch in ocamldebug";
    "-disable_once",     Arg.Set Common.disable_pr2_once, 
    "   to print more messages";
    "-save_tmp_files",   Arg.Set Common.save_tmp_files,   " ";
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
    (* could use Flag_parsing_c.options_pad instead *)
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
    "-relax_include_path", Arg.Set Flag_cocci.relax_include_path,
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

(* copy paste of Arg.parse. Don't want the default -help msg *)
let arg_parse2 l f msg =
  (try
    Arg.parse_argv Sys.argv l f msg;
  with
  | Arg.Bad msg -> (* eprintf "%s" msg; exit 2; *)
      let xs = Common.lines msg in
      (* take only head, it's where the error msg is *)
      pr2 (List.hd xs);
      !short_usage_func();
      raise (Common.UnixExit (2))
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

let adjust_stdin cfile k =
  if !dir
  then k()
  else
    let newin = 
      let (dir, base, ext) = Common.dbe_of_filename cfile in
      let varfile = Common.filename_of_dbe (dir, base, "var") in
      if ext = "c" && Common.lfile_exists varfile
      then Some varfile
      else None in
    Common.redirect_stdin_opt newin k

let glimpse_filter (coccifile, isofile) dir = 
  let (astcocci,_free_var_lists,_negated_positions,
       _used_after_lists,_positions_lists,_,query) =
    Cocci.sp_of_file coccifile (Some isofile) in
  match query with
    None -> pr2 "no glimpse keyword infered from snippet"; None
  | Some query ->
      let suffixes = if !include_headers then ["c";"h"] else ["c"] in
      pr2 ("glimpse request = " ^ query);
      let command = spf "glimpse -y -H %s -N -W -w '%s'" dir query in
      let (glimpse_res,stat) = Common.cmd_to_list_and_status command in
      match stat with
	Unix.WEXITED(0) | Unix.WEXITED(1) ->
	  Some
	    (glimpse_res +>
	     List.filter
	       (fun file -> List.mem (Common.filesuffix file) suffixes))
      |	_ -> None (* error, eg due to pattern too big *)
	
	


(*****************************************************************************)
(* The coccinelle main entry point *)
(*****************************************************************************)
let main () = 
  begin
    let args = ref [] in

    arg_parse2 (Arg.align all_options) (fun x -> args := x::!args) usage_msg;

    (if !dir && List.length !args > 1
    then
      begin
	let chosen = List.hd !args in
	pr2 ("ignoring all but the last specified directory: "^chosen);
	args := [chosen]
      end);
    args := List.rev !args;

    (if !Flag_cocci.all_includes && !Flag_cocci.no_includes
    then failwith "cannot set both all_includes and no_includes");

    if !cocci_file <> "" && (not (!cocci_file =~ ".*\\.\\(sgrep\\|spatch\\)$"))
    then cocci_file := Common.adjust_ext_if_needed !cocci_file ".cocci";

    if !Config.std_iso <> "" 
    then Config.std_iso := Common.adjust_ext_if_needed !Config.std_iso ".iso";
    if !Config.std_h <> "" 
    then Config.std_h := Common.adjust_ext_if_needed !Config.std_h ".h";

    if !Config.std_h <> "" 
    then Parse_c.init_defs !Config.std_h;


    (* must be done after Arg.parse, because Common.profile is set by it *)
    Common.profile_code "Main total" (fun () -> 


    let all_actions = Test_parsing_c.actions() in

    (match (!args) with

    (* --------------------------------------------------------- *)
    (* The test framework. Works with tests/ or .ok and .failed  *)
    (* --------------------------------------------------------- *)
    | [x] when !test_mode    -> 
        Flag_cocci.include_path := "tests/include";
        Testing.testone x !compare_with_expected

    | []  when !test_all -> 
        Flag_cocci.include_path := "tests/include";
        Testing.testall ()

    | [] when !test_regression_okfailed -> 
        Testing.test_regression_okfailed ()

    | x::xs when !test_okfailed -> 
        (* do its own timeout on Flag_cocci.timeout internally *)
        Flag_cocci.relax_include_path := true;
	adjust_stdin x (fun () -> 
          Testing.test_okfailed !cocci_file (x::xs)
        )

    (* --------------------------------------------------------- *)
    (* Actions, useful to debug subpart of coccinelle *)
    (* --------------------------------------------------------- *)

    | xs when List.mem !action (Common.action_list all_actions) -> 
        Common.do_action !action xs all_actions

    | [file] when !action = "-parse_cocci" -> 
        Testing.test_parse_cocci file

     (* I think this is used by some scripts in some Makefile for our
      * big-tests. So dont remove.
      *)
    | [file1;file2] when !action = "-compare_c" -> 
       Test_parsing_c.test_compare_c file1 file2 (* result = unix code *)

    (* could add the Test_parsing_c.test_actions such as -parse_c & co *)


    (* --------------------------------------------------------- *)
    (* This is the main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
	adjust_stdin x (fun () ->
          if !cocci_file = ""
          then failwith "I need a cocci file,  use -sp_file <file>";

	  if !dir && !Flag.patch = None
	  then
	    (match xs with
	    | [] -> Flag.patch := Some x
	    | _ ->
		pr2
		  ("warning: patch output can only be created when only one\n"^
		   "directory is specified or when the -patch flag is used")
            );

          let infiles = 
            Common.profile_code "Main.infiles computation" (fun () -> 
	      match !dir, !kbuild_info, !Flag.use_glimpse with
              (* glimpse *)
              | false, _, true -> 
                  failwith "-use_glimpse works only with -dir"
              | true, s, true when s <> "" -> 
                  failwith "-use_glimpse does not work with -kbuild"
              | true, "", true -> 
                  if not (null xs)
                  then failwith "-use_glimpse can accept only one dir";
                  
                  let files =
		    match glimpse_filter (!cocci_file, !Config.std_iso) x with
		      None ->
			Common.cmd_to_list (* same as "true, "", _" case *)
			  (if !include_headers
			  then ("find "^(join " " (x::xs))^" -name \"*.[ch]\"")
			  else ("find "^(join " " (x::xs))^" -name \"*.c\""))
		    | Some files -> files in
                  files +> List.map (fun x -> [x])
              (* normal *)
	      | false, _, _ -> [x::xs]
	      | true, "", _ -> 
		  Common.cmd_to_list
		    (if !include_headers
		    then ("find "^(join " " (x::xs))^" -name \"*.[ch]\"")
		    else ("find "^(join " " (x::xs))^" -name \"*.c\""))
		  +> List.map (fun x -> [x])

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
	    
          let outfiles = 
            Common.profile_code "Main.outfiles computation" (fun () -> 
	      infiles +> List.map (fun cfiles -> 
		pr2 ("HANDLING: " ^ (join " " cfiles));
		Common.timeout_function_opt !Flag_cocci.timeout (fun () -> 
  	        Common.report_if_take_time 10 (join " " cfiles) (fun () -> 
                    (* Unix.sleep 1; *)
                  try 
                    (* this is the main call *)
                    Cocci.full_engine (!cocci_file, !Config.std_iso) cfiles
		  with 
		  | Common.UnixExit x -> raise (Common.UnixExit x)
		  | e -> 
		      if !dir
		      then begin
			pr2 ("EXN:" ^ Printexc.to_string e); 
			[] (* *)
		      end
		      else raise e)))
            ) +> List.concat 
          in

          Common.profile_code "Main.result analysis" (fun () -> 
	      
	    Ctlcocci_integration.print_bench();
	      
            let outfiles = Cocci.check_duplicate_modif outfiles in
              
            outfiles +> List.iter (fun (infile, outopt) -> 
	      outopt +> Common.do_option (fun outfile -> 
		if !inplace_modif
		then begin
                  Common.command2 ("cp "^infile^" "^infile^".cocci_orig");
                  Common.command2 ("cp "^outfile^" "^infile);
		end;

		if !outplace_modif
		then Common.command2 ("cp "^outfile^" "^infile^".cocci_res");
		  
		if !output_file = "" 
		then begin
                  let tmpfile = "/tmp/"^Common.basename infile in
                  pr2 (spf "One file modified. Result is here: %s" tmpfile);
                  Common.command2 ("cp "^outfile^" "^tmpfile);
		end
	      ));
            if !output_file <> "" then
	      (match outfiles with 
	      | [infile, Some outfile] when infile = x && null xs -> 
                  Common.command2 ("cp " ^outfile^ " " ^ !output_file);
	      | [infile, None] when infile = x && null xs -> 
                  Common.command2 ("cp " ^infile^ " " ^ !output_file);
	      | _ -> 
                  failwith 
                    ("-o can not be applied because there is multiple " ^
                        "modified files");
	      );
            
            if !compare_with_expected
            then Testing.compare_with_expected outfiles))
          
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

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
    Ctlcocci_integration.print_bench();
  )
