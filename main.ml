open Common open Commonop

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
(* can't put "standard.iso" for iso_file because the user may want to
 * launch spatch from any directory and so to be more consistent,
 * better to not put a default value here.
 *)
let cocci_file = ref ""
let iso_file   = ref "" 
let default_output_file = ref "/tmp/output.c"

let dir = ref false

let test_mode = ref false
let testall_mode = ref false
let test_ctl_foo = ref false

let compare_with_expected = ref false

let save_output_file = ref false (* if true, stores output in file.cocci_res *)
let save_tmp_files = ref false

let reentrant = ref false 

let action = ref "" 
let function_cfg = ref "" (* used in conjonction with -action control_flow *)

let timeout = ref (None : int option)

(*****************************************************************************)
(* Profile *)
(*****************************************************************************)

(* pair of  (list of flags to set true, list of flags to set false *)
let quiet_profile = (
  [
    Flag.show_diff;
  ],
  [
    Flag.show_c;
    Flag.show_cocci;
    Flag.show_flow;
    Flag.show_before_fixed_flow;
    Flag.show_ctl_tex;
    Flag.show_ctl_text;
    Flag.show_transinfo;
    Flag.show_binding_in_out;
    Flag.show_misc;
    Flag_engine.show_misc;
    Flag_parsing_cocci.show_SP;
    Flag_ctl.verbose_ctl_engine;
    Flag_engine.debug_engine;
    Flag_parsing_c.verbose_parsing;
    Flag_parsing_c.verbose_type;
  ])

 
(*****************************************************************************)
(* The coccinelle main entry point *)
(*****************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let shortOptionNumber = 5 in 
    let options = Arg.align [ 
      (* --------------------------------------------------------------- *)
      (* The first shortOptionNumber options will be printed when use
       * only ./spatch. For the rest you have to use -help to see them.  
       *)

      "-cocci_file", Arg.Set_string cocci_file, 
      " <filename> the semantic patch file";

      "-iso_file",   Arg.Set_string iso_file, 
      " <filename> the iso file";

      "-o", Arg.Set_string default_output_file,
      (" <filename> the output file (default="^ !default_output_file^")");


      "-show_flow"              , Arg.Set Flag.show_flow,        " ";

      "-version",   Arg.Unit (fun () -> 
        pr2 "version: $Date$";
        raise (Common.UnixExit 0)
      ), " ";
      (* --------------------------------------------------------- *)

      "-c", Arg.Set_string cocci_file,     " short option of -cocci_file";
      "-i", Arg.Set_string iso_file,       " short option of -iso_file";

 
      "-dir", Arg.Set dir, 
        " <dirname> process all files in directory recursively";

      "-test", Arg.Set test_mode, 
         " automatically find the corresponding c and cocci file";
      "-testall", Arg.Set testall_mode, 
         " ";
      "-test_ctl_foo", Arg.Set test_ctl_foo, 
         " test the engine with the foo ctl in test.ml";

      "-compare_with_expected", Arg.Set compare_with_expected, " "; 
      "-save_output_file",  Arg.Set save_output_file, " ";
      "-save_tmp_files",    Arg.Set save_tmp_files,   " ";
      "-reentrant",         Arg.Set reentrant, " ";

      
      "-no_show_ctl_text"       , Arg.Clear Flag.show_ctl_text,  " ";
      "-no_show_transinfo"      , Arg.Clear Flag.show_transinfo, " ";
      "-show_binding_in_out",     Arg.Set Flag.show_binding_in_out, " ";
      "-no_show_diff"           , Arg.Clear Flag.show_diff, " ";
      "-no_show_misc",   Arg.Unit (fun () -> 
        Flag.show_misc := false;
        Flag_engine.show_misc := false;
        ), " ";

      (* works in conjunction with -show_ctl *)
      "-inline_let_ctl",        Arg.Set Flag.inline_let_ctl, " ";
      "-show_mcodekind_in_ctl", Arg.Set Flag.show_mcodekind_in_ctl, " ";

      "-verbose_ctl_engine",   Arg.Set Flag_ctl.verbose_ctl_engine, " ";
      "-verbose_engine",       Arg.Set Flag_engine.debug_engine,    " ";
      "-partial_match",        Arg.Set Flag_ctl.partial_match, " ";


      "-show_c"                 , Arg.Set Flag.show_c,           " ";
      "-show_cocci"             , Arg.Set Flag.show_cocci,       " ";
      "-show_before_fixed_flow" , Arg.Set Flag.show_before_fixed_flow,  " ";
      "-show_ctl_tex"           , Arg.Set Flag.show_ctl_tex,     " ";

      "-show_SP_julia"       , Arg.Set Flag_parsing_cocci.show_SP,  " ";



      "-no_parse_error_msg", Arg.Clear Flag_parsing_c.verbose_parsing, " ";
      "-no_type_error_msg",  Arg.Clear Flag_parsing_c.verbose_type, " ";

      "-debug_cpp",          Arg.Set  Flag_parsing_c.debug_cpp, " ";
      "-debug_lexer",        Arg.Set  Flag_parsing_c.debug_lexer , " ";
      "-debug_etdt",         Arg.Set  Flag_parsing_c.debug_etdt , "  ";
      "-debug_typedef",      Arg.Set  Flag_parsing_c.debug_typedef, "  ";
      "-debug_cfg",          Arg.Set  Flag_parsing_c.debug_cfg , "  ";

      (* todo: other profile ? *)
      "-quiet",   Arg.Unit (fun () -> 
        let (set_to_true, set_to_false) = quiet_profile in
        List.iter (fun x -> x := false) set_to_false;
        List.iter (fun x -> x := true) set_to_true;
      ), " ";


      "-timeout",              Arg.Int (fun x -> timeout := Some x), 
        "timeout, in seconds";
      
      "-profile",          Arg.Set  Common.profile , "  ";
      "-debugger",         Arg.Set  Common.debugger , "  ";
      "-bench", Arg.Int (function x -> Flag_ctl.bench := x), " ";

      "-loop",              Arg.Set Flag_ctl.loop_in_src_code,    " ";
      "-l1",                Arg.Clear Flag_parsing_c.label_strategy_2, " ";
      "-no_ref",            Arg.Clear Flag_engine.use_ref,    " ";
      "-casse_initialisation", Arg.Set Flag_parsing_c.casse_initialisation," ";
      "-ifdef", Arg.Set Flag_parsing_c.ifdef_to_if, 
        " convert ifdef to if, buggy!";
      "-add_typedef_root", Arg.Set Flag_parsing_c.add_typedef_root, " ";
      "-ng",   Arg.Set Flag_parsing_c.next_gen_parsing," next gen parsing";
      "-nong", Arg.Clear Flag_parsing_c.next_gen_parsing," next gen parsing";

      "-sgrep", Arg.Set Flag_parsing_cocci.sgrep_mode, " ";

      "-function", Arg.Set_string function_cfg, 
      " works with -action control_flow";

      "-action", Arg.Set_string action , 
         (" <action>  (default_value = " ^ !action ^")" ^ 
          "\n\t possibles actions are:

               tokens
               parse_c
               parse_cocci
               control_flow
               parse_unparse
               typeur
               compare_c

         so to test C parser, do -action parse_c ...
                "
       );

    ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ 
                     " [options] <path-to-c-file>\nOptions are:") 
    in
    Arg.parse options (fun file -> args := file::!args) usage_msg;

    let timeout_fn =
      match !timeout with
	Some x -> Common.timeout_function x
      |	None -> (function f -> f()) in
    timeout_fn (fun () -> 

    (* must be done after Arg.parse, because Common.profile is set by it *)
    Common.profile_code "Main total" (fun () -> 
    (match (!args) with

    (* --------------------------------------------------------- *)
    (* The test framework. Works with tests/  *)
    (* --------------------------------------------------------- *)
    | [x] when !test_mode    -> 
        let output_file =
          if !reentrant 
          then Common.new_temp_file "cocci-output" ".c" 
          else !default_output_file
        in
        Testing.testone x !compare_with_expected !iso_file output_file

    | []  when !testall_mode -> 
        Testing.testall !iso_file
    | [x] when !test_ctl_foo -> 
        Cocci.full_engine x (Right (Test.foo_ctl ())) !default_output_file

    (* --------------------------------------------------------- *)
    (* Actions, useful to debug subpart of coccinelle *)
    (* --------------------------------------------------------- *)
    | x::xs when !action <> "" -> 
        (match !action, x::xs with
        | "tokens_c", [file] -> 
            Flag_parsing_c.debug_lexer := true; 
            Flag_parsing_c.verbose_parsing := true;
            Parse_c.tokens file +> Common.pr2gen

        | "parse_c", x::xs -> 
            Flag_parsing_c.debug_cpp := true;
            Flag_parsing_c.debug_typedef := true;

            let fullxs = 
              if !dir
              then Common.process_output_to_list ("find " ^x^" -name \"*.c\"") 
              else x::xs 
            in
            
            let _stat_list = ref [] in
            let newscore  = Common.empty_score () in

            fullxs +> List.iter (fun file -> 
              if not (file =~ ".*\\.c") 
              then pr2 "warning: seems not a .c file";

              pr2 ("PARSING: " ^ file);

              let (_x, stat) = Parse_c.parse_print_error_heuristic file 
              in

              Common.push2 stat _stat_list;
              let s = 
                sprintf "bad = %d, timeout = %B" 
                  stat.Parse_c.bad stat.Parse_c.have_timeout
              in
              if stat.Parse_c.bad = 0 && not stat.Parse_c.have_timeout
              then Hashtbl.add newscore file (Common.Ok)
              else Hashtbl.add newscore file (Common.Pb s)

            );
            if not (null !_stat_list) 
            then Parse_c.print_parsing_stat_list !_stat_list;

            if !dir
            then begin 
              pr2 "--------------------------------";
              pr2 "regression testing  information";
              pr2 "--------------------------------";
              let file = Str.global_replace (Str.regexp "/") "__" x in
              Common.regression_testing newscore 
                ("/tmp/score_parsing__" ^ file ^ ".marshalled");
            end
              



        | "parse_cocci", [file] -> 
            if not (file =~ ".*\\.cocci") 
            then pr2 "warning: seems not a .cocci file";

            if !iso_file <> "" 
            then 
              iso_file := Common.adjust_extension_if_needed !iso_file ".iso";
            let iso_file = if !iso_file = "" then None else Some !iso_file in

            let (xs,_,_) = Cocci.sp_from_file file iso_file  in
            xs +> List.iter Pretty_print_cocci.unparse


        | "show_flow", [file] 
        | "control_flow", [file] -> 
            if not (file =~ ".*\\.c") 
            then pr2 "warning: seems not a .c file";

            let (program, _stat) = Parse_c.parse_print_error_heuristic file in
            program +> List.iter (fun (e,_) -> 
              match e with
              | Ast_c.Definition (((funcs, _, _, c),_) as def)  -> 
                  pr2 funcs;
                  if !function_cfg = "" || !function_cfg = funcs
                  then 

                    (* old: Flow_to_ast.test !Flag.show_flow def *)
                    (try 
                        let flow = Ast_to_flow.ast_to_control_flow def in
                        Ast_to_flow.deadcode_detection flow;
                        let fixed = Ctlcocci_integration.fix_flow_ctl flow in
                        Ograph_extended.print_ograph_extended 
                          ("/tmp/" ^ funcs ^ ".dot") fixed;
                                                   
                    with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
                    )
                  | _ -> ()
            )

        | "parse_unparse", [file] -> 
            let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
            let program2_with_ppmethod = 
              program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
            in
            Unparse_c.pp_program program2_with_ppmethod !default_output_file;
            Common.command2 ("cat " ^ !default_output_file);


        | "typeur", [file] -> 
            if not (file =~ ".*\\.c") 
            then pr2 "warning: seems not a .c file";

            let (program2, _stat) =  Parse_c.parse_print_error_heuristic file
            in
            let program2 =
              program2 
              +> Common.unzip 
              +> (fun (program, infos) -> 
                Type_annoter_c.annotate_program Type_annoter_c.initial_env
                  program +> List.map fst,
                infos
              )
              +> Common.uncurry Common.zip
            in
            let program2_with_ppmethod = 
              program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
            in
            Flag_parsing_c.pretty_print_type_info := true;
            Unparse_c.pp_program program2_with_ppmethod !default_output_file;
            Common.command2 ("cat " ^ !default_output_file);

        | "compare_c", xs -> 
            (match List.rev xs with
            | [file1;file2] -> 
                Testing.print_diff_expected_res_and_exit file1 file2 true
            | _ -> failwith "not enough argument for compare_c"
            )

        | "compare_c_hardcoded", _ -> 
            Testing.print_diff_expected_res_and_exit 
              "parsing_c/tests/compare1.c" 
              "parsing_c/tests/compare2.c" 
              (*
              "parsing_c/tests/equal_modulo1.c" 
              "parsing_c/tests/equal_modulo2.c" 
              *)
              false


        | "rule_regression_info", _ -> 
            let newscore  = Common.empty_score () in
            let oks = 
              (Common.process_output_to_list ("find -name \"*.ok\"") 
                ++
                Common.process_output_to_list ("find -name \"*.spatch_ok\"")
              )
            in
            let failed = 
              Common.process_output_to_list ("find -name \"*.failed\"") 
            in

            if null (oks ++ failed) 
            then failwith "no ok/failed file, you certainly did a make clean"
            else begin
              oks +> List.iter (fun s -> 
                Hashtbl.add newscore (Filename.chop_extension s) 
                  Common.Ok
              );
              failed +> List.iter (fun s -> 
                Hashtbl.add newscore (Filename.chop_extension s) 
                  (Common.Pb "fail")
              );

              pr2 "--------------------------------";
              pr2 "regression testing  information";
              pr2 "--------------------------------";
              Common.regression_testing newscore ("/tmp/score_rule.marshalled")
            end
                



        | "xxx", _ -> 
            Format.print_newline();
            Format.printf "@[<v 5>--@,--@,@[<v 5>--@,--@,@]--@,--@,@]";
            Format.print_newline();
            Format.printf "@[<v>(---@[<v>(---@[<v>(---@,)@]@,)@]@,)@]";


        | s, [] -> Arg.usage options usage_msg; failwith "too few arguments"
        | _ -> failwith "no action for this"
        )
    (* --------------------------------------------------------- *)
    (* This is the main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 

        if (!cocci_file = "") 
        then failwith "I need a cocci file,  use -cocci_file <filename>";

        cocci_file := Common.adjust_extension_if_needed !cocci_file ".cocci";

        if !iso_file <> "" 
        then iso_file := Common.adjust_extension_if_needed !iso_file ".iso";


        (* todo?: for iso could try to go back the parent dir recursively to
         * find the standard.iso 
         *)
        let cocci_file = !cocci_file in
        let iso_file = if !iso_file = "" then None else Some !iso_file in

        let fullxs = 
          if !dir 
          then Common.process_output_to_list ("find " ^ x ^ " -name \"*.c\"")
          else x::xs 
        in

        fullxs +> List.iter (fun cfile -> 

          let cfile = Common.adjust_extension_if_needed cfile ".c" in

          let base = if cfile =~ "\\(.*\\).c$" then matched1 cfile else cfile
          in 
          let generated_file = 
            if !reentrant 
            then Common.new_temp_file "cocci-output" ".c" 
            else !default_output_file
          in
          
          let expected_res = base ^ ".res" in
          let saved = cfile ^ ".cocci_res" in

          (try 
              Cocci.full_engine cfile (Left (cocci_file, iso_file)) 
                generated_file
            with e -> 
              if !dir 
              then pr2 ("EXN:" ^ Printexc.to_string e)
              else raise e
          );
          
	  Ctlcocci_integration.print_bench();

	  if !save_output_file 
          then Common.command2 ("cp " ^ generated_file ^ " " ^ saved);
          
          if !compare_with_expected then 
            Testing.print_diff_expected_res_and_exit
              generated_file expected_res (List.length fullxs = 1)
        ) (* iter *)

    | [] -> 
        Arg.usage (Common.take shortOptionNumber options) usage_msg; 
        pr2 "To get the full list of options, use -help";
        pr2 "Example of use:";
        pr2 "  ./spatch -cocci_file foo.cocci foo.c";
        pr2 "";
        raise (UnixExit (-1));
   )
  ));
  end

(*****************************************************************************)
let _ =
  if not (!Sys.interactive) then 
    Common.exn_to_unixexit (fun () -> 
      (* The finalize makes it tedious to go back to exn when use
       * 'back' in the debugger. Hence this special case. *)
      if Sys.argv +> Array.to_list +> List.exists (fun x -> x ="-debugger")
      then main ()
      else 
        Common.finalize (fun()-> Common.pp_do_in_zero_box (fun () -> 
          main ();
          Ctlcocci_integration.print_bench();
          Common.profile_diagnostic ();
        ))
          (fun()-> if not !save_tmp_files then Common.erase_temp_files ())
    )
      
