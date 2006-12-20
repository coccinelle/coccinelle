open Common open Commonop

(*****************************************************************************)
(* This module handle the IO, the special name of files, ... The pure
 * algorithmic stuff is in other modules. *)
(*****************************************************************************)

let dir = ref false

(* can't put "standard.iso" for iso_file because the user may want to
 * launch spatch from any directory and so to be more consistent,
 * better to not put a default value for iso_file.
 *)
let cocci_file = ref ""
let iso_file   = ref "" 

let test_mode = ref false
let testall_mode = ref false
let test_ctl_foo = ref false

let compare_with_expected = ref false
let save_output_file = ref false (* if true, stores output in file.cocci_res *)

let action = ref "" 

(*****************************************************************************)
let print_diff_expected_res_and_exit generated_file expected_res doexit = 
  if not (Common.lfile_exists expected_res)
  then failwith ("no such .res file: " ^ expected_res);
  let a = Cocci.cprogram_from_file generated_file +> List.map fst in
  let b = Cocci.cprogram_from_file expected_res   +> List.map fst in

  let (correct, diffxs) =  
    Compare_c.compare  (a, generated_file)  (b, expected_res)
  in
  match correct with
  | Compare_c.Correct -> 
      pr2 ("seems correct (comparing to " ^ expected_res ^ ")");
      if doexit then exit 0
  | Compare_c.Incorrect s -> 
      pr2 ("seems incorrect: " ^ s);
      pr2 "diff (result(-) vs expected_result(+)) = ";
      diffxs +> List.iter pr2;
      if doexit then exit (-1)
  | Compare_c.IncorrectOnlyInNotParsedCorrectly -> 
      pr2 "seems incorrect, but only because of code that was not parsable";
      if doexit then exit (-1)
  


(*****************************************************************************)
(* There can have multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and optional multiple
 * base_vernn.c and base_vernn.res 
 *)
let testone x = 
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in
  let cfile      = "tests/" ^ x ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in
  let iso_file = Some (if !iso_file = "" then "standard.iso" else !iso_file) in
  begin
    Cocci.full_engine cfile (Left (cocci_file, iso_file));

    let expected_res   = "tests/" ^ base ^ ".res" in
    let generated_file = ("/tmp/output.c") in
    if !compare_with_expected 
    then print_diff_expected_res_and_exit generated_file expected_res true;
  end
          

(*****************************************************************************)
let testall () =

  let _total = ref 0 in
  let _good  = ref 0 in

  let expected_result_files = 
    Common.readdir_to_file_list "tests/" +> List.filter (fun s -> 
      s =~ ".*\\.res$" && Common.filesize ("tests/" ^ s) > 0
    ) +> List.sort compare
  in

  let diagnose = ref [] in
  let add_diagnose s = Common.push2 s diagnose in

  begin
   expected_result_files +> List.iter (fun res -> 
    let x = if res =~ "\\(.*\\).res" then matched1 res else raise Impossible in
    let base = if x =~ "\\(.*\\)_ver[0-9]+" then matched1 x else x in 
    let cfile      = "tests/" ^ x ^ ".c" in
    let cocci_file = "tests/" ^ base ^ ".cocci" in
    let iso_file = Some (if !iso_file = "" then "standard.iso" else !iso_file) 
    in

    pr2 ("Test: " ^ x);

    add_diagnose (sprintf "%s:\t" x);
    incr _total;

    let timeout_value = 30 in

    try (
      Common.timeout_function timeout_value (fun () -> 
        
        Cocci.full_engine cfile (Left (cocci_file, iso_file));
        let generated = "/tmp/output.c" in
        let expected = "tests/" ^ res in

        let a = Cocci.cprogram_from_file generated +> List.map fst in
        let b = Cocci.cprogram_from_file expected  +> List.map fst in

        let (correct, diffxs) = Compare_c.compare (a, generated) (b, expected)
        in
	pr2 res;
	Ctlcocci_integration.print_bench();

        (match correct with
        | Compare_c.Correct -> 
            incr _good; 
            add_diagnose "CORRECT\n" 
        | Compare_c.Incorrect s -> 
            add_diagnose ("INCORRECT:" ^ s ^ "\n");
            add_diagnose "    diff (result(<) vs expected_result(>)) = \n";
            diffxs +> List.iter (fun s -> add_diagnose ("    " ^ s ^ "\n"));
        | Compare_c.IncorrectOnlyInNotParsedCorrectly -> 
            add_diagnose "seems incorrect, but only because of code that was not parsable";
        )
      )
     )
    with exn -> 
      add_diagnose "PROBLEM\n";
      add_diagnose ("   exn = " ^ Printexc.to_string exn ^ "\n")
    );

    pr2 "----------------------";
    pr2 "statistics";
    pr2 "----------------------";
    !diagnose +> List.rev +> List.iter (fun s -> print_string s; );
    flush stdout; flush stderr;

    pr2 "----------------------";
    pr2 "total score";
    pr2 "----------------------";
    pr2 (sprintf "good = %d/%d" !_good !_total);

  end

(*****************************************************************************)
let casse_initialisation prog = 
  let bigf = { Visitor_c.default_visitor_c_s with 
     Visitor_c.kstatement_s = (fun (k, bigf) st -> 
       match st with 
       | Ast_c.Compound statxs, ii -> 
           Ast_c.Compound [], ii
       | _ -> k st
       );
  }
  in
  prog +> List.map (fun (elem, x) -> 
    elem +> Visitor_c.visitor_program_k_s bigf,
    x
  )


(*****************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let options = Arg.align [ 
      "-cocci_file", Arg.Set_string cocci_file, 
        " <filename> the semantic patch file";

      "-iso_file",   Arg.Set_string iso_file, 
        " <filename> the iso file";


      "-dir", Arg.Set dir, 
        " <dirname> process all files in directory recursively";

      "-test", Arg.Set test_mode, 
         " automatically find the corresponding c and cocci file";
      "-testall", Arg.Set testall_mode, 
         " ";
      "-test_ctl_foo", Arg.Set test_ctl_foo, 
         " test the engine with the foo ctl in test.ml";

      "-compare_with_expected", Arg.Set compare_with_expected, " "; 
      "-save_output_file", Arg.Set save_output_file, " ";
      "-bench", Arg.Int (function x -> Flag_ctl.bench := x), " ";

      
      "-show_c"                 , Arg.Set Flag.show_c,           " ";
      "-show_cocci"             , Arg.Set Flag.show_cocci,       " ";
      "-show_flow"              , Arg.Set Flag.show_flow,        " ";
      "-show_before_fixed_flow" , Arg.Set Flag.show_before_fixed_flow,  " ";
      "-show_ctl_tex"           , Arg.Set Flag.show_ctl_tex,     " ";

      "-no_show_ctl_text"       , Arg.Clear Flag.show_ctl_text,  " ";
      "-no_show_transinfo"      , Arg.Clear Flag.show_transinfo, " ";
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

      "-no_parse_error_msg", Arg.Clear Flag_parsing_c.verbose_parsing, " ";
      "-no_type_error_msg", Arg.Clear Flag_parsing_c.verbose_type, " ";

      "-debug_cpp",          Arg.Set  Flag_parsing_c.debug_cpp, " ";
      "-debug_lexer",        Arg.Set  Flag_parsing_c.debug_lexer , " ";
      "-debug_etdt",         Arg.Set  Flag_parsing_c.debug_etdt , "  ";
      "-debug_typedef",      Arg.Set  Flag_parsing_c.debug_typedef, "  ";
      "-debug_cfg",          Arg.Set  Flag_parsing_c.debug_cfg , "  ";
      
      "-profile",          Arg.Set  Common.profile , "  ";

      "-loop",                 Arg.Set Flag_ctl.loop_in_src_code,    " ";
      "-l1",     Arg.Clear Flag_parsing_c.label_strategy_2, " ";
      "-cocci_vs_c",          Arg.Set Flag_engine.use_cocci_vs_c,    " ";
      "-casse_initialisation", Arg.Set Flag_parsing_c.casse_initialisation," ";

      "-sgrep", Arg.Set Flag_parsing_cocci.sgrep_mode, " ";
      "-ifdef", Arg.Set Flag_parsing_c.ifdef_to_if,"convert ifdef to if, buggy!";

      "-action", Arg.Set_string action , 
         (" <action>  (default_value = " ^ !action ^")" ^ 
          "\n\t possibles actions are:

               tokens
               parse_c
               parse_cocci
               control_flow

         so to test C parser, do -action parse_c ...
                "
       );

    ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ 
                     " [options] <path-to-c-dir>\nOptions are:") 
    in
    Arg.parse options (fun file -> args := file::!args) usage_msg;

    (* must be done after Arg.parse, because Common.profile is set by it *)
    Common.profile_code "Main total" (fun () -> 
    (match (!args) with

    (* the test framework. Works with tests/  *)
    | [x] when !test_mode    -> testone x 
    | []  when !testall_mode -> testall ()

    | [x] when !test_ctl_foo -> Cocci.full_engine x (Right (Test.foo_ctl ()))

    (* useful to debug *)
    | x::xs when !action <> "" -> 
        (match !action, x::xs with
        | "tokens_c", [file] -> 
            Flag_parsing_c.debug_lexer := true; 
            Flag_parsing_c.verbose_parsing := true;
            Parse_c.tokens file +> pr2gen
        | "parse_c", x::xs -> 
            let fullxs = 
              if !dir
              then Common.process_output_to_list ("find " ^x^" -name \"*.c\"") 
              else x::xs 
            in
            
            let _stat_list = ref [] in

            fullxs +> List.iter (fun file -> 
              pr2 ("HANDLING: " ^ file);
              
              if not (file =~ ".*\\.c") 
              then pr2 "warning: seems not a .c file";
              
              file +> Parse_c.parse_print_error_heuristic 
                   +> (fun (x, stat) -> Common.push2 stat _stat_list);
            );
            if not (null !_stat_list) 
            then Parse_c.print_parsing_stat_list !_stat_list;

        | "parse_cocci", [file] -> 
            if not (file =~ ".*\\.cocci") 
            then pr2 "warning: seems not a .cocci file";
            let (xs,_,_) = Cocci.sp_from_file file None in
            xs +> List.iter Pretty_print_cocci.unparse
        | "control_flow", [file] -> 
            if not (file =~ ".*\\.c") 
            then pr2 "warning: seems not a .c file";
            file 
              +> Parse_c.parse_print_error_heuristic
              +> (fun (program, stat) -> 
                program +> List.iter (fun (e,_) -> 
                  match e with
                  | Ast_c.Definition (((funcs, _, _, c),_) as def)  -> 
                      pr2 funcs;
                      (try 
                        Flow_to_ast.test !Flag.show_flow def
                      with 
                      | Ast_to_flow.DeadCode None      -> pr2 "deadcode detected, but cant trace back the place"
                      | Ast_to_flow.DeadCode (Some info)-> pr2 ("deadcode detected: " ^ (error_message file ("", info.charpos) ))
                        
                      )
                    
                  | _ -> ()
                 );
                 )
        | "parse_unparse", [file] -> 
            let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
            let program2_with_method = 
              program2 
              (* +> casse_initialisation *) (* done in parser_c.mly now *)
              +> List.map (fun x -> x, Unparse_c.PPnormal)
            in
            Unparse_c.pp_program program2_with_method "/tmp/output.c";
            Common.command2 "cat /tmp/output.c";


        | "typeur", [file] -> 
            if not (file =~ ".*\\.c") 
            then pr2 "warning: seems not a .c file";
            ignore(Cocci.cprogram_from_file file);

        | s, [] -> Arg.usage options usage_msg; failwith "too few arguments"
        | _ -> failwith "no action for this"
        )

    (* This is the main entry *)
    | x::xs -> 

        if (!cocci_file = "") 
        then failwith "I need a cocci file,  use -cocci_file <filename>";

        if not (!cocci_file =~ ".*\\.cocci") 
        then pr2 "warning: seems not a .cocci file";

        if !iso_file <> "" && not (!iso_file =~ ".*\\.iso") 
        then pr2 "warning: seems not a .iso file";

        (* todo?: for iso could try to go back the parent dir recursively to
           find the standard.iso *)
        let cocci_file = !cocci_file in
        let iso_file = if !iso_file = "" then None else Some !iso_file in

        let fullxs = 
          if !dir 
          then begin
            assert (xs = []); 
            Common.process_output_to_list ("find " ^ x ^ " -name \"*.c\"")
          end 
          else x::xs 
        in

        fullxs +> List.iter (fun cfile -> 
         (try Cocci.full_engine cfile (Left (cocci_file, iso_file))
          with e -> 
            if !dir 
            then pr2 ("EXN:" ^ Printexc.to_string e)
            else raise e
          );

          let base = if cfile =~ "\\(.*\\).c$" then matched1 cfile else cfile
          in 
          let generated_file = ("/tmp/output.c") in
          let expected_res = base ^ ".res" in
          let saved = cfile ^ ".cocci_res" in

	  Ctlcocci_integration.print_bench();

	  if !save_output_file 
          then Common.command2 ("cp /tmp/output.c "^ saved);

          if !compare_with_expected then 
            print_diff_expected_res_and_exit generated_file expected_res 
              (List.length fullxs = 1)
          );

    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
   )
  );
  Common.profile_diagnostic ();
  end


let _ =
  if not (!Sys.interactive) then begin
    main ();
    Ctlcocci_integration.print_bench()
  end
