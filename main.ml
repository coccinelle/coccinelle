open Common open Commonop

(*****************************************************************************)
(* Flags *)
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

let save_tmp_files = ref false

let reentrant = ref false 

let action = ref "" 

let default_output_file = ref "/tmp/output.c"

(*****************************************************************************)
(* The coccinelle main entry point *)
(*****************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let options = Arg.align [ 
      "-cocci_file", Arg.Set_string cocci_file, 
        " <filename> the semantic patch file";

      "-iso_file",   Arg.Set_string iso_file, 
        " <filename> the iso file";

      "-o", Arg.Set_string default_output_file,
        (" <filename> (default is " ^ !default_output_file ^ ")");
 
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
      "-cocci_vs_c_3",        Arg.Set Flag_engine.use_cocci_vs_c_3,    " ";
      "-casse_initialisation", Arg.Set Flag_parsing_c.casse_initialisation," ";
      "-ifdef", Arg.Set Flag_parsing_c.ifdef_to_if,"convert ifdef to if, buggy!";
      "-add_typedef_root", Arg.Set Flag_parsing_c.add_typedef_root, " ";

      "-sgrep", Arg.Set Flag_parsing_cocci.sgrep_mode, " ";


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
    | [x] when !test_mode    -> Testing.testone x !compare_with_expected
    | []  when !testall_mode -> Testing.testall ()

    | [x] when !test_ctl_foo -> 
        Cocci.full_engine x (Right (Test.foo_ctl ())) !default_output_file

    (* useful to debug *)
    | x::xs when !action <> "" -> 
        (match !action, x::xs with
        | "tokens_c", [file] -> 
            Flag_parsing_c.debug_lexer := true; 
            Flag_parsing_c.verbose_parsing := true;
            Parse_c.tokens file +> Common.pr2gen

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
            let program2_with_ppmethod = 
              program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
            in
            Unparse_c.pp_program program2_with_ppmethod !default_output_file;
            Common.command2 ("cat " ^ !default_output_file);


        | "typeur", [file] -> 
            if not (file =~ ".*\\.c") 
            then pr2 "warning: seems not a .c file";

            ignore(Cocci.cprogram_from_file file);

        | "compare_c", _ -> 
            Testing.print_diff_expected_res_and_exit 
              "parsing_c/tests/equal_modulo1.c" 
              "parsing_c/tests/equal_modulo2.c" 
              false

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
             Cocci.full_engine 
               cfile 
               (Left (cocci_file, iso_file)) 
               generated_file
          with e -> 
            if !dir 
            then pr2 ("EXN:" ^ Printexc.to_string e)
            else raise e
          );

	  Ctlcocci_integration.print_bench();

	  if !save_output_file 
          then Common.command2 ("cp " ^ generated_file ^ saved);

          if !compare_with_expected then 
            Testing.print_diff_expected_res_and_exit
              generated_file expected_res (List.length fullxs = 1)
          );

    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
   )
  );
  Common.profile_diagnostic ();
  end


let _ =
  if not (!Sys.interactive) then 
    Common.exn_to_unixexit (fun () -> 
      Common.finalize
        (fun()-> 
          main ();
          Ctlcocci_integration.print_bench())
        (fun()-> if not !save_tmp_files then Common.erase_temp_files ())
    )
  
