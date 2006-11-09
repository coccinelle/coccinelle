open Common open Commonop

(*****************************************************************************)
(* 
 * This module handle the IO, the special name of files, ... 
 * The pure algorithmic stuff is in other modules.
 *)
(*****************************************************************************)

let dir = ref false

let cocci_file = ref ""
let iso_file   = ref ""

let test_mode = ref false
let testall_mode = ref false
let test_ctl_foo = ref false

let compare_with_expected = ref false
let save_output_file = ref false (* if true, stores output in file.cocci_res *)


let action = ref "" 


let mktmp = Cocci.mktmp

(*****************************************************************************)
let print_diff_expected_res_and_exit generated_file expected_res doexit = 
  if Common.lfile_exists expected_res
  then 
    let (correct, diffxs) = 
      Compare_c.compare 
        (Cocci.cprogram_from_file generated_file)
        (Cocci.cprogram_from_file expected_res)
    in
    (match correct with
    | Compare_c.Correct -> 
      pr2 ("seems correct (comparing to " ^ expected_res ^ ")");
      if doexit then exit 0;
    | Compare_c.Incorrect s -> 
        pr2 ("seems incorrect: " ^ s);
        pr2 "diff (result(-) vs expected_result(+)) = ";
        diffxs +> List.iter pr2;
        if doexit then exit (-1);
    | Compare_c.IncorrectOnlyInNotParsedCorrectly -> 
        pr2 "seems incorrect, but only because of code that was not parsable";
        if doexit then exit (-1);
    )
  else failwith ("no such .res file: " ^ expected_res)

(*****************************************************************************)
let testone x = 
  let base = if x =~ "\\(.*\\)_ver[0-9]+.*" then matched1 x else x in
  let x'   = if x =~ "\\(.*\\)_ver0" then matched1 x else x in
  let x' = 
    if not(lfile_exists ("tests/" ^ x' ^ ".c"))
    then
      let candidates = 
        readdir_to_file_list "tests/" 
          +> filter (fun s -> s =~ (x' ^ "_.*\\.c$"))
          +> List.map (Str.global_replace (Str.regexp "\\.c$") "")
      in
      if (List.length candidates <> 1)
      then failwith ("cannot find a good match file for: " ^ x);
      List.hd candidates

    else x'
  in
  let cfile      = "tests/" ^ x'   ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in

  if !iso_file <> "" && not (!iso_file =~ ".*\\.iso")
  then pr2 "warning: seems not a .iso file";
  let iso_file = if !iso_file = "" then Some "standard.iso" else Some !iso_file
  in

  begin
    Cocci.full_engine cfile (Left (cocci_file, iso_file));

    let expected_res = "tests/" ^ x ^ ".res" in
    let generated_file = (mktmp "/tmp/output.c") in
    if !compare_with_expected then 
      print_diff_expected_res_and_exit generated_file expected_res true;
  end
          

(******************************************************************************)
let testall () =

  let _total = ref 0 in
  let _good  = ref 0 in

  let expected_result_files = 
    readdir_to_file_list "tests/" +> filter (fun s -> 
      s =~ ".*\\.res$" && filesize ("tests/" ^ s) > 0
    ) +> sort compare
  in

  let diagnose = ref [] in
  let add_diagnose s = push2 s diagnose in

  begin
   expected_result_files +> List.iter (fun expected_res -> 
    let fullbase = 
      if expected_res =~ "\\(.*\\).res" 
      then matched1 expected_res
      else raise Impossible
    in
    let base   = 
      if fullbase =~ "\\(.*\\)_ver[0-9]+" 
      then matched1 fullbase 
      else fullbase
    in
    
    let cfile      = "tests/" ^ fullbase ^ ".c" in
    let cocci_file = "tests/" ^ base ^ ".cocci" in

    if !iso_file <> "" && not (!iso_file =~ ".*\\.iso")
    then pr2 "warning: seems not a .iso file";
    let iso_file = Some (if !iso_file = "" then "standard.iso" else !iso_file) 
    in

    add_diagnose (sprintf "%s:\t" fullbase);
    incr _total;

    let timeout_value = 3 in
    (* benchmarking *)
    let timeout_value = 30 in
    (* end benchmarking *)

    try (
      Common.timeout_function timeout_value (fun () -> 
        
        Cocci.full_engine cfile (Left (cocci_file, iso_file));

        let (correct, diffxs) = 
          Compare_c.compare 
            (Cocci.cprogram_from_file (mktmp "/tmp/output.c"))
            (Cocci.cprogram_from_file ("tests/" ^ expected_res))
        in
	(* benchmarking *)
	Printf.printf "%s\n" expected_res;
	Ctlcocci_integration.print_bench();
	(* end benchmarking *)
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


(******************************************************************************)
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

      "-error_words_only", Arg.Set Flag.process_only_when_error_words, " ";
      
      "-show_c"                 , Arg.Set Flag.show_c,           " ";
      "-show_cocci"             , Arg.Set Flag.show_cocci,       " ";
      "-show_flow"              , Arg.Set Flag.show_flow,        " ";
      "-show_before_fixed_flow" , Arg.Set Flag.show_before_fixed_flow,  " ";
      "-show_ctl_tex"           , Arg.Set Flag.show_ctl_tex,     " ";
      "-no_show_ctl_text"       , Arg.Clear Flag.show_ctl_text,  " ";
      "-no_show_transinfo"      , Arg.Clear Flag.show_transinfo, " ";
      "-no_show_misc",   Arg.Unit (fun () -> 
        Flag.show_misc := false;
        Flag_engine.show_misc := false;
        ), " ";

      (* works in conjunction with -show_ctl* *)
      "-inline_let_ctl", Arg.Set Flag.inline_let_ctl, " ";
      "-show_mcodekind_in_ctl", Arg.Set Flag.show_mcodekind_in_ctl, " ";

      "-no_parse_error_msg", Arg.Clear Flag_parsing_c.verbose_parsing, " ";
      "-verbose_ctl_engine",   Arg.Set Flag_ctl.verbose_ctl_engine, " ";
      "-verbose_engine",       Arg.Set Flag_engine.debug_engine,    " ";


      "-debug_cpp", Arg.Set Flag_parsing_c.debug_cpp, " ";
      "-debug_lexer",        Arg.Set        Flag_parsing_c.debug_lexer , " ";
      "-debug_etdt",         Arg.Set        Flag_parsing_c.debug_etdt , "  ";
      "-debug_cfg",          Arg.Set        Flag_parsing_c.debug_cfg , "  ";
      

      "-loop",                 Arg.Set Flag_ctl.loop_in_src_code,    " ";
      "-l1",     Arg.Clear Flag_parsing_c.label_strategy_2, " ";

      "-sgrepmode", Arg.Set Flag_engine.sgrep_mode, " ";

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

    (match (!args) with

    | [x] when !test_mode    -> testone x 
    | []  when !testall_mode -> testall ()
    | [x] when !test_ctl_foo -> 
        let cfile = x in 
        Cocci.full_engine cfile (Right (Test.foo_ctl ()));

    | x::xs when !action <> "" -> 
        (match !action, x::xs with
        | "tokens_c", [file] -> 
            Flag_parsing_c.debug_lexer := true; 
            Flag_parsing_c.verbose_parsing := true;
            Parse_c.tokens file +> List.iter (fun x -> pr2 (Dumper.dump x))
        | "parse_c", x::xs -> 
            let fullxs = 
              if !dir
              then (assert (xs = []); 
                    process_output_to_list ("find " ^ x ^" -name \"*.c\"")) 
              else x::xs 
            in
            
            let _stat_list = ref [] in

            fullxs +> List.iter (fun file -> 
              pr2 ("HANDLING: " ^ file);

              if not (file =~ ".*\\.c") 
              then pr2 "warning: seems not a .c file";

              file +> Parse_c.parse_print_error_heuristic +> (fun (x, stat) -> 
                push2 stat _stat_list)
                ;
             (* file +> Parse_c.parse_print_error +> (fun (x) -> ()); *)
             );
            if not (null !_stat_list) 
            then Parse_c.print_parsing_stat_list !_stat_list;
        | "parse_cocci", [file] -> 
            if not (file =~ ".*\\.cocci") 
            then pr2 "warning: seems not a .cocci file";
            (try 
              let (xs,_,_) = Cocci.sp_from_file file None in
              xs +> List.iter (fun rule -> 
                Pretty_print_cocci.unparse rule
                              )
            with x -> pr2 "BAD" 
            )
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
                        Flow_to_ast.test def
                      with 
                      | Ast_to_flow.DeadCode None      -> pr2 "deadcode detected, but cant trace back the place"
                      | Ast_to_flow.DeadCode Some info -> pr2 ("deadcode detected: " ^ (error_message file ("", info.charpos) ))
                        
                      )
                    
                  | _ -> ()
                 );
                 )
        | s, [] -> Arg.usage options usage_msg; failwith "too few arguments"
        | _ -> failwith "no action for this"
        )
        
        
    | x::xs -> 

        if (!cocci_file = "") 
        then failwith "I need a cocci file,  use -cocci_file <filename>";

        if not (!cocci_file =~ ".*\\.cocci") 
        then pr2 "warning: seems not a .cocci file";

        if !iso_file <> "" && not (!iso_file =~ ".*\\.iso") 
        then pr2 "warning: seems not a .iso file";

        let cocci_file = !cocci_file in

        let iso_file = 
          (* todo: try to go back the parent dir recursively 
           * to find a standard.iso 
           *)
          if !iso_file = "" 
          then None 
          else Some !iso_file
        in

        let fullxs = 
          if !dir 
          then begin
            assert (xs = []); 
            process_output_to_list ("find " ^ x ^ " -name \"*.c\"")
          end 
          else x::xs 
        in

        fullxs +> List.iter (fun cfile -> 
          (try Cocci.full_engine cfile (Left (cocci_file, iso_file))
          with
            e -> 
              if !dir 
              then pr2 ("EXN:" ^ Printexc.to_string e)
              else raise e
          );

          let expected_res = 
            Str.global_replace (Str.regexp "\\.c$") ".res" cfile 
          in
          let generated_file = (mktmp "/tmp/output.c") in

	  Ctlcocci_integration.print_bench();

	  if !save_output_file
	  then command2 ("cp /tmp/output.c "^cfile^".cocci_res");

          if !compare_with_expected then 
            print_diff_expected_res_and_exit generated_file expected_res 
              (if List.length fullxs = 1 then true else false)
          );

    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
   (* pr2 (profiling_diagnostic ()); *)
   )
  end


let _ =
  if not (!Sys.interactive) then (main ();Ctlcocci_integration.print_bench())
