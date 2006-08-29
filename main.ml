open Common open Commonop

(*****************************************************************************)
let dir = ref false

let cocci_file = ref ""
let iso_file   = ref ""

let test_mode = ref false
let test_ctl_foo = ref false
let testall_mode = ref false

let compare_with_expected = ref false

(*****************************************************************************)
let testone x = 
  let base = if x =~ "\\(.*\\)_ver[0-9]+.*" then matched1 x else x in
  let x'   = if x =~ "\\(.*\\)_ver0" then matched1 x else x in
  let x' = 
    if not(lfile_exists ("tests/" ^ x' ^ ".c"))
    then
      let candidates = 
        readdir_to_file_list "tests/" 
          +> filter (fun s -> s =~ (x' ^ "_.*"))
          +> List.map (Str.global_replace (Str.regexp "\\.c$") "")
      in
      assert (List.length candidates = 1);
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
    if Common.lfile_exists expected_res && !compare_with_expected
    then 
      let (c1, _) = Cocci.cprogram_from_file "/tmp/output.c" in
      let (c2, _) = Cocci.cprogram_from_file expected_res in
      let c1' = Abstract_line_c.al_program (c1 +> List.map fst) in
      let c2' = Abstract_line_c.al_program (c2 +> List.map fst) in

      let xs = process_output_to_list ("diff -u -b -B " ^ "/tmp/output.c" ^ 
                                       " "  ^ expected_res) 
      in

      if null xs || c1' =*= c2'
      then pr2 ("seems correct (comparing to " ^ expected_res ^ ")")
      else 
        begin
          pr2 "seems incorrect";
          pr2 "diff (result(-) vs expected_result(+)) = ";
          xs +> List.iter pr2;
        end
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

    try (
      Common.timeout_function timeout_value (fun () -> 
        
        Cocci.full_engine cfile (Left (cocci_file, iso_file));

        let (c1, _) = Cocci.cprogram_from_file "/tmp/output.c" in
        let (c2, _) = Cocci.cprogram_from_file ("tests/" ^ expected_res) in
        let c1' = Abstract_line_c.al_program (c1 +> List.map fst) in
        let c2' = Abstract_line_c.al_program (c2 +> List.map fst) in

        let xs = 
          process_output_to_list ("diff -u -b -B " ^ "/tmp/output.c" ^
                                  " "  ^ "tests/" ^ expected_res) 
        in
              
        if null xs || c1' =*= c2'
        then 
          begin 
            incr _good; 
            add_diagnose "CORRECT\n" 
          end
        else 
          begin
            add_diagnose "INCORRECT\n";
            add_diagnose "    diff (result(<) vs expected_result(>)) = \n";
            xs +> List.iter (fun s -> add_diagnose ("    " ^ s ^ "\n"));
          end;
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
      "-dir", Arg.Set dir, 
        " <dirname> process all files in directory recursively";

      "-cocci_file", Arg.Set_string cocci_file, 
        " <filename> the semantic patch file";

      "-iso_file",   Arg.Set_string iso_file, 
        " <filename> the iso file";

      "-error_words_only", Arg.Set Flag.process_only_when_error_words, 
        " ";


      "-test", Arg.Set test_mode, 
         " automatically find the corresponding c and cocci file";
      "-testall", Arg.Set testall_mode, 
         " ";
      "-compare_with_expected", Arg.Set compare_with_expected, 
         " "; 
      "-test_ctl_foo", Arg.Set test_ctl_foo, 
         " test the engine with the foo ctl in test.ml";


      "-show_flow", Arg.Set Flag.show_flow,  
         " ";
      "-show_before_fixed_flow", Arg.Set Flag.show_before_fixed_flow,  
         " .";
      "-show_ctl",  Arg.Set Flag.show_ctl,    
         " ";
        (* works in conjunction with -show_ctl *)
        "-inline_let_ctl", Arg.Set Flag.inline_let_ctl, " ";
        "-show_mcodekind_in_ctl", Arg.Set Flag.show_mcodekind_in_ctl, " ";

      "-verbose_ctl_engine",   Arg.Set Flag_ctl.verbose_ctl_engine, " ";
      "-verbose_engine",       Arg.Set Flag_engine.debug_engine,    " ";
      "-loop",                 Arg.Set Flag_ctl.loop_in_src_code,    " ";

      "-label_strategy_2",     Arg.Set Flag_parsing_c.label_strategy_2, " ";

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
        
    | x::xs -> 

        if (!cocci_file = "") 
        then failwith "I need a cocci file,  use -cocci_file <filename>";

        if not (!cocci_file =~ ".*\\.cocci") 
        then pr2 "warning: seems not a .cocci file";

        if !iso_file <> "" && not (!iso_file =~ ".*\\.iso") 
        then pr2 "warning: seems not a .iso file";

        let cocci_file = !cocci_file in
        let iso_file = (if !iso_file = "" then None else Some !iso_file) in

        let fullxs = 
          if !dir 
          then begin
            assert (xs = []); 
            process_output_to_list ("find " ^ x ^ " -name \"*.c\"")
          end 
          else x::xs 
        in

        fullxs +> List.iter (fun cfile -> 
          Cocci.full_engine (*~print_input_file:(not !dir)*) 
            cfile (Left (cocci_file, iso_file))
            );

    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
   )
  end


let _ = if not (!Sys.interactive) then main ()
