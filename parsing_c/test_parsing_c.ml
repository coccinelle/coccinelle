open Common

let score_path = "/home/pad/c-yacfe/tmp"

let tmpfile = "/tmp/output.c" 

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_c file = 
  if not (file =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.debug_lexer := true; 
  Flag_parsing_c.verbose_lexing := true;
  Flag_parsing_c.verbose_parsing := true;

  Parse_c.tokens file +> List.iter (fun x -> pr2_gen x);
  ()
        


(* ---------------------------------------------------------------------- *)
let test_parse_gen xs ext = 
        
  Flag_parsing_c.debug_typedef := true;
  Flag_parsing_c.debug_cpp := true;
  Flag_parsing_c.debug_etdt := false;
  Flag_parsing_c.filter_msg := true;

  let dirname_opt = 
    match xs with
    | [x] when is_directory x -> Some x
    | _ -> None
  in

  (* old:
     let xs = if !Flag.dir then 
     process_output_to_list ("find " ^ x ^" -name \"*.c\"") else x::xs in
  *)
  let fullxs = Common.files_of_dir_or_files_no_vcs ext xs in

  let stat_list = ref [] in
  let newscore  = Common.empty_score () in

  Common.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file -> 
    if not (file =~ (".*\\."^ext))
    then pr2 ("warning: seems not a ."^ext^" file");


    pr2 "";
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_c.parse_print_error_heuristic file in
    xs +> List.iter (fun (ast, (s, toks)) -> 
      Parse_c.print_commentized toks
    );

    Common.push2 stat stat_list;
    let s = 
      sprintf "bad = %d, timeout = %B" 
        stat.Parsing_stat.bad stat.Parsing_stat.have_timeout
    in
    if stat.Parsing_stat.bad = 0 && not stat.Parsing_stat.have_timeout
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );
  
  if not (null !stat_list) 
  then Parsing_stat.print_parsing_stat_list !stat_list;
  
  dirname_opt +> Common.do_option (fun dirname -> 
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    let def = if !Flag_parsing_c.filter_define_error then "_def_" else "" in
    let ext = if ext = "c" then "" else ext in
    Common.regression_testing newscore 
      (Filename.concat score_path
       ("score_parsing__" ^str ^ def ^ ext ^ ".marshalled"))
  )


let test_parse_c xs = 
  test_parse_gen xs "c"
let test_parse_h xs = 
  test_parse_gen xs "h"
let test_parse_ch xs = 
  test_parse_gen xs "[ch]"












(* ---------------------------------------------------------------------- *)
(* file can be   "foo.c"  or "foo.c:main" *)
let test_cfg file = 
  let (file, specific_func) = 
    if file =~ "\\(.*\\.c\\):\\(.*\\)"
    then 
      let (a,b) = matched2 file in 
      a, Some b
    else 
      file, None
  in

  if not (file =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  let (program, _stat) = Parse_c.parse_print_error_heuristic file in

  program +> List.iter (fun (e,_) -> 
    let toprocess = 
      match specific_func, e with
      | None, _ -> true
      | Some s, Ast_c.Definition (defbis,_)  -> 
          s = defbis.Ast_c.f_name
      | _, _ -> false 
    in
          
    if toprocess
    then 
      (* old: Flow_to_ast.test !Flag.show_flow def *)
      (try 
          let flow = Ast_to_flow.ast_to_control_flow e in
          flow +> do_option (fun flow -> 
            Ast_to_flow.deadcode_detection flow;
            let flow = Ast_to_flow.annotate_loop_nodes flow in

            let flow' = 
(*
              if !Flag_cocci.show_before_fixed_flow 
              then flow
              else Ctlcocci_integration.fix_flow_ctl flow
*)
              flow
            in
            Ograph_extended.print_ograph_mutable flow' ("/tmp/output.dot") true
          )
        with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
      )
  )



(* ---------------------------------------------------------------------- *)
let test_parse_unparse infile = 
  if not (infile =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  let (program2, _stat) = Parse_c.parse_print_error_heuristic infile in
  let program2_with_ppmethod = 
    program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
  in
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile);
  (* if want see diff of space => no -b -B *)
  Common.command2 (spf "diff -u -p  %s %s" infile tmpfile);
  (* +> Transformation.test_simple_trans1;*)
  ()




let test_type_c infile = 
  if not (infile =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.pretty_print_type_info := true;

  let (program2, _stat) =  Parse_c.parse_print_error_heuristic infile in
  let _program2 =
    program2 
    +> Common.unzip 
    +> (fun (program, infos) -> 
      Type_annoter_c.annotate_program Type_annoter_c.initial_env true
        program +> List.map fst,
      infos
    )
    +> Common.uncurry Common.zip
  in
  let program2_with_ppmethod = 
    program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
  in
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile);
  ();;


(* ---------------------------------------------------------------------- *)
(* used by generic_makefile now *)
let test_compare_c file1 file2 = 
  let (correct, diffxs) = Compare_c.compare_default file1 file2 in
  let res = Compare_c.compare_result_to_bool correct in
  if res 
  then raise (Common.UnixExit 0)
  else raise (Common.UnixExit (-1))


let test_compare_c_hardcoded () =
  Compare_c.compare_default 
    "tests/compare1.c" 
    "tests/compare2.c" 
    (*
      "tests/equal_modulo1.c" 
      "tests/equal_modulo2.c" 
    *)
  +> Compare_c.compare_result_to_string 
  +> pr2



(* ---------------------------------------------------------------------- *)
let test_xxx a  = 
  ()

(*
  ignore(Parse_c.parse_cpp_define_file "standard.h")
  pr2 "pr2";
  pr  "pr"

  Format.print_newline();
  Format.printf "@[<v 5>--@,--@,@[<v 5>--@,--@,@]--@,--@,@]";
  Format.print_newline();
  Format.printf "@[<v>(---@[<v>(---@[<v>(---@,)@]@,)@]@,)@]"
*)



(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_c", "   <file>", 
  Common.mk_action_1_arg test_tokens_c;
  "-parse_c", "   <file or dir>", 
  Common.mk_action_n_arg test_parse_c;
  "-parse_h", "   <file or dir>", 
  Common.mk_action_n_arg test_parse_h;
  "-parse_ch", "   <file or dir>", 
  Common.mk_action_n_arg test_parse_ch;

  "-show_flow", "   <file or file:function>", 
  Common.mk_action_1_arg test_cfg;
  "-control_flow", "   <file or file:function>", 
  Common.mk_action_1_arg test_cfg;
  "-parse_unparse", "   <file>", 
  Common.mk_action_1_arg test_parse_unparse;
  "-type_c", "   <file>", 
  Common.mk_action_1_arg test_type_c;
  "-compare_c", "   <file1> <file2>", 
  Common.mk_action_2_arg test_compare_c (* result is in unix code *);

  "-compare_c_hardcoded", "  ", 
  Common.mk_action_0_arg test_compare_c_hardcoded;

  "-xxx", "   <file1> <>", 
  Common.mk_action_n_arg test_xxx;
]

