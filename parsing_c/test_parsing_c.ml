open Common

open Ast_c

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
    if stat.Parsing_stat.bad =|= 0 && not stat.Parsing_stat.have_timeout
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );
  
  dirname_opt +> Common.do_option (fun dirname -> 
    pr2_xxxxxxxxxxxxxxxxx();
    pr2 "regression testing  information";
    pr2_xxxxxxxxxxxxxxxxx();
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    let def = if !Flag_parsing_c.filter_define_error then "_def_" else "" in
    let ext = if ext =$= "c" then "" else ext in
    Common.regression_testing newscore 
      (Filename.concat score_path
       ("score_parsing__" ^str ^ def ^ ext ^ ".marshalled"))
  );

  if not (null !stat_list) 
  then begin 
    Parsing_stat.print_recurring_problematic_tokens !stat_list;
    Parsing_stat.print_parsing_stat_list !stat_list;
  end;
  ()


let test_parse_c xs = 
  test_parse_gen xs "c"
let test_parse_h xs = 
  test_parse_gen xs "h"
let test_parse_ch xs = 
  test_parse_gen xs "[ch]"


(* ---------------------------------------------------------------------- *)

let test_parse xs = 

  Flag_parsing_c.filter_msg_define_error := true;
  Flag_parsing_c.filter_define_error := true;
  Flag_parsing_c.verbose_lexing := false;
  Flag_parsing_c.verbose_parsing := false;

  let dirname_opt = 
    match xs with
    | [x] when is_directory x -> Some x
    | _ -> None
  in
  dirname_opt +> Common.do_option (fun dir -> 

    let ext = "h" in 
    let fullxs = Common.files_of_dir_or_files_no_vcs ext [dir] in
    fullxs +> List.iter (fun file -> 
      let xs = Parse_c.parse_cpp_define_file file in
      xs +> List.iter (fun (x, def) -> 
        let (s, params, body) = def in 
        Hashtbl.replace !Parse_c._defs s (s, params, body);
      );
    );
  );

  let ext = "[ch]" in

  let fullxs = Common.files_of_dir_or_files_no_vcs ext xs in

  let stat_list = ref [] in
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
  );
  
  if not (null !stat_list) 
  then begin 
    Parsing_stat.print_recurring_problematic_tokens !stat_list;
    Parsing_stat.print_parsing_stat_list !stat_list;
  end;
  ()









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
          s =$= Ast_c.str_of_name (defbis.Ast_c.f_name)
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
	    let filename = Filename.temp_file "output" ".dot" in
            Ograph_extended.print_ograph_mutable flow' (filename) true
          )
        with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
      )
  )



let test_cfg_ifdef file = 
  let (ast2, _stat) = Parse_c.parse_print_error_heuristic file in
  let ast = Parse_c.program_of_program2 ast2 in

  let ast = Cpp_ast_c.cpp_ifdef_statementize ast in

  ast +> List.iter (fun e -> 
    (try 
        let flow = Ast_to_flow.ast_to_control_flow e in
        flow +> do_option (fun flow -> 
          Ast_to_flow.deadcode_detection flow;
          let flow = Ast_to_flow.annotate_loop_nodes flow in
          Ograph_extended.print_ograph_mutable flow ("/tmp/output.dot") true
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
      Type_annoter_c.annotate_program !Type_annoter_c.initial_env 
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
(* ex: demos/platform_ifdef.c *)
let test_comment_annotater infile = 
  let (program2, _stat) =  Parse_c.parse_print_error_heuristic infile in
  let asts = program2 +> List.map (fun (ast,_) -> ast) in
  let toks = program2 +> List.map (fun (ast, (s, toks)) -> toks) +> 
    List.flatten in

  Flag_parsing_c.pretty_print_comment_info := true;

  pr2 "pretty print, before comment annotation: --->";
  Common.adjust_pp_with_indent (fun () -> 
  asts +> List.iter (fun ast -> 
    Pretty_print_c.pp_toplevel_simple ast;
  );
  );

  let _ = Comment_annotater_c.annotate_program toks asts in

  Common.adjust_pp_with_indent (fun () -> 
  pr2 "pretty print, after comment annotation: --->";
  asts +> List.iter (fun ast -> 
    Pretty_print_c.pp_toplevel_simple ast;
  );
  );


  ()
  
  
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
let test_attributes file = 
  let (ast2, _stat) = Parse_c.parse_c_and_cpp file in
  let ast = Parse_c.program_of_program2 ast2 in

  Visitor_c.vk_program { Visitor_c.default_visitor_c with
    Visitor_c.kdef = (fun (k, bigf) (defbis, ii) -> 
      let sattr  = Ast_c.s_of_attr defbis.f_attr in
      pr2 (spf "%-30s: %s" (Ast_c.str_of_name (defbis.f_name)) sattr);
    );
    Visitor_c.kdecl = (fun (k, bigf) decl -> 
      match decl with
      | DeclList (xs, ii) -> 
          xs +> List.iter (fun (onedecl, iicomma) -> 
            
            let sattr  = Ast_c.s_of_attr onedecl.v_attr in
            let idname = 
              match onedecl.v_namei with
              | Some (name, ini) -> Ast_c.str_of_name name
              | None -> "novar"
            in
            pr2 (spf "%-30s: %s" idname sattr);
          );
      | _ -> ()
          
    );
  } ast;
  ()


let cpp_options () = [
  Cpp_ast_c.I "/home/yyzhou/pad/linux/include";
] ++ 
  Cpp_ast_c.cpp_option_of_cmdline 
  (!Flag_parsing_c.cpp_i_opts,!Flag_parsing_c.cpp_d_opts)

let test_cpp file = 
  let (ast2, _stat) = Parse_c.parse_c_and_cpp file in
  let dirname = Filename.dirname file in
  let ast = Parse_c.program_of_program2 ast2 in
  let _ast' = Cpp_ast_c.cpp_expand_include (cpp_options()) dirname ast in
  
  ()



let extract_macros ~selection x = 
  (* CONFIG [ch] ? also do for .c ? maybe less needed now that I 
   * add local_macros.
   *)

  let ext = "h" in 
  let fullxs = Common.files_of_dir_or_files_no_vcs ext [x] in
  fullxs +> List.iter (fun file -> 
   
    pr ("/* PARSING: " ^ file ^ " */");
    let xs = Parse_c.parse_cpp_define_file file in
    xs +> List.iter (fun (x, def) -> 
      let (s, params, body) = def in 
      assert(s = x);
      (match params, body with
      | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [Parser_c.TInt _]
      | Cpp_token_c.NoParam, Cpp_token_c.DefineBody [] -> 
          ()
      | _ -> 

          let s1 = 
            match params with
            | Cpp_token_c.NoParam -> spf "#define %s " s
            | Cpp_token_c.Params xs -> 
                spf "#define %s(%s) "
                  s (Common.join "," xs)
          in
          let s2, bodytoks = 
            match body with
            | Cpp_token_c.DefineHint _ -> 
                failwith "weird, hint in regular header file"
            | Cpp_token_c.DefineBody xs -> 
                Common.join " " (xs +> List.map Token_helpers.str_of_tok),
                xs
          in

          let print = 
            match () with
            | () when s ==~ Parsing_hacks.regexp_annot -> true
            | () when List.exists (function
              | Parser_c.Tattribute _ -> true
              | _ -> false) bodytoks -> true
            | () -> false
          in
          if print || not selection then pr (s1 ^ s2)
      );
    );
  );
  ()



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

  "-parse", "   <file or dir>", 
  Common.mk_action_n_arg test_parse;

  "-show_flow", "   <file or file:function>", 
  Common.mk_action_1_arg test_cfg;
  "-control_flow", "   <file or file:function>", 
  Common.mk_action_1_arg test_cfg;
  "-test_cfg_ifdef", " <file>",
  Common.mk_action_1_arg test_cfg_ifdef;
  "-parse_unparse", "   <file>", 
  Common.mk_action_1_arg test_parse_unparse;
  "-type_c", "   <file>", 
  Common.mk_action_1_arg test_type_c;
  "-compare_c", "   <file1> <file2>", 
  Common.mk_action_2_arg test_compare_c (* result is in unix code *);
  "-comment_annotater_c", "   <file>", 
  Common.mk_action_1_arg test_comment_annotater;

  "-compare_c_hardcoded", "  ", 
  Common.mk_action_0_arg test_compare_c_hardcoded;

  "-test_attributes", " <file>",
  Common.mk_action_1_arg test_attributes;
  "-test_cpp", " <file>",
  Common.mk_action_1_arg test_cpp;

  "-extract_macros", " <file or dir>",
  Common.mk_action_1_arg (extract_macros ~selection:false) ;

  "-extract_macros_select", " <file or dir>",
  Common.mk_action_1_arg (extract_macros ~selection:true);


  "-xxx", "   <file1> <>", 
  Common.mk_action_n_arg test_xxx;
]

