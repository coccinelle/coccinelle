open Common

open Ast_c

let score_path = "/home/pad/c-yacfe/tmp"

let tmpfile = "/tmp/output.c"

module Ast_to_flow = Control_flow_c_build

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

(* Was in main, but using it in test_parsing_c *)
let get_files path =
  if !Flag.c_plus_plus
  then
    (* only C++ files, but contains .h files as that extension is ambiguous *)
    cmd_to_list
      (if !Flag.include_headers
      then
        "find "^ path ^" -name \"*.cpp\" -o -name \"*.cxx\" -o -name \"*.cc\""
        ^"-o name \"*.h\" -o -name \"*.hpp\" -o -name \"*.hxx\""
      else
        "find "^ path ^" -name \"*.cpp\" -o -name \"*.cxx\" -o -name \"*.cc\"")
  else
    (* only .c files and .h files *)
    cmd_to_list (* same as "true, "", _" case *)
      (if !Flag.include_headers
	  (* FIXME : Could we remove xs ?
	     -use_glimpse requires a singleton.
	     This is checked some lines before.
	     then ("find "^(join " " (x::xs))^" -name \"*.[ch]\"")
	     else ("find "^(join " " (x::xs))^" -name \"*.c\"")
	  *)
      then ("find "^ path ^" -name \"*.[ch]\"")
      else ("find "^ path ^" -name \"*.c\""))

let new_test_parse_gen xs =

  Flag_parsing_c.debug_typedef := true;
  Flag_parsing_c.debug_cpp := true;
  Flag_parsing_c.debug_etdt := false;
  Flag_parsing_c.filter_msg := true;

  (*let dirname_opt =
    match xs with
    | [x] when is_directory x -> Some x
    | _ -> None
  in*)

  (* old:
     let xs = if !Flag.dir then
     process_output_to_list ("find " ^ x ^" -name \"*.c\"") else x::xs in
  *)
  let fullxs = xs +> List.map get_files +> List.concat in

  let stat_list = ref [] in
  let newscore  = Common.empty_score () in

  Common.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file ->

    pr2 "";
    pr2 ("PARSING: " ^ file);

    (* test parsing of format strings as well *)
    let (xs, stat) = Parse_c.parse_c_and_cpp true false file in
    xs +> List.iter (fun (ast, (s, toks)) ->
      Parse_c.print_commentized toks
    );

    Common.push2 stat stat_list;
    let s =
      Printf.sprintf "bad = %d, timeout = %B"
        stat.Parsing_stat.bad stat.Parsing_stat.have_timeout
    in
    if stat.Parsing_stat.bad = 0 && not stat.Parsing_stat.have_timeout
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );

(* uses an explicit path; to fix
  dirname_opt +> Common.do_option (fun dirname ->
    pr2_xxxxxxxxxxxxxxxxx();
    pr2 "regression testing  information";
    pr2_xxxxxxxxxxxxxxxxx();
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    let def = if !Flag_parsing_c.filter_define_error then "_def_" else "" in
    let ext = if ext = "c" then "" else ext in
    let filename = "score_parsing__" ^str ^ def ^ ext ^ ".marshalled" in
    if Sys.file_exists filename
    then
      Common.regression_testing newscore
	(Filename.concat score_path
	   ("score_parsing__" ^str ^ def ^ ext ^ ".marshalled"))
  );
*)

  if !stat_list <> []
  then begin
    Parsing_stat.print_recurring_problematic_tokens !stat_list;
    Parsing_stat.print_parsing_stat_list !stat_list;
  end;
  ()

(* ---------------------------------------------------------------------- *)
let test_parse_gen xs ext =

  Flag_parsing_c.debug_typedef := true;
  Flag_parsing_c.debug_cpp := true;
  Flag_parsing_c.debug_etdt := false;
  Flag_parsing_c.filter_msg := true;

  (*let dirname_opt =
    match xs with
    | [x] when is_directory x -> Some x
    | _ -> None
  in*)

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

    (* test parsing of format strings as well *)
    let (xs, stat) = Parse_c.parse_c_and_cpp true false file in
    xs +> List.iter (fun (ast, (s, toks)) ->
      Parse_c.print_commentized toks
    );

    Common.push2 stat stat_list;
    let s =
      Printf.sprintf "bad = %d, timeout = %B"
        stat.Parsing_stat.bad stat.Parsing_stat.have_timeout
    in
    if stat.Parsing_stat.bad = 0 && not stat.Parsing_stat.have_timeout
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );

(* uses an explicit path; to fix
  dirname_opt +> Common.do_option (fun dirname ->
    pr2_xxxxxxxxxxxxxxxxx();
    pr2 "regression testing  information";
    pr2_xxxxxxxxxxxxxxxxx();
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    let def = if !Flag_parsing_c.filter_define_error then "_def_" else "" in
    let ext = if ext = "c" then "" else ext in
    let filename = "score_parsing__" ^str ^ def ^ ext ^ ".marshalled" in
    if Sys.file_exists filename
    then
      Common.regression_testing newscore
	(Filename.concat score_path
	   ("score_parsing__" ^str ^ def ^ ext ^ ".marshalled"))
  );
*)

  if !stat_list <> []
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

(* could use a simpler parser than heavy parse_c_and_cpp here as there
 * is no more cpp stuff in the .i files
 *)
let test_parse_i xs =
  test_parse_gen xs "i"







(* ---------------------------------------------------------------------- *)
(* file can be   "foo.c"  or "foo.c:main" *)
(* local function that is parametrized by whether to launch gv *)
let local_test_cfg launchgv file =
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

  (* no point to parse format strings *)
  let (program, _stat) = Parse_c.parse_c_and_cpp false false file in

  program +> List.iter (fun (e,_) ->
    let toprocess =
      match specific_func, e with
      | None, Ast_c.Definition (defbis,_) ->
	  Some (Ast_c.str_of_name (defbis.Ast_c.f_name))
      | Some s, Ast_c.Definition (defbis,_)  ->
	  let nm = Ast_c.str_of_name (defbis.Ast_c.f_name) in
          if s = nm then Some nm else None
      | _, _ -> None
    in

    match toprocess with
      None -> ()
    | Some fn -> (* old: Flow_to_ast.test !Flag.show_flow def *)
	try
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
	    let filename =
	      if launchgv
	      then Filename.temp_file "output" ".dot"
	      else
		let fl = Filename.chop_extension (Filename.basename file) in
		fl^":"^fn^".dot" in
            Control_flow_c.G.print_ograph_mutable flow' (filename) launchgv
          )
        with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
      )

let test_cfg = local_test_cfg true



let test_cfg_ifdef file =
  Flag_parsing_c.ifdef_to_if := true;
  (* no point to parse format strings *)
  let (ast2, _stat) = Parse_c.parse_c_and_cpp false false file in
  let ast = Parse_c.program_of_program2 ast2 in

  ast +> List.iter (fun e ->
    (try
        let flow = Ast_to_flow.ast_to_control_flow e in
        flow +> do_option (fun flow ->
          Ast_to_flow.deadcode_detection flow;
          let flow = Ast_to_flow.annotate_loop_nodes flow in
          Control_flow_c.G.print_ograph_mutable flow ("/tmp/output.dot") true
        )
      with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
    )
  )

(* ---------------------------------------------------------------------- *)
let test_parse_unparse infile =
  if not (infile =~ ".*\\.c")
  then pr2 "warning: seems not a .c file";

  (* test parsing of format strings *)
  let (program2, _stat) = Parse_c.parse_c_and_cpp true false infile in
  let program2_with_ppmethod =
    program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
  in
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile);
  (* if want see diff of space => no -b -B *)
  Common.command2 (spf "diff -u -p  %s %s" infile tmpfile);
  (* +> Transformation.test_simple_trans1;*)
  ()


(*
let parse_and_print_sexp file =
  let (ast2,_stat) = Parse_c.parse_c_and_cpp file in
  let ast = Parse_c.program_of_program2 ast2 in
  let _ast =
    Type_annoter_c.annotate_program !Type_annoter_c.initial_env ast
  in

  (*
  let sexp = Sexp_ast_c.sexp_of_program ast in
  let s = Sexp.to_string_hum sexp in
  *)
  Sexp_ast_c.show_info := false;
  let s = Sexp_ast_c.string_of_program ast in
  pr2 s;
  ()
*)


let test_type_c infile =
  if not (infile =~ ".*\\.c")
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.pretty_print_type_info := true;

  (* no point to parse format strings *)
  let (program2, _stat) =  Parse_c.parse_c_and_cpp false false infile in
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
  (* no point to parse format strings *)
  let (program2, _stat) =  Parse_c.parse_c_and_cpp false false infile in
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
  (* no point to parse format strings *)
  let (ast2, _stat) = Parse_c.parse_c_and_cpp false false file in
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
] @
  Cpp_ast_c.cpp_option_of_cmdline
  (!Flag_parsing_c.cpp_i_opts,!Flag_parsing_c.cpp_d_opts)

let test_cpp file =
  Flag_parsing_c.ifdef_to_if := true;
  (* no point to parse format strings *)
  let (ast2, _stat) = Parse_c.parse_c_and_cpp false false file in
  let dirname = Filename.dirname file in
  let ast = Parse_c.program_of_program2 ast2 in
  let _ast = Cpp_ast_c.cpp_expand_include (cpp_options()) dirname ast in


  ()



(* CONFIG [ch] ? also do for .c ? maybe less needed now that I
 * add local_macros.
 *)
let extract_macros ~selection dir =
  let ext = "h" in
  let fullxs = Common.files_of_dir_or_files_no_vcs ext [dir] in
  let macros_and_filename =
    fullxs +> List.map (fun file ->
      pr2 (spf "processing: %s" file);
      let xs = Parse_c.extract_macros file in
      file, xs
    )
  in

  let macros =
    if selection
    then Cpp_analysis_c.extract_dangerous_macros macros_and_filename
    else macros_and_filename
  in
  macros +> List.iter (fun (file, defs) ->
    pr ("/* PARSING: " ^ file ^ " */");
    defs +> List.iter (fun (s, def) ->
      let str = Cpp_token_c.string_of_define_def def in
      pr str;
    )
  );
  ()


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

    let macros_and_filename =
      fullxs +> List.map (fun file ->
        pr2 (spf "processing: %s" file);
        let xs = Parse_c.extract_macros file in
        file, xs
      )
    in
    let macros =
      Cpp_analysis_c.extract_dangerous_macros macros_and_filename
    in
    macros +> List.iter (fun (file, xs) ->
      xs +> List.iter (fun (x, def) ->
        let (s, params, body) = def in
        let str = Cpp_token_c.string_of_define_def def in
        pr str;
        (* builtins ? *)
        Hashtbl.replace !Parse_c._defs_builtins s (s, params, body);
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

    (* test parsing of format strings *)
    let (xs, stat) = Parse_c.parse_c_and_cpp true false file in
    xs +> List.iter (fun (ast, (s, toks)) ->
      Parse_c.print_commentized toks
    );

    Common.push2 stat stat_list;
  );

  if !stat_list <> []
  then begin
    Parsing_stat.print_recurring_problematic_tokens !stat_list;
    Parsing_stat.print_parsing_stat_list !stat_list;
  end;
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
  "--tokens-c", "   <file>",
  Common.mk_action_1_arg test_tokens_c;
  "--parse-c", "   <file or dir>",
  Common.mk_action_n_arg test_parse_c;
  "--parse-h", "   <file or dir>",
  Common.mk_action_n_arg test_parse_h;
  "--parse-ch", "   <file or dir>",
  Common.mk_action_n_arg test_parse_ch;
  "--parse-i", "   <file or dir>",
  Common.mk_action_n_arg test_parse_i;
  "--parse-c++", "   <file or dir>",
  Common.mk_action_n_arg new_test_parse_gen;

  "--parse", "   <file or dir>",
  Common.mk_action_n_arg test_parse;

  "--show-flow", "   <file or file:function>",
  Common.mk_action_1_arg (local_test_cfg true);
  "--control-flow", "   <file or file:function>",
  Common.mk_action_1_arg (local_test_cfg true);
  "--control-flow-to-file", "   <file or file:function>",
  Common.mk_action_1_arg (local_test_cfg false);
  "--test-cfg-ifdef", " <file>",
  Common.mk_action_1_arg test_cfg_ifdef;
  "--parse-unparse", "   <file>",
  Common.mk_action_1_arg test_parse_unparse;
(*  "--parse-and-print-sexp", "   <file>",
    Common.mk_action_1_arg parse_and_print_sexp;*)
  "--type-c", "   <file>",
  Common.mk_action_1_arg test_type_c;
  "--compare-c", "   <file1> <file2>",
  Common.mk_action_2_arg test_compare_c (* result is in unix code *);
  "--comment-annotater-c", "   <file>",
  Common.mk_action_1_arg test_comment_annotater;

  "--compare-c-hardcoded", "  ",
  Common.mk_action_0_arg test_compare_c_hardcoded;

  "--test-attributes", " <file>",
  Common.mk_action_1_arg test_attributes;
  "--test-cpp", " <file>",
  Common.mk_action_1_arg test_cpp;

  "--extract-macros", " <file or dir>",
  Common.mk_action_1_arg (extract_macros ~selection:false) ;

  "--extract-macros-select", " <file or dir>",
  Common.mk_action_1_arg (extract_macros ~selection:true);


  "--xxx", "   <file1> <>",
  Common.mk_action_n_arg test_xxx;
]
