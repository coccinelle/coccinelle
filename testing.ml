open Common open Commonop 

(*****************************************************************************)
(* Test framework *)
(*****************************************************************************)

(* There can have multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and some optional
 * base_vernn.[c,res].
 * 
 * Note that I use standard.iso when, iso_file parameter is empty.
 * If want to test without iso, use -iso_file empty.iso option.
 *)
let testone x iso_file compare_with_expected_flag = 
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in

  let cfile      = "tests/" ^ x ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in
  let iso_file = Some (if iso_file = "" then "standard.iso" else iso_file) in

  let expected_res   = "tests/" ^ x ^ ".res" in
  begin
    let res = Cocci.full_engine (cocci_file, iso_file) [cfile] in
    match List.assoc cfile res with
    | Some outfile -> 
        if compare_with_expected_flag
        then 
          Compare_c.compare_default outfile expected_res 
          +> Compare_c.compare_result_to_string 
          +> pr2
    | None -> pr2 "no modification on the input file"
  end
          

(* ------------------------------------------------------------------------ *)
let best_score_file = "/tmp/score_cocci_best.marshalled"

let timeout_testall = 30

let testall iso_file =

  let newscore  = empty_score () in

  let expected_result_files = 
    Common.readdir_to_file_list "tests/" +> List.filter (fun s -> 
      s =~ ".*\\.res$" && Common.filesize ("tests/" ^ s) > 0
    ) +> List.sort compare
  in

  begin
    expected_result_files +> List.iter (fun res -> 
      let x = if res =~ "\\(.*\\).res" then matched1 res else raise Impossible 
      in
      let base = if x =~ "\\(.*\\)_ver[0-9]+" then matched1 x else x in 
      let cfile      = "tests/" ^ x ^ ".c" in
      let cocci_file = "tests/" ^ base ^ ".cocci" in
      let iso_file = Some (if iso_file = "" then "standard.iso" else iso_file) 
      in
      let expected = "tests/" ^ res in

      try (
        Common.timeout_function timeout_testall  (fun () -> 
          
          let xs = Cocci.full_engine (cocci_file, iso_file) [cfile] in
          let generated = 
            match List.assoc cfile xs with
            | Some generated -> generated
            | None -> cfile
          in

          let (correct, diffxs) = Compare_c.compare_default generated expected
          in

	  pr2 res;
          (* I don't use Compare_c.compare_result_to_string because
           * I want to indent a little more the messages.
           *)
          (match correct with
          | Compare_c.Correct -> Hashtbl.add newscore res Common.Ok;
          | Compare_c.Pb s -> 
              let s = Str.global_replace 
                (Str.regexp "\"/tmp/cocci-output.*\"") "<COCCIOUTPUTFILE>" s
              in

              let s = 
                "INCORRECT:" ^ s ^ "\n" ^ 
                  "    diff (result(<) vs expected_result(>)) = \n" ^
                  (diffxs +> List.map (fun s -> ("    " ^ s ^ "\n")) 
                    +> Common.join ""
                  )
              in
              Hashtbl.add newscore res (Common.Pb s)
          | Compare_c.PbOnlyInNotParsedCorrectly s -> 
              let s = 
                "seems incorrect, but only because of code that " ^
                  "was not parsable" ^ s
              in
              Hashtbl.add newscore res (Common.Pb s)
          )
        )
      )
      with exn -> 
        Common.reset_pr_indent();
        let s = "PROBLEM\n" ^ ("   exn = " ^ Printexc.to_string exn ^ "\n") in
        Hashtbl.add newscore res (Common.Pb s)
    );


    pr2 "--------------------------------";
    pr2 "statistics";
    pr2 "--------------------------------";

    Common.hash_to_list newscore +> List.iter (fun (s, v) -> 
      print_string (Printf.sprintf "%-30s: " s);
      print_string (
        match v with
        | Common.Ok ->  "CORRECT\n" 
        | Common.Pb s -> s
      )
    );
    flush stdout; flush stderr;

    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    Common.regression_testing newscore best_score_file;


    pr2 "--------------------------------";
    pr2 "total score";
    pr2 "--------------------------------";
    let total = Common.hash_to_list newscore +> List.length in
    let good  = Common.hash_to_list newscore +> List.filter 
      (fun (s, v) -> v = Ok) +> List.length 
    in
    
    pr2 (sprintf "good = %d/%d" good total);

  end


(* ------------------------------------------------------------------------ *)

type okfailed = Ok | SpatchOK | Failed

let suffix_of_okfailed = function
  | Ok -> ".ok"
  | SpatchOK -> ".spatch_ok"
  | Failed -> ".failed"

let delete_previous_result_files infile = 
  [Ok;SpatchOK;Failed] +> List.iter (fun kind -> 
    Common.command2 ("rm -f " ^ infile ^ suffix_of_okfailed kind)
  )


let test_okfailed (cocci_file, iso_file) cfiles = 

  let iso_file = if iso_file = "" then None else Some iso_file in

  let tmpfile = Common.new_temp_file "cocci" ".stdout" in
  let final_files = ref [] in (* generate a okfailed per input file *)

  let redirect_in = 
    match cfiles with
    | x::xs -> 
        let (dir, base, ext) = Common.dbe_of_filename x in
        let varfile = Common.filename_of_dbe (dir, base, "var") in
        if ext = "c" && Common.lfile_exists varfile
        then Common.redirect_stdin varfile
        else (fun f -> f ())
    | [] -> failwith "wierd: no files for test_okfailed given"
  in
        
  redirect_in (fun () -> 
  Common.redirect_stdout_stderr tmpfile (fun () -> 
    try (
      Common.timeout_function_opt !Flag.timeout (fun () ->
        
        let outfiles = Cocci.full_engine (cocci_file, iso_file) cfiles in
        
        outfiles +> List.iter (fun (infile, outopt) -> 
          let (dir, base, ext) = Common.dbe_of_filename infile in
          let expected_suffix   = 
            match ext with
            | "c" -> "res"
            | "h" -> "h.res"
            | s -> failwith ("wierd C file, not a .c or .h :" ^ s)
          in
          let expected_res =  
            Common.filename_of_dbe  (dir, base, expected_suffix) in
          let expected_res2 = 
            Common.filename_of_dbe (dir,"corrected_"^ base, expected_suffix) in

          delete_previous_result_files infile;
          
          match outopt, Common.lfile_exists expected_res with
          | None, false -> 
              ()
          | Some outfile, false -> 
              pr2 ("PB: input file " ^ infile ^ " modified but no .res");
              push2 (infile ^ (suffix_of_okfailed Failed)) final_files

          | x, true -> 
              let outfile = 
                match x with 
                | Some outfile -> outfile 
                | None -> infile 
              in
              
              let diff = Compare_c.compare_default outfile expected_res in
              pr2 (Compare_c.compare_result_to_string diff);
              if fst diff = Compare_c.Correct
              then push2 (infile ^ (suffix_of_okfailed Ok)) final_files
              else 
                if Common.lfile_exists expected_res2
                then begin
                  let diff = Compare_c.compare_default outfile expected_res2 in
                  pr2 (Compare_c.compare_result_to_string diff);
                  if fst diff = Compare_c.Correct
                  then push2 (infile ^ (suffix_of_okfailed SpatchOK)) final_files
                  else push2 (infile ^ (suffix_of_okfailed Failed)) final_files
                end
              else push2 (infile ^ (suffix_of_okfailed Failed)) final_files
        )
      );
    )
    with exn -> 
      pr2 ("PROBLEM\n" ^ ("   exn = " ^ Printexc.to_string exn ^ "\n"));
      (* we may miss some file because cfiles is shorter than outfiles.
       * For instance the detected local headers are not in cfiles, so
       * may have less failed. But at least have some failed
       *)
      cfiles +> List.iter (fun infile -> 
        push2 (infile ^ (suffix_of_okfailed Failed)) final_files;
      )
  ));
  !final_files +> List.iter (fun file -> 
    Common.command2 ("cp " ^ tmpfile ^ " " ^ file);
  )

  


let test_regression_okfailed () = 

  let newscore  = Common.empty_score () in
  let oks = 
    Common.cmd_to_list ("find -name \"*.ok\"") 
    ++
    Common.cmd_to_list ("find -name \"*.spatch_ok\"")
  in
  let failed = Common.cmd_to_list ("find -name \"*.failed\"") in

  if null (oks ++ failed) 
  then failwith "no ok/failed file, you certainly did a make clean"
  else begin
    oks +> List.iter (fun s -> 
      Hashtbl.add newscore (Filename.chop_extension s)  Common.Ok
    );
    failed +> List.iter (fun s -> 
      Hashtbl.add newscore (Filename.chop_extension s) (Common.Pb "fail")
    );
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    Common.regression_testing newscore ("score_failed.marshalled")
  end
    





let compare_with_expected outfiles =
  raise Todo
(*
  outfiles +> List.iter (fun (infile, outopt) -> 
    let base = Common.fileprefix infile in
    let expected_res = base ^ ".res" in
            let (correct, diffxs) = 
              Compare_c.compare_default generated_file expected_res 
            in
            pr2 (Compare_c.compare_result_to_string (correct, diffxs));
            if (List.length fullxs = 1)
            then 
*)
  



(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let tmpfile = "/tmp/output.c" 


let test_tokens_c file = 
  if not (file =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.debug_lexer := true; 
  Flag_parsing_c.verbose_parsing := true;

  Parse_c.tokens file +> Common.pr2gen


        

let test_parse_c xs dirmode = 
        
  Flag_parsing_c.debug_cpp := true;
  Flag_parsing_c.debug_typedef := true;

  let fullxs = 
    if dirmode
    then Common.cmd_to_list ("find " ^(join " " xs) ^" -name \"*.c\"")
    else xs 
  in
      
  let stat_list = ref [] in
  let newscore  = Common.empty_score () in

  fullxs +> List.iter (fun file -> 
    if not (file =~ ".*\\.c") 
    then pr2 "warning: seems not a .c file";

    pr2 "";
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_c.parse_print_error_heuristic file in
    xs +> List.iter (fun (ast, (s, toks)) -> 
      Parse_c.print_tokens_commentized toks
    );

    Common.push2 stat stat_list;
    let s = 
      sprintf "bad = %d, timeout = %B" 
        stat.Parse_c.bad stat.Parse_c.have_timeout
    in
    if stat.Parse_c.bad = 0 && not stat.Parse_c.have_timeout
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );
  
  if not (null !stat_list) 
  then Parse_c.print_parsing_stat_list !stat_list;
  
  if dirmode
  then begin 
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" (List.hd xs) in
    Common.regression_testing newscore 
      ("/tmp/score_parsing__" ^ str ^ ".marshalled");
  end
        

let test_parse_cocci file iso_file = 
  if not (file =~ ".*\\.cocci") 
  then pr2 "warning: seems not a .cocci file";

  let iso_file = if iso_file = "" then None else Some iso_file in

  let (xs,_,_) = Parse_cocci.process file iso_file false in
  xs +> List.iter Pretty_print_cocci.unparse



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
      | Some s, Ast_c.Definition (((funcs, _, _, c),_))  -> 
          s = funcs
      | _, _ -> false 
    in
          
    if toprocess
    then 
      (* old: Flow_to_ast.test !Flag.show_flow def *)
      (try 
          let flow = Ast_to_flow.ast_to_control_flow e in
          flow +> do_option (fun flow -> 
            Ast_to_flow.deadcode_detection flow;
            let flow' = 
              if !Flag.show_before_fixed_flow 
              then flow
              else Ctlcocci_integration.fix_flow_ctl flow
            in
            Ograph_extended.print_ograph_mutable flow'  ("/tmp/output.dot")
          )
        with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
      )
  )



let test_parse_unparse infile = 
  if not (infile =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  let (program2, _stat) = Parse_c.parse_print_error_heuristic infile in
  let program2_with_ppmethod = 
    program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
  in
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile)




let test_typeur infile = 
  if not (infile =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.pretty_print_type_info := true;

  let (program2, _stat) =  Parse_c.parse_print_error_heuristic infile in
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
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile)


(* used by generic_makefile now *)
let test_compare_c file1 file2 = 
  let (correct, diffxs) = Compare_c.compare_default file1 file2 in
  let res = Compare_c.compare_result_to_bool correct in
  if res 
  then raise (Common.UnixExit 0)
  else raise (Common.UnixExit (-1))


let test_compare_c_hardcoded () =
  Compare_c.compare_default 
    "parsing_c/tests/compare1.c" 
    "parsing_c/tests/compare2.c" 
    (*
      "parsing_c/tests/equal_modulo1.c" 
      "parsing_c/tests/equal_modulo2.c" 
    *)
  +> Compare_c.compare_result_to_string 
  +> pr2


let test_xxx () = 
  raise Todo
(*
  Format.print_newline();
  Format.printf "@[<v 5>--@,--@,@[<v 5>--@,--@,@]--@,--@,@]";
  Format.print_newline();
  Format.printf "@[<v>(---@[<v>(---@[<v>(---@,)@]@,)@]@,)@]"
*)

