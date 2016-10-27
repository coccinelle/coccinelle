(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

(*****************************************************************************)
(* Test framework *)
(*****************************************************************************)

type redirected_output = {
    out_file: string;
    out_channel: out_channel;
    stdout_backup: Unix.file_descr;
    show_diff_backup: bool;
  }

let out_suffix = ".stdout"

let flush_scripts_output () =
  flush stdout;
  if Pycocci.py_isinitialized () then
    let _ = Pycocci.pyrun_simplestring "\
import sys
sys.stdout.flush()
" in
    ()

let begin_redirect_output expected_out =
  let has_expected_out = Sys.file_exists expected_out in
  if has_expected_out then
    let out_file = Common.new_temp_file "redirect" out_suffix in
    let out_channel = open_out out_file in
    let out_file_descr = Unix.descr_of_out_channel out_channel in
    let stdout_backup = Unix.dup Unix.stdout in
    let show_diff_backup = !Flag_cocci.show_diff in
    flush_scripts_output ();
    Unix.dup2 out_file_descr Unix.stdout;
    Flag_cocci.show_diff := false;
    Some { out_file; out_channel; stdout_backup; show_diff_backup }
  else
    None

let end_redirect_output = Common.map_option (
  fun { out_file; out_channel; stdout_backup; show_diff_backup } ->
    flush_scripts_output ();
    Unix.dup2 stdout_backup Unix.stdout;
    close_out out_channel;
    Flag_cocci.show_diff := show_diff_backup;
    out_file)

let rec test_loop cocci_file cfiles =
  let (cocci_infos,_) = Cocci.pre_engine (cocci_file, !Config.std_iso) in
  let res = Cocci.full_engine cocci_infos cfiles in
  match Iteration.get_pending_instance () with
    None -> (cocci_infos, res)
  | Some (cfiles', virt_rules, virt_ids) ->
      Flag.defined_virtual_rules := virt_rules;
      Flag.defined_virtual_env := virt_ids;
      Common.clear_pr2_once ();
      test_loop cocci_file cfiles'

let test_with_output_redirected cocci_file cfiles expected_out =
  Iteration.initialization_stack := [];
  let redirected_output = begin_redirect_output expected_out in
  let (cocci_infos, res) =
    try test_loop cocci_file cfiles
    with e ->
      ignore (end_redirect_output redirected_output);
      raise e in
  let current_out = end_redirect_output redirected_output in
  Cocci.post_engine cocci_infos;
  (res, current_out)

(* There can be multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and some optional
 * base_vernn.[c,res].
 *
 * If want to test without iso, use -iso_file empty.iso option.
 *)
let testone prefix x compare_with_expected_flag =
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in

  let cfile      = prefix ^ x ^ ".c" in
  let cocci_file = prefix ^ base ^ ".cocci" in

  let expected_out = prefix ^ base ^ out_suffix in

  let expected_res   = prefix ^ x ^ ".res" in
  begin
    let (res, current_out) =
      test_with_output_redirected cocci_file [cfile] expected_out in
    let generated =
      match Common.optionise (fun () -> List.assoc cfile res) with
      | Some (Some outfile) ->
          if List.length res > 1
          then pr2 ("note that not just " ^ cfile ^ " was involved");
          let tmpfile =
	    Printf.sprintf
	      "%s/%s" Config.get_temp_dir_name (Filename.basename cfile) in
          pr2 (Printf.sprintf "One file modified. Result is here: %s" tmpfile);
          Common.command2 ("mv "^outfile^" "^tmpfile);
          tmpfile
      | Some None ->
          pr2 "no modification on the input file";
          cfile
      | None -> raise (Impossible 163)
    in
    if compare_with_expected_flag
    then
      begin
	Compare_c.compare_default generated expected_res
	  +> Compare_c.compare_result_to_string
	  +> pr2;
	match current_out with
	  None -> ()
	| Some current_out' ->
	    Compare_c.exact_compare current_out' expected_out
	      +> Compare_c.compare_result_to_string
	      +> pr2
      end
  end;
  Common.erase_temp_files ()


let add_file_to_score score res correct diffxs =
  (* I don't use Compare_c.compare_result_to_string because
   * I want to indent a little more the messages.
   *)
  match correct with
    Compare_c.Correct -> Hashtbl.add score res Common.Ok;
  | Compare_c.Pb s ->
      let s = Str.global_replace
	  (Str.regexp "\"/tmp/cocci-output.*\"") "<COCCIOUTPUTFILE>" s
      in
      (* on macos the temporary files are stored elsewhere *)
      let s =
	Str.global_replace
	  (Str.regexp "\"/var/folders/.*/cocci-output.*\"")
	  "<COCCIOUTPUTFILE>" s
      in
      let s =
	"INCORRECT:" ^ s ^ "\n" ^
	"    diff (result(<) vs expected_result(>)) = \n" ^
	(diffxs +>
	 List.map(fun s -> "    "^s^"\n") +> String.concat "")
      in
      Hashtbl.add score res (Common.Pb s)
  | Compare_c.PbOnlyInNotParsedCorrectly s ->
      let s =
	"seems incorrect, but only because of code that " ^
	"was not parsable" ^ s
      in
      Hashtbl.add score res (Common.Pb s)

(* ------------------------------------------------------------------------ *)
(* note: if you get some weird results in -testall, and not in -test,
 * it is possible that a test file work in -test but may not
 * work while used inside a -testall. If we have some bugs in our
 * parser that modify some global state and that those states
 * are not reset between each test file, then having run previous
 * test files may have an influence on another test file which mean
 * than a test may work in isolation (via -test) but not otherwise
 * (via -testall). Fortunately such bugs are rare.
 *
 *)
(* If extra test is provided, then all failing tests with the standard
   comparison are considered ok, and only the correct result are subjected to
   the extra test *)
let testall_bis extra_test expected_score_file update_score_file =

  let score  = empty_score () in

  let expected_result_files =
    Common.glob "tests/*.res"
    +> List.filter (fun f -> Common.filesize f > 0)
    +> List.map Filename.basename
    +> List.sort compare
  in

  begin
    expected_result_files +> List.iter (fun res ->
      let x =
        if res =~ "\\(.*\\).res"
	then matched1 res
	else raise (Impossible 164) in
      let base = if x =~ "\\(.*\\)_ver[0-9]+" then matched1 x else x in
      let cfile      = "tests/" ^ x ^ ".c" in
      let cocci_file = "tests/" ^ base ^ ".cocci" in
      let expected = "tests/" ^ res in
      let out = base ^ out_suffix in
      let expected_out = "tests/" ^ out in

      let timeout_testall = 60 in

      try (
        Common.timeout_function "testing" timeout_testall  (fun () ->

	  pr2 res;

	  let (xs, current_out) =
	    test_with_output_redirected cocci_file [cfile] expected_out in

          let generated =
            match List.assoc cfile xs with
            | Some generated -> generated
            | None -> cfile
          in

          let (correct, diffxs) =
	    Compare_c.compare_default generated expected in

	  let (correct, diffxs) =
	    match extra_test with
	      None -> (correct, diffxs)
	    | Some extra_test ->
		(match correct with
		  Compare_c.Correct -> extra_test generated expected
		| _ ->
		    (* if there is an extra test, we don't care about the
		       things that fail on the first test *)
		    (Compare_c.Correct,[])) in

	  add_file_to_score score res correct diffxs;

	  begin
	    match current_out with
	      None -> ()
	    | Some current_out' ->
		let (correct, diffxs) =
		  Compare_c.exact_compare current_out' expected_out in
		add_file_to_score score out correct diffxs
	  end;
	  Common.erase_temp_files ();
        )
      )
      with exn ->
        Common.reset_pr_indent();
        let s = "PROBLEM\n" ^ ("   exn = " ^ Printexc.to_string exn ^ "\n") in
        Hashtbl.add score res (Common.Pb s)
    );


    pr2 "--------------------------------";
    pr2 "statistics";
    pr2 "--------------------------------";

    Common.hash_to_list score +> List.iter (fun (s, v) ->
      pr_no_nl (Printf.sprintf "%-30s: " s);
      pr_no_nl (
        match v with
        | Common.Ok ->  "CORRECT\n"
        | Common.Pb s -> s
      )
    );
    flush stdout; flush stderr;

    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";

    let expected_score_file_orig = "tests/SCORE_expected_orig.sexp" in
    let best_of_both_file = "tests/SCORE_best_of_both.sexp" in
    let actual_score_file = "tests/SCORE_actual.sexp" in

    pr2 ("regression file: "^ expected_score_file);
    let (expected_score : score) =
      if Sys.file_exists expected_score_file
      then
        Common.load_score expected_score_file ()
      else
        if Sys.file_exists expected_score_file_orig
        then begin
          pr2 (spf "use expected orig file (%s)" expected_score_file_orig);
          Common.command2 (spf "cp %s %s" expected_score_file_orig
                                          expected_score_file);
	  Common.load_score expected_score_file ()
        end
       else
          empty_score()
    in

    let new_bestscore = Common.regression_testing_vs score expected_score in

    Common.save_score score actual_score_file;

    Common.save_score new_bestscore best_of_both_file;

    Common.print_total_score score;

    let (good, total)                   = Common.total_scores score in
    let (expected_good, expected_total) = Common.total_scores expected_score in

    if good = expected_good
    then begin
      pr2 "Current score is equal to expected score; everything is fine";
      raise (UnixExit 0);
    end
    else
      if good < expected_good
      then begin
        pr2 "Current score is lower than expected :(";
        pr2 (spf "(was expecting %d but got %d)" expected_good good);
        pr2 "";
        pr2 "If you think it's normal, then maybe you need to update the";
        pr2 (spf "score file %s, copying info from %s."
                expected_score_file actual_score_file);
        raise (UnixExit 1);
      end
      else begin
        pr2 "Current score is greater than expected :)";
        pr2 (spf "(was expecting %d but got %d)" expected_good good);
        if update_score_file then
        begin
          pr2 "Generating new expected score file and saving old one";
          Common.command2_y_or_no_exit_if_no
            (spf "mv %s %s" expected_score_file (expected_score_file ^ ".save"));
          Common.command2_y_or_no_exit_if_no
            (spf "mv %s %s" best_of_both_file expected_score_file);
        end;

        (* when there are sufficient number of tests, abort if a substantial
         * amount of tests fail, which would indicate a broken build.
         *)
        if total > 40 && good < (total * 3) / 4
        then begin
	  pr2 "Still, less 75% the tests passed. Returning a nonzero exist status.";
          raise (UnixExit 1);
        end;

        raise (UnixExit 0);
      end

  end

let testall = testall_bis None
let test_spacing = testall_bis (Some Compare_c.exact_compare)

(* ------------------------------------------------------------------------ *)

type okfailed = Ok | SpatchOK | Failed

(* test_to_string *)
let t_to_s = function
  | Ok -> ".ok"
  | SpatchOK -> ".spatch_ok"
  | Failed -> ".failed"

let delete_previous_result_files infile =
  [Ok;SpatchOK;Failed] +> List.iter (fun kind ->
    Common.remove_file (infile ^ t_to_s kind)
  )

(* quite similar to compare_with_expected  below *)
let test_okfailed cocci_file cfiles =
  cfiles +> List.iter delete_previous_result_files;

  (* final_files contain the name of an output file (a .ok or .failed
   * or .spatch_ok), and also some additional strings to be printed in
   * this output file in addition to the general error message of
   * full_engine. *)
  let final_files = ref [] in


  let newout =
    Common.new_temp_file "cocci" ".stdout"
  in

  let t = Unix.gettimeofday () in
  let time_per_file_str () =
    let t' = Unix.gettimeofday () in
    let tdiff = t' -. t in
    let tperfile = tdiff /. (float_of_int (List.length cfiles)) in
    spf "time: %f" tperfile
  in

  let expected_out = Filename.chop_suffix cocci_file ".cocci" ^ out_suffix in

  Common.redirect_stdout_stderr newout (fun () ->
    try (
      Common.timeout_function_opt "testing" !Flag_cocci.timeout (fun () ->

	let (outfiles, current_out) =
	  test_with_output_redirected cocci_file cfiles expected_out in

        let time_str = time_per_file_str () in

        outfiles +> List.iter (fun (infile, outopt) ->
          let (dir, base, ext) = Common.dbe_of_filename infile in
          let expected_suffix   =
            match ext with
            | "c" -> "res"
            | "h" -> "h.res"
            | s -> pr2 ("WEIRD: not a .c or .h :" ^ base ^ "." ^ s);
                "" (* no extension, will compare to same file *)
          in
          let expected_res =
            Common.filename_of_dbe  (dir, base, expected_suffix) in
          let expected_res2 =
            Common.filename_of_dbe (dir,"corrected_"^ base,expected_suffix)
          in

            (* can delete more than the first delete_previous_result_files
               * because here we can have more files than in cfiles, for instance
               * the header files
            *)
          delete_previous_result_files infile;

          match outopt, Common.lfile_exists expected_res with
          | None, false ->
              ()
          | Some outfile, false ->
              let s =("PB: input file " ^ infile ^ " modified but no .res") in
              push2 (infile^t_to_s Failed, [s;time_str]) final_files

          | x, true ->
              let outfile =
                match x with
                | Some outfile -> outfile
                | None -> infile
              in

              let diff = Compare_c.compare_default outfile expected_res in
              let s1 = (Compare_c.compare_result_to_string diff) in
              if fst diff = Compare_c.Correct
              then push2 (infile ^ (t_to_s Ok), [s1;time_str]) final_files
              else
                if Common.lfile_exists expected_res2
                then begin
                  let diff = Compare_c.compare_default outfile expected_res2 in
                  let s2 = Compare_c.compare_result_to_string diff in
                  if fst diff = Compare_c.Correct
                  then push2 (infile ^ (t_to_s SpatchOK),[s2;s1;time_str])
                      final_files
                  else push2 (infile ^ (t_to_s Failed), [s2;s1;time_str])
                      final_files
                end
		else push2 (infile ^ (t_to_s Failed), [s1;time_str]) final_files
		    );
	begin
	  match current_out with
	    None -> ()
	  | Some current_out' ->
	      let diff = Compare_c.exact_compare current_out' expected_out in
	      let s = Compare_c.compare_result_to_string diff in
	      push2 (cocci_file ^ (t_to_s Failed), [s;time_str]) final_files
	end;
	 );
      )
    with exn ->
      let clean s =
	Str.global_replace (Str.regexp "\\\\n") "\n"
	  (Str.global_replace (Str.regexp ("\\\\\"")) "\""
	     (Str.global_replace (Str.regexp "\\\\t") "\t" s)) in
      let s = "PROBLEM\n"^("   exn = " ^ clean(Printexc.to_string exn) ^ "\n")
      in
      let time_str = time_per_file_str ()
      in
      (* we may miss some file because cfiles is shorter than outfiles.
	 * For instance the detected local headers are not in cfiles, so
	 * may have less failed. But at least have some failed.
      *)
      cfiles +> List.iter (fun infile ->
        push2 (infile ^ (t_to_s Failed), [s;time_str]) final_files;
	);
      );
  !final_files +> List.iter (fun (file, additional_strs) ->
    Common.command2 ("cp " ^ newout ^ " " ^ file);
    with_open_outfile file (fun (pr, chan) ->
      additional_strs +> List.iter (fun s -> pr (s ^ "\n"))
	);

    )


let test_regression_okfailed () =

  (* it's  xxx.c.ok *)
  let chop_ext f = f +> Filename.chop_extension in

  let newscore  = Common.empty_score () in
  let oks =
    Common.cmd_to_list ("find . -name \"*.ok\"")
    @
    Common.cmd_to_list ("find . -name \"*.spatch_ok\"")
  in
  let failed = Common.cmd_to_list ("find . -name \"*.failed\"") in

  if (oks @ failed) = []
  then failwith "no ok/failed file, you certainly did a make clean"
  else begin
    oks +> List.iter (fun s ->
      Hashtbl.add newscore (chop_ext s)  Common.Ok
    );
    failed +> List.iter (fun s ->
      Hashtbl.add newscore (chop_ext s) (Common.Pb "fail")
    );
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    Common.regression_testing newscore ("score_failed.marshalled")
  end


(* ------------------------------------------------------------------------ *)
(* quite similar to test_ok_failed. Maybe could factorize code *)
let compare_with_expected outfiles =
  pr2 "";
  outfiles +> List.iter (fun (infile, outopt) ->
    let (dir, base, ext) = Common.dbe_of_filename infile in
    let expected_suffix   =
      match ext with
      | "c" -> "res"
      | "h" -> "h.res"
      | s -> failwith ("weird C file, not a .c or .h :" ^ s)
    in
    let expected_res =
      Common.filename_of_dbe  (dir, base, expected_suffix) in
    let expected_res2 =
      Common.filename_of_dbe (dir,"corrected_"^ base,expected_suffix)
    in

    match outopt, Common.lfile_exists expected_res with
    | None, false -> ()
    | Some outfile, false ->
        let s =("PB: input file " ^ infile ^ " modified but no .res") in
        pr2 s
    | x, true ->
        let outfile =
          match x with
          | Some outfile -> outfile
          | None -> infile
        in
        let diff = Compare_c.compare_default outfile expected_res in
        let s1 = (Compare_c.compare_result_to_string diff) in
        if fst diff = Compare_c.Correct
        then pr2_no_nl (infile ^ " " ^ s1)
        else
          if Common.lfile_exists expected_res2
          then begin
            let diff = Compare_c.compare_default outfile expected_res2 in
            let s2 = Compare_c.compare_result_to_string diff in
            if fst diff = Compare_c.Correct
            then pr2 (infile ^ " is spatchOK " ^ s2)
            else pr2 (infile ^ " is failed " ^ s2)
          end
        else pr2 (infile ^ " is failed " ^ s1)
  )

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse_cocci file =
  if not (file =~ ".*\\.cocci")
  then pr2 "warning: seems not a .cocci file";

  let (mvs,xs,_,_,_,_,(grep_tokens,query,_,_),_) =
    Parse_cocci.process file (Some !Config.std_iso) false in
  xs +> List.iter2 Pretty_print_cocci.unparse mvs;
  Format.print_newline();
  (* compile ocaml script code *)
  (match Prepare_ocamlcocci.prepare file xs with
    None -> ()
  | Some ocaml_script_file ->
      (* compile file *)
      Prepare_ocamlcocci.load_file ocaml_script_file;
      (* remove file *)
      (if not !Common.save_tmp_files
      then Prepare_ocamlcocci.clean_file ocaml_script_file);
      (* Print the list of registered functions *)
      Prepare_ocamlcocci.test ());
  Printf.printf "grep tokens\n";
  (match grep_tokens with
    None -> pr "No query"
  | Some x -> pr (String.concat " || " x));
  (* could update to accomodate WTCocciGrep *)
  (match query with
    None -> pr "No query"
  | Some x ->
      Printf.printf "glimpse tokens\n";
      pr (String.concat "\nor on glimpse failure\n" x))

let print_link t a b =
  if not (a = b)
  then
    (try Hashtbl.find t (a,b)
    with Not_found ->
      (Hashtbl.add t (a,b) ();
       Printf.printf "  \"%s\" -> \"%s\";\n" b a))

let print_dotted_link dst = function
    "" -> ()
  | src -> Printf.printf "  \"%s\" -> \"%s\" [style = dotted];\n" src dst

let rec depto t from = function
    Ast_cocci.Dep x | Ast_cocci.EverDep x | Ast_cocci.NeverDep x ->
      print_link t from x
  | Ast_cocci.AndDep(x,y) | Ast_cocci.OrDep(x,y) ->
      depto t from x; depto t from y
  | _ -> ()

let test_rule_dependencies file =
  let t = Hashtbl.create 101 in
  if not (file =~ ".*\\.cocci")
  then pr2 "warning: seems not a .cocci file";
  Iso_pattern.verbose_iso := false;
  let (_,xs,fvs,_,_,_,_,_) =
    Parse_cocci.process file (Some !Config.std_iso) false in
  Printf.printf "digraph {\n";
  let prevrule = ref "" in
  List.iter2
    (fun def fvs ->
      match def with
	Ast_cocci.ScriptRule (nm,_,dep,script_vars,_,_) ->
	  print_dotted_link nm !prevrule;
	  prevrule := nm;
	  depto t nm dep;
	  List.iter (function (_,(parent,_),_,_) -> print_link t nm parent)
	    script_vars
      | Ast_cocci.InitialScriptRule (_,_,_,_,_)
      | Ast_cocci.FinalScriptRule (_,_,_,_,_) -> ()
      | Ast_cocci.CocciRule (nm,(dep,_,_),_,_,_) ->
	  print_dotted_link nm !prevrule;
	  prevrule := nm;
	  depto t nm dep;
	  List.iter (function (parent,_) -> print_link t nm parent)
	    (List.concat fvs))
    xs fvs;
  Printf.printf "}\n";
  Printf.printf
    "// pipe to: ccomps -Cx | dot | gvpack -array_1 | neato -n2 -T pdf\n"

(*****************************************************************************)
(* to be called by ocaml toplevel, to test. *)
(*****************************************************************************)

(* no point to memoize this one *)
let sp_of_file file iso    = Parse_cocci.process file iso false

(* TODO: Remove
*)

(*
let flows_of_ast astc =
  astc +> Common.map_filter (fun e -> ast_to_flow_with_error_messages e)

let one_flow flows =
  List.hd flows

let one_ctl ctls = List.hd (List.hd ctls)
*)
