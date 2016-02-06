(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* ------------------------------------------------------------------------- *)

(* Regression tests.
 * Process for spgen:
 *  - file.cocci      (original cocci file)
 *  - file.config     (config file for running spgen)
 *  - file.expected   (expected spgenerated file)
 *
 * Run <spgen file.cocci --config file.config -o file.actual.cocci>
 * Compare file.actual.cocci with file.expected
 * Run <spatch --parse-cocci file.actual.cocci -D context>.
 *)

(* ------------------------------------------------------------------------- *)

(* some common functions *)
let spf = Common.spf                    (* Printf.sprintf *)
let perr_nl = prerr_endline             (* Common.pr2 *)
let perr = prerr_string                 (* Common.pr_no_nl *)
let dbe2nm = Common.filename_of_dbe     (* (path,file,ext) -> path/file.ext *)
let nm2dbe = Common.dbe_of_filename     (* path/file.ext -> (path,file,ext) *)

(* hardcoded values; timeout for testing + extension names *)
let timeout_per_file = 60
let exp_ext = "expected"
let act_ext = "actual.cocci"
let score_ext = "score" (* marshalling format used by Common *)


(* ------------------------------------------------------------------------- *)

(* more or less the same as in coccinelle/parsing_c/compare_c.ml.
 * diff flags are
 *  -u: unified format (ie. output diffs with - and +)
 *  -b: ignore changes in amount of whitespace
 *  -B: ignore changes in blank lines
 *)
let get_diff filename1 filename2 =
  let com = spf "diff -u -b -B %s %s" filename1 filename2 in
  let xs = Common.cmd_to_list com in

  (* get rid of the --- and +++ lines *)
  if xs = [] then xs else Common.drop 2 xs

(* Run spgen on <file>.cocci with <file>.config,
 * compare to <file>.expected,
 * add result to <score>.
 *)
let compare_one score expected =

  let (dir, base, ext) = nm2dbe expected in
  let hashkey = base ^ "." ^ ext in
  let actual = dbe2nm (dir, base, act_ext) in
  let cocci = dbe2nm (dir, base, "cocci") in
  let config = dbe2nm (dir, base, "config") in

  if ext <> exp_ext then failwith ("expected extension "^exp_ext^", not "^ext);
  if not(Sys.file_exists cocci) then failwith ("no cocci for " ^ expected);
  if not(Sys.file_exists config) then failwith ("no config for " ^ expected);

  try

    Common.timeout_function "spgen_test" timeout_per_file (
      fun () ->

        perr_nl cocci;

        (* spgenerate the file *)
        let options = Spgen.make_options ~output:actual cocci in
        let _ = Spgen.run options in

        (* check that the spgenerated file is parsable. Note that the parsing
         * flag generating_mode must be false (this should be done in spgen.ml).
         *)
        Flag.set_defined_virtual_rules "context";
        let _ = Parse_cocci.process actual None false in

        match get_diff actual expected with
        | [] ->

            let _ = if Sys.file_exists actual then Sys.remove actual in
            Hashtbl.add score hashkey Common.Ok

        | difflist ->

            let difflist = List.map (spf "    %s\n") difflist in
            let difflist = String.concat "" difflist in
            let diff =
              spf "INCORRECT: %s\n    diff (actual vs expected) = \n%s"
              actual difflist in
            Hashtbl.add score hashkey (Common.Pb diff)
    )

  with exn ->

    let s = spf "PROBLEM\n   exn = %s\n" (Printexc.to_string exn) in
    Hashtbl.add score hashkey (Common.Pb s)

(* Prints regression test statistics and information + updates score files.
 * (perhaps split, but then also have to refactor Common.regression_testing_vs)
 *
 * <test_dir> The directory in which the regression test files are stored
 * <score>    The new test information
 * Similar to coccinelle/testing.ml, but with less stuff.
 *)
let print_update_regression test_dir score =

  if Hashtbl.length score <= 0 then failwith "There are no tests results ...";

  let expected_score_file = dbe2nm (test_dir, "SCORE_expected", score_ext) in
  let best_of_both_file = dbe2nm (test_dir, "SCORE_best_of_both", score_ext) in
  let actual_score_file = dbe2nm (test_dir, "SCORE_actual", score_ext) in

  perr_nl "--------------------------------";
  perr_nl "statistics";
  perr_nl "--------------------------------";

  let print_result (filename, result) =
    perr (spf "%-40s: " filename);
    perr (
      match result with
      | Common.Ok ->  "CORRECT\n"
      | Common.Pb s -> s
    ) in

  (* hash_to_list also sorts the entries by filename *)
  List.iter print_result (Common.hash_to_list score);

  perr_nl "--------------------------------";
  perr_nl "regression testing  information";
  perr_nl "--------------------------------";
  perr_nl ("regression file: "^ expected_score_file);

  let expected_score =
    if Sys.file_exists expected_score_file then
      Common.load_score expected_score_file()
    else
      let s = Common.empty_score() in
      let _ = Common.save_score s expected_score_file in
      s
  in

  (* find and print changes in test results since last time *)
  let new_bestscore = Common.regression_testing_vs score expected_score in

  Common.save_score score actual_score_file;
  Common.save_score new_bestscore best_of_both_file;
  Common.print_total_score score;

  let (good, total) = Common.total_scores score in
  let (expected_good, expected_total) = Common.total_scores expected_score in

  if good = expected_good then begin

    perr_nl "Current score is equal to expected score; everything is fine"

  end else
  if good < expected_good then begin

    perr_nl "Current score is lower than expected :(";
    perr_nl (spf "(was expecting %d but got %d)\n" expected_good good);
    perr_nl "If you think it's normal, then maybe you need to update the";
    perr_nl (spf "score file %s, copying info from %s."
             expected_score_file actual_score_file)

  end else
  (* if good > expected_good then *) begin

    perr_nl "Current score is greater than expected :)";
    perr_nl (spf "(was expecting %d but got %d)" expected_good good);
    perr_nl "Generating new expected score file and saving old one";
    Common.command2_y_or_no_exit_if_no
      (spf "mv %s %s" expected_score_file (expected_score_file ^ ".save"));
    Common.command2_y_or_no_exit_if_no
      (spf "mv %s %s" best_of_both_file expected_score_file)

  end


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let regression_test ~test_dir =

  (* sort expected result files by name *)
  let test_files = dbe2nm (test_dir, "*", exp_ext) in
  let e = Common.glob test_files in
  let e = List.filter (fun f -> Common.filesize f > 0) e in
  let expected_files = List.sort compare e in
  if e = [] then failwith (
    (spf "No test files with expected extension <.%s> found." exp_ext) ^
         " Are you sure this is the right directory?"
  );

  (* populate score table *)
  let actual_score = Common.empty_score() in
  List.iter (compare_one actual_score) expected_files;

  print_update_regression test_dir actual_score
