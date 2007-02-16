open Common open Commonop 

let default_output_file = "/tmp/output.c"

let _Best_score_file = "/tmp/score_cocci_best.marshalled"

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
      if doexit then raise (Common.UnixExit 0)
  | Compare_c.Pb s -> 
      pr2 ("seems incorrect: " ^ s);
      pr2 "diff (result(-) vs expected_result(+)) = ";
      diffxs +> List.iter pr2;
      if doexit then raise (Common.UnixExit (-1))
  | Compare_c.PbOnlyInNotParsedCorrectly -> 
      pr2 "seems incorrect, but only because of code that was not parsable";
      if doexit then raise (Common.UnixExit (-1))
  


(*****************************************************************************)
(* There can have multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and optional multiple
 * base_vernn.c and base_vernn.res 
 *)
let testone x compare_with_expected iso_file = 
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in

  let cfile      = "tests/" ^ x ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in
  let iso_file = Some (if iso_file = "" then "standard.iso" else iso_file) in

  let outfile = default_output_file in
  let expected_res   = "tests/" ^ x ^ ".res" in
  begin
    Cocci.full_engine cfile (Left (cocci_file, iso_file)) outfile;

    if compare_with_expected 
    then print_diff_expected_res_and_exit outfile expected_res true;
  end
          

(*****************************************************************************)
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

      let generated = default_output_file in
      let expected = "tests/" ^ res in

      let timeout_value = 30 in

      try (
        Common.timeout_function timeout_value (fun () -> 
          
          Cocci.full_engine cfile (Left (cocci_file, iso_file)) generated;

          let a = Cocci.cprogram_from_file generated +> List.map fst in
          let b = Cocci.cprogram_from_file expected  +> List.map fst in

          let (correct, diffxs)= Compare_c.compare (a, generated) (b, expected)
          in
	  pr2 res;
	  Ctlcocci_integration.print_bench();

          (match correct with
          | Compare_c.Correct -> Hashtbl.add newscore res Common.Ok;
          | Compare_c.Pb s -> 
              let s = 
                "INCORRECT:" ^ s ^ "\n" ^ 
                  "    diff (result(<) vs expected_result(>)) = \n" ^
                  (diffxs +> List.map (fun s -> ("    " ^ s ^ "\n")) 
                    +> Common.join ""
                  )
              in
              Hashtbl.add newscore res (Common.Pb s)
          | Compare_c.PbOnlyInNotParsedCorrectly -> 
              let s = 
                "seems incorrect, but only because of code that " ^
                "was not parsable" 
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
    Common.regression_testing newscore _Best_score_file;


    pr2 "--------------------------------";
    pr2 "total score";
    pr2 "--------------------------------";
    let total = 
      Common.hash_to_list newscore +> List.length in
    let good  = 
      Common.hash_to_list newscore +> List.filter (fun (s, v) -> v = Ok) +> 
        List.length 
    in
    
    pr2 (sprintf "good = %d/%d" good total);

  end
