open Common open Commonop 

let default_output_file_testall = "/tmp/output_testall.c"

let best_score_file = "/tmp/score_cocci_best.marshalled"

let timeout_value = 30


(*****************************************************************************)
(* There can have multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and optional multiple
 * base_vernn.c and base_vernn.res 
 *)
let testone x compare_with_expected iso_file outfile = 
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in

  let cfile      = "tests/" ^ x ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in
  let iso_file = Some (if iso_file = "" then "standard.iso" else iso_file) in

  let expected_res   = "tests/" ^ x ^ ".res" in
  begin
    Cocci.full_engine cfile (cocci_file, iso_file) outfile;

    if compare_with_expected 
    then 
      Compare_c.compare_default outfile expected_res +>
        Compare_c.compare_result_to_string +> pr2
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

      let generated = default_output_file_testall in
      let expected = "tests/" ^ res in

      try (
        Common.timeout_function timeout_value (fun () -> 
          
          Cocci.full_engine cfile (cocci_file, iso_file) generated;

          let (correct, diffxs)= Compare_c.compare_default generated expected 
          in

	  pr2 res;
	  Ctlcocci_integration.print_bench();

          (* I don't use Compare_c.compare_result_to_string because
           *  I want to indent a little more the message 
           *)
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
    let total = 
      Common.hash_to_list newscore +> List.length in
    let good  = 
      Common.hash_to_list newscore +> List.filter (fun (s, v) -> v = Ok) +> 
        List.length 
    in
    
    pr2 (sprintf "good = %d/%d" good total);

  end
