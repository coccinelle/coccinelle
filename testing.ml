(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
* This file is part of Coccinelle.
* 
* Coccinelle is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, according to version 2 of the License.
* 
* Coccinelle is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
* 
* The authors reserve the right to distribute this or future versions of
* Coccinelle under other licenses.
*)


open Common

(*****************************************************************************)
(* Test framework *)
(*****************************************************************************)

(* There can be multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and some optional
 * base_vernn.[c,res].
 * 
 * If want to test without iso, use -iso_file empty.iso option.
 *)
let testone x compare_with_expected_flag = 
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in

  let cfile      = "tests/" ^ x ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in

  let expected_res   = "tests/" ^ x ^ ".res" in
  begin
    let res = Cocci.full_engine (cocci_file, !Config.std_iso) [cfile] in
    let generated = 
      match Common.optionise (fun () -> List.assoc cfile res) with
      | Some (Some outfile) -> 
          if List.length res > 1 
          then pr2 ("note that not just " ^ cfile ^ " was involved");

          let tmpfile = "/tmp/"^Common.basename cfile in
          pr2 (sprintf "One file modified. Result is here: %s" tmpfile);
          Common.command2 ("mv "^outfile^" "^tmpfile);
          tmpfile
      | Some None -> 
          pr2 "no modification on the input file";
          cfile
      | None -> raise Impossible
    in
    if compare_with_expected_flag
    then 
      Compare_c.compare_default generated expected_res 
      +> Compare_c.compare_result_to_string 
      +> pr2;
  end
          

(* ------------------------------------------------------------------------ *)
let testall () =

  let newscore  = empty_score () in

  let expected_result_files = 
    Common.glob "tests/*.res" 
    +> List.filter (fun f -> Common.filesize f > 0)
    +> List.map Filename.basename
    +> List.sort compare
  in

  begin
    expected_result_files +> List.iter (fun res -> 
      let x = if res =~ "\\(.*\\).res" then matched1 res else raise Impossible 
      in
      let base = if x =~ "\\(.*\\)_ver[0-9]+" then matched1 x else x in 
      let cfile      = "tests/" ^ x ^ ".c" in
      let cocci_file = "tests/" ^ base ^ ".cocci" in
      let expected = "tests/" ^ res in

      let timeout_testall = 30 in

      try (
        Common.timeout_function timeout_testall  (fun () -> 
          
          let xs = Cocci.full_engine (cocci_file, !Config.std_iso) [cfile] in
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
                (diffxs +> List.map(fun s -> "    "^s^"\n") +> Common.join "")
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
    Common.regression_testing newscore 
      (Filename.concat Config.path "tests/score_cocci_best.marshalled");


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

(* test_to_string *)
let t_to_s = function
  | Ok -> ".ok"
  | SpatchOK -> ".spatch_ok"
  | Failed -> ".failed"

let delete_previous_result_files infile = 
  [Ok;SpatchOK;Failed] +> List.iter (fun kind -> 
    Common.command2 ("rm -f " ^ infile ^ t_to_s kind)
  )

(* quite similar to compare_with_expected  below *)
let test_okfailed cocci_file cfiles = 
  cfiles +> List.iter delete_previous_result_files;

  (* final_files contain the name of an output file (a .ok or .failed
   * or .spatch_ok), and also some additionnal strings to be printed in
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
  
  Common.redirect_stdout_stderr newout (fun () -> 
    try (
      Common.timeout_function_opt !Flag_cocci.timeout (fun () ->

        
        let outfiles = Cocci.full_engine (cocci_file, !Config.std_iso) cfiles
        in

        let time_str = time_per_file_str () in
          
        outfiles +> List.iter (fun (infile, outopt) -> 
          let (dir, base, ext) = Common.dbe_of_filename infile in
          let expected_suffix   = 
            match ext with
            | "c" -> "res"
            | "h" -> "h.res"
            | s -> pr2 ("WIERD: not a .c or .h :" ^ base ^ "." ^ s);
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
		    )
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
    Common.cmd_to_list ("find -name \"*.ok\"") 
    ++
    Common.cmd_to_list ("find -name \"*.spatch_ok\"")
  in
  let failed = Common.cmd_to_list ("find -name \"*.failed\"") in

  if null (oks ++ failed) 
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
      | s -> failwith ("wierd C file, not a .c or .h :" ^ s)
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

  let (xs,_,_,_,_,grep_tokens,query) =
    Parse_cocci.process file (Some !Config.std_iso) false in
  xs +> List.iter Pretty_print_cocci.unparse;
  Printf.printf "grep tokens\n";
  List.iter (function x -> Printf.printf "%s\n" (String.concat " " x))
    grep_tokens;
  if !Flag.use_glimpse
  then match query with None -> pr "No query" | Some x -> pr x








(*****************************************************************************)
(* to be called by ocaml toplevel, to test. *)
(*****************************************************************************)

(* no point to memoize this one *)
let sp_of_file file iso    = Parse_cocci.process file iso false

(* TODO: Remove
let (rule_elem_of_string: string -> filename option -> Ast_cocci.rule_elem) =
 fun s iso -> 
  begin
    Common.write_file ("/tmp/__cocci.cocci") (s);
    let (astcocci, _,_,_,_,_) = sp_of_file ("/tmp/__cocci.cocci") iso in
    let stmt =
      astcocci +>
      List.hd +> (function (_,_,x) -> List.hd x) +> (function x ->
	match Ast_cocci.unwrap x with
	| Ast_cocci.CODE stmt_dots -> Ast_cocci.undots stmt_dots +> List.hd
	| _ -> raise Not_found)
    in
    match Ast_cocci.unwrap stmt with
    | Ast_cocci.Atomic(re) -> re
    | _ -> failwith "only atomic patterns allowed"
  end
*)

(*
let flows_of_ast astc = 
  astc +> Common.map_filter (fun e -> ast_to_flow_with_error_messages e)

let one_flow flows = 
  List.hd flows

let one_ctl ctls = List.hd (List.hd ctls)
*)

