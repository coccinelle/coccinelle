open Fullcommon

open Ast_c


(* 
This module handle the IO, the special name of info files, ... and the pure algorithmic stuff is in ?
*)



(******************************************************************************)

(* todo?: stat per dir ?  give in terms of func_or_decl numbers:   nbfunc_or_decl pbs / nbfunc_or_decl total ?/ *)
(* note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les valeurs *)
(*    alors on parsera correctemet tout le fichier et pourtant y'aura aucune def  et donc *)
(*    aucune couverture en fait.   *)
(*  ==> todo evaluer les parties non parsé ? *)

let print_stat = fun statxs -> 
        let total = (List.length statxs) in
        let perfect = (statxs +> List.filter (function {Parse_c.have_timeout = false; bad = 0} -> true | _ -> false) +> List.length) in
        pr2 "\n\n\n---------------------------------------------------------------";
        pr2 "pbs with files:";
        statxs 
          +> List.filter (function {Parse_c.have_timeout = true} -> true | {Parse_c.bad = n} when n > 0 -> true | _ -> false)
          +> List.iter (function {Parse_c.filename = file; have_timeout = timeout; bad = n} -> 
                 pr2 (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
            );
        pr2 "\n\n\n---------------------------------------------------------------";
        pr2 (
          (sprintf "NB total files = %d; " total) ^
          (sprintf "perfect = %d; " perfect) ^
          (sprintf "pbs = %d; "     (statxs +> List.filter (function {Parse_c.have_timeout = b; bad = n} when n > 0 -> true | _ -> false) +> List.length)) ^
          (sprintf "timeout = %d; " (statxs +> List.filter (function {Parse_c.have_timeout = true; bad = n} -> true | _ -> false) +> List.length)) ^
          (sprintf "=========> %d" ((100 * perfect) / total)) ^ "%"
                                                          
         );
        let good = (statxs +> List.fold_left (fun acc {Parse_c.correct = x} -> acc+x) 0) in
        let bad  = (statxs +> List.fold_left (fun acc {Parse_c.bad = x} -> acc+x) 0)  in
        pr2 (
          (sprintf "nb good = %d,  nb bad = %d    " good bad) ^
          (sprintf "=========> %d"  (100 * good / (good+bad))) ^ "%"
         )



(******************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let options = Arg.align
      [ 
        "-no_verbose_parsing", Arg.Clear      Flag.verbose_parsing , "  ";
        "-debug_lexer",        Arg.Set        Flag.debug_lexer , " dont forget to add -action tokens, and to get rid of no_verbose_parsing ";
        "-debug_etdt",        Arg.Set        Flag.debug_etdt , "  ";
        "-action",             Arg.Set_string Flag.action , ("   (default_value = " ^ !Flag.action ^")"
       ^ "\n\t possibles actions are:
               parse_c
               tokens
         so to test C parser, do -action parse_c ...
                "
                                                          );
        "-dir",                Arg.Set        Flag.dir, "   process all c files in directory recursively";

      ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <path-to-c-files-or-dir>\nOptions are:") in
    Arg.parse options (fun file -> args := file::!args) usage_msg;
 
    let _stat_list = ref [] in

    (match (!args) with
    | [] -> Arg.usage options usage_msg; failwith "too few arguments"
    | x::xs -> 
      let xs = if !Flag.dir then process_output_to_list ("find " ^ x ^" -name \"*.c\"") else x::xs in
        
        (xs) +> List.iter (fun file -> 
            pr2 ("HANDLING: " ^ file);
            (match !Flag.action with

            | "parse_c" -> 
                if not (file =~ ".*\\.c") then pr2 "warning: seems not a .c file";
                file +> Parse_c.parse_print_error_heuristic 
                     +> (fun (x, stat) -> 
                          push2 stat _stat_list;
                        )

            | "parse_c_and_pp" -> 
                if not (file =~ ".*\\.c") then pr2 "warning: seems not a .c file";
                file +> Parse_c.parse_print_error_heuristic 
                     +> (fun (x, stat) -> 
                          push2 stat _stat_list;
                          Unparse_c.pp_program file (x +> List.map (fun (x, info) -> ((x, Unparse_c.PPnormal), info)));
                          ignore(Unix.system (sprintf "diff -u -p  %s %s" file "/tmp/output.c" )); (*want see diff of space => no -b -B *)
                                                  (* +> Transformation.test_simple_trans1);*)
                       )

            | "tokens" -> 
                Flag.debug_lexer := true; Flag.verbose_parsing := true;
                Parse_c.tokens file +> List.iter (fun x -> pr2 (Dumper.dump x))
            | _ -> failwith "no action for this"
            );
              (* pr2 (profiling_diagnostic ()); *)
          );
       if not (null !_stat_list) then print_stat !_stat_list;
    );
  end

let _ = 
  if not (!Sys.interactive) then main ()
