open Fullcommon

open Ast_c

(* 
This module handle the IO, the special name of info files, ... and the pure algorithmic stuff is in ?
*)

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
            pr2 ("PARSING: " ^ file);
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
                          Unparse_c.pp_program file (x +> List.map (fun (x, info) -> ((x, Unparse_c.PPnormal))));
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
