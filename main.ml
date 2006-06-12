open Common open Commonop

(* 
This module handle the IO, the special name of files, ... 
The pure algorithmic stuff is in other modules.
*)

(******************************************************************************)
let main () = 
  begin
    let args = ref [] in
    let options = Arg.align
      [ 
        "-no_verbose_parsing", Arg.Clear      Flag.verbose_parsing , "  ";
        (* still?: dont forget to add -action tokens, and to get rid of no_verbose_parsing *)
        "-debug_lexer",        Arg.Set        Flag.debug_lexer , " ";
        "-debug_etdt",         Arg.Set        Flag.debug_etdt , "  ";

        "-debug_cfg",          Arg.Set        Flag.debug_cfg , "  ";

        "-action",             Arg.Set_string Flag.action , ("   (default_value = " ^ !Flag.action ^")"
       ^ "\n\t possibles actions are:

               tokens
               parse_c
               parse_cocci
               control_flow

               cocci (require to be associated with -cocci_file)

         so to test C parser, do -action parse_c ...
                "
                                                          );
        "-dir",                Arg.Set        Flag.dir, "   process all files in directory recursively";

        "-cocci_file",         Arg.Set_string Flag.cocci_file, "   the semantic patch file";

        "-classic_patch_file", Arg.Set_string Flag.classic_patch_file, "   the patch file corresponding to the linux version we are analyzing"     ;
      ] in 
    let usage_msg = ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <path-to-c-or-cocci-files-or-dir>\nOptions are:") in
    Arg.parse options (fun file -> args := file::!args) usage_msg;
 

    (match (!Flag.action, !args) with

    | "tokens_c", [file] -> 
        Flag.debug_lexer := true; Flag.verbose_parsing := true;
        Parse_c.tokens file +> List.iter (fun x -> pr2 (Dumper.dump x))

    | "parse_c", x::xs -> 
        let fullxs = 
          if !Flag.dir 
          then (assert (xs = []); process_output_to_list ("find " ^ x ^" -name \"*.c\"")) 
          else x::xs 
        in

        let _stat_list = ref [] in

        fullxs +> List.iter (fun file -> 
          pr2 ("HANDLING: " ^ file);

          if not (file =~ ".*\\.c") then pr2 "warning: seems not a .c file";
          file +> Parse_c.parse_print_error_heuristic +> (fun (x, stat) -> push2 stat _stat_list;);
        );
        if not (null !_stat_list) then Parse_c.print_parsing_stat_list !_stat_list;

    | "parse_cocci", [file] -> 
        if not (file =~ ".*\\.cocci") then pr2 "warning: seems not a .cocci file";
        (try 
          let xs = Cocci.spbis file in
          xs +> List.iter (fun (meta, rule) -> 
            Unparse_cocci.unparse rule
              )
        with x -> pr2 "BAD"               )

    | "control_flow", [file] -> 
        if not (file =~ ".*\\.c") then pr2 "warning: seems not a .c file";
        file 
          +> Parse_c.parse_print_error_heuristic
          +> (fun (program, stat) -> 
            program +> List.iter (fun (e,_) -> 
              match e with
              | Ast_c.Definition ((funcs, _, _, c,_) as def)                    -> 
                  pr2 funcs;
                  (try 
                    Control_flow_c.test def
                  with 
                  | Control_flow_c.DeadCode None      -> pr2 "deadcode detected, but cant trace back the place"
                  | Control_flow_c.DeadCode Some info -> pr2 ("deadcode detected: " ^ (error_message file ("", info.charpos) ))
                        
                  )
                    
              | _ -> ()
                                 );
             )





    | "cocci", [file] -> 
        if (!Flag.cocci_file = "") then failwith "I need a cocci file,  use -cocci_file <filename>";
        pr2 ("processing C file: " ^ file);
        let (_ast_cfile, _stat) = Parse_c.parse_print_error_heuristic file in
        pr2 ("processing semantic patch file: " ^ !Flag.cocci_file);
        let _semantic_patch     = Parse_cocci.process !Flag.cocci_file false in
        ()


    | "special_request", _ -> 
        ignore(Test.test_transfo ())
   
    | "test_parse_classic_patch", [] -> Classic_patch.parse_patch (cat "/tmp/patch1") +> ignore
   
    | "test_filter_driver", [] ->  
        cat "/home/pad/kernels/patches/patch-2.5.71"
          +> Classic_patch.filter_driver_sound 
          +> List.iter pr2


    | s, [] -> Arg.usage options usage_msg; failwith "too few arguments"
    | _ -> failwith "no action for this"
   );
   (* pr2 (profiling_diagnostic ()); *)
   end


let _ = if not (!Sys.interactive) then main ()
