open Fullcommon

open Ast_c
open Visitor_c 


(* 
This module handle the IO, the special name of files, ... and the pure algorithmic stuff is in ?
*)



(******************************************************************************)

(* TODO stat per dir ?  give in terms of func_or_decl numbers:   nbfunc_or_decl pbs / nbfunc_or_decl total ?/ *)
(* note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les valeurs *)
(*    alors on parsera correctemet tout le fichier et pourtant y'aura aucune def  et donc *)
(*    aucune couverture en fait.   *)
(*  ==> TODO evaluer les parties non parsé ? *)

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

(*******************************************************************************)
let foo xs = 

  let _stat_list = ref [] in
  let correctly_parsed_sites = ref 0 in
  let badly_parsed_sites = ref 0 in

 (* CONFIG can uncomment some words *)
    let errorwords_list = 
    [
 "6",
   Right ["mem_map_reserve"; "cs4x_mem_map_reserve"; "mem_map_unreserve"; "cs4x_mem_map_unreserve"];
 "7",
   (* NEW *)
   Right [(*"i2c_client";*) (*".name"; "->data"*)];
 "9",
   Right ["scsi_host_hn_get";     "scsi_host_put"];
 "15",
   (* NEW *)
   Right ["video_device"; (* "mmap";*) "remap_page_range"; ];
  "19", (*    !!!!!!!!!!! *)
   (* NEW *)
   Right ["IsdnCard"; "irq_func";  "pt_regs"; "IsdnCardState"; (*"spin_lock"; "spin_unlock";*) ];
 "22",
   (* NEW *)
   Right ["i2c_sendbyte";"i2c_readbyte";"i2c-old";(*"i2c_bus";*)"I2C_DRIVERID_VIDEODECODER"; "i2c_register_driver"; 
   "i2c_driver_struct";"i2c_unregister_driver";
   "MOD_INC_USE_COUNT"; "MOD_DEC_USE_COUNT";
   "i2c_device";
   (* TODO mentionne des nomw de fonctions (write_fn, read_fn, ...) que je peux pas detecter via error words *)
   
   ]; (* TODO but i2c-old.h will be in comment so ... ? *)
 "31",
   (Left ["CLEAR_INTR"]); 
  "34", (*    !!!!!!!!!! *)
   Right [(*"queue"; "request_queue"; "QUEUE";*) "BLK_DEFAULT_QUEUE"]; (* TODO but some of it may have been erased by lexer cos considered as cpp line ? *)
 "35",
   Right ["PStack";
   "l1l2"; "l2l1"; "l2l3"; "l3l4"; "l3l2"; "l4l3"; "l4l3_proto";
   ];
  "38", (*   !!!!!!!! *)
   Right ["pnp_activate_dev"];
 "40", (* !!!!!!!!!!!! *)
   (* NEW *) 
   Right ["usb_register"; "usb_deregister"; "probe"; "disconnect"; "usb_driver"; "devfs_register"; "devfs_unregister"; ];
  "41",   (* !!!!!!!!!!!! *)
   Right ["usb_deregister_dev";   (*"minor";*) "num_minor"; "usb_register_dev"];
 "44",
   Right ["in_ntoa"];
 "47",
   Right ["synchronize_irq"];
 "52",
   Right ["input_dev";"gameport";"idbus"; "idvendor"; "idproduct";"idversion"];
 "53",
   Right ["ATA_MAX_PRD"];
 "58",
   Right ["bio_endio"; "bio_io_error"];
 "59",
   Right ["devfs_mk_dir"];
 "60",
   Right ["nr_real"; "nr_structs"];
 "64",
   Right ["snd_pcm_lib_preallocate_pci_pages_for_all";"snd_pcm_lib_preallocate_isa_pages_for_all"; "snd_pcm_lib_preallocate_sbus_pages_for_all"];
 "66", (*    !!!!!!!!!!!!!!!!!!!! *)
   Right ["snd_magic_cast"; "snd_magic_kfree"; "snd_magic_kmalloc"; "snd_magic_kcalloc"];
 "69",
   Right ["usb_sndbulkpipe";"usb_rcvbulkpipe"; "usb_sndctrlpipe"; "usb_rcvctrlpipe";     "pusb_dev"];
 "72",
   (* NEW *)
   Right ["init_etherdev";       "unregister_netdev"; "register_netdev"];
 "73",
   (* NEW *)
   Right ["remap_page_range";        "vm_page_prot";  "vm_area_struct"];
 "74",
   Right ["atomic_dec";    "mddev_to_conf";  "nr_pending"];
 "75",
   Right ["pci_set_dma_mask"; (*&*)  "pci_dma_supported"   ];
 "76",
      (* TODO how detect fonction ? if add the type of the argument then could *)
      Right ["con_blank";"consw"];
 "78",
      (* TODO how detect fonction ? if add the type of the argument then could *)
      Right ["usb_serial_device_type"];
 "80",
   Right ["remap_page_range";   "kvirt_to_pa";   (* "PAGE_SHIFT"*)];
 "83",
   Right ["scsi_cmd_ioctl"; "cdrom_ioctl";"generic_ide_ioctl"];

   ] 
         (* CONFIG *)
        (* filter some x *)
        +> List.map (fun (s, x) -> if (List.mem s ["34"; "40"; "41"; "66"; "19"]) then (s, Right []) else (s, x)) 
        (* l'inverse, select only some x *)
(*        +> List.map (fun (s, x) -> if (List.mem s ["9"]) then (s, x) else (s, Right [])) *)

    in
   let h_error_rule = Hashtbl.create 100 in
   let _ = errorwords_list +> List.map fst +> List.iter (fun s -> Hashtbl.add h_error_rule s (ref 0)) in
   let h_files = Hashtbl.create 100 in
   let _ = errorwords_list +> List.map fst +> List.iter (fun s -> Hashtbl.add h_files s (ref 0)) in


  begin
  (xs) +> List.iter (fun file -> 
    pr2 ("HANDLING: " ^ file);
                  (*
                      ["xpram_devops"];
                      ["r_alt_ser_baudrate_shadow"];
                  *)
                file +> Parse_c.parse_print_error_heuristic
                     +> (fun (program2, stat) -> 
                          program2 +> List.map (fun (e, info) -> 
                            let (filename, (pos1, pos2), stre, toks) = info in 
                            let (filename, evo_file) = 
                              if filename =~ ".*bugs/\\([0-9]+\\)/.*/linux-\\([0-9]\\.[0-9]\\.[0-9]+/.*\\)" 
                              then matched2 filename +> swap
                              else filename, "none"
                            in
                            let is_in_bad = 
                            (* todo: 
                                  have to look  in comments too ?  for the error words
                                  or at least inside the TCommentCpp (which include the #if 0)
                                  en meme temps ils parsent mal alors ...
                            *)

                            (match e with
                            | NotParsedCorrectly _  -> true
                            | _                    -> false
                            ) 
                            in
                            let stre2 = Str.global_replace (Str.regexp "\n") " " stre in (* cos caml regexp dont like \n ... *)
                            let have_found_one        = ref false in
                            let have_found_one_in_bad = ref false in

                            errorwords_list +> List.iter (fun (evo_error_rule, errorwords) -> 
                                  let (errorwords_elems, found) = 
                                   (match errorwords with
                                   | Left errorwords ->  errorwords,    errorwords +> List.for_all (fun s -> 
                                       stre2 =~ (".*" ^  s )) 
                                   | Right errorwords ->  errorwords,   errorwords +> List.exists (fun s ->
                                       stre2 =~ (".*" ^  s  ))  
                                   ) in
                                  if found then have_found_one := true;
                                  if found && is_in_bad then have_found_one_in_bad := true;
                                  if found && is_in_bad then 
                                    begin
                                      pr "##############################################################################";
                                      pr ("SLICEEEEEEEEEEEEEEEEEE, on " ^ filename ^ " = "); 
                                      pr ("CONTAINNNNNNNNNNN = " ^ (join " " errorwords_elems));
                                      pr stre; 
                                      pr "##############################################################################";
                                      (* CONFIG *)
                                      if evo_error_rule = evo_file then 
                                        begin
                                      incr (Hashtbl.find h_error_rule evo_error_rule);
                                      incr (Hashtbl.find h_files evo_file);
                                        end
                                    end;
                              );
                             assert (!have_found_one_in_bad ==> !have_found_one);
                             if !have_found_one_in_bad then incr badly_parsed_sites;
                             if !have_found_one && not !have_found_one_in_bad then incr correctly_parsed_sites;

                            if !have_found_one
                            then ((e, Unparse_c.PPnormal), info)
                            else ((e, Unparse_c.PPviatok), info)

                            )
                          +> (fun program2_with_selection -> 
                            Unparse_c.pp_program file program2_with_selection;
                            ignore(Unix.system (sprintf "diff -u -p  %s %s" file "/tmp/output.c" )); (*want see diff of space => no -b -B *)
                                                  (* +> Transformation.test_simple_trans1);*)

                              ()
                             );

                          push2 stat _stat_list;
                       )
                    );
  if not (null !_stat_list) then print_stat !_stat_list; 
  pr "\nError rule stats (columns format = number of the rule, number of errors with this rule,  words that describe this rule)";
  pr "(note that ONLY stat for pattern found in BADLY parsed region are reported)\n";
  h_error_rule +> hash_to_list +>  List.sort (fun (s1,i) (s2,j) -> compare !j !i) +> List.iter (fun (s, counter) -> 
    pr (sprintf "%s\t%d\t with %s" s !counter 
          (errorwords_list +> List.assoc s +> (function Left xs -> join " " xs | Right xs -> join " " xs)));
    );
  pr "\nEvo files stats (columns format = number of the rule, number of files that was normally concerned with such a rule and that have an error)\n";
  h_files +> hash_to_list +>  List.sort (fun (s1,i) (s2,j) -> compare !j !i) +> List.iter (fun (s, counter) -> 
    pr (sprintf "%s\t%d\t" s !counter 
          );
    );

  pr ((sprintf "\nSites stat: correctly parsed relevant sites %d, badly parsed relevant sites %d =====> %d " 
         !correctly_parsed_sites 
         !badly_parsed_sites           
         ((100 * !correctly_parsed_sites ) / (!correctly_parsed_sites + !badly_parsed_sites)) ^ "%\n");
);

  end




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
        "-cocci_error_words",  Arg.Set_string Flag.cocci_error_words, "   the corresponding error words";

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

        (fullxs) +> List.iter (fun file -> 
          pr2 ("HANDLING: " ^ file);

          if not (file =~ ".*\\.c") then pr2 "warning: seems not a .c file";
          file +> Parse_c.parse_print_error_heuristic 
            +> (fun (x, stat) -> 
              push2 stat _stat_list;
               );
            );
        if not (null !_stat_list) then print_stat !_stat_list;

    | "parse_cocci", [file] -> 
        if not (file =~ ".*\\.cocci") then pr2 "warning: seems not a .cocci file";
        (try 
          let xs = Parse_cocci.process file false in
          ()
        with x -> pr2 "BAD"               )

    | "control_flow", [file] -> 
        if not (file =~ ".*\\.c") then pr2 "warning: seems not a .c file";
        file 
          +> Parse_c.parse_print_error_heuristic
          +> (fun (program, stat) -> 
            program +> List.iter (fun (e,_) -> 
              match e with
              | Definition ((funcs, _, _, c,_) as def)                    -> 
                  pr2 funcs;
                  (try 
                    Control_flow_c.test def
                  with Control_flow_c.DeadCode None      -> pr2 "deadcode detected, but cant trace back the place"
                  | Control_flow_c.DeadCode Some info -> pr2 ("deadcode detected: " ^ (error_message file ("", info.charpos) ))
                        
                  )
                    
              | _ -> ()
                                 );
             )





    | "cocci", [file] -> 
        if (!Flag.cocci_file = "")
        then failwith "I need a cocci file,  use -cocci_file <filename>"
        else
          let semantic_patch = 
            begin
              pr2 ("processing semantic patch file: " ^ !Flag.cocci_file);
              Parse_cocci.process !Flag.cocci_file false
            end
          in
          let error_words = 
            if !Flag.cocci_error_words = "" 
            then []
            else [ !Flag.cocci_error_words ]
          in
          let patchinfo = 
            if !Flag.classic_patch_file = ""
            then None
            else 
              begin
                pr2 ("processing classic patch file: " ^ !Flag.classic_patch_file);
                      (* Some (Classic_patch.parse_patch (cat Ast_cocci.ex2_patch))  *)
                Some (cat !Flag.classic_patch_file +> Classic_patch.filter_driver_sound +> Classic_patch.parse_patch)
              end
          in
          let (ast_cfile, _stat) = Parse_c.parse_print_error_heuristic file in
          Cocci.cocci_grep  semantic_patch error_words  ast_cfile



    | "special_foo", x::xs -> foo (x::xs )
          
    | "special_request", _ -> ()
   
    | "test_parse_classic_patch", [] -> Classic_patch.parse_patch (cat "/tmp/patch1") +> ignore
   
    | "test_filter_driver", [] ->  cat "/home/pad/kernels/patches/patch-2.5.71"
                               +> Classic_patch.filter_driver_sound 
                              +> List.iter pr2


    | s, [] -> Arg.usage options usage_msg; failwith "too few arguments"
    | _ -> failwith "no action for this"
   );
   (* pr2 (profiling_diagnostic ()); *)
   end



let _ = if not (!Sys.interactive) then main ()
