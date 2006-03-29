open Common open Commonop

(* alt: C for c, P for patch *)
module A = Ast_cocci
module B = Ast_c

(*
 re for rule_elem (work for regexp too :) )
*)


(* 
less: debugging info => unparse_statement, and unparse_rule_elem
*)

let (cocci_grep: Ast_cocci.rule_with_metavars list ->   string list -> Ast_c.program2 ->  unit) = fun semantic_patch error_words cfile ->
  
  let current_bindings = ref [(Pattern.empty_metavars_binding)] in

  semantic_patch +> List.iter (fun (metavars, rule) -> 
    
   rule +> List.iter (fun toplevel -> 
    cfile +> List.iter (fun (cunit, info) -> 
      let (filename, (pos1, pos2), stre, toks) = info in 
      let stre2 = Str.global_replace (Str.regexp "\n") " " stre in (* cos caml regexp dont like \n ... *)

      let found = error_words +> List.exists (fun s -> stre2 =~ (".*" ^  s  )) in
      
      match cunit with
      | B.FinalDef _ -> ()
      | B.EmptyDef _ -> ()

      | B.SpecialDeclMacro _ -> ()

      | B.NotParsedCorrectly _ -> ()

      | B.Declaration _ -> ()
      | B.Definition ((funcs, functype, sto, compound, ii) as deffunc) -> 
          pr ("processing:" ^ funcs);
          if found then 
            begin 
              pr2 "found one error word in that function";

              let cflow = 
                (try 
                  Control_flow_c.build_control_flow deffunc
                with 
                | Control_flow_c.DeadCode None      -> failwith "deadcode detected, but cant trace back the place"
                | Control_flow_c.DeadCode Some info -> failwith ("deadcode detected: " ^ (Common.error_message filename ("", info.charpos) ))
                ) in


(*              let _ = Control_flow_c.print_control_flow cflow in  *)

              match toplevel with
              | A.FUNCTION (A.DOTS rexs | A.CIRCLES rexs | A.STARS rexs) 
              | A.CODE (A.DOTS rexs | A.CIRCLES rexs | A.STARS rexs) -> 
                  pr2 "searching a statement";

                  (* todo: look at order ? *)
                  rexs +> List.iter (fun re -> 
                  
                    let nodes = cflow#nodes  in
                    nodes#tolist +> List.iter (fun (i, node) -> 

                      let xs = !current_bindings +> List.map (Pattern.match_re_node re node) +> List.flatten in
                      if xs <> []
                      then 
                        begin 
                          pr2 ("FOUND");
                          (* pr2 (Dumper.dump (re, node)); *)
                          print_string ("C  : " ^ snd node ^ "\n");
                          print_string ("SP : ");
                          ignore(Unparse_cocci.rule_elem "" re);
                          Format.force_newline(); Format.print_flush();
                          current_bindings := xs;
                        end

                      else ()
                    );
                  );
                  pr2 (Dumper.dump current_bindings);

              | _ -> raise Todo


            end
          

      );

      
      
    );
  );


(*
                  pr2 "##############################################################################";
                  pr2 ("SLICEEEEEEEEEEEEEEEEEE, on " ^ filename ^ " = "); 
                  pr2 stre; 
                  pr2 "##############################################################################";


*)
