open Common open Commonop

type ppmethod = PPviatok of Ast_c.info list | PPnormal


(******************************************************************************)
(*
 todo: take care of priority. because of the transformations, for instance
 A => A+A, and  if had B*A, we dont want to generate  B*A+A, we must insert
  some extra  () (but not always, only if necessary)
 src: rene

 note: if add instruction, then try keep same indentation. So need introduce
  some spacings. Done via the semi global _current_tabbing variable. *)
(******************************************************************************)

open Ast_c




let pp_program file x = 
  
 with_open_outfile "/tmp/output.c" (fun (pr,chan) -> 
   let pr s = pr s; flush chan in

   let _table = Common.full_charpos_to_pos file in

   (* note: that not exactly same tokens as in parsing, cos in parsing there is
      some transformation of tokens such as TIdent in Typedef, TIdent in TString
      but for what we are interested here, it is not really important. *)
   let toks = (Parse_c.tokens file) in 
   let toks = ref (toks +> List.map (fun tok -> 
     (tok, Parse_c.info_from_token tok))) 
   in
   
   let _lastasked = ref (Common.fake_parse_info, Ast_c.emptyAnnot) in

   let is_between_two_minus (infoa,(mcoda,enva)) (infob,(mcodb,envb)) =
     match mcoda, mcodb with
     | Ast_cocci.MINUS _, Ast_cocci.MINUS _ -> true
     | _ -> false
   in

   let _current_tabbing = ref "" in
   let update_current_tabbing s = 
     let xs = list_of_string s in
     if (xs +> List.exists (fun c -> c = '\n'))
     then
       let newtabbing = 
       xs
       +> List.rev
       +> Common.take_until (fun c -> c = '\n')
       +> List.rev
       +> List.map string_of_char
       +> String.concat ""
       in
       _current_tabbing := newtabbing
   in

   (* ---------------------- *)
   let sync (elem,annot) = 
     assert (elem <> Common.fake_parse_info);
     (* todo: if fake_parse_info ?  print the comments that are here ? *)
      let (before, after) = !toks +> span (fun (tok, (info,_ANNOT)) -> 
        info.charpos < elem.charpos)   
      in
      toks := after;

      let (commentsbefore, passed) = 
        before +> List.rev +> span (fun (tok, tokinfo) -> 
          (match tok with
          | Parser_c.TComment i -> true
          | Parser_c.TCommentSpace i -> true
          | Parser_c.TCommentCpp i -> true
          | Parser_c.TCommentAttrOrMacro i -> true
          | _ -> false
          ))
      in
      let (commentsbefore, passed) = (List.rev commentsbefore, List.rev passed) 
      in

      passed +> List.iter (fun (tok, tokinfo) -> 
           (match tok with
            | Parser_c.TComment (i,_) -> 
                pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
            | Parser_c.TCommentCpp (i,_) -> 
                pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
            | Parser_c.TCommentAttrOrMacro (i,_) -> 
                pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
            | _ -> pr2 ("pp_passing_token: " ^ (fst tokinfo).str);
           )
         );

      commentsbefore +> List.iter (fun (tok, tokinfo) -> 
          (match tok with
          | Parser_c.TComment            (i,_) -> pr i.str
          | Parser_c.TCommentSpace       (i,_) -> 
              update_current_tabbing i.str;
              if not (is_between_two_minus !_lastasked (elem,annot))
              then pr i.str;
          | Parser_c.TCommentCpp         (i,_) -> pr i.str
          | Parser_c.TCommentAttrOrMacro (i,_) -> pr i.str
          | x -> error_cant_have x
          );
         );
   
      let (tok, tokinfo) = pop2 toks in 
      assert_equal (fst tokinfo).charpos elem.charpos;
      (* pasforcement: assert_equal tokinfo.str elem.str;
         Indeed we may have "reused" a token to keep its related comment  and
         just change its value (e.g. if decide to transform every 0 in 1, 
         we will reuse the info from 0) *)
      ()
        
   in


   (* ---------------------- *)

   let rec pr_elem ((info,(mcode,env)) as e) = 
    if not (Ast_c.is_al_info info)
    then 
      begin
        if not ((fst e).charpos > (fst !_lastasked).charpos)
        then begin 
          pr2 
            (sprintf 
               "pp_c: wrong order, you ask for %s but have already pass %s" 
               (Dumper.dump e) (Dumper.dump !_lastasked)); 
          assert false; 
        end;

        sync e; 
        _lastasked := e;
      end;
     
     (*old: pr info.str *)
     let s = info.str in

     (* UGLY trick *)
     let s = 
       if Ast_c.is_al_info info 
       then
         (match s with
         | "char" | "short" | "int" | "double" | "float" | "long" 
         | "struct" | "union" | "signed" | "unsigned" 
         | "const" | "volatile"
           -> s ^ " "

         | x -> x
         )
       else s
     in

     match mcode with
     | Ast_cocci.MINUS (any_xxs) -> 
         Unparse_cocci.pp_list_list_any 
           (env,!_current_tabbing,pr, pr_elem) any_xxs 
     | Ast_cocci.CONTEXT (any_befaft) -> 
         (match any_befaft with
         | Ast_cocci.NOTHING -> pr s
               
         | Ast_cocci.BEFORE xxs -> 
             Unparse_cocci.pp_list_list_any 
               (env,!_current_tabbing,pr, pr_elem)xxs;
             pr s;
         | Ast_cocci.AFTER xxs -> 
             pr s;
             Unparse_cocci.pp_list_list_any 
               (env,!_current_tabbing,pr, pr_elem) xxs;
         | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
             Unparse_cocci.pp_list_list_any 
               (env,!_current_tabbing,pr, pr_elem) xxs;
             pr s;
             Unparse_cocci.pp_list_list_any 
               (env,!_current_tabbing,pr, pr_elem) yys;
             
         )
     | Ast_cocci.PLUS -> raise Impossible

   in

  (* ---------------------- *)
  (* start point *)
  (* ---------------------- *)

   x +> List.iter (fun ((e, ppmethod)) -> 
   match ppmethod with
   | PPviatok toks -> 
       (match e with
       | FinalDef (ii,_ANNOT) -> 
           (* todo: less: assert that FinalDef is the last one in the list *)
           pr_elem ({ii with str = ""},Ast_c.emptyAnnot) 
       | e -> toks +> List.iter (fun x -> pr_elem x)
       )

   | PPnormal -> 
     (match e with
       | Declaration decl -> Pretty_print_c.pp_decl_gen pr_elem decl
       | Definition ((s, (returnt, (paramst, (b, iib))), sto, statxs), 
                     is::iifunc1::iifunc2::i1::i2::isto) -> 
                       
         Pretty_print_c.pp_type_with_ident_gen pr_elem None (Some (sto, isto)) 
                         returnt;
         pr_elem is;
         pr_elem iifunc1;
         
         (match paramst with
         | [(((bool, None, t), ii_b_s), iicomma)] -> 
             assert 
               (match t with 
               | qu, (BaseType Void, ii) -> true
               | _ -> false
               );
             assert (null iicomma);
             assert (null ii_b_s);
             Pretty_print_c.pp_type_with_ident_gen pr_elem
               None None t
             
         | paramst -> 
           paramst +> List.iter (fun (((bool, s, t), ii_b_s), iicomma) ->
            iicomma +> List.iter pr_elem;
           
            (match b, s, ii_b_s with
            | false, Some s, [i1] -> 
                Pretty_print_c.pp_type_with_ident_gen 
                  pr_elem (Some (s, i1)) None t;
            | true, Some s, [i1;i2] -> 
                pr_elem i1;
                Pretty_print_c.pp_type_with_ident_gen 
                  pr_elem (Some (s, i2)) None t;

            (* in definition we have name for params, except when f(void) *)
            | _, None, _ -> raise Impossible 
            | _ -> raise Impossible
            )
         );
         );
            

         (* normally ii represent the ",..."  but it is also abused with the f(void) case *)
         (* assert (List.length iib <= 2);*)
         iib +> List.iter pr_elem;

         pr_elem iifunc2;
         pr_elem i1; 
         statxs +> List.iter (Pretty_print_c.pp_statement_gen pr_elem);
         pr_elem i2;

     | EmptyDef ii -> ii +> List.iter pr_elem
     | CPPDefine ii -> ii +> List.iter pr_elem
     | CPPInclude ii -> ii +> List.iter pr_elem

     | SpecialDeclMacro (s, es,   [i1;i2;i3;i4]) -> 
         pr_elem i1;
         pr_elem i2;
         es +> List.iter (fun (e, opt) -> 
           assert (List.length opt <= 1);
           opt +> List.iter pr_elem;
          (match e with
          | Left e -> Pretty_print_c.pp_expression_gen pr_elem e
          | Right (returnType, (sto, iisto)) -> 
              assert (List.length iisto <= 1);
              iisto +> List.iter pr_elem;
              Pretty_print_c.pp_type_with_ident_gen pr_elem None None returnType
          );
         );
         pr_elem i3;
         pr_elem i4;


     | NotParsedCorrectly ii -> 
         assert (List.length ii >= 1);
         ii +> List.iter pr_elem 

     | FinalDef (ii,_ANNOT) -> pr_elem ({ii with str = ""},Ast_c.emptyAnnot)
     | _ -> raise Impossible
     )   
   );
 )
