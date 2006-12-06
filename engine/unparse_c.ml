open Common open Commonop

type ppmethod = PPviatok of Ast_c.info list | PPnormal

(******************************************************************************)
(* todo: take care of priority. For instance A => A+A, and if had B*A,
 * we dont want to generate B*A+A, we must insert some extra () (but
 * not always, only if necessary) src: rene

 * note: if add instruction, then try keep same indentation. So need introduce
 * some spacings. Done via the semi global _current_tabbing variable. *)
(******************************************************************************)

open Ast_c



(******************************************************************************)


(******************************************************************************)

(* In addition to the Ast in x, I also take the name of the input file,
 * because I reparse it to be able later to synchrnize, get the comments,
 * that are not in the Ast.
 *)
let pp_program2 x infile outfile  = 
  
 Common.with_open_outfile outfile (fun (pr,chan) -> 
   let pr s = pr s; flush chan in

   (* note: that not exactly same tokens as in parsing, cos in parsing there is
    * some transformation of tokens such as TIdent in Typedef,TIdent in TString
    * but for what we are interested here, it is not really important. 
    *)
   let toks = (Parse_c.tokens infile) in 
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

          (* Can be treated as comment, Hence this case.
           * If was not treated as comment, then should have synced on it
           * before, so no problem to add this case.
           * In fact can have a problem if the stuff before was deleted,
           * and so during the sync it will go too far taking also a possible
           * define that may be deleted by the SP. But, we normally 
           * cant delete a token. A token is marked as minus, but not deleted,
           * so normally no problem.
           *)
          | Parser_c.TDefine i -> true
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

          | Parser_c.TDefine (i,_) -> pr i.str
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
               (env,!_current_tabbing,pr, pr_elem) xxs;
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
       Pretty_print_c.pp_program_gen pr_elem e
   );
 )

let pp_program a b c = 
  Common.profile_code "C unparsing" (fun () -> pp_program2 a b c)
