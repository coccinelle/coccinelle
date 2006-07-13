open Common open Commonop

type ppmethod = PPviatok of Ast_c.info list | PPnormal

(**************************************************************************************)
(*
todo:
 take care of priority. because of the transformations, for instance
 A => A+A, and  if had B*A, we dont want to generate  B*A+A, we must insert some extra  ()
 (but not always, only if necessary)
 src: rene

note: if add instruction, then try keep same indentation. So need introduce some spacings.
  Done via the semi global _current_tabbing variable.

*)
(**************************************************************************************)

open Ast_c
open Parser_c

let term ((s,_,_) : 'a Ast_cocci.mcode) = s

let pp_program file x = 

  
  with_open_outfile "/tmp/output.c" (fun (pr,chan) -> 
    let pr s = pr s; flush chan in

   let _table = Common.full_charpos_to_pos file in

   (* note: that not exactly same tokens as in parsing, cos in parsing there is some transformation of tokens 
         such as TIdent in Typedef, TIdent in TString, but for what we are interested here, it is not really important
    *)
   let toks = (Parse_c.tokens file) in 
   let toks = ref (toks +> List.map (fun tok -> (tok, Parse_c.info_from_token tok))) in
   
   let _lastasked = ref (Common.fake_parse_info, Ast_c.dumbAnnot) in

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
      let (before, after) = !toks +> span (fun (tok, (info,_ANNOT)) -> info.charpos < elem.charpos)   in
      toks := after;

      let (commentsbefore, passed) = before +> List.rev +> span (fun (tok, tokinfo) -> 
        (match tok with
        | TComment i -> true
        | TCommentSpace i -> true
        | TCommentCpp i -> true
        | TCommentAttrOrMacro i -> true
        | _ -> false
        ))
      in
      let (commentsbefore, passed) = (List.rev commentsbefore, List.rev passed)    in

      passed +> List.iter (fun (tok, tokinfo) -> 
           (match tok with
            | TComment (i,_) -> pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
            | TCommentCpp (i,_) -> pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
            | TCommentAttrOrMacro (i,_) -> pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
            | _ -> pr2 ("pp_passing_token: " ^ (fst tokinfo).str);
           )
         );

      commentsbefore +> List.iter (fun (tok, tokinfo) -> 
          (match tok with
          | TComment            (i,_) -> pr i.str
          | TCommentSpace       (i,_) -> 
              update_current_tabbing i.str;
              if not (is_between_two_minus !_lastasked (elem,annot))
              then pr i.str;
          | TCommentCpp         (i,_) -> pr i.str
          | TCommentAttrOrMacro (i,_) -> pr i.str
          | x -> error_cant_have x
          );
         );
   
      let (tok, tokinfo) = pop2 toks in 
      assert_equal (fst tokinfo).charpos elem.charpos;
      (* pasforcement: assert_equal tokinfo.str elem.str;
         indeed we may have "reused" a token to keep its related comment  and just change
         its value (e.g. if decide to transform every 0 in 1, we will reuse the info from 0)
      *)
      ()
        
   in


   (* ---------------------- *)

   let rec pr_elem ((info,(mcode,env)) as e) = 
    if not (Ast_c.is_al_info info)
    then 
      begin
        if not ((fst e).charpos > (fst !_lastasked).charpos)
        then begin pr2 (sprintf "pp_c: wrong order, you ask for %s but have already pass %s" (Dumper.dump e) (Dumper.dump !_lastasked)); assert false; end;

        sync e; 
        _lastasked := e;
      end;
     
     (*old: pr info.str *)
     let s = info.str in

     match mcode with
     | Ast_cocci.MINUS (any_xxs) -> 
         pp_list_list_any env any_xxs 
     | Ast_cocci.CONTEXT (any_befaft) -> 
         (match any_befaft with
         | Ast_cocci.NOTHING -> pr s
               
         | Ast_cocci.BEFORE xxs -> 
             pp_list_list_any env xxs;
             pr s;
         | Ast_cocci.AFTER xxs -> 
             pr s;
             pp_list_list_any env xxs;
         | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
             pp_list_list_any env xxs;
             pr s;
             pp_list_list_any env yys;
             
         )
     | Ast_cocci.PLUS -> raise Impossible




   (**************************************************************************************)

   and pp_expression x = match x with
    | Constant (String s),        typ, is     -> is +> List.iter pr_elem
    | Ident (c),         typ,[i]     -> pr_elem i
   (* only a String can have multiple ii *)
    | Constant (c),         typ,[i]     -> pr_elem i 
    | FunCall  (e, es),     typ,[i1;i2] -> 
        pp_expression e; pr_elem i1; 
        es +> List.iter (fun (e, opt) -> 
          (match opt with
          | [] -> ()
          | [i] -> pr_elem i
          | x -> error_cant_have x
          );
          (match e with
          | Left e -> pp_expression e
          | Right (returnType, (sto, iisto)) -> 
              assert (List.length iisto <= 1);
              iisto +> List.iter pr_elem;
              pp_type_with_ident None None returnType
          );
            );
            
        pr_elem i2;
        
    | CondExpr (e1, e2, e3),    typ,[i1;i2]    -> pp_expression e1; pr_elem i1; pp_expression e2; pr_elem i2; pp_expression e3
    | Sequence (e1, e2),          typ,[i]  -> pp_expression e1; pr_elem i; pp_expression e2
    | Assignment (e1, op, e2),    typ,[i]  -> 
        pp_expression e1; 
        pr_elem i; 
        pp_expression e2
        
    | Postfix  (e, op),    typ,[i] -> pp_expression e; pr_elem i;
    | Infix    (e, op),    typ,[i] -> pr_elem i; pp_expression e;
    | Unary    (e, op),    typ,[i] -> pr_elem i; pp_expression e
    | Binary   (e1, op, e2),    typ,[i] -> pp_expression e1;   pr_elem i; pp_expression e2
        
    | ArrayAccess    (e1, e2),     typ,[i1;i2] -> pp_expression e1; pr_elem i1; pp_expression e2; pr_elem i2
    | RecordAccess   (e, s),       typ,[i1;i2] -> pp_expression e; pr_elem i1; pr_elem i2
    | RecordPtAccess (e, s),       typ,[i1;i2] -> pp_expression e; pr_elem i1; pr_elem i2

    | SizeOfExpr  (e),     typ,[i] -> pr_elem i; pp_expression e
    | SizeOfType  (t),     typ,[i1;i2;i3] -> pr_elem i1; pr_elem i2; pp_type_with_ident None None t; pr_elem i3
    | Cast    (t, e),      typ,[i1;i2] -> pr_elem i1; pp_type_with_ident None None t; pr_elem i2; pp_expression e

    | StatementExpr ((declxs_statxs), [ii1;ii2]),  typ,[i1;i2] -> 
        pr_elem i1;
        pr_elem ii1;
        declxs_statxs +> List.iter (function Left decl -> pp_decl decl | Right stat -> pp_statement stat);
        pr_elem ii2;
        pr_elem i2;
    | Constructor, typ,[] -> pr "<<constructur_or_strange_stuff>>"
    | NoExpr, typ,[] -> ()

    | ParenExpr (e), typ,[i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2;

    | MacroCall  (es),     typ,[i1;i2;i3] -> 

        let rec pp_action = function 
          | (ActMisc ii) -> ii +> List.iter pr_elem
          | (ActJump jump) -> 
              (match jump with
              | (Goto s), [i1;i2]               -> pr_elem i1; pr_elem i2; 
              | ((Continue|Break|Return)), [i1] -> pr_elem i1; 
              | (ReturnExpr e), [i1] -> pr_elem i1; pp_expression e; 
              | x -> error_cant_have x
              )
          | (ActExpr e) -> pp_expression e
          | (ActExpr2 (e, iptvirg, action)) -> pp_expression e; pr_elem iptvirg; pp_action action
          | (ActTodo) -> pr "<<actiontodo>"
        in

        pr_elem i1;
        pr_elem i2;
        es +> List.iter (fun (e, opt) -> 
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          (match e with
          | Left3 e -> pp_expression e
          | Middle3 (returnType, (sto, iisto)) -> 
              assert (List.length iisto <= 1);
              iisto +> List.iter pr_elem;
              pp_type_with_ident None None returnType
          | Right3  action -> pp_action action
          );
        );
            
        pr_elem i3;

    | MacroCall2  (arg),     typ,[i1;i2;i3] -> 
        pr_elem i1;
        pr_elem i2;
        (match arg with
        | Left e -> pp_expression e
        | Right xs -> 
            xs +> List.iter (function stat -> pp_statement stat);

        );
        pr_elem i3;

    | x  -> error_cant_have x



   (* ---------------------- *)
   and pp_statement = function
     | Labeled (Label (s, st)), [i1;i2] -> pr_elem i1; pr_elem i2; pp_statement st
     | Labeled (Case  (e, st)), [i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2; pp_statement st
     | Labeled (CaseRange  (e, e2, st)), _ -> pr "<<label>>\n";
     | Labeled (Default st), [i1;i2] -> pr_elem i1; pr_elem i2; pp_statement st
     | Compound (declxs_statxs), [i1;i2] -> 
         pr_elem i1; 
         (* old: when no mix decl/stat
         declxs +> List.iter pp_decl;
         statxs +> List.iter pp_statement;
         *)
         declxs_statxs +> List.iter (function Left decl -> pp_decl decl | Right stat -> pp_statement stat);
         pr_elem i2;
         
     | ExprStatement (None), [i] -> pr_elem i;
     | ExprStatement (None), [] -> ()
     | ExprStatement (Some e), [i] -> pp_expression e; pr_elem i
     | ExprStatement (Some e), [] -> pp_expression e; (* the last ExprStatement of a for does not have a trailing ';' hence the [] for ii  *)
     | Selection  (If (e, st1, st2)), i1::i2::i3::is -> 
         pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3; pp_statement st1; 
         (match (st2, is) with
         | ((ExprStatement None, []), _)  -> ()
         | st2, [i4] -> pr_elem i4; pp_statement st2
         | x -> error_cant_have x
         )
     | Selection  (Switch (e, st)), [i1;i2;i3] -> 
         pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3; pp_statement st
     | Iteration  (While (e, st)), [i1;i2;i3] -> 
         pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3; pp_statement st
     | Iteration  (DoWhile (st, e)), [i1;i2;i3;i4;i5] -> 
         pr_elem i1; pp_statement st; pr_elem i2; pr_elem i3; pp_expression e; pr_elem i4; pr_elem i5
         
     | Iteration  (For ((e1opt,il1), (e2opt,il2), (e3opt, il3), st)), [i1;i2;i3] -> 
         pr_elem i1;
         pr_elem i2;
         pp_statement (ExprStatement e1opt, il1);
         pp_statement (ExprStatement e2opt, il2);
         pp_statement (ExprStatement e3opt, il3);
         pr_elem i3;
         pp_statement st
          
     | Jump (Goto s), [i1;i2;i3]               -> pr_elem i1; pr_elem i2; pr_elem i3;
     | Jump ((Continue|Break|Return)), [i1;i2] -> pr_elem i1; pr_elem i2;
     | Jump (ReturnExpr e), [i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2
     | (Asm, []) -> pr "<<asm_or_strange_stuff>>";
     | x -> error_cant_have x




   (* ---------------------- *)
   and (pp_type_with_ident: (string * info) option -> (storage * il) option -> fullType -> unit) = fun ident sto ((qu, iiqu), (ty, iity)) -> 
     pp_base_type ((qu, iiqu), (ty, iity))  sto;
     pp_type_with_ident_rest ident ((qu, iiqu), (ty, iity))


   and (pp_base_type: fullType -> (storage * il) option -> unit) = fun (qu, (ty, iity)) sto -> 
     let get_sto sto = match sto with None -> [] | Some (s, iis) -> (*assert (List.length iis = 1);*) iis  in
     let print_sto_qu (sto, (qu, iiqu)) = 
       let all_ii = get_sto sto ++ iiqu in
       all_ii +> List.sort (fun i1 i2 -> compare (fst i1).charpos (fst i2).charpos) +> List.iter pr_elem;
       
     in
     let print_sto_qu_ty (sto, (qu, iiqu), iity) = 
       let all_ii = get_sto sto ++ iiqu ++ iity in
       let all_ii2 = all_ii +> List.sort (fun i1 i2 -> compare (fst i1).charpos (fst i2).charpos) in
       if all_ii <> all_ii2 
       then begin pr2 "STRANGEORDER"; all_ii2 +> List.iter pr_elem end
       else all_ii2 +> List.iter pr_elem
     in

     match ty, iity with
     | (Pointer t, [i])                           -> pp_base_type t sto
     | (ParenType t, _)                           -> pp_base_type t sto
     | (Array (eopt, t), [i1;i2])                 -> pp_base_type t sto
     | (FunctionType (returnt, paramst), [i1;i2]) -> pp_base_type returnt sto





     | (StructUnion (sopt, (su, fields)),iis) -> 
         print_sto_qu (sto, qu);

         (match sopt,iis with
         | Some s , [i1;i2;i3;i4] -> 
             pr_elem i1; pr_elem i2; pr_elem i3; 
         | None, [i1;i2;i3] -> 
             pr_elem i1; pr_elem i2; 
         | x -> error_cant_have x
         );

         fields +> List.iter (fun (FieldDeclList onefield_multivars, iipttvirg) -> 

           (match onefield_multivars with
           | x::xs -> 
               (* handling the first var. Special case, with the first var, we print the whole type *)
               (match x with
               | Simple (sopt, typ, iis), iivirg -> 
                   assert (List.length iivirg = 0); (* first var cant have a preceding ',' *)
                   let identinfo = (match sopt, iis with None,_ -> None | (Some s, [iis]) -> Some (s, iis) | x -> error_cant_have x) in
                   pp_type_with_ident identinfo None typ;

               | BitField (sopt, typ, expr, ii), iivirg -> 
                   assert (List.length iivirg = 0); (* first var cant have a preceding ',' *)
                   (match sopt, ii with
                   | (None , [idot]) -> 
                       pp_type_with_ident None None typ;
                       pr_elem idot;
                       pp_expression expr
                   | (Some s, [is;idot]) -> 
                       pp_type_with_ident (Some (s, is)) None typ;
                       pr_elem idot;
                       pp_expression expr
                   | x -> error_cant_have x
                   )
                   
               );
                 
               (* for other vars *)
               xs +> List.iter (function
               | Simple (sopt, typ, iis), iivirg -> 
                   iivirg +> List.iter pr_elem;
                   let identinfo = (match sopt, iis with None,_ -> None | (Some s, [iis]) -> Some (s, iis) | x -> error_cant_have x) in
                   pp_type_with_ident_rest identinfo typ;

               | BitField (sopt, typ, expr, ii), iivirg -> 
                   iivirg +> List.iter pr_elem;
                   (match sopt, ii with
                   | (Some s, [is;idot]) -> 
                       pp_type_with_ident_rest (Some (s, is)) typ;
                       pr_elem idot;
                       pp_expression expr
                   | x -> error_cant_have x
                   );
                   
               );

               assert (List.length iipttvirg = 1);
               iipttvirg +> List.iter pr_elem;
          | x -> error_cant_have x
              );
         );

         (match sopt,iis with
         | Some s , [i1;i2;i3;i4] -> 
             pr_elem i4
         | None, [i1;i2;i3] -> 
             pr_elem i3; 
         | x -> error_cant_have x
         );



     | (Enum  (sopt, enumt), iis) -> 
         print_sto_qu (sto, qu);

         (match sopt, iis with
         | (Some s, ([i1;i2;i3;i4]|[i1;i2;i3;i4;_])) -> 
             pr_elem i1; pr_elem i2; pr_elem i3;
         | (None, ([i1;i2;i3]|[i1;i2;i3;_])) -> 
             pr_elem i1; pr_elem i2
         | x -> error_cant_have x
         );

         enumt +> List.iter (fun (((s, is), eopt)    , iicomma) -> 
           assert (List.length iicomma <= 1);
           iicomma +> List.iter pr_elem;
           pr_elem is;
           
             );

         (match sopt, iis with
         | (Some s, [i1;i2;i3;i4]) -> 
             pr_elem i4
         | (Some s, [i1;i2;i3;i4;i5]) -> 
             (* trailing comma *)
             pr_elem i5; pr_elem i4
         | (None, [i1;i2;i3]) -> 
             pr_elem i3
         | (None, [i1;i2;i3;i4]) -> 
             (* trailing comma *)
             pr_elem i4; pr_elem i3
         | x -> error_cant_have x
         );


     | (BaseType _, iis) -> 
         print_sto_qu_ty (sto, qu, iis);

     | (StructUnionName (s, structunion), iis) -> 
         assert (List.length iis = 2);
         print_sto_qu_ty (sto, qu, iis);

     | (EnumName  s, iis) -> 
         assert (List.length iis = 2);
         print_sto_qu_ty (sto, qu, iis);

     | (TypeName (s), iis) -> 
         assert (List.length iis = 1);  
         print_sto_qu_ty (sto, qu, iis);

     | x -> error_cant_have x


   (* used because of DeclList, in    int i,*j[23];  we dont print anymore the int before *j *) 
   and (pp_type_with_ident_rest: (string * info) option -> fullType -> unit) = fun ident (((qu, iiqu), (ty, iity)) as fullt) -> 
     let print_ident ident = match ident with None -> () | Some (s, iis) -> pr_elem iis in

     match ty, iity with
     (* the work is to do in base_type !! *)
     | (BaseType _, iis)                       -> print_ident ident
     | (Enum  (sopt, enumt), iis)              -> print_ident ident
     | (StructUnion (sopt, (_, fields)),iis)   -> print_ident ident
     | (StructUnionName (s, structunion), iis) -> print_ident ident
     | (EnumName  s, iis)                      -> print_ident ident
     | (TypeName (s), iis)                     -> print_ident ident



     | (Pointer t, [i]) ->  
      (* subtil:  void ( *done)(int i)   is a Pointer (FunctionType (return=void, params=int i) *)
(*WRONG I THINK, use left & right function *)
         pr_elem i; (* bug: pp_type_with_ident_rest None t;      print_ident ident *)
         iiqu +> List.iter pr_elem; (* le const est forcement apres le '*' *)
         pp_type_with_ident_rest ident t;

     (* ugly special case ... todo? maybe sufficient in practice *)       
     | (ParenType (q1, (Pointer (q2, (FunctionType t, ii3))   , [ipointer])  ), [i1;i2]) ->  
         pp_type_left (q2, (FunctionType t, ii3));
         pr_elem i1;
         pr_elem ipointer;
         print_ident ident;
         pr_elem i2;
         pp_type_right (q2, (FunctionType t, ii3));

     | (ParenType t, [i1;i2]) ->  
         pr2 "PB PARENTYPE ZARB, I forget about the ()";
         pp_type_with_ident_rest ident t;
         

     | (Array (eopt, t), [i1;i2]) -> 

          pp_type_left fullt;

          iiqu +> List.iter pr_elem;
          print_ident ident;

          pp_type_right fullt;


     | (FunctionType (returnt, paramst), [i1;i2]) -> 
         pp_type_left fullt;

         iiqu +> List.iter pr_elem;
         print_ident ident;

         pp_type_right fullt;

     | x -> error_cant_have x
     

   and (pp_type_left: fullType -> unit) = fun ((qu, iiqu), (ty, iity)) -> 
     match ty, iity with
     | (Pointer t, [i]) ->  pr_elem i; pp_type_left t

     | (Array (eopt, t), [i1;i2]) -> pp_type_left t
     | (FunctionType (returnt, paramst), [i1;i2]) -> pp_type_left returnt

     | (ParenType t, _) ->  raise Todo


     | (BaseType _, iis)    -> ()    
     | (Enum  (sopt, enumt), iis) -> ()    
     | (StructUnion (sopt, (_, fields)),iis)  -> ()    
     | (StructUnionName (s, structunion), iis) -> ()    
     | (EnumName  s, iis) -> ()    
     | (TypeName (s), iis) -> ()
     | x -> error_cant_have x


   and (pp_type_right: fullType -> unit) = fun ((qu, iiqu), (ty, iity)) -> 
     match ty, iity with
     | (Pointer t, [i]) ->  pp_type_right t
     | (Array (eopt, t), [i1;i2]) -> 
          pr_elem i1;
          (match eopt with
          | None -> ()
          | Some e -> pp_expression e
          );
          pr_elem i2;
          pp_type_right t

     | (ParenType t, _) ->  raise Todo
     | (FunctionType (returnt, paramst), [i1;i2]) -> 
         pr_elem i1;
         (match paramst with
         | Classic (ts, b, ii) -> 
             ts +> List.iter (fun ((b, sopt, t, (ii2, ii4)),ii3) -> 
               assert ((List.length ii3) <= 1);
               ii3 +> List.iter pr_elem;

               (match sopt with
               | None -> assert (List.length ii4 = 0);
                   pp_type_with_ident None None t
               | Some s -> 
                   assert (List.length ii4 = 1); 
                   pp_type_with_ident (Some (s, List.hd ii4)) None t;
               );
               );
             (* normally ii represent the ",..."  but it is also abused with the f(void) case *)
             ii +> List.iter pr_elem;
         );
         pr_elem i2;
         



     | (BaseType _, iis)        -> ()    
     | (Enum  (sopt, enumt), iis) -> ()    
     | (StructUnion (sopt, (_, fields)),iis)-> ()      
     | (StructUnionName (s, structunion), iis) -> ()    
     | (EnumName  s, iis) -> ()    
     | (TypeName (s), iis) -> ()
     | x -> error_cant_have x

   (* ---------------------- *)
   and pp_decl = function
    | DeclList (((var, returnType, storage),[])::xs,                       (iisto, iivirg)) -> 

       (* old: iisto +> List.iter pr_elem; *)

       (* handling the first var. Special case, with the first var, we print the whole type *)
       (match var with
        | Some (s, ini,  iis) -> 
            pp_type_with_ident (Some (s, iis)) (Some (storage, iisto)) returnType;
               (match ini with
                | Some (init, iinit) -> pr_elem iinit; pp_init init
                | None -> ()
               );
        | None -> pp_type_with_ident None None returnType
       );

       (* for other vars, we just call pp_type_with_ident_rest. *)
       xs +> List.iter (function
         | ((Some (s, ini, iis), returnType, storage2), iivirg) -> 
             assert (storage2 = storage);
             iivirg +> List.iter pr_elem;
             pp_type_with_ident_rest (Some (s, iis)) returnType;
               (match ini with
                | Some (init, iinit) -> pr_elem iinit; pp_init init
                | None -> ()
               );


         | x -> error_cant_have x
        );

       pr_elem iivirg;

    | x -> error_cant_have x
  

   (* ---------------------- *)
   and pp_init = fun (init, iinit) -> 
     match init, iinit with
     | InitExpr e, [] -> pp_expression e;
     | InitList xs, [i1;i2] -> 
         pr_elem i1;
         xs +> List.iter (fun (x, ii) -> 
           assert (List.length ii <= 1);
           ii +> List.iter pr_elem;
           pp_init x
             );

         pr_elem i2;

     | InitList xs, [i1;i2;i3] -> (* case where have the optional trailing ','  *)
         pr_elem i1;
         xs +> List.iter (fun (x, ii) -> 
           assert (List.length ii <= 1);
           ii +> List.iter pr_elem;
           pp_init x
             );
         pr_elem i2;
         pr_elem i3;
         
     | InitGcc (string, initialiser), [i1;i2;i3] -> (* .label: *)
         pr_elem i1;
         pr_elem i2; 
         pr_elem i3;
         pp_init initialiser
     | InitGcc (string, initialiser), [i1;i2] -> (* label:   in oldgcc *)
         pr_elem i1; pr_elem i2; pp_init initialiser
     | InitGccIndex (expression, initialiser), [i1;i2;i3] -> 
         pr_elem i1; pp_expression expression; pr_elem i2; pr_elem i3;
         pp_init initialiser
     | InitGccRange (expression, expression2, initialiser), _ -> pr "<<ini>>"


     | x -> error_cant_have x


  (**************************************************************************************)

  (* assert: normally there is only CONTEXT NOTHING tokens in any *)
  and pp_any env x = match x with
  | Ast_cocci.FullTypeTag         e -> raise Todo
  | Ast_cocci.BaseTypeTag         e -> raise Todo
  | Ast_cocci.StructUnionTag      e -> raise Todo
  | Ast_cocci.SignTag             e -> raise Todo
  | Ast_cocci.IdentTag            e -> pp_cocci_ident env e
  | Ast_cocci.ExpressionTag       e -> pp_cocci_expr env e
  | Ast_cocci.ConstantTag         e -> raise Todo
  | Ast_cocci.UnaryOpTag          e -> raise Todo
  | Ast_cocci.AssignOpTag         e -> raise Todo
  | Ast_cocci.FixOpTag            e -> raise Todo
  | Ast_cocci.BinaryOpTag         e -> raise Todo
  | Ast_cocci.ArithOpTag          e -> raise Todo
  | Ast_cocci.LogicalOpTag        e -> raise Todo
  | Ast_cocci.DeclarationTag      e -> raise Todo
  | Ast_cocci.ParameterTypeDefTag e -> raise Todo
  | Ast_cocci.StorageTag          e -> raise Todo
  | Ast_cocci.Rule_elemTag        e -> pp_cocci_rule env e
  | Ast_cocci.StatementTag        e -> pp_cocci_statement env e
  | Ast_cocci.ConstVolTag         e -> raise Todo
  | Ast_cocci.Token               e -> pr e
  | Ast_cocci.Code                e -> pp_cocci_top_level env e
  | Ast_cocci.ExprDotsTag         e -> raise Todo
  | Ast_cocci.ParamDotsTag        e -> raise Todo
  | Ast_cocci.StmtDotsTag         e -> raise Todo
  | Ast_cocci.TypeCTag            e -> raise Todo
  | Ast_cocci.ParamTag            e -> pp_cocci_param env e 

  and pp_list_list_any env xxs =
     match xxs with
     | xs::xxs -> 
         xs +> List.iter (fun any -> 
           pp_any env any
             );
         xxs +> List.iter (fun xs -> 
           pr "\n"; 
           pr !_current_tabbing;
           xs +> List.iter (fun any -> 
               pp_any env any
             ); 
          )
     | [] -> ()

   (* ---------------------- *)
  and (pp_cocci_top_level: Ast_c.metavars_binding -> Ast_cocci.top_level -> unit) = fun env x -> match Ast_cocci.unwrap x with
  | Ast_cocci.CODE stmt_dots -> List.iter (pp_cocci_statement env) (Ast_cocci.undots stmt_dots)
  | _ -> raise Todo

   (* ---------------------- *)
  and (pp_cocci_statement: Ast_c.metavars_binding -> Ast_cocci.statement -> unit) = fun env x -> match Ast_cocci.unwrap x with
  | Ast_cocci.Atomic rule_elem -> pp_cocci_rule_elem env rule_elem
  | _ -> raise Todo


  and (pp_cocci_rule_elem: Ast_c.metavars_binding -> Ast_cocci.rule_elem -> unit) = fun env x -> match Ast_cocci.unwrap x with
  | Ast_cocci.ExprStatement (e, ptvirg) -> 
      pp_cocci_expr env e;
      pr (term ptvirg);
  | Ast_cocci.Exp e -> pp_cocci_expr env e;
  | _ -> raise Todo



   (* ---------------------- *)
  and (pp_cocci_expr: Ast_c.metavars_binding -> Ast_cocci.expression -> unit) = fun env x -> match Ast_cocci.unwrap x with
  | Ast_cocci.Ident id -> pp_cocci_ident env id
  | Ast_cocci.MetaExpr (s,_typeTODO) -> 
      let v = List.assoc (term s) env in
      (match v with 
      | Ast_c.MetaExprVal exp -> 
          pp_expression exp
      | _ -> raise Impossible
      )

  | Ast_cocci.Constant c ->
      (match term c with
	Ast_cocci.Int (s) -> pr s
      | Ast_cocci.String (s) -> pr ("\"" ^ s ^ "\"")
      | Ast_cocci.Char (s) -> pr s
      | Ast_cocci.Float (s) -> pr s)

  | Ast_cocci.FunCall (e, lp, es, rp) -> 
      pp_cocci_expr env e; pr (term lp);
      List.iter (pp_cocci_expr env) (Ast_cocci.undots es); pr (term rp)

  | Ast_cocci.EComma com -> pr (term com); pr " " (* pretty printing: add a space after ',' *)

  | Ast_cocci.DisjExpr _ -> raise Impossible
  | Ast_cocci.Edots _ -> raise Impossible
  | _ -> raise Todo
    

   (* ---------------------- *)
  and pp_cocci_rule env x = match Ast_cocci.unwrap x with
  | Ast_cocci.SeqStart brace -> pr (term brace)
  | Ast_cocci.SeqEnd   brace -> pr (term brace)

  | Ast_cocci.ExprStatement (e, sem) -> pp_cocci_expr env e; pr (term sem)



  | Ast_cocci.IfHeader (iff, lp, e, rp) -> pr (term iff); pr (term lp); pp_cocci_expr env e; pr (term rp)
  | Ast_cocci.Else     str -> pr (term str)

  | _ -> raise Todo
   (* ---------------------- *)
  and pp_cocci_param env x = match Ast_cocci.unwrap x with
  | Ast_cocci.Param (id, fullt) -> pp_cocci_fullType env fullt; pr " "; pp_cocci_ident env id
  | Ast_cocci.PComma (comma) -> pr (term comma); pr " "
  | x -> raise Todo

  (* ---------------------- *)
  and pp_cocci_fullType env x = match Ast_cocci.unwrap x with
  | Ast_cocci.Type(cv,ty) ->
      (match cv with
      | None -> ()
      | Some x -> raise Todo
      );
      pp_cocci_type env ty
        
  | Ast_cocci.OptType(ty)  | Ast_cocci.UniqueType(ty)  | Ast_cocci.MultiType(ty) -> 
      raise Impossible (* really ? *)



  and pp_cocci_type env x = match Ast_cocci.unwrap x with
  | Ast_cocci.BaseType(ty,sgn) -> 
      (match sgn with
      | Some x -> 
          (match term x with
          | Ast_cocci.Signed -> pr "signed "
          | Ast_cocci.Unsigned -> pr "unsigned "
          )
      | None -> ()
      );
      (match term ty with
      | Ast_cocci.VoidType -> pr "void"
      | Ast_cocci.CharType -> pr "char"
      | Ast_cocci.ShortType -> pr "short"
      | Ast_cocci.IntType -> pr "int"
      | Ast_cocci.DoubleType -> pr "double"
      | Ast_cocci.FloatType -> pr "float"
      | Ast_cocci.LongType -> pr "long"
      )
      
  | Ast_cocci.Pointer(ty,star) -> pp_cocci_fullType env ty; pr (term star)
  | Ast_cocci.Array(ty,lb,size,rb) -> raise Todo
  | Ast_cocci.StructUnionName(name,kind) -> 
       (match term kind with
         Ast_cocci.Struct -> pr "struct "
       | Ast_cocci.Union -> pr "union "
       );
      pr (term name)

  | Ast_cocci.TypeName(name)-> pr (term name)
  | Ast_cocci.MetaType(name)-> raise Todo


   (* ---------------------- *)
  and pp_cocci_ident env x = match Ast_cocci.unwrap x with
  | Ast_cocci.Id s -> pr (term s)
  | Ast_cocci.MetaId s -> 
      (try 
       let v = List.assoc (term s) env in
        (match v with 
       | Ast_c.MetaIdVal id -> 
          pr id
       | _ -> raise Impossible
       )
      with Not_found -> failwith ("unparse: I have not found a value in the environment for: " ^ term s)
      )
  | x -> raise Todo



   in

  (**************************************************************************************)
  (* ---------------------- *)
  (* start point *)
  (* ---------------------- *)

   x +> List.iter (fun ((e, ppmethod)) -> 
   match ppmethod with
   | PPviatok toks -> 
       (match e with
       | FinalDef (ii,_ANNOT) -> pr_elem ({ii with str = ""},Ast_c.dumbAnnot) (* todo: less: assert that FinalDef is the last one in the list *)
       | e -> toks +> List.iter (fun x -> pr_elem x)
       )

   | PPnormal -> 
     (match e with
     | Declaration decl -> pp_decl decl
     | Definition (s, (returnt, paramst, b, (iib,[iifunc1;iifunc2])), sto, (declxs_statxs), (is, isto, [i1;i2])) -> 
         pp_type_with_ident None (Some (sto, isto)) returnt;
         pr_elem is;
         pr_elem iifunc1;
         paramst +> List.iter (fun ((bool, s, fullt, (iib, (iis:info))), iicomma) -> 
             assert (List.length iicomma <= 1);
             assert (List.length iib <= 1);
             (* assert (List.length iis = 1);*)
             iicomma +> List.iter pr_elem;
             iib +> List.iter pr_elem;
             pp_type_with_ident (Some (s, iis)) None fullt;
         );
             

         (* normally ii represent the ",..."  but it is also abused with the f(void) case *)
         (* assert (List.length iib <= 2);*)
         iib +> List.iter pr_elem;

         pr_elem iifunc2;
         pr_elem i1; 
         declxs_statxs +> List.iter (function Left decl -> pp_decl decl | Right stat -> pp_statement stat);
         pr_elem i2;

     | EmptyDef ii -> 
           ii +> List.iter pr_elem

     | SpecialDeclMacro (s, es,   [i1;i2;i3;i4]) -> 
         pr_elem i1;
         pr_elem i2;
         es +> List.iter (fun (e, opt) -> 
           assert (List.length opt <= 1);
           opt +> List.iter pr_elem;
          (match e with
          | Left e -> pp_expression e
          | Right (returnType, (sto, iisto)) -> 
              assert (List.length iisto <= 1);
              iisto +> List.iter pr_elem;
              pp_type_with_ident None None returnType
          );
         );
         pr_elem i3;
         pr_elem i4;


     | NotParsedCorrectly ii -> 
         assert (List.length ii >= 1);
         ii +> List.iter pr_elem 

     | FinalDef (ii,_ANNOT) -> pr_elem ({ii with str = ""},Ast_c.dumbAnnot)

     | x -> error_cant_have x
     )   
   );
 );
