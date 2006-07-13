open Common open Commonop

type ppmethod = PPviatok of Ast_c.info list | PPnormal

module PPC = Pretty_print_c

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
open Parser_c

let term ((s,_,_) : 'a Ast_cocci.mcode) = s

let pp_program file x = 

 with_open_outfile "/tmp/output.c" (fun (pr,chan) -> 
   let pr s = pr s; flush chan in

   let _table = Common.full_charpos_to_pos file in

   (* note: that not exactly same tokens as in parsing, cos in parsing there is
      some transformation of tokens such as TIdent in Typedef, TIdent in 
      TString, but for what we are interested here, it is not really important
    *)
   let toks = (Parse_c.tokens file) in 
   let toks = ref (toks +> List.map (fun tok -> 
     (tok, Parse_c.info_from_token tok))) 
   in
   
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
      let (before, after) = !toks +> span (fun (tok, (info,_ANNOT)) -> 
        info.charpos < elem.charpos)   
      in
      toks := after;

      let (commentsbefore, passed) = 
        before +> List.rev +> span (fun (tok, tokinfo) -> 
          (match tok with
          | TComment i -> true
          | TCommentSpace i -> true
          | TCommentCpp i -> true
          | TCommentAttrOrMacro i -> true
          | _ -> false
          ))
      in
      let (commentsbefore, passed) = (List.rev commentsbefore, List.rev passed) 
      in

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






 (****************************************************************************)

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
  and (pp_cocci_top_level: 
    Ast_c.metavars_binding -> Ast_cocci.top_level -> unit) = 
   fun env x -> match Ast_cocci.unwrap x with
   | Ast_cocci.CODE stmt_dots -> 
       List.iter (pp_cocci_statement env) (Ast_cocci.undots stmt_dots)
   | _ -> raise Todo

   (* ---------------------- *)
  and (pp_cocci_statement: 
    Ast_c.metavars_binding -> Ast_cocci.statement -> unit) = 
   fun env x -> match Ast_cocci.unwrap x with
   | Ast_cocci.Atomic rule_elem -> pp_cocci_rule_elem env rule_elem
   | _ -> raise Todo


  and (pp_cocci_rule_elem: 
    Ast_c.metavars_binding -> Ast_cocci.rule_elem -> unit) = 
   fun env x -> match Ast_cocci.unwrap x with
   | Ast_cocci.ExprStatement (e, ptvirg) -> 
       pp_cocci_expr env e;
       pr (term ptvirg);
   | Ast_cocci.Exp e -> pp_cocci_expr env e;
   | _ -> raise Todo



   (* ---------------------- *)
  and (pp_cocci_expr: Ast_c.metavars_binding -> Ast_cocci.expression -> unit) = 
   fun env x -> match Ast_cocci.unwrap x with
   | Ast_cocci.Ident id -> pp_cocci_ident env id
   | Ast_cocci.MetaExpr (s,_typeTODO) -> 
       let v = List.assoc (term s) env in
       (match v with 
       | Ast_c.MetaExprVal exp -> 
          PPC.pp_expression_gen pr_elem  exp
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

   (* pretty printing: add a space after ',' *)
   | Ast_cocci.EComma com -> pr (term com); pr " "

   | Ast_cocci.DisjExpr _ -> raise Impossible
   | Ast_cocci.Edots _ -> raise Impossible
   | _ -> raise Todo
    

   (* ---------------------- *)
   and pp_cocci_rule env x = match Ast_cocci.unwrap x with
   | Ast_cocci.SeqStart brace -> pr (term brace)
   | Ast_cocci.SeqEnd   brace -> pr (term brace)
         
   | Ast_cocci.ExprStatement (e, sem) -> pp_cocci_expr env e; pr (term sem)
         
         
         
   | Ast_cocci.IfHeader (iff, lp, e, rp) -> 
       pr (term iff); pr (term lp); pp_cocci_expr env e; pr (term rp)
   | Ast_cocci.Else     str -> pr (term str)
         
   | _ -> raise Todo
   (* ---------------------- *)
  and pp_cocci_param env x = match Ast_cocci.unwrap x with
  | Ast_cocci.Param (id, fullt) -> 
      pp_cocci_fullType env fullt; pr " "; pp_cocci_ident env id
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
        
  | Ast_cocci.OptType(ty)  | Ast_cocci.UniqueType(ty)  | Ast_cocci.MultiType(ty)
    -> raise Impossible (* really ? *)



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
      with Not_found -> 
        failwith ("unparse: I have not found a value in the environment for: " ^
                  term s)
      )
  | x -> raise Todo



   in

  (***************************************************************************)
  (* ---------------------- *)
  (* start point *)
  (* ---------------------- *)

   x +> List.iter (fun ((e, ppmethod)) -> 
   match ppmethod with
   | PPviatok toks -> 
       (match e with
       | FinalDef (ii,_ANNOT) -> 
           (* todo: less: assert that FinalDef is the last one in the list *)
           pr_elem ({ii with str = ""},Ast_c.dumbAnnot) 
       | e -> toks +> List.iter (fun x -> pr_elem x)
       )

   | PPnormal -> 
     (match e with
     | Declaration decl -> PPC.pp_decl_gen pr_elem decl
     | Definition (s, (returnt, paramst, b, (iib,[iifunc1;iifunc2])), sto, 
                   (declxs_statxs), (is, isto, [i1;i2])) -> 
         PPC.pp_type_with_ident_gen pr_elem None (Some (sto, isto)) returnt;
         pr_elem is;
         pr_elem iifunc1;
         paramst +> List.iter (fun ((bool, s, fullt, (iib, (iis:info))), iicomma) -> 
             assert (List.length iicomma <= 1);
             assert (List.length iib <= 1);
             (* assert (List.length iis = 1);*)
             iicomma +> List.iter pr_elem;
             iib +> List.iter pr_elem;
             PPC.pp_type_with_ident_gen pr_elem (Some (s, iis)) None fullt;
         );
             

         (* normally ii represent the ",..."  but it is also abused with the f(void) case *)
         (* assert (List.length iib <= 2);*)
         iib +> List.iter pr_elem;

         pr_elem iifunc2;
         pr_elem i1; 
         declxs_statxs +> List.iter (function 
           | Left decl -> PPC.pp_decl_gen pr_elem decl 
           | Right stat -> PPC.pp_statement_gen pr_elem stat);
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
          | Left e -> PPC.pp_expression_gen pr_elem e
          | Right (returnType, (sto, iisto)) -> 
              assert (List.length iisto <= 1);
              iisto +> List.iter pr_elem;
              PPC.pp_type_with_ident_gen pr_elem None None returnType
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
