open Common open Commonop

(******************************************************************************)
(* It is not the same thing than Unparse_c. Unparse_c correctly insert
   spaces, comments, mcode, and so on, but that means that Unparse_c
   cannot pretty print an expression independently, and that Unparse_c
   need the original file to be able to unparse. So for debugging
   purpose Unparse_c is not practical, hence this module. *)
(******************************************************************************)

open Ast_c

type pr_elem_func = Ast_c.info -> unit


(* ---------------------- *)
let rec pp_expression_gen pr_elem = 
  (* subtil: dont try to shorten the def of pp_statement by omitting e,
     otherwise get infinite funcall and huge memory consumption *)
  let pp_statement e = pp_statement_gen pr_elem e in
  let rec pp_expression = function
  | Ident (c),         typ,[i]     -> pr_elem i
  | Constant (String s),        typ, is     -> is +> List.iter pr_elem
  (* only a String can have multiple ii *)
  | Constant (c),         typ,[i]     -> pr_elem i 
  | FunCall  (e, es),     typ,[i1;i2] -> 
      pp_expression e; pr_elem i1; 
      es +> List.iter (fun (e, opt) -> 
        (match opt with
        | [] -> ()
        | [i] -> pr_elem i
        | x -> raise Impossible
        );
        (match e with
        | Left e -> pp_expression e
        | Right (returnType, (sto, iisto)) -> 
            assert (List.length iisto <= 1);
            iisto +> List.iter pr_elem;
            pp_type_with_ident_gen pr_elem   None None returnType
        );
                      );
      
      pr_elem i2;
      
  | CondExpr (e1, e2, e3),    typ,[i1;i2]    -> 
      pp_expression e1; pr_elem i1; do_option pp_expression e2; pr_elem i2; 
      pp_expression e3
  | Sequence (e1, e2),          typ,[i]  -> 
      pp_expression e1; pr_elem i; pp_expression e2
  | Assignment (e1, op, e2),    typ,[i]  -> 
      pp_expression e1; 
      pr_elem i; 
      pp_expression e2
        
  | Postfix  (e, op),    typ,[i] -> pp_expression e; pr_elem i;
  | Infix    (e, op),    typ,[i] -> pr_elem i; pp_expression e;
  | Unary    (e, op),    typ,[i] -> pr_elem i; pp_expression e
  | Binary   (e1, op, e2),    typ,[i] -> 
      pp_expression e1;   pr_elem i; pp_expression e2
        
  | ArrayAccess    (e1, e2),   typ,[i1;i2] -> 
      pp_expression e1; pr_elem i1; pp_expression e2; pr_elem i2
  | RecordAccess   (e, s),     typ,[i1;i2] -> 
      pp_expression e; pr_elem i1; pr_elem i2
  | RecordPtAccess (e, s),     typ,[i1;i2] -> 
      pp_expression e; pr_elem i1; pr_elem i2

  | SizeOfExpr  (e),     typ,[i] -> pr_elem i; pp_expression e
  | SizeOfType  (t),     typ,[i1;i2;i3] -> 
      pr_elem i1; pr_elem i2; pp_type_with_ident_gen pr_elem  None None t; 
      pr_elem i3
  | Cast    (t, e),      typ,[i1;i2] -> 
      pr_elem i1; pp_type_with_ident_gen pr_elem None None t; pr_elem i2; 
      pp_expression e

  | StatementExpr (declxs_statxs, [ii1;ii2]),  typ,[i1;i2] -> 
      pr_elem i1;
      pr_elem ii1;
      declxs_statxs +> List.iter (function 
        | Left decl -> pp_decl_gen pr_elem decl 
        | Right stat -> pp_statement stat);
      pr_elem ii2;
      pr_elem i2;
  | Constructor, typ,[] -> pr "<<constructur_or_strange_stuff>>"

  | ParenExpr (e), typ,[i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2;

  | MacroCall  (es),     typ,[i1;i2;i3] -> 

      let rec pp_action = function 
        | (ActMisc ii) -> ii +> List.iter pr_elem
        | (ActJump jump) -> 
            (match jump with
            | (Goto s), [i1;i2]               -> pr_elem i1; pr_elem i2; 
            | ((Continue|Break|Return)), [i1] -> pr_elem i1; 
            | (ReturnExpr e), [i1] -> pr_elem i1; pp_expression e; 
            | x -> raise Impossible
            )
        | (ActExpr e) -> pp_expression e
        | (ActExpr2 (e, iptvirg, action)) -> 
            pp_expression e; pr_elem iptvirg; pp_action action
        | (ActTodo) -> pr "<<actiontodo>>"
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
            pp_type_with_ident_gen pr_elem None None returnType
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

  | x  -> raise Impossible
  in
  pp_expression



(* ---------------------- *)
and pp_statement_gen pr_elem = 
 let pp_expression e = pp_expression_gen pr_elem e in
 let rec pp_statement = function
  | Labeled (Label (s, st)), [i1;i2] -> pr_elem i1; pr_elem i2; pp_statement st
  | Labeled (Case  (e, st)), [i1;i2] -> 
      pr_elem i1; pp_expression e; pr_elem i2; pp_statement st
  | Labeled (CaseRange  (e, e2, st)), _ -> pr "<<label>>\n";
  | Labeled (Default st), [i1;i2] -> pr_elem i1; pr_elem i2; pp_statement st
  | Compound (declxs_statxs), [i1;i2] -> 
      pr_elem i1; 
      (* old: when no mix decl/stat
         declxs +> List.iter pp_decl;
         statxs +> List.iter pp_statement;
       *)
      declxs_statxs +> List.iter (function 
        | Left decl -> pp_decl_gen pr_elem decl 
        | Right stat -> pp_statement stat);
      pr_elem i2;
      
  | ExprStatement (None), [i] -> pr_elem i;
  | ExprStatement (None), [] -> ()
  | ExprStatement (Some e), [i] -> pp_expression e; pr_elem i
   (* the last ExprStatement of a for does not have a trailing ';' hence the
      [] for ii  *)
  | ExprStatement (Some e), [] -> pp_expression e; 
  | Selection  (If (e, st1, st2)), i1::i2::i3::is -> 
      pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3; pp_statement st1; 
      (match (st2, is) with
      | ((ExprStatement None, []), [])  -> ()
      | st2, [i4] -> pr_elem i4; pp_statement st2
      | x -> raise Impossible
      )
  | Selection  (Switch (e, st)), [i1;i2;i3] -> 
      pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3; pp_statement st
  | Iteration  (While (e, st)), [i1;i2;i3] -> 
      pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3; pp_statement st
  | Iteration  (DoWhile (st, e)), [i1;i2;i3;i4;i5] -> 
      pr_elem i1; pp_statement st; pr_elem i2; pr_elem i3; pp_expression e; 
      pr_elem i4; pr_elem i5
        
  | Iteration  (For ((e1opt,il1), (e2opt,il2), (e3opt, il3), st)), [i1;i2;i3] ->
      pr_elem i1;
      pr_elem i2;
      pp_statement (ExprStatement e1opt, il1);
      pp_statement (ExprStatement e2opt, il2);
      assert (null il3);
      pp_statement (ExprStatement e3opt, il3);
      pr_elem i3;
      pp_statement st
        
  | Jump (Goto s), [i1;i2;i3]               -> 
      pr_elem i1; pr_elem i2; pr_elem i3;
  | Jump ((Continue|Break|Return)), [i1;i2] -> pr_elem i1; pr_elem i2;
  | Jump (ReturnExpr e), [i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2
  | (Asm, []) -> pr "<<asm_or_strange_stuff>>";
  | x -> raise Impossible
 in
 pp_statement



(* ---------------------- *)
and (pp_type_with_ident_gen: 
   pr_elem_func -> 
   (string * info) option -> (storage * il) option -> fullType -> unit) = 
 fun pr_elem -> 
   fun ident sto ((qu, iiqu), (ty, iity)) -> 
  pp_base_type_gen pr_elem ((qu, iiqu), (ty, iity))  sto;
  pp_type_with_ident_rest_gen pr_elem  ident ((qu, iiqu), (ty, iity))


and (pp_base_type_gen: 
      pr_elem_func -> fullType -> (storage * il) option -> unit) = 
 fun pr_elem -> 
 let pp_expression e = pp_expression_gen pr_elem e in
 let rec pp_base_type = 
  fun (qu, (ty, iity)) sto -> 
  let get_sto sto = 
    match sto with 
    | None -> [] | Some (s, iis) -> (*assert (List.length iis = 1);*) iis  
  in
  let print_sto_qu (sto, (qu, iiqu)) = 
    let all_ii = get_sto sto ++ iiqu in
    all_ii 
     +> List.sort (fun i1 i2 -> compare (fst i1).charpos (fst i2).charpos) 
     +> List.iter pr_elem;
    
  in
  let print_sto_qu_ty (sto, (qu, iiqu), iity) = 
    let all_ii = get_sto sto ++ iiqu ++ iity in
    let all_ii2 = all_ii +> List.sort (fun i1 i2 -> 
      compare (fst i1).charpos (fst i2).charpos) 
    in
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
      | x -> raise Impossible
      );

      fields +> List.iter (fun (FieldDeclList onefield_multivars, iipttvirg) -> 

        (match onefield_multivars with
        | x::xs -> 
            (* handling the first var. Special case, with the first var, we
               print the whole type *)
            (match x with
            | Simple (sopt, typ, iis), iivirg -> 
                (* first var cant have a preceding ',' *)
                assert (List.length iivirg = 0); 
                let identinfo = 
                  (match sopt, iis with 
                    None,_ -> None 
                  | (Some s, [iis]) -> Some (s, iis) 
                  | x -> raise Impossible) 
                in
                pp_type_with_ident_gen pr_elem  identinfo None typ;

            | BitField (sopt, typ, expr, ii), iivirg -> 
                (* first var cant have a preceding ',' *)
                assert (List.length iivirg = 0); 
                (match sopt, ii with
                | (None , [idot]) -> 
                    pp_type_with_ident_gen  pr_elem None None typ;
                    pr_elem idot;
                    pp_expression expr
                | (Some s, [is;idot]) -> 
                    pp_type_with_ident_gen pr_elem (Some (s, is)) None typ;
                    pr_elem idot;
                    pp_expression expr
                | x -> raise Impossible
                )
                  
            );
            
            (* for other vars *)
            xs +> List.iter (function
              | Simple (sopt, typ, iis), iivirg -> 
                  iivirg +> List.iter pr_elem;
                  let identinfo = 
                    (match sopt, iis with 
                    | None,_ -> None 
                    | (Some s, [iis]) -> Some (s, iis) 
                    | x -> raise Impossible) 
                  in
                  pp_type_with_ident_rest_gen pr_elem identinfo typ;

              | BitField (sopt, typ, expr, ii), iivirg -> 
                  iivirg +> List.iter pr_elem;
                  (match sopt, ii with
                  | (Some s, [is;idot]) -> 
                      pp_type_with_ident_rest_gen pr_elem (Some (s, is)) typ;
                      pr_elem idot;
                      pp_expression expr
                  | x -> raise Impossible
                  );
                  
                            );

            assert (List.length iipttvirg = 1);
            iipttvirg +> List.iter pr_elem;
        | x -> raise Impossible
        );
                          );

      (match sopt,iis with
      | Some s , [i1;i2;i3;i4] -> pr_elem i4
      | None, [i1;i2;i3] ->       pr_elem i3; 
      | x -> raise Impossible
      );



  | (Enum  (sopt, enumt), iis) -> 
      print_sto_qu (sto, qu);

      (match sopt, iis with
      | (Some s, ([i1;i2;i3;i4]|[i1;i2;i3;i4;_])) -> 
          pr_elem i1; pr_elem i2; pr_elem i3;
      | (None, ([i1;i2;i3]|[i1;i2;i3;_])) -> 
          pr_elem i1; pr_elem i2
      | x -> raise Impossible
      );

      enumt +> List.iter (fun (((s, is), eopt)    , iicomma) -> 
        assert (List.length iicomma <= 1);
        iicomma +> List.iter pr_elem;
        pr_elem is;
        
                         );

      (match sopt, iis with
      | (Some s, [i1;i2;i3;i4]) ->    pr_elem i4
      | (Some s, [i1;i2;i3;i4;i5]) -> pr_elem i5; pr_elem i4 (* trailing comma *)
      | (None, [i1;i2;i3]) ->         pr_elem i3
      | (None, [i1;i2;i3;i4]) ->      pr_elem i4; pr_elem i3 (* trailing comma *)


      | x -> raise Impossible
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

  | x -> raise Impossible
 in
 pp_base_type


(* used because of DeclList, in    int i,*j[23];  we dont print anymore the 
   int before *j *) 
and (pp_type_with_ident_rest_gen: 
       pr_elem_func -> (string * info) option -> fullType -> unit) = 
 fun pr_elem -> 
  fun ident (((qu, iiqu), (ty, iity)) as fullt) -> 
  let print_ident ident = 
    match ident with 
    | None -> () 
    | Some (s, iis) -> pr_elem iis 
  in

  match ty, iity with
    (* the work is to do in base_type !! *)
  | (BaseType _, iis)                       -> print_ident ident
  | (Enum  (sopt, enumt), iis)              -> print_ident ident
  | (StructUnion (sopt, (_, fields)),iis)   -> print_ident ident
  | (StructUnionName (s, structunion), iis) -> print_ident ident
  | (EnumName  s, iis)                      -> print_ident ident
  | (TypeName (s), iis)                     -> print_ident ident



  | (Pointer t, [i]) ->  
      (* subtil:  void ( *done)(int i)   is a Pointer 
         (FunctionType (return=void, params=int i) *)
      (*WRONG I THINK, use left & right function *)
      (* bug: pp_type_with_ident_rest None t;      print_ident ident *)
      pr_elem i; 
      iiqu +> List.iter pr_elem; (* le const est forcement apres le '*' *)
      pp_type_with_ident_rest_gen pr_elem ident t;

  (* ugly special case ... todo? maybe sufficient in practice *)       
  | (ParenType (q1, (Pointer (q2, (FunctionType t, ii3))   , 
                     [ipointer])  ), [i1;i2]) ->  
      pp_type_left_gen pr_elem (q2, (FunctionType t, ii3));
      pr_elem i1;
      pr_elem ipointer;
      print_ident ident;
      pr_elem i2;
      pp_type_right_gen pr_elem (q2, (FunctionType t, ii3));

  | (ParenType t, [i1;i2]) ->  
      pr2 "PB PARENTYPE ZARB, I forget about the ()";
      pp_type_with_ident_rest_gen pr_elem  ident t;
      

  | (Array (eopt, t), [i1;i2]) -> 
      pp_type_left_gen pr_elem fullt;

      iiqu +> List.iter pr_elem;
      print_ident ident;

      pp_type_right_gen pr_elem fullt;


  | (FunctionType (returnt, paramst), [i1;i2]) -> 
      pp_type_left_gen pr_elem fullt;

      iiqu +> List.iter pr_elem;
      print_ident ident;

      pp_type_right_gen pr_elem fullt;

  | x -> raise Impossible
        

and (pp_type_left_gen: pr_elem_func -> fullType -> unit) = 
 fun pr_elem ->
  let rec pp_type_left = fun ((qu, iiqu), (ty, iity)) -> 
  match ty, iity with
  | (Pointer t, [i]) ->  pr_elem i; pp_type_left t

  | (Array (eopt, t), [i1;i2]) -> pp_type_left t
  | (FunctionType (returnt, paramst), [i1;i2]) -> pp_type_left returnt

  | (ParenType t, _) ->  failwith "parenType"


  | (BaseType _, iis)    -> ()    
  | (Enum  (sopt, enumt), iis) -> ()    
  | (StructUnion (sopt, (_, fields)),iis)  -> ()    
  | (StructUnionName (s, structunion), iis) -> ()    
  | (EnumName  s, iis) -> ()    
  | (TypeName (s), iis) -> ()
  | x -> raise Impossible
  in
  pp_type_left


and (pp_type_right_gen: pr_elem_func -> fullType -> unit) = 
 fun pr_elem -> 
  let rec pp_type_right = fun ((qu, iiqu), (ty, iity)) -> 
  match ty, iity with
  | (Pointer t, [i]) ->  pp_type_right t
  | (Array (eopt, t), [i1;i2]) -> 
      pr_elem i1;
      (match eopt with
      | None -> ()
      | Some e -> pp_expression_gen pr_elem e
      );
      pr_elem i2;
      pp_type_right t

  | (ParenType t, _) ->  failwith "parenType"
  | (FunctionType (returnt, paramst), [i1;i2]) -> 
      pr_elem i1;
      (match paramst with
      | Classic (ts, b, ii) -> 
          ts +> List.iter (fun ((b, sopt, t, (ii2, ii4)),ii3) -> 
            assert ((List.length ii3) <= 1);
            ii3 +> List.iter pr_elem;

            (match sopt with
            | None -> assert (List.length ii4 = 0);
                pp_type_with_ident_gen pr_elem None None t
            | Some s -> 
                assert (List.length ii4 = 1); 
                pp_type_with_ident_gen pr_elem (Some (s, List.hd ii4)) None t;
            );
                          );
          (* normally ii represent the ",..."  but it is also abused with the
             f(void) case *)
          ii +> List.iter pr_elem;
      );
      pr_elem i2;
      



  | (BaseType _, iis)        -> ()    
  | (Enum  (sopt, enumt), iis) -> ()    
  | (StructUnion (sopt, (_, fields)),iis)-> ()      
  | (StructUnionName (s, structunion), iis) -> ()    
  | (EnumName  s, iis) -> ()    
  | (TypeName (s), iis) -> ()
  | x -> raise Impossible
  in 
  pp_type_right

(* ---------------------- *)
and pp_decl_gen pr_elem = function
  | DeclList (((var, returnType, storage),[])::xs,      (iisto, iivirg)) -> 

      (* old: iisto +> List.iter pr_elem; *)

      (* handling the first var. Special case, with the first var, we print 
         the whole type *)
      (match var with
      | Some (s, ini,  iis) -> 
          pp_type_with_ident_gen pr_elem (Some (s, iis)) (Some (storage, iisto))
                                 returnType;
          (match ini with
          | Some (init, iinit) -> pr_elem iinit; pp_init_gen pr_elem init
          | None -> ()
          );
      | None -> pp_type_with_ident_gen pr_elem None None returnType
      );

      (* for other vars, we just call pp_type_with_ident_rest. *)
      xs +> List.iter (function
        | ((Some (s, ini, iis), returnType, storage2), iivirg) -> 
            assert (storage2 = storage);
            iivirg +> List.iter pr_elem;
            pp_type_with_ident_rest_gen pr_elem (Some (s, iis)) returnType;
            (match ini with
            | Some (init, iinit) -> pr_elem iinit; pp_init_gen pr_elem init
            | None -> ()
            );


        | x -> raise Impossible
                      );

      pr_elem iivirg;

  | x -> raise Impossible
        

(* ---------------------- *)
and pp_init_gen = fun pr_elem -> 
 let pp_expression e = pp_expression_gen pr_elem e in
 let rec pp_init = fun (init, iinit) -> 
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


  | x -> raise Impossible
 in
 pp_init



(******************************************************************************)

(* Do not use (mcode, env). It is a simple C pretty printer. *)
let pr_elem (info,(mcode,env)) = 
  let s = info.str in
  pp s


let pp_expression_simple = pp_expression_gen pr_elem
let pp_statement_simple  = pp_statement_gen pr_elem


let rec pp_binding_kind = function
  | MetaIdVal        s -> pp ("id " ^ s)
  | MetaFuncVal      s -> pp ("func " ^ s)
  | MetaLocalFuncVal s -> pp ("localfunc " ^ s)
  | MetaExprVal      expr -> pp_expression_simple expr
  | MetaExprListVal  expr_list -> pp "<<exprlist>>"
  | MetaTypeVal      typ -> pp "<<type>>"
  | MetaStmtVal      statement -> pp_statement_simple statement
  | MetaParamVal     params -> pp "<<param>>"
  | MetaParamListVal params -> pp "<<paramlist>>"

and pp_binding subst = 
  begin
    pp "[";
    Common.print_between (fun () -> pp ";" ) 
      (fun (s, kind) -> pp s; pp " --> "; pp_binding_kind kind)
      subst;
    pp "]";
  end
