open Common
open Commonop
open Ast_c


(******************************************************************************)
(* visitor based on continuation, cleaner (src: based on a (vague) idea from remy douence) *)  
(*
let (iter_expr: ( (expression -> unit) -> expression -> unit)  -> expression -> unit) = fun f expr ->
  let rec k e = 
    match e with
    | Constant c -> ()
    | FunCall  (e, es)         ->  f k e; List.iter (f k) es
    | CondExpr (e1, e2, e3)    -> f k e1; f k e2; f k e3
    | Sequence (e1, e2)        -> f k e1; f k e2;
    | Assignment (e1, op, e2)  -> f k e1; f k e2;
        
    | Postfix  (e, op) -> f k e
    | Infix    (e, op) -> f k e
    | Unary    (e, op) -> f k e
    | Binary   (e1, op, e2) -> f k e1; f k  e2;
        
    | ArrayAccess    (e1, e2) -> f k e1; f k e2;
    | RecordAccess   (e, s) -> f k e
    | RecordPtAccess (e, s) -> f k e

    | SizeOfExpr  e -> f k e
    | SizeOfType  t -> ()
    | _ -> failwith "Todo"

  in f k expr
*)

(*
let ex1 = Sequence (Sequence (Constant (Ident "1"), Constant (Ident "2")), Constant (Ident "4"))
let test = 
  iter_expr (fun k e ->  match e with
  | Constant (Ident x) -> Common.pr2 x
  | rest -> k rest
  ) ex1 
==> 
1
2
4

*)

(* full visitors for all langage concept,  not just for expression *)
type visitor_c_continuation = 
    { 
      kexpr:      (((expression  -> unit) * visitor_c_continuation) -> expression  -> unit);
      kstatement: (((statement   -> unit) * visitor_c_continuation) -> statement   -> unit);
      ktype:      (((fullType    -> unit) * visitor_c_continuation) -> fullType    -> unit);

      kdecl:      (((declaration -> unit) * visitor_c_continuation) -> declaration -> unit);
      kdef:       (((definition  -> unit) * visitor_c_continuation) -> definition  -> unit); 
      kini:      (((initialiser  -> unit) * visitor_c_continuation) -> initialiser  -> unit); 
    } 

let default_visitor_c_continuation = 
  { kexpr =      (fun (k,_) e  -> k e);
    kstatement = (fun (k,_) st -> k st);
    ktype      = (fun (k,_) t  -> k t);
    kdecl      = (fun (k,_) d  -> k d);
    kdef       = (fun (k,_) d  -> k d);
    kini       = (fun (k,_) ie  -> k ie);
  } 


let rec visitor_expr_k = fun bigf expr ->
  let f = bigf.kexpr in
  let rec k e = 
    match e with
    | Ident (s),typ, i -> ()
    | Constant (c),typ,is -> ()
    | FunCall  (e, es),typ, is         -> f (k, bigf) e;  
        (es +> List.map fst) +> List.iter (fun e -> 
          match e with
          | Left e -> (f (k, bigf)) e
          | Right (t, stoil) -> visitor_type_k bigf t
                );
    | CondExpr (e1, e2, e3), typ,is    -> f (k, bigf) e1; f (k, bigf) e2; f (k, bigf) e3
    | Sequence (e1, e2), typ,is        -> f (k, bigf) e1; f (k, bigf) e2;
    | Assignment (e1, op, e2), typ,is  -> f (k, bigf) e1; f (k, bigf) e2;
        
    | Postfix  (e, op), typ,is -> f (k, bigf) e
    | Infix    (e, op), typ,is -> f (k, bigf) e
    | Unary    (e, op), typ,is -> f (k, bigf) e
    | Binary   (e1, op, e2), typ,i -> f (k, bigf) e1; f (k, bigf)  e2;
        
    | ArrayAccess    (e1, e2), typ,is -> f (k, bigf) e1; f (k, bigf) e2;
    | RecordAccess   (e, s), typ,is -> f (k, bigf) e
    | RecordPtAccess (e, s), typ,is -> f (k, bigf) e

    | SizeOfExpr  (e), typ,is -> f (k, bigf) e
    | SizeOfType  (t), typ,is -> visitor_type_k bigf t
    | Cast    (t, e), typ,is -> visitor_type_k bigf t; f (k, bigf) e

(*    | StatementExpr (((declxs, statxs), is)), is2 -> List.iter (visitor_decl_k bigf) declxs; List.iter (visitor_statement_k bigf) statxs *)
    | StatementExpr (((declxs_statxs), is)), typ,is2 -> 
        declxs_statxs +> List.iter (function Left decl -> visitor_decl_k bigf decl | Right stat -> visitor_statement_k bigf stat);

    | Constructor,typ,[] -> ()
    | NoExpr,typ,[] -> ()
          
    | ParenExpr (e), typ,is -> f (k, bigf) e
    | x -> error_cant_have x
  in f (k, bigf) expr

and visitor_statement_k = fun bigf st -> 
  let f = bigf.kstatement in
  let rec k st = 
    match st with
    | Labeled (Label (s, st)), _ -> f (k, bigf)  st;
    | Labeled (Case  (e, st)), _ -> visitor_expr_k bigf e; f (k, bigf) st;
    | Labeled (CaseRange  (e, e2, st)), _ -> visitor_expr_k bigf e; visitor_expr_k bigf e2; f (k, bigf) st;
    | Labeled (Default st), _ -> f (k, bigf) st;
(*old:    | Compound ((declxs, statxs)), is -> List.iter (visitor_decl_k bigf) declxs; List.iter (visitor_statement_k bigf) statxs *)
    | Compound ((declxs_statxs)), is ->
        declxs_statxs +> List.iter (function Left decl -> visitor_decl_k bigf decl | Right stat -> visitor_statement_k bigf stat);
        
    | ExprStatement (None), _ -> ()
    | ExprStatement (Some e), _ -> visitor_expr_k bigf e;
    | Selection  (If (e, st1, st2)), _ -> visitor_expr_k bigf e; f (k, bigf) st1; f (k, bigf) st2;
    | Selection  (Switch (e, st)), _ -> visitor_expr_k bigf e; f (k, bigf) st;
    | Iteration  (While (e, st)), _ -> visitor_expr_k bigf e; f (k, bigf) st;
    | Iteration  (DoWhile (st, e)), _ -> f (k, bigf) st; visitor_expr_k bigf e; 
    | Iteration  (For ((e1opt,i1), (e2opt,i2), (e3opt,i3), st)), _ -> f (k, bigf) (ExprStatement (e1opt),i1); f (k, bigf) (ExprStatement (e2opt),i2); f (k, bigf) (ExprStatement (e3opt),i3); f (k, bigf) st;
          
    | Jump (Goto s), _ -> ()
    | Jump ((Continue|Break|Return)), _ -> ()
    | Jump (ReturnExpr e), _ -> visitor_expr_k bigf e;
    | Asm, _ -> ()

  in f (k, bigf) st

and visitor_type_k = fun bigf t -> 
  let f = bigf.ktype in
  let rec k t = 
    match t with
    | (_, (BaseType _,_)) -> ()
    | (_, (Pointer t,_)) -> f (k, bigf) t
    | (_, (Array (eopt, t),_)) -> f (k, bigf) t (* TODO? eopt  *)
    | (_, (FunctionType (returnt, paramst),_)) -> 
        f (k, bigf) returnt;
        (match paramst with
        | Classic (ts, b,_) -> 
            ts +> List.iter (fun ((b, sopt, t, _),_) -> f (k, bigf) t)
        )

    | (_, (Enum  (sopt, enumt),_)) -> () (* TODO enumt contain some expression *)
    | (_, (StructUnion (sopt, (_, fields)),_)) -> 

       fields +> List.iter (fun (FieldDeclList onefield_multivars, ii) -> 
         onefield_multivars +> List.iter (fun (field, iicomma) ->
         match field with
         | Simple (s, t, ii) -> f (k, bigf) t
         | BitField (sopt, t, expr, _) -> f (k, bigf) t (* TODO expr *)
         (*
         | DefEnum (s, t) -> f (k, bigf) (nQ, (Enum (Some s, t), iitodovide))
         | DefStruct (s, t) -> f (k, bigf) (nQ, (StructUnion (Some s, t), iitodovide))
         *)
         ))


    | (_, (StructUnionName (s, structunion),_)) -> ()
    | (_, (EnumName  s,_)) -> ()

    | (_, (TypeName (s),_)) -> ()

    | (_, (ParenType t,_)) -> f (k, bigf) t


  in f (k, bigf) t

and visitor_decl_k = fun bigf d -> 
  let f = bigf.kdecl in 
  let rec k d = 
    match d with 
      (* TODO parcourir aussi les autres decl, dans les xs *)
    | DeclList (((Some (s, Some (init, iinit), _), t, sto),_)::xs, _) -> 
        visitor_type_k bigf t;
        visitor_ini_k bigf init;
    | DeclList (((Some (s, None, _), t, sto),_)::xs, _) -> visitor_type_k bigf t
    | DeclList (((None, t, sto),_)::xs, _) -> visitor_type_k bigf t
    | x -> error_cant_have x
      
  in f (k, bigf) d 

and visitor_ini_k = fun bigf ini -> 
  let f = bigf.kini in
  let rec k (ini, iini) = 
    match ini with
    | InitExpr e -> visitor_expr_k bigf e
    | InitList initxs -> List.iter (f (k, bigf)) (initxs +> List.map fst)
    | InitGcc (s, e) -> f (k, bigf) e
    | InitGccIndex (e1, e) -> visitor_expr_k bigf e1; f (k, bigf) e
    | InitGccRange (e1, e2, e) -> visitor_expr_k bigf e1; visitor_expr_k bigf e2; f (k, bigf) e
  in f (k, bigf) ini

and visitor_def_k = fun bigf d -> 
  let f = bigf.kdef in
  let rec k d = 
    match d with
    | (s, (returnt, paramst, _, _), sto, (declxs_statxs), _) -> 
        visitor_type_k bigf returnt;
        List.iter (fun ((b, s, t, _),_) -> visitor_type_k bigf t) paramst;
        
        declxs_statxs +> List.iter (function Left decl -> visitor_decl_k bigf decl | Right stat -> visitor_statement_k bigf stat);
  in f (k, bigf) d 

    


(*******************************************************************************)

type visitor_c_continuation_s = 
    { 
      kexpr_s:      (((expression  -> expression) * visitor_c_continuation_s) -> expression  -> expression);
      kstatement_s: (((statement   -> statement)  * visitor_c_continuation_s) -> statement   -> statement);
      ktype_s:      (((fullType    -> fullType)   * visitor_c_continuation_s) -> fullType    -> fullType);

      kdecl_s:      (((declaration -> declaration)  * visitor_c_continuation_s) -> declaration -> declaration);
      kdef_s:       (((definition  -> definition)   * visitor_c_continuation_s) -> definition  -> definition); 
      kini_s:       (((initialiser  -> initialiser) * visitor_c_continuation_s) -> initialiser -> initialiser); 
    } 

let default_visitor_c_continuation_s = 
  { kexpr_s =      (fun (k,_) e  -> k e);
    kstatement_s = (fun (k,_) st -> k st);
    ktype_s      = (fun (k,_) t  -> k t);
    kdecl_s      = (fun (k,_) d  -> k d);
    kdef_s       = (fun (k,_) d  -> k d);
    kini_s       = (fun (k,_) d  -> k d);
  } 

(* as the functions may do side effect (such as maintaining an environment), 
   important that they get called in right order hence the let in let in to force the order of 
   evaluation
*)
let rec visitor_expr_k_s = fun bigf expr ->
  let f = bigf.kexpr_s in
  let rec k e = 
    match e with
    | Ident (s), typ, i -> Ident (s), typ, i
    | Constant (c), typ, is -> Constant (c), typ, is
    | FunCall  (e, es), typ, is         -> let e'  = (f (k, bigf)) e  in 
      let es' = (es +> List.map fst)  +> List.map (fun e -> 
        match e with
        | Left e -> Left ((f (k, bigf)) e)
        | Right e -> raise Todo
      )
      in  FunCall (e', (zip es' (es +> List.map snd))), typ, is
    | CondExpr (e1, e2, e3), typ, is    -> let e1' = (f (k, bigf)) e1 in let e2' = (f (k, bigf)) e2 in let e3' =  (f (k, bigf)) e3 in  CondExpr (e1', e2', e3'), typ, is
    | Sequence (e1, e2), typ, is        -> let e1' = (f (k, bigf)) e1 in let e2' = (f (k, bigf)) e2 in                             Sequence (e1', e2'), typ, is
    | Assignment (e1, op, e2), typ, is  -> let e1' = (f (k, bigf)) e1 in let e2' = (f (k, bigf)) e2 in                             Assignment (e1', op,  e2'), typ, is
        
    | Postfix  (e, op), typ, is -> let e' = (f (k, bigf)) e in Postfix (e', op), typ, is
    | Infix    (e, op), typ, is -> let e' = (f (k, bigf)) e in Infix   (e', op), typ, is
    | Unary    (e, op), typ, is -> let e' = (f (k, bigf)) e in Unary   (e', op), typ, is
    | Binary   (e1, op, e2), typ, is -> let e1' = (f (k, bigf)) e1 in let e2' = (f (k, bigf)) e2 in                                Binary (e1', op,  e2'), typ, is
        
    | ArrayAccess    (e1, e2), typ, is -> let e1' = (f (k, bigf)) e1 in let e2' = (f (k, bigf)) e2 in                              ArrayAccess (e1', e2'), typ, is
    | RecordAccess   (e, s), typ, is -> let e' = (f (k, bigf)) e in RecordAccess     (e', s), typ, is 
    | RecordPtAccess (e, s), typ, is -> let e' = (f (k, bigf)) e in RecordPtAccess   (e', s), typ, is 

    | SizeOfExpr  (e), typ, is -> let e' = (f (k, bigf)) e in  SizeOfExpr   (e'), typ, is
    | SizeOfType  (t), typ, is -> let t' = visitor_type_k_s bigf t in SizeOfType (t'), typ, is
    | Cast    (t, e), typ, is -> let t' = visitor_type_k_s bigf t in let e' = (f (k, bigf)) e in Cast   (t', e'), typ, is

(*    | StatementExpr (((declxs, statxs), is)), is2 -> let declxs' = List.map (visitor_decl_k_s bigf) declxs in let statxs' = List.map (visitor_statement_k_s bigf) statxs in StatementExpr (((declxs', statxs'), is)), is2 *)
    | StatementExpr (((declxs_statxs), is)), typ, is2 -> 
        let declxs_statxs' = declxs_statxs +> List.map (function Left decl -> Left (visitor_decl_k_s bigf decl) | Right stat -> Right (visitor_statement_k_s bigf stat)) 
        in StatementExpr (((declxs_statxs'), is)), typ, is2 
    | Constructor,typ, is -> Constructor,typ, is
    | NoExpr,typ, is -> NoExpr,typ,is
    | ParenExpr (e), typ, is -> let e' = (f (k, bigf)) e in ParenExpr (e'), typ, is
    | x -> error_cant_have x
  in f (k, bigf) expr

and visitor_statement_k_s = fun bigf st -> 
  let f = bigf.kstatement_s in
  let rec k st = 
    match st with
    | Labeled (Label (s, st)), ii -> let st' = (f (k, bigf)) st in                         Labeled (Label (s, st')), ii
    | Labeled (Case  (e, st)), ii -> let e' = (visitor_expr_k_s bigf) e in let st' = (f (k, bigf)) st in Labeled (Case  (e', st')), ii
    | Labeled (CaseRange  (e, e2, st)),ii -> let e' = (visitor_expr_k_s bigf) e in let e2' = (visitor_expr_k_s bigf) e2 in let st' = (f (k, bigf)) st in Labeled (CaseRange  (e', e2', st')), ii
    | Labeled (Default st), ii -> let st' = (f (k, bigf)) st in                            Labeled (Default st'), ii
    | Compound ((declxs_statxs)), is -> 
        let declxs_statxs' = declxs_statxs +> List.map (function Left decl -> Left (visitor_decl_k_s bigf decl) | Right stat -> Right (visitor_statement_k_s bigf stat)) 
        in Compound (declxs_statxs'), is
    | ExprStatement (None), ii ->                           ExprStatement (None), ii
    | ExprStatement (Some e), ii -> let e' = (visitor_expr_k_s bigf) e in ExprStatement (Some e'), ii
    | Selection  (If (e, st1, st2)), ii -> let e' = (visitor_expr_k_s bigf) e in let st1' = (f (k, bigf)) st1 in let st2' = (f (k, bigf)) st2 in Selection  (If (e', st1', st2')), ii
    | Selection  (Switch (e, st)), ii -> let e' = (visitor_expr_k_s bigf) e in let st' = (f (k, bigf)) st in Selection  (Switch (e', st')), ii
    | Iteration  (While (e, st)), ii -> let e' = (visitor_expr_k_s bigf) e in let st' = (f (k, bigf)) st in Iteration  (While (e', st')), ii
    | Iteration  (DoWhile (st, e)), ii -> let st' = (f (k, bigf)) st in let e' = (visitor_expr_k_s bigf) e in Iteration  (DoWhile (st', e')), ii
    | Iteration  (For ((e1opt,i1), (e2opt,i2), (e3opt,i3), st)), ii -> 
          let e1opt' = (f (k, bigf)) (ExprStatement (e1opt),i1) in
          let e2opt' = (f (k, bigf)) (ExprStatement (e2opt),i2) in
          let e3opt' = (f (k, bigf)) (ExprStatement (e3opt),i3) in
          let st' = (f (k, bigf)) st in
          (match (e1opt', e2opt', e3opt') with
          | ((ExprStatement x1,i1), (ExprStatement x2,i2), ((ExprStatement x3,i3))) -> 
              Iteration (For ((x1,i1), (x2,i2), (x3,i3), st')), ii
          | x -> pr2 "cant be here if iterator keep ExprStatement as is"; error_cant_have x
          )

          
    | Jump (Goto s), ii -> Jump (Goto s), ii
    | Jump (((Continue|Break|Return) as x)), ii -> Jump (x), ii
    | Jump (ReturnExpr e), ii -> let e' = (visitor_expr_k_s bigf) e in Jump (ReturnExpr e'), ii
    | Asm,ii -> Asm,ii

  in f (k, bigf) st

and visitor_type_k_s = fun bigf t -> 
  let f = bigf.ktype_s in
  let rec k t = 
    match t with
    | (q, (BaseType _,ii)) -> t
    | (q, (Pointer t,ii)) -> let t' = f (k, bigf) t in  (q, (Pointer t',ii))
    | (q, (Array (eopt, t),ii)) -> let t' = f (k, bigf) t in (q, (Array (eopt, t'),ii))              (* TODO? eopt  *)
    | (q, (FunctionType (returnt, paramst),ii)) -> 
        let returnt' = f (k, bigf) returnt in
        let paramst' = 
        (match paramst with
        | Classic (ts, b, ii1) -> 
           Classic
            (ts +> List.map (fun ((b, sopt, t,i2),i3) -> let t' = f (k, bigf) t in ((b, sopt, t',i2),i3)),
             b, ii1)
        )
       in
       (q, (FunctionType (returnt', paramst'),ii))

    | (q, (Enum  (sopt, enumt),ii)) -> (q, (Enum  (sopt, enumt),ii)) (* TODO enumt contain some expression *)
    | (q, (StructUnion (sopt, (su, fields)),ii)) -> 
       let fields' = 

       fields +> List.map (fun (FieldDeclList onefield_multivars, ii) -> 

        onefield_multivars +> List.map (fun (field, iicomma) ->

         (match field with
         | Simple (s, t, ii) -> let t' = f (k, bigf) t in Simple (s, t', ii)
         | BitField (sopt, t, expr, ii) -> let t' = f (k, bigf) t in BitField (sopt, t', expr, ii)    (* TODO expr *)
         (*
         | DefEnum (s, t) -> let t' =   f (k, bigf) (nQ, (Enum (Some s, t), iitodovide)) in 
                (match t' with 
                | (_, (Enum (Some s, t'),iitodo)) -> DefEnum (s, t') 
                | x -> Common.error_cant_have x
                )
         | DefStruct (s, t) -> let t' = f (k, bigf) (nQ, (StructUnion (Some s, t), iitodovide)) in
                (match t' with
                | (_, (StructUnion (Some s, t'), iitodo)) -> DefStruct (s, t')
                | x -> Common.error_cant_have x
                )
         *)
         ),iicomma
         ) +> (fun onefield_multivars' -> FieldDeclList onefield_multivars', ii)
         ) in
       (q, (StructUnion (sopt, (su, fields')),ii))


    | (q, (StructUnionName (s, structunion),ii)) -> t
    | (q, (EnumName  s,ii)) -> t

    | (q, (TypeName (s),ii)) -> t

    | (q, (ParenType t,ii)) -> let t' = f (k, bigf) t in  (q, (ParenType t',ii))


  in f (k, bigf) t

and visitor_decl_k_s = fun bigf d -> 
  let f = bigf.kdecl_s in 
  let rec k d = 
    match d with 
(* todo xs *)
    | DeclList (((Some (s, Some (init,iinit), ii2), t, sto),ii6)::xs, ii) -> 
        let t' = visitor_type_k_s bigf t in
        let init' = visitor_ini_k_s bigf init in
        DeclList (((Some (s, Some (init',iinit), ii2), t', sto),ii6)::xs, ii)

    | DeclList (((Some (s, None, ii2), t, sto),ii6)::xs, ii) -> let t' = visitor_type_k_s bigf t in DeclList (((Some (s, None, ii2), t', sto),ii6)::xs, ii)
    | DeclList (((None, t, sto),ii6)::xs, ii) -> let t' = visitor_type_k_s bigf t in DeclList (((None, t', sto),ii6)::xs, ii)
    | x -> error_cant_have x
      
  in f (k, bigf) d 

and visitor_ini_k_s = fun bigf ini -> 
  let f = bigf.kini_s in
  let rec k (ini, iinit) = 
    match ini with
    | InitExpr e -> let e' = visitor_expr_k_s bigf e in InitExpr e', iinit
    | InitList initxs -> let initxs' = List.map (f (k, bigf)) (initxs +> List.map fst) in InitList (zip initxs' (List.map snd initxs)), iinit
    | InitGcc (s, e) -> let e' = f (k, bigf) e in InitGcc (s, e'), iinit
    | InitGccIndex (e1, e) -> let e1' = visitor_expr_k_s bigf e1 in let e' =  f (k, bigf) e in InitGccIndex (e1', e') , iinit
    | InitGccRange (e1, e2, e) -> let e1' = visitor_expr_k_s bigf e1 in let e2' = visitor_expr_k_s bigf e2 in let e' = f (k, bigf) e in
                       InitGccRange (e1', e2', e') , iinit
  in f (k, bigf) ini

and visitor_def_k_s = fun bigf d -> 
  let f = bigf.kdef_s in
  let rec k d = 
    match d with
    | (s, (returnt, paramst, b, ii1), sto, (declxs_statxs), ii2) -> 
        let returnt' = visitor_type_k_s bigf returnt in
        let paramst' = List.map (fun ((b, s, t, ii3),ii4) -> ((b, s, visitor_type_k_s bigf t, ii3), ii4)) paramst in
        let declxs_statxs' = declxs_statxs +> List.map (function Left decl -> Left (visitor_decl_k_s bigf decl) | Right stat -> Right (visitor_statement_k_s bigf stat)) 
        in
        (s, (returnt', paramst', b, ii1), sto, (declxs_statxs'), ii2)
  in f (k, bigf) d 

