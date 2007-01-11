open Common open Commonop

open Ast_c

(******************************************************************************)
(* Visitor based on continuation. Cleaner than the one based on mutable 
 * pointer functions.
 * src: based on a (vague) idea from remy douence.
 *) 
(******************************************************************************)
 
(*
let (iter_expr:((expression -> unit) -> expression -> unit) -> expression -> unit)
 = fun f expr ->
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
    | _ -> failwith "to complete"

  in f k expr

let ex1 = Sequence (Sequence (Constant (Ident "1"), Constant (Ident "2")), 
                             Constant (Ident "4"))
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
type visitor_c = 
 { 
   kexpr:      (expression  -> unit) * visitor_c -> expression  -> unit;
   kstatement: (statement   -> unit) * visitor_c -> statement   -> unit;
   ktype:      (fullType    -> unit) * visitor_c -> fullType    -> unit;

   kdecl:      (declaration -> unit) * visitor_c -> declaration -> unit;
   kdef:       (definition  -> unit) * visitor_c -> definition  -> unit; 
   kini:       (initialiser  -> unit) * visitor_c -> initialiser  -> unit; 
 } 

let default_visitor_c = 
  { kexpr =      (fun (k,_) e  -> k e);
    kstatement = (fun (k,_) st -> k st);
    ktype      = (fun (k,_) t  -> k t);
    kdecl      = (fun (k,_) d  -> k d);
    kdef       = (fun (k,_) d  -> k d);
    kini       = (fun (k,_) ie  -> k ie);
  } 


let rec visitor_expr_k = fun bigf expr ->
  let f = bigf.kexpr in
  let rec k ((e,typ), ii) = 
    match e,ii with
    | Ident (s), i -> ()
    | Constant (c),is -> ()
    | FunCall  (e, es), is         -> 
        let rec do_action = function 
          | (ActMisc ii) -> ()
          | (ActJump jump) -> 
              (match jump with
              | (Goto s), is               -> ()
              | ((Continue|Break|Return)), is -> ()
              | (ReturnExpr e), is ->  f (k, bigf) e; 
              )
          | (ActSeq ((e,ii), action)) -> 
              do_option (f (k, bigf)) e; 
              do_action action
        in


        f (k, bigf) e;  
        (es +> List.map fst) +> List.iter (fun e -> 
          match e with
          | Left e -> (f (k, bigf)) e
          | Right (ArgType (t, stoil)) -> visitor_type_k bigf t
          | Right (ArgAction action) -> do_action action
          );
    | CondExpr (e1, e2, e3), is    -> 
        f (k, bigf) e1; do_option (f (k, bigf)) e2; f (k, bigf) e3
    | Sequence (e1, e2), is        -> f (k, bigf) e1; f (k, bigf) e2;
    | Assignment (e1, op, e2), is  -> f (k, bigf) e1; f (k, bigf) e2;
        
    | Postfix  (e, op), is -> f (k, bigf) e
    | Infix    (e, op), is -> f (k, bigf) e
    | Unary    (e, op), is -> f (k, bigf) e
    | Binary   (e1, op, e2), i -> f (k, bigf) e1; f (k, bigf)  e2;
        
    | ArrayAccess    (e1, e2), is -> f (k, bigf) e1; f (k, bigf) e2;
    | RecordAccess   (e, s), is -> f (k, bigf) e
    | RecordPtAccess (e, s), is -> f (k, bigf) e

    | SizeOfExpr  (e), is -> f (k, bigf) e
    | SizeOfType  (t), is -> visitor_type_k bigf t
    | Cast    (t, e), is -> visitor_type_k bigf t; f (k, bigf) e

    (* old: | StatementExpr (((declxs, statxs), is)), is2 -> 
     *          List.iter (visitor_decl_k bigf) declxs; 
     *          List.iter (visitor_statement_k bigf) statxs 
     *)
    | StatementExpr ((statxs, is)), is2 -> 
        statxs +> List.iter (visitor_statement_k bigf);

    (* TODO, we will certainly have to then do a special visitor for 
     * initializer 
     *)
    | Constructor,is -> ()
          
    | ParenExpr (e), is -> f (k, bigf) e

    | MacroCall arg,_ -> 
        (match arg with
        | Left e -> f (k, bigf) e
        | Right xs -> xs +> List.iter (visitor_statement_k bigf)
        )

        

  in f (k, bigf) expr

and visitor_statement_k = fun bigf st -> 
  let f = bigf.kstatement in
  let rec k st = 
    match st with
    | Labeled (Label (s, st)), _ -> f (k, bigf)  st;
    | Labeled (Case  (e, st)), _ -> visitor_expr_k bigf e; f (k, bigf) st;
    | Labeled (CaseRange  (e, e2, st)), _ -> 
        visitor_expr_k bigf e; visitor_expr_k bigf e2; f (k, bigf) st;
    | Labeled (Default st), _ -> f (k, bigf) st;

    | Compound statxs, is -> statxs +> List.iter (visitor_statement_k bigf)
    | ExprStatement (None), _ -> ()
    | ExprStatement (Some e), _ -> visitor_expr_k bigf e;

    | Selection  (If (e, st1, st2)), _ -> 
        visitor_expr_k bigf e; f (k, bigf) st1; f (k, bigf) st2;
    | Selection (IfCpp (st1s, st2s)), _ -> 
        st1s +> List.iter (visitor_statement_k bigf);
        st2s +> List.iter (visitor_statement_k bigf)
    | Selection  (Switch (e, st)), _ -> 
        visitor_expr_k bigf e; f (k, bigf) st;
    | Iteration  (While (e, st)), _ -> 
        visitor_expr_k bigf e; f (k, bigf) st;
    | Iteration  (DoWhile (st, e)), _ -> f (k, bigf) st; visitor_expr_k bigf e; 
    | Iteration  (For ((e1opt,i1), (e2opt,i2), (e3opt,i3), st)), _ -> 
        f (k, bigf) (ExprStatement (e1opt),i1); 
        f (k, bigf) (ExprStatement (e2opt),i2); 
        f (k, bigf) (ExprStatement (e3opt),i3); 
        f (k, bigf) st;
          
    | Jump (Goto s), _ -> ()
    | Jump ((Continue|Break|Return)), _ -> ()
    | Jump (ReturnExpr e), _ -> visitor_expr_k bigf e;

    | Decl decl, _ -> visitor_decl_k bigf decl 
    | Asm, _ -> ()

  in f (k, bigf) st

and visitor_type_k = fun bigf t -> 
  let f = bigf.ktype in
  let rec k t = 
    match snd t with
    | (BaseType _,_) -> ()
    | (Pointer t,_) -> f (k, bigf) t
    | (Array (eopt, t),_) -> 
        do_option (visitor_expr_k bigf) eopt;
        f (k, bigf) t 
    | (FunctionType (returnt, paramst),_) -> 
        f (k, bigf) returnt;
        (match paramst with
        | (ts, (b,_)) -> 
            ts +> List.iter (fun (((b, sopt, t), _),_) -> f (k, bigf) t)
        )

    | (Enum  (sopt, enumt),_) -> 
        enumt +> List.iter (fun (((s, eopt),ii_s_eq), iicomma) -> 
          eopt +> do_option (visitor_expr_k bigf)
          );    
        
    | (StructUnion (sopt, (_, fields)),_) -> 

       fields +> List.iter (fun (FieldDeclList onefield_multivars, ii) -> 
         onefield_multivars +> List.iter (fun (field, iicomma) ->
         match field with
         | Simple (s, t), ii -> f (k, bigf) t
         | BitField (sopt, t, expr), ii -> 
             visitor_expr_k bigf expr;
             f (k, bigf) t 
         ))


    | (StructUnionName (s, structunion),_) -> ()
    | (EnumName  s,_) -> ()

    | (TypeName (s),_) -> ()

    | (ParenType t,_) -> f (k, bigf) t


  in f (k, bigf) t

and visitor_decl_k = fun bigf d -> 
  let f = bigf.kdecl in 
  let rec k (DeclList (xs,ii)) = List.iter aux xs 
  and aux ((var, t, sto), iicomma) = 
    visitor_type_k bigf t;
    var +> do_option (fun ((s, ini), ii_s_ini) -> 
      ini +> do_option (visitor_ini_k bigf)
        );
  in f (k, bigf) d 

and visitor_ini_k = fun bigf ini -> 
  let f = bigf.kini in
  let rec k (ini, iini) = 
    match ini with
    | InitExpr e -> visitor_expr_k bigf e
    | InitList initxs -> List.iter (f (k, bigf)) (initxs +> List.map fst)
    | InitGcc (s, e) -> f (k, bigf) e
    | InitGccIndex (e1, e) -> visitor_expr_k bigf e1; f (k, bigf) e
    | InitGccRange (e1, e2, e) -> 
        visitor_expr_k bigf e1; 
        visitor_expr_k bigf e2; 
        f (k, bigf) e
  in f (k, bigf) ini

and visitor_def_k = fun bigf d -> 
  let f = bigf.kdef in
  let rec k d = 
    match d with
    | (s, (returnt, (paramst, b)), sto, statxs), _ -> 
        visitor_type_k bigf returnt;
        paramst +> List.iter (fun (((b, s, t), _),_) -> visitor_type_k bigf t);
        statxs +> List.iter (visitor_statement_k bigf)
  in f (k, bigf) d 

    


(******************************************************************************)
type 'a inout = 'a -> 'a 

(* _s for synthetizized attributes *)
type visitor_c_s = { 
  kexpr_s:      (expression inout * visitor_c_s) -> expression inout;
  kstatement_s: (statement  inout * visitor_c_s) -> statement  inout;
  ktype_s:      (fullType   inout * visitor_c_s) -> fullType   inout;

  kdecl_s: (declaration  inout * visitor_c_s) -> declaration inout;
  kdef_s:  (definition   inout * visitor_c_s) -> definition  inout; 
  kini_s:  (initialiser  inout * visitor_c_s) -> initialiser inout; 
  kprogram_s: (programElement inout * visitor_c_s) -> programElement inout;

  kinfo_s: (info inout * visitor_c_s) -> info inout;
 } 

let default_visitor_c_s = 
  { kexpr_s =      (fun (k,_) e  -> k e);
    kstatement_s = (fun (k,_) st -> k st);
    ktype_s      = (fun (k,_) t  -> k t);
    kdecl_s      = (fun (k,_) d  -> k d);
    kdef_s       = (fun (k,_) d  -> k d);
    kini_s       = (fun (k,_) d  -> k d);
    kprogram_s   = (fun (k,_) p  -> k p);
    kinfo_s      = (fun (k,_) i  -> k i);

  } 

let rec visitor_expr_k_s = fun bigf expr ->
  let infolistf ii = List.map (visitor_info_k_s bigf) ii in
  let rec exprf e = bigf.kexpr_s  (k, bigf) e
  and k e = 
    let ((unwrap_e, typ), ii) = e in
    let typ' = typ +> map_option (visitor_type_k_s bigf) in
    let e' = 
    match unwrap_e with
    | Ident (s) -> Ident (s)
    | Constant (c) -> Constant (c)
    | FunCall  (e, es)         -> 
        FunCall (exprf e,
                 es +> List.map (fun (e,ii) -> 
                   visitor_argument_k_s bigf e, infolistf ii
                     ))
                
    | CondExpr (e1, e2, e3)    -> CondExpr (exprf e1, fmap exprf e2, exprf e3)
    | Sequence (e1, e2)        -> Sequence (exprf e1, exprf e2)
    | Assignment (e1, op, e2)  -> Assignment (exprf e1, op, exprf e2)
        
    | Postfix  (e, op) -> Postfix (exprf e, op)
    | Infix    (e, op) -> Infix   (exprf e, op)
    | Unary    (e, op) -> Unary   (exprf e, op)
    | Binary   (e1, op, e2) -> Binary (exprf e1, op, exprf e2)
        
    | ArrayAccess    (e1, e2) -> ArrayAccess (exprf e1, exprf e2)
    | RecordAccess   (e, s) -> RecordAccess     (exprf e, s) 
    | RecordPtAccess (e, s) -> RecordPtAccess   (exprf e, s) 

    | SizeOfExpr  (e) -> SizeOfExpr   (exprf e)
    | SizeOfType  (t) -> SizeOfType (visitor_type_k_s bigf t)
    | Cast    (t, e) ->  Cast   (visitor_type_k_s bigf t, exprf e)

    | StatementExpr (statxs, is) -> 
        StatementExpr (
          statxs +> List.map (visitor_statement_k_s bigf),
          infolistf is)
    | Constructor -> Constructor
    | ParenExpr (e) -> ParenExpr (exprf e)

    | MacroCall arg -> 
        MacroCall
        (match arg with
        | Left e -> Left (exprf e)
        | Right xs -> Right (xs +> List.map (visitor_statement_k_s bigf))
        )
    in
    (e', typ'), (infolistf ii)
  in exprf expr

and visitor_argument_k_s bigf argument = 
  let infolistf ii = List.map (visitor_info_k_s bigf) ii in
  let rec do_action = function 
    | (ActMisc ii) -> ActMisc (infolistf ii)
    | (ActJump jump) -> 
        ActJump
          (match jump with
          | (Goto s), is               -> (Goto s), infolistf is
          | ((Continue|Break|Return) as x), is -> x, infolistf is
          | (ReturnExpr e), is ->  ReturnExpr (visitor_expr_k_s bigf e), 
                                   infolistf is
          )
    | (ActSeq ((e, iptvirg), action)) -> 
        ActSeq ((map_option (visitor_expr_k_s bigf) e, 
                 infolistf iptvirg), 
                do_action action)
  in

  (match argument with
  | Left e -> Left (visitor_expr_k_s bigf e)
  | Right (ArgType (t, stoil)) -> 
      let (unwrap_st, ii) = stoil in
      Right (ArgType 
               (visitor_type_k_s bigf t, 
                (unwrap_st, infolistf ii
                )))
  | Right (ArgAction action) -> 
      Right (ArgAction (do_action action))
  )






and visitor_statement_k_s = fun bigf st -> 
  let rec statf st = bigf.kstatement_s (k, bigf) st 
  and k st = 
    let (unwrap_st, ii) = st in
    let st' = 
    match unwrap_st with
    | Labeled (Label (s, st)) -> 
        Labeled (Label (s, statf st))
    | Labeled (Case  (e, st)) -> 
        Labeled (Case  ((visitor_expr_k_s bigf) e , statf st))
    | Labeled (CaseRange  (e, e2, st)) -> 
        Labeled (CaseRange  ((visitor_expr_k_s bigf) e, 
                             (visitor_expr_k_s bigf) e2, 
                             statf st))
    | Labeled (Default st) -> Labeled (Default (statf st))
    | Compound statxs -> 
        Compound (statxs +> List.map (visitor_statement_k_s bigf))
    | ExprStatement (None) ->  ExprStatement (None)
    | ExprStatement (Some e) -> ExprStatement (Some ((visitor_expr_k_s bigf) e))
    | Selection (If (e, st1, st2)) -> 
        Selection  (If ((visitor_expr_k_s bigf) e, statf st1, statf st2))
    | Selection (IfCpp (st1s, st2s)) -> 
        Selection  (IfCpp 
                      (st1s +> List.map (visitor_statement_k_s bigf),
                       st2s +> List.map (visitor_statement_k_s bigf)))
    | Selection (Switch (e, st))   -> 
        Selection  (Switch ((visitor_expr_k_s bigf) e, statf st))
    | Iteration (While (e, st))    -> 
        Iteration  (While ((visitor_expr_k_s bigf) e, statf st))
    | Iteration (DoWhile (st, e))  -> 
        Iteration  (DoWhile (statf st, (visitor_expr_k_s bigf) e))
    | Iteration (For ((e1opt,i1), (e2opt,i2), (e3opt,i3), st)) -> 
          let e1opt' = statf (ExprStatement (e1opt),i1) in
          let e2opt' = statf (ExprStatement (e2opt),i2) in
          let e3opt' = statf (ExprStatement (e3opt),i3) in
          (match (e1opt', e2opt', e3opt') with
          | ((ExprStatement x1,i1), (ExprStatement x2,i2), ((ExprStatement x3,i3))) -> 
              Iteration (For ((x1,i1), (x2,i2), (x3,i3), statf st))
          | x -> failwith "cant be here if iterator keep ExprStatement as is"
          )

          
    | Jump (Goto s) -> Jump (Goto s)
    | Jump (((Continue|Break|Return) as x)) -> Jump (x)
    | Jump (ReturnExpr e) -> Jump (ReturnExpr ((visitor_expr_k_s bigf) e))

    | Decl decl -> Decl (visitor_decl_k_s bigf decl)
    | Asm -> Asm
    in
    st', List.map (visitor_info_k_s bigf) ii
  in statf st

and visitor_type_k_s = fun bigf t -> 
  let rec typef t = bigf.ktype_s (k,bigf) t
  and infolistf ii = List.map (visitor_info_k_s bigf) ii
  and k t = 
    let (q, t) = t in
    let (unwrap_q, iiq) = q in
    let q' = unwrap_q in     (* todo? a visitor for qualifier *)
    let (unwrap_t, iit) = t in
    let t' = 
    match unwrap_t with
    | BaseType x -> BaseType x
    | Pointer t  -> Pointer (typef t)
    | Array (eopt, t) -> Array (fmap (visitor_expr_k_s bigf) eopt, typef t) 
    | FunctionType (returnt, paramst) -> 
        FunctionType 
          (typef returnt, 
           (match paramst with
           | (ts, (b, iihas3dots)) -> 
                 (ts +> List.map (fun (((b, sopt, t), ii_b_s),iicomma) -> 
                   (((b, sopt, typef t), infolistf ii_b_s), 
                    infolistf iicomma)),
                  (b, infolistf iihas3dots))
           ))

    | Enum  (sopt, enumt) -> 
        Enum (sopt,
              enumt +> List.map (fun (((s, eopt),ii_s_eq), iicomma) -> 
                ((s, fmap (visitor_expr_k_s bigf) eopt), infolistf ii_s_eq),
                infolistf iicomma
                                )
             )
    | StructUnion (sopt, (su, fields)) -> 
       StructUnion (sopt, (su, 
        fields +> List.map (fun (FieldDeclList onefield_multivars, iiptvirg) -> 
         FieldDeclList (
          onefield_multivars +> List.map (fun (field, iicomma) ->
            (match field with
            | Simple (s, t), iis -> Simple (s, typef t), infolistf iis
            | BitField (sopt, t, expr), iis -> 
                BitField (sopt, typef t, visitor_expr_k_s bigf expr), 
                          infolistf iis
            ), infolistf iicomma
                                         )
         ), infolistf iiptvirg
         )
         ))


    | StructUnionName (s, structunion) -> StructUnionName (s, structunion)
    | EnumName  s -> EnumName  s
    | TypeName s -> TypeName s

    | ParenType t -> ParenType (typef t)
    in
    (q', infolistf iiq), 
    (t', infolistf iit)


  in typef t

and visitor_decl_k_s = fun bigf d -> 
  let f = bigf.kdecl_s in 
  let infolistf ii = List.map (visitor_info_k_s bigf) ii in
  let rec k (DeclList (xs, ii)) = DeclList (List.map aux xs,   infolistf ii)
  and aux ((var, t, sto), iicomma) = 
    ((var +> map_option (fun ((s, ini), ii_s_ini) -> 
      (s, ini +> map_option (fun init -> visitor_ini_k_s bigf init)),
      infolistf ii_s_ini
        )
     ),
     visitor_type_k_s bigf t, 
     sto),
    infolistf iicomma

  in f (k, bigf) d 

and visitor_ini_k_s = fun bigf ini -> 
  let rec inif ini = bigf.kini_s (k,bigf) ini
  and k ini = 
    let (unwrap_ini, ii) = ini in
    let ini' = 
    match unwrap_ini with
    | InitExpr e -> InitExpr (visitor_expr_k_s bigf e)
    | InitList initxs -> 
         InitList (initxs +> List.map (fun (ini, ii) -> 
           inif ini, List.map (visitor_info_k_s bigf) ii) 
                     )
    | InitGcc (s, e) -> InitGcc (s, inif e)
    | InitGccIndex (e1, e) -> 
        InitGccIndex (visitor_expr_k_s bigf e1 , inif e)
    | InitGccRange (e1, e2, e) -> 
        InitGccRange (visitor_expr_k_s bigf e1, visitor_expr_k_s bigf e2, inif e)
    in ini', List.map (visitor_info_k_s bigf) ii
  in inif ini


and visitor_def_k_s = fun bigf d -> 
  let f = bigf.kdef_s in
  let infolistf ii = List.map (visitor_info_k_s bigf) ii in
  let rec k d = 
    match d with
    | (s, (returnt, (paramst, (b, iib))), sto, statxs), ii  -> 
        (s, 
         (visitor_type_k_s bigf returnt, 
          (paramst +> List.map (fun (((b, s, t), iibs), iicomma) ->
          ((b, s, visitor_type_k_s bigf t), infolistf iibs), infolistf iicomma
          ), 
           (b, infolistf iib))), 
         sto, 
         statxs +> List.map (visitor_statement_k_s bigf) 
         ),
        infolistf ii

  in f (k, bigf) d 

and visitor_program_k_s = fun bigf p -> 
  let f = bigf.kprogram_s in
  let infolistf ii = List.map (visitor_info_k_s bigf) ii in
  let rec k p = 
    match p with
    | Declaration decl -> Declaration (visitor_decl_k_s bigf decl)
    | Definition def -> Definition (visitor_def_k_s bigf def)
    | EmptyDef ii -> EmptyDef (infolistf ii)
    | SpecialDeclMacro (s, xs, ii) -> 
        SpecialDeclMacro 
          (s, 
           xs +> List.map (fun (elem, iicomma) -> 
             visitor_argument_k_s bigf elem, infolistf iicomma
            ),
           infolistf ii
          )
    | CPPInclude (s, ii) -> CPPInclude (s, infolistf ii)
    | CPPDefine (ss, ii) -> CPPDefine (ss, infolistf ii)
    | NotParsedCorrectly ii -> NotParsedCorrectly (infolistf ii)
    | FinalDef info -> FinalDef (visitor_info_k_s bigf info)
  in f (k, bigf) p
  

and visitor_info_k_s = fun bigf info -> 
  let rec infof ii = bigf.kinfo_s (k, bigf) ii
  and k i = i
  in
  infof info

  
