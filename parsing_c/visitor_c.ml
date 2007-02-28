open Common open Commonop

open Ast_c
module F = Control_flow_c

(*****************************************************************************)
(* Visitor based on continuation. Cleaner than the one based on mutable 
 * pointer functions. src: based on a (vague) idea from remy douence.
 * 
 * 
 * 
 * Diff with Julia's visitor ? She does:
 * 
 * let ident r k i =
 *  ...
 * let expression r k e =
 *  ... 
 *   ... (List.map r.V0.combiner_expression expr_list) ...
 *  ...
 * let res = V0.combiner bind option_default 
 *   mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
 *   donothing donothing donothing donothing
 *   ident expression typeC donothing parameter declaration statement
 *   donothing in
 * ...
 * collect_unitary_nonunitary
 *   (List.concat (List.map res.V0.combiner_top_level t))
 * 
 * 
 * 
 * So she has to remember at which position you must put the 'expression'
 * function. I use record which is easier. 
 * 
 * When she calls recursively, her res.V0.combiner_xxx does not take bigf
 * in param whereas I do 
 *   | F.Decl decl -> Visitor_c.vk_decl bigf decl 
 * And with the record she gets, she does not have to do my
 * multiple defs of function such as 'let al_type = V0.vk_type_s bigf'
 * 
 * The code of visitor.ml is cleaner with julia, because mutual recursive calls
 * are clean such as ... 'expression e' ... and not  'f (k, bigf) e'
 * or 'vk_expr bigf e'.
 * 
 * So it is very dual:
 * - I give a record but then I must handle bigf.
 * - She gets a record, and gives a list of function
 * 
 *) 
(*****************************************************************************)
 
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

(*****************************************************************************)
(* Side effect style visitor *)
(*****************************************************************************)

(* full visitors for all langage concept,  not just for expression *)
type visitor_c = 
 { 
   kexpr:      (expression  -> unit) * visitor_c -> expression  -> unit;
   kstatement: (statement   -> unit) * visitor_c -> statement   -> unit;
   ktype:      (fullType    -> unit) * visitor_c -> fullType    -> unit;

   kdecl:      (declaration -> unit) * visitor_c -> declaration -> unit;
   kdef:       (definition  -> unit) * visitor_c -> definition  -> unit; 
   kini:       (initialiser  -> unit) * visitor_c -> initialiser  -> unit; 

   kinfo: (info -> unit) * visitor_c -> info -> unit;
   
   knode: (F.node -> unit) * visitor_c -> F.node -> unit;
 } 

let default_visitor_c = 
  { kexpr =      (fun (k,_) e  -> k e);
    kstatement = (fun (k,_) st -> k st);
    ktype      = (fun (k,_) t  -> k t);
    kdecl      = (fun (k,_) d  -> k d);
    kdef       = (fun (k,_) d  -> k d);
    kini       = (fun (k,_) ie  -> k ie);
    kinfo      = (fun (k,_) ii  -> k ii);
    knode      = (fun (k,_) n  -> k n);
  } 


let rec vk_expr = fun bigf expr ->
  let iif ii = List.iter (vk_info bigf) ii in

  let rec exprf e = bigf.kexpr (k,bigf) e
  and k ((e,typ), ii) = 
    iif ii;
    match e with
    | Ident (s) -> ()
    | Constant (c) -> ()
    | FunCall  (e, es)         -> 
        exprf e;  
        es +> List.iter (fun (e, ii) -> 
          iif ii;
          vk_argument bigf e
          );
    | CondExpr (e1, e2, e3)    -> 
        exprf e1; do_option (exprf) e2; exprf e3
    | Sequence (e1, e2)        -> exprf e1; exprf e2;
    | Assignment (e1, op, e2)  -> exprf e1; exprf e2;
        
    | Postfix  (e, op) -> exprf e
    | Infix    (e, op) -> exprf e
    | Unary    (e, op) -> exprf e
    | Binary   (e1, op, e2) -> exprf e1; exprf  e2;
        
    | ArrayAccess    (e1, e2) -> exprf e1; exprf e2;
    | RecordAccess   (e, s) -> exprf e
    | RecordPtAccess (e, s) -> exprf e

    | SizeOfExpr  (e) -> exprf e
    | SizeOfType  (t) -> vk_type bigf t
    | Cast    (t, e) -> vk_type bigf t; exprf e

    (* old: | StatementExpr (((declxs, statxs), is)), is2 -> 
     *          List.iter (vk_decl bigf) declxs; 
     *          List.iter (vk_statement bigf) statxs 
     *)
    | StatementExpr ((statxs, is)) -> 
        iif is;
        statxs +> List.iter (vk_statement bigf);

    (* TODO, we will certainly have to then do a special visitor for 
     * initializer 
     *)
    | Constructor -> ()
          
    | ParenExpr (e) -> exprf e


  in exprf expr

and vk_argument = fun bigf arg -> 
  let iif ii = List.iter (vk_info bigf) ii in

  let rec do_action = function 
    | (ActMisc ii) -> iif ii
  in
  match arg with
  | Left e -> (vk_expr bigf) e
  | Right (ArgType param) -> vk_param bigf param
  | Right (ArgAction action) -> do_action action




and vk_statement = fun bigf st -> 
  let iif ii = List.iter (vk_info bigf) ii in

  let rec statf x = bigf.kstatement (k,bigf) x 
  and k st = 
    let (unwrap_st, ii) = st in
    iif ii;
    match unwrap_st with
    | Labeled (Label (s, st)) -> statf  st;
    | Labeled (Case  (e, st)) -> vk_expr bigf e; statf st;
    | Labeled (CaseRange  (e, e2, st)) -> 
        vk_expr bigf e; vk_expr bigf e2; statf st;
    | Labeled (Default st) -> statf st;

    | Compound statxs -> statxs +> List.iter (vk_statement bigf)
    | ExprStatement (eopt) -> do_option (vk_expr bigf) eopt;

    | Selection  (If (e, st1, st2)) -> 
        vk_expr bigf e; statf st1; statf st2;
    | Selection (IfCpp (st1s, st2s)) -> 
        st1s +> List.iter (vk_statement bigf);
        st2s +> List.iter (vk_statement bigf)
    | Selection  (Switch (e, st)) -> 
        vk_expr bigf e; statf st;
    | Iteration  (While (e, st)) -> 
        vk_expr bigf e; statf st;
    | Iteration  (DoWhile (st, e)) -> statf st; vk_expr bigf e; 
    | Iteration  (For ((e1opt,i1), (e2opt,i2), (e3opt,i3), st)) -> 
        statf (ExprStatement (e1opt),i1); 
        statf (ExprStatement (e2opt),i2); 
        statf (ExprStatement (e3opt),i3); 
        statf st;
          
    | Jump (Goto s) -> ()
    | Jump ((Continue|Break|Return)) -> ()
    | Jump (ReturnExpr e) -> vk_expr bigf e;

    | Decl decl -> vk_decl bigf decl 
    | Asm -> ()
    | MacroStmt -> ()

  in statf st

and vk_type = fun bigf t -> 
  let iif ii = List.iter (vk_info bigf) ii in

  let rec typef x = bigf.ktype (k, bigf) x 
  and k t = 
    let (q, t) = t in
    let (unwrap_q, iiq) = q in
    let (unwrap_t, iit) = t in
    iif iiq;
    iif iit;
    match unwrap_t with
    | BaseType _ -> ()
    | Pointer t -> typef t
    | Array (eopt, t) -> 
        do_option (vk_expr bigf) eopt;
        typef t 
    | FunctionType (returnt, paramst) -> 
        typef returnt;
        (match paramst with
        | (ts, (b,iihas3dots)) -> 
            iif iihas3dots;
            ts +> List.iter (fun (param,iicomma) -> 
              vk_param bigf param;
              iif iicomma;
              
            )
        )

    | Enum  (sopt, enumt) -> 
        enumt +> List.iter (fun (((s, eopt),ii_s_eq), iicomma) -> 
          iif ii_s_eq; iif iicomma;
          eopt +> do_option (vk_expr bigf)
          );    
        
    | StructUnion (sopt, (_su, fields)) -> 

       fields +> List.iter (fun (FieldDeclList onefield_multivars, ii) -> 
         iif ii;
         onefield_multivars +> List.iter (fun (field, iicomma) ->
           iif iicomma;
           match field with
           | Simple (s, t), ii -> iif ii; typef t;
           | BitField (sopt, t, expr), ii -> 
               iif ii;
               vk_expr bigf expr;
               typef t 
         ))


    | StructUnionName (s, structunion) -> ()
    | EnumName  s -> ()

    | TypeName (s) -> ()

    | ParenType t -> typef t


  in typef t

and vk_decl = fun bigf d -> 
  let iif ii = List.iter (vk_info bigf) ii in

  let f = bigf.kdecl in 
  let rec k (DeclList (xs,ii)) = iif ii; List.iter aux xs 
  and aux ((var, t, sto), iicomma) = 
    iif iicomma;
    vk_type bigf t;
    var +> do_option (fun ((s, ini), ii_s_ini) -> 
      iif ii_s_ini;
      ini +> do_option (vk_ini bigf)
        );
  in f (k, bigf) d 

and vk_ini = fun bigf ini -> 
  let iif ii = List.iter (vk_info bigf) ii in

  let rec inif x = bigf.kini (k, bigf) x 
  and k (ini, iini) = 
    iif iini;
    match ini with
    | InitExpr e -> vk_expr bigf e
    | InitList initxs -> List.iter (inif) (initxs +> List.map fst)
    | InitGcc (s, e) -> inif e
    | InitGccIndex (e1, e) -> vk_expr bigf e1; inif e
    | InitGccRange (e1, e2, e) -> 
        vk_expr bigf e1; 
        vk_expr bigf e2; 
        inif e
  in inif ini

and vk_def = fun bigf d -> 
  let iif ii = List.iter (vk_info bigf) ii in

  let f = bigf.kdef in
  let rec k d = 
    match d with
    | (s, (returnt, (paramst, (b, iib))), sto, statxs), ii -> 
        iif ii;
        iif iib;
        vk_type bigf returnt;
        paramst +> List.iter (fun (param,iicomma) -> 
          vk_param bigf param;
          iif iicomma;
        );
        statxs +> List.iter (vk_statement bigf)
  in f (k, bigf) d 


(* Now keep fullstatement inside the control flow node, 
 * so that can then get in a MetaStmtVar the fullstatement to later
 * pp back when the S is in a +. But that means that 
 * Exp will match an Ifnode even if there is no such exp
 * inside the condition of the Ifnode (because the exp may
 * be deeper, in the then branch). So have to not visit
 * all inside a node anymore.
 * 
 * update: j'ai choisi d'accrocher au noeud du CFG à la
 * fois le fullstatement et le partialstatement et appeler le 
 * visiteur que sur le partialstatement.
 *)

and vk_node = fun bigf node -> 
  let iif ii = List.iter (vk_info bigf) ii in
  let infof info = vk_info bigf info in

  let f = bigf.knode in
  let rec k n = 
    match F.unwrap n with

    | F.FunHeader ((idb, (rett, (paramst,(isvaargs,iidotsb))), stob),ii) ->
        vk_type bigf rett;
        paramst +> List.iter (fun (param, iicomma) ->
          vk_param bigf param;
          iif iicomma;
        );


    | F.Decl decl -> vk_decl bigf decl 
    | F.ExprStatement (st, (eopt, ii)) ->  
        iif ii;
        eopt +> do_option (vk_expr bigf)

    | F.IfHeader (_, (e,ii)) 
    | F.SwitchHeader (_, (e,ii))
    | F.WhileHeader (_, (e,ii))
    | F.DoWhileTail (e,ii) -> 
        iif ii;
        vk_expr bigf e

    | F.ForHeader (_st, (((e1opt,i1), (e2opt,i2), (e3opt,i3)), ii)) -> 
        iif i1; iif i2; iif i3;
        iif ii;
        e1opt +> do_option (vk_expr bigf);
        e2opt +> do_option (vk_expr bigf);
        e3opt +> do_option (vk_expr bigf);
        
    | F.ReturnExpr (_st, (e,ii)) -> iif ii; vk_expr bigf e
        
    | F.Case  (_st, (e,ii)) -> iif ii; vk_expr bigf e
    | F.CaseRange (_st, ((e1, e2),ii)) -> 
        iif ii; vk_expr bigf e1; vk_expr bigf e2


    | F.CaseNode i -> ()

    | F.CPPDefine (s, ii) -> iif ii
    | F.CPPInclude (s, ii) -> iif ii
    | F.IfCpp (st, ((),ii)) -> iif ii

    | F.Break    (st,((),ii)) -> iif ii
    | F.Continue (st,((),ii)) -> iif ii
    | F.Default  (st,((),ii)) -> iif ii
    | F.Return   (st,((),ii)) -> iif ii
    | F.Goto  (st, (s,ii)) -> iif ii
    | F.Label (st, (s,ii)) -> iif ii
    | F.EndStatement iopt -> do_option infof iopt
    | F.DoHeader (st, info) -> infof info
    | F.Else info -> infof info
    | F.SeqEnd (i, info) -> infof info
    | F.SeqStart (st, i, info) -> infof info

    | F.Macro (st, ((),ii)) -> iif ii

    | (
        F.ErrorExit|F.Exit|
        F.FallThroughNode|F.AfterNode|F.FalseNode|F.TrueNode|
        F.Fake|F.Enter|F.Asm
      ) -> ()



  in
  f (k, bigf) node

and vk_info = fun bigf info -> 
  let rec infof ii = bigf.kinfo (k, bigf) ii
  and k i = ()
  in
  infof info


and vk_param = fun bigf (((b, s, t), ii_b_s)) ->  
  let iif ii = List.iter (vk_info bigf) ii in
  iif ii_b_s;
  vk_type bigf t


let vk_args_splitted = fun bigf args_splitted -> 
  let iif ii = List.iter (vk_info bigf) ii in
  args_splitted +> List.iter (function  
  | Left arg -> vk_argument bigf arg
  | Right ii -> iif ii
  )



let vk_params_splitted = fun bigf args_splitted -> 
  let iif ii = List.iter (vk_info bigf) ii in
  args_splitted +> List.iter (function  
  | Left arg -> vk_param bigf arg
  | Right ii -> iif ii
  )
  

(*****************************************************************************)
(* "syntetisized attributes" style *)
(*****************************************************************************)
type 'a inout = 'a -> 'a 

(* _s for synthetizized attributes *)
type visitor_c_s = { 
  kexpr_s:      (expression inout * visitor_c_s) -> expression inout;
  kstatement_s: (statement  inout * visitor_c_s) -> statement  inout;
  ktype_s:      (fullType   inout * visitor_c_s) -> fullType   inout;
  kini_s:  (initialiser  inout * visitor_c_s) -> initialiser inout; 

  kdecl_s: (declaration  inout * visitor_c_s) -> declaration inout;
  kdef_s:  (definition   inout * visitor_c_s) -> definition  inout; 

  kprogram_s: (programElement inout * visitor_c_s) -> programElement inout;
  knode_s: (F.node inout * visitor_c_s) -> F.node inout;

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
    knode_s      = (fun (k,_) n  -> k n);
    kinfo_s      = (fun (k,_) i  -> k i);
  } 

let rec vk_expr_s = fun bigf expr ->
  let infolistf ii = List.map (vk_info_s bigf) ii in
  let rec exprf e = bigf.kexpr_s  (k, bigf) e
  and k e = 
    let ((unwrap_e, typ), ii) = e in
    (* don't analyse optional type
     * old:  typ +> map_option (vk_type_s bigf) in 
     *)
    let typ' = typ in 
    let e' = 
      match unwrap_e with
      | Ident (s) -> Ident (s)
      | Constant (c) -> Constant (c)
      | FunCall  (e, es)         -> 
          FunCall (exprf e,
                  es +> List.map (fun (e,ii) -> 
                    vk_argument_s bigf e, infolistf ii
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
      | SizeOfType  (t) -> SizeOfType (vk_type_s bigf t)
      | Cast    (t, e) ->  Cast   (vk_type_s bigf t, exprf e)

      | StatementExpr (statxs, is) -> 
          StatementExpr (
            statxs +> List.map (vk_statement_s bigf),
            infolistf is)
      | Constructor -> Constructor
      | ParenExpr (e) -> ParenExpr (exprf e)

    in
    (e', typ'), (infolistf ii)
  in exprf expr

and vk_argument_s bigf argument = 
  let infolistf ii = List.map (vk_info_s bigf) ii in
  let rec do_action = function 
    | (ActMisc ii) -> ActMisc (infolistf ii)
  in
  (match argument with
  | Left e -> Left (vk_expr_s bigf e)
  | Right (ArgType param) ->    Right (ArgType (vk_param_s bigf param))
  | Right (ArgAction action) -> Right (ArgAction (do_action action))
  )






and vk_statement_s = fun bigf st -> 
  let rec statf st = bigf.kstatement_s (k, bigf) st 
  and k st = 
    let (unwrap_st, ii) = st in
    let st' = 
      match unwrap_st with
      | Labeled (Label (s, st)) -> 
          Labeled (Label (s, statf st))
      | Labeled (Case  (e, st)) -> 
          Labeled (Case  ((vk_expr_s bigf) e , statf st))
      | Labeled (CaseRange  (e, e2, st)) -> 
          Labeled (CaseRange  ((vk_expr_s bigf) e, 
                              (vk_expr_s bigf) e2, 
                              statf st))
      | Labeled (Default st) -> Labeled (Default (statf st))
      | Compound statxs -> 
          Compound (statxs +> List.map (vk_statement_s bigf))
      | ExprStatement (None) ->  ExprStatement (None)
      | ExprStatement (Some e) -> ExprStatement (Some ((vk_expr_s bigf) e))
      | Selection (If (e, st1, st2)) -> 
          Selection  (If ((vk_expr_s bigf) e, statf st1, statf st2))
      | Selection (IfCpp (st1s, st2s)) -> 
          Selection  (IfCpp 
                         (st1s +> List.map (vk_statement_s bigf),
                         st2s +> List.map (vk_statement_s bigf)))
      | Selection (Switch (e, st))   -> 
          Selection  (Switch ((vk_expr_s bigf) e, statf st))
      | Iteration (While (e, st))    -> 
          Iteration  (While ((vk_expr_s bigf) e, statf st))
      | Iteration (DoWhile (st, e))  -> 
          Iteration  (DoWhile (statf st, (vk_expr_s bigf) e))
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
      | Jump (ReturnExpr e) -> Jump (ReturnExpr ((vk_expr_s bigf) e))

      | Decl decl -> Decl (vk_decl_s bigf decl)
      | Asm -> Asm
      | MacroStmt -> MacroStmt
    in
    st', List.map (vk_info_s bigf) ii
  in statf st

and vk_type_s = fun bigf t -> 
  let rec typef t = bigf.ktype_s (k,bigf) t
  and infolistf ii = List.map (vk_info_s bigf) ii
  and k t = 
    let (q, t) = t in
    let (unwrap_q, iiq) = q in
    let q' = unwrap_q in     (* todo? a visitor for qualifier *)
    let (unwrap_t, iit) = t in
    let t' = 
      match unwrap_t with
      | BaseType x -> BaseType x
      | Pointer t  -> Pointer (typef t)
      | Array (eopt, t) -> Array (fmap (vk_expr_s bigf) eopt, typef t) 
      | FunctionType (returnt, paramst) -> 
          FunctionType 
            (typef returnt, 
            (match paramst with
            | (ts, (b, iihas3dots)) -> 
                (ts +> List.map (fun (param,iicomma) -> 
                  (vk_param_s bigf param, infolistf iicomma)),
                (b, infolistf iihas3dots))
            ))

      | Enum  (sopt, enumt) -> 
          Enum (sopt,
               enumt +> List.map (fun (((s, eopt),ii_s_eq), iicomma) -> 
                 ((s, fmap (vk_expr_s bigf) eopt), infolistf ii_s_eq),
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
                                       BitField (sopt, typef t, vk_expr_s bigf expr), 
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

and vk_decl_s = fun bigf d -> 
  let f = bigf.kdecl_s in 
  let infolistf ii = List.map (vk_info_s bigf) ii in
  let rec k (DeclList (xs, ii)) = DeclList (List.map aux xs,   infolistf ii)
  and aux ((var, t, sto), iicomma) = 
    ((var +> map_option (fun ((s, ini), ii_s_ini) -> 
      (s, ini +> map_option (fun init -> vk_ini_s bigf init)),
      infolistf ii_s_ini
    )
    ),
    vk_type_s bigf t, 
    sto),
  infolistf iicomma

  in f (k, bigf) d 

and vk_ini_s = fun bigf ini -> 
  let rec inif ini = bigf.kini_s (k,bigf) ini
  and k ini = 
    let (unwrap_ini, ii) = ini in
    let ini' = 
      match unwrap_ini with
      | InitExpr e -> InitExpr (vk_expr_s bigf e)
      | InitList initxs -> 
          InitList (initxs +> List.map (fun (ini, ii) -> 
            inif ini, List.map (vk_info_s bigf) ii) 
          )
      | InitGcc (s, e) -> InitGcc (s, inif e)
      | InitGccIndex (e1, e) -> 
          InitGccIndex (vk_expr_s bigf e1 , inif e)
      | InitGccRange (e1, e2, e) -> 
          InitGccRange (vk_expr_s bigf e1, vk_expr_s bigf e2, inif e)
    in ini', List.map (vk_info_s bigf) ii
  in inif ini


and vk_def_s = fun bigf d -> 
  let f = bigf.kdef_s in
  let infolistf ii = List.map (vk_info_s bigf) ii in
  let rec k d = 
    match d with
    | (s, (returnt, (paramst, (b, iib))), sto, statxs), ii  -> 
        (s, 
        (vk_type_s bigf returnt, 
        (paramst +> List.map (fun (param, iicomma) ->
          (vk_param_s bigf param, infolistf iicomma)
        ), 
        (b, infolistf iib))), 
        sto, 
        statxs +> List.map (vk_statement_s bigf) 
        ),
        infolistf ii

  in f (k, bigf) d 

and vk_program_s = fun bigf p -> 
  let f = bigf.kprogram_s in
  let infolistf ii = List.map (vk_info_s bigf) ii in
  let rec k p = 
    match p with
    | Declaration decl -> Declaration (vk_decl_s bigf decl)
    | Definition def -> Definition (vk_def_s bigf def)
    | EmptyDef ii -> EmptyDef (infolistf ii)
    | SpecialDeclMacro (s, xs, ii) -> 
        SpecialDeclMacro 
          (s, 
          xs +> List.map (fun (elem, iicomma) -> 
            vk_argument_s bigf elem, infolistf iicomma
          ),
          infolistf ii
          )
    | CPPInclude (s, ii) -> CPPInclude (s, infolistf ii)
    | CPPDefine (ss, ii) -> CPPDefine (ss, infolistf ii)
    | NotParsedCorrectly ii -> NotParsedCorrectly (infolistf ii)
    | FinalDef info -> FinalDef (vk_info_s bigf info)
  in f (k, bigf) p
  

and vk_info_s = fun bigf info -> 
  let rec infof ii = bigf.kinfo_s (k, bigf) ii
  and k i = i
  in
  infof info


and vk_node_s = fun bigf node -> 
  let iif ii = List.map (vk_info_s bigf) ii in
  let infof info = vk_info_s bigf info  in

  let rec nodef n = bigf.knode_s (k, bigf) n
  and k node = 
    F.rewrap node (
    match F.unwrap node with
    | F.FunHeader ((idb, (rett, (paramst,(isvaargs,iidotsb))), stob),ii) ->
        F.FunHeader 
          ((idb,
           (vk_type_s bigf rett,
           (paramst +> List.map (fun (param, iicomma) ->
             (vk_param_s bigf param, iif iicomma)
           ), (isvaargs,iif iidotsb))), stob),iif ii)
          
          
    | F.Decl declb -> F.Decl (vk_decl_s bigf declb)
    | F.ExprStatement (st, (eopt, ii)) ->  
        F.ExprStatement (st, (eopt +> map_option (vk_expr_s bigf), iif ii))
          
    | F.IfHeader (st, (e,ii))     -> 
        F.IfHeader    (st, (vk_expr_s bigf e, iif ii))
    | F.SwitchHeader (st, (e,ii)) -> 
        F.SwitchHeader(st, (vk_expr_s bigf e, iif ii))
    | F.WhileHeader (st, (e,ii))  -> 
        F.WhileHeader (st, (vk_expr_s bigf e, iif ii))
    | F.DoWhileTail (e,ii)  -> 
        F.DoWhileTail (vk_expr_s bigf e, iif ii)

    | F.ForHeader (st, (((e1opt,i1), (e2opt,i2), (e3opt,i3)), ii)) -> 
        F.ForHeader (st,
                    (((e1opt +> Common.map_option (vk_expr_s bigf), iif i1),
                     (e2opt +> Common.map_option (vk_expr_s bigf), iif i2),
                     (e3opt +> Common.map_option (vk_expr_s bigf), iif i3)),
                    iif ii))
          
    | F.ReturnExpr (st, (e,ii)) -> 
        F.ReturnExpr (st, (vk_expr_s bigf e, iif ii))
        
    | F.Case  (st, (e,ii)) -> F.Case (st, (vk_expr_s bigf e, ii))
    | F.CaseRange (st, ((e1, e2),ii)) -> 
        F.CaseRange (st, ((vk_expr_s bigf e1, vk_expr_s bigf e2), iif ii))

    | F.CaseNode i -> F.CaseNode i

    | F.CPPDefine (s, ii) -> F.CPPDefine (s, iif ii)
    | F.CPPInclude (s, ii) -> F.CPPInclude (s, iif ii)
    | F.IfCpp (st, ((),ii)) -> F.IfCpp (st, ((),iif ii))

    | F.Macro (st, ((),ii)) -> F.Macro (st, ((),iif ii))

    | F.Break    (st,((),ii)) -> F.Break    (st,((),iif ii))
    | F.Continue (st,((),ii)) -> F.Continue (st,((),iif ii))
    | F.Default  (st,((),ii)) -> F.Default  (st,((),iif ii))
    | F.Return   (st,((),ii)) -> F.Return   (st,((),iif ii))
    | F.Goto  (st, (s,ii)) -> F.Goto  (st, (s,iif ii))
    | F.Label (st, (s,ii)) -> F.Label (st, (s,iif ii))
    | F.EndStatement iopt -> F.EndStatement (map_option infof iopt)
    | F.DoHeader (st, info) -> F.DoHeader (st, infof info)
    | F.Else info -> F.Else (infof info)
    | F.SeqEnd (i, info) -> F.SeqEnd (i, infof info)
    | F.SeqStart (st, i, info) -> F.SeqStart (st, i, infof info)

    | ((F.ErrorExit|F.FallThroughNode|F.AfterNode|F.FalseNode|F.TrueNode|
       F.Fake|F.Exit|F.Enter|F.Asm) as x) -> x


    )
  in
  nodef node
  
and vk_param_s = fun bigf ((b, s, t), ii_b_s) -> 
  let iif ii = List.map (vk_info_s bigf) ii in
  ((b, s, vk_type_s bigf t), iif ii_b_s)
        
let vk_args_splitted_s = fun bigf args_splitted -> 
  let iif ii = List.map (vk_info_s bigf) ii in
  args_splitted +> List.map (function  
  | Left arg -> Left (vk_argument_s bigf arg)
  | Right ii -> Right (iif ii)
  )


let vk_params_splitted_s = fun bigf args_splitted -> 
  let iif ii = List.map (vk_info_s bigf) ii in
  args_splitted +> List.map (function  
  | Left arg -> Left (vk_param_s bigf arg)
  | Right ii -> Right (iif ii)
  )
