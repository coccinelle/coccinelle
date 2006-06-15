open Common open Commonop
(* 
 can either:
  - do also a kind of inferer
     * can first do a simple inferer, that just pass context
     * then a real inferer, managing partial info.
  - extract the information from the .h files

 todo: expression contain types, and statements,   which in turn can contain
  expression, so need recurse. Need define an annote_statement and annotate_type.
*)


open Ast_c

type environment = (string, fullType) Common.assoc

type context = fullType option

let boolType = Ast_c.nullQualif, Ast_c.defaultInt
let mktyp x = Ast_c.nullQualif, x

let rec (annotate_expr: environment -> context -> expression -> expression) = fun env ctx -> fun expr -> 
  let rec aux expr = 
  match expr with
    | Ident (s), typ, i -> 
        let typ' = raise Todo in
        Ident (s), typ', i
    | Constant (c), typ, is -> 
        let typ' = raise Todo in
        Constant (c), typ', is
    | FunCall  (e, es), typ, is         -> 
        let typ' = raise Todo in
        let e'  = aux e  in 
        (* the commas info are in snd, in fst there is the paramtype_or_expr *)
        let es' = (es +> List.map fst)  +> List.map (fun e -> 
        match e with
        | Left e -> Left (aux e)
        | Right e -> raise Todo
      )
      in  FunCall (e', (zip es' (es +> List.map snd))), typ', is
    | CondExpr (e1, e2, e3), typ, is    -> 
        let typ' = raise Todo in

        let e1' = aux e1 in let e2' = aux e2 in let e3' =  aux e3 in  
        CondExpr (e1', e2', e3'), typ', is
    | Sequence (e1, e2), typ, is        -> 
        let typ' = raise Todo in
        let e1' = aux e1 in let e2' = aux e2 in                             
        Sequence (e1', e2'), typ', is
    | Assignment (e1, op, e2), typ, is  -> 
        let typ' = raise Todo in
        let e1' = aux e1 in let e2' = aux e2 in                             
        Assignment (e1', op,  e2'), typ', is
        
    | Postfix  (e, op), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in 
        Postfix (e', op), typ', is
    | Infix    (e, op), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in 
        Infix   (e', op), typ', is
    | Unary    (e, op), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in 
        Unary   (e', op), typ', is
    | Binary   (e1, op, e2), typ, is -> 
        let typ' = raise Todo in
        let e1' = aux e1 in let e2' = aux e2 in                                
        Binary (e1', op,  e2'), typ', is
        
    | ArrayAccess    (e1, e2), typ, is -> 
        let typ' = raise Todo in
        let e1' = aux e1 in let e2' = aux e2 in                              
        ArrayAccess (e1', e2'), typ', is
    | RecordAccess   (e, s), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in 
        RecordAccess     (e', s), typ', is 
    | RecordPtAccess (e, s), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in 
        RecordPtAccess   (e', s), typ', is 

    | SizeOfExpr  (e), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in  
        SizeOfExpr   (e'), typ', is
    | SizeOfType  (t), typ, is -> 
        let typ' = raise Todo in
        let t' = raise Todo in 
        SizeOfType (t'), typ', is
    | Cast    (t, e), typ, is -> 
        let typ' = raise Todo in
        let t' = raise Todo in let e' = aux e in 
        Cast   (t', e'), typ', is

    | StatementExpr (((declxs_statxs), is)), typ, is2 -> 
        let typ' = raise Todo in
        let declxs_statxs' = declxs_statxs +> List.map (function 
            Left decl ->  raise Todo
          | Right stat -> raise Todo
        )
        in StatementExpr (((declxs_statxs'), is)), typ', is2 
    | Constructor,typ, is -> 
        let typ' = raise Todo in
        Constructor,typ', is
    | NoExpr,typ, is -> 
        let typ' = raise Todo in
        NoExpr,typ',is
    | ParenExpr (e), typ, is -> 
        let typ' = raise Todo in
        let e' = aux e in 
        ParenExpr (e'), typ', is
    | x -> error_cant_have x
  in aux expr

let rec (annotate_program: environment -> program -> program) = fun env ctx -> 

  (* use visitor_synth *)
  (* catch all the decl to grow the environment *)
  raise Todo
