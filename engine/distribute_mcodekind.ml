open Common open Commonop

open Ast_c

type 'a distributer = 
    (Ast_c.info -> Ast_c.info) * 
    (Ast_c.info -> Ast_c.info) * 
    (Ast_c.info -> Ast_c.info) -> 
    'a -> 'a


(* -------------------------------------------------------------------------- *)
let (minusize_token: Ast_c.info -> Ast_c.info) = fun (s, (mcode,env))  -> 
  let mcode' =
    match mcode with
    | Ast_cocci.CONTEXT (Ast_cocci.NOTHING) -> Ast_cocci.MINUS ([])
    | _ -> failwith "have already minused this token"
  in
  (s, (mcode', env))



let add_left (xxs, binding) = fun (s, (mcode,env))  -> 
  let mcode' = 
    match mcode with
    | Ast_cocci.MINUS ([]) -> Ast_cocci.MINUS (xxs)
    | Ast_cocci.MINUS (x::xs) -> 
        failwith "have already added stuff on this token"

    | Ast_cocci.CONTEXT (Ast_cocci.NOTHING) -> 
        Ast_cocci.CONTEXT (Ast_cocci.BEFORE xxs)
    | Ast_cocci.CONTEXT (Ast_cocci.AFTER yys) -> 
        Ast_cocci.CONTEXT (Ast_cocci.BEFOREAFTER (xxs, yys))
    | _ -> raise Impossible

  in
  s, (mcode', binding)


let add_right (yys, binding) = fun (s,(mcode,env))  -> 
  let mcode' = 
    match mcode with
    | Ast_cocci.MINUS ([]) -> 
        Ast_cocci.MINUS (yys)
    | Ast_cocci.MINUS (_) -> failwith "have already added stuff on this token"


    | Ast_cocci.CONTEXT (Ast_cocci.NOTHING) -> 
        Ast_cocci.CONTEXT (Ast_cocci.AFTER yys)
    | Ast_cocci.CONTEXT (Ast_cocci.BEFORE xxs) -> 
        Ast_cocci.CONTEXT (Ast_cocci.BEFOREAFTER (xxs, yys))
    | _ -> raise Impossible
  in
  s, (mcode', binding)


let no_minusize x = x
let nothing_right x = x
let nothing_left  x = x


(* -------------------------------------------------------------------------- *)

let  (distribute_mck: 
  Ast_cocci.mcodekind -> 'a distributer -> 'a -> Ast_c.metavars_binding -> 'a) =
 fun mcodekind distributef expr binding ->
  match mcodekind with
  | Ast_cocci.MINUS (any_xxs) -> 
      distributef 
        (minusize_token, add_left (any_xxs, binding), nothing_right)
        expr
  | Ast_cocci.CONTEXT (any_befaft) -> 
        (match any_befaft with
        | Ast_cocci.NOTHING -> expr

        | Ast_cocci.BEFORE xxs -> 
            distributef
              (no_minusize, add_left (xxs, binding), nothing_right)
              expr
        | Ast_cocci.AFTER xxs ->  
            distributef
              (no_minusize, nothing_left, add_right (xxs, binding))
              expr
        | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
            distributef
              (no_minusize, add_left (xxs, binding) , add_right (xxs, binding))
              expr
        )
  | Ast_cocci.PLUS -> raise Impossible





(* -------------------------------------------------------------------------- *)
(* Could do the minus more easily by extending visitor_c.ml and adding a 
   function applied to every mcode. But as I also need to do the add_left and 
   add_right, which requires to do a different thing for each case, I have not
   defined this not-so-useful visitor.
   op = minusize operator.
   lop = stuff to do on the left.
   rop = stuff to do on the right.
*)

let rec (distribute_mck_e: Ast_c.expression distributer) = fun (op, lop, rop) ->
 function
  | Ident s,  typ,[i1] -> 
      Ident s,  
      typ, [i1 +> op +> lop +> rop] 
  | Constant (String s),        typ, is     -> 
      Constant (String s), typ,
      (match is with
      | [] -> raise Impossible
      | [i] -> [i +> op +> lop +> rop]
      | x::y::xs -> 
          let (head, middle, tail) = head_middle_tail (x::y::xs) in
          [head +> op +> lop] @ List.map op middle @ [tail +> op +> rop]
      )
  (* only a String can have multiple ii *)
  | Constant c,  typ, [i1] -> 
      Constant c,  
      typ, [i1 +> op +> lop +> rop]

  | FunCall (e, xs), typ,[i2;i3] -> 
      FunCall 
        (distribute_mck_e (op, lop, nothing_right) e,
         xs +> List.map (function 
           | (Left e, ii) -> 
               Left (distribute_mck_e (op, nothing_left, nothing_right) e),
               (ii +> List.map op)
           | (Right e, ii) -> failwith "not handling type in funcall"
                        ) 
        ),
      typ, [i2 +> op; i3 +> op +> rop]

  | CondExpr (e1, e2, e3),    typ,[i1;i2]    -> 
      CondExpr 
        (distribute_mck_e (op, lop, nothing_right) e1,
         map_option (distribute_mck_e (op, nothing_left, nothing_left)) e2,
         distribute_mck_e (op, nothing_left, rop) e3),
      typ, [i1 +> op; i2 +> op]
  | Sequence (e1, e2),          typ,[i]  -> 
      Sequence
        (distribute_mck_e (op, lop, nothing_right) e1,
         distribute_mck_e (op, nothing_left, rop) e2),
      typ, [i +> op]
  | Assignment (e1, opbis, e2),    typ,[i]  -> 
      Assignment
        (distribute_mck_e (op, lop, nothing_right) e1,
         opbis,
         distribute_mck_e (op, nothing_left, rop) e2),
      typ, [i +> op]

  | Postfix  (e, opbis),    typ,[i] -> 
      Postfix (distribute_mck_e (op, lop, nothing_right) e, opbis),
      typ, [i +> op +> rop]
         
  | Infix    (e, opbis),    typ,[i] -> 
      Infix (distribute_mck_e (op, nothing_left, rop) e, opbis),
      typ, [i +> op +> lop]

  | Unary    (e, opbis),    typ,[i] -> 
      Unary (distribute_mck_e (op, nothing_left, rop) e, opbis),
      typ, [i +> op +> lop]
  | Binary   (e1, opbis, e2),    typ,[i] -> 
      Binary
        (distribute_mck_e (op, lop, nothing_right) e1,
         opbis,
         distribute_mck_e (op, nothing_left, rop) e2),
      typ, [i +> op]


  | ArrayAccess    (e1, e2),   typ,[i1;i2] -> 
      ArrayAccess
        (distribute_mck_e (op, lop, nothing_right) e1,
         distribute_mck_e (op, nothing_left, nothing_right) e2),
      typ, [i1 +> op; i2 +> op +> rop]
  | RecordAccess (e, id), typ, [i1;i2] -> 
      RecordAccess (distribute_mck_e (op, lop, nothing_right) e, id), 
      typ, [i1 +> op; i2 +> op +> rop]
  | RecordPtAccess (e, id), typ, [i1;i2] -> 
      RecordPtAccess (distribute_mck_e (op, lop, nothing_right) e, id), 
      typ, [i1 +> op; i2 +> op +> rop]

  | SizeOfExpr  (e),     typ,[i] -> 
      SizeOfExpr (distribute_mck_e (op, nothing_left, rop) e),
      typ, [i +> op +> lop]
  | SizeOfType  (t),     typ,[i1;i2;i3] -> 
      SizeOfType (distribute_mck_type (op, nothing_left, nothing_right) t),
      typ, [i1 +> op +> lop; i2 +> op; i3 +> op +> rop]
  | Cast    (t, e),      typ,[i1;i2] -> 
      Cast 
        (distribute_mck_type (op, nothing_left, nothing_right) t,
         distribute_mck_e (op, nothing_left, rop) e),
      typ, [i1 +> op +> lop; i2 +> op]
      
  | StatementExpr (declxs_statxs, [ii1;ii2]),  typ,[i1;i2] -> 
      StatementExpr
        (declxs_statxs +> List.map (function
          | Left decl -> 
              Left (distribute_mck_decl (op, nothing_left, nothing_right) decl)
          | Right stat -> 
              Right (distribute_mck_stat (op, nothing_left, nothing_right) stat)
                                   ),
         [ii1 +> op; ii2 +> op]),
      typ, [i1 +> op +> lop; i2 +> op +> rop]

  | Constructor, typ,[] -> failwith "Constructor, what to do ? not enough info"

  | ParenExpr (e), typ,[i1;i2] -> 
      ParenExpr (distribute_mck_e (op, nothing_left, nothing_right) e),
      typ, [i1 +> op +> lop; i2 +> op +> rop]

  | MacroCall  (es),     typ,[i1;i2;i3] -> 
      failwith "MacroCall"

  | MacroCall2  (arg),     typ,[i1;i2;i3] -> 
      failwith "MacroCall2"

  | x -> raise Impossible


and (distribute_mck_decl: Ast_c.declaration distributer) = fun (op, lop, rop) ->
 fun decl ->
  raise Todo

and (distribute_mck_stat: Ast_c.statement distributer) = fun (op, lop, rop) -> 
 function

  | Labeled (Label (s, st)), [i1;i2] -> 
      Labeled (Label (s, distribute_mck_stat (op, nothing_left, rop) st)),
      [i1 +> op +> lop;i2 +> op]
  | Labeled (Case  (e, st)), [i1;i2] -> 
      Labeled (Case  (distribute_mck_e (op, nothing_left, nothing_right) e,
                      distribute_mck_stat (op, nothing_left, rop) st)),
      [i1 +> op +> lop; i2 +> op] 
  | Labeled (CaseRange  (e, e2, st)), _ -> raise Todo
  | Labeled (Default st), [i1;i2] -> 
      Labeled (Default (distribute_mck_stat (op, nothing_left, rop) st)),
      [i1 +> op +> lop; i2 +> op]

  | Compound (declxs_statxs), [i1;i2] -> 
      Compound 
        (declxs_statxs +> List.map (function
          | Left decl -> 
              Left (distribute_mck_decl (op, nothing_left, nothing_right) decl)
          | Right stat -> 
              Right (distribute_mck_stat (op, nothing_left, nothing_right) stat)
                )),
      [i1 +> op +> lop; i2 +> op +> rop]

  | ExprStatement None, [i] -> 
      ExprStatement None, 
      [i +> op +> lop +> rop]
  | ExprStatement None, [] -> 
      ExprStatement None, []
  | ExprStatement (Some e), [i] -> 
      ExprStatement (Some (distribute_mck_e (op, lop, nothing_right) e)),
      [i +> op +> rop]
   (* the last ExprStatement of a for does not have a trailing ';' hence the
      [] for ii  *)
  | ExprStatement (Some e), [] -> 
      ExprStatement (Some (distribute_mck_e (op, lop, rop) e)),
      []

  | Selection  (If (e, st1, st2)), i1::i2::i3::is -> 
      (match (st2, is) with
      | ((ExprStatement None, []), [])  -> 
          Selection 
            (If
               (distribute_mck_e (op, nothing_left, nothing_right) e,
                distribute_mck_stat (op, nothing_left, rop) st1,
                (ExprStatement None, []))),
          [i1 +> op +> lop; i2 +> op; i3 +> op]
             
          
      | st2, [i4] -> 
          Selection 
            (If
               (distribute_mck_e (op, nothing_left, nothing_right) e,
                distribute_mck_stat (op, nothing_left, nothing_right) st1,
                distribute_mck_stat (op, nothing_left, rop) st2)),
         [i1 +> op +> lop; i2 +> op; i3 +> op;i4 +> op]
        
      | x -> raise Impossible
      )

  | Selection  (Switch (e, st)), [i1;i2;i3] -> 
      Selection 
        (Switch
           (distribute_mck_e (op, nothing_left, nothing_right) e,
            distribute_mck_stat (op, nothing_left, rop) st)),
      [i1 +> op +> lop; i2 +> op; i3 +> op]
  | Iteration  (While (e, st)), [i1;i2;i3] -> 
      Iteration 
        (While
           (distribute_mck_e (op, nothing_left, nothing_right) e,
            distribute_mck_stat (op, nothing_left, rop) st)),
      [i1 +> op +> lop; i2 +> op; i3 +> op]

  | Iteration  (DoWhile (st, e)), [i1;i2;i3;i4;i5] -> 
      Iteration
        (DoWhile
           (distribute_mck_stat (op, nothing_left, nothing_right) st,
            distribute_mck_e (op, nothing_left, nothing_right) e)),
      [i1 +> op +> lop; i2 +> op; i3 +> op; i4 +> op; i5 +> op +> rop]

  | Iteration  (For ((e1opt,il1), (e2opt,il2), (e3opt, il3), st)), [i1;i2;i3] ->
      assert (null il3);
      Iteration
        (For 
           ((map_option (distribute_mck_e (op, nothing_left, nothing_left)) 
               e1opt, il1 +> List.map op),
            (map_option (distribute_mck_e (op, nothing_left, nothing_left)) 
               e2opt, il2 +> List.map op),
            (map_option (distribute_mck_e (op, nothing_left, nothing_left)) 
               e3opt, il3 +> List.map op),
            distribute_mck_stat (op, nothing_left, rop) st)),
      [i1 +> op +> lop; i2 +> op; i3 +> op]
            
  | Jump (Goto s), [i1;i2;i3]               -> 
      Jump (Goto s),
      [i1 +> op +> lop; i2 +> op; i3 +> op +> rop]
  | Jump ((Continue|Break|Return) as x), [i1;i2] -> 
      Jump x,
      [i1 +> op +> lop; i2 +> op +> rop]
  | Jump (ReturnExpr e), [i1;i2] -> 
      Jump 
        (ReturnExpr (distribute_mck_e (op, nothing_left, nothing_right) e)),
      [i1 +> op +> lop; i2 +> op +> rop]
  | (Asm, []) -> failwith "Asm, what to do ? not enough info"
  | x -> raise Impossible




and (distribute_mck_type: Ast_c.fullType distributer) = fun (op, lop, rop) ->
 fun decl ->
  raise Todo
  

