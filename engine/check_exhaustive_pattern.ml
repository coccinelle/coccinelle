
(* 
 * Just to warn me when there is some news in the types in ast_cocci.ml or 
 * even ast_c.ml, so that I can then adjust my code in pattern.ml or
 * transformation.ml.
 *
 * For the moment I do it only for myself (pad), that is I check only for news
 * in ast_cocci.ml, because I already know when I add stuff in my code
 * in ast_c.ml or control_flow_c.ml.
 *)

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(* dependencies_to_adjust: pattern.ml, transformaton.ml *)

let dumb_astcocci_rule_elem = function
 | A.MetaRuleElem _ -> ()
 | A.MetaStmt (ida,_,_) -> ()
 | A.MetaStmtList _ -> ()
 | A.Exp expr -> ()
 | A.FunHeader (allminus, stoa, tya, ida, _, paramsa, _) -> ()
 | A.Decl decla -> ()
 | A.SeqStart _ -> ()
 | A.SeqEnd _ -> ()
 | A.ExprStatement (ea, _) -> ()
 | A.IfHeader (_,_, ea, _) -> ()
 | A.Else _ -> ()
 | A.WhileHeader (_, _, ea, _) -> ()
 | A.DoHeader _ -> ()
 | A.WhileTail (_,_,ea,_,_) -> ()
 | A.ForHeader (_, _, ea1opt, _, ea2opt, _, ea3opt, _) -> ()
 | A.Break _ -> ()
 | A.Continue _ -> ()
 | A.Return _ -> ()
 | A.ReturnExpr (_, ea, _) -> ()



let dumb_astcocci_decl = function
 | A.UnInit (stg, typa, sa, _)     -> ()
 | A.Init (stg, typa, sa, _, expa, _) -> ()
 | A.TyDecl (typa, _)     -> ()
 | A.MetaDecl _ -> ()
 | A.DisjDecl xs -> ()
 | A.OptDecl _ | A.UniqueDecl _ | A.MultiDecl _ -> ()

let dumb_astcocci_initialiser = function
    A.Init(stg,ty,id,eq,ini,sem) -> ()
  | A.UnInit(stg,ty,id,sem) -> ()
  | A.TyDecl(ty,sem) -> ()
  | A.DisjDecl(decls) -> ()
  | A.MetaDecl(name,_) -> ()
  | A.OptDecl(decl) -> ()
  | A.UniqueDecl(decl) -> ()
  | A.MultiDecl(decl) -> ()

let dumb_astcocci_expr = function
 | A.MetaExpr (ida, opttypa, _) -> ()
 | A.Edots (_,_) -> ()
 | A.MetaConst _ -> ()
 | A.MetaErr _ -> ()
 | A.Ident ida -> ()
 | A.Constant (A.String sa,_,_) -> ()
 | A.Constant (A.Char sa,_,_) -> ()
 | A.Constant (A.Int sa,_,_) -> ()
 | A.Constant (A.Float sa,_,_) -> ()
 | A.FunCall (ea1, _, eas, _) -> ()
 | A.Assignment (ea1, opa, ea2) -> ()
 | A.CondExpr (ea1,_,ea2opt,_,ea3) -> ()
 | A.Postfix (ea, opa) -> ()
 | A.Infix (ea, opa) -> ()
 | A.Unary (ea, opa) -> ()
 | A.Binary (ea1, opa, ea2) -> ()
 | A.ArrayAccess (ea1, _, ea2, _) -> ()
 | A.RecordAccess (ea, _, ida) -> ()
 | A.RecordPtAccess (ea, _, ida) -> ()
 | A.Cast (_, typa, _, ea) -> ()
 | A.SizeOfExpr (_, ea) -> ()
 | A.SizeOfType (_, _, typa, _) -> ()
 | A.TypeExp (typa) -> ()
 | A.Paren (_, ea, _) -> ()
 | A.NestExpr _ -> ()
 | A.MetaExprList _ -> ()
 | A.EComma _ -> ()
 | A.Ecircles _ -> ()
 | A.Estars _ -> ()
 | A.DisjExpr eas -> ()
 | A.MultiExp _ -> ()
 | A.UniqueExp _ -> ()
 | A.OptExp _ -> ()




let dumb_astcocci_type = function
 | A.MetaType(ida,_) -> ()
 | A.BaseType (basea, signaopt) -> ()
 | A.Pointer (typa, _) -> ()
 | A.Array (typa, _, eaopt, _) -> ()
 | A.StructUnionName(sa, sua) -> ()
 | A.StructUnionDef(sa, sua, lb, decls, rb) -> ()
 | A.TypeName sa -> ()


(* ------------------------------------------------------------------------- *)
(* for C *)
(*
  | (Ident (_) | Constant _ | FunCall (_,_) | CondExpr (_,_,_) 
    | Sequence (_,_)
    | Assignment (_,_,_) 
    | Postfix (_,_) | Infix (_,_) | Unary (_,_) | Binary (_,_,_)
    | ArrayAccess (_,_) | RecordAccess (_,_) | RecordPtAccess (_,_)
    | SizeOfExpr (_) | SizeOfType (_) | Cast (_,_) 
    | StatementExpr (_) | Constructor 
    | ParenExpr (_) | MacroCall (_) | MacroCall2 (_)
    ),_ -> 

  | ( Labeled (Label (_,_)) | Labeled (Case  (_,_)) 
    | Labeled (CaseRange  (_,_,_)) | Labeled (Default _)
    | Compound _ | ExprStatement _ 
    | Selection  (If (_, _, _)) | Selection  (Switch (_, _))
    | Iteration  (While (_, _)) | Iteration  (DoWhile (_, _)) 
    | Iteration  (For ((_,_), (_,_), (_, _), _))
    | Jump (Goto _) | Jump ((Continue|Break|Return)) | Jump (ReturnExpr _)
    | Decl _ | Asm | Selection (IfCpp (_,_))
    ), _ -> 
*)

(* for control flow nodes 

  | ( F.ExprStatement (_, _) 
    | F.IfHeader  (_, _) | F.SwitchHeader (_, _)
    | F.WhileHeader (_, _) | (* F.DoHeader (_, _) | *) F.DoWhileTail (_, _) 
    | F.ForHeader (_, _)
    | F.Return     (_, _)  | F.ReturnExpr (_, _)
        (* no counter part in cocci *)
    | F.Label (_, _) 
    | F.Case  (_,_) | (* F.CaseRange (_, _) | *) F.Default   (_, _)
    | F.Goto (_, _) | F.Continue (_, _) | F.Break    (_, _)
    ) -> raise Impossible

*)
