(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)


(* Just to warn me when there is some news in the types in
 * ast_cocci.ml or even ast_c.ml, so that I can then adjust my code in
 * pattern.ml or transformation.ml.
 *
 * For the moment I do it only for myself (pad), that is I check only
 * for news in ast_cocci.ml, because I already know when I add stuff in
 * my code in ast_c.ml or control_flow_c.ml. *)

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(* dependencies_to_adjust: pattern.ml, transformaton.ml *)

let dumb_astcocci_rule_elem = function
 | A.MetaRuleElem _ -> ()
 | A.MetaStmt (ida,_,_,_,_) -> ()
 | A.MetaStmtList _ -> ()
 | A.Exp expr -> ()
 | A.TopExp expr -> ()
 | A.Ty ty -> ()
 | A.TopId id -> ()
 | A.TopInit init -> ()
 | A.FunHeader (bef,allminus, fninfo, ida, _, paramsa, _, _) -> ()
 | A.Decl decl -> ()
 | A.SeqStart _ -> ()
 | A.SeqEnd _ -> ()
 | A.ExprStatement (ea, _) -> ()
 | A.IfHeader (_,_, ea, _) -> ()
 | A.Else _ -> ()
 | A.WhileHeader (_, _, ea, _) -> ()
 | A.DoHeader _ -> ()
 | A.WhileTail (_,_,ea,_,_) -> ()
 | A.ForHeader (_, _, _, ea2opt, _, ea3opt, _) -> ()
 | A.IteratorHeader (ia1, ia2, ea, ia3) -> ()
 | A.SwitchHeader _ -> ()
 | A.Break _ -> ()
 | A.Continue _ -> ()
 | A.Label _ -> ()
 | A.Goto(_,_,_) -> ()
 | A.Return _ -> ()
 | A.ReturnExpr (_, ea, _) -> ()
 | A.Exec(_,_,_,_) -> ()
 | A.DefineHeader _ -> ()
 | A.Undef _ -> ()
 | A.Pragma _ -> ()
 | A.Include _ -> ()
 | A.MetaInclude _ -> ()
 | A.Default _ -> ()
 | A.Case _ -> ()
 | A.AsRe _ -> ()
 | A.DisjRuleElem _ -> failwith "not possible - compiled away in asttoctl"

let dumb_astcocci_decl = function
   A.UnInit (stg, typa, sa, attr, _)     -> ()
 | A.Init (stg, typa, sa, attr, _, expa, _) -> ()
 | A.FunProto _ -> ()
 | A.TyDecl (typa, _)     -> ()
 | A.MacroDecl(stg, fn, _, eas, _, _) -> ()
 | A.MacroDeclInit(stg, fn, _, eas, _, _, _, _) -> ()
 | A.MetaDecl _ -> ()
 | A.AsDecl _ -> ()
 | A.Typedef(d,ty1,ty2,pv) -> ()
 | A.DisjDecl xs -> ()
 | A.ConjDecl xs -> ()
 | A.OptDecl _ -> ()

let dumb_astcocci_initialiser = function (* seems same as the above *)
    A.Init(stg,ty,id,attr,eq,ini,sem) -> ()
  | A.UnInit(stg,ty,id,attr,sem) -> ()
  | A.FunProto _ -> ()
  | A.MacroDecl(_, fn, _, eas, _, _) -> ()
  | A.MacroDeclInit(_, fn, _, eas, _, _, _, _) -> ()
  | A.TyDecl(ty,sem) -> ()
  | A.Typedef(d,ty1,ty2,pv) -> ()
  | A.DisjDecl(decls) -> ()
  | A.ConjDecl(decls) -> ()
  | A.MetaDecl(name,_,_,_) -> ()
  | A.AsDecl(_,_) -> ()
  | A.OptDecl(decl) -> ()

let dumb_astcocci_field = function
   A.Field (typa, sa, _bf, _)     -> ()
 | A.MetaField _ -> ()
 | A.MetaFieldList _ -> ()
 | A.DisjField xs -> ()
 | A.ConjField xs -> ()
 | A.OptField _ -> ()

let dumb_astcocci_expr = function
 | A.MetaExpr (ida,_,_, opttypa, _, _, _bitfield) -> ()
 | A.AsExpr (_,_) -> ()
 | A.AsSExpr (_,_) -> ()
 | A.Edots (_,_) -> ()
 | A.MetaErr _ -> ()
 | A.Ident ida -> ()
 | A.Constant (A.String sa,_,_,_) -> ()
 | A.Constant (A.Char sa,_,_,_) -> ()
 | A.Constant (A.Int sa,_,_,_) -> ()
 | A.Constant (A.Float sa,_,_,_) -> ()
 | A.Constant (A.DecimalConst _,_,_,_) -> ()
 | A.StringConstant (lq,frags,rq) -> ()
 | A.FunCall (ea1, _, eas, _) -> ()
 | A.Assignment (ea1, opa, ea2, _) -> ()
 | A.Sequence (ea1, opa, ea2) -> ()
 | A.CondExpr (ea1,_,ea2opt,_,ea3) -> ()
 | A.Postfix (ea, opa) -> ()
 | A.Infix (ea, opa) -> ()
 | A.Unary (ea, opa) -> ()
 | A.Binary (ea1, opa, ea2) -> ()
 | A.Nested (ea1, opa, ea2) -> ()
 | A.ArrayAccess (ea1, _, ea2, _) -> ()
 | A.RecordAccess (ea, _, ida) -> ()
 | A.RecordPtAccess (ea, _, ida) -> ()
 | A.Cast (_, typa, _, ea) -> ()
 | A.SizeOfExpr (_, ea) -> ()
 | A.SizeOfType (_, _, typa, _) -> ()
 | A.TypeExp (typa) -> ()
 | A.Constructor (_, typa, _, ia) -> ()
 | A.Paren (_, ea, _) -> ()
 | A.NestExpr _ -> ()
 | A.MetaExprList _ -> ()
 | A.EComma _ -> ()
 | A.DisjExpr eas -> ()
 | A.ConjExpr eas -> ()
 | A.OptExp _ -> ()

let dumb_astcocci_fulltype = function
    A.Type(_,cv,ty) -> ()
  | A.AsType(_,_) -> ()
  | A.DisjType(types) -> ()
  | A.ConjType(types) -> ()
  | A.OptType(ty) -> ()

let dumb_astcocci_type = function
 | A.MetaType(ida,_,_,_) -> ()
 | A.BaseType (basea,strings) -> ()
 | A.SignedT (signa,tya) -> ()
 | A.Pointer (typa, _) -> ()
 | A.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) -> ()
 | A.Array (typa, _, eaopt, _) -> ()
 | A.Decimal(_, _, _, _, _, _) -> ()
 | A.EnumName(en, ena) -> ()
 | A.EnumDef(ty, lb, ids, rb) -> ()
 | A.StructUnionName(sa, sua) -> ()
 | A.StructUnionDef(ty, lb, decls, rb) -> ()
 | A.TypeOfExpr(tf, lp, exp, rp) -> ()
 | A.TypeOfType(tf, lp, ty, rp) -> ()
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
