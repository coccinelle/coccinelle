module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module Snap = Snapshot

(* ------------------------------------------------------------------------- *)

(* Given an Ast0 component, returns the same component with a generated
 * metaposition added.
 *)

(* ------------------------------------------------------------------------- *)
(* POSITION HELPERS *)

(* always make a new pos even if the mcode already has an associated position,
 * an existing pos might have undesirable constraints or inheritance.
 *)
let make_pos (_, arity, info, mcodekind, _, adj) snp =
  let (name, snp) = Snap.add_position snp in
  let meta_mcode = (("",name), arity, info, Ast0.PLUS Ast.ONE, ref [], adj) in
  let list_constraints = [] in
  let meta_collect = Ast.PER in
  let new_pos = Ast0.MetaPos(meta_mcode, list_constraints, meta_collect) in
  (Ast0.MetaPosTag(new_pos), snp)


(* ------------------------------------------------------------------------- *)
(* POSITION GENERATORS *)

let wrap a snp = Some (Ast0.wrap a, snp)
let wrap0 a snp = (Ast0.wrap a, snp)
let all_same = function [] -> true | x :: xs -> List.for_all (( = ) x) xs

(* adds generated metaposition to mcode unless it is optional *)
let mcode_pos ((x, a, info, mc, pos, q) as mco) snp =
  if a = Ast0.OPT then (mco, snp) else
  let (newpos, snp) = make_pos mco snp in
  ((x, a, info, mc, ref (newpos :: !pos), q), snp)

(* Auxiliary helpers for structures that largely follow the same format *)
let rec exp_one exp fn snp =
  match expression_pos exp snp with
  | Some (a, snp) -> wrap (fn a) snp
  | None -> None

and exp_two exp1 exp2 fn snp =
  match expression_pos exp1 snp with
  | Some (a, snp) -> wrap (fn a exp2) snp
  | None ->
      (match expression_pos exp2 snp with
       | None -> None
       | Some (a, snp) -> wrap (fn exp1 a) snp
      )

(* generate a position for an identifier.
 * Always possible! but not done in optional expressions *)
and ident_pos i snp =
  match Ast0.unwrap i with
  | Ast0.Id mc ->
      let (mc, snp) = mcode_pos mc snp in
      wrap0 (Ast0.Id(mc)) snp
  | Ast0.MetaId(metamc, i, s, p) ->
      let (metamc, snp) = mcode_pos metamc snp in
      wrap0 (Ast0.MetaId(metamc, i, s, p)) snp
  | Ast0.MetaFunc(metamc, i, p) ->
      let (metamc, snp) = mcode_pos metamc snp in
      wrap0 (Ast0.MetaFunc(metamc, i, p)) snp
  | Ast0.MetaLocalFunc(metamc, i, p) ->
      let (metamc, snp) = mcode_pos metamc snp in
      wrap0 (Ast0.MetaLocalFunc(metamc, i, p)) snp
  | Ast0.DisjId _ -> (i, snp)
  | Ast0.OptIdent (id) ->
      let (id, snp) = ident_pos id snp in
      wrap0 (Ast0.OptIdent (id)) snp
  | Ast0.UniqueIdent (id) ->
      let (id, snp) = ident_pos id snp in
      wrap0 (Ast0.UniqueIdent (id)) snp
  | Ast0.AsIdent(id1, id2) -> failwith "Should only be in metavars"

(* TODO: fix the disjunction thing. Usually we don't want to put positions
 * at the types, but we can be forced to if there are disjunctions with
 * types that use SmPL pattern-matching.
 *)
and type_pos t snp =
  match Ast0.unwrap t with
  | Ast0.DisjType(lp,tlist,pipelist,rp) ->
      let boollist = Snap.get_disj (Ast0.get_mcode_line lp) snp in
      if all_same boollist
      then None
      else failwith ("Mixed match/patch type disjunctions not supported " ^
                     "in position generator.")
  | _ -> None

(* TODO: fix the disjunction thing. Usually we don't want to put positions
 * at the cases, but we can be forced to if there are disjunctions with
 * cases that use SmPL pattern-matching.
 * NB: make sure that the statement dots in the case_line cases are generated
 * in no_gen mode...
 *)
and case_line_pos c snp =
  match Ast0.unwrap c with
  | Ast0.DisjCase(lp, clist, pipelist, rp) ->
      let boollist = Snap.get_disj (Ast0.get_mcode_line lp) snp in
      if all_same boollist
      then None
      else failwith ("Mixed match/patch case disjunctions in switch cases " ^
                     "not supported in position generator.")
  | _ -> None

and case_line_dots_pos c snp =
  List.map (fun x -> case_line_pos x snp) (Ast0.undots c)

(* Returns Some Ast0.declaration with inserted pos if it was possible to insert
 * a pos or None if it was not possible. *)
and declaration_pos d snp =
  match Ast0.unwrap d with
  | Ast0.DisjDecl _ | Ast0.Ddots _ | Ast0.MetaDecl _ | Ast0.MetaField _
  | Ast0.MetaFieldList _ | Ast0.AsDecl _ -> None
  | Ast0.Init(st, ty, id, eq, ini, sem) ->
      let _ = type_pos ty snp in (* sanity check *)
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Init(st, ty, id, eq, ini, sem)) snp
  | Ast0.UnInit(st, ty, id, sem) ->
      let _ = type_pos ty snp in (* sanity check *)
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.UnInit(st, ty, id, sem)) snp
  | Ast0.TyDecl _ -> failwith "tydecl"
  | Ast0.Typedef (tm, tc, tc2, sem) ->
      let (mc, snp) = mcode_pos tm snp in
      wrap (Ast0.Typedef (mc,tc,tc2,sem)) snp
  | Ast0.MacroDecl (id,lp,ed,rp,sem) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.MacroDecl (id,lp,ed,rp,sem)) snp
  | Ast0.MacroDeclInit (id,lp,ed,rp,eq,init,sem) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.MacroDeclInit (id,lp,ed,rp,eq,init,sem)) snp
  | Ast0.OptDecl(dec) ->
      (match declaration_pos dec snp with
       | Some (d, snp) -> wrap (Ast0.OptDecl d) snp
       | None -> None
      )
  | Ast0.UniqueDecl(dec) ->
      (match declaration_pos dec snp with
       | Some (d, snp) -> wrap (Ast0.UniqueDecl d) snp
       | None -> None
      )
  | Ast0.FunProto(fninfo,name,lp1,params,va,rp1,sem) ->
      let (name, snp) = ident_pos name snp in
      wrap (Ast0.FunProto(fninfo,name,lp1,params,va,rp1,sem)) snp

(* Returns Some Ast0.forinfo with inserted pos if it was possible to insert
 * a pos or None if it was not possible.
 *)
and forinfo_pos f snp =
  match Ast0.unwrap f with
  | Ast0.ForExp (Some exp, sem) ->
      (match expression_pos exp snp with
       | Some (a, snp) -> wrap (Ast0.ForExp(Some a, sem)) snp
       | None ->
           let (m, snp) = mcode_pos sem snp in
           wrap (Ast0.ForExp(Some exp, m)) snp
      )
  | Ast0.ForExp (None, sem) ->
      let (m,snp) = mcode_pos sem snp in
      wrap (Ast0.ForExp (None, m)) snp
  | Ast0.ForDecl (bef, decl) ->
      (match declaration_pos decl snp with
       | Some (d, snp) -> wrap (Ast0.ForDecl(bef, d)) snp
       | None -> None
      )

(* Returns Some Ast0.expression with inserted pos if it was possible to insert
 * a pos or None if it was not possible.
 *)
and expression_pos e snp =
  match Ast0.unwrap e with
  | Ast0.Ident(id) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Ident(id)) snp
  | Ast0.Constant(cmc) ->
      let (cmc, snp) = mcode_pos cmc snp in
      wrap (Ast0.Constant(cmc)) snp
  | Ast0.StringConstant(q1, sd, q2) ->
      let (q2, snp) = mcode_pos q2 snp in
      wrap (Ast0.StringConstant(q1, sd, q2)) snp
  | Ast0.FunCall(exp, lp, expdots, rp) ->
      let fn x = Ast0.FunCall(x, lp, expdots, rp) in
      (match exp_one exp fn snp with
       | None ->
           let (m,snp) = mcode_pos lp snp in
           wrap (Ast0.FunCall(exp, m, expdots, rp)) snp
       | a -> a
      )
  | Ast0.Assignment(exp1, amc, exp2, st) ->
      let fn x y =  Ast0.Assignment(x, amc, y, st) in
      (match exp_two exp1 exp2 fn snp with
       | None ->
           let (m,snp) = assignOp_pos amc snp in
           wrap (Ast0.Assignment(exp1, m, exp2, st)) snp
       | a -> a
      )
  | Ast0.Sequence(exp1, com, exp2) ->
      let fn x y = Ast0.Sequence(x, com, y) in
      (match exp_two exp1 exp2 fn snp with
       | None ->
           let (m,snp) = mcode_pos com snp in
           wrap (Ast0.Sequence(exp1,m,exp2)) snp
       | a -> a
      )
  | Ast0.CondExpr(exp1, why, expopt, colon, exp2) ->
      let fn x y = Ast0.CondExpr(x, why, expopt, colon, y) in
      (match exp_two exp1 exp2 fn snp with
       | None ->
           let (m,snp) = mcode_pos why snp in
           wrap (Ast0.CondExpr(exp1, m, expopt, colon, exp2)) snp
       | a -> a
      )
  | Ast0.Postfix(exp, fixmc) ->
      let fn x = Ast0.Postfix(x, fixmc) in
      (match exp_one exp fn snp with
       | None ->
           let (m,snp) = mcode_pos fixmc snp in
           wrap (Ast0.Postfix(exp,m)) snp
       | a -> a
      )
  | Ast0.Infix(exp, fixmc) ->
      let fn x = Ast0.Infix(x, fixmc) in
      (match exp_one exp fn snp with
       | None ->
           let (m,snp) = mcode_pos fixmc snp in
           wrap (Ast0.Infix(exp,m)) snp
       | a -> a
      )
  | Ast0.Unary(exp, unmc) ->
      let fn x = Ast0.Unary(x, unmc) in
      (match exp_one exp fn snp with
       | None ->
           let (m,snp) = mcode_pos unmc snp in
           wrap (Ast0.Unary(exp,m)) snp
       | a -> a
      )
  | Ast0.Binary(exp1, bin, exp2) ->
      let fn x y = Ast0.Binary(x, bin, y) in
      (match exp_two exp1 exp2 fn snp with
       | None ->
           let (m,snp) = binaryOp_pos bin snp in
           wrap (Ast0.Binary(exp1, m, exp2)) snp
       | a -> a
      )
  | Ast0.Nested(exp1, bin, exp2) ->
      let fn x y = Ast0.Nested(x, bin, y) in
      (match exp_two exp1 exp2 fn snp with
       | None ->
           let (m,snp) = binaryOp_pos bin snp in
           wrap (Ast0.Nested(exp1, m, exp2)) snp
       | a -> a
      )
  | Ast0.Paren(lp, exp, rp) ->
      let fn x = Ast0.Paren(lp, x, rp) in
      (match exp_one exp fn snp with
       | None ->
           let (m,snp) = mcode_pos lp snp in
           wrap (Ast0.Paren(m,exp,rp)) snp
       | a -> a
      )
  | Ast0.ArrayAccess(arrexp, lb, exp, rb) ->
      let fn x y = Ast0.ArrayAccess(x, lb, y, rb) in
      (match exp_two arrexp exp fn snp with
       | None ->
           let (m,snp) = mcode_pos lb snp in
           wrap (Ast0.ArrayAccess(arrexp, m, exp,rb)) snp
       | a -> a
      )
  | Ast0.RecordAccess(exp, stop, id) ->
      Some (
        match expression_pos exp snp with
        | Some (a, snp) -> (Ast0.wrap (Ast0.RecordAccess(a, stop, id)), snp)
        | None ->
            let (id, snp) = ident_pos id snp in
            (Ast0.wrap (Ast0.RecordAccess(exp, stop, id)), snp)
      )
  | Ast0.RecordPtAccess(exp, arrow, id) ->
      Some (
        match expression_pos exp snp with
        | Some (a,snp) -> (Ast0.wrap (Ast0.RecordPtAccess(a, arrow, id)), snp)
        | None ->
            let (id, snp) = ident_pos id snp in
            (Ast0.wrap(Ast0.RecordPtAccess(exp, arrow, id)), snp)
      )
  | Ast0.Cast(lp, typec, rp, exp) ->
      let _ = type_pos typec snp in (*sanity check for disj*)
      let fn x = Ast0.Cast(lp, typec, rp, exp) in
      (match exp_one exp fn snp with
       | None ->
           let (m,snp) = mcode_pos rp snp in
           wrap (Ast0.Cast(lp, typec, m,exp)) snp
       | a -> a
      )
  | Ast0.SizeOfExpr(sizeofmc, exp) ->
      let (sizeofmc, snp) = mcode_pos sizeofmc snp in
      wrap (Ast0.SizeOfExpr (sizeofmc, exp)) snp
  | Ast0.SizeOfType(sizeofmc, lp, typec, rp) ->
      let _ = type_pos typec snp in (* sanity check for disj *)
      let (sizeofmc, snp) = mcode_pos sizeofmc snp in
      wrap (Ast0.SizeOfType (sizeofmc, lp, typec, rp)) snp
  | Ast0.TypeExp(typec) -> type_pos typec snp (* sanity check, always None *)
  | Ast0.Constructor(lp, typec, rp, init) ->
      let _ = type_pos typec snp in (* sanity check for disj *)
      let (lp, snp) = mcode_pos lp snp in
      wrap (Ast0.Constructor (lp, typec, rp, init)) snp
  | Ast0.MetaErr (mc, co, pu) -> (* is this ever within the rule body? *)
      let (mc, snp) = mcode_pos mc snp in
      wrap (Ast0.MetaErr (mc, co, pu)) snp
  | Ast0.MetaExpr(mc, co, ty, fo, pu) ->
      let (mc, snp) = mcode_pos mc snp in
      wrap (Ast0.MetaExpr(mc, co, ty, fo, pu)) snp
  | Ast0.UniqueExp e ->
      exp_one e (fun x -> Ast0.UniqueExp x) snp
  | Ast0.OptExp e ->
      exp_one e (fun x -> Ast0.OptExp x) snp
  | Ast0.NestExpr _ | Ast0.Edots _ | Ast0.Ecircles _ | Ast0.Estars _
  | Ast0.AsExpr _ | Ast0.EComma _ | Ast0.MetaExprList _ -> None
  | Ast0.DisjExpr _ -> None

and assignOp_pos op snp =
  match Ast0.unwrap op with
    Ast0.SimpleAssign(op) ->
      let (op, snp) = mcode_pos op snp in
      wrap0 (Ast0.SimpleAssign(op)) snp
  | Ast0.OpAssign(aop) ->
      let (aop, snp) = mcode_pos aop snp in
      wrap0 (Ast0.OpAssign(aop)) snp
  | Ast0.MetaAssign(mc,c,pure) ->
      let (mc, snp) = mcode_pos mc snp in
      wrap0 (Ast0.MetaAssign(mc,c,pure)) snp

and binaryOp_pos op snp =
  match Ast0.unwrap op with
    Ast0.Arith(aop) ->
      let (aop, snp) = mcode_pos aop snp in
      wrap0 (Ast0.Arith(aop)) snp
  | Ast0.Logical(lop) ->
      let (lop, snp) = mcode_pos lop snp in
      wrap0 (Ast0.Logical(lop)) snp
  | Ast0.MetaBinary(mc,c,pure) ->
      let (mc, snp) = mcode_pos mc snp in
      wrap0 (Ast0.MetaBinary(mc,c,pure)) snp

(* returns Some statement with inserted position if it was possible to insert
 * a position or None if it was not possible. *)
and statement_pos s snp =
  match Ast0.unwrap s with
  | Ast0.Nest _ | Ast0.Dots _ | Ast0.Circles _ | Ast0.Stars _ | Ast0.Disj _
  | Ast0.MetaStmt _ | Ast0.Seq _ -> None

  (* uncertainty of whether these should be handled! *)
  | Ast0.Exec _ -> None
  | Ast0.MetaStmtList _ -> None
  | Ast0.AsStmt _ -> None
  | Ast0.TopExp _ -> None
  | Ast0.Ty _ -> None
  | Ast0.TopInit _ -> None

  | Ast0.Include (incmc,filemc) ->
      let (filemc, snp) = mcode_pos filemc snp in
      wrap (Ast0.Include(incmc, filemc)) snp
  | Ast0.Undef (defmc, id) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Undef(defmc, id)) snp
  | Ast0.Define (defmc, id, defparam, stmtdots) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Define(defmc, id, defparam, stmtdots)) snp
  | Ast0.Pragma (pragmc, id, praginfo) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Pragma(pragmc, id, praginfo)) snp
  | Ast0.OptStm stm ->
      (match statement_pos stm snp with
       | Some (v, sn) -> wrap (Ast0.OptStm v) sn
       | None -> None
      )
  | Ast0.UniqueStm stm ->
      (match statement_pos stm snp with
       | Some (v, sn) -> wrap (Ast0.UniqueStm v) sn
       | None -> None
      )
  | Ast0.ExprStatement(None, sem) -> None
  | Ast0.ExprStatement(Some e, sem) ->
      (match expression_pos e snp with
       | Some (v, sn) -> wrap (Ast0.ExprStatement(Some v, sem)) sn
       | None ->
           let (m,snp) = mcode_pos sem snp in
           wrap (Ast0.ExprStatement(Some e, m)) snp
      )
  | Ast0.Exp e ->
      (match expression_pos e snp with
       | Some (v, sn) -> wrap (Ast0.Exp v) sn
       | None -> None 
      )
  | Ast0.Decl (bef, decl) ->
      (match declaration_pos decl snp with
       | Some (d, sn) -> wrap (Ast0.Decl(bef, d)) sn
       | None -> None
      )
  | Ast0.IfThen (ifm, l, exp, r, st, a) ->
      (match expression_pos exp snp with
       | Some (v, sn) -> wrap (Ast0.IfThen(ifm, l, v, r, st, a)) sn
       | None ->
           let (ifm, snp) = mcode_pos ifm snp in
          wrap (Ast0.IfThen(ifm, l, exp, r, st, a)) snp
      )
  | Ast0.IfThenElse (ifm, l, exp, r, s1, e, s2, a) ->
      (match expression_pos exp snp with
       | Some (v, snp) ->
           wrap (Ast0.IfThenElse(ifm,l,v,r,s1,e,s2,a)) snp
       | None ->
           let (ifm, snp) = mcode_pos ifm snp in
           wrap (Ast0.IfThenElse(ifm,l,exp,r,s1,e,s2,a)) snp
      )
  | Ast0.While (whmc, l, exp, r, s, a) ->
      (match expression_pos exp snp with
       | Some (v, snp) -> wrap (Ast0.While(whmc, l, v, r, s, a)) snp
       | None ->
           let (whmc, snp) = mcode_pos whmc snp in
           wrap (Ast0.While(whmc, l, exp, r, s, a)) snp
      )
  | Ast0.Do (d, s, whmc, l, exp, r, sem) ->
      (match expression_pos exp snp with
       | Some (v,snp) -> wrap (Ast0.Do(d, s, whmc, l, v, r, sem)) snp
       | None ->
           let (d, snp) = mcode_pos d snp in
           wrap (Ast0.Do(d, s, whmc, l, exp, r, sem)) snp
      )
  | Ast0.For (fo,lp, fi,expo1,sem,expo2,rp,stmt,a) ->
      (match forinfo_pos fi snp with
       | Some (f,snp) ->
           wrap (Ast0.For (fo,lp, f, expo1, sem, expo2, rp, stmt, a)) snp
       | None ->
           let (fo, snp) = mcode_pos fo snp in
           wrap (Ast0.For(fo, lp,fi,expo1,sem,expo2,rp,stmt,a)) snp
      )
  | Ast0.Iterator (id,lp,expdots,rp,stmt,a) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Iterator(id, lp, expdots, rp, stmt, a)) snp
  | Ast0.Switch (sw, lp, exp, rp, lb, sd, cd, rb) ->
      let _ = case_line_dots_pos cd snp in (* sanity check for disj *)
      (match expression_pos exp snp with
       | Some (v, snp) ->
           wrap (Ast0.Switch(sw, lp, v, rp, lb, sd, cd, rb)) snp
       | None ->
           let (sw, snp) = mcode_pos sw snp in
           wrap (Ast0.Switch(sw, lp, exp, rp, lb, sd, cd,rb)) snp
      )
  | Ast0.Label (id,col) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Label (id, col)) snp
  | Ast0.Goto (goto,id,sem) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.Goto (goto, id, sem)) snp
  | Ast0.Break (bmc,sem) ->
      let (bmc, snp) = mcode_pos bmc snp in
      wrap (Ast0.Break(bmc,sem)) snp
  | Ast0.Continue (cmc,sem) ->
      let (cmc, snp) = mcode_pos cmc snp in
      wrap (Ast0.Continue(cmc,sem)) snp
  | Ast0.ReturnExpr (retmc, exp, sem) ->
      (match expression_pos exp snp with
       | Some (v,snp) -> wrap (Ast0.ReturnExpr(retmc, v, sem)) snp
       | None ->
           let (retmc, snp) = mcode_pos retmc snp in
           wrap (Ast0.ReturnExpr(retmc, exp, sem)) snp
      )
  | Ast0.Return (retmc,sem) ->
      let (retmc, snp) = mcode_pos retmc snp in
      wrap (Ast0.Return(retmc,sem)) snp
  | Ast0.FunDecl (b, f, id, lp, ps, op, rp, lb, sd, rb, a) ->
      let (id, snp) = ident_pos id snp in
      wrap (Ast0.FunDecl(b,f,id, lp, ps, op, rp, lb, sd, rb, a)) snp
