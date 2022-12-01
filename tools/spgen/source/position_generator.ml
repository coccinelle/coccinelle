(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module Snap = Snapshot

(* ------------------------------------------------------------------------- *)

(* Given an Ast0 component, returns the same component with a generated
 * metaposition added.
 *
 * This is all pretty messy and requires, for each component, individual
 * assessment of where to put the position.
 * The general heuristic is to add it either:
 *  - recursively to a smaller subcomponent (e.g. first expression in e1 + e2)
 *  - at an id (e.g. a function name) or
 *  - at an mcode (e.g. an operator).
 *)

(* ------------------------------------------------------------------------- *)
(* POSITION HELPERS *)

(* always make a new pos even if the mcode already has an associated position,
 * an existing pos might have undesirable constraints or inheritance.
 *)
let make_pos (_, arity, info, mcodekind, _, adj) snp
: Ast0.anything * Snap.t =
  let (name, snp) = Snap.add_position snp in
  let meta_mcode = (("",name),arity,info,Ast0.PLUS Ast.ONE,ref [],adj) in
  let list_constraints = Ast.CstrTrue in
  let meta_collect = Ast.PER in
  let new_pos = Ast0.MetaPos(meta_mcode, list_constraints, meta_collect) in
  (Ast0.MetaPosTag(new_pos), snp)


(* ------------------------------------------------------------------------- *)
(* ALWAYS POSITION GENERATORS: always possible to generate pos *)

(* adds generated metaposition to mcode unless it is optional.
 * yes, this contradicts the above statement. But it will never result in an
 * error because it is not possible to write an SmPL script with only optional
 * components.
 *)
let mcode_pos ((x, a, info, mc, pos, q) as mco) snp
: 'a Ast0.mcode * Snap.t =
  if a = Ast0.OPT then
    (mco, snp)
  else
    let (newpos, snp) = make_pos mco snp in
    ((x, a, info, mc, ref (newpos :: !pos), q), snp)

(* helper for adding mcode position, reconstructs component *)
let mcode
  ~mc           (* 'a Ast0.mcode *)
  ~constructor  (* mc:'a Ast0.mcode -> 'b *)
  snp           (* Snap.t *)
: 'b Ast0.wrap * Snap.t =
    let (mc, snp) = mcode_pos mc snp in
    (Ast0.wrap (constructor ~mc), snp)

let assignOp_pos a snp
: Ast0.base_assignOp Ast0.wrap * Snap.t =
  match Ast0.unwrap a with
  | Ast0.SimpleAssign mc ->
      let constructor ~mc = Ast0.SimpleAssign mc in
      mcode ~mc ~constructor snp
  | Ast0.OpAssign mc ->
      let constructor ~mc = Ast0.OpAssign mc in
      mcode ~mc ~constructor snp
  | Ast0.MetaAssign (mc, v, w) ->
      let constructor ~mc = Ast0.MetaAssign(mc,v,w) in
      mcode ~mc ~constructor snp

let binaryOp_pos a snp
: Ast0.base_binaryOp Ast0.wrap * Snap.t =
  match Ast0.unwrap a with
  | Ast0.Arith mc ->
      let constructor ~mc = Ast0.Arith mc in
      mcode ~mc ~constructor snp
  | Ast0.Logical mc ->
      let constructor ~mc = Ast0.Logical mc in
      mcode ~mc ~constructor snp
  | Ast0.MetaBinary (mc,v,w) ->
      let constructor ~mc = Ast0.MetaBinary(mc,v,w) in
      mcode ~mc ~constructor snp

let rec ident_pos i snp
: Ast0.base_ident Ast0.wrap * Snap.t =
  match Ast0.unwrap i with
  | Ast0.Id mc ->
      let constructor ~mc = Ast0.Id mc in
      mcode ~mc ~constructor snp
  | Ast0.MetaId(mc, i, s, p) ->
      let constructor ~mc = Ast0.MetaId(mc,i,s,p) in
      mcode ~mc ~constructor snp
  | Ast0.MetaFunc(mc, i, p) ->
      let constructor ~mc = Ast0.MetaFunc(mc,i,p) in
      mcode ~mc ~constructor snp
  | Ast0.MetaLocalFunc(mc, i, p) ->
      let constructor ~mc = Ast0.MetaLocalFunc(mc,i,p) in
      mcode ~mc ~constructor snp
  | Ast0.DisjId _ | Ast0.ConjId _ ->
      (i, snp)
  | Ast0.OptIdent (id) ->
      let (id, snp) = ident_pos id snp in
      (Ast0.wrap (Ast0.OptIdent (id)), snp)
  | Ast0.AsIdent(id1, id2) ->
      failwith "pos_gen: <id1 as id2> should only be in metavars"


(* ------------------------------------------------------------------------- *)
(* HELPERS FOR SOMETIMES POSITION GENERATORS *)

let all_same = function [] -> true | x :: xs -> List.for_all (( = ) x) xs

(* wraps Ast0 component and wraps it in Some *)
let wrap (a : 'a) (snp : Snap.t)
: ('a Ast0.wrap * Snap.t) option =
  Some (Ast0.wrap a, snp)

(* adds position to the mcode, reconstructs component, and wraps it in Some *)
let mcode_wrap
  ~mc           (* 'a Ast0.mcode *)
  ~constructor  (* mc:'a Ast0.mcode -> b *)
  snp           (* Snap.t *)
: ('b Ast0.wrap * Snap.t) option =
  Some (mcode ~mc ~constructor snp)

(* adds position to the id, reconstructs component, and wraps it in Some *)
let id_wrap
  ~id           (* Ast0.base_ident Ast0.wrap *)
  ~constructor  (* id:Ast0.base_ident Ast0.wrap -> 'a *)
  snp           (* Snap.t *)
: ('a Ast0.wrap * Snap.t) option =
  let (id, snp) = ident_pos id snp in
  wrap (constructor ~id) snp

(* generic helper function.
 * Arguments:
 *  - item, an Ast0 component:           'a
 *  - item_posfn, position generator:    'a -> Snap.t -> ('b * Snap.t) option
 *  - constructor:                       item:'b -> 'c
 *  - alt, alt function:                 unit -> ('c Ast0.wrap * Snap.t) option
 *  - snp, snapshot:                     Snap.t
 *
 * Tries to generate position with item_posfn and reconstruct outer structure.
 * If no position could be generated, call alt() as a backup.
 *)
let item_wrap ~item ~item_posfn ~constructor ?(alt = fun _ -> None) snp
: ('c Ast0.wrap * Snap.t) option =
  match item_posfn item snp with
  | Some (item, snp) -> wrap (constructor ~item) snp
  | None -> alt()


(* ------------------------------------------------------------------------- *)
(* SOMETIMES POSITION GENERATORS - not always possible to generate pos *)

(* DISJUNCTION RELATED: type_pos and case_line_pos *)

(* These functions are deliberately left unfinished for now.
 * Implementing requires changes to disj_generator, but the cases are fairly
 * rare, so for now just throw an exception if encountered.
 * (to implement, disjunctions should return None here, and be added as special
 * cases in rule_body.ml and disj_generator.ml. See DisjExpr for example.)
 *)

let rec type_pos t snp
: (Ast0.base_typeC Ast0.wrap * Snap.t) option =
  match Ast0.unwrap t with
  | Ast0.AsType _
  | Ast0.BaseType _ ->
      None
  | Ast0.DisjType(lp,tlist,pipelist,rp) ->
      let boollist = Snap.get_disj (Ast0.get_mcode_line lp) snp in
      if all_same boollist then None
      else failwith (
        "pos_gen: Mixed match/patch type disjunctions not supported " ^
        "in position generator."
      )
  | Ast0.ConjType(lp,tlist,pipelist,rp) -> None (* not sure *)
  | Ast0.ConstVol(const,t) ->
      let constructor ~item = Ast0.ConstVol(const,item) in
      item_wrap ~item:t ~item_posfn:type_pos ~constructor snp
  | Ast0.Signed(sign,t) ->
      let constructor ~mc = Ast0.Signed(mc,t) in
      mcode_wrap ~mc:sign ~constructor snp
  | Ast0.Pointer(t,star) ->
      let constructor ~mc = Ast0.Pointer(t,mc) in
      mcode_wrap ~mc:star ~constructor snp
  | Ast0.ParenType(lp,t,rp) ->
      let constructor ~mc = Ast0.ParenType(lp,t,rp) in
      mcode_wrap ~mc:rp ~constructor snp
  | Ast0.FunctionType(t,lp,params,rp) ->
      let constructor ~mc = Ast0.FunctionType(t,lp,params,rp) in
      mcode_wrap ~mc:rp ~constructor snp
  | Ast0.Array(t,lb,expr,rb) ->
      let constructor ~mc = Ast0.Array(t,lb,expr,mc) in
      mcode_wrap ~mc:rb ~constructor snp
  | Ast0.Decimal(dec,lp,expr,comma,expr2,rp) ->
      let constructor ~mc = Ast0.Decimal(dec,lp,expr,comma,expr2,mc) in
      mcode_wrap ~mc:rp ~constructor snp
  | Ast0.EnumName(enum, key, Some nm) ->
      let constructor ~id = Ast0.EnumName(enum, key, Some id) in
      id_wrap ~id:nm ~constructor snp
  | Ast0.EnumName(enum, key, None) ->
      let constructor ~mc = Ast0.EnumName(mc, key, None) in
      mcode_wrap ~mc:enum ~constructor snp
  | Ast0.EnumDef(t,base,lcb,exprdots,rcb) ->
      let c ~item ~mc = Ast0.EnumDef(item,base,lcb,exprdots,mc) in
      let alt() = mcode_wrap ~mc:rcb ~constructor:(c ~item:t) snp in
      item_wrap ~item:t ~item_posfn:type_pos ~constructor:(c ~mc:rcb) ~alt snp
  | Ast0.StructUnionName(sumc,Some nm) ->
      let constructor ~id = Ast0.StructUnionName(sumc,Some id) in
      id_wrap ~id:nm ~constructor snp
  | Ast0.StructUnionName(sumc,None) ->
      let constructor ~mc = Ast0.StructUnionName(mc,None) in
      mcode_wrap ~mc:sumc ~constructor snp
  | Ast0.StructUnionDef(t,lcb,decldots,rcb) ->
      let c ~item ~mc = Ast0.StructUnionDef(item,lcb,decldots,mc) in
      let alt() = mcode_wrap ~mc:rcb ~constructor:(c ~item:t) snp in
      item_wrap ~item:t ~item_posfn:type_pos ~constructor:(c ~mc:rcb) ~alt snp
  | Ast0.TypeOfExpr(sizeofmc, lp, exp, rp) ->
      let constructor ~mc = Ast0.TypeOfExpr(mc, lp, exp, rp) in
      mcode_wrap ~mc:rp ~constructor snp
  | Ast0.TypeOfType(sizeofmc, lp, typec, rp) ->
      let _ = type_pos typec snp in (* sanity check for disj *)
      let constructor ~mc = Ast0.TypeOfType(mc, lp, typec, rp) in
      mcode_wrap ~mc:sizeofmc ~constructor snp
  | Ast0.TypeName(nm) ->
      let constructor ~mc = Ast0.TypeName(mc) in
      mcode_wrap ~mc:nm ~constructor snp
  | Ast0.AutoType(auto) ->
      let constructor ~mc = Ast0.AutoType(mc) in
      mcode_wrap ~mc:auto ~constructor snp
  | Ast0.MetaType(mnmc,cstr,pure) ->
      let constructor ~mc = Ast0.MetaType(mc,cstr,pure) in
      mcode_wrap ~mc:mnmc ~constructor snp
  | Ast0.OptType(t) ->
      let constructor ~item = Ast0.OptType(item) in
      item_wrap ~item:t ~item_posfn:type_pos ~constructor snp

(* NB: if implementing disj generation, make sure that the statement dots in
 * the clist are generated in no_gen mode...
 *)
let case_line_pos c snp
: (Ast0.base_case_line Ast0.wrap * Snap.t) option =
  match Ast0.unwrap c with
  | Ast0.DisjCase(lp, clist, pipelist, rp) ->
      let boollist = Snap.get_disj (Ast0.get_mcode_line lp) snp in
      if all_same boollist then None
      else failwith (
        "pos_gen: Mixed match/patch case disjunctions in switch cases " ^
        "not supported in position generator."
      )
  | _ -> None

let case_line_dots_pos c snp
: (Ast0.base_case_line Ast0.wrap * Snap.t) option list =
  List.map (fun x -> case_line_pos x snp) (Ast0.unwrap c)

let rec expression_pos exp snp
: (Ast0.base_expression Ast0.wrap * Snap.t) option =

  (* try adding a position to the internal expression. If that failed, try
   * the alt function (usually, we use mcodes or ids as fallbacks)
   *)
  let exp_wrap ~exp ~constructor ?(alt = fun _ -> None) snp =
    let constructor ~item = constructor ~exp:item in
    item_wrap ~item:exp ~item_posfn:expression_pos ~constructor ~alt snp in

  (* try adding a position to internal expressions, first try exp1, then exp2.
   * if both have failed then call alt function.
   *)
  let exp_wrap2 ~exp1 ~exp2 ~constructor ?(alt = fun _ -> None) snp =
    let c1 ~exp = constructor ~exp1:exp ~exp2 in
    let c2 ~exp = constructor ~exp1 ~exp2:exp in
    let try_exp2() = exp_wrap ~exp:exp2 ~constructor:c2 ~alt snp in
    exp_wrap ~exp:exp1 ~constructor:c1 ~alt:try_exp2 snp in

  match Ast0.unwrap exp with
  | Ast0.NestExpr _
  | Ast0.Edots _
  | Ast0.AsExpr _
  | Ast0.AsSExpr _
  | Ast0.EComma _
  | Ast0.MetaExprList _
  | Ast0.DisjExpr _
  | Ast0.ConjExpr _ ->
      None
  | Ast0.Ident(id) ->
      let constructor ~id = Ast0.Ident id in
      id_wrap ~id ~constructor snp
  | Ast0.Constant(mc) ->
      let constructor ~mc = Ast0.Constant mc in
      mcode_wrap ~mc ~constructor snp
  | Ast0.StringConstant(q1, sd, q2, sz) ->
      let constructor ~mc = Ast0.StringConstant (q1, sd, mc, sz) in
      mcode_wrap ~mc:q2 ~constructor snp
  | Ast0.FunCall(exp, lp, expdots, rp) ->
      let c ~exp ~mc = Ast0.FunCall(exp, mc, expdots, rp) in
      let alt() = mcode_wrap ~mc:lp ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:lp) ~alt snp
  | Ast0.Assignment(exp1, asop, exp2, st) ->
      let c ~exp1 ~exp2 ~mc = Ast0.Assignment(exp1, mc, exp2, st) in
      let alt() =
        let (mc, snp) = assignOp_pos asop snp in
        wrap (c ~exp1 ~exp2 ~mc) snp in
      exp_wrap2 ~exp1 ~exp2 ~constructor:(c ~mc:asop) ~alt snp
  | Ast0.Sequence(exp1, com, exp2) ->
      let c ~exp1 ~exp2 ~mc = Ast0.Sequence(exp1, mc, exp2) in
      let alt() = mcode_wrap ~mc:com ~constructor:(c ~exp1 ~exp2) snp in
      exp_wrap2 ~exp1 ~exp2 ~constructor:(c ~mc:com) ~alt snp
  | Ast0.CondExpr(exp1, why, expopt, colon, exp2) ->
      let c ~exp1 ~exp2 ~mc = Ast0.CondExpr(exp1, mc, expopt, colon, exp2) in
      let alt() = mcode_wrap ~mc:why ~constructor:(c ~exp1 ~exp2) snp in
      exp_wrap2 ~exp1 ~exp2 ~constructor:(c ~mc:why) ~alt snp
  | Ast0.Postfix(exp, fixmc) ->
      let c ~exp ~mc = Ast0.Postfix (exp, mc) in
      let alt() = mcode_wrap ~mc:fixmc ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:fixmc) ~alt snp
  | Ast0.Infix(exp, fixmc) ->
      let c ~exp ~mc = Ast0.Infix (exp, mc) in
      let alt() = mcode_wrap ~mc:fixmc ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:fixmc) ~alt snp
  | Ast0.Unary(exp, unmc) ->
      let c ~exp ~mc = Ast0.Unary (exp, mc) in
      let alt() = mcode_wrap ~mc:unmc ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:unmc) ~alt snp
  | Ast0.Binary(exp1, bin, exp2) ->
      let c ~exp1 ~exp2 ~mc = Ast0.Binary(exp1, mc, exp2) in
      let alt() =
        let (mc, snp) = binaryOp_pos bin snp in
        wrap (c ~exp1 ~exp2 ~mc) snp in
      exp_wrap2 ~exp1 ~exp2 ~constructor:(c ~mc:bin) ~alt snp
  | Ast0.Nested(exp1, bin, exp2) ->
      let c ~exp1 ~exp2 ~mc = Ast0.Nested(exp1, mc, exp2) in
      let alt() =
        let (mc, snp) = binaryOp_pos bin snp in
        wrap (c ~exp1 ~exp2 ~mc) snp in
      exp_wrap2 ~exp1 ~exp2 ~constructor:(c ~mc:bin) ~alt snp
  | Ast0.Paren(lp, exp, rp) ->
      let c ~exp ~mc = Ast0.Paren (mc, exp, rp) in
      let alt() = mcode_wrap ~mc:lp ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:lp) ~alt snp
  | Ast0.ArrayAccess(exp1, lb, exp2, rb) ->
      let c ~exp1 ~exp2 ~mc = Ast0.ArrayAccess(exp1, mc, exp2, rb) in
      let alt() = mcode_wrap ~mc:lb ~constructor:(c ~exp1 ~exp2) snp in
      exp_wrap2 ~exp1 ~exp2 ~constructor:(c ~mc:lb) ~alt snp
  | Ast0.RecordAccess(exp, stop, id) ->
      let c ~exp ~id = Ast0.RecordAccess(exp, stop, id) in
      let alt() = id_wrap ~id ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~id) ~alt snp
  | Ast0.RecordPtAccess(exp, arrow, id) ->
      let c ~exp ~id = Ast0.RecordPtAccess(exp, arrow, id) in
      let alt() = id_wrap ~id ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~id) ~alt snp
  | Ast0.Cast(lp, typec, attr, rp, exp) ->
      let _ = type_pos typec snp in (* sanity check for disj *)
      let c ~exp ~mc = Ast0.Cast(lp, typec, attr, mc, exp) in
      let alt() = mcode_wrap ~mc:rp ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:rp) ~alt snp
  | Ast0.SizeOfExpr(sizeofmc, exp) ->
      let constructor ~mc = Ast0.SizeOfExpr(mc, exp) in
      mcode_wrap ~mc:sizeofmc ~constructor snp
  | Ast0.SizeOfType(sizeofmc, lp, typec, rp) ->
      let _ = type_pos typec snp in (* sanity check for disj *)
      let constructor ~mc = Ast0.SizeOfType(mc, lp, typec, rp) in
      mcode_wrap ~mc:sizeofmc ~constructor snp
  | Ast0.New(nw,pp_opt,lp_opt,ty,rp_opt,args_opt) ->
      let constructor ~mc = Ast0.New(mc,pp_opt,lp_opt,ty,rp_opt,args_opt) in
      mcode_wrap ~mc:nw ~constructor snp
  | Ast0.Delete(deletemc, exp) ->
      let constructor ~mc = Ast0.Delete(mc, exp) in
      mcode_wrap ~mc:deletemc ~constructor snp
  | Ast0.DeleteArr(deletemc,lb,rb,exp) ->
      let constructor ~mc = Ast0.DeleteArr(mc,lb,rb,exp) in
      mcode_wrap ~mc:deletemc ~constructor snp
  | Ast0.TypeExp(typec) ->
      let constructor ~item = Ast0.TypeExp(item) in
      item_wrap ~item:typec ~item_posfn:type_pos ~constructor snp
  | Ast0.Constructor(lp, typec, rp, init) ->
      let _ = type_pos typec snp in (* sanity check for disj *)
      let constructor ~mc = Ast0.Constructor (mc, typec, rp, init) in
      mcode_wrap ~mc:lp ~constructor snp
  | Ast0.MetaErr (mc, co, pu) -> (* is this ever within the rule body? *)
      let constructor ~mc = Ast0.MetaErr (mc, co, pu) in
      mcode_wrap ~mc ~constructor snp
  | Ast0.MetaExpr(mc, co, ty, fo, pu, bitfield) ->
      let constructor ~mc = Ast0.MetaExpr (mc, co, ty, fo, pu, bitfield) in
      mcode_wrap ~mc ~constructor snp
  | Ast0.OptExp exp ->
      let constructor ~exp = Ast0.OptExp exp in
      exp_wrap ~exp ~constructor snp

(* redefine exp_wrap outside scope of expression_pos due to internal exp_wrap
 * being typed to only work for expression constructors and not 'a constructors
 *)
let exp_wrap ~exp ~constructor ?(alt = fun _ -> None) snp
: ('a Ast0.wrap * Snap.t) option =
  let constructor ~item = constructor ~exp:item in
  item_wrap ~item:exp ~item_posfn:expression_pos ~constructor ~alt snp

let rec declaration_pos decl snp
: (Ast0.base_declaration Ast0.wrap * Snap.t) option =
  match Ast0.unwrap decl with
    Ast0.DisjDecl _
  | Ast0.ConjDecl _
  | Ast0.MetaDecl _
  | Ast0.AsDecl _ ->
      None
  | Ast0.Init(st, ty, midattr, id, endattr, eq, ini, sem) ->
      let _ = type_pos ty snp in (* sanity check *)
      let constructor ~id = Ast0.Init(st, ty, midattr, id, endattr, eq, ini, sem) in
      id_wrap ~id ~constructor snp
  | Ast0.UnInit(st, ty, midattr, id, endattr, sem) ->
      let _ = type_pos ty snp in (* sanity check *)
      let constructor ~id = Ast0.UnInit(st, ty, midattr, id, endattr, sem) in
      id_wrap ~id ~constructor snp
  | Ast0.TyDecl (t, attr, sem) ->
      let c ~item ~mc = Ast0.TyDecl(item, attr, mc) in
      let alt() = mcode_wrap ~mc:sem ~constructor:(c ~item:t) snp in
      item_wrap ~item:t ~item_posfn:type_pos ~constructor:(c ~mc:sem) ~alt snp
  | Ast0.Typedef (tm, tc, tc2, sem) ->
      let constructor ~mc = Ast0.Typedef (mc, tc, tc2, sem) in
      mcode_wrap ~mc:tm ~constructor snp
  | Ast0.MacroDecl (st,id,lp,ed,rp,attr,sem) ->
      let constructor ~id = Ast0.MacroDecl (st, id, lp, ed, rp, attr, sem) in
      id_wrap ~id ~constructor snp
  | Ast0.MacroDeclInit (st,id,lp,ed,rp,eq,init,sem) ->
      let constructor ~id = Ast0.MacroDeclInit (st,id,lp,ed,rp,eq,init,sem) in
      id_wrap ~id ~constructor snp
  | Ast0.OptDecl(dec) ->
      let constructor ~item = Ast0.OptDecl item in
      item_wrap ~item:dec ~item_posfn:declaration_pos ~constructor snp
  | Ast0.FunProto(fninfo,attr,id,lp1,params,va,rp1,sem) ->
      let constructor ~id = Ast0.FunProto(fninfo,attr,id,lp1,params,va,rp1,sem) in
      id_wrap ~id ~constructor snp

let rec field_pos decl snp
: (Ast0.base_field Ast0.wrap * Snap.t) option =
  match Ast0.unwrap decl with
    Ast0.DisjField _
  | Ast0.ConjField _
  | Ast0.Fdots _
  | Ast0.MetaField _
  | Ast0.MetaFieldList _
  | Ast0.Field(_, None, _, _) -> None
  | Ast0.Field(ty, Some id, bf, sem) ->
      let _ = type_pos ty snp in (* sanity check *)
      let constructor ~id = Ast0.Field(ty, Some id, bf, sem) in
      id_wrap ~id ~constructor snp
  | Ast0.OptField(dec) ->
      let constructor ~item = Ast0.OptField item in
      item_wrap ~item:dec ~item_posfn:field_pos ~constructor snp

let forinfo_pos f snp
: (Ast0.base_forinfo Ast0.wrap * Snap.t) option =
  match Ast0.unwrap f with
  | Ast0.ForExp (Some exp,sem,expo1,sem,expo2) ->
      let c ~exp ~mc = Ast0.ForExp(Some exp, mc) in
      let alt() = mcode_wrap ~mc:sem ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:sem) ~alt snp
  | Ast0.ForExp (None,sem,expo1,sem,expo2) ->
      let constructor ~mc = Ast0.ForExp (None, mc) in
      mcode_wrap ~mc:sem ~constructor snp
  | Ast0.ForDecl (bef,decl,expo1,sem,expo2) ->
      let constructor ~item = Ast0.ForDecl(bef, item) in
      item_wrap ~item:decl ~item_posfn:declaration_pos ~constructor snp
  | Ast0.ForDecl (bef,decl,expo) ->
      let constructor ~item = Ast0.ForDecl(bef, item) in
      item_wrap ~item:decl ~item_posfn:declaration_pos ~constructor snp

let rec statement_pos s snp
: (Ast0.base_statement Ast0.wrap * Snap.t) option =
  match Ast0.unwrap s with

  (* these cannot have positions (disjunctions are handled separately) *)
  | Ast0.Nest _
  | Ast0.Dots _
  | Ast0.Disj _
  | Ast0.Conj _
  | Ast0.MetaStmtList _ -> None
  | Ast0.AsStmt _ -> None
  | Ast0.MetaStmt _ -> None

  (* uncertainty of whether these should be handled! *)
  | Ast0.Exec _ -> None
  | Ast0.TopExp _ -> None
  | Ast0.TopId _ -> None
  | Ast0.Ty _ -> None
  | Ast0.TopInit _ -> None

  | Ast0.Include (incmc,filemc) ->
      let constructor ~mc = Ast0.Include(incmc, mc) in
      mcode_wrap ~mc:filemc ~constructor snp
  | Ast0.MetaInclude (incmc,filemc) ->
      let constructor ~mc = Ast0.MetaInclude(mc, filemc) in
      mcode_wrap ~mc:incmc ~constructor snp
  | Ast0.Undef (defmc, id) ->
      let constructor ~id = Ast0.Undef(defmc, id) in
      id_wrap ~id ~constructor snp
  | Ast0.Define (defmc, id, defparam, stmtdots) ->
    (* if the #define directive has parameters, we cannot put the position
     * after the identifier as it will break up the token. Therefore we put
     * it after the parenthesis.
     *)
    let c ~id ~defparam = Ast0.Define(defmc, id, defparam, stmtdots) in
    (match Ast0.unwrap defparam with
     | Ast0.NoParams ->
       id_wrap ~id ~constructor:(c ~defparam) snp
     | Ast0.DParams(lp, defp, rp) ->
       let (newrp, snp) = mcode_pos rp snp in
       let defparam = Ast0.wrap (Ast0.DParams(lp, defp, newrp)) in
       wrap (c ~id ~defparam) snp
    )
  | Ast0.Pragma (pragmc, id, praginfo) ->
      let constructor ~id = Ast0.Pragma(pragmc, id, praginfo) in
      id_wrap ~id ~constructor snp
  | Ast0.OptStm stm ->
      let c ~item = Ast0.OptStm item in
      item_wrap ~item:stm ~item_posfn:statement_pos ~constructor:c snp
  | Ast0.ExprStatement(None, sem) ->
      None
  | Ast0.ExprStatement(Some exp, sem) ->
      let c ~exp ~mc = Ast0.ExprStatement(Some exp, mc) in
      let alt() = mcode_wrap ~mc:sem ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:sem) ~alt snp
  | Ast0.Exp exp ->
      let constructor ~exp = Ast0.Exp exp in
      exp_wrap ~exp ~constructor snp
  | Ast0.Decl (bef, decl) ->
      let c ~item = Ast0.Decl (bef, item) in
      item_wrap ~item:decl ~item_posfn:declaration_pos ~constructor:c snp
  | Ast0.Seq (lb, stmtdots, rb) ->
      let constructor ~mc = Ast0.Seq(mc, stmtdots, rb) in
      mcode_wrap ~mc:lb ~constructor snp
  | Ast0.IfThen (ifm, l, exp, r, st, a) ->
      let c ~exp ~mc = Ast0.IfThen(mc, l, exp, r, st, a) in
      let alt() = mcode_wrap ~mc:ifm ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:ifm) ~alt snp
  | Ast0.IfThenElse (ifm, l, exp, r, s1, e, s2, a) ->
      let c ~exp ~mc = Ast0.IfThenElse(mc, l, exp, r, s1, e, s2, a) in
      let alt() = mcode_wrap ~mc:ifm ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:ifm) ~alt snp
  | Ast0.While (whmc, l, exp, r, s, a) ->
      let c ~exp ~mc = Ast0.While(mc, l, exp, r, s, a) in
      let alt() = mcode_wrap ~mc:whmc ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:whmc) ~alt snp
  | Ast0.Do (d, s, whmc, l, exp, r, sem) ->
      let c ~exp ~mc = Ast0.Do(mc, s, whmc, l, exp, r, sem) in
      let alt() = mcode_wrap ~mc:d ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:d) ~alt snp
  | Ast0.For (fo,lp, fi,rp,stmt,a) ->
      let c ~item ~mc = Ast0.For (mc,lp,item,rp,stmt,a) in
      let alt() = mcode_wrap ~mc:fo ~constructor:(c ~item:fi) snp in
      item_wrap
        ~item:fi ~item_posfn:forinfo_pos ~constructor:(c ~mc:fo) ~alt snp
  | Ast0.Iterator (id,lp,expdots,rp,stmt,a) ->
      let constructor ~id = Ast0.Iterator(id, lp, expdots, rp, stmt, a) in
      id_wrap ~id ~constructor snp
  | Ast0.Switch (sw, lp, exp, rp, lb, sd, cd, rb) ->
      let _ = case_line_dots_pos cd snp in (* sanity check for disj *)
      let c ~exp ~mc = Ast0.Switch(mc, lp, exp, rp, lb, sd, cd, rb) in
      let alt() = mcode_wrap ~mc:sw ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:sw) ~alt snp
  | Ast0.Label (id,col) ->
      let constructor ~id = Ast0.Label(id, col) in
      id_wrap ~id ~constructor snp
  | Ast0.Goto (goto,id,sem) ->
      let constructor ~id = Ast0.Goto(goto, id, sem) in
      id_wrap ~id ~constructor snp
  | Ast0.Break (bmc,sem) ->
      let constructor ~mc = Ast0.Break(mc, sem) in
      mcode_wrap ~mc:bmc ~constructor snp
  | Ast0.Continue (cmc,sem) ->
      let constructor ~mc = Ast0.Continue(mc, sem) in
      mcode_wrap ~mc:cmc ~constructor snp
  | Ast0.ReturnExpr (retmc, exp, sem) ->
      let c ~exp ~mc = Ast0.ReturnExpr(mc, exp, sem) in
      let alt() = mcode_wrap ~mc:retmc ~constructor:(c ~exp) snp in
      exp_wrap ~exp ~constructor:(c ~mc:retmc) ~alt snp
  | Ast0.Return (retmc,sem) ->
      let constructor ~mc = Ast0.Return(mc, sem) in
      mcode_wrap ~mc:retmc ~constructor snp
  | Ast0.FunDecl (b, f, id, lp, ps, op, rp, ea, lb, sd, rb, a) ->
      let constructor ~id = Ast0.FunDecl(b,f,id,lp,ps,op,rp,ea,lb,sd,rb,a) in
      id_wrap ~id ~constructor snp
