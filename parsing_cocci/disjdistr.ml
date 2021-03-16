(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module V = Visitor_ast

let disjmult2 e1 e2 k =
  List.concat
    (List.map (function e1 -> List.map (function e2 -> k e1 e2) e2) e1)

let disjmult3 e1 e2 e3 k =
  List.concat
    (List.map
       (function e1 ->
	 List.concat
	   (List.map
	      (function e2 -> List.map (function e3 -> k e1 e2 e3) e3)
	      e2))
       e1)

let rec disjmult f = function
    [] -> [[]]
  | x::xs ->
      let cur = f x in
      let rest = disjmult f xs in
      disjmult2 cur rest (function cur -> function rest -> cur :: rest)

let disjmult_two fstart frest (start,rest) =
  let cur = fstart start in
  let rest = disjmult frest rest in
  disjmult2 cur rest (function cur -> function rest -> (cur,rest))

let disjtwoelems fstart frest (start,rest) =
  let cur = fstart start in
  let rest = frest rest in
  disjmult2 cur rest (function cur -> function rest -> (cur,rest))

let disjoption f = function
    None -> [None]
  | Some x -> List.map (function x -> Some x) (f x)

let disjdots f d =
  List.map (function l -> Ast.rewrap d l) (disjmult f (Ast.unwrap d))

let rec disjty ft =
  match Ast.unwrap ft with
    Ast.Type(allminus,cv,ty) ->
      let ty = disjtypeC ty in
      List.map (function ty -> Ast.rewrap ft (Ast.Type(allminus,cv,ty))) ty
  | Ast.AsType(ty,asty) -> (* as ty doesn't contain disj *)
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap ft (Ast.AsType(ty,asty))) ty
  | Ast.DisjType(types) -> List.concat (List.map disjty types)
  | Ast.ConjType(types) ->
      let types = disjmult disjty types in
      List.map (function types -> Ast.rewrap ft (Ast.ConjType(types))) types
  | Ast.OptType(ty) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap ft (Ast.OptType(ty))) ty

and disjtypeC bty =
  match Ast.unwrap bty with
    Ast.BaseType(_) | Ast.SignedT(_,_) -> [bty]
  | Ast.Pointer(ty,star) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap bty (Ast.Pointer(ty,star))) ty
  | Ast.ParenType(lp,ty,rp) ->
      let ty = disjty ty in
      List.map
        (function ty ->
          Ast.rewrap bty (Ast.ParenType(lp,ty,rp))) ty
  | Ast.FunctionType(ty,lp,params,rp) ->
      let ty = disjty ty in
      List.map
        (function ty ->
          Ast.rewrap bty (Ast.FunctionType(ty,lp,params,rp))) ty
  | Ast.Array(ty,lb,size,rb) ->
      disjmult2 (disjty ty) (disjoption disjexp size)
	(function ty -> function size ->
	  Ast.rewrap bty (Ast.Array(ty,lb,size,rb)))
  | Ast.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      disjmult2 (disjexp length) (disjoption disjexp precision_opt)
	(function length -> function precision_opt ->
	  Ast.rewrap bty (Ast.Decimal(dec,lp,length,comma,precision_opt,rp)))
  | Ast.EnumName(enum,name) ->
      let name = disjoption disjident name in
      List.map (function name -> Ast.rewrap bty (Ast.EnumName(enum,name))) name
  | Ast.StructUnionName(su,name) ->
      let name = disjoption disjident name in
      List.map
	(function name -> Ast.rewrap bty (Ast.StructUnionName(su,name)))
	name
  | Ast.EnumDef(ty,lb,ids,rb) ->
      disjmult2 (disjty ty) (disjdots disjenumdecl ids)
	(function ty -> function ids ->
	  Ast.rewrap bty (Ast.EnumDef(ty,lb,ids,rb)))
  | Ast.StructUnionDef(ty,lb,decls,rb) ->
      disjmult2 (disjty ty) (disjdots anndisjfield decls)
	(function ty -> function decls ->
	  Ast.rewrap bty (Ast.StructUnionDef(ty,lb,decls,rb)))
  | Ast.TypeOfExpr(tf,lp,exp,rp) ->
      let exp = disjexp exp in
      List.map
	(function exp -> Ast.rewrap bty (Ast.TypeOfExpr(tf,lp,exp,rp))) exp
  | Ast.TypeOfType(tf,lp,ty,rp) ->
      let ty = disjty ty in
      List.map
	(function ty -> Ast.rewrap bty (Ast.TypeOfType(tf,lp,ty,rp))) ty
  | Ast.TypeName(_) | Ast.AutoType(_) | Ast.MetaType(_,_,_,_) -> [bty]

and anndisjdecl d =
  match Ast.unwrap d with
    Ast.DElem(bef,allminus,decls) ->
      List.map
	(function decl -> Ast.rewrap d (Ast.DElem(bef,allminus,decl)))
	(disjdecl decls)

and anndisjfield d =
  match Ast.unwrap d with
    Ast.FElem(bef,allminus,decls) ->
      List.map
	(function decl -> Ast.rewrap d (Ast.FElem(bef,allminus,decl)))
	(disjfield decls)
  | Ast.Fdots(_,_) -> [d]
  | Ast.DisjField(decls) -> List.concat (List.map anndisjfield decls)
  | Ast.ConjField(decl_list) ->
      let decl_list = disjmult anndisjfield decl_list in
      List.map (function decl_list -> Ast.rewrap d (Ast.ConjField(decl_list)))
	decl_list
  | Ast.OptField(decl) ->
      let decl = anndisjfield decl in
      List.map (function decl -> Ast.rewrap d (Ast.OptField(decl))) decl

and disjenumdecl d =
  match Ast.unwrap d with
    Ast.Enum(name,enum_val) ->
      let name = disjident name in
      (match enum_val with
        None ->
          List.map (function name -> Ast.rewrap d (Ast.Enum(name,None)))
            name
      | Some (eq,eval) ->
          disjmult2 name (disjexp eval)
          (function name -> function eval ->
	    Ast.rewrap d (Ast.Enum(name,Some(eq,eval)))))
  | Ast.EnumComma(cm) -> [d]
  | Ast.EnumDots(dots,whencode) -> [d]

and disjident e =
  match Ast.unwrap e with
    Ast.DisjId(id_list) -> List.concat (List.map disjident id_list)
  | Ast.ConjId(id_list) ->
      let id_list = disjmult disjident id_list in
      List.map (function id_list -> Ast.rewrap e (Ast.ConjId(id_list)))
	id_list
  | Ast.OptIdent(id) ->
      let id = disjident id in
      List.map (function id -> Ast.rewrap e (Ast.OptIdent(id))) id
  | _ -> [e]

and disjexp e =
  match Ast.unwrap e with
    Ast.Ident(_) | Ast.Constant _ | Ast.StringConstant _ ->
      [e] (* even Ident can't contain disj, nor StringConstant *)
  | Ast.FunCall(fn,lp,args,rp) ->
      disjmult2 (disjexp fn) (disjdots disjexp args)
	(function fn -> function args ->
	  Ast.rewrap e (Ast.FunCall(fn,lp,args,rp)))
  | Ast.Assignment(left,op,right,simple) ->
      disjmult2 (disjexp left) (disjexp right)
	(function left -> function right ->
	  Ast.rewrap e (Ast.Assignment(left,op,right,simple)))
  | Ast.Sequence(left,op,right) ->
      disjmult2 (disjexp left) (disjexp right)
	(function left -> function right ->
	  Ast.rewrap e (Ast.Sequence(left,op,right)))
  | Ast.CondExpr(exp1,why,Some exp2,colon,exp3) ->
      let res = disjmult disjexp [exp1;exp2;exp3] in
      List.map
	(function
	    [exp1;exp2;exp3] ->
	      Ast.rewrap e (Ast.CondExpr(exp1,why,Some exp2,colon,exp3))
	  | _ -> failwith "not possible")
	res
  | Ast.CondExpr(exp1,why,None,colon,exp3) ->
      disjmult2 (disjexp exp1) (disjexp exp3)
	(function exp1 -> function exp3 ->
	  Ast.rewrap e (Ast.CondExpr(exp1,why,None,colon,exp3)))
  | Ast.Postfix(exp,op) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.Postfix(exp,op))) exp
  | Ast.Infix(exp,op) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.Infix(exp,op))) exp
  | Ast.Unary(exp,op) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.Unary(exp,op))) exp
  | Ast.Binary(left,op,right) ->
      disjmult2 (disjexp left) (disjexp right)
	(function left -> function right ->
	  Ast.rewrap e (Ast.Binary(left,op,right)))
  | Ast.Nested(exp,op,right) ->
      (* disj not possible in right *)
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.Nested(exp,op,right))) exp
  | Ast.Paren(lp,exp,rp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.Paren(lp,exp,rp))) exp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      disjmult2 (disjexp exp1) (disjexp exp2)
	(function exp1 -> function exp2 ->
	  Ast.rewrap e (Ast.ArrayAccess(exp1,lb,exp2,rb)))
  | Ast.RecordAccess(exp,pt,field) ->
      disjmult2 (disjexp exp) (disjident field)
	(fun exp field -> Ast.rewrap e (Ast.RecordAccess(exp,pt,field)))
  | Ast.RecordPtAccess(exp,ar,field) ->
      disjmult2 (disjexp exp) (disjident field)
	(fun exp field -> Ast.rewrap e (Ast.RecordPtAccess(exp,ar,field)))
  | Ast.Cast(lp,ty,attr,rp,exp) ->
      disjmult2 (disjty ty) (disjexp exp)
	(function ty -> function exp ->
          Ast.rewrap e (Ast.Cast(lp,ty,attr,rp,exp)))
  | Ast.SizeOfExpr(szf,exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.SizeOfExpr(szf,exp))) exp
  | Ast.SizeOfType(szf,lp,ty,rp) ->
      let ty = disjty ty in
      List.map
	(function ty -> Ast.rewrap e (Ast.SizeOfType(szf,lp,ty,rp))) ty
  | Ast.TypeExp(ty) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap e (Ast.TypeExp(ty))) ty
  | Ast.Constructor(lp,ty,rp,init) ->
      disjmult2 (disjty ty) (disjini init)
	(function ty ->
	  function exp -> Ast.rewrap e (Ast.Constructor(lp,ty,rp,init)))
  | Ast.MetaErr(_,_,_,_) | Ast.MetaExpr(_,_,_,_,_,_,_)
  | Ast.MetaExprList(_,_,_,_,_) | Ast.EComma(_) -> [e]
  | Ast.AsExpr(exp,asexp) -> (* as exp doesn't contain disj *)
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.AsExpr(exp,asexp))) exp
  | Ast.AsSExpr(exp,asstm) -> (* as exp doesn't contain disj *)
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.AsSExpr(exp,asstm))) exp
  | Ast.DisjExpr(exp_list) -> List.concat (List.map disjexp exp_list)
  | Ast.ConjExpr(exp_list) ->
      let exp_list = disjmult disjexp exp_list in
      List.map (function exp_list -> Ast.rewrap e (Ast.ConjExpr(exp_list)))
	exp_list
  | Ast.NestExpr(starter,expr_dots,ender,whencode,multi) ->
      (* not sure what to do here, so ambiguities still possible *)
      [e]
  | Ast.Edots(dots,_) -> [e]
  | Ast.OptExp(exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.OptExp(exp))) exp

and disjparam p =
  match Ast.unwrap p with
    Ast.VoidParam(ty,attr) -> [p] (* void is the only possible value *)
  | Ast.Param(ty,id,attr) ->
      disjmult2 (disjty ty) (disjoption disjident id)
	(fun ty id -> Ast.rewrap p (Ast.Param(ty,id,attr)))
  | Ast.AsParam(pm,asexp) -> (* as exp doesn't contain disj *)
      let pm = disjparam pm in
      List.map (function pm -> Ast.rewrap p (Ast.AsParam(pm,asexp))) pm
  | Ast.MetaParam(_,_,_,_) | Ast.MetaParamList(_,_,_,_,_) | Ast.PComma(_) -> [p]
  | Ast.Pdots(dots) -> [p]
  | Ast.OptParam(param) ->
      let param = disjparam param in
      List.map (function param -> Ast.rewrap p (Ast.OptParam(param))) param

and disjini i =
  match Ast.unwrap i with
    Ast.MetaInit(_,_,_,_) | Ast.MetaInitList(_,_,_,_,_) -> [i]
  | Ast.AsInit(ini,asini) ->
      let ini = disjini ini in
      List.map (function ini -> Ast.rewrap i (Ast.AsInit(ini,asini))) ini
  | Ast.InitExpr(exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap i (Ast.InitExpr(exp))) exp
  | Ast.ArInitList(lb,initlist,rb) ->
      List.map
	(function initlist ->
	  Ast.rewrap i (Ast.ArInitList(lb,initlist,rb)))
	(disjdots disjini initlist)
  | Ast.StrInitList(allminus,lb,initlist,rb,whencode) ->
      List.map
	(function initlist ->
	  Ast.rewrap i (Ast.StrInitList(allminus,lb,initlist,rb,whencode)))
	(disjmult disjini initlist)
  | Ast.InitGccExt(designators,eq,ini) ->
      let designators = disjmult designator designators in
      let ini = disjini ini in
      disjmult2 designators ini
	(function designators -> function ini ->
	  Ast.rewrap i (Ast.InitGccExt(designators,eq,ini)))
  | Ast.InitGccName(name,eq,ini) ->
      let ini = disjini ini in
      List.map
	(function ini -> Ast.rewrap i (Ast.InitGccName(name,eq,ini)))
	ini
  | Ast.IComma(comma) -> [i]
  | Ast.Idots(dots,_) -> [i]
  | Ast.OptIni(ini) ->
      let ini = disjini ini in
      List.map (function ini -> Ast.rewrap i (Ast.OptIni(ini))) ini

and designator = function
    Ast.DesignatorField(dot,id) ->
      let id = disjident id in
      List.map (function id -> Ast.DesignatorField(dot,id)) id
  | Ast.DesignatorIndex(lb,exp,rb) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.DesignatorIndex(lb,exp,rb)) exp
  | Ast.DesignatorRange(lb,min,dots,max,rb) ->
      disjmult2 (disjexp min) (disjexp max)
	(function min -> function max ->
	  Ast.DesignatorRange(lb,min,dots,max,rb))

and disjfninfo = function
  | Ast.FType(ty) -> List.map (function ty -> Ast.FType(ty)) (disjty ty)
  | fi -> [fi]

and disjdecl d =
  match Ast.unwrap d with
    Ast.MetaDecl(_,_,_,_) -> [d]
  | Ast.AsDecl(decl,asdecl) ->
      let decl = disjdecl decl in
      List.map (function decl -> Ast.rewrap d (Ast.AsDecl(decl,asdecl))) decl
  | Ast.Init(stg,ty,id,attr,eq,ini,sem) ->
      disjmult3 (disjty ty) (disjident id) (disjini ini)
	(fun ty id ini ->
	  Ast.rewrap d (Ast.Init(stg,ty,id,attr,eq,ini,sem)))
  | Ast.UnInit(stg,ty,id,attr,sem) ->
      disjmult2 (disjty ty) (disjident id)
	(fun ty id ->
	  Ast.rewrap d (Ast.UnInit(stg,ty,id,attr,sem)))
  | Ast.FunProto(fninfo,name,lp1,params,va,rp1,sem) ->
      disjmult2 (disjmult disjfninfo fninfo) (disjident name)
	(fun fninfo name ->
	  Ast.rewrap d (Ast.FunProto(fninfo,name,lp1,params,va,rp1,sem)))
  | Ast.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
      disjmult2 (disjident name) (disjdots disjexp args)
	(fun name args ->
	  Ast.rewrap d (Ast.MacroDecl(stg,name,lp,args,rp,attr,sem)))
  | Ast.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
      disjmult3 (disjident name) (disjdots disjexp args) (disjini ini)
	(fun name args ini ->
	  Ast.rewrap d (Ast.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem)))
  | Ast.TyDecl(ty,attr,sem) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap d (Ast.TyDecl(ty,attr,sem))) ty
  | Ast.Typedef(stg,ty,id,sem) ->
      let ty = disjty ty in (* disj not allowed in id *)
      List.map (function ty -> Ast.rewrap d (Ast.Typedef(stg,ty,id,sem))) ty
  | Ast.DisjDecl(decls) -> List.concat (List.map disjdecl decls)
  | Ast.ConjDecl(decl_list) ->
      let decl_list = disjmult disjdecl decl_list in
      List.map (function decl_list -> Ast.rewrap d (Ast.ConjDecl(decl_list)))
	decl_list
  | Ast.OptDecl(decl) ->
      let decl = disjdecl decl in
      List.map (function decl -> Ast.rewrap d (Ast.OptDecl(decl))) decl

and disjfield d =
  match Ast.unwrap d with
    Ast.MetaField(_,_,_,_)
  | Ast.MetaFieldList(_,_,_,_,_) -> [d]
  | Ast.Field(ty,id,bf,sem) ->
      let disjbf (c, e) = List.map (fun e -> (c, e)) (disjexp e) in
      disjmult3 (disjty ty) (disjoption disjident id) (disjoption disjbf bf)
	(fun ty id bf ->
	  Ast.rewrap d (Ast.Field(ty,id,bf,sem)))

let generic_orify_rule_elem f re exp rebuild =
  match f exp with
    [exp] -> re
  | orexps -> Ast.rewrap re (Ast.DisjRuleElem (List.map rebuild orexps))

let orify_rule_elem re exp rebuild =
  generic_orify_rule_elem disjexp re exp rebuild

let orify_rule_elem_ty = generic_orify_rule_elem disjty
let orify_rule_elem_id = generic_orify_rule_elem disjident
let orify_rule_elem_param = generic_orify_rule_elem disjparam
let orify_rule_elem_decl = generic_orify_rule_elem disjdecl
let orify_rule_elem_anndecl = generic_orify_rule_elem anndisjdecl
let orify_rule_elem_ini = generic_orify_rule_elem disjini

let rec disj_rule_elem r k re =
  match Ast.unwrap re with
    Ast.FunHeader(bef,allminus,fninfo,name,lp,params,va,rp) ->
      generic_orify_rule_elem
	(disjtwoelems disjident (disjdots disjparam)) re
	(name,params)
	(fun (name,params) ->
	  Ast.rewrap re
	    (Ast.FunHeader(bef,allminus,fninfo,name,lp,params,va,rp)))
  | Ast.Decl decl ->
      orify_rule_elem_anndecl re decl
	(function decl -> Ast.rewrap re (Ast.Decl decl))
  | Ast.SeqStart(brace) -> re
  | Ast.SeqEnd(brace) -> re
  | Ast.ExprStatement(Some exp,sem) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.ExprStatement(Some exp,sem)))
  | Ast.ExprStatement(None,sem) -> re
  | Ast.IfHeader(iff,lp,exp,rp) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.IfHeader(iff,lp,exp,rp)))
  | Ast.Else(els) -> re
  | Ast.WhileHeader(whl,lp,exp,rp) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.WhileHeader(whl,lp,exp,rp)))
  | Ast.DoHeader(d) -> re
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.WhileTail(whl,lp,exp,rp,sem)))
  | Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
      let disjfirst = function
	  Ast.ForExp(e1,sem1) ->
	    List.map (function e1 -> Ast.ForExp(e1,sem1))
	      (disjoption disjexp e1)
	| Ast.ForDecl decl ->
	    List.map (function decl -> Ast.ForDecl decl) (anndisjdecl decl) in
      generic_orify_rule_elem
	(disjmult_two disjfirst (disjoption disjexp)) re (first,[e2;e3])
        (function
            (first,[exp2;exp3]) ->
              Ast.rewrap re (Ast.ForHeader(fr,lp,first,exp2,sem2,exp3,rp))
          |  _ -> failwith "not possible")
  | Ast.IteratorHeader(whl,lp,args,rp) ->
      generic_orify_rule_elem (disjdots disjexp) re args
	(function args -> Ast.rewrap re (Ast.IteratorHeader(whl,lp,args,rp)))
  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.SwitchHeader(switch,lp,exp,rp)))
  | Ast.Break(_,_) | Ast.Continue(_,_) | Ast.Label(_,_) | Ast.Goto(_,_,_)
  | Ast.Return(_,_) -> re
  | Ast.ReturnExpr(ret,exp,sem) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.ReturnExpr(ret,exp,sem)))
  | Ast.Exec _ -> re (* no ors possible *)
  | Ast.MetaRuleElem(_,_,_,_) | Ast.MetaStmt(_,_,_,_,_)
  | Ast.MetaStmtList(_,_,_,_,_) -> re
  | Ast.Exp(exp) ->
      orify_rule_elem re exp (function exp -> Ast.rewrap exp (Ast.Exp(exp)))
  | Ast.TopExp(exp) ->
      orify_rule_elem re exp (function exp -> Ast.rewrap exp (Ast.TopExp(exp)))
  | Ast.Ty(ty) ->
      orify_rule_elem_ty re ty (function ty -> Ast.rewrap ty (Ast.Ty(ty)))
  | Ast.TopId(id) ->
      orify_rule_elem_id re id (function id -> Ast.rewrap id (Ast.TopId(id)))
  | Ast.TopInit(init) ->
      orify_rule_elem_ini re init
	(function init -> Ast.rewrap init (Ast.TopInit(init)))
  | Ast.Include(inc,_) | Ast.MetaInclude(inc,_) -> re
  | Ast.Undef(def,id) -> re
  | Ast.DefineHeader(def,id,params) -> re
  | Ast.Pragma(prg,id,body) -> re
  | Ast.Default(def,colon) -> re
  | Ast.Case(case,exp,colon) ->
      orify_rule_elem re exp
	(function exp -> Ast.rewrap re (Ast.Case(case,exp,colon)))
  | Ast.AsRe(rre,asre) -> (* as re doesn't contain disj *)
      let rre = disj_rule_elem r k rre in
      Ast.rewrap re (Ast.AsRe(rre,asre))
  | Ast.DisjRuleElem(l) ->
      (* only case lines *)
      Ast.rewrap re (Ast.DisjRuleElem(List.map (disj_rule_elem r k) l))

let disj_all =
  let mcode x = x in
  let donothing r k e = k e in
  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing disj_rule_elem donothing donothing donothing donothing
    donothing donothing

(* ----------------------------------------------------------------------- *)
(* collect iso information at the rule_elem level *)

let collect_all_isos =
  let bind = (@) in
  let option_default = [] in
  let mcode r x = [] in
  let donothing r k e = Common.union_set (Ast.get_isos e) (k e) in
  let doanything r k e = k e in
  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing
    doanything donothing doanything donothing donothing donothing donothing
    donothing donothing donothing doanything

let collect_iso_info =
  let mcode x = x in
  let donothing r k e = k e in
  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.DisjRuleElem(l) -> k e
    | _ ->
	let isos = collect_all_isos.V.combiner_rule_elem e in
	Ast.set_isos e isos in
  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing rule_elem donothing donothing
    donothing donothing donothing donothing

(* ----------------------------------------------------------------------- *)

let disj rules =
  List.map
    (function (mv,r) ->
      match r with
        Ast.ScriptRule _
      | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> (mv, r)
      | Ast.CocciRule (nm, rule_info, r, isexp, ruletype) ->
	  let res =
	    List.map
	      (function x ->
		let res = disj_all.V.rebuilder_top_level x in
		if !Flag.track_iso_usage
		then collect_iso_info.V.rebuilder_top_level res
		else res)
	      r in
	  (mv, Ast.CocciRule (nm,rule_info,res,isexp,ruletype)))
    rules
