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

let disjdots f d =
  match Ast.unwrap d with
    Ast.DOTS(l) ->
      List.map (function l -> Ast.rewrap d (Ast.DOTS(l))) (disjmult f l)
  | Ast.CIRCLES(l) ->
      List.map (function l -> Ast.rewrap d (Ast.CIRCLES(l))) (disjmult f l)
  | Ast.STARS(l) ->
      List.map (function l -> Ast.rewrap d (Ast.STARS(l))) (disjmult f l)

let rec disjty ft =
  match Ast.unwrap ft with
    Ast.Type(cv,ty) -> [ft]
  | Ast.DisjType(types) -> List.concat (List.map disjty types)
  | Ast.OptType(ty) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap ft (Ast.OptType(ty))) ty
  | Ast.UniqueType(ty) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap ft (Ast.UniqueType(ty))) ty
  | Ast.MultiType(ty) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap ft (Ast.MultiType(ty))) ty

let rec disjexp e =
  match Ast.unwrap e with
    Ast.Ident(_) | Ast.Constant(_) -> [e]
  | Ast.FunCall(fn,lp,args,rp) ->
      disjmult2 (disjexp fn) (disjdots disjexp args)
	(function fn -> function args ->
	  Ast.rewrap e (Ast.FunCall(fn,lp,args,rp)))
  | Ast.Assignment(left,op,right) ->
      disjmult2 (disjexp left) (disjexp right)
	(function left -> function right ->
	  Ast.rewrap e (Ast.Assignment(left,op,right)))
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
  | Ast.Paren(lp,exp,rp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.Paren(lp,exp,rp))) exp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      disjmult2 (disjexp exp1) (disjexp exp2)
	(function exp1 -> function exp2 ->
	  Ast.rewrap e (Ast.ArrayAccess(exp1,lb,exp2,rb)))
  | Ast.RecordAccess(exp,pt,field) ->
      let exp = disjexp exp in
      List.map
	(function exp -> Ast.rewrap e (Ast.RecordAccess(exp,pt,field))) exp
  | Ast.RecordPtAccess(exp,ar,field) ->
      let exp = disjexp exp in
      List.map
	(function exp -> Ast.rewrap e (Ast.RecordPtAccess(exp,ar,field))) exp
  | Ast.Cast(lp,ty,rp,exp) ->
      disjmult2 (disjty ty) (disjexp exp)
	(function ty -> function exp -> Ast.rewrap e (Ast.Cast(lp,ty,rp,exp)))
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
  | Ast.MetaErr(_,_,_) | Ast.MetaExpr(_,_,_,_,_)
  | Ast.MetaExprList(_,_,_) | Ast.EComma(_) -> [e]
  | Ast.DisjExpr(exp_list) -> List.concat (List.map disjexp exp_list)
  | Ast.NestExpr(expr_dots,whencode) ->
      (* not sure what to do here, so ambiguities still possible *)
      [e]
  | Ast.Edots(dots,_) | Ast.Ecircles(dots,_) | Ast.Estars(dots,_) -> [e]
  | Ast.OptExp(exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.OptExp(exp))) exp
  | Ast.UniqueExp(exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.UniqueExp(exp))) exp
  | Ast.MultiExp(exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap e (Ast.MultiExp(exp))) exp

let rec disjparam p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> [p] (* void is the only possible value *)
  | Ast.Param(ty,id) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap p (Ast.Param(ty,id))) ty
  | Ast.MetaParam(_,_,_) | Ast.MetaParamList(_,_,_) | Ast.PComma(_) -> [p]
  | Ast.Pdots(dots) | Ast.Pcircles(dots) -> [p]
  | Ast.OptParam(param) ->
      let param = disjparam param in
      List.map (function param -> Ast.rewrap p (Ast.OptParam(param))) param
  | Ast.UniqueParam(param) ->
      let param = disjparam param in
      List.map (function param -> Ast.rewrap p (Ast.UniqueParam(param))) param

let rec disjini i =
  match Ast.unwrap i with
    Ast.InitExpr(exp) ->
      let exp = disjexp exp in
      List.map (function exp -> Ast.rewrap i (Ast.InitExpr(exp))) exp
  | Ast.InitList(lb,initlist,rb,whencode) ->
      List.map
	(function initlist ->
	  Ast.rewrap i (Ast.InitList(lb,initlist,rb,whencode)))
	(disjmult disjini initlist)
  | Ast.InitGccDotName(dot,name,eq,ini) ->
      let ini = disjini ini in
      List.map
	(function ini -> Ast.rewrap i (Ast.InitGccDotName(dot,name,eq,ini)))
	ini
  | Ast.InitGccName(name,eq,ini) ->
      let ini = disjini ini in
      List.map
	(function ini -> Ast.rewrap i (Ast.InitGccName(name,eq,ini)))
	ini
  | Ast.InitGccIndex(lb,exp,rb,eq,ini) ->
      disjmult2 (disjexp exp) (disjini ini)
	(function exp -> function ini ->
	  Ast.rewrap i (Ast.InitGccIndex(lb,exp,rb,eq,ini)))
  | Ast.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
      disjmult3 (disjexp exp1) (disjexp exp2) (disjini ini)
	(function exp1 -> function exp2 -> function ini ->
	  Ast.rewrap i (Ast.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini)))
  | Ast.IComma(comma) -> [i]
  | Ast.OptIni(ini) -> 
      let ini = disjini ini in
      List.map (function ini -> Ast.rewrap i (Ast.OptIni(ini))) ini
  | Ast.UniqueIni(ini) -> 
      let ini = disjini ini in
      List.map (function ini -> Ast.rewrap i (Ast.UniqueIni(ini))) ini
  | Ast.MultiIni(ini) -> 
      let ini = disjini ini in
      List.map (function ini -> Ast.rewrap i (Ast.MultiIni(ini))) ini

let rec disjdecl d =
  match Ast.unwrap d with
    Ast.Init(stg,ty,id,eq,ini,sem) ->
      disjmult2 (disjty ty) (disjini ini)
	(function ty -> function ini ->
	  Ast.rewrap d (Ast.Init(stg,ty,id,eq,ini,sem)))
  | Ast.UnInit(stg,ty,id,sem) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap d (Ast.UnInit(stg,ty,id,sem))) ty
  | Ast.MacroDecl(name,lp,args,rp,sem) ->
      List.map
	(function args -> Ast.rewrap d (Ast.MacroDecl(name,lp,args,rp,sem)))
	(disjdots disjexp args)
  | Ast.TyDecl(ty,sem) ->
      let ty = disjty ty in
      List.map (function ty -> Ast.rewrap d (Ast.TyDecl(ty,sem))) ty
  | Ast.Typedef(stg,ty,id,sem) ->
      let ty = disjty ty in (* disj not allowed in id *)
      List.map (function ty -> Ast.rewrap d (Ast.Typedef(stg,ty,id,sem))) ty
  | Ast.DisjDecl(decls) -> List.concat (List.map disjdecl decls)
  | Ast.Ddots(_,_) | Ast.MetaDecl(_,_,_) -> [d]
  | Ast.OptDecl(decl) ->
      let decl = disjdecl decl in
      List.map (function decl -> Ast.rewrap d (Ast.OptDecl(decl))) decl
  | Ast.UniqueDecl(decl) ->
      let decl = disjdecl decl in
      List.map (function decl -> Ast.rewrap d (Ast.UniqueDecl(decl))) decl
  | Ast.MultiDecl(decl) ->
      let decl = disjdecl decl in
      List.map (function decl -> Ast.rewrap d (Ast.MultiDecl(decl))) decl

let generic_orify_rule_elem f re exp rebuild =
  match f exp with
    [exp] -> re
  | orexps ->
      Ast.rewrap re
	(Ast.DisjRuleElem
	   (List.map (function exp -> Ast.rewrap re (rebuild exp)) orexps))

let orify_rule_elem re exp rebuild =
  generic_orify_rule_elem disjexp re exp rebuild

let orify_rule_elem_ty = generic_orify_rule_elem disjty
let orify_rule_elem_param = generic_orify_rule_elem disjparam
let orify_rule_elem_decl = generic_orify_rule_elem disjdecl

let disj_rule_elem r k re =
  match Ast.unwrap re with      
    Ast.FunHeader(bef,allminus,fninfo,name,lp,params,rp) ->
      generic_orify_rule_elem (disjdots disjparam) re params
	(function params ->
	  Ast.FunHeader(bef,allminus,fninfo,name,lp,params,rp))
  | Ast.Decl(bef,allminus,decl) ->
      orify_rule_elem_decl re decl
	(function decl -> Ast.Decl(bef,allminus,decl))
  | Ast.SeqStart(brace) -> re
  | Ast.SeqEnd(brace) -> re
  | Ast.ExprStatement(exp,sem) ->
      orify_rule_elem re exp (function exp -> Ast.ExprStatement(exp,sem))
  | Ast.IfHeader(iff,lp,exp,rp) ->
      orify_rule_elem re exp (function exp -> Ast.IfHeader(iff,lp,exp,rp))
  | Ast.Else(els) -> re
  | Ast.WhileHeader(whl,lp,exp,rp) ->
      orify_rule_elem re exp (function exp -> Ast.WhileHeader(whl,lp,exp,rp))
  | Ast.DoHeader(d) -> re
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      orify_rule_elem re exp (function exp -> Ast.WhileTail(whl,lp,exp,rp,sem))
  | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
      let disjexpopt = function
	  None -> [None]
	| Some exp -> List.map (function x -> Some x) (disjexp exp) in
      generic_orify_rule_elem (disjmult disjexpopt) re [e1;e2;e3]
	(function
	    [exp1;exp2;exp3] -> Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp)
	  | _ -> failwith "not possible")
  | Ast.IteratorHeader(whl,lp,args,rp) ->
      generic_orify_rule_elem (disjdots disjexp) re args
	(function args -> Ast.IteratorHeader(whl,lp,args,rp))
  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      orify_rule_elem re exp
	(function exp -> Ast.SwitchHeader(switch,lp,exp,rp))
  | Ast.Break(_,_) | Ast.Continue(_,_) | Ast.Goto
  | Ast.Return(_,_) -> re
  | Ast.ReturnExpr(ret,exp,sem) ->
      orify_rule_elem re exp (function exp -> Ast.ReturnExpr(ret,exp,sem))
  | Ast.MetaRuleElem(_,_,_) | Ast.MetaStmt(_,_,_,_)
  | Ast.MetaStmtList(_,_,_) -> re
  | Ast.Exp(exp) ->
      orify_rule_elem re exp (function exp -> Ast.Exp(exp))
  | Ast.TopExp(exp) ->
      orify_rule_elem re exp (function exp -> Ast.TopExp(exp))
  | Ast.Ty(ty) ->
      orify_rule_elem_ty re ty (function ty -> Ast.Ty(ty))
  | Ast.Include(inc,s) -> re
  | Ast.DefineHeader(def,id,params) -> re
  | Ast.Default(def,colon) -> re
  | Ast.Case(case,exp,colon) ->
      orify_rule_elem re exp (function exp -> Ast.Case(case,exp,colon))
  | Ast.DisjRuleElem(_) -> failwith "not possible"

let disj_all =
  let mcode x = x in
  let donothing r k e = k e in
  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    disj_rule_elem donothing donothing donothing donothing

(* ----------------------------------------------------------------------- *)

let disj rules =
  List.map
    (function (mv,(nm,rule_info,r)) ->
      let res =
	List.map disj_all.V.rebuilder_top_level r in (mv,(nm,rule_info,res)))
    rules
