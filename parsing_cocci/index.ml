(* create an index for each constructor *)

(* doesn't really work - requires that identical terms with no token
subterms (eg dots) not appear on the same line *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci

(* if a dot list is empty, add the starting line of the dot list to the
address.  Otherwise add 0.  An empty dot list should only match with another
empty one. *)
let expression_dots d =
  let ln = (Ast0.get_info d).Ast0.line_start in
  match Ast0.unwrap d with
    Ast0.DOTS(l) -> 1::(if l = [] then [ln] else [0])
  | Ast0.CIRCLES(l) -> 2::(if l = [] then [ln] else [0])
  | Ast0.STARS(l) -> 3::(if l = [] then [ln] else [0])
	
let parameter_dots d =
  let ln = (Ast0.get_info d).Ast0.line_start in
  match Ast0.unwrap d with
    Ast0.DOTS(l) -> 4::(if l = [] then [ln] else [0])
  | Ast0.CIRCLES(l) -> 5::(if l = [] then [ln] else [0])
  | Ast0.STARS(l) -> 6::(if l = [] then [ln] else [0])
	
let statement_dots d =
  let ln = (Ast0.get_info d).Ast0.line_start in
  match Ast0.unwrap d with
    Ast0.DOTS(l) -> 7::(if l = [] then [ln] else [0])
  | Ast0.CIRCLES(l) -> 8::(if l = [] then [ln] else [0])
  | Ast0.STARS(l) -> 9::(if l = [] then [ln] else [0])
	
let ident i =
  match Ast0.unwrap i with
    Ast0.Id(name) -> [10]
  | Ast0.MetaId(name) -> [11]
  | Ast0.MetaFunc(name) -> [12]
  | Ast0.MetaLocalFunc(name) -> [13]
  | Ast0.OptIdent(id) -> [14]
  | Ast0.UniqueIdent(id) -> [15]
  | Ast0.MultiIdent(id) -> [16]
	
let expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) -> [17]
  | Ast0.Constant(const) -> [18]
  | Ast0.FunCall(fn,lp,args,rp) -> [19]
  | Ast0.Assignment(left,op,right) -> [20]
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) -> [21]
  | Ast0.Postfix(exp,op) -> [22]
  | Ast0.Infix(exp,op) -> [23]
  | Ast0.Unary(exp,op) -> [24]
  | Ast0.Binary(left,op,right) -> [25]
  | Ast0.Paren(lp,exp,rp) -> [26]
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) -> [27]
  | Ast0.RecordAccess(exp,pt,field) -> [28]
  | Ast0.RecordPtAccess(exp,ar,field) -> [29]
  | Ast0.Cast(lp,ty,rp,exp) -> [30]
  | Ast0.SizeOfExpr(szf,exp) -> [98] (* added after *)
  | Ast0.SizeOfType(szf,lp,ty,rp) -> [99] (* added after *)
  | Ast0.MetaConst(name,ty) -> [31]
  | Ast0.MetaErr(name) -> [32]
  | Ast0.MetaExpr(name,ty) -> [33]
  | Ast0.MetaExprList(name) -> [34]
  | Ast0.EComma(cm) -> [35]
  | Ast0.DisjExpr(_,expr_list,_,_) -> [36]
  | Ast0.NestExpr(_,expr_dots,_,_) -> [37]
  | Ast0.Edots(dots,whencode) -> [38]
  | Ast0.Ecircles(dots,whencode) -> [39]
  | Ast0.Estars(dots,whencode) -> [40]
  | Ast0.OptExp(exp) -> [41]
  | Ast0.UniqueExp(exp) -> [42]
  | Ast0.MultiExp(exp) -> [43]

let typeC t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) -> [44]
  | Ast0.BaseType(ty,sign) -> [48]
  | Ast0.Pointer(ty,star) -> [49]
  | Ast0.Array(ty,lb,size,rb) -> [50]
  | Ast0.StructUnionName(name,kind) -> [51]
  | Ast0.TypeName(name) -> [52]
  | Ast0.MetaType(name) -> [53]
  | Ast0.OptType(ty) -> [45]
  | Ast0.UniqueType(ty) -> [46]
  | Ast0.MultiType(ty) -> [47]
	
let declaration d =
  match Ast0.unwrap d with
    Ast0.Init(ty,id,eq,exp,sem) -> [54]
  | Ast0.UnInit(ty,id,sem) -> [55]
  | Ast0.DisjDecl(_,decls,_,_) -> [97] (* added after *)
  | Ast0.OptDecl(decl) -> [56]
  | Ast0.UniqueDecl(decl) -> [57]
  | Ast0.MultiDecl(decl) -> [58]
	
let parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty) -> [59]
  | Ast0.Param(id,ty) -> [60]
  | Ast0.MetaParam(name) -> [61]
  | Ast0.MetaParamList(name) -> [62]
  | Ast0.PComma(cm) -> [63]
  | Ast0.Pdots(dots) -> [64]
  | Ast0.Pcircles(dots) -> [65]
  | Ast0.OptParam(param) -> [66]
  | Ast0.UniqueParam(param) -> [67]
	
let statement s =
  match Ast0.unwrap s with
    Ast0.FunDecl(stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
      [68]
  | Ast0.Decl(decl) -> [69]
  | Ast0.Seq(lbrace,body,rbrace) -> [70]
  | Ast0.ExprStatement(exp,sem) -> [71]
  | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) -> [72]
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) -> [73]
  | Ast0.While(whl,lp,exp,rp,body,_) -> [74]
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) -> [75]
  | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,_) -> [76]
  | Ast0.Break(br,sem) -> [100]
  | Ast0.Continue(cont,sem) -> [101]
  | Ast0.Return(ret,sem) -> [77]
  | Ast0.ReturnExpr(ret,exp,sem) -> [78]
  | Ast0.MetaStmt(name) -> [79]
  | Ast0.MetaStmtList(name) -> [80]
  | Ast0.Disj(_,statement_dots_list,_,_) -> [81]
  | Ast0.Nest(_,stmt_dots,_,_) -> [82]
  | Ast0.Exp(exp) -> [83]
  | Ast0.Dots(d,whencode) -> [84]
  | Ast0.Circles(d,whencode) -> [85]
  | Ast0.Stars(d,whencode) -> [86]
  | Ast0.OptStm(re) -> [87]
  | Ast0.UniqueStm(re) -> [88]
  | Ast0.MultiStm(re) -> [89]
	
let top_level t =
  match Ast0.unwrap t with
    Ast0.DECL(decl) -> [90]
  | Ast0.INCLUDE(inc,name) -> [91]
  | Ast0.FILEINFO(old_file,new_file) -> [92]
  | Ast0.FUNCTION(stmt_dots) -> [93]
  | Ast0.CODE(stmt_dots) -> [94]
  | Ast0.ERRORWORDS(exps) -> [95]
  | Ast0.OTHER(_) -> [96]

(* 99-101 already used *)
