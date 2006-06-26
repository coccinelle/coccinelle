(* create an index for each constructor *)

(* doesn't really work - requires that identical terms with no token
subterms (eg dots) not appear on the same line *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci

let get_info (_,info,_,_) =
  (info.Ast0.line_start,info.Ast0.line_end,info.Ast0.offset)

let expression_dots d =
  let (lns,lne,offset) = get_info d in
  match Ast0.unwrap d with
    Ast0.DOTS(l) -> [1;lns;lne;offset;List.length l]
  | Ast0.CIRCLES(l) -> [2;lns;lne;offset;List.length l]
  | Ast0.STARS(l) -> [3;lns;lne;offset;List.length l]
	
let parameter_dots d =
  let (lns,lne,offset) = get_info d in
  match Ast0.unwrap d with
    Ast0.DOTS(l) -> [4;lns;lne;offset;List.length l]
  | Ast0.CIRCLES(l) -> [5;lns;lne;offset;List.length l]
  | Ast0.STARS(l) -> [6;lns;lne;offset;List.length l]
	
let statement_dots d =
  let (lns,lne,offset) = get_info d in
  match Ast0.unwrap d with
    Ast0.DOTS(l) -> [7;lns;lne;offset;List.length l]
  | Ast0.CIRCLES(l) -> [8;lns;lne;offset;List.length l]
  | Ast0.STARS(l) -> [9;lns;lne;offset;List.length l]
	
let ident i =
  let (lns,lne,offset) = get_info i in
  match Ast0.unwrap i with
    Ast0.Id(name) -> [10;lns;lne;offset]
  | Ast0.MetaId(name) -> [11;lns;lne;offset]
  | Ast0.MetaFunc(name) -> [12;lns;lne;offset]
  | Ast0.MetaLocalFunc(name) -> [13;lns;lne;offset]
  | Ast0.OptIdent(id) -> [14;lns;lne;offset]
  | Ast0.UniqueIdent(id) -> [15;lns;lne;offset]
  | Ast0.MultiIdent(id) -> [16;lns;lne;offset]
	
let expression e =
  let (lns,lne,offset) = get_info e in
  match Ast0.unwrap e with
    Ast0.Ident(id) -> [17;lns;lne;offset]
  | Ast0.Constant(const) -> [18;lns;lne;offset]
  | Ast0.FunCall(fn,lp,args,rp) -> [19;lns;lne;offset]
  | Ast0.Assignment(left,op,right) -> [20;lns;lne;offset]
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) -> [21;lns;lne;offset]
  | Ast0.Postfix(exp,op) -> [22;lns;lne;offset]
  | Ast0.Infix(exp,op) -> [23;lns;lne;offset]
  | Ast0.Unary(exp,op) -> [24;lns;lne;offset]
  | Ast0.Binary(left,op,right) -> [25;lns;lne;offset]
  | Ast0.Paren(lp,exp,rp) -> [26;lns;lne;offset]
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) -> [27;lns;lne;offset]
  | Ast0.RecordAccess(exp,pt,field) -> [28;lns;lne;offset]
  | Ast0.RecordPtAccess(exp,ar,field) -> [29;lns;lne;offset]
  | Ast0.Cast(lp,ty,rp,exp) -> [30;lns;lne;offset]
  | Ast0.MetaConst(name,ty) -> [31;lns;lne;offset]
  | Ast0.MetaErr(name) -> [32;lns;lne;offset]
  | Ast0.MetaExpr(name,ty) -> [33;lns;lne;offset]
  | Ast0.MetaExprList(name) -> [34;lns;lne;offset]
  | Ast0.EComma(cm) -> [35;lns;lne;offset]
  | Ast0.DisjExpr(_,expr_list,_) -> [36;lns;lne;offset]
  | Ast0.NestExpr(_,expr_dots,_) -> [37;lns;lne;offset]
  | Ast0.Edots(dots,whencode) -> [38;lns;lne;offset]
  | Ast0.Ecircles(dots,whencode) -> [39;lns;lne;offset]
  | Ast0.Estars(dots,whencode) -> [40;lns;lne;offset]
  | Ast0.OptExp(exp) -> [41;lns;lne;offset]
  | Ast0.UniqueExp(exp) -> [42;lns;lne;offset]
  | Ast0.MultiExp(exp) -> [43;lns;lne;offset]

let typeC t =
  let (lns,lne,offset) = get_info t in
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) -> [44;lns;lne;offset]
  | Ast0.BaseType(ty,sign) -> [48;lns;lne;offset]
  | Ast0.Pointer(ty,star) -> [49;lns;lne;offset]
  | Ast0.Array(ty,lb,size,rb) -> [50;lns;lne;offset]
  | Ast0.StructUnionName(name,kind) -> [51;lns;lne;offset]
  | Ast0.TypeName(name) -> [52;lns;lne;offset]
  | Ast0.MetaType(name) -> [53;lns;lne;offset]
  | Ast0.OptType(ty) -> [45;lns;lne;offset]
  | Ast0.UniqueType(ty) -> [46;lns;lne;offset]
  | Ast0.MultiType(ty) -> [47;lns;lne;offset]
	
let declaration d =
  let (lns,lne,offset) = get_info d in
  match Ast0.unwrap d with
    Ast0.Init(ty,id,eq,exp,sem) -> [54;lns;lne;offset]
  | Ast0.UnInit(ty,id,sem) -> [55;lns;lne;offset]
  | Ast0.OptDecl(decl) -> [56;lns;lne;offset]
  | Ast0.UniqueDecl(decl) -> [57;lns;lne;offset]
  | Ast0.MultiDecl(decl) -> [58;lns;lne;offset]
	
let parameterTypeDef p =
  let (lns,lne,offset) = get_info p in
  match Ast0.unwrap p with
    Ast0.VoidParam(ty) -> [59;lns;lne;offset]
  | Ast0.Param(id,ty) -> [60;lns;lne;offset]
  | Ast0.MetaParam(name) -> [61;lns;lne;offset]
  | Ast0.MetaParamList(name) -> [62;lns;lne;offset]
  | Ast0.PComma(cm) -> [63;lns;lne;offset]
  | Ast0.Pdots(dots) -> [64;lns;lne;offset]
  | Ast0.Pcircles(dots) -> [65;lns;lne;offset]
  | Ast0.OptParam(param) -> [66;lns;lne;offset]
  | Ast0.UniqueParam(param) -> [67;lns;lne;offset]
	
let statement s =
  let (lns,lne,offset) = get_info s in
  match Ast0.unwrap s with
    Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      [68;lns;lne;offset]
  | Ast0.Decl(decl) -> [69;lns;lne;offset]
  | Ast0.Seq(lbrace,body,rbrace) -> [70;lns;lne;offset]
  | Ast0.ExprStatement(exp,sem) -> [71;lns;lne;offset]
  | Ast0.IfThen(iff,lp,exp,rp,branch1) -> [72;lns;lne;offset]
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) -> [73;lns;lne;offset]
  | Ast0.While(whl,lp,exp,rp,body) -> [74;lns;lne;offset]
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) -> [75;lns;lne;offset]
  | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body) -> [76;lns;lne;offset]
  | Ast0.Return(ret,sem) -> [77;lns;lne;offset]
  | Ast0.ReturnExpr(ret,exp,sem) -> [78;lns;lne;offset]
  | Ast0.MetaStmt(name) -> [79;lns;lne;offset]
  | Ast0.MetaStmtList(name) -> [80;lns;lne;offset]
  | Ast0.Disj(_,statement_dots_list,_) -> [81;lns;lne;offset]
  | Ast0.Nest(_,stmt_dots,_) -> [82;lns;lne;offset]
  | Ast0.Exp(exp) -> [83;lns;lne;offset]
  | Ast0.Dots(d,whencode) -> [84;lns;lne;offset]
  | Ast0.Circles(d,whencode) -> [85;lns;lne;offset]
  | Ast0.Stars(d,whencode) -> [86;lns;lne;offset]
  | Ast0.OptStm(re) -> [87;lns;lne;offset]
  | Ast0.UniqueStm(re) -> [88;lns;lne;offset]
  | Ast0.MultiStm(re) -> [89;lns;lne;offset]
	
let top_level t =
  let (lns,lne,offset) = get_info t in
  match Ast0.unwrap t with
    Ast0.DECL(decl) -> [90;lns;lne;offset]
  | Ast0.INCLUDE(inc,name) -> [91;lns;lne;offset]
  | Ast0.FILEINFO(old_file,new_file) -> [92;lns;lne;offset]
  | Ast0.FUNCTION(stmt_dots) -> [93;lns;lne;offset]
  | Ast0.CODE(stmt_dots) -> [94;lns;lne;offset]
  | Ast0.ERRORWORDS(exps) -> [95;lns;lne;offset]
  | Ast0.OTHER(_) -> [96;lns;lne;offset]
