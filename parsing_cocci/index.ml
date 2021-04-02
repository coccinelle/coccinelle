(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* create an index for each constructor *)
(* current max is 192, also unused: 8-9, 15, 42, 46, 57, 65, 85-86,
 113-115 *)

(* doesn't really work - requires that identical terms with no token
subterms (eg dots) not appear on the same line *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci

(* if a dot list is empty, add the starting line of the dot list to the
address.  Otherwise add 0.  An empty dot list should only match with another
empty one. *)
let dots d =
  match Ast0.unwrap d with
    [] -> [(Ast0.get_info d).Ast0.pos_info.Ast0.line_start]
  | _ -> [0]

let expression_dots x  = 1 :: dots x
let initialiser_dots x = 2 :: dots x
let parameter_dots x =   3 :: dots x
let statement_dots x =   4 :: dots x
let declaration_dots x = 5 :: dots x
let field_dots x = 8 :: dots x
let enum_decl_dots x = 9 :: dots x
let case_line_dots x =   6 :: dots x
let define_param_dots x =7 :: dots x

let ident i =
  match Ast0.unwrap i with
      Ast0.Id(name) -> [10]
    | Ast0.MetaId(name,_,_,_) -> [11]
    | Ast0.MetaFunc(name,_,_) -> [12]
    | Ast0.MetaLocalFunc(name,_,_) -> [13]
    | Ast0.DisjId(_,id_list,_,_) -> [152]
    | Ast0.ConjId(_,id_list,_,_) -> [67]
    | Ast0.OptIdent(id) -> [14]
    | Ast0.AsIdent _ -> failwith "not possible"

let expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) -> [17]
  | Ast0.Constant(const) -> [18]
  | Ast0.StringConstant(lq,str,rq,sz) -> [165]
  | Ast0.FunCall(fn,lp,args,rp) -> [19]
  | Ast0.Assignment(left,op,right,simple) -> [20]
  | Ast0.Sequence(left,op,right) -> [156]
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) -> [21]
  | Ast0.Postfix(exp,op) -> [22]
  | Ast0.Infix(exp,op) -> [23]
  | Ast0.Unary(exp,op) -> [24]
  | Ast0.Binary(left,op,right) -> [25]
  | Ast0.Nested(left,op,right) -> failwith "nested in index not possible"
  | Ast0.Paren(lp,exp,rp) -> [26]
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) -> [27]
  | Ast0.RecordAccess(exp,pt,field) -> [28]
  | Ast0.RecordPtAccess(exp,ar,field) -> [29]
  | Ast0.Cast(lp,ty,attr,rp,exp) -> [30]
  | Ast0.SizeOfExpr(szf,exp) -> [98] (* added after *)
  | Ast0.SizeOfType(szf,lp,ty,rp) -> [99] (* added after *)
  | Ast0.TypeExp(ty) -> [123] (* added after *)
  | Ast0.Constructor(lp,ty,rp,init) -> [155]
  | Ast0.MetaErr(name,_,_) -> [32]
  | Ast0.MetaExpr(name,_,ty,_,_,_bitfield) -> [33]
  | Ast0.MetaExprList(name,_,_,_) -> [34]
  | Ast0.EComma(cm) -> [35]
  | Ast0.DisjExpr(_,expr_list,_,_) -> [36]
  | Ast0.ConjExpr(_,expr_list,_,_) -> [187]
  | Ast0.NestExpr(_,expr_dots,_,_,_) -> [37]
  | Ast0.Edots(dots,whencode) -> [38]
  | Ast0.OptExp(exp) -> [41]
  | Ast0.AsExpr _  | Ast0.AsSExpr _ -> failwith "not possible"

let assignOp op = match Ast0.unwrap op with
  | Ast0.SimpleAssign _ -> [180]
  | Ast0.OpAssign _ -> [181]
  | Ast0.MetaAssign(_,_,_) -> [182]

let binaryOp op = match Ast0.unwrap op with
  | Ast0.Arith _ -> [183]
  | Ast0.Logical _ -> [184]
  | Ast0.MetaBinary(_,_,_) -> [185]

let typeC t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) -> [44]
  | Ast0.BaseType(ty,strings) -> [48]
  | Ast0.Signed(sign,ty) -> [129]
  | Ast0.Pointer(ty,star) -> [49]
  | Ast0.ParenType(lp,ty,rp) -> [138]
  | Ast0.FunctionType(ty,lp,params,rp) -> [139]
  | Ast0.Array(ty,lb,size,rb) -> [50]
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) -> [160]
  | Ast0.EnumName(kind,name) -> [146]
  | Ast0.EnumDef(ty,lb,decls,rb) -> [150]
  | Ast0.StructUnionName(kind,name) -> [51]
  | Ast0.StructUnionDef(ty,lb,decls,rb) -> [117]
  | Ast0.TypeOfExpr(tf,lp,exp,rp) -> [135]
  | Ast0.TypeOfType(tf,lp,ty,rp) -> [136]
  | Ast0.TypeName(name) -> [52]
  | Ast0.AutoType _ -> [192]
  | Ast0.MetaType(name,_,_) -> [53]
  | Ast0.DisjType(_,type_list,_,_) -> [130]
  | Ast0.ConjType(_,type_list,_,_) -> [134]
  | Ast0.OptType(ty) -> [45]
  | Ast0.AsType _ -> failwith "not possible"

let declaration d =
  match Ast0.unwrap d with
    Ast0.MetaDecl(name,_,_) -> [148]
  | Ast0.Init(stg,ty,id,attr,eq,exp,sem) -> [54]
  | Ast0.UnInit(stg,ty,id,attr,sem) -> [55]
  | Ast0.FunProto(fi,name,lp1,params,va,rp1,sem) -> [132]
  | Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem) -> [137]
  | Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) -> [157]
  | Ast0.TyDecl(ty,attr,sem) -> [116]
  | Ast0.Typedef(stg,ty,id,sem) -> [143]
  | Ast0.DisjDecl(_,decls,_,_) -> [97] (* added after *)
  | Ast0.ConjDecl(_,decls,_,_) -> [88] (* added after *)
  | Ast0.OptDecl(decl) -> [56]
  | Ast0.AsDecl _ -> failwith "not possible"

let field d =
  match Ast0.unwrap d with
  | Ast0.MetaField(name,_,_) -> [149]
  | Ast0.MetaFieldList(name,_,_,_) -> [152]
  | Ast0.Field(ty,id,_bf,sem) -> [55]
  | Ast0.DisjField(_,decls,_,_) -> [189] (* added after *)
  | Ast0.ConjField(_,decls,_,_) -> [190] (* added after *)
  | Ast0.Fdots(dots,whencode) -> [133]
  | Ast0.OptField(decl) -> [191]

let enum_decl d =
  match Ast0.unwrap d with
  | Ast0.Enum(name,enum_val) -> [113]
  | Ast0.EnumComma(cm) -> [114]
  | Ast0.EnumDots(dots,whencode) -> [115]

let initialiser i =
  match Ast0.unwrap i with
    Ast0.MetaInit(nm,_,_) -> [106] (* added after *)
  | Ast0.MetaInitList(nm,_,_,_) -> [153] (* added after *)
  | Ast0.InitExpr(exp) -> [102]
  | Ast0.InitList(lb,initlist,rb,ordered) -> [103]
  | Ast0.InitGccExt(designators,eq,ini) -> [104]
  | Ast0.InitGccName(name,eq,ini) -> [105]
  | Ast0.IComma(cm) -> [108]
  | Ast0.Idots(d,whencode) -> [109]
  | Ast0.OptIni(id) -> [110]
  | Ast0.AsInit _ -> failwith "not possible"

let parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty,attr) -> [59]
  | Ast0.Param(ty,id,attr) -> [60]
  | Ast0.MetaParam(name,_,_) -> [61]
  | Ast0.MetaParamList(name,_,_,_) -> [62]
  | Ast0.PComma(cm) -> [63]
  | Ast0.Pdots(dots) -> [64]
  | Ast0.OptParam(param) -> [66]
  | Ast0.AsParam _ -> failwith "not possible"

let statement s =
  match Ast0.unwrap s with
    Ast0.FunDecl(bef,fninfo,name,lp,params,va,rp,lbrace,body,rbrace,aft) -> [68]
  | Ast0.Decl(bef,decl) -> [69]
  | Ast0.Seq(lbrace,body,rbrace) -> [70]
  | Ast0.ExprStatement(exp,sem) -> [71]
  | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) -> [72]
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) -> [73]
  | Ast0.While(whl,lp,exp,rp,body,_) -> [74]
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) -> [75]
  | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,_) -> [76]
  | Ast0.Iterator(nm,lp,args,rp,body,_) -> [142]
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) -> [125]
  | Ast0.Break(br,sem) -> [100]
  | Ast0.Continue(cont,sem) -> [101]
  | Ast0.Label(l,dd) -> [144]
  | Ast0.Goto(goto,l,sem) -> [145]
  | Ast0.Return(ret,sem) -> [77]
  | Ast0.ReturnExpr(ret,exp,sem) -> [78]
  | Ast0.Exec(exec,lang,code,sem) -> [170]
  | Ast0.MetaStmt(name,_,_) -> [79]
  | Ast0.MetaStmtList(name,_,_,_) -> [80]
  | Ast0.Disj(_,statement_dots_list,_,_) -> [81]
  | Ast0.Conj(_,statement_dots_list,_,_) -> [188]
  | Ast0.Nest(_,stmt_dots,_,_,_) -> [82]
  | Ast0.Exp(exp) -> [83]
  | Ast0.TopExp(exp) -> [141]
  | Ast0.Ty(ty) -> [124]
  | Ast0.TopId(ty) -> [186]
  | Ast0.TopInit(init) -> [146]
  | Ast0.Dots(d,whencode) -> [84]
  | Ast0.Include(inc,name) -> [118]
  | Ast0.MetaInclude(inc,name) -> [111]
  | Ast0.Undef(def,id) -> [151]
  | Ast0.Define(def,id,params,body) -> [119]
  | Ast0.Pragma(prg,id,body) -> [161]
  | Ast0.OptStm(re) -> [87]
  | Ast0.AsStmt _ -> failwith "not possible"

let forinfo fi =
  match Ast0.unwrap fi with
    Ast0.ForExp(exp,sem) -> [158]
  | Ast0.ForDecl (bef,decl) -> [159]

and pragmainfo pi =
  match Ast0.unwrap pi with
    Ast0.PragmaString(s) -> [163]
  | Ast0.PragmaDots (dots) -> [164]

let case_line c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) -> [126]
  | Ast0.Case(case,exp,colon,code) -> [127]
  | Ast0.DisjCase(_,case_lines,_,_) -> [107]
  | Ast0.OptCase(case) -> [128]

let string_fragment f =
  match Ast0.unwrap f with
    Ast0.ConstantFragment(str) -> [166]
  | Ast0.FormatFragment(pct,fmt) -> [167]
  | Ast0.Strdots(dots) -> [168]
  | Ast0.MetaFormatList(pct,name,cstr,lenname) -> [169]

let attribute a =
  match Ast0.unwrap a with
    Ast0.Attribute(attr) -> [39]
  | Ast0.GccAttribute(attr_,lp1,lp2,arg,rp1,rp2) -> [162]

let attr_arg a =
  match Ast0.unwrap a with
    Ast0.MetaAttr(name,_,_) -> [40]
  | Ast0.AttrName(attr) -> [140]

let top_level t =
  match Ast0.unwrap t with
    Ast0.NONDECL(stmt) -> [90]
  | Ast0.FILEINFO(old_file,new_file) -> [92]
  | Ast0.CODE(stmt_dots) -> [94]
  | Ast0.ERRORWORDS(exps) -> [95]
  | Ast0.OTHER(_) -> [96]
  | Ast0.TOPCODE(_) -> [154]

(* 99-101 already used *)
