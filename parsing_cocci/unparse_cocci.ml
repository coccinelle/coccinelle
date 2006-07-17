open Format
module Ast = Ast_cocci

let print_plus_flag = ref true
let print_minus_flag = ref true

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

let print_string_box s = print_string s; open_box 0


let print_option = Common.do_option
let print_between = Common.print_between

(* --------------------------------------------------------------------- *)
(* Modified code *)

(* avoid polyvariance problems *)
let anything : (Ast.anything -> unit) ref = ref (function _ -> ())

let rec print_anything str = function
    [] -> ()
  | stream ->
      start_block();
      print_between force_newline
	(function x ->
	  print_string str; open_box 0; print_anything_list x; close_box())
	stream;
      end_block()

and print_anything_list = function
    [] -> ()
  | [x] -> !anything x
  | bef::((aft::_) as rest) ->
      !anything bef;
      let space =
	(match bef with
	  Ast.Rule_elemTag(_) | Ast.AssignOpTag(_) | Ast.BinaryOpTag(_)
	| Ast.ArithOpTag(_) | Ast.LogicalOpTag(_)
	| Ast.Token("if") | Ast.Token("while") -> true | _ -> false) or
	(match aft with
	  Ast.Rule_elemTag(_) | Ast.AssignOpTag(_) | Ast.BinaryOpTag(_)
	| Ast.ArithOpTag(_) | Ast.LogicalOpTag(_) | Ast.Token("{") -> true
	| _ -> false) in
      if space then print_string " ";
      print_anything_list rest

let print_around printer term = function
    Ast.NOTHING -> printer term
  | Ast.BEFORE(bef) -> print_anything "<<< " bef; printer term
  | Ast.AFTER(aft) -> printer term; print_anything ">>> " aft
  | Ast.BEFOREAFTER(bef,aft) ->
      print_anything "<<< " bef; printer term; print_anything ">>> " aft

let mcode fn = function
    ((x, _, Ast.MINUS(plus_stream)) : 'a Ast.mcode) ->
      if !print_minus_flag
      then print_string "-"; 
      fn x; 
      if !print_plus_flag 
      then print_anything ">>> " plus_stream
  | (x, _, Ast.CONTEXT(plus_streams)) -> 
      if !print_plus_flag
      then print_around fn x plus_streams
      else fn x
  | (x, _, Ast.PLUS) -> fn x

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots between fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) -> print_between between fn l
  | Ast.CIRCLES(l) -> print_between between fn l
  | Ast.STARS(l) -> print_between between fn l

let nest_dots fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) ->
      print_string "<..."; start_block();
      print_between force_newline fn l;
      end_block(); print_string "...>"
  | Ast.CIRCLES(l) ->
      print_string "<ooo"; start_block();
      print_between force_newline fn l;
      end_block(); print_string "ooo>"
  | Ast.STARS(l) ->
      print_string "<***"; start_block();
      print_between force_newline fn l;
      end_block(); print_string "***>"

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
    Ast.Id(name) -> mcode print_string name
  | Ast.MetaId(name) -> mcode print_string name
  | Ast.MetaFunc(name) -> mcode print_string name
  | Ast.MetaLocalFunc(name) -> mcode print_string name
  | Ast.OptIdent(id) -> print_string "?"; ident id
  | Ast.UniqueIdent(id) -> print_string "!"; ident id
  | Ast.MultiIdent(id) -> print_string "\\+"; ident id

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  match Ast.unwrap e with
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> mcode constant const
  | Ast.FunCall(fn,lp,args,rp) ->
      expression fn; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp
  | Ast.Assignment(left,op,right) ->
      expression left; print_string " "; mcode assignOp op;
      print_string " "; expression right
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      expression exp1; print_string " "; mcode print_string why;
      print_option (function e -> print_string " "; expression e) exp2;
      print_string " "; mcode print_string colon; expression exp3
  | Ast.Postfix(exp,op) -> expression exp; mcode fixOp op
  | Ast.Infix(exp,op) -> mcode fixOp op; expression exp
  | Ast.Unary(exp,op) -> mcode unaryOp op; expression exp
  | Ast.Binary(left,op,right) ->
      expression left; print_string " "; mcode binaryOp op; print_string " ";
      expression right
  | Ast.Paren(lp,exp,rp) ->
      mcode print_string_box lp; expression exp; close_box();
      mcode print_string rp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      expression exp1; mcode print_string_box lb; expression exp2; close_box();
      mcode print_string rb
  | Ast.RecordAccess(exp,pt,field) ->
      expression exp; mcode print_string pt; ident field
  | Ast.RecordPtAccess(exp,ar,field) ->
      expression exp; mcode print_string ar; ident field
  | Ast.Cast(lp,ty,rp,exp) ->
      mcode print_string_box lp; fullType ty; close_box();
      mcode print_string rp; expression exp
  | Ast.MetaConst(name,None) -> mcode print_string name
  | Ast.MetaConst(name,Some ty) ->
      mcode print_string name; print_string "/* ";
      print_between (function _ -> print_string ", ") fullType ty;
      print_string "*/"
  | Ast.MetaErr(name) -> mcode print_string name
  | Ast.MetaExpr(name,None) -> mcode print_string name
  | Ast.MetaExpr(name,Some ty) ->
      mcode print_string name; print_string "/*";
      print_between (function _ -> print_string ", ") fullType ty;
      print_string "*/"
  | Ast.MetaExprList(name) -> mcode print_string name
  | Ast.EComma(cm) -> mcode print_string cm; print_space()
  | Ast.DisjExpr(exp_list) ->
      print_string "\n("; force_newline();
      print_between
	(function _ -> print_string "\n|"; force_newline())
	expression exp_list;
      print_string "\n)"
  | Ast.NestExpr(expr_dots) -> nest_dots expression expr_dots
  | Ast.Edots(dots,Some whencode)
  | Ast.Ecircles(dots,Some whencode)
  | Ast.Estars(dots,Some whencode) ->
      mcode print_string dots; print_string "   WHEN != "; expression whencode
  | Ast.Edots(dots,None)
  | Ast.Ecircles(dots,None)
  | Ast.Estars(dots,None) -> mcode print_string dots
  | Ast.OptExp(exp) -> print_string "?"; expression exp
  | Ast.UniqueExp(exp) -> print_string "!"; expression exp
  | Ast.MultiExp(exp) -> print_string "\\+"; expression exp

and  unaryOp = function
    Ast.GetRef -> print_string "&"
  | Ast.DeRef -> print_string "*"
  | Ast.UnPlus -> print_string "+"
  | Ast.UnMinus -> print_string "-"
  | Ast.Tilde -> print_string "~"
  | Ast.Not -> print_string "!"

and  assignOp = function
    Ast.SimpleAssign -> print_string "="
  | Ast.OpAssign(aop) -> arithOp aop; print_string "="

and  fixOp = function
    Ast.Dec -> print_string "--"
  | Ast.Inc -> print_string "++"

and  binaryOp = function
    Ast.Arith(aop) -> arithOp aop
  | Ast.Logical(lop) -> logicalOp lop

and  arithOp = function
    Ast.Plus -> print_string "+"
  | Ast.Minus -> print_string "-"
  | Ast.Mul -> print_string "*"
  | Ast.Div -> print_string "/"
  | Ast.Mod -> print_string "%"
  | Ast.DecLeft -> print_string "<<"
  | Ast.DecRight -> print_string ">>"
  | Ast.And -> print_string "&"
  | Ast.Or -> print_string "|"
  | Ast.Xor -> print_string "^"

and  logicalOp = function
    Ast.Inf -> print_string "<"
  | Ast.Sup -> print_string ">"
  | Ast.InfEq -> print_string "<="
  | Ast.SupEq -> print_string ">="
  | Ast.Eq -> print_string "=="
  | Ast.NotEq -> print_string "!="
  | Ast.AndLog -> print_string "&&"
  | Ast.OrLog -> print_string "||"

and constant = function
    Ast.String(s) -> print_string "\""; print_string s; print_string "\""
  | Ast.Char(s) -> print_string s
  | Ast.Int(s) -> print_string s
  | Ast.Float(s) -> print_string s

(* --------------------------------------------------------------------- *)
(* Types *)

and fullType ft =
  match Ast.unwrap ft with
    Ast.Type(cv,ty) ->
      print_option (function x -> mcode const_vol x; print_string " ") cv;
      typeC ty
  | Ast.OptType(ty) -> print_string "?"; fullType ty
  | Ast.UniqueType(ty) -> print_string "!"; fullType ty
  | Ast.MultiType(ty) -> print_string "\\+"; fullType ty

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,sgn) -> mcode baseType ty; print_option (mcode sign) sgn
  | Ast.Pointer(ty,star) -> fullType ty; mcode print_string star
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.StructUnionName(name,kind) ->
      mcode structUnion kind; mcode print_string name; print_string " "
  | Ast.TypeName(name)-> mcode print_string name; print_string " "
  | Ast.MetaType(name)-> mcode print_string name; print_string " "

and baseType = function
    Ast.VoidType -> print_string "void "
  | Ast.CharType -> print_string "char "
  | Ast.ShortType -> print_string "short "
  | Ast.IntType -> print_string "int "
  | Ast.DoubleType -> print_string "double "
  | Ast.FloatType -> print_string "float "
  | Ast.LongType -> print_string "long "

and structUnion = function
    Ast.Struct -> print_string "struct "
  | Ast.Union -> print_string "union "

and sign = function
    Ast.Signed -> print_string "signed "
  | Ast.Unsigned -> print_string "unsigned "

and const_vol = function
    Ast.Const -> print_string "const"
  | Ast.Volatile -> print_string "volatile"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration d =
  match Ast.unwrap d with
    Ast.Init(ty,id,eq,exp,sem) ->
      fullType ty; ident id; print_string " "; mcode print_string eq;
      print_string " "; expression exp; mcode print_string sem
  | Ast.UnInit(ty,id,sem) -> fullType ty; ident id; mcode print_string sem
  | Ast.DisjDecl(decls) ->
      print_string "\n("; force_newline();
      print_between
	(function _ -> print_string "\n|"; force_newline())
	declaration decls;
      print_string "\n)"
  | Ast.OptDecl(decl) -> print_string "?"; declaration decl
  | Ast.UniqueDecl(decl) -> print_string "!"; declaration decl
  | Ast.MultiDecl(decl) -> print_string "\\+"; declaration decl

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> fullType ty
  | Ast.Param(id,ty) -> fullType ty; ident id
  | Ast.MetaParam(name) -> mcode print_string name
  | Ast.MetaParamList(name) -> mcode print_string name
  | Ast.PComma(cm) -> mcode print_string cm; print_space()
  | Ast.Pdots(dots) -> mcode print_string dots
  | Ast.Pcircles(dots) -> mcode print_string dots
  | Ast.OptParam(param) -> print_string "?"; parameterTypeDef param
  | Ast.UniqueParam(param) -> print_string "!"; parameterTypeDef param

let parameter_list = dots (function _ -> ()) parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let storage Ast.Static = print_string "static "

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rule_elem arity re =
  match Ast.unwrap re with
    Ast.FunHeader(stg,name,lp,params,rp) ->
      print_string arity;
      print_option (mcode storage) stg;
      ident name; mcode print_string_box lp;
      parameter_list params; close_box(); mcode print_string rp;
      print_string " "
  | Ast.Decl(decl) -> print_string arity; declaration decl
  | Ast.SeqStart(brace) ->
      print_string arity; mcode print_string brace; start_block()
  | Ast.SeqEnd(brace) ->
      end_block(); print_string arity; mcode print_string brace
  | Ast.ExprStatement(exp,sem) ->
      print_string arity; expression exp; mcode print_string sem
  | Ast.IfHeader(iff,lp,exp,rp) ->
      print_string arity;
      mcode print_string iff; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "
  | Ast.Else(els) ->
      print_string arity; mcode print_string els; print_string " "
  | Ast.WhileHeader(whl,lp,exp,rp) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "
  | Ast.DoHeader(d) ->
      print_string arity; mcode print_string d; print_string " "
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp;
      mcode print_string sem
  | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
      print_string arity;
      mcode print_string fr; mcode print_string_box lp;
      print_option expression e1; mcode print_string sem1;
      print_option expression e2; mcode print_string sem2;
      print_option expression e3; close_box();
      mcode print_string rp; print_string " "
  | Ast.Return(ret,sem) ->
      print_string arity; mcode print_string ret; mcode print_string sem
  | Ast.ReturnExpr(ret,exp,sem) ->
      print_string arity; mcode print_string ret; print_string " ";
      expression exp; mcode print_string sem
  | Ast.MetaRuleElem(name) ->
      print_string arity; mcode print_string name
  | Ast.MetaStmt(name) ->
      print_string arity; mcode print_string name
  | Ast.MetaStmtList(name) ->
      print_string arity;  mcode print_string name
  | Ast.Exp(exp) -> print_string arity; expression exp

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,body,rbrace) ->
      rule_elem arity lbrace; dots force_newline (statement arity) body;
      rule_elem arity rbrace
  | Ast.IfThen(header,branch) ->
      rule_elem arity header; statement arity branch
  | Ast.IfThenElse(header,branch1,els,branch2) ->
      rule_elem arity header; statement arity branch1; print_string " ";
      rule_elem arity els; statement arity branch2
  | Ast.While(header,body) ->
      rule_elem arity header; statement arity body
  | Ast.Do(header,body,tail) ->
      rule_elem arity header; statement arity body;
      rule_elem arity tail
  | Ast.For(header,body) ->
      rule_elem arity header; statement arity body
  | Ast.Atomic(re) -> rule_elem arity re
  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) body; rule_elem arity rbrace
  | Ast.Disj([stmt_dots]) ->
      print_string arity;
      dots force_newline (statement arity) stmt_dots
  | Ast.Disj(stmt_dots_list) ->
      print_string arity;
      print_string "\n("; force_newline();
      print_between
	(function _ -> print_string "\n|"; force_newline())
	(dots force_newline (statement arity))
	stmt_dots_list;
      print_string "\n)"
  | Ast.Nest(stmt_dots) ->
      print_string arity;
      nest_dots (statement arity) stmt_dots
  | Ast.Dots(d,[],_) | Ast.Circles(d,[],_) | Ast.Stars(d,[],_) ->
      print_string arity; mcode print_string d
  | Ast.Dots(d,whencode,_) | Ast.Circles(d,whencode,_)
  | Ast.Stars(d,whencode,_) ->
      print_string arity; mcode print_string d;
      print_string "   WHEN != ";
      open_box 0;
      print_between (function _ -> print_string " &"; force_newline())
      (dots force_newline (statement "")) whencode;
      close_box()
  | Ast.OptStm(s) -> statement "?" s
  | Ast.UniqueStm(s) -> statement "!" s
  | Ast.MultiStm(s) -> statement "\\+" s

let top_level t =
  match Ast.unwrap t with
    Ast.DECL(decl) -> declaration decl
  | Ast.INCLUDE(inc,s) ->
      mcode print_string inc; print_string " "; mcode print_string s
  | Ast.FILEINFO(old_file,new_file) ->
      print_string "--- "; mcode print_string old_file; force_newline();
      print_string "+++ "; mcode print_string new_file
  | Ast.FUNCTION(stmt) -> statement "" stmt
  | Ast.CODE(stmt_dots) ->
      dots force_newline (statement "") stmt_dots
  | Ast.ERRORWORDS(exps) ->
      print_string "error words = [";
      print_between (function _ -> print_string ", ") expression exps;
      print_string "]"

let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level

let _ =
  anything := function
      Ast.FullTypeTag(x) -> fullType x
    | Ast.BaseTypeTag(x) -> baseType x
    | Ast.StructUnionTag(x) -> structUnion x
    | Ast.SignTag(x) -> sign x
    | Ast.IdentTag(x) -> ident x
    | Ast.ExpressionTag(x) -> expression x
    | Ast.ConstantTag(x) -> constant x
    | Ast.UnaryOpTag(x) -> unaryOp x
    | Ast.AssignOpTag(x) -> assignOp x
    | Ast.FixOpTag(x) -> fixOp x
    | Ast.BinaryOpTag(x) -> binaryOp x
    | Ast.ArithOpTag(x) -> arithOp x
    | Ast.LogicalOpTag(x) -> logicalOp x
    | Ast.DeclarationTag(x) -> declaration x
    | Ast.ParameterTypeDefTag(x) -> parameterTypeDef x
    | Ast.StorageTag(x) -> storage x
    | Ast.Rule_elemTag(x) -> rule_elem "" x
    | Ast.StatementTag(x) -> statement "" x
    | Ast.ConstVolTag(x) -> const_vol x
    | Ast.Token(x) -> print_string x
    | Ast.Code(x) -> let _ = top_level x in ()
    | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x
    | Ast.ParamDotsTag(x) -> parameter_list x
    | Ast.StmtDotsTag(x) -> dots (function _ -> ()) (statement "") x
    | Ast.TypeCTag(x) -> typeC x
    | Ast.ParamTag(x) -> parameterTypeDef x

let unparse x =
  print_string "\n@@\n@@";
  force_newline();
  force_newline();
  rule x;
  print_newline()

let rule_elem_to_string x =
  Common.format_to_string (function _ -> rule_elem "" x)

let unparse_to_string x =
  Common.format_to_string (function _ -> unparse x)
