open Format
module Ast = Ast_cocci

let print_plus_flag = ref true
let print_minus_flag = ref true
let print_newlines_disj = ref true

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
    (x, _, Ast.MINUS(plus_stream)) ->
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

let nest_dots fn f d =
  match Ast.unwrap d with
    Ast.DOTS(l) ->
      print_string "<..."; f(); start_block();
      print_between force_newline fn l;
      end_block(); print_string "...>"
  | Ast.CIRCLES(l) ->
      print_string "<ooo"; f(); start_block();
      print_between force_newline fn l;
      end_block(); print_string "ooo>"
  | Ast.STARS(l) ->
      print_string "<***"; f(); start_block();
      print_between force_newline fn l;
      end_block(); print_string "***>"

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
    Ast.Id(name) -> mcode print_string name
  | Ast.MetaId(name,_) -> mcode print_string name
  | Ast.MetaFunc(name,_) -> mcode print_string name
  | Ast.MetaLocalFunc(name,_) -> mcode print_string name
  | Ast.OptIdent(id) -> print_string "?"; ident id
  | Ast.UniqueIdent(id) -> print_string "!"; ident id
  | Ast.MultiIdent(id) -> print_string "\\+"; ident id

(* --------------------------------------------------------------------- *)
(* Expression *)

let print_disj_list fn l =
  if !print_newlines_disj
  then (print_string "\n("; force_newline())
  else print_string "(";
  print_between
    (function _ ->
      if !print_newlines_disj
      then (print_string "\n|"; force_newline())
      else print_string " | ")
    fn l;
  if !print_newlines_disj
  then (print_string "\n)"; force_newline())
  else print_string ")"

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
  | Ast.SizeOfExpr(sizeof,exp) ->
      mcode print_string sizeof; expression exp
  | Ast.SizeOfType(sizeof,lp,ty,rp) ->
      mcode print_string sizeof;
      mcode print_string_box lp; fullType ty; close_box();
      mcode print_string rp; 

  | Ast.MetaConst(name,None,_) -> mcode print_string name
  | Ast.MetaConst(name,Some ty,_) ->
      mcode print_string name; print_string "/* ";
      print_between (function _ -> print_string ", ") Type_cocci.typeC ty;
      print_string "*/"
  | Ast.MetaErr(name,_) -> mcode print_string name
  | Ast.MetaExpr(name,None,_) -> mcode print_string name
  | Ast.MetaExpr(name,Some ty,_) ->
      mcode print_string name; print_string "/*";
      print_between (function _ -> print_string ", ") Type_cocci.typeC ty;
      print_string "*/"
  | Ast.MetaExprList(name,_) -> mcode print_string name
  | Ast.EComma(cm) -> mcode print_string cm; print_space()
  | Ast.DisjExpr(exp_list) -> print_disj_list expression exp_list
  | Ast.NestExpr(expr_dots,Some whencode) ->
      nest_dots expression
	(function _ -> print_string "   when != "; expression whencode)
	expr_dots
  | Ast.NestExpr(expr_dots,None) ->
      nest_dots expression (function _ -> ()) expr_dots
  | Ast.Edots(dots,Some whencode)
  | Ast.Ecircles(dots,Some whencode)
  | Ast.Estars(dots,Some whencode) ->
      mcode print_string dots; print_string "   when != "; expression whencode
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
    Ast.BaseType(ty,sgn) -> print_option (mcode sign) sgn; mcode baseType ty
  | Ast.Pointer(ty,star) -> fullType ty; mcode print_string star
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.StructUnionName(name,kind) ->
      mcode structUnion kind; mcode print_string name; print_string " "
  | Ast.TypeName(name) -> mcode print_string name; print_string " "
  | Ast.MetaType(name,_) -> mcode print_string name; print_string " "

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
    Ast.Init(ty,id,eq,ini,sem) ->
      fullType ty; ident id; print_string " "; mcode print_string eq;
      print_string " "; initialiser ini; mcode print_string sem
  | Ast.UnInit(ty,id,sem) -> fullType ty; ident id; mcode print_string sem
  | Ast.DisjDecl(decls) -> print_disj_list declaration decls
  | Ast.MetaDecl(name,_) -> mcode print_string name
  | Ast.OptDecl(decl) -> print_string "?"; declaration decl
  | Ast.UniqueDecl(decl) -> print_string "!"; declaration decl
  | Ast.MultiDecl(decl) -> print_string "\\+"; declaration decl

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser i =
  match Ast.unwrap i with
    Ast.InitExpr(exp) -> expression exp
  | Ast.InitList(lb,initlist,rb) ->
      mcode print_string lb;
      let _ = dots (function _ -> ()) initialiser initlist in
      mcode print_string rb
  | Ast.InitGccDotName(dot,name,eq,ini) ->
      mcode print_string dot; ident name; print_string " ";
      mcode print_string eq; print_string " "; initialiser ini
  | Ast.InitGccName(name,eq,ini) ->
      ident name; mcode print_string eq; initialiser ini
  | Ast.InitGccIndex(lb,exp,rb,eq,ini) ->
      mcode print_string lb; expression exp; mcode print_string rb;
      print_string " "; mcode print_string eq; print_string " ";
      initialiser ini
  | Ast.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
      mcode print_string lb; expression exp1; mcode print_string dots;
      expression exp2; mcode print_string rb;
      print_string " "; mcode print_string eq; print_string " ";
      initialiser ini
  | Ast.IComma(cm) -> mcode print_string cm
  | Ast.Idots(d,Some whencode) ->
      mcode print_string d; print_string "   WHEN != ";
      initialiser whencode
  | Ast.Idots(d,None) -> mcode print_string d
  | Ast.OptIni(ini) -> print_string "?"; initialiser ini
  | Ast.UniqueIni(ini) -> print_string "!"; initialiser ini
  | Ast.MultiIni(ini) -> print_string "\\+"; initialiser ini

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> fullType ty
  | Ast.Param(id,ty) -> fullType ty; ident id
  | Ast.MetaParam(name,_) -> mcode print_string name
  | Ast.MetaParamList(name,_) -> mcode print_string name
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
    Ast.FunHeader(allminus,stg,ty,name,lp,params,rp) ->
      print_string arity;
      print_option (mcode storage) stg;
      print_option fullType ty;
      ident name; mcode print_string_box lp;
      parameter_list params; close_box(); mcode print_string rp;
      print_string " "
  | Ast.Decl(decl) -> print_string arity; declaration decl
  | Ast.SeqStart(brace) ->
      print_string arity; mcode print_string brace;
      if !print_newlines_disj then start_block()
  | Ast.SeqEnd(brace) ->
      if !print_newlines_disj then end_block();
      print_string arity; mcode print_string brace
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
  | Ast.Break(br,sem) ->
      print_string arity; mcode print_string br; mcode print_string sem
  | Ast.Continue(cont,sem) ->
      print_string arity; mcode print_string cont; mcode print_string sem
  | Ast.Return(ret,sem) ->
      print_string arity; mcode print_string ret; mcode print_string sem
  | Ast.ReturnExpr(ret,exp,sem) ->
      print_string arity; mcode print_string ret; print_string " ";
      expression exp; mcode print_string sem
  | Ast.MetaRuleElem(name,_) ->
      print_string arity; mcode print_string name
  | Ast.MetaStmt(name,_,_) ->
      print_string arity; mcode print_string name
  | Ast.MetaStmtList(name,_) ->
      print_string arity;  mcode print_string name
  | Ast.Exp(exp) -> print_string arity; expression exp

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,decls,_,body,rbrace) ->
      rule_elem arity lbrace;
      dots force_newline (statement arity) decls;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace
  | Ast.IfThen(header,branch,aft) ->
      rule_elem arity header; statement arity branch;
      mcode (function _ -> ()) ((),(),aft)
  | Ast.IfThenElse(header,branch1,els,branch2,aft) ->
      rule_elem arity header; statement arity branch1; print_string " ";
      rule_elem arity els; statement arity branch2;
      mcode (function _ -> ()) ((),(),aft)
  | Ast.While(header,body,aft) ->
      rule_elem arity header; statement arity body;
      mcode (function _ -> ()) ((),(),aft)
  | Ast.Do(header,body,tail) ->
      rule_elem arity header; statement arity body;
      rule_elem arity tail
  | Ast.For(header,body,aft) ->
      rule_elem arity header; statement arity body;
      mcode (function _ -> ()) ((),(),aft)
  | Ast.Atomic(re) -> rule_elem arity re
  | Ast.FunDecl(header,lbrace,decls,_,body,rbrace) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) decls;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace
  | Ast.Disj([stmt_dots]) ->
      print_string arity;
      dots (function _ -> if !print_newlines_disj then force_newline())
	(statement arity) stmt_dots
  | Ast.Disj(stmt_dots_list) -> (* ignores newline directive for readability *)
      print_string arity;
      print_string "\n("; force_newline();
      print_between
	(function _ -> print_string "\n|"; force_newline())
	(dots force_newline (statement arity))
	stmt_dots_list;
      print_string "\n)"
  | Ast.Nest(stmt_dots,whn,_) ->
      print_string arity;
      nest_dots (statement arity)
	(function _ ->
	  whencode (dots force_newline (statement "")) (statement "") whn)
	stmt_dots
  | Ast.Dots(d,whn,_) | Ast.Circles(d,whn,_) | Ast.Stars(d,whn,_) ->
      print_string arity; mcode print_string d;
      whencode (dots force_newline (statement "")) (statement "") whn
  | Ast.OptStm(s) -> statement "?" s
  | Ast.UniqueStm(s) -> statement "!" s
  | Ast.MultiStm(s) -> statement "\\+" s

and print_statement_when whencode =
  print_string "   WHEN != ";
  open_box 0;
  print_between (function _ -> print_string " &"; force_newline())
    (dots force_newline (statement "")) whencode;
  close_box()


and whencode notfn alwaysfn = function
    Ast.NoWhen -> ()
  | Ast.WhenNot a ->
      print_string "   WHEN != "; open_box 0; notfn a; close_box()
  | Ast.WhenAlways a ->
      print_string "   WHEN = "; open_box 0; alwaysfn a; close_box()


(* for export only *)
let statement_dots l = dots force_newline (statement "") l

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
    | Ast.InitTag(x) -> initialiser x
    | Ast.DeclarationTag(x) -> declaration x
    | Ast.ParameterTypeDefTag(x) -> parameterTypeDef x
    | Ast.StorageTag(x) -> storage x
    | Ast.Rule_elemTag(x) -> rule_elem "" x
    | Ast.StatementTag(x) -> statement "" x
    | Ast.ConstVolTag(x) -> const_vol x
    | Ast.Token(x) -> print_string x
    | Ast.Code(x) -> let _ = top_level x in ()
    | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x
    | Ast.InitDotsTag(x) -> dots (function _ -> ()) initialiser x
    | Ast.ParamDotsTag(x) -> parameter_list x
    | Ast.StmtDotsTag(x) -> dots (function _ -> ()) (statement "") x
    | Ast.TypeCTag(x) -> typeC x
    | Ast.ParamTag(x) -> parameterTypeDef x

let unparse x =
  print_string "\n@@\n@@";
  print_newlines_disj := true;
  force_newline();
  force_newline();
  rule x;
  print_newline()

let rule_elem_to_string x =
  print_newlines_disj := true;
  Common.format_to_string (function _ -> rule_elem "" x)

let unparse_to_string x =
  print_newlines_disj := true;
  Common.format_to_string (function _ -> unparse x)

let print_rule_elem re =
  let nl = !print_newlines_disj in
  print_newlines_disj := false;
  rule_elem "" re;
  print_newlines_disj := nl

