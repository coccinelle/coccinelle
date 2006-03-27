(* the various booleans that are passed around are to control where
newlines are needed.  For example, a newline is not needed before a },
because it changes the indentation before doing the newline *)

open Format
module Ast = Ast_cocci

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

let print_option fn = function
    None -> ()
  | Some x -> fn x

let rec print_between between nnl fn = function
    [] -> ()
  | [x] -> let _ = fn x in ()
  | x::xp::xs ->
      let res = fn x in (if res && nnl xp then between());
      print_between between nnl fn (xp::xs)

let true_fn _ = true

(* --------------------------------------------------------------------- *)
(* Modified code *)

(* avoid polyvariance problems *)
let anything : (Ast.anything -> unit) ref = ref (function _ -> ())

let rec print_anything str plus_stream =
  match !plus_stream with
    [] -> ()
  | stream ->
      start_block();
      print_between force_newline true_fn
	(function x ->
	  print_string str;
	  print_between (function _ -> print_string " ") true_fn
	    (function x -> !anything x ; true)
	    x;
	  true)
	stream;
      end_block()

let print_around printer term plus_streams =
  match !plus_streams with
    Ast.NOTHING -> printer term
  | Ast.BEFORE(bef) -> print_anything "<<< " (ref bef); printer term
  | Ast.AFTER(aft) -> printer term; print_anything ">>> " (ref aft)
  | Ast.BEFOREAFTER(bef,aft) ->
      print_anything "<<< " (ref bef); printer term;
      print_anything ">>> " (ref aft)

let mcode fn = function
    Ast.MINUS(x,info,plus_stream) ->
      print_string "-"; fn x; print_anything ">>> " plus_stream
  | Ast.CONTEXT(x,info,plus_streams) ->
      print_around fn x plus_streams
  | Ast.PLUS(x,info) -> fn x

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots between fn nnl = function
    Ast.DOTS(l) -> print_between between nnl fn l
  | Ast.CIRCLES(l) -> print_between between nnl fn l
  | Ast.STARS(l) -> print_between between nnl fn l

let nest_dots fn nnl = function
    Ast.DOTS(l) ->
      print_string "<..."; start_block();
      print_between force_newline nnl fn l;
      end_block(); print_string "...>"
  | Ast.CIRCLES(l) ->
      print_string "<ooo"; start_block();
      print_between force_newline nnl fn l;
      end_block(); print_string "ooo>"
  | Ast.STARS(l) ->
      print_string "<***"; start_block();
      print_between force_newline nnl fn l;
      end_block(); print_string "***>"

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident = function
    Ast.Id(name) -> mcode print_string name
  | Ast.MetaId(name) -> mcode print_string name
  | Ast.MetaFunc(name) -> mcode print_string name
  | Ast.MetaLocalFunc(name) -> mcode print_string name
  | Ast.OptIdent(id) -> print_string "?"; ident id
  | Ast.UniqueIdent(id) -> print_string "!"; ident id
  | Ast.MultiIdent(id) -> print_string "\\+"; ident id

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression = function
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> mcode constant const
  | Ast.FunCall(fn,lp,args,rp) ->
      expression fn; mcode print_string lp; open_box 0;
      let _ =
	dots (function _ -> ())
	  (function x -> expression x; true) true_fn args in
      close_box(); mcode print_string rp
  | Ast.Assignment(left,op,right) ->
      expression left; print_string " "; mcode assignOp op;
      print_string " "; expression right
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      expression exp1; print_string " "; mcode print_string why;
      (match exp2 with None -> () | Some e -> print_string " "; expression e);
      print_string " "; mcode print_string colon; expression exp3
  | Ast.Postfix(exp,op) -> expression exp; mcode fixOp op
  | Ast.Infix(exp,op) -> mcode fixOp op; expression exp
  | Ast.Unary(exp,op) -> mcode unaryOp op; expression exp
  | Ast.Binary(left,op,right) ->
      expression left; print_string " "; mcode binaryOp op; print_string " ";
      expression right
  | Ast.Paren(lp,exp,rp) ->
      mcode print_string lp; expression exp; mcode print_string rp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      expression exp1; mcode print_string lb; expression exp2;
      mcode print_string rb
  | Ast.RecordAccess(exp,pt,field) ->
      expression exp; mcode print_string pt; ident field
  | Ast.RecordPtAccess(exp,ar,field) ->
      expression exp; mcode print_string ar; ident field
  | Ast.Cast(lp,ty,rp,exp) ->
      mcode print_string lp; typeC ty; mcode print_string rp;
      expression exp
  | Ast.MetaConst(name,None) -> mcode print_string name
  | Ast.MetaConst(name,Some ty) ->
      mcode print_string name; print_string "/* ";
      print_between (function _ -> print_string ", ") true_fn
	(function x -> typeC x; true) ty;
      print_string "*/"
  | Ast.MetaExpr(name,None) -> mcode print_string name
  | Ast.MetaExpr(name,Some ty) ->
      mcode print_string name; print_string "/*";
      print_between (function _ -> print_string ", ") true_fn
	(function x -> typeC x; true) ty;
      print_string "*/"
  | Ast.MetaExprList(name) -> mcode print_string name
  | Ast.EComma(cm) -> mcode print_string cm; print_space()
  | Ast.DisjExpr(exp_list) ->
      print_string "\n("; force_newline();
      print_between
	(function _ -> print_string "\n|"; force_newline())
	true_fn
	(function x -> expression x; true) exp_list;
      print_string "\n)"
  | Ast.NestExpr(expr_dots) ->
      nest_dots (function x -> expression x; true) true_fn expr_dots
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
  | Ast. UnMinus -> print_string "-"
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

and typeC = function
    Ast.BaseType(ty,Some sgn) -> mcode baseType ty; mcode sign sgn
  | Ast.BaseType(ty,None) -> mcode baseType ty
  | Ast.Pointer(ty,star) -> typeC ty; mcode print_string star
  | Ast.Array(ty,lb,size,rb) ->
      typeC ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.StructUnionName(name,kind) ->
      mcode structUnion kind; mcode print_string name; print_string " "
  | Ast.TypeName(name)-> mcode print_string name; print_string " "
  | Ast.MetaType(name)-> mcode print_string name; print_string " "
  | Ast.OptType(ty) -> print_string "?"; typeC ty
  | Ast.UniqueType(ty) -> print_string "!"; typeC ty
  | Ast.MultiType(ty) -> print_string "\\+"; typeC ty

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

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration = function
    Ast.Init(ty,id,eq,exp,sem) ->
      typeC ty; ident id; mcode print_string eq; expression exp;
      mcode print_string sem
  | Ast.UnInit(ty,id,sem) -> typeC ty; ident id; mcode print_string sem
  | Ast.OptDecl(decl) -> print_string "?"; declaration decl
  | Ast.UniqueDecl(decl) -> print_string "!"; declaration decl
  | Ast.MultiDecl(decl) -> print_string "\\+"; declaration decl

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef = function
    Ast.VoidParam(ty) -> typeC ty
  | Ast.Param(id,None,ty) -> typeC ty; ident id
  | Ast.Param(id,Some vs,ty) ->
      mcode value_qualif vs; print_string " "; typeC ty; ident id
  | Ast.MetaParam(name) -> mcode print_string name
  | Ast.MetaParamList(name) -> mcode print_string name
  | Ast.PComma(cm) -> mcode print_string cm; print_space()
  | Ast.Pdots(dots) -> mcode print_string dots
  | Ast.Pcircles(dots) -> mcode print_string dots
  | Ast.OptParam(param) -> print_string "?"; parameterTypeDef param
  | Ast.UniqueParam(param) -> print_string "!"; parameterTypeDef param

and value_qualif = function
    Ast.Const -> print_string "const"
  | Ast.Volatile -> print_string "volatile"

let parameter_list =
  dots (function _ -> ()) (function x -> parameterTypeDef x; false) true_fn

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let storage Ast.Static = print_string "static "

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let not_brace = function
    Ast.SeqEnd(brace) -> false
  | Ast.Disj(x::xs) -> false
  | _ -> true

let rec rule_elem arity = function
    Ast.FunDecl(stg,name,lp,params,rp) ->
      print_string arity;
      print_option (mcode storage) stg;
      ident name; mcode print_string lp;
      open_box 0; parameter_list params; close_box(); mcode print_string rp;
      print_string " "; false
  | Ast.Decl(decl) -> print_string arity; declaration decl; true
  | Ast.SeqStart(brace) ->
      print_string arity; mcode print_string brace; start_block(); false
  | Ast.SeqEnd(brace) ->
      end_block(); print_string arity; mcode print_string brace; true
  | Ast.ExprStatement(exp,sem) ->
      print_string arity; expression exp; mcode print_string sem; true
  | Ast.IfHeader(iff,lp,exp,rp) ->
      print_string arity;
      mcode print_string iff; print_string " "; mcode print_string lp;
      expression exp; mcode print_string rp; print_string " "; false
  | Ast.Else(els) ->
      print_string arity; mcode print_string els; print_string " "; false
  | Ast.WhileHeader(whl,lp,exp,rp) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string lp;
      expression exp; mcode print_string rp; print_string " "; false
  | Ast.Do(d) ->
      print_string arity; mcode print_string d; print_string " "; false
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string lp;
      expression exp; mcode print_string rp; mcode print_string sem; true
  | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
      print_string arity;
      mcode print_string fr; mcode print_string lp;
      print_option expression e1; mcode print_string sem1;
      print_option expression e2; mcode print_string sem2;
      print_option expression e3; mcode print_string rp; print_string " ";
      false
  | Ast.Return(ret,sem) ->
      print_string arity; mcode print_string ret; mcode print_string sem; true
  | Ast.ReturnExpr(ret,exp,sem) ->
      print_string arity;
      mcode print_string ret; expression exp; mcode print_string sem; true
  | Ast.MetaStmt(name) ->
      print_string arity; mcode print_string name; true
  | Ast.MetaStmtList(name) ->
      print_string arity;  mcode print_string name; true
  | Ast.Disj([rule_elem_dots]) ->
      print_string arity;
      dots force_newline (rule_elem arity) not_brace rule_elem_dots; true
  | Ast.Disj(rule_elem_dots_list) ->
      print_string arity;
      print_string "\n("; force_newline();
      print_between
	(function _ -> print_string "\n|"; force_newline())
	true_fn
	(function x -> dots force_newline (rule_elem arity) not_brace x; true)
	rule_elem_dots_list;
      print_string "\n)"; true
  | Ast.Nest(rule_elem_dots) ->
      print_string arity;
      nest_dots (rule_elem arity) not_brace rule_elem_dots; true
  | Ast.Exp(exp) -> print_string arity; expression exp; true
  | Ast.Dots(dots,None) | Ast.Circles(dots,None) | Ast.Stars(dots,None) ->
      print_string arity; mcode print_string dots; true
  | Ast.Dots(d,Some x) | Ast.Circles(d,Some x) | Ast.Stars(d,Some x)->
      print_string arity; mcode print_string d;
      print_string "   WHEN != ";
      open_box 0; dots force_newline (rule_elem "") not_brace x;
      close_box(); true
  | Ast.OptRuleElem(re) ->
      print_between force_newline not_brace
	(function x -> rule_elem "?" x) re;
      true
  | Ast.UniqueRuleElem(re) ->
      print_between force_newline not_brace
	(function x -> rule_elem "!" x) re;
      true
  | Ast.MultiRuleElem(re) ->
      print_between force_newline not_brace
	(function x -> rule_elem "\\+" x) re;
      true

let top_level = function
    Ast.DECL(decl) -> declaration decl; true
  | Ast.INCLUDE(inc,s) ->
      mcode print_string inc; print_string " "; mcode print_string s; true
  | Ast.FILEINFO(old_file,new_file) ->
      print_string "--- "; mcode print_string old_file; force_newline();
      print_string "+++ "; mcode print_string new_file; true
  | Ast.FUNCTION(rule_elem_dots) ->
      dots force_newline (rule_elem "") not_brace rule_elem_dots; true
  | Ast.CODE(rule_elem_dots) ->
      dots force_newline (rule_elem "") not_brace rule_elem_dots; true

let rule = print_between force_newline true_fn top_level

let _ =
  anything := function
      Ast.FullTypeTag(x) -> typeC x
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
    | Ast.Rule_elemTag(x) -> let _ = rule_elem "" x in ()
    | Ast.ValueQualifTag(x) -> value_qualif x
    | Ast.Token(x) -> print_string x
    | Ast.Code(x) -> let _ = top_level x in ()

let unparse x =
  print_string "\n@@\n@@";
  force_newline();
  force_newline();
  rule x;
  force_newline()
