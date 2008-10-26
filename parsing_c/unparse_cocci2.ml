open Common

(*****************************************************************************)
(* mostly a copy paste of parsing_cocci/pretty_print_cocci.ml 
 * todo?: try to factorize ?
 *)
(*****************************************************************************)

module Ast = Ast_cocci

let term s = Ast.unwrap_mcode s

(* or perhaps can have in plus, for instance a Disj, but those Disj must be 
 *  handled by interactive tool (by proposing alternatives) 
 *)
exception CantBeInPlus

(*****************************************************************************)

type pos = Before | After | InPlace

let rec pp_list_list_any (env, pr, pr_elem, pr_space, indent, unindent)
    xxs before =

(* Just to be able to copy paste the code from pretty_print_cocci.ml. *)
let print_string = pr in
let close_box _ = () in
let print_space() = pr " "  in
let force_newline () = pr "\n" in

let start_block () = force_newline(); indent() in
let end_block () = unindent(); force_newline () in
let print_string_box s = print_string s in

let print_option = Common.do_option in
let print_between = Common.print_between in

(* --------------------------------------------------------------------- *)

let handle_metavar name fn = 
  match (Common.optionise (fun () -> List.assoc (term name) env)) with
  | None ->
      let name_string (_,s) = s in
      failwith (Printf.sprintf "SP line %d: Not found a value in env for: %s"
		  (Ast_cocci.get_mcode_line name) (name_string (term name)))
  | Some e  -> fn e
in

(* --------------------------------------------------------------------- *)
(* Here we don't care about the annotation on s. *)
let mcode fn (s,info,_,_) =
  List.iter (function str -> print_string str; print_string "\n")
    info.Ast.strbef;
  if info.Ast.column > 0 && not(info.Ast.strbef = [])
  then print_string (String.make info.Ast.column ' ');
  fn s;
  match info.Ast.straft with
    [] -> ()
  | aft ->
      List.iter (function str -> print_string "\n"; print_string str) aft;
      print_string "\n"; (*XXX pr current_tabbing *)
in

(* --------------------------------------------------------------------- *)
let dots between fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) -> print_between between fn l
  | Ast.CIRCLES(l) -> print_between between fn l
  | Ast.STARS(l) -> print_between between fn l
in


(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
    Ast.Id(name) -> mcode print_string name
  | Ast.MetaId(name,_,_,_) -> 
      handle_metavar name (function
        | (Ast_c.MetaIdVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.MetaFunc(name,_,_,_) -> 
      handle_metavar name (function
        | (Ast_c.MetaFuncVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.MetaLocalFunc(name,_,_,_) -> 
      handle_metavar name (function
        | (Ast_c.MetaLocalFuncVal id) -> pr id
        | _ -> raise Impossible
        )

  | Ast.OptIdent(_) | Ast.UniqueIdent(_) -> 
      raise CantBeInPlus

in

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
  | Ast.Assignment(left,op,right,_) ->
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
  | Ast.Nested(left,op,right) -> failwith "nested only in minus code"
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
      mcode print_string rp
  | Ast.TypeExp(ty) -> fullType ty

  | Ast.MetaErr(name,_,_,_) -> 
      failwith "metaErr not handled"

  | Ast.MetaExpr (name,_,_,_typedontcare,_formdontcare,_) ->
      handle_metavar name  (function
        | Ast_c.MetaExprVal exp -> 
            Pretty_print_c.pp_expression_gen pr_elem pr_space  exp
        | _ -> raise Impossible
      )

  | Ast.MetaExprList (name,_,_,_) -> 
      failwith "not handling MetaExprList"
      
  | Ast.EComma(cm) -> mcode print_string cm; print_space()

  | Ast.DisjExpr _ 
  | Ast.NestExpr(_) 
  | Ast.Edots(_)
  | Ast.Ecircles(_)
  | Ast.Estars(_) 
    -> raise CantBeInPlus

  | Ast.OptExp(exp) | Ast.UniqueExp(exp) -> 
      raise CantBeInPlus

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
  | Ast.DisjType _ -> failwith "can't be in plus"
  | Ast.OptType(_) | Ast.UniqueType(_) ->
      raise CantBeInPlus

and print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2) fn =
  fullType ty; mcode print_string lp1; mcode print_string star; fn();
  mcode print_string rp1; mcode print_string lp1;
  parameter_list params; mcode print_string rp2

and print_function_type (ty,lp1,params,rp1) fn =
  print_option fullType ty; fn(); mcode print_string lp1;
  parameter_list params; mcode print_string rp1

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,sgn) -> print_option (mcode sign) sgn; mcode baseType ty
  | Ast.ImplicitInt(sgn) -> mcode signns sgn
  | Ast.Pointer(ty,star) -> fullType ty; ft_space ty; mcode print_string star
  | Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	(function _ -> ())
  | Ast.FunctionType (am,ty,lp1,params,rp1) ->
      print_function_type (ty,lp1,params,rp1) (function _ -> ())
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.StructUnionName(kind,name) ->
      mcode structUnion kind;
      print_option ident name
  | Ast.StructUnionDef(ty,lb,decls,rb) ->
      fullType ty;
      mcode print_string lb;
      dots force_newline declaration decls;
      mcode print_string rb
  | Ast.TypeName(name)-> mcode print_string name
  | Ast.MetaType(name,_,_) -> 
      handle_metavar name  (function
          Ast_c.MetaTypeVal exp -> 
            Pretty_print_c.pp_type_gen pr_elem pr_space exp
        | _ -> raise Impossible)

and baseType = function
    Ast.VoidType -> print_string "void"
  | Ast.CharType -> print_string "char"
  | Ast.ShortType -> print_string "short"
  | Ast.IntType -> print_string "int"
  | Ast.DoubleType -> print_string "double"
  | Ast.FloatType -> print_string "float"
  | Ast.LongType -> print_string "long"

and structUnion = function
    Ast.Struct -> print_string "struct "
  | Ast.Union -> print_string "union "

and sign = function
    Ast.Signed -> print_string "signed "
  | Ast.Unsigned -> print_string "unsigned "

and signns = function (* no space, like a normal type *)
    Ast.Signed -> print_string "signed"
  | Ast.Unsigned -> print_string "unsigned"


and const_vol = function
    Ast.Const -> print_string "const "
  | Ast.Volatile -> print_string "volatile "

(* --------------------------------------------------------------------- *)
(* Function declaration *)

and storage = function
    Ast.Static -> print_string "static "
  | Ast.Auto -> print_string "auto "
  | Ast.Register -> print_string "register "
  | Ast.Extern -> print_string "extern "

(* --------------------------------------------------------------------- *)
(* Variable declaration *)

and print_named_type ty id =
  match Ast.unwrap ty with
    Ast.Type(None,ty1) ->
      (match Ast.unwrap ty1 with
	Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	  print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	    (function _ -> print_string " "; ident id)
      | Ast.FunctionType(am,ty,lp1,params,rp1) ->
	  print_function_type (ty,lp1,params,rp1)
	    (function _ -> print_string " "; ident id)
      | Ast.Array(_,_,_,_) ->
	  let rec loop ty k =
	    match Ast.unwrap ty with
	      Ast.Array(ty,lb,size,rb) ->
		(match Ast.unwrap ty with
		  Ast.Type(None,ty) ->
		    loop ty
		      (function _ ->
			k ();
			mcode print_string lb;
			print_option expression size;
			mcode print_string rb)
		| _ -> failwith "complex array types not supported")
	    | _ -> typeC ty; ty_space ty; ident id; k () in
	  loop ty1 (function _ -> ())
    (*| should have a case here for pointer to array or function type
        that would put ( * ) around the variable.  This makes one wonder
        why we really need a special case for function pointer *)
      | _ -> fullType ty; ft_space ty; ident id)
  | _ -> fullType ty; ft_space ty; ident id

and ty_space ty =
  match Ast.unwrap ty with
    Ast.Pointer(_,_) -> ()
  | _ -> print_space()

and ft_space ty =
  match Ast.unwrap ty with
    Ast.Type(cv,ty) ->
      (match Ast.unwrap ty with
	Ast.Pointer(_,_) -> ()
      | _ -> print_space())
  | _ -> print_space()

and declaration d =
  match Ast.unwrap d with
    Ast.Init(stg,ty,id,eq,ini,sem) ->
      print_option (mcode storage) stg;
      print_named_type ty id;
      print_string " "; mcode print_string eq;
      print_string " "; initialiser true ini; mcode print_string sem
  | Ast.UnInit(stg,ty,id,sem) ->
      print_option (mcode storage) stg;
      print_named_type ty id;
      mcode print_string sem
  | Ast.MacroDecl(name,lp,args,rp,sem) ->
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp; mcode print_string sem
  | Ast.TyDecl(ty,sem) -> fullType ty; mcode print_string sem
  | Ast.Typedef(stg,ty,id,sem) ->
      mcode print_string stg;
      fullType ty; typeC id;
      mcode print_string sem
  | Ast.DisjDecl(_) | Ast.MetaDecl(_,_,_) -> raise CantBeInPlus
  | Ast.Ddots(_,_) -> raise CantBeInPlus
  | Ast.OptDecl(decl)  | Ast.UniqueDecl(decl) -> 
      raise CantBeInPlus

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser nlcomma i =
  match Ast.unwrap i with
    Ast.InitExpr(exp) -> expression exp
  | Ast.InitList(lb,initlist,rb,[]) ->
      mcode print_string lb; start_block();
      (* awkward, because the comma is separate from the initialiser *)
      let rec loop = function
	  [] -> ()
	| [x] -> initialiser false x
	| x::xs -> initialiser nlcomma x; loop xs in
      loop initlist;
      end_block(); mcode print_string rb
  | Ast.InitList(lb,initlist,rb,_) -> failwith "unexpected whencode in plus"
  | Ast.InitGccDotName(dot,name,eq,ini) ->
      mcode print_string dot; ident name; print_string " ";
      mcode print_string eq; print_string " "; initialiser nlcomma ini
  | Ast.InitGccName(name,eq,ini) ->
      ident name; mcode print_string eq; initialiser nlcomma ini
  | Ast.InitGccIndex(lb,exp,rb,eq,ini) ->
      mcode print_string lb; expression exp; mcode print_string rb;
      print_string " "; mcode print_string eq; print_string " ";
      initialiser nlcomma ini
  | Ast.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
      mcode print_string lb; expression exp1; mcode print_string dots;
      expression exp2; mcode print_string rb;
      print_string " "; mcode print_string eq; print_string " ";
      initialiser nlcomma ini
  | Ast.IComma(comma) ->
      mcode print_string comma;
      if nlcomma then force_newline()
  | Ast.OptIni(ini) | Ast.UniqueIni(ini) ->
      raise CantBeInPlus

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> fullType ty
  | Ast.Param(ty,Some id) -> print_named_type ty id
  | Ast.Param(ty,None) -> fullType ty

  | Ast.MetaParam(name,_,_) -> 
      failwith "not handling MetaParam"
  | Ast.MetaParamList(name,_,_,_) -> 
      failwith "not handling MetaParamList"

  | Ast.PComma(cm) -> mcode print_string cm; print_space()
  | Ast.Pdots(dots) 
  | Ast.Pcircles(dots) 
    ->  raise CantBeInPlus
  | Ast.OptParam(param) | Ast.UniqueParam(param) -> raise CantBeInPlus

and parameter_list l = dots (function _ -> ()) parameterTypeDef l
in


(* --------------------------------------------------------------------- *)
(* CPP code *)

let rec inc_file = function
    Ast.Local(elems) ->
      print_string "\"";
      print_between (function _ -> print_string "/") inc_elem elems;
      print_string "\""
  | Ast.NonLocal(elems) ->
      print_string "<";
      print_between (function _ -> print_string "/") inc_elem elems;
      print_string ">"

and inc_elem = function
    Ast.IncPath s -> print_string s
  | Ast.IncDots -> print_string "..."

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and rule_elem arity re =
  match Ast.unwrap re with
    Ast.FunHeader(_,_,fninfo,name,lp,params,rp) ->
      print_string arity; List.iter print_fninfo fninfo;
      ident name; mcode print_string_box lp;
      parameter_list params; close_box(); mcode print_string rp;
      print_string " "
  | Ast.Decl(_,_,decl) -> print_string arity; declaration decl

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
  | Ast.IteratorHeader(nm,lp,args,rp) ->
      print_string arity;
      ident nm; print_string " "; mcode print_string_box lp;
      dots (function _ -> ()) expression args; close_box();
      mcode print_string rp; print_string " "

  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      print_string arity;
      mcode print_string switch; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "

  | Ast.Break(br,sem) ->
      print_string arity; mcode print_string br; mcode print_string sem
  | Ast.Continue(cont,sem) ->
      print_string arity; mcode print_string cont; mcode print_string sem
  | Ast.Label(l,dd) -> ident l; mcode print_string dd
  | Ast.Goto(goto,l,sem) ->
      mcode print_string goto; ident l; mcode print_string sem
  | Ast.Return(ret,sem) ->
      print_string arity; mcode print_string ret; 
      mcode print_string sem
  | Ast.ReturnExpr(ret,exp,sem) ->
      print_string arity; mcode print_string ret; print_string " ";
      expression exp; mcode print_string sem

  | Ast.Exp(exp) -> print_string arity; expression exp
  | Ast.TopExp(exp) -> print_string arity; expression exp
  | Ast.Ty(ty) -> print_string arity; fullType ty
  | Ast.TopInit(init) -> initialiser false init
  | Ast.Include(inc,s) ->
      mcode print_string inc; print_string " "; mcode inc_file s
  | Ast.DefineHeader(def,id,params) ->
      mcode print_string def; print_string " "; ident id; 
      print_define_parameters params
  | Ast.Default(def,colon) ->
      mcode print_string def; mcode print_string colon; print_string " "
  | Ast.Case(case,exp,colon) ->
      mcode print_string case; print_string " "; expression exp;
      mcode print_string colon; print_string " "
  | Ast.DisjRuleElem(res) -> raise CantBeInPlus

  | Ast.MetaRuleElem(name,_,_) ->
      raise Impossible

  | Ast.MetaStmt(name,_,_,_) ->
      handle_metavar name  (function
        | Ast_c.MetaStmtVal exp -> 
            Pretty_print_c.pp_statement_gen pr_elem pr_space  exp
        | _ -> raise Impossible
                           )
  | Ast.MetaStmtList(name,_,_) ->
      failwith
	"MetaStmtList not supported (not even in ast_c metavars binding)"

and print_define_parameters params =
  match Ast.unwrap params with
    Ast.NoParams -> ()
  | Ast.DParams(lp,params,rp) ->
      mcode print_string lp;
      dots (function _ -> ()) print_define_param params; mcode print_string rp

and print_define_param param =
  match Ast.unwrap param with
    Ast.DParam(id) -> ident id
  | Ast.DPComma(comma) -> mcode print_string comma
  | Ast.DPdots(dots) -> mcode print_string dots
  | Ast.DPcircles(circles) -> mcode print_string circles
  | Ast.OptDParam(dp) -> print_string "?"; print_define_param dp
  | Ast.UniqueDParam(dp) -> print_string "!"; print_define_param dp

and print_fninfo = function
    Ast.FStorage(stg) -> mcode storage stg
  | Ast.FType(ty) -> fullType ty
  | Ast.FInline(inline) -> mcode print_string inline; print_string " "
  | Ast.FAttr(attr) -> mcode print_string attr; print_string " " in

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,decls,body,rbrace) ->
      rule_elem arity lbrace;
      dots force_newline (statement arity) decls;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace

  | Ast.IfThen(header,branch,_) ->
      rule_elem arity header; statement arity branch
  | Ast.IfThenElse(header,branch1,els,branch2,_) ->
      rule_elem arity header; statement arity branch1; print_string " ";
      rule_elem arity els; statement arity branch2

  | Ast.While(header,body,_) ->
      rule_elem arity header; statement arity body
  | Ast.Do(header,body,tail) ->
      rule_elem arity header; statement arity body;
      rule_elem arity tail
  | Ast.For(header,body,_) ->
      rule_elem arity header; statement arity body
  | Ast.Iterator(header,body,(_,_,_,aft)) ->
      rule_elem arity header; statement arity body;
      mcode (function _ -> ()) ((),Ast.no_info,aft,Ast.NoMetaPos)

  | Ast.Switch(header,lb,cases,rb) ->
      rule_elem arity header; rule_elem arity lb;
      List.iter (function x -> case_line arity x; force_newline()) cases;
      rule_elem arity rb

  | Ast.Atomic(re) -> rule_elem arity re

  | Ast.FunDecl(header,lbrace,decls,body,rbrace) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) decls;
      dots force_newline (statement arity) body; rule_elem arity rbrace

  | Ast.Define(header,body) ->
      rule_elem arity header; print_string " ";
      dots force_newline (statement arity) body

  | Ast.Disj(_)| Ast.Nest(_)
  | Ast.Dots(_) | Ast.Circles(_) | Ast.Stars(_) ->
      raise CantBeInPlus

  | Ast.OptStm(s) | Ast.UniqueStm(s) -> 
      raise CantBeInPlus

and case_line arity c =
  match Ast.unwrap c with
    Ast.CaseLine(header,code) ->
      rule_elem arity header; print_string " ";
      dots force_newline (statement arity) code
  | Ast.OptCase(case) -> raise CantBeInPlus in

let top_level t =
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) -> raise CantBeInPlus
  | Ast.DECL(stmt) -> statement "" stmt
  | Ast.CODE(stmt_dots) -> dots force_newline (statement "") stmt_dots
  | Ast.ERRORWORDS(exps) -> raise CantBeInPlus
in

(*    
let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level
in
*)

let if_open_brace  = function "{" -> true | _ -> false in

let rec pp_any = function
  (* assert: normally there is only CONTEXT NOTHING tokens in any *)
    Ast.FullTypeTag(x) -> fullType x; false
  | Ast.BaseTypeTag(x) -> baseType x; false
  | Ast.StructUnionTag(x) -> structUnion x; false
  | Ast.SignTag(x) -> sign x; false

  | Ast.IdentTag(x) -> ident x; false

  | Ast.ExpressionTag(x) -> expression x; false

  | Ast.ConstantTag(x) -> constant x; false
  | Ast.UnaryOpTag(x) -> unaryOp x; false
  | Ast.AssignOpTag(x) -> assignOp x; false
  | Ast.FixOpTag(x) -> fixOp x; false
  | Ast.BinaryOpTag(x) -> binaryOp x; false
  | Ast.ArithOpTag(x) -> arithOp x; false
  | Ast.LogicalOpTag(x) -> logicalOp x; false

  | Ast.InitTag(x) -> initialiser false x; false
  | Ast.DeclarationTag(x) -> declaration x; false

  | Ast.StorageTag(x) -> storage x; false
  | Ast.IncFileTag(x) -> inc_file x; false

  | Ast.Rule_elemTag(x) -> rule_elem "" x; false
  | Ast.StatementTag(x) -> statement "" x; false
  | Ast.CaseLineTag(x) -> case_line "" x; false

  | Ast.ConstVolTag(x) -> const_vol x; false
  | Ast.Token(x,None) -> print_string x; if_open_brace x
  | Ast.Token(x,Some info) -> 
      mcode
	(function x ->
	  (match x with
	    "else" -> pr "\n"
	  | _ -> ());
	  print_string x;
	  (* if x ==~ Common.regexp_alpha then print_string " "; *)
	  (match x with
	    (*"return" |*) "else" -> print_string " "
	  | _ -> ()))
	(x,info,(),Ast.NoMetaPos);
      if_open_brace x

  | Ast.Code(x) -> let _ = top_level x in false

  (* this is not '...', but a list of expr/statement/params, and 
     normally there should be no '...' inside them *)
  | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x; false
  | Ast.ParamDotsTag(x) -> parameter_list x; false
  | Ast.StmtDotsTag(x) -> dots (function _ -> pr "\n") (statement "") x; false
  | Ast.DeclDotsTag(x) -> dots (function _ -> pr "\n") declaration x; false

  | Ast.TypeCTag(x) -> typeC x; false
  | Ast.ParamTag(x) -> parameterTypeDef x; false
  | Ast.SgrepStartTag(x) -> failwith "unexpected start tag"
  | Ast.SgrepEndTag(x) -> failwith "unexpected end tag"
in

  (* todo? imitate what is in pretty_print_cocci ? *)
  match xxs with
    [] -> ()
  | x::xs -> 
      (* for many tags, we must not do a newline before the first '+' *)
      let isfn s =
	match Ast.unwrap s with Ast.FunDecl _ -> true | _ -> false in
      let unindent_before = function
        (* need to get unindent before newline for } *)
	  (Ast.Token ("}",_)::_) -> true
	| _ -> false in
      let prnl x =
	(if unindent_before x then unindent());
	pr "\n" in
      let newline_before _ =
	if before = After
	then
	  let hd = List.hd xxs in
	  match hd with
            (Ast.StatementTag s::_) when isfn s -> pr "\n\n"
          | (Ast.Rule_elemTag _::_) | (Ast.StatementTag _::_)
	  | (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("}",_)::_) -> prnl hd
          | _ -> () in
      let newline_after _ =
	if before = Before
	then
	  match List.rev(List.hd(List.rev xxs)) with
	    (Ast.StatementTag s::_) when isfn s -> pr "\n\n"
          | (Ast.Rule_elemTag _::_) | (Ast.StatementTag _::_)
	  | (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("{",_)::_) -> pr "\n"
          | _ -> () in
      (* print a newline at the beginning, if needed *)
      newline_before();
      (* print a newline before each of the rest *)
      let rec loop leading_newline indent_needed = function
	  [] -> ()
	| x::xs ->
	    (if leading_newline
	    then
	      match (indent_needed,unindent_before x) with
		(true,true) -> pr "\n"
	      | (true,false) -> pr "\n"; indent()
	      | (false,true) -> unindent(); pr "\n"
	      | (false,false) -> pr "\n");
	    let indent_needed =
	      List.fold_left (function indent_needed -> pp_any) false x in
	    loop true indent_needed xs in
      loop false false (x::xs);
      (* print a newline at the end, if needed *)
      newline_after()

