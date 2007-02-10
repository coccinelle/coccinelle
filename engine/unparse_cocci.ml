open Common open Commonop

(*****************************************************************************)
(* mostly a copy paste of parsing_cocci/pretty_print_cocci.ml 
 * todo?: try to factorize ?
 *)
(*****************************************************************************)

module Ast = Ast_cocci

let term ((s,_,_) : 'a Ast_cocci.mcode) = s

(* or perhaps can have in plus, for instance a Disj, but those Disj must be 
 *  handled by interactive tool (by proposing alternatives) 
 *)
exception CantBeInPlus

(*****************************************************************************)

let rec pp_list_list_any (env, current_tabbing, pr, pr_elem) xxs =

(* Just to be able to copy paste the code from pretty_print_cocci.ml. *)
let print_string = pr in
let close_box() = () in
let print_space() = pr " "  in
let force_newline () = () in

let start_block () = () in
let end_block () = () in
let print_string_box s = print_string s in

let print_option = Common.do_option in
let print_between = Common.print_between in

(* --------------------------------------------------------------------- *)

let handle_metavar name fn = 
  match (Common.optionise (fun () -> List.assoc (term name) env)) with
  | None ->
      failwith (Printf.sprintf "SP line %d: Not found a value in env for: %s"
		  (Ast_cocci.get_mcode_line name) (term name))
  | Some e  -> fn e
in

(* --------------------------------------------------------------------- *)
(* Here we don't care about the annotation on s. *)
let mcode fn = function ((s,_,_)) -> fn s in

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
  | Ast.MetaId(name,true,_) -> 
      handle_metavar name (function
        | (Ast_c.MetaIdVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.MetaFunc(name,true,_) -> 
      handle_metavar name (function
        | (Ast_c.MetaFuncVal id) -> pr id
        | _ -> raise Impossible
        ) 
  | Ast.MetaLocalFunc(name,true,_) -> 
      handle_metavar name (function
        | (Ast_c.MetaLocalFuncVal id) -> pr id
        | _ -> raise Impossible
        )

  | Ast.MetaId(_,false,_) | Ast.MetaFunc(_,false,_)
  | Ast.MetaLocalFunc(_,false,_)
  | Ast.OptIdent(_) | Ast.UniqueIdent(_) | Ast.MultiIdent(_) -> 
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
      mcode print_string rp
  | Ast.TypeExp(ty) -> fullType ty

  | Ast.MetaConst(name,true,None,_) -> 
      failwith "metaConst not handled"
  | Ast.MetaConst(name,true,Some ty,_) ->
      failwith "metaConst not handled"

  | Ast.MetaErr(name,true,_) -> 
      failwith "metaErr not handled"

  | Ast.MetaExpr (name,true,_typedontcare,_) -> 
      handle_metavar name  (function
        | Ast_c.MetaExprVal exp -> 
            Pretty_print_c.pp_expression_gen pr_elem  exp
        | _ -> raise Impossible
                           )

  | Ast.MetaExprList (name,true,_) -> 
      failwith "not handling MetaExprList"

  | Ast.MetaConst(name,false,_,_)
  | Ast.MetaErr(name,false,_)
  | Ast.MetaExpr (name,false,_,_)
  | Ast.MetaExprList (name,false,_) -> raise CantBeInPlus
      
  | Ast.EComma(cm) -> mcode print_string cm; print_space()

  | Ast.DisjExpr _ 
  | Ast.NestExpr(_) 
  | Ast.Edots(_)
  | Ast.Ecircles(_)
  | Ast.Estars(_) 
    -> raise CantBeInPlus

  | Ast.OptExp(exp) | Ast.UniqueExp(exp) | Ast.MultiExp(exp) -> 
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
  | Ast.OptType(_) | Ast.UniqueType(_) | Ast.MultiType(_) ->
      raise CantBeInPlus

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,sgn) -> mcode baseType ty; print_option (mcode sign) sgn
  | Ast.ImplicitInt(sgn) -> mcode sign sgn
  | Ast.Pointer(ty,star) -> fullType ty; mcode print_string star
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.StructUnionName(kind,name) ->
      mcode structUnion kind; ident name; print_string " "
  | Ast.StructUnionDef(kind,name,lb,decls,rb) ->
      mcode structUnion kind; ident name; print_string " ";
      mcode print_string lb;
      print_between force_newline declaration decls;
      mcode print_string rb
  | Ast.TypeName(name)-> mcode print_string name; print_string " "
  | Ast.MetaType(name,true,_) -> 
      handle_metavar name  (function
        | Ast_c.MetaTypeVal exp -> 
            Pretty_print_c.pp_type_gen pr_elem  exp
        | _ -> raise Impossible
                           )
  | Ast.MetaType(name,false,_) -> 
      raise CantBeInPlus



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
(* Function declaration *)

and storage = function
    Ast.Static -> print_string "static "
  | Ast.Auto -> print_string "auto "
  | Ast.Register -> print_string "register "
  | Ast.Extern -> print_string "extern "

(* --------------------------------------------------------------------- *)
(* Variable declaration *)

and declaration d =
  match Ast.unwrap d with
    Ast.Init(stg,ty,id,eq,ini,sem) ->
      print_option (mcode storage) stg;
      fullType ty; ident id; print_string " "; mcode print_string eq;
      print_string " "; initialiser ini; mcode print_string sem
  | Ast.UnInit(stg,ty,id,sem) ->
      print_option (mcode storage) stg;
      fullType ty; ident id; mcode print_string sem
  | Ast.TyDecl(ty,sem) -> fullType ty; mcode print_string sem
  | Ast.DisjDecl(_) | Ast.MetaDecl(_,_,_) -> raise CantBeInPlus
  | Ast.OptDecl(decl)  | Ast.UniqueDecl(decl) | Ast.MultiDecl(decl) -> 
      raise CantBeInPlus

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser i =
  match Ast.unwrap i with
    Ast.InitExpr(exp) -> expression exp
  | Ast.InitList(lb,initlist,rb,[]) ->
      mcode print_string lb;
      let _ =
	print_between (function _ -> print_string ", ") initialiser initlist in
      mcode print_string rb
  | Ast.InitList(lb,initlist,rb,_) -> failwith "unexpected whencode in plus"
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
  | Ast.OptIni(ini) | Ast.UniqueIni(ini) | Ast.MultiIni(ini) ->
      raise CantBeInPlus
in

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> fullType ty
  | Ast.Param(id,ty) -> fullType ty; ident id

  | Ast.MetaParam(name,true,_) -> 
      failwith "not handling MetaParam"
  | Ast.MetaParamList(name,true,_) -> 
      failwith "not handling MetaParamList"
  | Ast.MetaParam(name,false,_)
  | Ast.MetaParamList(name,false,_) -> raise CantBeInPlus

  | Ast.PComma(cm) -> mcode print_string cm; print_space()
  | Ast.Pdots(dots) 
  | Ast.Pcircles(dots) 
    ->  raise CantBeInPlus
  | Ast.OptParam(param) | Ast.UniqueParam(param) -> raise CantBeInPlus
in


let parameter_list = dots (function _ -> ()) parameterTypeDef
in


(* --------------------------------------------------------------------- *)
(* CPP code *)

let define_body m =
  match Ast.unwrap m with
    Ast.DMetaId(name,true) -> 
      handle_metavar name (function
      | (Ast_c.MetaTextVal text) -> pr text
      | _ -> raise Impossible
      ) 

  | Ast.DMetaId(name,false) -> raise CantBeInPlus
  | Ast.Ddots(dots) -> mcode print_string dots in

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rule_elem arity re =
  match Ast.unwrap re with
    Ast.FunHeader(_,_,stg,ty,name,lp,params,rp) ->
      print_string arity;
      print_option (mcode storage) stg;
      print_option fullType ty;
      ident name; mcode print_string_box lp;
      parameter_list params; close_box(); mcode print_string rp;
      print_string " "
  | Ast.Decl(_,decl) -> print_string arity; declaration decl

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

  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      print_string arity;
      mcode print_string switch; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "

  | Ast.Break(br,sem) ->
      print_string arity; mcode print_string br; mcode print_string sem
  | Ast.Continue(cont,sem) ->
      print_string arity; mcode print_string cont; mcode print_string sem
  | Ast.Goto -> print_string "goto"
  | Ast.Return(ret,sem) ->
      print_string arity; mcode print_string ret; mcode print_string sem
  | Ast.ReturnExpr(ret,exp,sem) ->
      print_string arity; mcode print_string ret; print_string " ";
      expression exp; mcode print_string sem

  | Ast.Exp(exp) -> print_string arity; expression exp
  | Ast.Ty(ty) -> print_string arity; fullType ty
  | Ast.Include(inc,s) ->
      mcode print_string inc; print_string " "; mcode print_string s
  | Ast.Define(def,id,body) ->
      mcode print_string def; print_string " "; ident id; 
      define_body body
  | Ast.Default(def,colon) ->
      mcode print_string def; mcode print_string colon; print_string " "
  | Ast.Case(case,exp,colon) ->
      mcode print_string case; print_string " "; expression exp;
      mcode print_string colon; print_string " "

  | Ast.MetaRuleElem(name,true,_) ->
      raise Impossible

  | Ast.MetaStmt(name,true,_,_) ->
      handle_metavar name  (function
        | Ast_c.MetaStmtVal exp -> 
            Pretty_print_c.pp_statement_gen pr_elem  exp
        | _ -> raise Impossible
                           )
  | Ast.MetaStmtList(name,true,_) ->
      failwith
	"MetaStmtList not supported (not even in ast_c metavars binding)"

  | Ast.MetaRuleElem(name,false,_) | Ast.MetaStmt(name,false,_,_)
  | Ast.MetaStmtList(name,false,_) ->
      raise CantBeInPlus in

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,decls,_,body,rbrace) ->
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

  | Ast.Switch(header,lb,cases,rb) ->
      rule_elem arity header; rule_elem arity lb;
      List.iter (function x -> case_line arity x; force_newline()) cases;
      rule_elem arity rb

  | Ast.Atomic(re) -> rule_elem arity re

  | Ast.FunDecl(header,lbrace,decls,_,body,rbrace) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) decls;
      dots force_newline (statement arity) body; rule_elem arity rbrace

  | Ast.Disj(_)| Ast.Nest(_)
  | Ast.Dots(_) | Ast.Circles(_) | Ast.Stars(_) ->
      raise CantBeInPlus

  | Ast.OptStm(s) | Ast.UniqueStm(s) | Ast.MultiStm(s) -> 
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


let rec pp_any = function
  (* assert: normally there is only CONTEXT NOTHING tokens in any *)
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

  | Ast.StorageTag(x) -> storage x

  | Ast.Rule_elemTag(x) -> rule_elem "" x
  | Ast.StatementTag(x) -> statement "" x
  | Ast.CaseLineTag(x) -> case_line "" x

  | Ast.ConstVolTag(x) -> const_vol x
  | Ast.Token(x) -> print_string x
  | Ast.Code(x) -> let _ = top_level x in ()

  (* this is not '...', but a list of expr/statement/params, and 
     normally there should be no '...' inside them *)
  | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x
  | Ast.ParamDotsTag(x) -> parameter_list x
  | Ast.StmtDotsTag(x) -> dots (function _ -> ()) (statement "") x

  | Ast.TypeCTag(x) -> typeC x
  | Ast.ParamTag(x) -> parameterTypeDef x
  | Ast.SgrepStartTag(x) -> failwith "unexpected start tag"
  | Ast.SgrepEndTag(x) -> failwith "unexpected end tag"
in

  (* todo? imitate what is in unparse_cocci ? *)
  match xxs with
  | xs::xxs -> 
      xs +> List.iter (fun any -> 
        (match any with
        | Ast.Rule_elemTag _ -> pr "\n"; pr current_tabbing;
        | Ast.StatementTag _ -> pr "\n"; pr current_tabbing;
        | Ast.InitTag x -> 
            (match Ast.unwrap x with
            | Ast.InitGccDotName _ 
            | Ast.InitGccName _
            | Ast.InitGccIndex _
            | Ast.InitGccRange _ ->
                pr ","; pr "\n"; pr current_tabbing; 
            | _ -> ()
            )
        | _ -> ()
        );

        pp_any any
      );
      xxs +> List.iter (fun xs -> 
        pr "\n"; 
        pr current_tabbing;
        xs +> List.iter pp_any
      )
  | [] -> ()

