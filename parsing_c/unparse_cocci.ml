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
    generating xxs before =

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

let outdent _ = () (* should go to leftmost col, does nothing now *) in

let pretty_print_c =
  Pretty_print_c.pretty_print_c pr_elem pr_space
    force_newline indent outdent unindent in

(* --------------------------------------------------------------------- *)
(* Only for make_hrule, print plus code, unbound metavariables *)

(* avoid polyvariance problems *)
let anything : (Ast.anything -> unit) ref = ref (function _ -> ()) in

let rec print_anything = function
    [] -> ()
  | stream ->
      start_block();
      print_between force_newline print_anything_list stream;
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
	| Ast.Token("if",_) | Ast.Token("while",_) -> true | _ -> false) or
	(match aft with
	  Ast.Rule_elemTag(_) | Ast.AssignOpTag(_) | Ast.BinaryOpTag(_)
	| Ast.ArithOpTag(_) | Ast.LogicalOpTag(_) | Ast.Token("{",_) -> true
	| _ -> false) in
      if space then print_string " ";
      print_anything_list rest in

let print_around printer term = function
    Ast.NOTHING -> printer term
  | Ast.BEFORE(bef) -> print_anything bef; printer term
  | Ast.AFTER(aft) -> printer term; print_anything aft
  | Ast.BEFOREAFTER(bef,aft) ->
      print_anything bef; printer term; print_anything aft in

let print_string_befaft fn fn1 x info =
  List.iter (function (s,_,_) -> fn1(); print_string s; force_newline())
    info.Ast.strbef;
  fn x;
  List.iter (function (s,_,_) -> force_newline(); fn1(); print_string s)
    info.Ast.straft in

let print_meta (r,x) = print_string x in

let print_pos = function
    Ast.MetaPos(name,_,_,_,_) ->
      let name = Ast.unwrap_mcode name in
      print_string "@"; print_meta name
  | _ -> () in

(* --------------------------------------------------------------------- *)

let mcode fn arg =
  match (generating,arg) with
    (false,(s,info,_,_)) ->
    (* printing for transformation *)
    (* Here we don't care about the annotation on s. *)
      let print_comments lb comments =
	List.fold_left
	  (function line_before ->
	    function (str,line,col) ->
	      match line_before with
		None -> print_string str; Some line
	      |	Some lb when line =|= lb -> print_string str; Some line
	      |	_ -> print_string "\n"; print_string str; Some line)
	  lb comments in
      let line_before = print_comments None info.Ast.strbef in
      (match line_before with
	None -> ()
      |	Some lb when lb =|= info.Ast.line -> ()
      |	_ -> print_string "\n");
      fn s;
      let _ = print_comments (Some info.Ast.line) info.Ast.straft in
      ()
      (* printing for rule generation *)
  | (true, (x, _, Ast.MINUS(_,plus_stream), pos)) ->
      print_string "\n- ";
      fn x; print_pos pos;
      print_anything plus_stream
  | (true, (x, _, Ast.CONTEXT(_,plus_streams), pos)) ->
      let fn x = print_string "\n "; fn x; print_pos pos in
      print_around fn x plus_streams
  | (true,( x, info, Ast.PLUS, pos)) ->
      let fn x = print_string "\n+ "; fn x; print_pos pos in
      print_string_befaft fn (function _ -> print_string "+ ") x info
in


(* --------------------------------------------------------------------- *)

let handle_metavar name fn =
  match (Common.optionise (fun () -> List.assoc (term name) env)) with
  | None ->
      let name_string (_,s) = s in
      if generating
      then mcode (function _ -> pr (name_string (term name))) name
      else
	failwith
	  (Printf.sprintf "SP line %d: Not found a value in env for: %s"
	     (Ast_cocci.get_mcode_line name) (name_string (term name)))
  | Some e  ->
      if generating
      then mcode (function _ -> fn e) name
      else fn e
in
(* --------------------------------------------------------------------- *)
let dots between fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) -> print_between between fn l
  | Ast.CIRCLES(l) -> print_between between fn l
  | Ast.STARS(l) -> print_between between fn l
in

let nest_dots multi fn f d =
  let mo s = if multi then "<+"^s else "<"^s in
  let mc s = if multi then s^"+>" else s^">" in
  match Ast.unwrap d with
    Ast.DOTS(l) ->
      print_string (mo "..."); f(); start_block();
      print_between force_newline fn l;
      end_block(); print_string (mc "...")
  | Ast.CIRCLES(l) ->
      print_string (mo "ooo"); f(); start_block();
      print_between force_newline fn l;
      end_block(); print_string (mc "ooo")
  | Ast.STARS(l) ->
      print_string (mo "***"); f(); start_block();
      print_between force_newline fn l;
      end_block(); print_string (mc "***")
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

let print_disj_list fn l =
  force_newline(); print_string "("; force_newline();
  print_between
    (function _ ->
      force_newline(); print_string "|"; force_newline())
    fn l;
  force_newline(); print_string ")"; force_newline() in

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
            pretty_print_c.Pretty_print_c.expression exp
        | _ -> raise Impossible
      )

  | Ast.MetaExprList (name,_,_,_) -> 
      handle_metavar name  (function
        | Ast_c.MetaExprListVal args -> 
            pretty_print_c.Pretty_print_c.arg_list args
        | _ -> raise Impossible
      )

  | Ast.EComma(cm) -> mcode print_string cm; print_space()

  | Ast.DisjExpr(exp_list) ->
      if generating
      then print_disj_list expression exp_list
      else raise CantBeInPlus
  | Ast.NestExpr(expr_dots,Some whencode,multi) when generating ->
      nest_dots multi expression
	(function _ -> print_string "   when != "; expression whencode)
	expr_dots
  | Ast.NestExpr(expr_dots,None,multi) when generating ->
      nest_dots multi expression (function _ -> ()) expr_dots
  | Ast.NestExpr(_) -> raise CantBeInPlus
  | Ast.Edots(dots,Some whencode)
  | Ast.Ecircles(dots,Some whencode)
  | Ast.Estars(dots,Some whencode) ->
      if generating
      then
	(mcode print_string dots;
	 print_string "   when != ";
	 expression whencode)
      else raise CantBeInPlus
  | Ast.Edots(dots,None)
  | Ast.Ecircles(dots,None)
  | Ast.Estars(dots,None) ->
      if generating
      then mcode print_string dots
      else raise CantBeInPlus

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
      print_option (mcode const_vol) cv;
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
    Ast.BaseType(ty,strings) ->
      print_between pr_space (mcode print_string) strings
  | Ast.SignedT(sgn,Some ty) -> mcode sign sgn; typeC ty
  | Ast.SignedT(sgn,None) -> mcode signns sgn
  | Ast.Pointer(ty,star) -> fullType ty; ft_space ty; mcode print_string star
  | Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	(function _ -> ())
  | Ast.FunctionType (am,ty,lp1,params,rp1) ->
      print_function_type (ty,lp1,params,rp1) (function _ -> ())
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.EnumName(kind,name) -> mcode print_string kind; print_string " ";
      ident name
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
            pretty_print_c.Pretty_print_c.ty exp
        | _ -> raise Impossible)

and baseType = function
    Ast.VoidType -> print_string "void"
  | Ast.CharType -> print_string "char"
  | Ast.ShortType -> print_string "short"
  | Ast.IntType -> print_string "int"
  | Ast.DoubleType -> print_string "double"
  | Ast.FloatType -> print_string "float"
  | Ast.LongType -> print_string "long"
  | Ast.LongLongType -> print_string "long long"

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
    Ast.MetaInit(name,_,_) -> 
      handle_metavar name  (function
          Ast_c.MetaInitVal ini ->
            pretty_print_c.Pretty_print_c.init ini
        | _ -> raise Impossible)
  | Ast.InitExpr(exp) -> expression exp
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
  | Ast.InitGccExt(designators,eq,ini) ->
      List.iter designator designators; print_string " ";
      mcode print_string eq; print_string " "; initialiser nlcomma ini
  | Ast.InitGccName(name,eq,ini) ->
      ident name; mcode print_string eq; initialiser nlcomma ini
  | Ast.IComma(comma) ->
      mcode print_string comma;
      if nlcomma then force_newline()
  | Ast.OptIni(ini) | Ast.UniqueIni(ini) ->
      raise CantBeInPlus

and designator = function
    Ast.DesignatorField(dot,id) -> mcode print_string dot; ident id
  | Ast.DesignatorIndex(lb,exp,rb) ->
      mcode print_string lb; expression exp; mcode print_string rb
  | Ast.DesignatorRange(lb,min,dots,max,rb) ->
      mcode print_string lb; expression min; mcode print_string dots;
      expression max; mcode print_string rb

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
  | Ast.Pdots(dots) | Ast.Pcircles(dots) when generating ->
      mcode print_string dots
  | Ast.Pdots(dots) | Ast.Pcircles(dots) -> raise CantBeInPlus
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
      expression exp; close_box(); mcode print_string rp
  | Ast.Else(els) ->
      print_string arity; mcode print_string els

  | Ast.WhileHeader(whl,lp,exp,rp) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp
  | Ast.DoHeader(d) ->
      print_string arity; mcode print_string d
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
      mcode print_string rp
  | Ast.IteratorHeader(nm,lp,args,rp) ->
      print_string arity;
      ident nm; print_string " "; mcode print_string_box lp;
      dots (function _ -> ()) expression args; close_box();
      mcode print_string rp

  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      print_string arity;
      mcode print_string switch; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp

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
  | Ast.DisjRuleElem(res) ->
      if generating
      then
	(print_string arity;
	 force_newline(); print_string "("; force_newline();
	 print_between
	   (function _ -> force_newline(); print_string "|"; force_newline())
	   (rule_elem arity)
	   res;
	 force_newline(); print_string ")")
      else raise CantBeInPlus

  | Ast.MetaRuleElem(name,_,_) ->
      raise Impossible

  | Ast.MetaStmt(name,_,_,_) ->
      handle_metavar name  (function
        | Ast_c.MetaStmtVal stm ->
            pretty_print_c.Pretty_print_c.statement stm
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

let indent_if_needed s f =
  match Ast.unwrap s with
    Ast.Seq(lbrace,decls,body,rbrace) -> pr_space(); f()
  | _ ->
      (*no newline at the end - someone else will do that*)
      start_block(); f(); unindent() in

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,decls,body,rbrace) ->
      rule_elem arity lbrace;
      dots force_newline (statement arity) decls;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace

  | Ast.IfThen(header,branch,_) ->
      rule_elem arity header;
      indent_if_needed branch (function _ -> statement arity branch)
  | Ast.IfThenElse(header,branch1,els,branch2,_) ->
      rule_elem arity header;
      indent_if_needed branch1 (function _ -> statement arity branch1);
      print_string " ";
      rule_elem arity els;
      indent_if_needed branch2 (function _ -> statement arity branch2)

  | Ast.While(header,body,_) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body)
  | Ast.Do(header,body,tail) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body);
      rule_elem arity tail
  | Ast.For(header,body,_) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body)
  | Ast.Iterator(header,body,(_,_,_,aft)) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body);
      mcode (function _ -> ()) ((),Ast.no_info,aft,Ast.NoMetaPos)

  | Ast.Switch(header,lb,cases,rb) ->
      rule_elem arity header; print_string " "; rule_elem arity lb;
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

  | Ast.Disj([stmt_dots]) ->
      if generating
      then
	(print_string arity;
	 dots force_newline (statement arity) stmt_dots)
      else raise CantBeInPlus
  | Ast.Disj(stmt_dots_list) -> (* ignores newline directive for readability *)
      if generating
      then
	(print_string arity;
	 force_newline(); print_string "("; force_newline();
	 print_between
	   (function _ -> force_newline();print_string "|"; force_newline())
	   (dots force_newline (statement arity))
	   stmt_dots_list;
	 force_newline(); print_string ")")
      else raise CantBeInPlus
  | Ast.Nest(stmt_dots,whn,multi,_,_) when generating ->
      print_string arity;
      nest_dots multi (statement arity)
	(function _ ->
	  print_between force_newline
	    (whencode (dots force_newline (statement "")) (statement "")) whn;
	  force_newline())
	stmt_dots
  | Ast.Nest(_) -> raise CantBeInPlus
  | Ast.Dots(d,whn,_,_) | Ast.Circles(d,whn,_,_) | Ast.Stars(d,whn,_,_) ->
      if generating
      then
	(print_string arity; mcode print_string d;
	 print_between force_newline
	   (whencode (dots force_newline (statement "")) (statement "")) whn;
	 force_newline())
      else raise CantBeInPlus

  | Ast.OptStm(s) | Ast.UniqueStm(s) -> 
      raise CantBeInPlus

and whencode notfn alwaysfn = function
    Ast.WhenNot a ->
      print_string "   WHEN != "; notfn a
  | Ast.WhenAlways a ->
      print_string "   WHEN = "; alwaysfn a
  | Ast.WhenModifier x -> print_string "   WHEN "; print_when_modif x
  | Ast.WhenNotTrue a ->
      print_string "   WHEN != TRUE "; rule_elem "" a
  | Ast.WhenNotFalse a ->
      print_string "   WHEN != FALSE "; rule_elem "" a

and print_when_modif = function
  | Ast.WhenAny    -> print_string "ANY"
  | Ast.WhenStrict -> print_string "STRICT"
  | Ast.WhenForall -> print_string "FORALL"
  | Ast.WhenExists -> print_string "EXISTS"

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

(* boolean result indicates whether an indent is needed *)
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
  | Ast.Pragma(xs) -> print_between force_newline print_string xs; false
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
	(let nomcodekind = Ast.CONTEXT(Ast.DontCarePos,Ast.NOTHING) in
	(x,info,nomcodekind,Ast.NoMetaPos));
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

  anything := (function x -> let _ = pp_any x in ());

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
	if before =*= After
	then
	  let hd = List.hd xxs in
	  match hd with
            (Ast.StatementTag s::_) when isfn s -> pr "\n\n"
	  | (Ast.Pragma _::_)
          | (Ast.Rule_elemTag _::_) | (Ast.StatementTag _::_)
	  | (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("}",_)::_) -> prnl hd
          | _ -> () in
      let newline_after _ =
	if before =*= Before
	then
	  match List.rev(List.hd(List.rev xxs)) with
	    (Ast.StatementTag s::_) ->
	      if isfn s then pr "\n\n" else pr "\n"
	  | (Ast.Pragma _::_)
          | (Ast.Rule_elemTag _::_) | (Ast.InitTag _::_)
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

