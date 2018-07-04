(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* For minus fragment, checks that all of the identifier metavariables that
are used are not declared as fresh, and check that all declared variables
are used.  For plus fragment, just check that the variables declared as
fresh are used.  What is the issue about error variables? (don't remember) *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* all fresh identifiers *)
let fresh_table = (Hashtbl.create(51) : (Ast.meta_name, unit) Hashtbl.t)

let warning s = Printf.fprintf stderr "warning: %s\n" s

let promote name = (name,(),Ast0.default_info(),(),None,-1)

(* --------------------------------------------------------------------- *)

let find_loop table name =
  let rec loop = function
      [] -> raise Not_found
    | x::xs -> (try Hashtbl.find x name with Not_found -> loop xs) in
  loop table

let check_table table minus (name,_,info,_,_,_) =
  let rl = info.Ast0.pos_info.Ast0.line_start in
  if minus
  then
    (try (find_loop table name) := true
    with
      Not_found ->
	(try
	  Hashtbl.find fresh_table name;
	  let (_,name) = name in
	  failwith
	    (Printf.sprintf
	       "%d: unexpected use of a fresh identifier %s" rl name)
	with Not_found -> ()))
  else (try (find_loop table name) := true with Not_found -> ())

let get_opt fn = Common.do_option fn

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn d = List.iter fn (Ast0.unwrap d)

(* --------------------------------------------------------------------- *)
(* Identifier *)

type context = ID | FIELD | FN | GLOBAL

(* heuristic for distinguishing ifdef variables from undeclared metavariables*)
let is_ifdef name =
  String.length name > 2 && Stdcompat.String.uppercase_ascii name = name

let rec ident context old_metas table minus i =
  match Ast0.unwrap i with
    Ast0.Id((name,_,info,_,_,_) : string Ast0.mcode) ->
      let rl = info.Ast0.pos_info.Ast0.line_start in
      let is_plus i =
 	match Ast0.get_mcodekind i with Ast0.PLUS _ -> true | _ -> false in
      let err =
	if List.exists (function x -> x = name) old_metas
	    && (minus || is_plus i)
	then
	  begin
	    warning
	      (Printf.sprintf
		 "line %d: %s, previously declared as a metavariable, is used as an identifier" rl name);
	    true
	  end
	else false in
      (match context with
	ID ->
	  if not (is_ifdef name) && minus &&
	    not err(* warn only once per id *) && not info.Ast0.isSymbolIdent
	  then
	    warning
	      (Printf.sprintf "line %d: should %s be a metavariable?" rl name)
      | _ -> ())
  | Ast0.MetaId(name,_,seedval,_) ->
      check_table table minus name;
      seed table minus seedval
  | Ast0.MetaFunc(name,_,_) -> check_table table minus name
  | Ast0.MetaLocalFunc(name,_,_) -> check_table table minus name
  | Ast0.AsIdent(id,asid) -> failwith "not generated yet"
  | Ast0.DisjId(_,id_list,_,_) ->
      List.iter (ident context old_metas table minus) id_list
  | Ast0.OptIdent(_) ->
      failwith "unexpected code"

and seed table minus = function
    Ast.NoVal -> ()
  | Ast.StringSeed _ -> ()
  | Ast.ListSeed elems ->
      List.iter
	(function
	    Ast.SeedString _ -> ()
	  | Ast.SeedId name -> check_table table minus (promote name))
	elems

(* --------------------------------------------------------------------- *)
(* Operators *)

let assignOp context old_metas table minus op = match Ast0.unwrap op with
  | Ast0.SimpleAssign _ -> ()
  | Ast0.OpAssign _ -> ()
  | Ast0.MetaAssign (name,_,_) -> check_table table minus name

let binaryOp context old_metas table minus op = match Ast0.unwrap op with
  | Ast0.Arith _ -> ()
  | Ast0.Logical _ -> ()
  | Ast0.MetaBinary (name,_,_) -> check_table table minus name

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression context old_metas table minus e =
  match Ast0.unwrap e with
    Ast0.Ident(id) ->
      ident context old_metas table minus id
  | Ast0.StringConstant(lq,str,rq) ->
      dots (string_fragment old_metas table minus) str
  | Ast0.FunCall(fn,lp,args,rp) ->
      expression FN old_metas table minus fn;
      dots (expression ID old_metas table minus) args
  | Ast0.Assignment(left,op,right,_) ->
      expression context old_metas table minus left;
      assignOp context old_metas table minus op;
      expression ID old_metas table minus right
  | Ast0.Sequence(left,op,right) ->
      expression context old_metas table minus left;
      expression ID old_metas table minus right
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      expression ID old_metas table minus exp1;
      get_opt (expression ID old_metas table minus) exp2;
      expression ID old_metas table minus exp3
  | Ast0.Postfix(exp,op) ->
      expression ID old_metas table minus exp
  | Ast0.Infix(exp,op) ->
      expression ID old_metas table minus exp
  | Ast0.Unary(exp,op) ->
      expression ID old_metas table minus exp
  | Ast0.Binary(left,op,right) ->
      expression ID old_metas table minus left;
      binaryOp context old_metas table minus op;
      expression ID old_metas table minus right
  | Ast0.Nested(left,op,right) ->
      expression ID old_metas table minus left;
      binaryOp context old_metas table minus op;
      expression ID old_metas table minus right
  | Ast0.Paren(lp,exp,rp) ->
      expression ID old_metas table minus exp
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      expression ID old_metas table minus exp1;
      expression ID old_metas table minus exp2
  | Ast0.RecordAccess(exp,pt,field) ->
      expression ID old_metas table minus exp;
      ident FIELD old_metas table minus field
  | Ast0.RecordPtAccess(exp,ar,field) ->
      expression ID old_metas table minus exp;
      ident FIELD old_metas table minus field
  | Ast0.Cast(lp,ty,rp,exp) ->
      typeC old_metas table minus ty; expression ID old_metas table minus exp
  | Ast0.SizeOfExpr(szf,exp) -> expression ID old_metas table minus exp
  | Ast0.SizeOfType(szf,lp,ty,rp) -> typeC old_metas table minus ty
  | Ast0.TypeExp(ty) -> typeC old_metas table minus ty
  | Ast0.Constructor(lp,ty,rp,init) ->
      typeC old_metas table minus ty; initialiser old_metas table minus init
  | Ast0.MetaExpr(name,_,Some tys,_,_,bitfield) ->
      List.iter
	(function x ->
	  List.iter
	    (function ty -> check_table table minus (promote ty))
	    (Ast0.meta_names_of_typeC x))
	tys;
      check_table table minus name;
      Common.do_option (check_len table minus) bitfield
  | Ast0.MetaExpr(name,_,_,_,_,_) | Ast0.MetaErr(name,_,_) ->
      check_table table minus name
  | Ast0.MetaExprList(name,len,_,_) ->
      check_table table minus name;
      check_len table minus len
  | Ast0.AsExpr(exp,asexp) -> failwith "not generated yet"
  | Ast0.AsSExpr(exp,asstm) -> failwith "not generated yet"
  | Ast0.DisjExpr(_,exps,_,_)
  | Ast0.ConjExpr(_,exps,_,_) ->
      List.iter (expression context old_metas table minus) exps
  | Ast0.NestExpr(_,exp_dots,_,w,_) ->
      dots (expression ID old_metas table minus) exp_dots;
      get_opt (function (_,_,x) -> expression ID old_metas table minus x) w
  | Ast0.Edots(_,Some (_,_,x)) ->
      expression ID old_metas table minus x
  | Ast0.OptExp(x) ->
      expression ID old_metas table minus x
  | _ -> () (* no metavariable subterms *)

and check_len table minus len =
  match len with
    Ast0.MetaListLen (lenname, _) -> check_table table minus lenname
  | _ -> ()

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC old_metas table minus t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) -> typeC old_metas table minus ty
  | Ast0.Signed(sgn,ty) ->
      get_opt (typeC old_metas table minus) ty
  | Ast0.Pointer(ty,star) -> typeC old_metas table minus ty
  | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      typeC old_metas table minus ty;
      parameter_list old_metas table minus params
  | Ast0.Array(ty,lb,size,rb) ->
      typeC old_metas table minus ty;
      get_opt (expression ID old_metas table minus) size
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      expression ID old_metas table minus length;
      get_opt (expression ID old_metas table minus) precision_opt
  | Ast0.MetaType(name,_,_) ->
      check_table table minus name
  | Ast0.AsType(ty,asty) -> failwith "not generated yet"
  | Ast0.DisjType(_,types,_,_)
  | Ast0.ConjType(_,types,_,_) ->
      List.iter (typeC old_metas table minus) types
  | Ast0.EnumName(en,Some id) -> ident GLOBAL old_metas table minus id
  | Ast0.EnumDef(ty,lb,ids,rb) ->
      typeC old_metas table minus ty;
      dots (expression GLOBAL old_metas table minus) ids
  | Ast0.StructUnionName(su,Some id) -> ident GLOBAL old_metas table minus id
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      typeC old_metas table minus ty;
      dots (field GLOBAL old_metas table minus) decls
  | Ast0.OptType(ty) ->
      failwith "unexpected code"
  | _ -> () (* no metavariable subterms *)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and declaration context old_metas table minus d =
  match Ast0.unwrap d with
    Ast0.MetaDecl(name,_,_) ->
      check_table table minus name
  | Ast0.AsDecl(decl,asdecl) -> failwith "not generated yet"
  | Ast0.Init(stg,ty,id,attr,eq,ini,sem) ->
      typeC old_metas table minus ty;
      ident context old_metas table minus id;
      (match Ast0.unwrap ini with
	Ast0.InitExpr exp ->
	  expression ID old_metas table minus exp
      |	_ ->
	  (*
	  if minus
	  then
	    failwith "complex initializer specification not allowed in - code"
	  else*)
	    initialiser old_metas table minus ini)
  | Ast0.UnInit(stg,ty,id,attr,sem) ->
      typeC old_metas table minus ty; ident context old_metas table minus id
  | Ast0.FunProto(fi,name,lp1,params,va,rp1,sem) ->
      ident FN old_metas table minus name;
      List.iter (fninfo old_metas table minus) fi;
      parameter_list old_metas table minus params
  | Ast0.MacroDecl(stg,name,lp,args,rp,sem) ->
      ident GLOBAL old_metas table minus name;
      dots (expression ID old_metas table minus) args
  | Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
      ident GLOBAL old_metas table minus name;
      dots (expression ID old_metas table minus) args;
      (match Ast0.unwrap ini with
	Ast0.InitExpr exp -> expression ID old_metas table minus exp
      |	_ -> initialiser old_metas table minus ini)
  | Ast0.TyDecl(ty,sem) -> typeC old_metas table minus ty
  | Ast0.Typedef(stg,ty,id,sem) ->
      typeC old_metas table minus ty;
      typeC old_metas table minus id
  | Ast0.DisjDecl(_,decls,_,_)
  | Ast0.ConjDecl(_,decls,_,_) ->
      List.iter (declaration ID old_metas table minus) decls
  | Ast0.OptDecl(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and field context old_metas table minus d =
  match Ast0.unwrap d with
    Ast0.MetaField(name,_,_) ->
      check_table table minus name
  | Ast0.MetaFieldList(name,len,_,_) ->
      check_table table minus name;
      check_len table minus len
  | Ast0.Field(ty,id,bf,sem) ->
      typeC old_metas table minus ty;
      Common.do_option (ident context old_metas table minus) id;
      let bitfield (c, e) = expression context old_metas table minus e in
      Common.do_option bitfield bf
  | Ast0.DisjField(_,decls,_,_)
  | Ast0.ConjField(_,decls,_,_) ->
      List.iter (field ID old_metas table minus) decls
  | Ast0.OptField(_) ->
      failwith "unexpected code"
  | Ast0.Fdots(_,Some (_,_,x)) -> field ID old_metas table minus x
  | Ast0.Fdots(_,None) -> ()

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser old_metas table minus ini =
  match Ast0.unwrap ini with
    Ast0.MetaInit(name,_,_) ->
      check_table table minus name
  | Ast0.MetaInitList(name,len,_,_) ->
      check_table table minus name;
      check_len table minus len
  | Ast0.AsInit(ini,asini) -> failwith "not generated yet"
  | Ast0.InitExpr(exp) -> expression ID old_metas table minus exp
  | Ast0.InitList(lb,initlist,rb,ordered) ->
      dots (initialiser old_metas table minus) initlist
  | Ast0.InitGccExt(designators,eq,ini) ->
      List.iter (designator old_metas table minus) designators;
      initialiser old_metas table minus ini
  | Ast0.InitGccName(name,eq,ini) ->
      ident FIELD old_metas table minus name;
      initialiser old_metas table minus ini
  | Ast0.Idots(_,Some (_,_,x)) -> initialiser old_metas table minus x
  | Ast0.OptIni(_) ->
      failwith "unexpected code"
  | _ -> () (* no metavariable subterms *)

and designator old_metas table minus = function
    Ast0.DesignatorField(dot,id) ->
      ident FIELD old_metas table minus id
  | Ast0.DesignatorIndex(lb,exp,rb) ->
      expression ID old_metas table minus exp
  | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
      expression ID old_metas table minus min;
      expression ID old_metas table minus max

and initialiser_list old_metas table minus =
  dots (initialiser old_metas table minus)

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef old_metas table minus param =
  match Ast0.unwrap param with
    Ast0.Param(ty,id) ->
      get_opt (ident ID old_metas table minus) id;
      typeC old_metas table minus ty
  | Ast0.MetaParam(name,_,_) ->
      check_table table minus name
  | Ast0.MetaParamList(name,len,_,_) ->
      check_table table minus name;
      check_len table minus len
  | _ -> () (* no metavariable subterms *)

and parameter_list old_metas table minus =
  dots (parameterTypeDef old_metas table minus)

(* --------------------------------------------------------------------- *)
(* String fragment *)

and string_fragment old_metas table minus e =
  match Ast0.unwrap e with
    Ast0.ConstantFragment(str) -> ()
  | Ast0.FormatFragment(pct,fmt) ->
      string_format old_metas table minus fmt
  | Ast0.Strdots dots -> ()
  | Ast0.MetaFormatList(pct,name,_,len) ->
      check_table table minus name;
      check_len table minus len

and string_format old_metas table minus e =
  match Ast0.unwrap e with
    Ast0.ConstantFormat(str) -> ()
  | Ast0.MetaFormat(name,_) ->
      check_table table minus name

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and statement old_metas table minus s =
  match Ast0.unwrap s with
    Ast0.Decl(_,decl) -> declaration ID old_metas table minus decl
  | Ast0.Seq(lbrace,body,rbrace) -> dots (statement old_metas table minus) body
  | Ast0.ExprStatement(exp,sem) ->
      get_opt (expression ID old_metas table minus) exp
  | Ast0.IfThen(iff,lp,exp,rp,branch,_) ->
      expression ID old_metas table minus exp;
      statement old_metas table minus branch
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,_) ->
      expression ID old_metas table minus exp;
      statement old_metas table minus branch1;
      statement old_metas table minus branch2
  | Ast0.While(wh,lp,exp,rp,body,_) ->
      expression ID old_metas table minus exp;
      statement old_metas table minus body
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      statement old_metas table minus body;
      expression ID old_metas table minus exp
  | Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,_) ->
      (match Ast0.unwrap first with
	Ast0.ForExp(exp1,sem1) ->
	  get_opt (expression ID old_metas table minus) exp1
      |	Ast0.ForDecl (_,decl) ->
	  declaration ID old_metas table minus decl);
      get_opt (expression ID old_metas table minus) exp2;
      get_opt (expression ID old_metas table minus) exp3;
      statement old_metas table minus body
  | Ast0.Iterator(nm,lp,args,rp,body,_) ->
      ident GLOBAL old_metas table minus nm;
      dots (expression ID old_metas table minus) args;
      statement old_metas table minus body
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
      expression ID old_metas table minus exp;
      dots (statement old_metas table minus) decls;
      dots (case_line old_metas table minus) cases
  | Ast0.ReturnExpr(ret,exp,sem) -> expression ID old_metas table minus exp
  | Ast0.Exec(exec,lang,code,sem) ->
      dots (exec_code ID old_metas table minus) code
  | Ast0.MetaStmt(name,_,_) ->     check_table table minus name
  | Ast0.MetaStmtList(name,len,_,_) ->
      check_table table minus name;
      check_len table minus len
  | Ast0.AsStmt(stm,asstm) -> failwith "not generated yet"
  | Ast0.Exp(exp) -> expression ID old_metas table minus exp
  | Ast0.TopExp(exp) -> expression ID old_metas table minus exp
  | Ast0.Ty(ty) -> typeC old_metas table minus ty
  | Ast0.TopId(id) -> ident ID old_metas table minus id
  | Ast0.TopInit(init) -> initialiser old_metas table minus init
  | Ast0.Disj(_,rule_elem_dots_list,_,_)
  | Ast0.Conj(_,rule_elem_dots_list,_,_) ->
      List.iter (dots (statement old_metas table minus)) rule_elem_dots_list
  | Ast0.Nest(_,rule_elem_dots,_,w,_) ->
      dots (statement old_metas table minus) rule_elem_dots;
      List.iter (whencode (dots (statement old_metas table minus))
		   (statement old_metas table minus)
		   (expression ID old_metas table minus))
	w
  | Ast0.Dots(_,x) ->
      List.iter
	(whencode (dots (statement old_metas table minus))
	   (statement old_metas table minus)
	   (expression ID old_metas table minus)) x
  | Ast0.FunDecl(_,fi,name,lp,params,va,rp,lbrace,body,rbrace,_) ->
      ident FN old_metas table minus name;
      List.iter (fninfo old_metas table minus) fi;
      parameter_list old_metas table minus params;
      dots (statement old_metas table minus) body
  | Ast0.Include(inc,s) -> () (* no metavariables possible *)
  | Ast0.MetaInclude(inc,s) -> expression ID old_metas table minus s
  | Ast0.Undef(def,id) ->
      ident GLOBAL old_metas table minus id
  | Ast0.Define(def,id,params,body) ->
      ident GLOBAL old_metas table minus id;
      define_parameters old_metas table minus params;
      dots (statement old_metas table minus) body
  | Ast0.Pragma(prg,id,body) ->
      ident GLOBAL old_metas table minus id;
      pragmainfo old_metas table minus body
  | Ast0.Label(i,_) -> ident ID old_metas table minus i
  | Ast0.Goto(_,i,_) -> ident ID old_metas table minus i
  | _ -> () (* no metavariable subterms *)

and pragmainfo old_metas table minus pi =
  match Ast0.unwrap pi with
      Ast0.PragmaTuple(lp,args,rp) ->
	dots (expression ID old_metas table minus) args
    | Ast0.PragmaIdList(ids) -> dots (ident GLOBAL old_metas table minus) ids
    | Ast0.PragmaDots (dots) -> ()

and define_param old_metas table minus p =
  match Ast0.unwrap p with
    Ast0.DParam(id) -> ident GLOBAL old_metas table minus id
  | Ast0.MetaDParamList(name,len,_,_) ->
      check_table table minus name;
      check_len table minus len
  | Ast0.DPComma(_) | Ast0.DPdots(_) ->
      () (* no metavariable subterms *)
  | Ast0.OptDParam(dp)    -> define_param old_metas table minus dp

and define_parameters old_metas table minus x =
  match Ast0.unwrap x with
    Ast0.NoParams -> ()
  | Ast0.DParams(lp,dp,rp) -> dots (define_param old_metas table minus) dp

and fninfo old_metas table minus = function
    Ast0.FStorage(stg) -> ()
  | Ast0.FType(ty) -> typeC old_metas table minus ty
  | Ast0.FInline(inline) -> ()
  | Ast0.FAttr(attr) -> ()

and whencode notfn alwaysfn expression = function
    Ast0.WhenNot (_,_,a) -> notfn a
  | Ast0.WhenAlways (_,_,a) -> alwaysfn a
  | Ast0.WhenModifier _ -> ()
  | Ast0.WhenNotTrue (_,_,a) -> expression a
  | Ast0.WhenNotFalse (_,_,a) -> expression a

and case_line old_metas table minus c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) ->
      dots (statement old_metas table minus) code
  | Ast0.Case(case,exp,colon,code) ->
      expression GLOBAL old_metas table minus exp;
      dots (statement old_metas table minus) code
  | Ast0.DisjCase(_,case_lines,_,_) ->
      List.iter (case_line old_metas table minus) case_lines
  | Ast0.OptCase(case) -> failwith "unexpected code"

and exec_code context old_metas table minus e =
  match Ast0.unwrap e with
    Ast0.ExecEval(colon,id) -> expression context old_metas table minus id
  | Ast0.ExecToken(tok) -> ()
  | Ast0.ExecDots(dots) -> ()

(* --------------------------------------------------------------------- *)
(* Rules *)

let top_level old_metas table minus t =
  match Ast0.unwrap t with
    Ast0.NONDECL(stmt) -> statement old_metas table minus stmt
  | Ast0.CODE(stmt_dots) | Ast0.TOPCODE(stmt_dots) ->
      dots (statement old_metas table minus) stmt_dots
  | Ast0.ERRORWORDS(exps) ->
      List.iter (expression FN old_metas table minus) exps
  | _ -> () (* no metavariables possible *)

let rule old_metas table minus rules =
  List.iter (top_level old_metas table minus) rules

(* --------------------------------------------------------------------- *)

let positions rname table rules =
  let do_tynames var =
    let tynames = Ast0.meta_pos_constraint_names var in
    List.iter
      (function name ->
	(* only needed if the name is a local variable, not an inherited one *)
	if fst name = rname
	then (find_loop table name) := true)
      tynames in
  let rec rmcode x = (* needed for type inference, nonpolymorphic *)
    List.iter
      (function var ->
	let name = Ast0.meta_pos_name var in
	(find_loop table (Ast0.unwrap_mcode name)) := true;
	rmcode name;
	do_tynames var)
      (Ast0.get_pos x) in
  let mcode x =
    List.iter
      (function var ->
	let name = Ast0.meta_pos_name var in
	(find_loop table (Ast0.unwrap_mcode name)) := true;
	rmcode name;
	do_tynames var)
      (Ast0.get_pos x) in
  let option_default = () in
  let bind x y = () in
  let donothing r k e = k e in
  let fn =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing in

  List.iter fn.VT0.combiner_rec_top_level rules

let dup_positions rules =
  let mcode x =
    List.concat
      (List.map
	 (function
	     Ast0.MetaPosTag(Ast0.MetaPos(name,constraints,_)) ->
	       [Ast0.unwrap_mcode name]
	   | _ -> [])
	 (Ast0.get_pos x)) in
  let option_default = [] in
  let bind x y = x@y in

  (* Case for everything that has a disj.
     Note, no positions on ( | ) of a disjunction, so no need to recurse on
     these. *)

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.DisjExpr(_,explist,_,_)
    | Ast0.ConjExpr(_,explist,_,_) ->
	List.fold_left Common.union_set option_default
	  (List.map r.VT0.combiner_rec_expression explist)
    | _ -> k e in

  let typeC r k e = (* not sure relevant because "only after iso" *)
    match Ast0.unwrap e with
      Ast0.DisjType(_,types,_,_)
    | Ast0.ConjType(_,types,_,_) ->
	List.fold_left Common.union_set option_default
	  (List.map r.VT0.combiner_rec_typeC types)
    | _ -> k e in

  let declaration r k e =
    match Ast0.unwrap e with
      Ast0.DisjDecl(_,decls,_,_)
    | Ast0.ConjDecl(_,decls,_,_) ->
	List.fold_left Common.union_set option_default
	  (List.map r.VT0.combiner_rec_declaration decls)
    | _ -> k e in

  let field r k e =
    match Ast0.unwrap e with
      Ast0.DisjField(_,decls,_,_)
    | Ast0.ConjField(_,decls,_,_) ->
	List.fold_left Common.union_set option_default
	  (List.map r.VT0.combiner_rec_field decls)
    | _ -> k e in

  let statement r k e =
    match Ast0.unwrap e with
      Ast0.Disj(_,stmts,_,_)
    | Ast0.Conj(_,stmts,_,_) ->
	List.fold_left Common.union_set option_default
	  (List.map r.VT0.combiner_rec_statement_dots stmts)
    | _ -> k e in

  let donothing r k e = k e in
  let fn =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing expression donothing donothing typeC donothing
      donothing declaration field statement
      donothing donothing donothing donothing in

  let res =
    List.sort compare
      (List.fold_left Common.union_set option_default
	 (List.map fn.VT0.combiner_rec_top_level rules)) in
  let rec loop = function
      [] | [_] -> ()
    | ((rule,name) as x)::y::_ when x = y ->
	failwith
	  (Printf.sprintf "duplicate use of %s.%s" rule name)
    | _::xs -> loop xs in
  loop res

(* --------------------------------------------------------------------- *)

let make_table l =
  let table =
    (Hashtbl.create(List.length l) :
       (Ast.meta_name, bool ref) Hashtbl.t) in
  List.iter
    (function x -> Hashtbl.add table (Ast.get_meta_name x) (ref false)) l;
  table

let add_to_fresh_table l =
  List.iter
    (function x ->
      let name = Ast.get_meta_name x in Hashtbl.replace fresh_table name ())
    l

let check_all_marked rname err table after_err =
  Hashtbl.iter
    (function name ->
      function (cell) ->
	if not (!cell)
	then
	  let (_,name) = name in
	  warning
	    (Printf.sprintf "%s: %s %s not used %s" rname err name after_err))
    table

let check_both_marked rname required optional =
  Hashtbl.iter
    (fun name ocell ->
      if !ocell
      then
	let rcell = Hashtbl.find required name in
	if not !rcell
	then
	  failwith
	    (Printf.sprintf "error: %s: %s appears only in + code" rname
	       (snd name)))
    optional

let check_meta rname old_metas inherited_metavars metavars minus plus =
  let old_metas =
    List.map (function (_,x) -> x) (List.map Ast.get_meta_name old_metas) in
  let (fresh,other) =
    List.partition (function Ast.MetaFreshIdDecl(_,_) -> true | _ -> false)
      metavars in
  let (err,other) =
    List.partition (function Ast.MetaErrDecl(_,_) -> true | _ -> false)
      other in
  let (ierr,iother) =
    List.partition (function Ast.MetaErrDecl(_,_) -> true | _ -> false)
      inherited_metavars in
  let frsh_table = make_table fresh in
  let err_table = make_table (err@ierr) in
  let other_table = make_table other in
  let plus_table = make_table other in
  let iother_table = make_table iother in

  add_to_fresh_table fresh;
  rule old_metas [iother_table;other_table;err_table] true minus;
  positions rname [iother_table;other_table] minus;
  dup_positions minus;
  check_all_marked rname "metavariable" other_table "in the - or context code";
  rule old_metas [iother_table;plus_table;frsh_table;err_table] false plus;
  check_all_marked rname "inherited metavariable" iother_table
    "in the -, +, or context code";
  check_all_marked rname "metavariable" frsh_table "in the + code";
  check_all_marked rname "error metavariable" err_table "";
  check_both_marked rname other_table plus_table;
  Stdcompat.Hashtbl.reset fresh_table
