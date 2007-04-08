(* For minus fragment, checks that all of the identifier metavariables that
are used are not declared as fresh, and check that all declared variables
are used.  For plus fragment, just check that the variables declared as
fresh are used.  What is the issue about error variables? (don't remember) *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* all fresh identifiers *)
let fresh_table = (Hashtbl.create(50) : ((string * string), unit) Hashtbl.t)

let warning s = Printf.fprintf stderr "warning: %s\n" s

(* --------------------------------------------------------------------- *)

let find_loop table name =
  let rec loop = function
      [] -> raise Not_found
    | x::xs -> (try Hashtbl.find x name with Not_found -> loop xs) in
  loop table

let check_table table minus ((name,_,info,_) : (string * string) Ast0.mcode) =
  let rl = info.Ast0.line_start in
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

let dots fn d =
  match Ast0.unwrap d with
    Ast0.DOTS(x) -> List.iter fn x
  | Ast0.CIRCLES(x) -> List.iter fn x
  | Ast0.STARS(x) -> List.iter fn x

(* --------------------------------------------------------------------- *)
(* Identifier *)

type context = ID | FIELD | FN | GLOBAL

(* heuristic for distinguishing ifdef variables from undeclared metavariables*)
let is_ifdef name =
  String.length name > 2 && String.uppercase name = name

let ident context table minus i =
  match Ast0.unwrap i with
    Ast0.Id((name,_,info,_) : string Ast0.mcode) ->
      let rl = info.Ast0.line_start in
      (match context with
	ID ->
	  if not (is_ifdef name) && minus
	  then
	    warning
	      (Printf.sprintf "line %d: should %s be a metavariable?" rl name)
      | _ -> ())	
  | Ast0.MetaId(name,_) -> check_table table minus name
  | Ast0.MetaFunc(name,_) ->
      if minus then check_table table minus name
  | Ast0.MetaLocalFunc(name,_) ->
      if minus then check_table table minus name
  | Ast0.OptIdent(_) | Ast0.UniqueIdent(_) | Ast0.MultiIdent(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression context table minus e =
  match Ast0.unwrap e with
    Ast0.Ident(id) -> ident context table minus id
  | Ast0.FunCall(fn,lp,args,rp) ->
      expression FN table minus fn; dots (expression ID table minus) args
  | Ast0.Assignment(left,op,right) ->
      expression context table minus left ; expression ID table minus right
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      expression ID table minus exp1;
      get_opt (expression ID table minus) exp2;
      expression ID table minus exp3
  | Ast0.Postfix(exp,op) ->
      expression ID table minus exp
  | Ast0.Infix(exp,op) ->
      expression ID table minus exp
  | Ast0.Unary(exp,op) ->
      expression ID table minus exp
  | Ast0.Binary(left,op,right) ->
      expression ID table minus left ; expression ID table minus right
  | Ast0.Paren(lp,exp,rp) ->
      expression ID table minus exp
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      expression ID table minus exp1; expression ID table minus exp2
  | Ast0.RecordAccess(exp,pt,field) ->
      expression ID table minus exp; ident FIELD table minus field
  | Ast0.RecordPtAccess(exp,ar,field) ->
      expression ID table minus exp; ident FIELD table minus field
  | Ast0.Cast(lp,ty,rp,exp) ->
      typeC table minus ty; expression ID table minus exp
  | Ast0.SizeOfExpr(szf,exp) -> expression ID table minus exp
  | Ast0.SizeOfType(szf,lp,ty,rp) -> typeC table minus ty
  | Ast0.TypeExp(ty) -> typeC table minus ty
  | Ast0.MetaConst(name,ty,_) ->
      if minus then check_table table minus name
  | Ast0.MetaExpr(name,ty,_)  ->
      if minus then check_table table minus name
  | Ast0.MetaErr(name,_)      ->
      check_table table minus name
  | Ast0.MetaExprList(name,_) ->
      if minus then check_table table minus name
  | Ast0.DisjExpr(_,exps,_,_) ->
      List.iter (expression ID table minus) exps
  | Ast0.NestExpr(_,exp_dots,_,w) ->
      dots (expression ID table minus) exp_dots;
      get_opt (expression ID table minus) w
  | Ast0.Edots(_,Some x) | Ast0.Ecircles(_,Some x) | Ast0.Estars(_,Some x) ->
      expression ID table minus x
  | _ -> () (* no metavariable subterms *)

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC table minus t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) -> typeC table minus ty
  | Ast0.Pointer(ty,star) -> typeC table minus ty
  | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      typeC table minus ty;
      parameter_list table minus params
  | Ast0.FunctionType(ty,lp1,params,rp1) ->
      get_opt (typeC table minus) ty;
      parameter_list table minus params
  | Ast0.Array(ty,lb,size,rb) ->
      typeC table minus ty; get_opt (expression ID table minus) size
  | Ast0.MetaType(name,_) -> if minus then check_table table minus name
  | Ast0.DisjType(_,types,_,_) ->
      List.iter (typeC table minus) types
  | Ast0.StructUnionName(su,id) -> ident GLOBAL table minus id
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      typeC table minus ty;
      dots (declaration GLOBAL table minus) decls
  | Ast0.OptType(ty) | Ast0.UniqueType(ty) | Ast0.MultiType(ty) ->
      failwith "unexpected code"
  | _ -> () (* no metavariable subterms *)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and declaration context table minus d =
  match Ast0.unwrap d with
    Ast0.Init(stg,ty,id,eq,ini,sem) ->
      (match Ast0.unwrap ini with
	Ast0.InitExpr exp ->
	  typeC table minus ty;
	  ident context table minus id; expression ID table minus exp
      |	_ ->
	  (*
	  if minus
	  then
	    failwith "complex initializer specification not allowed in - code"
	  else*)
	    (typeC table minus ty;
	     ident context table minus id; initialiser table minus ini))
  | Ast0.UnInit(stg,ty,id,sem) ->
      typeC table minus ty; ident context table minus id
  | Ast0.MacroDecl(name,lp,args,rp,sem) ->
      dots (expression ID table minus) args
  | Ast0.TyDecl(ty,sem) -> typeC table minus ty
  | Ast0.DisjDecl(_,decls,_,_) ->
      List.iter (declaration ID table minus) decls
  | Ast0.Ddots(_,Some x) -> declaration ID table minus x
  | Ast0.Ddots(_,None) -> ()
  | Ast0.OptDecl(_) | Ast0.UniqueDecl(_) | Ast0.MultiDecl(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser table minus ini =
  match Ast0.unwrap ini with
    Ast0.InitExpr(exp) -> expression ID table minus exp
  | Ast0.InitList(lb,initlist,rb) -> dots (initialiser table minus) initlist
  | Ast0.InitGccDotName(dot,name,eq,ini) ->
      ident FIELD table minus name; initialiser table minus ini
  | Ast0.InitGccName(name,eq,ini) ->
      ident FIELD table minus name; initialiser table minus ini
  | Ast0.InitGccIndex(lb,exp,rb,eq,ini) ->
      expression ID table minus exp; initialiser table minus ini
  | Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
      expression ID table minus exp1; expression ID table minus exp2;
      initialiser table minus ini
  | Ast0.Idots(_,Some x) -> initialiser table minus x
  | Ast0.OptIni(_) | Ast0.UniqueIni(_) | Ast0.MultiIni(_) ->
      failwith "unexpected code"
  | _ -> () (* no metavariable subterms *)
	
and initialiser_list table minus = dots (initialiser table minus)

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef table minus param =
  match Ast0.unwrap param with
    Ast0.Param(ty,id) ->
      get_opt (ident ID table minus) id; typeC table minus ty
  | Ast0.MetaParam(name,_) ->
      if minus then check_table table minus name
  | Ast0.MetaParamList(name,_) ->
      if minus then check_table table minus name
  | _ -> () (* no metavariable subterms *)

and parameter_list table minus = dots (parameterTypeDef table minus)

(* --------------------------------------------------------------------- *)
(* CPP code *)

let rec define_body table minus s =
  match Ast0.unwrap s with
    Ast0.DMetaId(name,_) -> check_table table minus name
  | Ast0.DStm(stmtdots) -> dots (statement table minus) stmtdots

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and statement table minus s =
  match Ast0.unwrap s with
    Ast0.Decl(_,decl) -> declaration ID table minus decl
  | Ast0.Seq(lbrace,body,rbrace) -> dots (statement table minus) body
  | Ast0.ExprStatement(exp,sem) -> expression ID table minus exp
  | Ast0.IfThen(iff,lp,exp,rp,branch,_) ->
      expression ID table minus exp; statement table minus branch
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,_) ->
      expression ID table minus exp; statement table minus branch1;
      statement table minus branch2
  | Ast0.While(wh,lp,exp,rp,body,_) ->
      expression ID table minus exp; statement table minus body
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      statement table minus body; expression ID table minus exp
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body,_) ->
      get_opt (expression ID table minus) exp1;
      get_opt (expression ID table minus) exp2;
      get_opt (expression ID table minus) exp3;
      statement table minus body
  | Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
      expression ID table minus exp; dots (case_line table minus) cases
  | Ast0.ReturnExpr(ret,exp,sem) -> expression ID table minus exp
  | Ast0.MetaStmt(name,_) -> if minus then check_table table minus name
  | Ast0.MetaStmtList(name,_) ->
      if minus then check_table table minus name
  | Ast0.Exp(exp) -> expression ID table minus exp
  | Ast0.Ty(ty) -> typeC table minus ty
  | Ast0.Disj(_,rule_elem_dots_list,_,_) ->
      List.iter (dots (statement table minus)) rule_elem_dots_list
  | Ast0.Nest(_,rule_elem_dots,_,w) ->
      dots (statement table minus) rule_elem_dots;
      get_opt (dots (statement table minus)) w
  | Ast0.Dots(_,x) | Ast0.Circles(_,x) | Ast0.Stars(_,x) ->
      whencode (dots (statement table minus)) (statement table minus) x
  | Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
      ident FN table minus name;
      List.iter
	(function
	    Ast0.Storage(stg) -> ()
	  | Ast0.Type(ty) -> typeC table minus ty
	  | Ast0.Inline(inline) -> ()
	  | Ast0.Init(init) -> ())
	fninfo;
      parameter_list table minus params;
      dots (statement table minus) body
  | Ast0.Include(inc,s) -> () (* no metavariables possible *)
  | Ast0.Define(def,id,_,body) ->
      ident GLOBAL table minus id; define_body table minus body
  | _ -> () (* no metavariable subterms *)

and whencode notfn alwaysfn = function
    Ast0.NoWhen -> ()
  | Ast0.WhenNot a -> notfn a
  | Ast0.WhenAlways a -> alwaysfn a

and case_line table minus c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) ->
      dots (statement table minus) code
  | Ast0.Case(case,exp,colon,code) ->
      dots (statement table minus) code
  | Ast0.OptCase(case) -> failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Rules *)

let top_level table minus t =
  match Ast0.unwrap t with
    Ast0.DECL(stmt) -> statement table minus stmt
  | Ast0.CODE(stmt_dots) -> dots (statement table minus) stmt_dots
  | Ast0.ERRORWORDS(exps) -> List.iter (expression FN table minus) exps
  | _ -> () (* no metavariables possible *)

let rule table minus rules = List.iter (top_level table minus) rules

(* --------------------------------------------------------------------- *)

let make_table l =
  let table =
    (Hashtbl.create(List.length l) :
       ((string * string), bool ref) Hashtbl.t) in
  List.iter
    (function x -> Hashtbl.add table (Ast.get_meta_name x) (ref false)) l;
  table

let add_to_fresh_table l =
  List.iter
    (function x ->
      let name = Ast.get_meta_name x in Hashtbl.replace fresh_table name ())
    l

let check_all_marked err table after_err =
  Hashtbl.iter
    (function name ->
      function (cell) ->
	if not (!cell)
	then
	  let (_,name) = name in
	  warning (Printf.sprintf "%s %s not used %s" err name after_err))
    table

let check_meta metavars minus plus =
  let (fresh,other) =
    List.partition (function Ast.MetaFreshIdDecl(_,_) -> true | _ -> false)
      metavars in
  let (err,other) =
    List.partition (function Ast.MetaErrDecl(_,_) -> true | _ -> false)
      other in
  let fresh_table = make_table fresh in
  let err_table = make_table err in
  let other_table = make_table other in
  add_to_fresh_table fresh;
  rule [other_table;err_table] true minus;
  check_all_marked "metavariable" other_table "in the - or context code";
  rule [fresh_table;err_table] false plus;
  check_all_marked "fresh identifier metavariable" fresh_table "in the + code";
  check_all_marked "error metavariable" err_table ""
