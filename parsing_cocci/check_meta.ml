(* For minus fragment, checks that all of the identifier metavariables that
are used are not declared as fresh, and check that all declared variables
are used.  For plus fragment, just check that the variables declared as
fresh are used.  What is the issue about error variables? (don't remember) *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0

(* all fresh identifiers *)
let fresh_table = (Hashtbl.create(50) : (string, unit) Hashtbl.t)

let warning s = Printf.fprintf stderr "warning: %s\n" s

(* --------------------------------------------------------------------- *)

let find_loop table name =
  let rec loop = function
      [] -> raise Not_found
    | x::xs -> (try Hashtbl.find x name with Not_found -> loop xs) in
  loop table

let check_table table minus ((name,_,info,_) : string Ast0.mcode) =
  let rl = info.Ast0.line_start in
  if minus
  then
    (try (find_loop table name) := true
    with
      Not_found ->
	(try
	  Hashtbl.find fresh_table name;
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
  | Ast0.MetaId(name,inherited) -> check_table table minus name
  | Ast0.MetaFunc(name,inherited) ->
      if minus then check_table table minus name
  | Ast0.MetaLocalFunc(name,inherited) ->
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
  | Ast0.SizeOfExpr(szf,exp) ->
      expression ID table minus exp
  | Ast0.SizeOfType(szf,lp,ty,rp) ->
      typeC table minus ty
  | Ast0.MetaConst(name,ty,inherited) ->
      if minus then check_table table minus name
  | Ast0.MetaExpr(name,ty,inherited)  ->
      if minus then check_table table minus name
  | Ast0.MetaErr(name,inherited)      ->
      check_table table minus name
  | Ast0.MetaExprList(name,inherited) ->
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
  | Ast0.Array(ty,lb,size,rb) ->
      typeC table minus ty; get_opt (expression ID table minus) size
  | Ast0.MetaType(name,inherited) -> if minus then check_table table minus name
  | Ast0.OptType(ty) -> typeC table minus ty
  | Ast0.UniqueType(ty) -> typeC table minus ty
  | Ast0.MultiType(ty) -> typeC table minus ty
  | _ -> () (* no metavariable subterms *)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration context table minus d =
  match Ast0.unwrap d with
    Ast0.Init(ty,id,eq,exp,sem) ->
      typeC table minus ty;
      ident context table minus id; expression ID table minus exp
  | Ast0.UnInit(ty,id,sem) ->
      typeC table minus ty; ident context table minus id
  | Ast0.DisjDecl(_,decls,_,_) ->
      List.iter (declaration ID table minus) decls
  | Ast0.OptDecl(_) | Ast0.UniqueDecl(_) | Ast0.MultiDecl(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Parameter *)

let parameterTypeDef table minus param =
  match Ast0.unwrap param with
    Ast0.Param(id,ty) -> ident ID table minus id; typeC table minus ty
  | Ast0.MetaParam(name,inherited) ->
      if minus then check_table table minus name
  | Ast0.MetaParamList(name,inherited) ->
      if minus then check_table table minus name
  | _ -> () (* no metavariable subterms *)

let parameter_list table minus = dots (parameterTypeDef table minus)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec statement table minus s =
  match Ast0.unwrap s with
    Ast0.Decl(decl) -> declaration ID table minus decl
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
  | Ast0.ReturnExpr(ret,exp,sem) -> expression ID table minus exp
  | Ast0.MetaStmt(name,inherited) -> if minus then check_table table minus name
  | Ast0.MetaStmtList(name,inherited) ->
      if minus then check_table table minus name
  | Ast0.Exp(exp) -> expression ID table minus exp
  | Ast0.Disj(_,rule_elem_dots_list,_,_) ->
      List.iter (dots (statement table minus)) rule_elem_dots_list
  | Ast0.Nest(_,rule_elem_dots,_,w) ->
      dots (statement table minus) rule_elem_dots;
      get_opt (dots (statement table minus)) w
  | Ast0.Dots(_,x) | Ast0.Circles(_,x) | Ast0.Stars(_,x) ->
      whencode (dots (statement table minus)) (statement table minus) x
  | Ast0.FunDecl(stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
      ident FN table minus name;
      get_opt (typeC table minus) ty;
      parameter_list table minus params;
      dots (statement table minus) body
  | _ -> () (* no metavariable subterms *)

and whencode notfn alwaysfn = function
    Ast0.NoWhen -> ()
  | Ast0.WhenNot a -> notfn a
  | Ast0.WhenAlways a -> alwaysfn a

(* --------------------------------------------------------------------- *)
(* Rules *)

let top_level table minus t =
  match Ast0.unwrap t with
    Ast0.DECL(decl) -> declaration GLOBAL table minus decl
  | Ast0.FUNCTION(stmt) -> statement table minus stmt
  | Ast0.CODE(stmt_dots) -> dots (statement table minus) stmt_dots
  | Ast0.ERRORWORDS(exps) -> List.iter (expression FN table minus) exps
  | _ -> () (* no metavariables possible *)

let rule table minus rules = List.iter (top_level table minus) rules

(* --------------------------------------------------------------------- *)
(* determine for each metavar whether it is declared in the current rule or
previously *)

let update_metavars metavars =
  let donothing r k e = k e in
  let mcode x = x in
  let free_mv (name,_,_,_) = not(List.mem name metavars) in

  let ident r k e =
    match Ast0.unwrap e with
      Ast0.MetaId(name,_) ->
	Ast0.rewrap e (Ast0.MetaId(name,free_mv name))
    | Ast0.MetaFunc(name,_) ->
	Ast0.rewrap e (Ast0.MetaFunc(name,free_mv name))
    | Ast0.MetaLocalFunc(name,_) ->
	Ast0.rewrap e (Ast0.MetaLocalFunc(name,free_mv name))
    | _ -> k e in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.MetaConst(name,ty,_) ->
	Ast0.rewrap e (Ast0.MetaConst(name,ty,free_mv name))
    | Ast0.MetaErr(name,_) ->
	Ast0.rewrap e (Ast0.MetaErr(name,free_mv name))
    | Ast0.MetaExpr(name,ty,_) ->
	Ast0.rewrap e (Ast0.MetaExpr(name,ty,free_mv name))
    | Ast0.MetaExprList(name,_) ->
	Ast0.rewrap e (Ast0.MetaExprList(name,free_mv name))
    | _ -> k e in

  let typeC r k e =
    match Ast0.unwrap e with
      Ast0.MetaType(name,_) ->
	Ast0.rewrap e (Ast0.MetaType(name,free_mv name))
    | _ -> k e in

  let param r k e =
    match Ast0.unwrap e with
      Ast0.MetaParam(name,_) ->
	Ast0.rewrap e (Ast0.MetaParam(name,free_mv name))
    | Ast0.MetaParamList(name,_) ->
	Ast0.rewrap e (Ast0.MetaParamList(name,free_mv name))
    | _ -> k e in

  let statement r k e =
    match Ast0.unwrap e with
      Ast0.MetaStmt(name,_) ->
	Ast0.rewrap e (Ast0.MetaStmt(name,free_mv name))
    | Ast0.MetaStmtList(name,_) ->
	Ast0.rewrap e (Ast0.MetaStmtList(name,free_mv name))
    | _ -> k e in

  let fn = V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing
    ident expression typeC param donothing statement donothing in

  fn.V0.rebuilder_top_level

(* --------------------------------------------------------------------- *)

let metavar2name = function
    Ast.MetaIdDecl(arity,name) -> name
  | Ast.MetaFreshIdDecl(arity,name) -> name
  | Ast.MetaTypeDecl(arity,name) -> name
  | Ast.MetaParamDecl(arity,name) -> name
  | Ast.MetaParamListDecl(arity,name) -> name
  | Ast.MetaConstDecl(arity,name) -> name
  | Ast.MetaExpDecl(arity,name) -> name
  | Ast.MetaErrDecl(arity,name) -> name
  | Ast.MetaExpListDecl(arity,name) -> name
  | Ast.MetaStmDecl(arity,name) -> name
  | Ast.MetaStmListDecl(arity,name) -> name
  | Ast.MetaFuncDecl(arity,name) -> name
  | Ast.MetaLocalFuncDecl(arity,name) -> name

let make_table l =
  let table = (Hashtbl.create(List.length l) : (string, bool ref) Hashtbl.t) in
  List.iter
    (function x -> Hashtbl.add table (metavar2name x) (ref false)) l;
  table

let add_to_fresh_table l =
  List.iter
    (function x ->
      let name = metavar2name x in Hashtbl.replace fresh_table name ())
    l

let check_all_marked err table after_err =
  Hashtbl.iter
    (function name ->
      function (cell) ->
	if not (!cell)
	then warning (Printf.sprintf "%s %s not used %s" err name after_err))
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
  check_all_marked "error metavariable" err_table "";
  let names = List.map metavar2name metavars in
  List.map (update_metavars names) minus
