(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* remove things that can't happen due to false dependencies *)

module Ast = Ast_cocci
module V = Visitor_ast

let dropped = ref []
let drop_stack = ref []

let set_failed = function
    cell::_ -> cell := true
  | _ -> failwith "bad drop_stack"

let check_failed = function
    cell::rest ->
      drop_stack := rest;
      !cell
  | _ -> failwith "bad drop_stack"

let run_loop k l k_one k_many =
  let res =
    List.fold_left
      (fun prev cur ->
	drop_stack := (ref false) :: !drop_stack;
	let cur = k cur in
	if check_failed !drop_stack
	then prev
	else cur :: prev)
      [] l in
  (if res = []
  then set_failed !drop_stack);
  match List.rev res with
    [x] -> k_one x
  | x -> k_many x

let get_rule mc = fst (Ast.unwrap_mcode mc)

let check_pos l =
  let undefined =
    List.exists
      (function
	  Ast.MetaPos(name,_,_,_,_) -> List.mem (get_rule name) !dropped
	| _ -> false)
      l in
  if undefined
  then set_failed !drop_stack

let mcode mc =
    let (x, _, _, pos) = mc in
    check_pos pos;
    mc

let listlen l =
  match l with
    Ast.MetaListLen(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack)
  | _ -> ()

let ident r k i =
  match Ast.unwrap i with
    Ast.MetaId(name,_,_,_) | Ast.MetaFunc(name,_,_,_)
  | Ast.MetaLocalFunc(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k i
  | Ast.DisjId(id_list) ->
      run_loop k id_list (fun x -> x) (fun x -> Ast.rewrap i (Ast.DisjId x))
  | Ast.OptIdent id -> i
  | _ -> k i

let expression r k e =
  match Ast.unwrap e with
    Ast.MetaErr(name,_,_,_) | Ast.MetaExpr(name,_,_,_,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k e
  | Ast.MetaExprList(name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k e
  | Ast.DisjExpr(exp_list) ->
      run_loop k exp_list (fun x -> x) (fun x -> Ast.rewrap e (Ast.DisjExpr x))
  | Ast.OptExp(exp) -> e
  | _ -> k e

let string_fragment r k e =
  match Ast.unwrap e with
    Ast.MetaFormatList(pct,name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k e
  | _ -> k e

let string_format r k e =
  match Ast.unwrap e with
    Ast.MetaFormat(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k e
  | _ -> k e

let assignOp r k op =
  match Ast.unwrap op with
    Ast.MetaAssign(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k op
  | _ -> k op

let binaryOp r k op =
  match Ast.unwrap op with
    Ast.MetaBinary(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k op
  | _ -> k op

let fullType r k ft =
  match Ast.unwrap ft with
    Ast.DisjType(decls) ->
      run_loop k decls (fun x -> x) (fun x -> Ast.rewrap ft (Ast.DisjType x))
  | Ast.OptType(ty) -> ft
  | _ -> k ft

let typeC r k ty =
  match Ast.unwrap ty with
    Ast.MetaType(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k ty
  | _ -> k ty

let initialiser r k i =
  match Ast.unwrap i with
    Ast.MetaInit(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k i
  | Ast.MetaInitList(name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k i
  | Ast.OptIni(ini) -> i
  | _ -> k i

let parameterTypeDef r k p =
  match Ast.unwrap p with
    Ast.MetaParam(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k p
  | Ast.MetaParamList(name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k p
  | Ast.OptParam(param) -> p
  | _ -> k p

let define_param r k param =
  match Ast.unwrap param with
  | Ast.MetaDParamList(name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k param
  | Ast.OptDParam(dp) -> param
  | _ -> k param

let declaration r k d =
  match Ast.unwrap d with
    Ast.MetaDecl(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k d
  | Ast.DisjDecl(decls) ->
      run_loop k decls (fun x -> x) (fun x -> Ast.rewrap d (Ast.DisjDecl x))
  | Ast.OptDecl(decl) -> d
  | _ -> k d

let field r k d =
  match Ast.unwrap d with
    Ast.MetaField(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k d
  | Ast.MetaFieldList(name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k d
  | _ -> k d

let ann_field r k d =
  match Ast.unwrap d with
  | Ast.DisjField(decls) ->
      run_loop k decls (fun x -> x) (fun x -> Ast.rewrap d (Ast.DisjField x))
  | Ast.OptField(decl) -> d
  | _ -> k d

let rule_elem r k re =
  match Ast.unwrap re with
  | Ast.MetaRuleElem(name,_,_,_)
  | Ast.MetaStmt(name,_,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k re
  | Ast.MetaStmtList(name,len,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      listlen len;
      k re
  | Ast.DisjRuleElem(res) ->
      run_loop k res (fun x -> x) (fun x -> Ast.rewrap re (Ast.DisjRuleElem x))
  | _ -> k re

let statement r k s =
  match Ast.unwrap s with
    Ast.Disj(stmt_dots_list) ->
      (* type change, so don't optimize for 1 element case *)
      let cont x = Ast.rewrap s (Ast.Disj x) in
      run_loop r.V.rebuilder_statement_dots stmt_dots_list
	(fun x -> cont [x]) cont
  | Ast.Nest(starter,stmt_dots,ender,whn,false,a,b) ->
      drop_stack := (ref false) :: !drop_stack;
      let s = k s in
      if check_failed !drop_stack
      then
	(match Ast.unwrap s with
	  Ast.Nest(starter,stmt_dots,ender,whn,false,a,b) ->
	    let dots = Ast.rewrap_mcode starter "..." in
	    Ast.rewrap s (Ast.Dots(dots,whn,a,b))
	| _ -> failwith "not possible")
      else s
  | Ast.OptStm(_) -> s
  | _ -> k s

let attr_arg r k a =
  match Ast.unwrap a with
    Ast.MetaAttr(name,_,_,_) ->
      (if List.mem (get_rule name) !dropped
      then set_failed !drop_stack);
      k a
  | _ -> k a

let do_cleanup =
  let donothing r k e = k e in
  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing
    donothing donothing (* dots *)
    ident expression string_fragment string_format assignOp
    binaryOp fullType typeC initialiser parameterTypeDef define_param
    declaration donothing field ann_field donothing
    rule_elem statement donothing donothing attr_arg donothing donothing

let cleanup_rules rules d =
  dropped := d;
  let rules =
    List.fold_left
      (fun prev (mv,r) ->
	match r with
          Ast.ScriptRule _
	| Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> (mv,r)::prev
        | Ast.CocciRule (nm, rule_info, r, is_exp, ruletype) ->
	    drop_stack := [ref false];
	    let code = List.map do_cleanup.V.rebuilder_top_level r in
	    if !(List.hd !drop_stack)
	    then
	      begin
		dropped := nm :: !dropped;
		prev
	      end
	    else (mv,Ast.CocciRule(nm,rule_info,code,is_exp,ruletype))::prev)
      [] rules in
  (List.rev rules,!dropped)
