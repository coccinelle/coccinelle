(* This phase sets the safe_for_multi_decls field, which is normally false,
to true for transformations on declarations where the only change is on the
declared variable.  This is the only kind of change on such a declaration
that can safely be done without splitting the declaration. *)

module Ast = Ast_cocci
module V = Visitor_ast

let mcode _ (_,_,kind,_) =
  match kind with
    Ast.MINUS(_,_,_,_) -> true
  | Ast.PLUS _ -> failwith "not possible"
  | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING)

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let do_nothing r k e = k e in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,rp) ->
	bind (mcode r ((),(),bef,[])) res
    | Ast.Decl(bef,_,decl) -> bind (mcode r ((),(),bef,[])) res
    | _ -> res in
  let init r k i =
    let res = k i in
    match Ast.unwrap i with
      Ast.StrInitList(allminus,_,_,_,_) -> allminus or res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing init do_nothing
      do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_fullType

let decl r k e =
  let e = k e in
  match Ast.unwrap e with
    Ast.Init(stg,ty,_,_,_,sem)
  | Ast.UnInit(stg,ty,_,sem) ->
      let stg_modif =
	match stg with
	  Some stg -> mcode () stg
	| None -> false in
      let ft_modif = contains_modif ty in
      let sem_modif = mcode () sem in
      if not(stg_modif or ft_modif or sem_modif)
      then {e with Ast.safe_for_multi_decls = true}
      else e
  | _ -> e
let mcode e = e
let donothing r k e = k e

let process =
  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing
      donothing donothing donothing donothing
      donothing donothing decl donothing
      donothing donothing donothing donothing in
  List.map fn.V.rebuilder_top_level

let safe_for_multi_decls rules =
  List.map
    (function (mv,r) ->
      (mv,
       match r with
        Ast.ScriptRule _
      | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> r
      | Ast.CocciRule (nm, rule_info, r, is_exp,ruletype) ->
	  Ast.CocciRule(nm, rule_info,process r,is_exp,ruletype)))
    rules
