(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module CTL = Ast_ctl

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let mcode(term,_,mcodekind) = (term,mcodekind)

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn = function
    Ast0.DOTS(x) -> Ast.DOTS(List.map fn x)
  | Ast0.CIRCLES(x) -> Ast.CIRCLES(List.map fn x)
  | Ast0.STARS(x) -> Ast.STARS(List.map fn x)

let only_dots l =
  not (List.exists
	(function Ast.Circles(_,_) | Ast.Stars(_,_) -> true | _ -> false) l)

let only_circles l =
  not (List.exists
	(function Ast.Dots(_,_) | Ast.Stars(_,_) -> true | _ -> false) l)

let only_stars l =
  not (List.exists
	(function Ast.Dots(_,_) | Ast.Circles(_,_) -> true | _ -> false) l)


let top_dots l =
  if List.exists (function Ast.Circles(_) -> true | _ -> false) l
  then
    if only_circles l
    then Ast.CIRCLES(l)
    else failwith "inconsistent dots usage"
  else if List.exists (function Ast.Stars(_,_) -> true | _ -> false) l
  then
    if only_stars l
    then Ast.STARS(l)
    else failwith "inconsistent dots usage"
  else
    if only_dots l
    then Ast.DOTS(l)
    else failwith "inconsistent dots usage"

let concat_dots fn = function
    Ast0.DOTS(x) ->
      let l = List.concat(List.map fn x) in
      if only_dots l
      then Ast.DOTS(l)
      else failwith "inconsistent dots usage"
  | Ast0.CIRCLES(x) ->
      let l = List.concat(List.map fn x) in
      if only_circles l
      then Ast.CIRCLES(l)
      else failwith "inconsistent dots usage"
  | Ast0.STARS(x) ->
      let l = List.concat(List.map fn x) in
      if only_stars l
      then Ast.STARS(l)
      else failwith "inconsistent dots usage"

let flat_concat_dots fn = function
    Ast0.DOTS(x) -> List.concat(List.map fn x)
  | Ast0.CIRCLES(x) -> List.concat(List.map fn x)
  | Ast0.STARS(x) -> List.concat(List.map fn x)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec statement = function
    Ast0.Decl(decl) -> CTL.Pred(Ast.Decl(declaration decl))
  | Ast0.Seq(lbrace,body,rbrace) -> 
      let lbrace = mcode lbrace in
      let body = flat_concat_dots statement body in
      let rbrace = mcode rbrace in
      Ast.SeqStart(lbrace)::body@[Ast.SeqEnd(rbrace)]
  | Ast0.ExprStatement(exp,sem) ->
      CTL.Pred(Ast.ExprStatement(expression exp,mcode sem))
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)
      :: statement branch
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)
      :: statement branch1 @ Ast.Else(mcode els) :: statement branch2
  | Ast0.While(wh,lp,exp,rp,body) ->
      Ast.WhileHeader(mcode wh,mcode lp,expression exp,mcode rp)
      :: statement body
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      Ast.Do(mcode d) :: statement body @
      [Ast.WhileTail(mcode wh,mcode lp,expression exp,mcode rp,mcode sem)]
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body) ->
      let fr = mcode fr in
      let lp = mcode lp in
      let exp1 = get_option expression exp1 in
      let sem1 = mcode sem1 in
      let exp2 = get_option expression exp2 in
      let sem2= mcode sem2 in
      let exp3 = get_option expression exp3 in
      let rp = mcode rp in
      let body = statement body in
      Ast.ForHeader(fr,lp,exp1,sem1,exp2,sem2,exp3,rp) :: body
  | Ast0.Return(ret,sem) -> CTL.Pred(Ast.Return(mcode ret,mcode sem))
  | Ast0.ReturnExpr(ret,exp,sem) ->
      CTL.Pred(Ast.ReturnExpr(mcode ret,expression exp,mcode sem))
  | Ast0.MetaStmt(name) -> CTL.Pred(Ast.MetaStmt(mcode name))
  | Ast0.MetaStmtList(name) -> CTL.Pred(Ast.MetaStmtList(mcode name))
  | Ast0.Exp(exp) -> CTL.Pred(Ast.Exp(expression exp))
  | Ast0.Disj(rule_elem_dots_list) ->
      [Ast.Disj(List.map (function x -> concat_dots statement x)
		  rule_elem_dots_list)]
  | Ast0.Nest(rule_elem_dots) ->
      [Ast.Nest(concat_dots statement rule_elem_dots)]
  | Ast0.Dots(dots,whencode)    ->
      let dots = mcode dots in
      let whencode = get_option (concat_dots statement) whencode in
      [Ast.Dots(dots,whencode)]
  | Ast0.Circles(dots,whencode) -> failwith "circles not yet supported"
  | Ast0.Stars(dots,whencode)   -> failwith "stars not yet supported"
  | Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let stg = get_option mcode stg in
      let name = ident name in
      let lp = mcode lp in
      let params = parameter_list params in
      let rp = mcode rp in
      let lbrace = mcode lbrace in
      let body = flat_concat_dots statement body in
      let rbrace = mcode rbrace in
      Ast.FunDecl(stg,name,lp,params,rp) :: Ast.SeqStart(lbrace) :: body @
      [Ast.SeqEnd(rbrace)]
  | Ast0.OptStm(stm) -> failwith "arities not yet supported"
  | Ast0.UniqueStm(stm) -> failwith "arities not yet supported"
  | Ast0.MultiStm(stm) -> failwith "arities not yet supported"
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)

let top_level = function
    Ast0.DECL(decl) -> Ast.DECL(declaration decl)
  | Ast0.INCLUDE(inc,s) -> Ast.INCLUDE(mcode inc,mcode s)
  | Ast0.FILEINFO(old_file,new_file) ->
      Ast.FILEINFO(mcode old_file,mcode new_file)
  | Ast0.FUNCTION(stmt) ->
      Ast.FUNCTION(top_dots(statement stmt))
  | Ast0.CODE(rule_elem_dots) ->
      Ast.CODE(concat_dots statement rule_elem_dots)
  | Ast0.ERRORWORDS(exps) ->
      Ast.ERRORWORDS(List.map expression exps)
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let ast0toast = List.map top_level
