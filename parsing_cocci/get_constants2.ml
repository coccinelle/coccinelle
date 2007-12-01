module Ast = Ast_cocci
module V = Visitor_ast
module TC = Type_cocci

(* Issues:

1.  If a rule X depends on a rule Y (in a positive way), then we can ignore
    the constants in X.

2.  If a rule X contains a metavariable that is not under a disjunction and
    that is inherited from rule Y, then we can ignore the constants in X.

3.  If a rule contains a constant x in + code then subsequent rules that
    have it in - or context should not include it in their list of required
    constants.
*)

(* ----------------------------------------------------------------------- *)
(* This phase collects everything.  One can then filter out what it not
wanted *)

(* True means nothing was found
   False should never drift to the top, it is the neutral element of or
   and an or is never empty *)
type combine =
    And of combine list | Or of combine list | Elem of string | False | True

let interpret strict x =
  let rec loop = function
      Elem x -> x
    | And [x] -> loop x
    | Or [x] -> loop x
    | And l -> String.concat ";" (List.map loop l)
    | Or l -> Printf.sprintf "{%s}" (String.concat "," (List.map loop l))
    | True -> "True"
    | False ->
	if strict
	then failwith "False should not be in the final result"
	else "False" in
  match x with
    True -> None
  | False when strict -> failwith "False should not be in the final result"
  | _ -> Some (loop x)

let combine2c x =
  match interpret false x with
    None -> "None"
  | Some x -> x

let norm = function
    And l -> And (List.sort compare l)
  | Or l  -> Or (List.sort compare l)
  | x -> x

let rec merge l1 l2 =
  match (l1,l2) with
    ([],l2) -> l2
  | (l1,[]) -> l1
  | (x::xs,y::ys) ->
      (match compare x y with
	-1 -> x::(merge xs l2)
      |	0  -> x::(merge xs ys)
      |	1  -> y::(merge l1 ys)
      |	_ -> failwith "not possible")

let rec insert x l = merge [x] l

let build_and x y =
  if x = y
  then x
  else
    match (x,y) with
      (True,x) | (x,True) -> x
    | (False,x) | (x,False) -> False
    | (And l1,And l2) -> And (merge l1 l2)
    | (x,Or l) when List.mem x l -> x
    | (Or l,x) when List.mem x l -> x
    | (x,And l) | (And l,x) ->
	if List.mem x l then And l else And (insert x l)
    | (x,y) -> norm(And [x;y])

let build_or x y =
  if x = y
  then x
  else
    match (x,y) with
      (True,x) | (x,True) -> True
    | (False,x) | (x,False) -> x
    | (Or l1,Or l2) -> Or (merge l1 l2)
    | (x,And l) when List.mem x l -> x
    | (And l,x) when List.mem x l -> x
    | (x,Or l) | (Or l,x) ->
	if List.mem x l then Or l else Or (insert x l)
    | (x,y) -> norm(Or [x;y])

let keep x = Elem x
let drop x = True

let do_get_constants constants keywords =
  let donothing r k e = k e in
  let option_default = True in
  let mcode _ _ = option_default in
  let bind = build_and in

  (* if one branch gives no information, then we have to take anything *)
  let disj_union_all = List.fold_left build_or False in

  let ident r k i =
    match Ast.unwrap i with
      Ast.Id(name) ->
	(match Ast.unwrap_mcode name with
	  "NULL" -> keywords "NULL"
	| nm -> constants nm)
    | _ -> k i in

  (* no point to do anything special for records because glimpse is
     word-oriented *)
  let expression r k e =
    match Ast.unwrap e with
      Ast.Constant(const) ->
	(match Ast.unwrap_mcode const with
	  Ast.String s -> constants s
	| Ast.Char s -> constants s
	| Ast.Int "0" -> keywords "0"
	| Ast.Int "1" -> keywords "1"
	| Ast.Int s -> constants s
	| Ast.Float s -> constants s)
    | Ast.SizeOfExpr(sizeof,exp) -> bind (keywords "sizeof") (k e)
    | Ast.SizeOfType(sizeof,lp,ty,rp) -> bind (keywords "sizeof") (k e)
    | Ast.NestExpr(expr_dots,wc,false) -> option_default
    | Ast.NestExpr(expr_dots,wc,true) ->
	r.V.combiner_expression_dots expr_dots
    | Ast.DisjExpr(exps) ->
	disj_union_all (List.map r.V.combiner_expression exps)
    | Ast.OptExp(exp) -> option_default
    | Ast.Edots(_,_) | Ast.Ecircles(_,_) | Ast.Estars(_,_) -> option_default
    | _ -> k e in

  let fullType r k ft =
    match Ast.unwrap ft with
      Ast.DisjType(decls) ->
	disj_union_all (List.map r.V.combiner_fullType decls)
    | Ast.OptType(ty) -> option_default
    | _ -> k ft in

  let baseType = function
      Ast.VoidType -> keywords "void "
    | Ast.CharType -> keywords "char "
    | Ast.ShortType -> keywords "short "
    | Ast.IntType -> keywords "int "
    | Ast.DoubleType -> keywords "double "
    | Ast.FloatType -> keywords "float "
    | Ast.LongType -> keywords "long " in

  let typeC r k ty =
    match Ast.unwrap ty with
      Ast.BaseType(ty,sgn) -> baseType (Ast.unwrap_mcode ty)
    | Ast.TypeName(name) -> constants (Ast.unwrap_mcode name)
    | _ -> k ty in

  let declaration r k d =
    match Ast.unwrap d with
      Ast.DisjDecl(decls) ->
	disj_union_all (List.map r.V.combiner_declaration decls)
    | Ast.MacroDecl(nm,lp,args,rp,pv) -> constants (Ast.unwrap_mcode nm)
    | Ast.OptDecl(decl) -> option_default
    | Ast.Ddots(dots,whencode) -> option_default
    | _ -> k d in

  let initialiser r k i =
    match Ast.unwrap i with
      Ast.OptIni(ini) -> option_default
    | _ -> k i in

  let parameter r k p =
    match Ast.unwrap p with
      Ast.OptParam(param) -> option_default
    | _ -> k p in

  let rule_elem r k re =
    match Ast.unwrap re with
      Ast.WhileHeader(whl,lp,exp,rp) ->
	bind (keywords "while") (k re)
    | Ast.WhileTail(whl,lp,exp,rp,sem) ->
	bind (keywords "do") (k re)
    | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
	bind (keywords "for") (k re)
    | Ast.IteratorHeader(nm,lp,args,rp) ->
	bind (constants (Ast.unwrap_mcode nm)) (k re)
    | Ast.SwitchHeader(switch,lp,exp,rp) ->
	bind (keywords "switch") (k re)
    | Ast.Break(br,sem) ->
	keywords "break"
    | Ast.Continue(cont,sem) ->
	keywords "continue"
    | Ast.Goto ->
	keywords "goto"
    | Ast.Default(def,colon) ->
	keywords "default"
    | Ast.Include(inc,s) ->
	(match Ast.unwrap_mcode s with
	  Ast.Local l | Ast.NonLocal l ->
	    let strings =
	      List.fold_left
		(function prev ->
		  function
		      Ast.IncPath s -> (Elem s)::prev
		    | Ast.IncDots -> prev)
		[] l in
	    (match strings with
	      [] -> True
	    | x::xs -> List.fold_left bind x xs))
    | Ast.DisjRuleElem(res) ->
	disj_union_all (List.map r.V.combiner_rule_elem res)
    | _ -> k re in

  let statement r k s =
    match Ast.unwrap s with
      Ast.Disj(stmt_dots) ->
	disj_union_all (List.map r.V.combiner_statement_dots stmt_dots)
    | Ast.Nest(stmt_dots,whn,false,_,_) -> option_default
    | Ast.Nest(stmt_dots,whn,true,_,_) ->
	r.V.combiner_statement_dots stmt_dots
    | Ast.OptStm(s) -> option_default
    | Ast.Dots(d,whn,_,_) | Ast.Circles(d,whn,_,_) | Ast.Stars(d,whn,_,_) ->
	option_default
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    ident expression fullType typeC initialiser parameter declaration
    rule_elem statement donothing donothing donothing

(* ------------------------------------------------------------------------ *)

let filter_combine combine to_drop =
  let rec and_loop = function
      Elem x when List.mem x to_drop -> True
    | Or l ->  List.fold_left build_or  False (List.map or_loop l)
    | x -> x
  and or_loop = function
      Elem x when List.mem x to_drop -> False
    | And l -> List.fold_left build_and True  (List.map and_loop l)
    | x -> x in
 or_loop combine

(* ------------------------------------------------------------------------ *)

let get_all_constants minus_only =
  let donothing r k e = k e in
  let bind = Common.union_set in
  let option_default = [] in
  let mcode r (x,_,mcodekind) =
    match mcodekind with
      Ast.MINUS(_,_) -> [x]
    | _ when minus_only -> []
    | _ -> [x] in
  let other r (x,_,mcodekind) = [] in

  V.combiner bind option_default
    other mcode other other other other other other other other other other
    other

    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

(* ------------------------------------------------------------------------ *)

let get_plus_constants =
  let donothing r k e = k e in
  let bind = Common.union_set in
  let option_default = [] in
  let mcode r (_,_,mcodekind) =
    let recurse l =
      List.fold_left
	(List.fold_left
	   (function prev ->
	     function cur ->
	       bind ((get_all_constants false).V.combiner_anything cur) prev))
	[] l in
    match mcodekind with
      Ast.MINUS(_,anythings) -> recurse anythings
    | Ast.CONTEXT(_,Ast.BEFORE(a)) -> recurse a
    | Ast.CONTEXT(_,Ast.AFTER(a)) -> recurse a
    | Ast.CONTEXT(_,Ast.BEFOREAFTER(a1,a2)) ->
	Common.union_set (recurse a1) (recurse a2)
    | _ -> [] in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

(* ------------------------------------------------------------------------ *)
(* see if there are any inherited variables that must be bound for this rule
to match *)

let check_inherited nm =
  let donothing r k e = k e in
  let option_default = false in
  let bind x y = x or y in
  let mcode _ _ = option_default in
  let inherited (nm1,_) = not(nm = nm1) in
  let minherited (name,_,_) = inherited name in

  (* a case for everything for there is a metavariable, also disjunctions
     or optional things *)

  let strictident recursor k i =
    match Ast.unwrap i with
      Ast.MetaId(name,_,_) | Ast.MetaFunc(name,_,_)
    | Ast.MetaLocalFunc(name,_,_) -> minherited name
    | _ -> k i in

  let rec type_collect res = function
      TC.ConstVol(_,ty) | TC.Pointer(ty) | TC.FunctionPointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.MetaType(tyname,_,_) -> inherited tyname
    | ty -> res in

  let strictexpr recursor k e =
    match Ast.unwrap e with
      Ast.MetaExpr(name,_,Some type_list,_,_) ->
	let types = List.fold_left type_collect option_default type_list in
	bind (minherited name) types
    | Ast.MetaErr(name,_,_) | Ast.MetaExpr(name,_,_,_,_) -> minherited name
    | Ast.MetaExprList(name,None,_,_) -> minherited name
    | Ast.MetaExprList(name,Some (lenname,_,_),_,_) ->
	bind (minherited name) (inherited lenname)
    | Ast.DisjExpr(exps) ->
	(* could see if there are any variables that appear in all branches,
	   but perhaps not worth it *)
	option_default
    | _ -> k e in

  let strictdecls recursor k d =
    match Ast.unwrap d with
      Ast.DisjDecl(decls) -> option_default
    | _ -> k d in

  let strictfullType recursor k ty =
    match Ast.unwrap ty with
      Ast.DisjType(types) -> option_default
    | _ -> k ty in

  let stricttypeC recursor k ty =
    match Ast.unwrap ty with
      Ast.MetaType(name,_,_) -> minherited name
    | _ -> k ty in

  let strictparam recursor k p =
    match Ast.unwrap p with
      Ast.MetaParam(name,_,_) -> minherited name
    | Ast.MetaParamList(name,None,_,_) -> minherited name
    | Ast.MetaParamList(name,Some(lenname,_,_),_,_) ->
	bind (minherited name) (inherited lenname)
    | _ -> k p in

  let strictrule_elem recursor k re =
    (*within a rule_elem, pattern3 manages the coherence of the bindings*)
    match Ast.unwrap re with
      Ast.MetaRuleElem(name,_,_) | Ast.MetaStmt(name,_,_,_)
    | Ast.MetaStmtList(name,_,_) -> minherited name
    | _ -> k re in

  let strictstatement recursor k s =
    match Ast.unwrap s with
      Ast.Disj(stms) -> option_default
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    strictident strictexpr strictfullType stricttypeC donothing strictparam
    strictdecls strictrule_elem strictstatement donothing donothing donothing

(* ------------------------------------------------------------------------ *)

let rec dependent = function
    Ast.Dep s -> true
  | Ast.AntiDep s -> false
  | Ast.EverDep s -> true
  | Ast.NeverDep s -> false
  | Ast.AndDep (d1,d2) -> dependent d1 or dependent d2
  | Ast.OrDep (d1,d2) -> dependent d1 && dependent d2
  | Ast.NoDep -> false

(* ------------------------------------------------------------------------ *)

let rule_fn tls in_plus =
  List.fold_left
    (function (rest_info,in_plus) ->
      function cur ->
	let minuses =
	  (do_get_constants keep drop).V.combiner_top_level cur in
	let all_minuses =
	  if !Flag.sgrep_mode2
	  then [] (* nothing removed for sgrep *)
	  else (get_all_constants true).V.combiner_top_level cur in
	let plusses = get_plus_constants.V.combiner_top_level cur in
	(* the following is for eg -foo(2) +foo(x) then in another rule
	   -foo(10); don't want to consider that foo is guaranteed to be
	   created by the rule.  not sure this works completely: what if foo is
	   in both - and +, but in an or, so the cases aren't related?
	   not sure this whole thing is a good idea.  how do we know that
	   something that is only in plus is really freshly created? *)
	let plusses = Common.minus_set plusses all_minuses in
	let was_bot = minuses = True in
	let new_minuses = filter_combine minuses in_plus in
	let new_plusses = Common.union_set plusses in_plus in
	match new_minuses with
	  True ->
	    (match (do_get_constants drop keep).V.combiner_top_level cur with
	      True when not was_bot -> (rest_info, new_plusses)
	    | x -> (build_or x rest_info, new_plusses))
	| x -> (build_or x rest_info, new_plusses))
    (False,in_plus) tls

let get_constants rules =
  if not !Flag.use_glimpse
  then None
  else
    let (info,_) =
      List.fold_left
	(function (rest_info,in_plus) ->
	  function (nm,(dep,_,_),cur) ->
	    let (cur_info,cur_plus) = rule_fn cur in_plus in
	    (* no dependencies if dependent on another rule; then we need to
	       find the constants of that rule *)
	    if dependent dep or
	      List.for_all (check_inherited nm).V.combiner_top_level cur
	    then (rest_info,cur_plus)
	    else (build_or cur_info rest_info,cur_plus))
	(False,[]) (rules : Ast.rule list) in
    interpret true info
