(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module V = Visitor_ast

(* Issues:

1.  If a rule X depends on a rule Y (in a positive way), then we can ignore
    the constants in X.

2.  If a rule X contains a metavariable that is not under a disjunction and
    that is inherited from rule Y, then we can ignore the constants in X.

3.  If a rule contains a constant x in + code then subsequent rules that
    have it in - or context should not include it in their list of required
    constants.
*)

(* This doesn't do the . -> trick of get_constants for record fields, as
    that does not fit well with the recursive structure.  It was not clear
    that that was completely safe either, although eg putting a newline
    after the . or -> is probably unusual. *)

(* ----------------------------------------------------------------------- *)
(* This phase collects everything.  One can then filter out what it not
wanted *)

(* True means nothing was found
   False should never drift to the top, it is the neutral element of or
   and an or is never empty *)
type combine =
    And of combine list | Or of combine list | Not of combine
  | Elem of string | False | True

let false_on_top_err virt =
  Printf.sprintf
    "No rules apply.  Perhaps your semantic patch doesn't contain any +/-/* code, or you have a failed dependency.  If the problem is not clear, try --debug-parse-cocci%s."
    (match virt with
      [] -> ""
    | x::_ ->
	Printf.sprintf
	  " or check whether any virtual rules (e.g., %s) should be defined"
	  x)

let rec dep2c = function
    And l -> Printf.sprintf "(%s)" (String.concat "&" (List.map dep2c l))
  | Or l -> Printf.sprintf "(%s)" (String.concat "|" (List.map dep2c l))
  | Not x -> Printf.sprintf "!(%s)" (dep2c x)
  | Elem x -> x
  | False -> "false"
  | True -> "true"

(* glimpse often fails on large queries.  We can safely remove arguments of
&& as long as we don't remove all of them (note that there is no negation).
This tries just removing one of them and then orders the results by
increasing number of ors (ors are long, increasing the chance of failure,
and are less restrictive, possibly increasing the chance of irrelevant
code. *)
let reduce_glimpse x =
  let rec loop x k q =
    match x with
      Elem _ -> q()
    | Not _ -> failwith "no not in constant formula"
    | And [x] -> loop x (function changed_l -> k (And [changed_l])) q
    | And l ->
	kloop l
	  (function changed_l -> k (And changed_l))
	  (function _ ->
	    let rec rloop l k =
	      match l with
		[] -> q()
	      | x::xs ->
		  (k xs) ::
		  rloop xs (function changed_xs -> k (x :: changed_xs)) in
	    rloop l (function changed_l -> k (And changed_l)))
    | Or l -> kloop l (function changed_l -> k (Or changed_l)) q
    | _ -> failwith "not possible"
  and kloop l k q =
    match l with
      [] -> q()
    | x::xs ->
	loop x
	  (function changed_x -> k (changed_x::xs))
	  (function _ ->
	    kloop xs
	      (function changed_xs -> k (x :: changed_xs))
	      q) in
  let rec count_ors = function
      Elem _ -> 0
    | And l -> List.fold_left (+) 0 (List.map count_ors l)
    | Or l ->
	((List.length l) - 1) +
	  (List.fold_left (+) 0 (List.map count_ors l))
    | _ -> failwith "not possible" in
  let res = loop x (function x -> x) (function _ -> []) in
  let res = List.map (function x -> (count_ors x,x)) res in
  let res = List.sort compare res in
  List.map (function (_,x) -> x) res

let interpret_glimpse strict x virt =
  let rec loop = function
      Elem x -> x
    | Not x -> failwith "not unexpected in glimpse arg"
    | And [x] -> loop x
    | Or [x] -> loop x
    | And l -> Printf.sprintf "{%s}" (String.concat ";" (List.map loop l))
    | Or l -> Printf.sprintf "{%s}" (String.concat "," (List.map loop l))
    | True ->
	if strict
	then failwith "True should not be in the final result"
	else "True"
    | False ->
	if strict
	then failwith (false_on_top_err virt)
	else "False" in
  match x with
    True -> None
  | False when strict ->
      failwith (false_on_top_err virt)
  | _ ->
      Some (if strict then List.map loop (x::reduce_glimpse x) else [loop x])

(* grep only does or *)
let interpret_grep strict x virt =
  let add x l = if List.mem x l then l else x :: l in
  let rec loop collected = function
      Elem x -> add x collected
    | Not x -> failwith "not unexpected in grep arg"
    | And l | Or l ->
	let rec iloop collected = function
	    [] -> collected
	  | x::xs -> iloop (loop collected x) xs in
	iloop collected l
    | True ->
	if strict
	then failwith "True should not be in the final result"
	else add "True" collected
    | False ->
	if strict
	then failwith (false_on_top_err virt)
	else add "False" collected in
  match x with
    True -> None
  | False when strict ->
      failwith (false_on_top_err virt)
  | _ -> Some (loop [] x)

let max_cnf = 5

let interpret_cocci_git_grep strict x virt =
  (* convert to cnf *)
  let subset l1 l2 = List.for_all (fun e1 -> List.mem e1 l2) l1 in
  let opt_union_set longer shorter =
    (* (A v B) & (A v B v C) = A v B *)
    (* tries to be efficient by not updating prv, so optimize is still
       needed *)
    List.fold_left
      (function prev ->
	function cur ->
	  if List.exists (function x -> subset x cur) prev
	  then prev
	  else cur :: prev)
      longer shorter in
  let rec cnf = function
      Elem x -> [[x]]
    | Not x -> failwith "not unexpected in coccigrep arg"
    | And l ->
	(match List.map cnf l with
	  [] -> failwith "and should not be empty"
	| x ::xs -> List.fold_left opt_union_set x xs)
    | Or l ->
	let l = List.map cnf l in
	let icount =
	  List.length
	    (List.filter (function [] | [_] -> false | _ -> true) l) in
	if icount > max_cnf
	then [] (* true *)
	else
	  (match l with
	    fst::rest ->
	      List.fold_left
		(function prev ->
		  function cur ->
		    List.fold_left opt_union_set
		      []
		      (List.map (fun x -> List.map (Common.union_set x) prev)
			 cur))
		fst rest
	  | [] -> [[]]) (* false *)
    | True -> []
    | False ->
	if strict
	then failwith (false_on_top_err virt)
	else [[]] in
  let optimize (l : string list list) =
    let l = List.map (function clause -> (List.length clause, clause)) l in
    let l = List.sort compare l in
    let l = List.rev (List.map (function (len,clause) -> clause) l) in
    List.fold_left
      (fun prev cur ->
	if List.exists (subset cur) prev then prev else cur :: prev)
      [] l in
  let rec atoms acc = function
      Elem x -> if List.mem x acc then acc else x :: acc
    | Not x -> failwith "not unexpected in atoms"
    | And l | Or l -> List.fold_left atoms acc l
    | True | False -> acc in
  let wordify x = "\\b" ^ x ^"\\b" in
  match x with
    True -> None
  | False when strict ->
      failwith (false_on_top_err virt)
  | _ ->
      let orify l = Str.regexp (String.concat "\\|" (List.map wordify l)) in
      let res1 = orify (atoms [] x) in (* all atoms *)
      let res = cnf x in
      let res = optimize res in
      let res = Cocci_grep.split res in
      let res2 = List.map orify res in (* atoms in conjunction *)
      (*List.iter
	(function clause ->
	  Printf.printf "%s\n" (String.concat " " clause))
	res;*)
      let res3 =
	List.map (function x -> "\\( -e "^(String.concat " -e " x)^" \\)")
	  res in
      Some (res1,res2,res3)

let interpret_idutils = function
    True -> None
  | x -> Some x

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

let intersect l1 l2 = List.filter (function l1e -> List.mem l1e l2) l1

let minus_set l1 l2 = List.filter (function l1e -> not (List.mem l1e l2)) l1

let insert x l = merge [x] l

let rec build_and x y =
  if x = y
  then x
  else
    match (x,y) with
      (True,x) | (x,True) -> x
    | (False,x) | (x,False) -> False
    | (And l1,And l2) -> And (merge l1 l2)
    | (x,Or l) when List.mem x l -> x
    | (Or l,x) when List.mem x l -> x
    | (Or l1,Or l2) when not ((intersect l1 l2) = []) ->
	let inner =
	  build_and
	    (List.fold_left build_or False (minus_set l1 l2))
	    (List.fold_left build_or False (minus_set l2 l1)) in
	List.fold_left build_or inner (intersect l1 l2)
    | (x,And l) | (And l,x) ->
	if List.mem x l
	then And l
	else
	  let others =
	    List.filter
	      (function
		  Or l -> not(List.mem x l)
		| _ -> true)
	      l in
	  And (insert x others)
    | (x,y) -> norm(And [x;y])

and build_or x y =
  if x = y
  then x
  else
    match (x,y) with
      (True,x) | (x,True) -> True
    | (False,x) | (x,False) -> x
    | (Or l1,Or l2) -> Or (merge l1 l2)
    | (x,And l) when List.mem x l -> x
    | (And l,x) when List.mem x l -> x
    | (And l1,And l2) when not ((intersect l1 l2) = []) ->
	let inner =
	  build_or
	    (List.fold_left build_and True (minus_set l1 l2))
	    (List.fold_left build_and True (minus_set l2 l1)) in
	List.fold_left build_and inner (intersect l1 l2)
    | (x,Or l) | (Or l,x) ->
	if List.mem x l
	then Or l
	else
	  let others =
	    List.filter
	      (function
		  And l -> not(List.mem x l)
		| _ -> true)
	      l in
	  Or (insert x others)
    | (x,y) -> norm(Or [x;y])

let keep x = Elem x
let drop x = True

let do_get_constants constants keywords env (neg_pos,_) =
  let donothing r k e = k e in
  let option_default = True in
  let bad_default = False in
  let bind = build_and in
  let inherited ((nm1,_) as x) =
    (* ignore virtuals, can never match *)
    if nm1 = "virtual" then bad_default
    (* perhaps inherited, but value not required, so no constraints *)
    else if List.mem x neg_pos then option_default
    else (try List.assoc nm1 env with Not_found -> False) in
  let minherited name = inherited (Ast.unwrap_mcode name) in
  let mcode _ x =
    List.fold_left bind option_default
      (List.map
	 (function Ast.MetaPos(name,constraints,_,keep,inh) -> minherited name)
	 (Ast.get_pos_var x)) in

  (* if one branch gives no information, then we have to take anything *)
  let disj_union_all = List.fold_left build_or False in

  (*get inheritance information from fresh variable construction information*)
  (* can't do anything with DisjRuleElem, don't know which will be used *)
  (* expect that the same info will be in branches, which after disjdistr
     should be atomic *)
  let fresh_info re =
    match Ast.unwrap re with
      Ast.DisjRuleElem(res) -> option_default
    | _ ->
	let fresh = Ast.get_fresh re in
	List.fold_left
	  (function prev ->
	    function
		(_,Ast.NoVal) -> prev
	      | (_,Ast.StringSeed _) -> prev
	      | (_,Ast.ListSeed l) ->
		  List.fold_left
		    (function prev ->
		      function
			  Ast.SeedString _ -> prev
			| Ast.SeedId name ->
			    bind (inherited name) prev)
		    prev l)
	  option_default fresh in

  let rec cstr r k c =
    match c with
      Ast.CstrFalse -> False
    | Ast.CstrTrue -> True
    | Ast.CstrAnd list ->
	List.fold_left (fun accu c -> build_and accu (cstr r k c)) True list
    | Ast.CstrOr list ->
	List.fold_left (fun accu c -> build_or accu (cstr r k c)) False list
    | Ast.CstrConstant (Ast.CstrString s) -> True
    | Ast.CstrMeta_name mv -> inherited mv
    | Ast.CstrScript (_, (_name, _lang, params, _pos, _code)) ->
	List.fold_left (fun accu (mv, _) -> build_and accu (inherited mv)) True
	  params
    | Ast.CstrSub list ->
	List.fold_left (fun accu mv -> build_or accu (inherited mv)) False list
    | Ast.CstrExpr e -> r.V.combiner_expression e
    | Ast.CstrType ty -> r.V.combiner_fullType ty
    | Ast.CstrNot _
    | Ast.CstrConstant (Ast.CstrInt _)
    | Ast.CstrRegexp _ | Ast.CstrOperator _ -> True in

  let ident r k i =
    match Ast.unwrap i with
      Ast.Id(name) ->
	bind (k i)
	  (match Ast.unwrap_mcode name with
	    "NULL" -> keywords "NULL"
	  | nm -> constants nm)
    | Ast.MetaId(name,c,_,_)
    | Ast.MetaFunc(name,c,_,_)
    | Ast.MetaLocalFunc(name,c,_,_) ->
	bind (cstr r k (Ast.cstr_push_not c)) (bind (k i) (minherited name))
    | Ast.DisjId(ids) -> disj_union_all (List.map r.V.combiner_ident ids)
    | _ -> k i in

  let type_collect ty =
    let add x res = build_and res x in
    let add_ident ident =
      match Ast.unwrap ident with
        Ast.Id name -> add (constants (Ast.unwrap_mcode name))
      | Ast.MetaId(name,_,_,_) -> add (minherited name)
      | _ -> Common.id in
    let enumOrStructUnionName _ ident res =
      Common.default res (fun ident' -> add_ident ident' res) ident in
    let pieces ty res =
      Ast.fullType_fold
	{ Ast.empty_transformer with
	  Ast.decimal = Some (fun _ _ _ _ _ _ -> add (keywords "decimal"));
	  metaType =
	  Some (fun tyname _ _ _ -> add (inherited (Ast.unwrap_mcode tyname)));
	  typeName =
	  Some(fun tyname -> add (constants (Ast.unwrap_mcode tyname)));
	  enumName = Some enumOrStructUnionName;
	  structUnionName = Some enumOrStructUnionName
	} ty res in
    let rec loop ty =
      match Ast.unwrap ty with
	Ast.DisjType l ->
	  List.fold_left (fun prev ty -> build_or prev (loop ty)) False l
      | _ -> pieces ty option_default in
    loop ty in

  (* no point to do anything special for records because glimpse is
     word-oriented *)
  let expression r k e =
    match Ast.unwrap e with
      Ast.Constant(const) ->
	bind (k e)
	  (match Ast.unwrap_mcode const with
	    Ast.String s -> (*constants s*)
	      (* not useful if the string contains non letters, etc *)
	      (* seems safer to ignore *)
	      option_default
	  | Ast.Char "\\0" -> option_default (* glimpse doesn't like it *)
	  | Ast.Char s -> option_default (* probably not chars either *)
	  (* the following were eg keywords "1", but not good for glimpse *)
	  | Ast.Int s -> option_default (* glimpse doesn't index integers *)
	  | Ast.Float s -> option_default (* probably not floats either *)
	  | Ast.DecimalConst _ -> option_default (* or decimals *))
    | Ast.StringConstant(lq,str,rq) -> option_default
	(* Like the above constant case, this information is not likely indexed
	let str = Ast.undots str in
	(* pick up completely constant strings *)
	let strs =
	  List.fold_left
	    (function strs ->
	      function frag ->
		match (strs, Ast.unwrap frag) with
		  (None,_) -> None
		| (Some strs, Ast.ConstantFragment(str)) ->
		    Some ((Ast.unwrap_mcode str)::strs)
		| (Some strs, Ast.FormatFragment(pct,fmt)) ->
		    let cstfmt =
		      match Ast.unwrap fmt with
			Ast.ConstantFormat s -> Some (Ast.unwrap_mcode s)
		      |	_ -> None in
		    (match cstfmt with
		      Some f -> Some (f :: "%" :: strs)
		    | _ -> None)
		| (Some strs, Ast.Strdots _)
		| (Some strs, Ast.MetaFormatList _) -> None)
	    (Some []) str in
	bind (k e)
	  (match strs with
	    Some strs -> constants (String.concat "" (List.rev strs))
	  | None ->  option_default)
	*)
    | Ast.MetaExpr(name,_,_,Some type_list,_,_,_) ->
	let types =
	  match type_list with
	    [] -> True (* no constraint *)
	  | _ -> (* at least one constraint must be satisfied *)
	      List.fold_left (fun prev ty -> build_or (type_collect ty) prev)
		(type_collect(List.hd type_list)) (List.tl type_list) in
	bind (k e) (bind (minherited name) types)
    | Ast.MetaErr(name,_,_,_) | Ast.MetaExpr(name,_,_,_,_,_,_) ->
	bind (k e) (minherited name)
    | Ast.MetaExprList(name,Ast.MetaListLen (lenname,_,_,_),_,_,_) ->
	bind (k e) (bind (minherited name) (minherited lenname))
    | Ast.MetaExprList(name,_,_,_,_) -> minherited name
    | Ast.SizeOfExpr(sizeof,exp) -> bind (keywords "sizeof") (k e)
    | Ast.SizeOfType(sizeof,lp,ty,rp) -> bind (keywords "sizeof") (k e)
    | Ast.NestExpr(starter,expr_dots,ender,wc,false) -> option_default
    | Ast.NestExpr(starter,expr_dots,ender,wc,true) ->
	r.V.combiner_expression_dots expr_dots
    | Ast.DisjExpr(exps) ->
	disj_union_all (List.map r.V.combiner_expression exps)
    | Ast.OptExp(exp) -> option_default
    | Ast.Edots(_,_) -> option_default
    | _ -> k e in

  (* cases for metavariabes *)
  let string_fragment r k ft =
    match Ast.unwrap ft with
      Ast.MetaFormatList(pct,name,Ast.MetaListLen (lenname,_,_,_),_,_,_) ->
	bind (k ft) (bind (minherited name) (minherited lenname))
    | Ast.MetaFormatList(pct,name,_,_,_,_) -> bind (k ft) (minherited name)
    | _ -> k ft in

  let string_format r k ft =
    match Ast.unwrap ft with
      Ast.MetaFormat(name,_,_,_) -> bind (k ft) (minherited name)
    | _ -> k ft in

  let fullType r k ft =
    match Ast.unwrap ft with
      Ast.DisjType(decls) ->
	disj_union_all (List.map r.V.combiner_fullType decls)
    | Ast.OptType(ty) -> option_default
    | _ -> k ft in

  let baseType ty = keywords (Ast.string_of_baseType ty) in

  let typeC r k ty =
    match Ast.unwrap ty with
      Ast.BaseType(ty1,strings) -> bind (k ty) (baseType ty1)
    | Ast.TypeOfExpr(sizeof,lp,e,rp) -> bind (keywords "typeof") (k ty)
    | Ast.TypeOfType(sizeof,lp,t,rp) -> bind (keywords "typeof") (k ty)
    | Ast.TypeName(name) -> bind (k ty) (constants (Ast.unwrap_mcode name))
    | Ast.MetaType(name,_,_,_) -> bind (minherited name) (k ty)
    | _ -> k ty in

  let declaration r k d =
    match Ast.unwrap d with
      Ast.MetaDecl(name,_,_,_) ->
	bind (k d) (minherited name)
    | Ast.DisjDecl(decls) ->
	disj_union_all (List.map r.V.combiner_declaration decls)
    | Ast.OptDecl(decl) -> option_default
    (* need things with explicit names too *)
    | Ast.Init(_,_,_,attr,_,_,_) | Ast.UnInit(_,_,_,attr,_) ->
	List.fold_left bind (k d)
	  (List.map (fun attr -> constants (Ast.unwrap_mcode attr)) attr)
    | _ -> k d in

  let field r k d =
    match Ast.unwrap d with
      Ast.MetaField(name,_,_,_) ->
	bind (k d) (minherited name)
    | Ast.MetaFieldList(name,Ast.MetaListLen(lenname,_,_,_),_,_,_) ->
	bind (minherited name) (bind (minherited lenname) (k d))
    | Ast.DisjField(decls) ->
	disj_union_all (List.map r.V.combiner_field decls)
    | Ast.OptField(decl) -> option_default
    | _ -> k d in

  let initialiser r k i =
    match Ast.unwrap i with
      Ast.OptIni(ini) -> option_default
    | _ -> k i in

  let parameter r k p =
    match Ast.unwrap p with
      Ast.OptParam(param) -> option_default
    | Ast.MetaParam(name,_,_,_) -> bind (k p) (minherited name)
    | Ast.MetaParamList(name,Ast.MetaListLen(lenname,_,_,_),_,_,_) ->
	bind (minherited name) (bind (minherited lenname) (k p))
    | Ast.MetaParamList(name,_,_,_,_) -> bind (k p) (minherited name)
    | _ -> k p in

  let define_parameter r k p =
    match Ast.unwrap p with
      Ast.MetaDParamList(name,Ast.MetaListLen(lenname,_,_,_),_,_,_) ->
	bind (minherited name) (bind (minherited lenname) (k p))
    | Ast.MetaDParamList(name,_,_,_,_) -> bind (k p) (minherited name)
    | _ -> k p in

  let rule_elem r k re =
    bind (fresh_info re)
    (match Ast.unwrap re with
      Ast.MetaStmtList(name,Ast.MetaListLen (lenname,_,_,_),_,_,_) ->
	bind (minherited name) (bind (minherited lenname) (k re))
    | Ast.MetaRuleElem(name,_,_,_) | Ast.MetaStmt(name,_,_,_,_)
    | Ast.MetaStmtList(name,_,_,_,_) -> bind (minherited name) (k re)
    | Ast.WhileHeader(whl,lp,exp,rp) ->
	bind (keywords "while") (k re)
    | Ast.WhileTail(whl,lp,exp,rp,sem) ->
	bind (keywords "do") (k re)
    | Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
	bind (keywords "for") (k re)
    | Ast.SwitchHeader(switch,lp,exp,rp) ->
	bind (keywords "switch") (k re)
    | Ast.Break(br,sem) ->
	bind (keywords "break") (k re)
    | Ast.Continue(cont,sem) ->
	bind (keywords "continue") (k re)
    | Ast.Goto(_,i,_) ->
	bind (keywords "goto") (k re)
    | Ast.Default(def,colon) ->
	bind (keywords "default") (k re)
    | Ast.Include(inc,s) ->
	bind (k re)
	  (match Ast.unwrap_mcode s with
	    Ast.AnyInc -> True
	  | Ast.Local l | Ast.NonLocal l ->
	      let strings =
		List.fold_left
		  (function prev ->
		    function
			(* just take the last thing, probably the most
			   specific.  everything is necessary anyway. *)
			Ast.IncPath s -> [Elem s]
		      | Ast.IncDots -> prev)
		  [] l in
	      (match strings with
		[] -> True
	      | x::xs -> List.fold_left bind x xs))
    | Ast.Pragma(prg,id,body) ->
	bind (keywords "pragma") (k re)
    | Ast.DisjRuleElem(res) ->
	disj_union_all (List.map r.V.combiner_rule_elem res)
    | _ -> k re) in

  let statement r k s =
    match Ast.unwrap s with
      Ast.Disj(stmt_dots) ->
	disj_union_all (List.map r.V.combiner_statement_dots stmt_dots)
    | Ast.Nest(starter,stmt_dots,ender,whn,false,_,_) -> option_default
    | Ast.Nest(starter,stmt_dots,ender,whn,true,_,_) ->
	r.V.combiner_statement_dots stmt_dots
    | Ast.OptStm(s) -> option_default
    | Ast.Dots(d,whn,_,_) -> option_default
    | _ -> k s in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    ident expression string_fragment string_format donothing donothing
    fullType typeC initialiser parameter define_parameter declaration donothing
    field donothing rule_elem statement donothing donothing donothing

(* ------------------------------------------------------------------------ *)

(* true means the rule should be analyzed, false means it should be ignored *)
let rec dependencies env d =
  let rec loop = function
      Ast.Dep s -> (try List.assoc s env with Not_found -> False)
    | Ast.AntiDep s -> True
    | Ast.EverDep s -> (try List.assoc s env with Not_found -> False)
    | Ast.NeverDep s -> True
    | Ast.AndDep (d1,d2) -> build_and (loop d1) (loop d2)
    | Ast.OrDep (d1,d2) -> build_or (loop d1) (loop d2)
    | Ast.FileIn _ | Ast.NotFileIn _ -> True in
  match d with
    Ast.NoDep -> True
  | Ast.FailDep -> False
  | Ast.ExistsDep d | Ast.ForallDep d -> loop d

(* ------------------------------------------------------------------------ *)

let all_context =
  let bind x y = x && y in
  let option_default = true in

  let donothing recursor k e = k e in

  let process_mcodekind = function
      Ast.CONTEXT(_,Ast.NOTHING) -> true
    | _ -> false in

  let mcode r e = process_mcodekind (Ast.get_mcodekind e) in

  let end_info (_,_,_,mc) = process_mcodekind mc in

  let initialiser r k e =
    match Ast.unwrap e with
      Ast.StrInitList(all_minus,_,_,_,_) ->
	not all_minus && k e
    | _ -> k e in

  let annotated_decl decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,_,_) -> bef in

  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.FunHeader(bef,_,_,_,_,_,_,_) -> bind (process_mcodekind bef) (k e)
    | Ast.Decl decl ->
	bind (process_mcodekind (annotated_decl decl)) (k e)
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	bind (process_mcodekind (annotated_decl decl)) (k e)
    | _ -> k e in

  let statement r k e =
    match Ast.unwrap e with
      Ast.IfThen(_,_,ei) | Ast.IfThenElse(_,_,_,_,ei)
    | Ast.While(_,_,ei)  | Ast.For(_,_,ei)
    | Ast.Iterator(_,_,ei) | Ast.FunDecl(_,_,_,_,ei) ->
	bind (k e) (end_info ei)
    | _ -> k e in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    initialiser donothing donothing donothing donothing donothing donothing
    rule_elem statement donothing donothing donothing

(* ------------------------------------------------------------------------ *)

(* The whole "in_plus" idea is flawed.  If something is added in one rule and
matched in a later one, we want to include files that originally contain
the thing, so no point to keep track of what is added by earlier rules.
The situation is something like a -> b v (b & c).  We don't actually need
both b and c, but if we don't have b, then the only way that we can get it is
fro the first rule matching, in which case the formula is already true. *)
let rule_fn nm tls env neg_pos =
  (* tls seems like it is supposed to relate to multiple minirules.  If we
     were to actually allow that, then the following could be inefficient,
     because it could run sat on the same rule name (x) more than once. *)
  List.fold_left
    (function rest_info ->
      function (cur,neg_pos) ->
	let minuses =
	  let getter = do_get_constants keep drop env neg_pos in
	  getter.V.combiner_top_level cur in
	(* the following is for eg -foo(2) +foo(x) then in another rule
	   -foo(10); don't want to consider that foo is guaranteed to be
	   created by the rule.  not sure this works completely: what if foo is
	   in both - and +, but in an or, so the cases aren't related?
	   not sure this whole thing is a good idea.  how do we know that
	   something that is only in plus is really freshly created? *)
	let was_bot = minuses = True in
	(* perhaps it should be build_and here?  we don't really have multiple
	   minirules anymore anyway. *)
	match minuses with
	  True ->
	    let getter = do_get_constants drop keep env neg_pos in
	    let retry = getter.V.combiner_top_level cur in
	    (match retry with
	      True when not was_bot -> rest_info
	    | x -> build_or x rest_info)
	| x -> build_or x rest_info)
    False (List.combine tls neg_pos)

let debug_deps nm deps res from_code =
  if !Flag_parsing_cocci.debug_parse_cocci
  then
    begin
      Printf.fprintf stderr "Rule: %s\n" nm;
      Printf.fprintf stderr "Dependencies: %s\n"
	(Common.format_to_string
	   (function _ -> Pretty_print_cocci.dependency deps));
      Printf.fprintf stderr "Result: %s\n" (dep2c res);
      (match from_code with
	Some deps ->
	  Printf.fprintf stderr "Result_from_code: %s\n" (dep2c deps)
      | None -> ());
      Printf.fprintf stderr "\n";
    end

let run rules neg_pos_vars =
  let (info,_,_) =
    List.fold_left
      (function (rest_info,env,locals(*dom of env*)) ->
        function
	    (Ast.ScriptRule (nm,_,deps,mv,_,_,_),_) ->
	      let extra_deps =
		List.fold_left
		  (function prev ->
		    function
			(_,("virtual",_),_,_) -> prev
		      | (_,(rule,_),_,Ast.NoMVInit) -> Ast.Dep rule :: prev
		      | (_,(rule,_),_,_) ->
			  (* default initializer, so no dependency *)
			  prev)
		  [] mv in
	      let extra_deps =
		match extra_deps with
		  [] -> deps
		| x::xs ->
		    let extra_deps =
		      List.fold_left (fun prev x -> Ast.AndDep(x,prev)) x xs in
		    match deps with
		      Ast.NoDep -> Ast.ExistsDep(extra_deps)
		    | Ast.FailDep -> Ast.FailDep
		    | Ast.ExistsDep d ->
			Ast.ExistsDep(Ast.AndDep(d,extra_deps))
		    | Ast.ForallDep d ->
			Ast.ForallDep(Ast.AndDep(d,extra_deps)) in
	      let dependencies = dependencies env extra_deps in
	      debug_deps nm extra_deps dependencies None;
	      (match dependencies with
		False ->
		  (rest_info, (nm,True)::env, nm::locals)
	      | dependencies ->
		  (build_or dependencies rest_info, env, locals))
          | (Ast.InitialScriptRule (_,_,deps,_,_,_),_)
	  | (Ast.FinalScriptRule (_,_,deps,_,_,_),_) ->
	      (* initialize and finalize dependencies are irrelevant to
		 get_constants *)
	      (* only possible metavariables are virtual *)
	      (rest_info, env, locals)
          | (Ast.CocciRule (nm,(dep,_,_),cur,_,_),neg_pos_vars) ->
	      let dependencies = dependencies env dep in
	      (match dependencies with
		False -> (rest_info,env,locals)
	      | dependencies ->
		  let cur_info =
		    rule_fn nm cur ((nm,True)::env) neg_pos_vars in
		  let re_cur_info = build_and dependencies cur_info in
		  debug_deps nm dep dependencies (Some cur_info);
		  if List.for_all all_context.V.combiner_top_level cur
		  then (rest_info,(nm,re_cur_info)::env,nm::locals)
		  else
		    (* no constants if dependent on another rule; then we need
		       to find the constants of that rule *)
		    (* why does env not use re_cur_info? *)
		    (build_or re_cur_info rest_info,
		     (nm,cur_info)::env,locals)))
      (False,[],[])
      (List.combine (rules : Ast.rule list) neg_pos_vars) in
  info

(* The return value is a tuple of four components.
1. A list of all words, regardless of & and |, for use with grep (or only)
2. A list of single strings using the glimpse ; and ,  operators
3. A triple of 1 and of a CNF representation, both as regexps, and of the
CNF as a list of git grep strings.  coccigrep uses 1 for basic scanning and
then the CNF regexp for more refined scanning.  git grep uses the second
CNF representation.
4. An arbitrary formula, usable by the support for idutils *)

let get_constants rules neg_pos_vars virt =
  if !Flag.worth_trying_opt
  then
    begin
    let res = run rules neg_pos_vars in
    let grep =
      interpret_grep true res virt in (* useful because in string form *)
    let coccigrep = interpret_cocci_git_grep true res virt in
    match !Flag.scanner with
      Flag.NoScanner ->
	(grep,None,coccigrep,None)
    | Flag.Glimpse ->
	(grep,interpret_glimpse true res virt,coccigrep,None)
    | Flag.IdUtils ->
	(grep,None,coccigrep,interpret_idutils res)
    | Flag.CocciGrep | Flag.GitGrep -> (grep,None,coccigrep,None)
    end
  else (None,None,None,None)
