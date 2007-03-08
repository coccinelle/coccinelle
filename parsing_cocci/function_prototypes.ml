module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0

let rec get_name name =
  match Ast0.unwrap name with
    Ast0.Id(nm) | Ast0.MetaId(nm,_) | Ast0.MetaFunc(nm,_)
  | Ast0.MetaLocalFunc(nm,_) -> Ast0.unwrap_mcode nm
  | Ast0.OptIdent(id) | Ast0.UniqueIdent(id) | Ast0.MultiIdent(id) ->
      get_name id

(* --------------------------------------------------------------------- *)
(* collect all of the functions *)

let brace_to_semi (_,arity,info,mcodekind) = (";",Ast0.NONE,info,mcodekind)

let collect_function (stm : Ast0.statement) =
  match Ast0.unwrap stm with
    Ast0.FunDecl(_,stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
      [(get_name name,
	Ast0.copywrap stm
	  (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),
		     Ast0.copywrap stm
		       (Ast0.UnInit
			  (stg,
			   Ast0.copywrap stm
			     (Ast0.FunctionType(ty,lp,params,rp)),
			   name,brace_to_semi lbrace)))))]
  | _ -> []

let collect_functions stmt_dots =
  List.concat (List.map collect_function (Ast0.undots stmt_dots))

let get_all_functions rule =
  let res =
    match Ast0.unwrap rule with
      Ast0.DECL(stmt) -> collect_function stmt
    | Ast0.CODE(rule_elem_dots) -> collect_functions rule_elem_dots
    | _ -> [] in
  List.map
    (function (nm,vl) ->
      (nm,(Iso_pattern.rebuild_mcode None).V0.rebuilder_statement vl))
    res

(* --------------------------------------------------------------------- *)
(* try to match up the functions *)

(* pass through the - and + functions in lockstep, until one runs out.
Then process the remaining minuses, if any.  If we can find another
function of the same name for either the current - or + function, take that
one.  Otherwise, align the two current ones. *)

let rec align all_minus all_plus =
  let rec loop = function
      ([],_) -> []
    | ((mname,mproto)::minus,[]) ->
	(try
	  let pproto = List.assoc mname all_plus in
	  (mname,mproto,Some pproto)::(loop (minus,[]))
	with Not_found -> (mname,mproto,None)::(loop (minus, [])))
    | ((mname,mproto)::minus,(pname,pproto)::plus) ->
	if mname = pname
	then (mname,mproto,Some pproto)::(loop (minus, []))
	else
	  (try
	    let pproto_for_minus = List.assoc mname all_plus in
	    (try
	      let _ = List.assoc mname all_plus in
	      (* protos that match both *)
	      (mname,mproto,Some pproto_for_minus)::(loop (minus, plus))
	    with Not_found ->
	      (* proto that matches only minus *)
	      (mname,mproto,Some pproto_for_minus)::
	      (loop (minus, ((pname,pproto)::plus))))
	  with Not_found ->
	    (try
	      let _ = List.assoc mname all_plus in
	      (* proto only for plus *)
	      (mname,mproto,None)::(loop (minus, plus))
	    with Not_found ->
	      (* protos for no one *)
	      (mname,mproto,Some pproto)::(loop (minus, plus)))) in
  List.filter changed_proto (loop (all_minus, all_plus))

(* --------------------------------------------------------------------- *)

and strip =
  let donothing r k e =
    let (x,_,_,_,_,_) = k e in
    (x,Ast0.default_info(),ref 0,ref Ast0.PLUS,ref None,Ast0.NoDots) in
  let mcode (mc,_,_,_) = (mc,Ast0.NONE,Ast0.default_info(),Ast0.PLUS) in

  (* need a case for everything that has an unvisited component and can be in
     a function prototype *)

  let ident r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaId(nm,pure) -> Ast0.MetaId(nm,true)
	 | Ast0.MetaFunc(nm,pure) -> Ast0.MetaFunc(nm,true)
	 | Ast0.MetaLocalFunc(nm,pure) -> Ast0.MetaLocalFunc(nm,true)
	 | e -> e)) in

  let typeC r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaType(nm,pure) -> Ast0.MetaType(nm,true)
	 | e -> e)) in

  let param r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaParam(nm,pure) -> Ast0.MetaParam(nm,true)
	 | Ast0.MetaParamList(nm,pure) -> Ast0.MetaParamList(nm,true)
	 | e -> e)) in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing
    ident donothing typeC donothing param donothing donothing
    donothing donothing

and changed_proto = function
    (mname,mproto,None) -> true
  | (mname,mproto,Some pproto) ->
      not ((strip.V0.rebuilder_statement mproto) =
	   (strip.V0.rebuilder_statement pproto))

(* --------------------------------------------------------------------- *)
(* make rules *)

let rec drop_param_name p =
  Ast0.rewrap p
    (match Ast0.unwrap p with
      Ast0.Param(p,_) -> Ast0.Param(p,None)
    | Ast0.OptParam(p) -> Ast0.OptParam(drop_param_name p)
    | Ast0.UniqueParam(p) -> Ast0.UniqueParam(p)
    | p -> p)

let drop_names dec =
  let dec = (Iso_pattern.rebuild_mcode None).V0.rebuilder_statement dec in
  match Ast0.unwrap dec with
    Ast0.Decl(info,uninit) ->
      (match Ast0.unwrap uninit with
	Ast0.UnInit(stg,typ,name,sem) ->
	  (match Ast0.unwrap typ with
	    Ast0.FunctionType(ty,lp,params,rp) ->
	      let params =
		match Ast0.unwrap params with
		  Ast0.DOTS(l) ->
		    Ast0.rewrap params (Ast0.DOTS(List.map drop_param_name l))
		| Ast0.CIRCLES(l) ->
		    Ast0.rewrap params
		      (Ast0.CIRCLES(List.map drop_param_name l))
		| Ast0.STARS(l) -> failwith "unexpected stars" in
	      Ast0.rewrap dec
		(Ast0.Decl
		   (info,
		    Ast0.rewrap uninit
		      (Ast0.UnInit
			 (stg,
			  Ast0.rewrap typ
			    (Ast0.FunctionType(ty,lp,params,rp)),
			  name,sem))))
	  | _ -> failwith "unexpected type")
      |	_ -> failwith "unexpected declaration")
  | _ -> failwith "unexpected term"

(* only fails if there are dots at both ends of the parameter list, because in
that case names may be needed to identify the parameters unambiguously *)
let no_dots param_list =
  let param_list = List.map Ast0.unwrap param_list in
  match param_list with
    Ast0.Pdots(_)::_ ->
      (match List.rev param_list with
	Ast0.Pdots(_)::_ -> false
      |	_ -> true)
  | _ -> true

let ct = ref 0

let rename_param param =
  match Ast0.unwrap param with
    Ast0.Param(ty,Some id) ->
      (match Ast0.unwrap id with
	Ast0.MetaId((name,arity,info,mcodekind),pure) ->
	  let new_name = name^"__"^(string_of_int !ct) in
	  ct := !ct + 1;
	  let new_id =
	    Ast0.rewrap id
	      (Ast0.MetaId((new_name,arity,info,mcodekind),true)) in
	  ([Ast.MetaIdDecl(Ast.NONE,new_name)],
	   Ast0.rewrap param (Ast0.Param(ty,Some new_id)))
      |	_ -> ([],param))
  | _ -> ([],param)

(* try to convert names in the - parameter list to new metavariables, to
account for spelling mistakes on the part of the programmer *)
let fresh_names dec =
  let res = ([],dec) in
  match Ast0.unwrap dec with
    Ast0.Decl(info,uninit) ->
      (match Ast0.unwrap uninit with
	Ast0.UnInit(stg,typ,name,sem) ->
	  (match Ast0.unwrap typ with
	    Ast0.FunctionType(ty,lp,params,rp) ->
	      (match Ast0.unwrap params with
		Ast0.DOTS(l) ->
		  if no_dots l
		  then
		    let (metavars,l) =
		      List.split(List.map rename_param l) in
		    (List.concat metavars,
		     Ast0.rewrap dec
		       (Ast0.Decl
			  (info,
			   Ast0.rewrap uninit
			     (Ast0.UnInit
				(stg,
				 Ast0.rewrap typ
				   (Ast0.FunctionType
				      (ty,lp,
				       Ast0.rewrap params (Ast0.DOTS(l)),
				       rp)),
				 name,sem)))))
		  else res
	      |	_ -> res)
	  | _ -> res)
      |	_ -> res)
  | _ -> res
	      

let merge mproto pproto =
  let mproto =
    Compute_lines.compute_lines [Ast0.copywrap mproto (Ast0.DECL mproto)] in
  let pproto =
    Compute_lines.compute_lines [Ast0.copywrap pproto (Ast0.DECL pproto)] in
  let (m,p) = List.split(Context_neg.context_neg mproto pproto) in
  Insert_plus.insert_plus m p;
  (* convert to ast so that the + code will fall down to the tokens
     and off the artificially added Ast0.DECL *)
  let mproto = Ast0toast.ast0toast mproto in
  (* clean up the wrapping added above *)
  match mproto with
    [mproto] ->
      (match Ast.unwrap mproto with
	Ast.DECL mproto -> mproto
      |	_ -> failwith "not possible")
  | _ -> failwith "not possible"

let make_rule = function
    (mname,mproto,Some pproto) ->
      let (metavars,mproto) = fresh_names mproto in
      let no_name_mproto = drop_names mproto in
      let no_name_pproto = drop_names pproto in
      (metavars,
       [merge mproto pproto; merge no_name_mproto no_name_pproto])
  | (mname,mproto,None) ->
      let (metavars,mproto) = fresh_names mproto in
      (metavars,
       [Ast0toast.statement mproto;Ast0toast.statement(drop_names mproto)])

let make_rules minus plus =
  let minus_functions = List.concat (List.map get_all_functions minus) in
  match minus_functions with
    [] -> None
  | _ ->
      let plus_functions =
	List.concat (List.map get_all_functions plus) in
      let protos = align minus_functions plus_functions in
      let (metavars,rules) = List.split(List.map make_rule protos) in
      let metavars = List.concat metavars in
      let rules = List.concat rules in
      match rules with
	[] -> None
      | [x] ->
	  (* probably not possible, since there is always the version with
	     variables and the version without *)
	  Some (metavars,[Ast.rewrap x (Ast.DECL x)])
      |	x::_ ->
	  let drules =
	    List.map (function x -> Ast.rewrap x (Ast.DOTS [x])) rules in
	  Some
	    (metavars,
	     [Ast.rewrap x (Ast.DECL (Ast.rewrap x (Ast.Disj drules)))])

(* --------------------------------------------------------------------- *)
(* entry point *)

let process minus plus = make_rules minus plus
