(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

type id = Id of string | Meta of Ast.meta_name

let rec get_name name =
  match Ast0.unwrap name with
    Ast0.Id(nm) -> [Id(Ast0.unwrap_mcode nm)]
  | Ast0.MetaId(nm,_,_,_) | Ast0.MetaFunc(nm,_,_)
  | Ast0.MetaLocalFunc(nm,_,_) -> [Meta(Ast0.unwrap_mcode nm)]
  | Ast0.AsIdent(id1,id2) -> failwith "not supported"
  | Ast0.DisjId(_,id_list,_,_) -> List.concat (List.map get_name id_list)
  | Ast0.OptIdent(id) ->
      get_name id

(* --------------------------------------------------------------------- *)
(* collect all of the functions *)

let make_semi info =
  let info =
    (* drop column information, so that with -smpl_spacing the semicolon
       will come out right after the close parenthesis *)
    {info with
      Ast0.pos_info = {info.Ast0.pos_info with Ast0.column = -1};
      Ast0.mcode_start = []; Ast0.mcode_end = []} in
  let (tok,arity,_,mcodekind,pos,adj) = Ast0.make_mcode ";" in
  (tok,arity,info,mcodekind,pos,adj)

let collect_function (stm : Ast0.statement) =
  match Ast0.unwrap stm with
    Ast0.FunDecl((bef_info,_),
		 fninfo,name,lp,params,va,rp,lbrace,body,rbrace,
		 (aft_info,_)) ->
      let new_bef_info =
	{(Ast0.default_info()) with
	  Ast0.strings_before = bef_info.Ast0.strings_before} in
      List.map
	(function nm ->
	  (nm,stm,
	   Ast0.copywrap stm
	     (Ast0.Decl((new_bef_info,Ast0.context_befaft()),
			Ast0.copywrap stm
			  (Ast0.FunProto
			     (fninfo,name,lp,params,None,rp,make_semi aft_info))))))
	(get_name name)
  | _ -> []

let collect_functions stmt_dots =
  List.concat (List.map collect_function (Ast0.unwrap stmt_dots))

let drop_positions =
  let mcode (term,arity,info,mc,_,adj) =
    (term,arity,info,mc,ref [],adj) in
  let donothing r k e = k e in
  let res =
    V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing in
  res.VT0.rebuilder_rec_statement

let get_all_functions rule =
  let res =
    match Ast0.unwrap rule with
      Ast0.NONDECL(stmt) -> collect_function stmt
    | Ast0.CODE(rule_elem_dots) -> collect_functions rule_elem_dots
    | _ -> [] in
  List.map
    (function (nm,def,vl) ->
      (nm,
       (def,
	drop_positions
	  ((Iso_pattern.rebuild_mcode None).VT0.rebuilder_rec_statement vl))))
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
    | ((mname,(mdef,mproto))::minus,[]) ->
	(try
	  let (_,pproto) = List.assoc mname all_plus in
	  (mname,mdef,mproto,Some pproto)::(loop (minus,[]))
	with Not_found -> (mname,mdef,mproto,None)::(loop (minus, [])))
    | ((mname,(mdef,mproto))::minus,(pname,(pdef,pproto))::plus) ->
	if mname = pname
	then (mname,mdef,mproto,Some pproto)::(loop (minus, []))
	else
	  (try
	    let (_,pproto_for_minus) = List.assoc mname all_plus in
	    (try
	      let _ = List.assoc mname all_plus in
	      (* protos that match both *)
	      (mname,mdef,mproto,Some pproto_for_minus)::(loop (minus, plus))
	    with Not_found ->
	      (* proto that matches only minus *)
	      (mname,mdef,mproto,Some pproto_for_minus)::
	      (loop (minus, ((pname,(pdef,pproto))::plus))))
	  with Not_found ->
	    (try
	      let _ = List.assoc mname all_plus in
	      (* proto only for plus *)
	      (mname,mdef,mproto,None)::(loop (minus, plus))
	    with Not_found ->
	      (* protos for no one *)
	      (mname,mdef,mproto,Some pproto)::(loop (minus, plus)))) in
  List.filter changed_proto (loop (all_minus, all_plus))

(* --------------------------------------------------------------------- *)

and strip =
  let donothing r k e =
    {(Ast0.wrap (Ast0.unwrap (k e))) with
      Ast0.mcodekind = ref  (Ast0.PLUS Ast.ONE)} in
  let mcode (mc,_,_,_,_,_) =
    (mc,Ast0.NONE,Ast0.default_info(),Ast0.PLUS Ast.ONE,
     ref [],-1) in

  (* need a case for everything that has an unvisited component and can be in
     a function prototype. *)

  let ident r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaId(nm,constraints,seed,pure) ->
	     Ast0.MetaId(nm,constraints,seed,Ast0.Pure)
	 | Ast0.MetaFunc(nm,constraints,pure) ->
	     Ast0.MetaFunc(nm,constraints,Ast0.Pure)
	 | Ast0.MetaLocalFunc(nm,constraints,pure) ->
	     Ast0.MetaLocalFunc(nm,constraints,Ast0.Pure)
	 | e -> e)) in

  let typeC r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaType(nm,cstr,pure) -> Ast0.MetaType(nm,cstr,Ast0.Pure)
	 | e -> e)) in

  let param r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaParam(nm,cstr,pure) ->
	     Ast0.MetaParam(nm,cstr,Ast0.Pure)
	 | Ast0.MetaParamList(nm,lenname,cstr,pure) ->
	     Ast0.MetaParamList(nm,lenname,cstr,Ast0.Pure)
	 | e -> e)) in

  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing
    ident donothing donothing donothing typeC donothing param
    donothing donothing donothing donothing donothing donothing donothing

and changed_proto = function
    (mname,mdef,mproto,None) -> true
  | (mname,mdef,mproto,Some pproto) ->
      not ((strip.VT0.rebuilder_rec_statement mproto) =
	   (strip.VT0.rebuilder_rec_statement pproto))

(* --------------------------------------------------------------------- *)
(* make rules *)

let collect_ident_strings id =
  let bind x y = x @ y in
  let option_default = [] in
  let donothing r k e = k e in
  let mcode (_,_,info,_,_,_) =
    info.Ast0.strings_before @ info.Ast0.strings_after in
  let v =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing in
      v.VT0.combiner_rec_ident id

let right_attach_mcode strings (x,ar,info,mc,pos,adj) =
  let info =
    {info with
      Ast0.strings_after = info.Ast0.strings_after @ strings} in
  (x,ar,info,mc,pos,adj)

let rec right_attach_ident strings id =
  Ast0.rewrap id
    (match Ast0.unwrap id with
      Ast0.Id(name) -> Ast0.Id(right_attach_mcode strings name)
    | Ast0.MetaId(name,x,y,z) ->
	Ast0.MetaId(right_attach_mcode strings name,x,y,z)
    | Ast0.AsIdent(id,asid) -> Ast0.AsIdent(right_attach_ident strings id,asid)
    | _ -> failwith "disj, opt, and funcs not supported")

let rec attach_right strings ty =
  Ast0.rewrap ty
    (match Ast0.unwrap ty with
      Ast0.ConstVol(cv,ty) -> Ast0.ConstVol(cv,attach_right strings ty)
    | Ast0.BaseType(bt,sl) ->
	let slhd = right_attach_mcode strings (List.hd(List.rev sl)) in
	Ast0.BaseType(bt,List.rev (slhd :: (List.tl (List.rev sl))))
    | Ast0.Signed(sgn,None) -> Ast0.Signed(right_attach_mcode strings sgn,None)
    | Ast0.Signed(sgn,Some ty) ->
	Ast0.Signed(sgn,Some (attach_right strings ty))
    | Ast0.Pointer(ty,star) -> Ast0.Pointer(ty,right_attach_mcode strings star)
    | Ast0.FunctionPointer(ty,lp,star,rp,lp1,ps,rp1) ->
	Ast0.FunctionPointer(ty,lp,star,rp,lp1,ps,
			     right_attach_mcode strings rp1)
    | Ast0.Array(ty,lb,e,rb) ->
	Ast0.Array(ty,lb,e,right_attach_mcode strings rb)
    | Ast0.Decimal(dec,lp,e1,comma,e2,rp) ->
	Ast0.Decimal(dec,lp,e1,comma,e2,right_attach_mcode strings rp)
    | Ast0.EnumName(enum,None) ->
	Ast0.EnumName(right_attach_mcode strings enum, None)
    | Ast0.EnumName(enum,Some id) ->
	Ast0.EnumName(enum,Some (right_attach_ident strings id))
    | Ast0.EnumDef(ty,lb,es,rb) ->
	Ast0.EnumDef(ty,lb,es,right_attach_mcode strings rb)
    | Ast0.StructUnionName(su,None) ->
	Ast0.StructUnionName(right_attach_mcode strings su, None)
    | Ast0.StructUnionName(su,Some id) ->
	Ast0.StructUnionName(su,Some (right_attach_ident strings id))
    | Ast0.StructUnionDef(ty,lb,decls,rb) ->
	Ast0.StructUnionDef(ty,lb,decls,right_attach_mcode strings rb)
    | Ast0.TypeOfExpr(tf,lp,exp,rp) ->
	Ast0.TypeOfExpr(tf,lp,exp,right_attach_mcode strings rp)
    | Ast0.TypeOfType(tf,lp,ty,rp) ->
	Ast0.TypeOfType(tf,lp,ty,right_attach_mcode strings rp)
    | Ast0.TypeName(nm) -> Ast0.TypeName(right_attach_mcode strings nm)
    | Ast0.MetaType(nm,cstr,pure) ->
	Ast0.MetaType(right_attach_mcode strings nm,cstr,pure)
    | Ast0.AsType(ty,asty) -> Ast0.AsType(attach_right strings ty,asty)
    | _ -> failwith "disj and opt type not supported")

let rec drop_param_name p =
  Ast0.rewrap p
    (match Ast0.unwrap p with
      Ast0.Param(p,Some id) ->
	let strings = collect_ident_strings id in
	let p = attach_right strings p in
	Ast0.Param(p,None)
    | Ast0.OptParam(p) -> Ast0.OptParam(drop_param_name p)
    | p -> p)

let drop_names dec =
  let dec = (Iso_pattern.rebuild_mcode None).VT0.rebuilder_rec_statement dec in
  match Ast0.unwrap dec with
    Ast0.Decl(info,proto) ->
      (match Ast0.unwrap proto with
	Ast0.FunProto(fninfo,name,lp,params,va,rp,sem) ->
	  let params =
	    Ast0.rewrap params
	      (List.map drop_param_name (Ast0.unwrap params)) in
	  Ast0.rewrap dec
	    (Ast0.Decl
	       (info,
		Ast0.rewrap proto
		  (Ast0.FunProto(fninfo,name,lp,params,va,rp,sem))))
      |	_ -> failwith "unexpected declaration")
  | _ -> failwith "unexpected term"

let ct = ref 0

let new_name name =
  let n = !ct in
  ct := !ct + 1;
  name^"__"^(string_of_int n)

let new_iname name index =
  name^"__"^(string_of_int index)

let rec rename_param old_name all param index =
  match Ast0.unwrap param with
    Ast0.Param(ty,Some id) when all ->
      (match Ast0.unwrap id with
	Ast0.MetaId
	  (((_,name),arity,info,mcodekind,pos,adj),constraints,seed,pure) ->
	  let nm = ("__no_name__",new_name name) in
	  let new_id =
	    Ast0.rewrap id
	      (Ast0.MetaId
		 ((nm,arity,info,mcodekind,pos,adj),constraints,seed,
		  Ast0.Pure)) in
	  ([Ast.MetaIdDecl(Ast.NONE,nm)],
	   Ast0.rewrap param (Ast0.Param(ty,Some new_id)))
      |	_ -> ([],param))
  | Ast0.Pdots(d) ->
      let nm = (old_name,new_iname "__P" index) in
      let nml = (old_name,new_iname "__n" index) in
      let new_id =
	Ast0.rewrap param
	  (Ast0.MetaParamList(Ast0.rewrap_mcode d nm,
			      Ast0.MetaListLen
				(Ast0.rewrap_mcode d nml,Ast.CstrTrue),
			      Ast.CstrTrue,
			      Ast0.Pure)) in
      (* only add both new metavariable declarations for the function
	 definition case.  For the prototype case the length
	 should be inherited *)
      ((if not all
      then [Ast.MetaParamListDecl(Ast.NONE,nm,Ast.MetaLen (nml,Ast.CstrTrue));
	     Ast.MetaListlenDecl(nml)]
      else [Ast.MetaParamListDecl(Ast.NONE,nm,Ast.MetaLen (nml,Ast.CstrTrue))]),
       new_id)
  | Ast0.OptParam(p) ->
      let (metavars,p) = rename_param old_name all p index in
      (metavars,Ast0.rewrap param (Ast0.OptParam(p)))
  | _ -> ([],param)

let iota l =
  let rec loop n = function
      [] -> []
    | x::xs -> n :: (loop (n+1) xs) in
  loop 1 l

(* try to convert names in the - parameter list to new metavariables, to
account for spelling mistakes on the part of the programmer *)
let fresh_names old_name mdef dec =
  let res = ([],[],dec,mdef) in
  match Ast0.unwrap dec with
    Ast0.Decl(info,proto) ->
      (match Ast0.unwrap proto with
	Ast0.FunProto(fninfo,name,lp,params,va,rp,sem) ->
	  let (metavars,newdec) =
	    let (metavars,l) =
	      let params = Ast0.unwrap params in
	      List.split
		(List.map2 (rename_param old_name true)
		   params (iota params)) in
	    (List.concat metavars,
	     Ast0.rewrap dec
	       (Ast0.Decl
		  (info,
		   Ast0.rewrap proto
		     (Ast0.FunProto
			(fninfo,name,lp,Ast0.rewrap params l,va,rp,sem))))) in
	  let (def_metavars,newdef) =
	    match Ast0.unwrap mdef with
	      Ast0.FunDecl(x,fninfo,name,lp,params,va,rp,lb,body,rb,y) ->
		let (def_metavars,def_l) =
		  let params = Ast0.unwrap params in
		  List.split
		    (List.map2 (rename_param old_name false)
		       params (iota params)) in
		(List.concat def_metavars,
		 Ast0.rewrap mdef
		   (Ast0.FunDecl(x,fninfo,name,lp,Ast0.rewrap params def_l,va,
				 rp,lb,body,rb,y)))
	    | _ -> failwith "unexpected function definition" in
	  (metavars,def_metavars,newdec,newdef)
      |	_ -> res)
  | _ -> res

(* since there is no + counterpart, the function must be completely deleted *)
let no_names dec =
  match Ast0.unwrap dec with
    Ast0.Decl(info,proto) ->
      (match Ast0.unwrap proto with
	Ast0.FunProto(fninfo,name,lp,params,va,rp,sem) ->
	  let sem =
	    (* convert semicolon to minus, since we are dropping the whole
	       thing *)
	    let (_,_,info,_,_,_) = sem in
	    let (tok,arity,_,mcodekind,pos,adj) = Ast0.make_minus_mcode ";" in
	    (tok,arity,info,mcodekind,pos,adj) in
	  Ast0.rewrap dec
	    (Ast0.Decl
	       (info,
		Ast0.rewrap proto
		  (Ast0.FunProto
		     (fninfo,name,lp,
		      Ast0.rewrap params
			(let info = Ast0.get_info params in
			let mcodekind =
			  (* use the mcodekind of an atomic minused thing *)
			  Ast0.get_mcode_mcodekind lp in
			let pdots =
			  ("...",Ast0.NONE,info,mcodekind,ref [],-1) in
			[Ast0.rewrap params (Ast0.Pdots(pdots))]),
		      va,rp,sem))))
      |	_ -> dec)
  | _ -> dec

let mkcode proto =
  Ast0.copywrap proto (Ast0.CODE(Ast0.copywrap proto [proto]))

let merge mproto pproto =
  let mproto = Compute_lines.compute_lines true [mkcode mproto] in
  let pproto = Compute_lines.compute_lines true [mkcode pproto] in
  let (m,p) = List.split(Context_neg.context_neg mproto pproto) in
  Insert_plus.insert_plus m p true (* no isos for protos *);
  (* convert to ast so that the + code will fall down to the tokens
     and off the artificially added Ast0.CODE *)
  let mproto = Ast0toast.ast0toast_toplevel (List.hd mproto) in
  (* clean up the wrapping added above *)
  match Ast.unwrap mproto with
    Ast.CODE mproto -> List.hd (Ast.unwrap mproto)
  | _ -> failwith "not possible"

let make_rule rule_name = function
    (mname,mdef,mproto,Some pproto) ->
      let (metavars,mdef_metavars,mproto,mdef) =
	fresh_names rule_name mdef mproto in
      let no_name_mproto = drop_names mproto in
      let no_name_pproto = drop_names pproto in
      (metavars,mdef_metavars,
       [merge mproto pproto; merge no_name_mproto no_name_pproto],mdef)
  | (mname,mdef,mproto,None) ->
      ([],[],[Ast0toast.statement(no_names mproto)],mdef)

(* --------------------------------------------------------------------- *)

let reinsert mdefs minus =
  let table =
    List.map
      (function x ->
	match Ast0.unwrap x with
	  Ast0.FunDecl(_,fninfo,name,lp,params,va,rp,lbrace,body,rbrace,_) ->
	    (name,x)
	| _ -> failwith "bad mdef")
      mdefs in
  List.map
    (function x ->
      match Ast0.unwrap x with
	Ast0.NONDECL(stmt) ->
	  (match Ast0.unwrap stmt with
	    Ast0.FunDecl(_,fninfo,name,lp,params,va,rp,lbrace,body,rbrace,_) ->
	      (try Ast0.rewrap x (Ast0.NONDECL(List.assoc name table))
	      with Not_found -> x)
	  | _ -> x)
      | Ast0.CODE(rule_elem_dots) ->
	  (match Ast0.unwrap rule_elem_dots with
	    [f] ->
	      (match Ast0.unwrap f with
		Ast0.FunDecl(_,fninfo,name,lp,params,va,rp,lbrace,body,
			     rbrace,_) ->
		  (try
		    Ast0.rewrap x
		      (Ast0.CODE(Ast0.rewrap x [List.assoc name table]))
		  with Not_found -> x)
	      | _ ->
	      (* let's hope there are no functions in here... *)
	      x)
	  | _ ->
	      (* let's hope there are no functions in here... *)
	      x)
      |	_ -> x)
    minus

(* --------------------------------------------------------------------- *)
(* entry point *)

let rec split4 = function
    [] -> ([],[],[],[])
  | (a,b,c,d)::rest ->
      let (ax,bx,cx,dx) = split4 rest in (a::ax,b::bx,c::cx,d::dx)

let mk_ast_code proto =
  Ast.rewrap proto (Ast.CODE(Ast.rewrap proto [proto]))

let process rule_name rule_metavars dropped_isos minus plus ruletype =
  if List.mem "prototypes" dropped_isos
  then ((rule_metavars,minus),None)
  else
    let minus_functions = List.concat (List.map get_all_functions minus) in
    match minus_functions with
      [] -> ((rule_metavars,minus),None)
    | _ ->
	let plus_functions =
	  List.concat (List.map get_all_functions plus) in
	let protos = align minus_functions plus_functions in
	let (metavars,mdef_metavars,rules,mdefs) =
	  split4(List.map (make_rule rule_name) protos) in
	let metavars = List.concat metavars in
	let mdef_metavars = (List.concat mdef_metavars) @ rule_metavars in
	let rules = List.concat rules in
	let minus = reinsert mdefs minus in
	match rules with
	  [] -> ((rule_metavars,minus),None)
	| [x] ->
	  (* probably not possible, since there is always the version with
	     variables and the version without *)
	    ((mdef_metavars,minus),
	     Some
	       (metavars,
		Ast.CocciRule
		  ("proto for "^rule_name,
		   (Ast.ExistsDep(Ast.Dep rule_name),dropped_isos,Ast.Forall),
		   [mk_ast_code x],
		   [false],ruletype)))
	| x::_ ->
	    let drules = List.map (function x -> Ast.rewrap x [x]) rules in
	    let res =
              Ast.CocciRule
		("proto for "^rule_name,
		 (Ast.ExistsDep(Ast.Dep rule_name),dropped_isos,Ast.Forall),
		 [mk_ast_code (Ast.rewrap x (Ast.Disj drules))],
		 [false],ruletype) in
	    ((mdef_metavars,minus),Some(metavars,res))
