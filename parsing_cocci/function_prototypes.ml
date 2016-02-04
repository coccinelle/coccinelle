(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./function_prototypes.ml"
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
  | Ast0.OptIdent(id) | Ast0.UniqueIdent(id) ->
      get_name id

(* --------------------------------------------------------------------- *)
(* collect all of the functions *)

let brace_to_semi (_,arity,info,mcodekind,pos,adj) =
  let info =
    (* drop column information, so that with -smpl_spacing the semicolon
       will come out right after the close parenthesis *)
    {info with Ast0.pos_info = {info.Ast0.pos_info with Ast0.column = -1}} in
  (";",Ast0.NONE,info,mcodekind,pos,adj)

let collect_function (stm : Ast0.statement) =
  match Ast0.unwrap stm with
    Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
      let stg =
	match
	  List.filter (function Ast0.FStorage(_) -> true | _ -> false)
	    fninfo with [Ast0.FStorage(s)] -> Some s | _ -> None in
      let ty =
	match
	  List.filter (function Ast0.FType(_) -> true | _ -> false)
	    fninfo with [Ast0.FType(t)] -> Some t | _ -> None in
      List.map
	(function nm ->
	  (nm,stm,
	   Ast0.copywrap stm
	     (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),
			Ast0.copywrap stm
			  (Ast0.UnInit
			     (stg,
			      Ast0.copywrap stm
				(Ast0.FunctionType(ty,lp,params,rp)),
			      name,brace_to_semi lbrace))))))
	(get_name name)
  | _ -> []

let collect_functions stmt_dots =
  List.concat (List.map collect_function (Ast0.undots stmt_dots))

let drop_positions =
  let mcode (term,arity,info,mc,_,adj) =
    (term,arity,info,mc,ref [],adj) in
  let donothing r k e = k e in
  let res =
    V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing in
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
     a function prototype.  Also get rid of constraints because pcre
     constraints cannot be compared. *)

  let ident r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaId(nm,constraints,seed,pure) ->
	     Ast0.MetaId(nm,Ast.IdNoConstraint,seed,Ast0.Pure)
	 | Ast0.MetaFunc(nm,constraints,pure) ->
	     Ast0.MetaFunc(nm,Ast.IdNoConstraint,Ast0.Pure)
	 | Ast0.MetaLocalFunc(nm,constraints,pure) ->
	     Ast0.MetaLocalFunc(nm,Ast.IdNoConstraint,Ast0.Pure)
	 | e -> e)) in

  let typeC r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaType(nm,pure) -> Ast0.MetaType(nm,Ast0.Pure)
	 | e -> e)) in

  let param r k e =
    donothing r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.MetaParam(nm,pure) ->
	     Ast0.MetaParam(nm,Ast0.Pure)
	 | Ast0.MetaParamList(nm,lenname,pure) ->
	     Ast0.MetaParamList(nm,lenname,Ast0.Pure)
	 | e -> e)) in

  V0.flat_rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    ident donothing typeC donothing param donothing donothing donothing
    donothing donothing

and changed_proto = function
    (mname,mdef,mproto,None) -> true
  | (mname,mdef,mproto,Some pproto) ->
      not ((strip.VT0.rebuilder_rec_statement mproto) =
	   (strip.VT0.rebuilder_rec_statement pproto))

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
  let dec = (Iso_pattern.rebuild_mcode None).VT0.rebuilder_rec_statement dec in
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
	  | _ -> failwith "function prototypes: unexpected type")
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
			      Ast0.MetaListLen (Ast0.rewrap_mcode d nml),
			      Ast0.Pure)) in
      (* only add both new metavariable declarations for the function
	 definition case.  For the prototype case the length
	 should be inherited *)
      ((if not all
      then [Ast.MetaParamListDecl(Ast.NONE,nm,Ast.MetaLen nml);
	     Ast.MetaListlenDecl(nml)]
      else [Ast.MetaParamListDecl(Ast.NONE,nm,Ast.MetaLen nml)]),
       new_id)
  | Ast0.OptParam(p) ->
      let (metavars,p) = rename_param old_name all p index in
      (metavars,Ast0.rewrap param (Ast0.OptParam(p)))
  | Ast0.UniqueParam(p) ->
      let (metavars,p) = rename_param old_name all p index in
      (metavars,Ast0.rewrap param (Ast0.UniqueParam(p)))
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
    Ast0.Decl(info,uninit) ->
      (match Ast0.unwrap uninit with
	Ast0.UnInit(stg,typ,name,sem) ->
	  (match Ast0.unwrap typ with
	    Ast0.FunctionType(ty,lp,params,rp) ->
	      let (metavars,newdec) =
		let (metavars,l) =
		  let params = Ast0.undots params in
		  List.split
		    (List.map2 (rename_param old_name true)
		       params (iota params)) in
		(List.concat metavars,
		 Ast0.rewrap dec
		   (Ast0.Decl
		      (info,
		       Ast0.rewrap uninit
			 (Ast0.UnInit
			    (stg,
			     Ast0.rewrap typ
			       (Ast0.FunctionType
				  (ty,lp,Ast0.rewrap params (Ast0.DOTS(l)),
				   rp)),
			     name,sem))))) in
	      let (def_metavars,newdef) =
		match Ast0.unwrap mdef with
		  Ast0.FunDecl(x,fninfo,name,lp,params,rp,lb,body,rb) ->
		    let (def_metavars,def_l) =
		      let params = Ast0.undots params in
		      List.split
			(List.map2 (rename_param old_name false)
			   params (iota params)) in
		    (List.concat def_metavars,
		     Ast0.rewrap mdef
		       (Ast0.FunDecl(x,fninfo,name,lp,
				     Ast0.rewrap params (Ast0.DOTS(def_l)),
				     rp,lb,body,rb)))
		   | _ -> failwith "unexpected function definition" in
	      (metavars,def_metavars,newdec,newdef)
	  | _ -> res)
      |	_ -> res)
  | _ -> res

(* since there is no + counterpart, the function must be completely deleted *)
let no_names dec =
  match Ast0.unwrap dec with
    Ast0.Decl(info,uninit) ->
      (match Ast0.unwrap uninit with
	Ast0.UnInit(stg,typ,name,sem) ->
	  (match Ast0.unwrap typ with
	    Ast0.FunctionType(ty,lp,params,rp) ->
	      Ast0.rewrap dec
		(Ast0.Decl
		   (info,
		    Ast0.rewrap uninit
		      (Ast0.UnInit
			 (stg,
			  Ast0.rewrap typ
			    (Ast0.FunctionType
			       (ty,lp,
				Ast0.rewrap params
				  (let info = Ast0.get_info params in
				  let mcodekind =
				    (* use the mcodekind of an atomic minused
				       thing *)
				    Ast0.get_mcode_mcodekind lp in
				  let pdots =
				    ("...",Ast0.NONE,info,mcodekind,
				     ref [],-1) in
				  Ast0.DOTS
				    ([Ast0.rewrap params
					 (Ast0.Pdots(pdots))])),
				rp)),
			  name,sem))))
	  | _ -> dec)
      |	_ -> dec)
  | _ -> dec

let mkcode proto =
  Ast0.copywrap proto (Ast0.CODE(Ast0.copywrap proto (Ast0.DOTS [proto])))

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
    Ast.CODE mproto -> List.hd (Ast.undots mproto)
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
	  Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
	    (name,x)
	| _ -> failwith "bad mdef")
      mdefs in
  List.map
    (function x ->
      match Ast0.unwrap x with
	Ast0.NONDECL(stmt) ->
	  (match Ast0.unwrap stmt with
	    Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
	      (try Ast0.rewrap x (Ast0.NONDECL(List.assoc name table))
	      with Not_found -> x)
	  | _ -> x)
      | Ast0.CODE(rule_elem_dots) ->
	  (match Ast0.undots rule_elem_dots with
	    [f] ->
	      (match Ast0.unwrap f with
		Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
		  (try
		    Ast0.rewrap x
		      (Ast0.CODE
			 (Ast0.rewrap x (Ast0.DOTS [List.assoc name table])))
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
  Ast.rewrap proto (Ast.CODE(Ast.rewrap proto (Ast.DOTS [proto])))

let process rule_name rule_metavars dropped_isos minus plus ruletype =
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
		 (Ast.Dep rule_name,dropped_isos,Ast.Forall),
		 [mk_ast_code x],
		 [false],ruletype)))
      |	x::_ ->
	  let drules =
	    List.map (function x -> Ast.rewrap x (Ast.DOTS [x])) rules in
	  let res =
            Ast.CocciRule
	    ("proto for "^rule_name,
	     (Ast.Dep rule_name,dropped_isos,Ast.Forall),
	     [mk_ast_code (Ast.rewrap x (Ast.Disj drules))],
	     [false],ruletype) in
	  ((mdef_metavars,minus),Some(metavars,res))
