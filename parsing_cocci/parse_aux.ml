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


# 0 "./parse_aux.ml"
(* exports everything, used only by parser_cocci_menhir.mly *)
module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* types for metavariable tokens *)
type info = Ast.meta_name * Ast0.pure * Data.clt
type midinfo =
    Ast.meta_name * Data.iconstraints * Ast.seed * Ast0.pure * Data.clt
type idinfo = Ast.meta_name * Data.iconstraints * Ast0.pure * Data.clt
type expinfo = Ast.meta_name * Data.econstraints * Ast0.pure * Data.clt
type tyinfo = Ast.meta_name * Ast0.typeC list * Ast0.pure * Data.clt
type list_info = Ast.meta_name * Ast.list_len * Ast0.pure * Data.clt
type typed_expinfo =
    Ast.meta_name * Data.econstraints * Ast0.pure *
      Type_cocci.typeC list option * Data.clt
type pos_info = Ast.meta_name * Data.pconstraints * Ast.meta_collect * Data.clt

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

let make_info line logical_line offset col strbef straft isSymbol =
  let new_pos_info =
    {Ast0.line_start = line; Ast0.line_end = line;
      Ast0.logical_start = logical_line; Ast0.logical_end = logical_line;
      Ast0.column = col; Ast0.offset = offset; } in
  { Ast0.pos_info = new_pos_info;
    Ast0.attachable_start = true; Ast0.attachable_end = true;
    Ast0.mcode_start = []; Ast0.mcode_end = [];
    Ast0.strings_before = strbef; Ast0.strings_after = straft;
    Ast0.isSymbolIdent = isSymbol; }

let clt2info (_,line,logical_line,offset,col,strbef,straft,pos) =
  make_info line logical_line offset col strbef straft false

let drop_bef (arity,line,lline,offset,col,strbef,straft,pos) =
  (arity,line,lline,offset,col,[],straft,pos)

let drop_aft (arity,line,lline,offset,col,strbef,straft,pos) =
  (arity,line,lline,offset,col,strbef,[],pos)

(* used for #define, to put aft on ident/( *)
let get_aft (arity,line,lline,offset,col,strbef,straft,pos) = straft

let set_aft aft (arity,line,lline,offset,col,strbef,_,pos) =
  (arity,line,lline,offset,col,strbef,aft,pos)

let drop_pos (arity,line,lline,offset,col,strbef,straft,pos) =
  (arity,line,lline,offset,col,strbef,straft,[])

let clt2mcode_ext str isSymbol = function
    (Data.MINUS,line,lline,offset,col,strbef,straft,pos)       ->
      (str,Ast0.NONE,make_info line lline offset col strbef straft isSymbol,
       Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)),ref pos,-1)
  | (Data.OPTMINUS,line,lline,offset,col,strbef,straft,pos)    ->
      (str,Ast0.OPT,make_info line lline offset col strbef straft isSymbol,
       Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)),ref pos,-1)
  | (Data.UNIQUEMINUS,line,lline,offset,col,strbef,straft,pos) ->
      (str,Ast0.UNIQUE,make_info line lline offset col strbef straft isSymbol,
       Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)),ref pos,-1)
  | (Data.PLUS,line,lline,offset,col,strbef,straft,pos)        ->
      (str,Ast0.NONE,make_info line lline offset col strbef straft isSymbol,
       Ast0.PLUS(Ast.ONE),ref pos,-1)
  | (Data.PLUSPLUS,line,lline,offset,col,strbef,straft,pos)        ->
      (str,Ast0.NONE,make_info line lline offset col strbef straft isSymbol,
       Ast0.PLUS(Ast.MANY),ref pos,-1)
  | (Data.CONTEXT,line,lline,offset,col,strbef,straft,pos)     ->
      (str,Ast0.NONE,make_info line lline offset col strbef straft isSymbol,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)),
       ref pos,-1)
  | (Data.OPT,line,lline,offset,col,strbef,straft,pos)         ->
      (str,Ast0.OPT,make_info line lline offset col strbef straft isSymbol,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)),
       ref pos,-1)
  | (Data.UNIQUE,line,lline,offset,col,strbef,straft,pos)      ->
      (str,Ast0.UNIQUE,make_info line lline offset col strbef straft isSymbol,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)),
       ref pos,-1)

let clt2mcode name clt = clt2mcode_ext name false clt
let id2name   (name, clt) = name
let id2clt    (name, clt) = clt
let id2mcode  (name, clt) = clt2mcode name clt
let sym2mcode (name, clt) = clt2mcode_ext name true clt

let mkdots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Dots(clt2mcode str dot, whencode))
  | "ooo" -> Ast0.wrap(Ast0.Circles(clt2mcode str dot, whencode))
  | "***" -> Ast0.wrap(Ast0.Stars(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkedots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Edots(clt2mcode str dot, whencode))
  | "ooo" -> Ast0.wrap(Ast0.Ecircles(clt2mcode str dot, whencode))
  | "***" -> Ast0.wrap(Ast0.Estars(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkdpdots str dot =
  match str with
    "..." -> Ast0.wrap(Ast0.DPdots(clt2mcode str dot))
  | "ooo" -> Ast0.wrap(Ast0.DPcircles(clt2mcode str dot))
  | _ -> failwith "cannot happen"

let mkidots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Idots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkddots str (dot,whencode) =
  match (str,whencode) with
    ("...",None) -> Ast0.wrap(Ast0.Ddots(clt2mcode str dot, None))
  | ("...",Some [w]) -> Ast0.wrap(Ast0.Ddots(clt2mcode str dot, Some w))
  | _ -> failwith "cannot happen"

let mkddots_one str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Ddots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkpdots str dot =
  match str with
    "..." -> Ast0.wrap(Ast0.Pdots(clt2mcode str dot))
  | "ooo" -> Ast0.wrap(Ast0.Pcircles(clt2mcode str dot))
  | _ -> failwith "cannot happen"

let arith_op ast_op left op right =
  Ast0.wrap
    (Ast0.Binary(left, clt2mcode (Ast.Arith ast_op) op, right))

let logic_op ast_op left op right =
  Ast0.wrap
    (Ast0.Binary(left, clt2mcode (Ast.Logical ast_op) op, right))

let make_cv cv ty =
  match cv with None -> ty | Some x -> Ast0.wrap (Ast0.ConstVol(x,ty))

let top_dots l =
  let circle x =
    match Ast0.unwrap x with Ast0.Circles(_) -> true | _ -> false in
  let star x =
    match Ast0.unwrap x with Ast0.Stars(_) -> true | _ -> false in
  if List.exists circle l
  then Ast0.wrap(Ast0.CIRCLES(l))
  else
    if List.exists star l
    then Ast0.wrap(Ast0.STARS(l))
    else Ast0.wrap(Ast0.DOTS(l))

(* here the offset is that of the first in the sequence of *s, not that of
each * individually *)
let pointerify ty m =
  List.fold_left
    (function inner ->
      function cur ->
	Ast0.wrap(Ast0.Pointer(inner,clt2mcode "*" cur)))
    ty m

let ty_pointerify ty m =
  List.fold_left
    (function inner -> function cur -> Type_cocci.Pointer(inner))
    ty m

let arrayify ty ar =
  List.fold_right
    (function (l,i,r) ->
      function rest ->
	Ast0.wrap (Ast0.Array(rest,clt2mcode "[" l,i,clt2mcode "]" r)))
    ar ty

(* Left is <=>, Right is =>.  Collect <=>s. *)
(* The parser should have done this, with precedences.  But whatever... *)
let iso_adjust first_fn fn first rest =
  let rec loop = function
      [] -> [[]]
    | (Common.Left x)::rest ->
	(match loop rest with
	  front::after -> (fn x::front)::after
	| _ -> failwith "not possible")
    | (Common.Right x)::rest ->
	(match loop rest with
	  front::after -> []::(fn x::front)::after
	| _ -> failwith "not possible") in
  match loop rest with
    front::after -> (first_fn first::front)::after
  | _ -> failwith "not possible"

let lookup rule name =
  try
    let info = Hashtbl.find Data.all_metadecls rule in
    List.find (function mv -> Ast.get_meta_name mv = (rule,name)) info
  with
    Not_found ->
      raise
	(Semantic_cocci.Semantic("bad rule "^rule^" or bad variable "^name))

let check_meta_tyopt type_irrelevant = function
    Ast.MetaMetaDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaMetaDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaIdDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaIdDecl(_,_) | Ast.MetaFreshIdDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaFreshIdDecl((rule,name),seed) ->
      raise
	(Semantic_cocci.Semantic
	   "can't inherit the freshness of an identifier")
  | Ast.MetaTypeDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaTypeDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaInitDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaInitDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaInitListDecl(Ast.NONE,(rule,name),len_name) ->
      (match lookup rule name with
	Ast.MetaInitListDecl(_,_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaListlenDecl((rule,name)) ->
      (match lookup rule name with
	Ast.MetaListlenDecl(_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaParamDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaParamDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaParamListDecl(Ast.NONE,(rule,name),len_name) ->
      (match lookup rule name with
	Ast.MetaParamListDecl(_,_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaConstDecl(Ast.NONE,(rule,name),ty) ->
      (match lookup rule name with
	Ast.MetaConstDecl(_,_,ty1) when type_irrelevant or ty = ty1 -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaErrDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaErrDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaExpDecl(Ast.NONE,(rule,name),ty) ->
      (match lookup rule name with
	Ast.MetaExpDecl(_,_,ty1) when type_irrelevant or ty = ty1 -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaIdExpDecl(Ast.NONE,(rule,name),ty) ->
      (match lookup rule name with
	Ast.MetaIdExpDecl(_,_,ty1) when type_irrelevant or ty = ty1 -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaLocalIdExpDecl(Ast.NONE,(rule,name),ty) ->
      (match lookup rule name with
	Ast.MetaLocalIdExpDecl(_,_,ty1) when type_irrelevant or ty = ty1 -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaExpListDecl(Ast.NONE,(rule,name),len_name) ->
      (match lookup rule name with
	Ast.MetaExpListDecl(_,_,_) -> ()
      | Ast.MetaParamListDecl(_,_,_) when not (!Flag.make_hrule = None) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaDeclDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaDeclDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaFieldDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaFieldDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaFieldListDecl(Ast.NONE,(rule,name),len_name) ->
      (match lookup rule name with
	Ast.MetaFieldListDecl(_,_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaStmDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaStmDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaStmListDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaStmListDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaFuncDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaFuncDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaLocalFuncDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaLocalFuncDecl(_,_) -> ()
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | Ast.MetaPosDecl(Ast.NONE,(rule,name)) ->
      (match lookup rule name with
	Ast.MetaPosDecl(_,_) ->
	  if not (List.mem rule !Data.inheritable_positions) &&
	    not !Data.ignore_patch_or_match
	  then
	    raise
	      (Semantic_cocci.Semantic
		 ("position cannot be inherited over modifications: "^name))
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       ("incompatible inheritance declaration "^name)))
  | _ ->
      raise
	(Semantic_cocci.Semantic ("arity not allowed on imported declaration"))

let check_meta m = check_meta_tyopt false m

let check_inherited_constraint meta_name fn =
  match meta_name with
    (None,_) -> failwith "constraint must be an inherited variable"
  | (Some rule,name) ->
      let i = (rule,name) in
      check_meta_tyopt true (fn i);
      i

let create_metadec ar ispure kindfn ids current_rule =
  List.concat
    (List.map
       (function (rule,nm) ->
	 let (rule,checker) =
	   match rule with
	     None -> ((current_rule,nm),function x -> [Common.Left x])
	   | Some rule ->
	       ((rule,nm),
		function x -> check_meta x; [Common.Right x]) in
	 kindfn ar rule ispure checker)
       ids)


let create_metadec_virt ar ispure kindfn ids current_rule =
  List.concat
    (List.map
       (function nm ->
	 let checker = function x -> [Common.Right x] in
	 kindfn ar nm ispure checker !Flag.defined_virtual_env)
       ids)

let create_fresh_metadec kindfn ids current_rule =
  List.concat
    (List.map
       (function ((rule,nm),seed) ->
	 let (rule,checker) =
	   match rule with
	     None -> ((current_rule,nm),function x -> [Common.Left x])
	   | Some rule ->
	       ((rule,nm),
		function x -> check_meta x; [Common.Right x]) in
	 kindfn rule checker seed)
       ids)

let create_metadec_with_constraints ar ispure kindfn ids current_rule =
  List.concat
    (List.map
       (function ((rule,nm),constraints) ->
	 let (rule,checker) =
	   match rule with
	       None -> ((current_rule,nm),function x -> [Common.Left x])
	     | Some rule ->
		 ((rule,nm),
		  function x -> check_meta x; [Common.Right x]) in
	   kindfn ar rule ispure checker constraints)
       ids)

let create_metadec_ty ar ispure kindfn ids current_rule =
  List.concat
    (List.map
       (function ((rule,nm),constraints) ->
	 let (rule,checker) =
	   match rule with
	     None -> ((current_rule,nm),function x -> [Common.Left x])
	   | Some rule ->
	       ((rule,nm),
		function x -> check_meta x; [Common.Right x]) in
	 kindfn ar rule ispure checker constraints)
       ids)

let create_len_metadec ar ispure kindfn lenid ids current_rule =
  let (lendec,lenname) =
    match lenid with
      Common.Left lenid ->
	let lendec =
	  create_metadec Ast.NONE Ast0.Impure
	    (fun _ name _ check_meta -> check_meta(Ast.MetaListlenDecl(name)))
	    [lenid] current_rule in
	let lenname =
	  match lendec with
	    [Common.Left (Ast.MetaListlenDecl(x))] -> Ast.MetaLen x
	  | [Common.Right (Ast.MetaListlenDecl(x))] -> Ast.MetaLen x
	  | _ -> failwith "unexpected length declaration" in
	(lendec,lenname)
    | Common.Right n -> ([],Ast.CstLen n) in
  lendec@(create_metadec ar ispure (kindfn lenname) ids current_rule)

(* ---------------------------------------------------------------------- *)

let str2inc s =
  let elements = Str.split (Str.regexp "/") s in
  List.map (function "..." -> Ast.IncDots | s -> Ast.IncPath s) elements

(* ---------------------------------------------------------------------- *)
(* declarations and statements *)

let meta_decl name =
  let (nm,pure,clt) = name in
  Ast0.wrap(Ast0.MetaDecl(clt2mcode nm clt,pure))

let meta_field name =
  let (nm,pure,clt) = name in
  Ast0.wrap(Ast0.MetaField(clt2mcode nm clt,pure))

let meta_field_list name =
  let (nm,lenname,pure,clt) = name in
  let lenname =
    match lenname with
      Ast.AnyLen -> Ast0.AnyListLen
    | Ast.MetaLen nm -> Ast0.MetaListLen(clt2mcode nm clt)
    | Ast.CstLen n -> Ast0.CstListLen n in
  Ast0.wrap(Ast0.MetaFieldList(clt2mcode nm clt,lenname,pure))

let meta_stm name =
  let (nm,pure,clt) = name in
  Ast0.wrap(Ast0.MetaStmt(clt2mcode nm clt,pure))

let exp_stm exp pv =
  Ast0.wrap(Ast0.ExprStatement (exp, clt2mcode ";" pv))

let ifthen iff lp tst rp thn =
  Ast0.wrap(Ast0.IfThen(clt2mcode "if" iff,
    clt2mcode "(" lp,tst,clt2mcode ")" rp,thn,
    (Ast0.default_info(),Ast0.context_befaft())))

let ifthenelse iff lp tst rp thn e els =
  Ast0.wrap(Ast0.IfThenElse(clt2mcode "if" iff,
    clt2mcode "(" lp,tst,clt2mcode ")" rp,thn,
    clt2mcode "else" e,els,
    (Ast0.default_info(),Ast0.context_befaft())))

let forloop fr lp e1 sc1 e2 sc2 e3 rp s =
  Ast0.wrap(Ast0.For(clt2mcode "for" fr,clt2mcode "(" lp,
		     Ast0.wrap(Ast0.ForExp(e1,clt2mcode ";" sc1)),e2,
		     clt2mcode ";" sc2,e3,clt2mcode ")" rp,s,
		     (Ast0.default_info(),Ast0.context_befaft())))

let forloop2 fr lp decl e2 sc2 e3 rp s =
  let bef = (Ast0.default_info(),Ast0.context_befaft()) in
  Ast0.wrap(Ast0.For(clt2mcode "for" fr,clt2mcode "(" lp,
		     Ast0.wrap(Ast0.ForDecl (bef,decl)),e2,
		     clt2mcode ";" sc2,e3,clt2mcode ")" rp,s,
		     (Ast0.default_info(),Ast0.context_befaft())))

let whileloop w lp e rp s =
  Ast0.wrap(Ast0.While(clt2mcode "while" w,clt2mcode "(" lp,
		       e,clt2mcode ")" rp,s,
		       (Ast0.default_info(),Ast0.context_befaft())))

let doloop d s w lp e rp pv =
  Ast0.wrap(Ast0.Do(clt2mcode "do" d,s,clt2mcode "while" w,
		    clt2mcode "(" lp,e,clt2mcode ")" rp,
		    clt2mcode ";" pv))

let iterator i lp e rp s =
  Ast0.wrap(Ast0.Iterator(i,clt2mcode "(" lp,e,clt2mcode ")" rp,s,
			  (Ast0.default_info(),Ast0.context_befaft())))

let switch s lp e rp lb d c rb =
  let d =
    List.map
      (function d ->
	Ast0.wrap(Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),d)))
      d in
  Ast0.wrap(Ast0.Switch(clt2mcode "switch" s,clt2mcode "(" lp,e,
			clt2mcode ")" rp,clt2mcode "{" lb,
			Ast0.wrap(Ast0.DOTS(d)),
			Ast0.wrap(Ast0.DOTS(c)),clt2mcode "}" rb))

let ret_exp r e pv =
  Ast0.wrap(Ast0.ReturnExpr(clt2mcode "return" r,e,clt2mcode ";" pv))

let ret r pv =
  Ast0.wrap(Ast0.Return(clt2mcode "return" r,clt2mcode ";" pv))

let break b pv =
  Ast0.wrap(Ast0.Break(clt2mcode "break" b,clt2mcode ";" pv))

let cont c pv =
  Ast0.wrap(Ast0.Continue(clt2mcode "continue" c,clt2mcode ";" pv))

let label i dd =
  Ast0.wrap(Ast0.Label(i,clt2mcode ":" dd))

let goto g i pv =
  Ast0.wrap(Ast0.Goto(clt2mcode "goto" g,i,clt2mcode ";" pv))

let seq lb s rb =
  Ast0.wrap(Ast0.Seq(clt2mcode "{" lb,s,clt2mcode "}" rb))

(* ---------------------------------------------------------------------- *)

let check_rule_name = function
    Some nm ->
      let n = id2name nm in
      (try let _ =  Hashtbl.find Data.all_metadecls n in
      raise (Semantic_cocci.Semantic ("repeated rule name"))
      with Not_found -> Some n)
  | None -> None

let make_iso_rule_name_result n =
  (try let _ =  Hashtbl.find Data.all_metadecls n in
  raise (Semantic_cocci.Semantic ("repeated rule name"))
  with Not_found -> ());
  Ast.CocciRulename
    (Some n,Ast.NoDep,[],[],Ast.Undetermined,false (*discarded*))

let fix_dependencies d =
  let rec loop inverted = function
      Ast0.Dep s when inverted -> Ast.AntiDep s
    | Ast0.Dep s -> Ast.Dep s
    | Ast0.AntiDep d -> loop (not inverted) d
    | Ast0.EverDep s when inverted -> Ast.NeverDep s
    | Ast0.EverDep s -> Ast.EverDep s
    | Ast0.NeverDep s when inverted -> Ast.EverDep s
    | Ast0.NeverDep s -> Ast.NeverDep s
    | Ast0.AndDep(d1,d2) when inverted ->
	Ast.OrDep(loop inverted d1,loop inverted d2)
    | Ast0.AndDep(d1,d2) ->
	Ast.AndDep(loop inverted d1,loop inverted d2)
    | Ast0.OrDep(d1,d2) when inverted ->
	Ast.AndDep(loop inverted d1,loop inverted d2)
    | Ast0.OrDep(d1,d2) ->
	Ast.OrDep(loop inverted d1,loop inverted d2)
    | Ast0.NoDep -> Ast.NoDep
    | Ast0.FailDep -> Ast.FailDep in
  loop false d

let make_cocci_rule_name_result nm d i a e ee =
  Ast.CocciRulename (check_rule_name nm,fix_dependencies d,i,a,e,ee)

let make_generated_rule_name_result nm d i a e ee =
  Ast.GeneratedRulename (check_rule_name nm,fix_dependencies d,i,a,e,ee)

let make_script_rule_name_result lang nm deps =
  let l = id2name lang in
  Ast.ScriptRulename (check_rule_name nm,l,fix_dependencies deps)

let make_initial_script_rule_name_result lang deps =
  let l = id2name lang in
  Ast.InitialScriptRulename(None,l,fix_dependencies deps)

let make_final_script_rule_name_result lang deps =
  let l = id2name lang in
  Ast.FinalScriptRulename(None,l,fix_dependencies deps)

(* Allows type alone only when it is void and only when there is only one
    parameter.  This avoids ambiguity problems in the parser. *)
let verify_parameter_declarations = function
    [] -> ()
  | [x] ->
      (match Ast0.unwrap x with
	Ast0.Param(t, None) ->
	  (match Ast0.unwrap t with
	    Ast0.BaseType(Ast.VoidType,_) -> ()
	  | _ ->
	      failwith
		(Printf.sprintf
		   "%d: only void can be a parameter without an identifier"
		   (Ast0.get_line t)))
      |	_ -> ())
  | l ->
      List.iter
	(function x ->
	  match Ast0.unwrap x with
	    Ast0.Param(t, None) ->
	      failwith
		(Printf.sprintf
		   "%d: only void alone can be a parameter without an identifier"
		   (Ast0.get_line t))
	  | _ -> ())
	l

(* ---------------------------------------------------------------------- *)
(* decide whether an init list is ordered or unordered *)

let struct_initializer initlist =
  let rec loop i =
    match Ast0.unwrap i with
      Ast0.InitGccExt _ -> true
    | Ast0.InitGccName _ -> true
    | Ast0.OptIni i | Ast0.UniqueIni i -> loop i
    | Ast0.MetaInit _ | Ast0.MetaInitList _ -> false (* ambiguous... *)
    | _ -> false in
  let l = Ast0.undots initlist in
  (l = []) or (List.exists loop l)

let drop_dot_commas initlist =
  match Ast0.unwrap initlist with
    Ast0.DOTS(l) ->
      let rec loop after_comma = function
	  [] -> []
	| x::xs ->
	    (match Ast0.unwrap x with
	      Ast0.Idots(dots,whencode) -> x :: (loop true xs)
	    | Ast0.IComma(comma) when after_comma -> (*drop*) loop false xs
	    | _ -> x :: (loop false xs)) in
      Ast0.rewrap initlist (Ast0.DOTS(loop false l))
  | _ -> failwith "not supported"
