(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* exports everything, used only by parser_cocci_menhir.mly *)
module Ast0 = Ast0_cocci
module Ast = Ast_cocci

let contains_string_constant = ref false

(* types for metavariable tokens *)
type info = Ast.meta_name * Ast0.pure * Data.clt
type midinfo =
    Ast.meta_name * Ast0.constraints * Ast.seed * Ast0.pure * Data.clt
type cstrinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type assignOpinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type binaryOpinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type expinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type tyinfo = Ast.meta_name * Ast0.typeC list * Ast0.pure * Data.clt
type list_info = Ast.meta_name * Ast.list_len * Ast0.constraints * Ast0.pure *
      Data.clt
type typed_expinfo =
    Ast.meta_name * Ast0.constraints * Ast0.pure *
      Ast0.typeC list option * Data.clt
type typed_expinfo_bitfield =
    Ast.meta_name * Ast0.constraints * Ast0.pure *
      Ast0.typeC list option * Data.clt * Ast.list_len option
type pos_info = Ast.meta_name * Ast0.constraints * Ast.meta_collect * Data.clt
type com_info = Ast.meta_name * Ast0.constraints * Data.clt

let make_info line logical_line logical_line_end offset col strbef straft 
    isSymbol ws =
  let new_pos_info =
    {Ast0.line_start = line; Ast0.line_end = line;
      Ast0.logical_start = logical_line; Ast0.logical_end = logical_line_end;
      Ast0.column = col; Ast0.offset = offset; } in
  { Ast0.pos_info = new_pos_info; Ast0.whitespace = ws;
    Ast0.attachable_start = true; Ast0.attachable_end = true;
    Ast0.mcode_start = []; Ast0.mcode_end = [];
    Ast0.strings_before = strbef; Ast0.strings_after = straft;
    Ast0.isSymbolIdent = isSymbol; }

let clt2info (_,line,logical_line,logical_line_end,offset,col,
              strbef,straft,pos,ws) =
 make_info line logical_line logical_line_end offset col strbef straft false ws

let drop_bef (arity,line,lline,llineend,offset,col,strbef,straft,pos,ws) =
  (arity,line,lline,llineend,offset,col,[],straft,pos,ws)

let drop_aft (arity,line,lline,llineend,offset,col,strbef,straft,pos,ws) =
  (arity,line,lline,llineend,offset,col,strbef,[],pos,ws)

(* used for #define, to put aft on ident/( *)
let get_aft (arity,line,lline,llineen,offset,col,strbef,straft,pos,ws) = straft

let set_aft aft
  (arity,line,lline,llineend,offset,col,strbef,_,pos,ws) =
  (arity,line,lline,llineend,offset,col,strbef,aft,pos,ws)

let drop_pos
  (arity,line,lline,llineend,offset,col,strbef,straft,pos,ws) =
  (arity,line,lline,llineend,offset,col,strbef,straft,[],ws)

let clt2mcode_ext str isSymbol = function
    (Data.MINUS,line,lline,llineend,offset,col,strbef,straft,pos,ws) ->
      (str,Ast0.NONE,
       make_info line lline llineend offset col strbef straft isSymbol ws,
       Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)),ref pos,-1)
  | (Data.OPTMINUS,line,lline,llineend,offset,col,strbef,straft,pos,ws)->
      (str,Ast0.OPT,
       make_info line lline llineend offset col strbef straft isSymbol ws,
       Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)),ref pos,-1)
  | (Data.PLUS,line,lline,llineend,offset,col,strbef,straft,pos,ws)        ->
      (str,Ast0.NONE,
       make_info line lline llineend offset col strbef straft isSymbol ws,
       Ast0.PLUS(Ast.ONE),ref pos,-1)
  | (Data.PLUSPLUS,line,lline,llineend,offset,col,strbef,straft,pos,ws)    ->
      (str,Ast0.NONE,
       make_info line lline llineend offset col strbef straft isSymbol ws,
       Ast0.PLUS(Ast.MANY),ref pos,-1)
  | (Data.CONTEXT,line,lline,llineend,offset,col,strbef,straft,pos,ws)     ->
      (str,Ast0.NONE,
       make_info line lline llineend offset col strbef straft isSymbol ws,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)),
       ref pos,-1)
  | (Data.OPT,line,lline,llineend,offset,col,strbef,straft,pos,ws)         ->
      (str,Ast0.OPT,
       make_info line lline llineend offset col strbef straft isSymbol ws,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)),
       ref pos,-1)

let clt2mcode name clt = clt2mcode_ext name false clt
let id3name   (name, _, clt) = name
let id2name   (name, clt) = name
let id2clt    (name, clt) = clt
let id2mcode  (name, clt) = clt2mcode name clt
let sym2mcode (name, clt) = clt2mcode_ext name true clt

let mkdots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Dots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkedots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Edots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkdpdots str dot =
  match str with
    "..." -> Ast0.wrap(Ast0.DPdots(clt2mcode str dot))
  | _ -> failwith "cannot happen"

let mkidots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Idots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkfdots str (dot,whencode) =
  match (str,whencode) with
    ("...",None) -> Ast0.wrap(Ast0.Fdots(clt2mcode str dot, None))
  | ("...",Some [w]) -> Ast0.wrap(Ast0.Fdots(clt2mcode str dot, Some w))
  | _ -> failwith "cannot happen"

let mkfdots_one str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Fdots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkpdots str dot =
  match str with
    "..." -> Ast0.wrap(Ast0.Pdots(clt2mcode str dot))
  | _ -> failwith "cannot happen"

let mkenumdots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.EnumDots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let arith_op ast_op left op right =
  let op' = Ast0.wrap (Ast0.Arith (clt2mcode ast_op op)) in  
  Ast0.wrap (Ast0.Binary(left, op', right))

let logic_op ast_op left op right =
  let op' = Ast0.wrap (Ast0.Logical (clt2mcode ast_op op)) in  
  Ast0.wrap (Ast0.Binary(left, op', right))

let make_cv cv ty =
  match cv with None -> ty | Some x -> Ast0.wrap (Ast0.ConstVol(x,ty))

let make_attr attr =
  Ast0.wrap(Ast0.Attribute(attr))

let top_dots l = Ast0.wrap l

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
    (fun inner cur -> Ast0.wrap (Ast0.Pointer (inner, Ast0.make_mcode "")))
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
    let info = !(Hashtbl.find Data.all_metadecls rule) in
    List.find (function mv -> Ast.get_meta_name mv = (rule,name)) info
  with
    Not_found ->
      raise
	(Semantic_cocci.Semantic("bad rule "^rule^" or bad variable "^name))

let meta_lookup rule name v =
  match lookup rule name with
    Ast.MetaScriptDecl(cell,_) ->
      (match !cell with
	Some x -> x
      | None -> cell := Some v; v)
  | res -> res

let check_meta_tyopt type_irrelevant v =
  let fail name =
    raise
      (Semantic_cocci.Semantic
	 ("incompatible inheritance declaration "^name)) in
  let type_equal ty0 ty1 =
    type_irrelevant ||
    Common.equal_option (Common.equal_list Unify_ast.fullType_compatible)
      ty0 ty1 in
  match v with
    Ast.MetaMetaDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaMetaDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaIdDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaIdDecl(_,_) | Ast.MetaFreshIdDecl(_,_) -> ()
      | x -> fail name)
  | Ast.MetaFreshIdDecl((rule,name),seed) ->
      raise
	(Semantic_cocci.Semantic
	   "can't inherit the freshness of an identifier")
  | Ast.MetaTypeDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaTypeDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaInitDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaInitDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaInitListDecl(Ast.NONE,(rule,name),len_name) ->
      (match meta_lookup rule name v with
	Ast.MetaInitListDecl(_,_,_) -> ()
      | _ -> fail name)
  | Ast.MetaListlenDecl((rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaListlenDecl(_) -> ()
      | _ -> fail name)
  | Ast.MetaParamDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaParamDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaParamListDecl(Ast.NONE,(rule,name),len_name) ->
      (match meta_lookup rule name v with
	Ast.MetaParamListDecl(_,_,_) -> ()
      | _ -> fail name)
  | Ast.MetaBinaryOperatorDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaBinaryOperatorDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaAssignmentOperatorDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaAssignmentOperatorDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaConstDecl(Ast.NONE,(rule,name),ty) ->
      (match meta_lookup rule name v with
	Ast.MetaConstDecl(_,_,ty1) when type_equal ty ty1 -> ()
      | _ -> fail name)
  | Ast.MetaErrDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaErrDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaExpDecl(Ast.NONE,(rule,name),ty,_) ->
      (match meta_lookup rule name v with
	Ast.MetaExpDecl(_,_,ty1,_) when type_equal ty ty1 -> ()
      | _ -> fail name)
  | Ast.MetaIdExpDecl(Ast.NONE,(rule,name),ty) ->
      (match meta_lookup rule name v with
	Ast.MetaIdExpDecl(_,_,ty1) when type_equal ty ty1 -> ()
      | _ -> fail name)
  | Ast.MetaLocalIdExpDecl(Ast.NONE,(rule,name),ty) ->
      (match meta_lookup rule name v with
	Ast.MetaLocalIdExpDecl(_,_,ty1) when type_equal ty ty1 -> ()
      | _ -> fail name)
  | Ast.MetaGlobalIdExpDecl(Ast.NONE,(rule,name),ty) ->
      (match meta_lookup rule name v with
	Ast.MetaGlobalIdExpDecl(_,_,ty1) when type_equal ty ty1 -> ()
      | _ -> fail name)
  | Ast.MetaExpListDecl(Ast.NONE,(rule,name),len_name) ->
      (match meta_lookup rule name v with
	Ast.MetaExpListDecl(_,_,_) -> ()
      | Ast.MetaParamListDecl(_,_,_) when not (!Flag.make_hrule = None) -> ()
      | _ -> fail name)
  | Ast.MetaDeclDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaDeclDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaFieldDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaFieldDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaFieldListDecl(Ast.NONE,(rule,name),len_name) ->
      (match meta_lookup rule name v with
	Ast.MetaFieldListDecl(_,_,_) -> ()
      | _ -> fail name)
  | Ast.MetaStmDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaStmDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaStmListDecl(Ast.NONE,(rule,name),len_name) ->
      (match meta_lookup rule name v with
	Ast.MetaStmListDecl(_,_,_) -> ()
      | _ -> fail name)
  | Ast.MetaFuncDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaFuncDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaLocalFuncDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaLocalFuncDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaPosDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaPosDecl(_,_) ->
	  if not (List.mem rule !Data.inheritable_positions) &&
	    not !Data.ignore_patch_or_match
	  then
	    raise
	      (Semantic_cocci.Semantic
		 ("position cannot be inherited over modifications: "^name))
      | _ -> fail name)
  | Ast.MetaComDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaComDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaFmtDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaFmtDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaFragListDecl(Ast.NONE,(rule,name),len) ->
      (match meta_lookup rule name v with
	Ast.MetaFragListDecl(_,_,_) -> ()
      | _ -> fail name)
  | Ast.MetaAttributeDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaAttributeDecl(_,_) -> ()
      | _ -> fail name)
  | Ast.MetaAnalysisDecl(analyzer,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaAnalysisDecl(analyzer1,_) ->
	  if analyzer = analyzer1
	  then ()
	  else fail name
      | _ -> fail name)
  | Ast.MetaDeclarerDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaDeclarerDecl(Ast.NONE,(rule,name)) -> ()
      | _ -> fail name)
  | Ast.MetaIteratorDecl(Ast.NONE,(rule,name)) ->
      (match meta_lookup rule name v with
	Ast.MetaIteratorDecl(Ast.NONE,(rule,name)) -> ()
      | _ -> fail name)
  | _ ->
      raise
	(Semantic_cocci.Semantic ("arity not allowed on imported declaration"))

let check_meta m = check_meta_tyopt false m

let check_inherited_constraint_without_type meta_name =
  match meta_name with
    None, _ -> failwith "constraint must be an inherited variable"
  | Some rule, name -> rule, name

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
      Common.Left (lenid,cstr) ->
	let lendec =
	  create_metadec Ast.NONE Ast0.Impure
	    (fun _ name _ check_meta -> check_meta(Ast.MetaListlenDecl(name)))
	    [lenid] current_rule in
	let cstr = Ast0toast.constraints cstr in
	let lenname =
	  match lendec with
	    [Common.Left (Ast.MetaListlenDecl(x))] -> Ast.MetaLen (x,cstr)
	  | [Common.Right (Ast.MetaListlenDecl(x))] -> Ast.MetaLen (x,cstr)
	  | _ -> failwith "unexpected length declaration" in
	(lendec,lenname)
    | Common.Right n -> ([],Ast.CstLen n) in
  lendec@
  create_metadec_with_constraints ar ispure (kindfn lenname) ids current_rule

(* ---------------------------------------------------------------------- *)

let str2inc s =
  let elements = Str.split (Str.regexp "/") s in
  List.map (function "..." -> Ast.IncDots | s -> Ast.IncPath s) elements

(* ---------------------------------------------------------------------- *)
(* declarations and statements *)

let meta_decl name =
  let (nm,cstr,pure,clt) = name in
  Ast0.wrap(Ast0.MetaDecl(clt2mcode nm clt,cstr,pure))

let meta_field name =
  let (nm,cstr,pure,clt) = name in
  Ast0.wrap(Ast0.MetaField(clt2mcode nm clt,cstr,pure))

let dolen clt = function
    Ast.AnyLen -> Ast0.AnyListLen
  | Ast.MetaLen (nm,cstr) ->
      let cstr' =
	(* Discard expression! (from Ast to Ast0) *)
	Ast.cstr_map Ast.empty_cstr_transformer cstr in
      Ast0.MetaListLen(clt2mcode nm clt,cstr')
  | Ast.CstLen n -> Ast0.CstListLen n

let meta_field_list name =
  let (nm,lenname,cstr,pure,clt) = name in
  Ast0.wrap(Ast0.MetaFieldList(clt2mcode nm clt,dolen clt lenname,cstr,pure))

let meta_stm name =
  let (nm,cstr,pure,clt) = name in
  Ast0.wrap(Ast0.MetaStmt(clt2mcode nm clt,cstr,pure))

let meta_stm_list name =
  let (nm,lenname,cstr,pure,clt) = name in
  Ast0.wrap(Ast0.MetaStmtList(clt2mcode nm clt,dolen clt lenname,cstr,pure))

let meta_dparam_list name =
  let (nm,lenname,cstr,pure,clt) = name in
  Ast0.wrap(Ast0.MetaDParamList(clt2mcode nm clt,dolen clt lenname,cstr,pure))

let exp_stm exp pv =
  Ast0.wrap(Ast0.ExprStatement (exp, clt2mcode ";" pv))

let make_fake_mcode _ = (Ast0.default_info(),Ast0.context_befaft(),-1)

let ifthen iff lp tst rp thn =
  Ast0.wrap(Ast0.IfThen(clt2mcode "if" iff,
    clt2mcode "(" lp,tst,clt2mcode ")" rp,thn,make_fake_mcode()))

let ifthenelse iff lp tst rp thn e els =
  Ast0.wrap(Ast0.IfThenElse(clt2mcode "if" iff,
    clt2mcode "(" lp,tst,clt2mcode ")" rp,thn,
    clt2mcode "else" e,els,make_fake_mcode()))

let forloop fr lp e1 sc1 e2 sc2 e3 rp s =
  Ast0.wrap(Ast0.For(clt2mcode "for" fr,clt2mcode "(" lp,
		     Ast0.wrap(Ast0.ForExp(e1,clt2mcode ";" sc1)),e2,
		     clt2mcode ";" sc2,e3,clt2mcode ")" rp,s,
		     make_fake_mcode()))

let forloop2 fr lp decl e2 sc2 e3 rp s =
  let bef = (Ast0.default_info(),Ast0.context_befaft()) in
  Ast0.wrap(Ast0.For(clt2mcode "for" fr,clt2mcode "(" lp,
		     Ast0.wrap(Ast0.ForDecl (bef,decl)),e2,
		     clt2mcode ";" sc2,e3,clt2mcode ")" rp,s,
		     make_fake_mcode()))

let whileloop w lp e rp s =
  Ast0.wrap(Ast0.While(clt2mcode "while" w,clt2mcode "(" lp,
		       e,clt2mcode ")" rp,s,make_fake_mcode()))

let doloop d s w lp e rp pv =
  Ast0.wrap(Ast0.Do(clt2mcode "do" d,s,clt2mcode "while" w,
		    clt2mcode "(" lp,e,clt2mcode ")" rp,
		    clt2mcode ";" pv))

let iterator i lp e rp s =
  Ast0.wrap(Ast0.Iterator(i,clt2mcode "(" lp,e,clt2mcode ")" rp,s,
			  make_fake_mcode()))

let switch s lp e rp lb d c rb =
  let d =
    List.map
      (function d ->
	Ast0.wrap(Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),d)))
      d in
  Ast0.wrap(Ast0.Switch(clt2mcode "switch" s,clt2mcode "(" lp,e,
			clt2mcode ")" rp,clt2mcode "{" lb,
			Ast0.wrap d,Ast0.wrap c,clt2mcode "}" rb))

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
      if Hashtbl.mem Data.all_metadecls n
      then raise (Semantic_cocci.Semantic ("repeated rule name"))
      else Some n
  | None -> None

let make_iso_rule_name_result n =
  (if Hashtbl.mem Data.all_metadecls n
  then raise (Semantic_cocci.Semantic ("repeated rule name")));
  Ast.CocciRulename
    (Some n,Ast.NoDep,[],[],Ast.Undetermined,Ast.AnyP (*discarded*))

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
    | Ast0.FileIn s when inverted -> Ast.NotFileIn s
    | Ast0.FileIn s -> Ast.FileIn s in
  match d with
    Ast0.NoDep -> Ast.NoDep
  | Ast0.FailDep -> Ast.FailDep
  | Ast0.ExistsDep d -> Ast.ExistsDep (loop false d)
  | Ast0.ForallDep d -> Ast.ForallDep (loop false d)

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

(* ---------------------------------------------------------------------- *)
(* decide whether an init list is ordered or unordered *)

let struct_initializer initlist =
  let rec loop i =
    match Ast0.unwrap i with
      Ast0.InitGccExt _ -> true
    | Ast0.InitGccName _ -> true
    | Ast0.OptIni i -> loop i
    | Ast0.MetaInit _ | Ast0.MetaInitList _ -> false (* ambiguous... *)
    | _ -> false in
  let l = Ast0.unwrap initlist in
  (l = []) || (List.exists loop l)

let drop_dot_commas initlist =
  let rec loop after_comma = function
      [] -> []
    | x::xs ->
	(match Ast0.unwrap x with
	  Ast0.Idots(dots,whencode) -> x :: (loop true xs)
	| Ast0.IComma(comma) when after_comma -> (*drop*) loop false xs
	| _ -> x :: (loop false xs)) in
  Ast0.rewrap initlist (loop false (Ast0.unwrap initlist))

(* ----------------------------------------------------------------------- *)
(* strings *)

type metavars =
    MFrag of (string Ast0.mcode -> Ast0.string_fragment)
  | MFmt of Ast0.string_format

let string_metavariables str clt =
  try
    let (name,constraints) = List.assoc str !Data.format_metavariables in
    MFmt(Ast0.wrap(Ast0.MetaFormat(clt2mcode name clt,constraints)))
  with Not_found ->
    try
      let (name,lenname,constraints) =
	List.assoc str !Data.format_list_metavariables in
      let lenname =
	match lenname with
	  Ast.AnyLen -> Ast0.AnyListLen
	| Ast.MetaLen (nm,cstr) ->
	    let cstr' =
	      (* Discard expression! (from Ast to Ast0) *)
	      Ast.cstr_map Ast.empty_cstr_transformer cstr in
	    Ast0.MetaListLen(clt2mcode nm clt,cstr')
	| Ast.CstLen n ->
	    if n < 1
	    then
	      failwith
		(Printf.sprintf "length of format list %s must be at least 1"
		   str)
	    else Ast0.CstListLen n in
      MFrag
	(fun pct ->
	  Ast0.wrap(
	  Ast0.MetaFormatList(pct,clt2mcode name clt,constraints,lenname)))
    with Not_found -> failwith "bad metavariable in string"

let pct_split str =
  let lst = Common.list_of_string str in
  let complete l =
    let l = List.rev l in
    String.concat "" (List.map (function c -> Printf.sprintf "%c" c) l) in
  let rec loop acc cur = function
      [] -> List.rev ((complete cur)::acc)
    | '%'::'%'::rest -> loop acc ('%'::'%'::cur) rest
    | ['%'] -> raise Parse_printf.Not_format_string
    | '%'::rest -> loop ((complete cur)::acc) [] rest
    | x :: rest -> loop acc (x :: cur) rest in
  loop [] [] lst

let parse_middle middle clt =
  let pieces = pct_split middle in
  let update_clt 
    (a,line,logical_line,logical_line_end,offset,col,strbef,straft,pos,ws) 
    chars =
    (* not sure how to update col: wrong if there are newlines *)
    (a,line,logical_line,logical_line_end,offset+chars,col+chars,strbef,straft,
     pos,ws)
  in
  match pieces with
    [] -> failwith "not possible"
  | fst::rest ->
      let chars = 1 in
      let clt = update_clt clt chars in
      let first =
	match fst with
	  "" -> []
	| "..." -> [Ast0.wrap(Ast0.Strdots(clt2mcode fst clt))]
	| _ -> [Ast0.wrap (Ast0.ConstantFragment(clt2mcode fst clt))] in
      let chars = String.length fst in
      let mkrest clt = function
	  "" -> []
	| "..." -> [Ast0.wrap(Ast0.Strdots(clt2mcode "..." clt))]
	| s -> [Ast0.wrap(Ast0.ConstantFragment(clt2mcode s clt))] in
      let rec loop chars = function
	  [] -> []
	| r::rs ->
	    (* there may be bugs in the management of clt here... *)
	    let clt = update_clt clt chars in
	    let pct = clt2mcode "%" clt in
	    let mkfmt d = Ast0.wrap (Ast0.ConstantFormat(clt2mcode d clt)) in
	    let rres =
	      match String.get r 0 with
		'@' ->
		  (match Str.split (Str.regexp "@") r with
		    first::rest ->
		      (* 3+ for the % and the starting and ending @ *)
		      let clt2 = update_clt clt (3+(String.length first)) in
		      (match string_metavariables first clt with
			MFmt fmtvar ->
			  (Ast0.wrap (Ast0.FormatFragment(pct,fmtvar)))::
			  (mkrest clt2 (String.concat "@" rest))
		      | MFrag fragvar ->
			  (fragvar pct)::
			  (mkrest clt2 (String.concat "@" rest)))
		  | _ -> failwith "bad string2")
	      | _ ->
		  match Parse_printf.get_format_string r with
		    (d,"") -> [Ast0.wrap (Ast0.FormatFragment(pct,mkfmt d))]
		  | (d,rest) ->
		      let clt2 = update_clt clt 1 in
		      (Ast0.wrap (Ast0.FormatFragment(pct,mkfmt d))) ::
		      (mkrest clt2 rest) in
	    (* +1 is for the %, which is not shown *)
	    rres @ (loop (chars + (String.length r) + 1) rs) in
      first @ (loop chars rest)

(* This doesn't allow a newline in the middle of a string except at a %,
perhaps not ideal *)
let check_no_duplicates l =
  let rec loop = function
      [] | [_] -> ()
    | x :: y :: rest ->
	(match (Ast0.unwrap x, Ast0.unwrap y) with
	  (Ast0.FormatFragment _, Ast0.FormatFragment _)
	| (Ast0.ConstantFragment _, Ast0.ConstantFragment _)
	| (Ast0.Strdots _, Ast0.Strdots _)
	| (Ast0.MetaFormatList _, Ast0.MetaFormatList _) ->
	    failwith "adjacent string fragments of the same kind not allowed"
	| _ -> loop (y :: rest)) in
  loop l

let update_line (c,l,ll,lle,lex_start,preceeding_spaces,cb,ca,m,ws) line =
  let l = l + line in
  let ll = ll + line in
  let lle = lle + line in
  let lex_start = if line > 0 then 0 else lex_start in
  let preceeding_spaces = if line > 0 then 0 else preceeding_spaces in
  (c,l,ll,lle,lex_start,preceeding_spaces,cb,ca,m,ws)

let drop_minus_plus l clt =
  let pclt (_,a,b,c,d,e,cb,ca,m,w) = (Data.PLUS,a,b,c,d,e,cb,ca,m,w) in
  let mclt (_,a,b,c,d,e,cb,ca,m,w) = (Data.MINUS,a,b,c,d,e,cb,ca,m,w) in
  (* not sure this works for all kinds of newlines, cf lexer *)
  let pieces = Str.split (Str.regexp "\n") l in
  if pieces = []
  then (1,[]) (* split gives [] on empty string? *)
  else
    let (line,pieces) =
      List.fold_left
	(function (line,prev) ->
	  let clt = update_line clt line in
	  function
	      "" ->
		let empty =
		  Ast0.wrap (Ast0.ConstantFragment(clt2mcode "" clt)) in
		(line+1, empty :: prev)
	    | cur ->
		let res =
		  let first = String.get cur 0 in
		  match first with
		    '-' ->
		      if !Flag_parsing_cocci.in_minus
		      then
			let str = String.sub cur 1 ((String.length cur) - 1) in
			(List.rev(parse_middle str (mclt clt))) @ prev
		      else prev 
		  | '+' ->
		      if !Flag_parsing_cocci.in_minus
		      then prev
		      else
			let str = String.sub cur 1 ((String.length cur) - 1) in
			(List.rev(parse_middle str (pclt clt))) @ prev
		  | _ -> (List.rev(parse_middle cur clt)) @ prev in
		(line+1,res))
	(0,[]) pieces in
    let res = List.rev pieces in
    check_no_duplicates res;
    (line,res)

let not_format_string str clt isChar =
  Ast0.wrap(Ast0.Constant (clt2mcode (Ast.String(str, isChar)) clt))

let nometas str =
  match Str.split_delim (Str.regexp "@") str with
    before::within::after::_ -> false (* need at least %@d@ *)
  | _ -> true

let parse_string str ((mc,b,c,d,e,f,g,h,i,_) as clt) isChar =
  match mc with
    Data.PLUS when nometas str ->
      (* not matched against, no internal changes possible, so no need to
	 parse *)
      not_format_string str clt isChar
   | _ ->
       if List.length(Str.split_delim (Str.regexp "%") str) > 1
       then
	 try
	   begin
	     let first = clt2mcode "\"" clt in
	     (*do not want subsequent tokens to inherit whitespace
		from first*)
	     let clt = (mc,b,c,d,e,f,g,h,i,"") in
	     let (line,middle) = drop_minus_plus str clt in
	     let middle = Ast0.wrap middle in
	     let last = clt2mcode "\"" (update_line clt (line-1)) in
	     contains_string_constant := true;
	     Ast0.wrap(Ast0.StringConstant(first,middle,last,isChar))
	   end
	 with Parse_printf.Not_format_string -> not_format_string str clt isChar
       else not_format_string str clt isChar

let unfloatl s =
  match Str.split_delim (Str.regexp_string ".") s with
    [thing;""] -> thing
  | _ -> failwith (Printf.sprintf "unexpected float in length range: %s\n" s)

let unfloatr s =
  match Str.split_delim (Str.regexp_string ".") s with
    ["";thing] -> thing
  | _ -> failwith (Printf.sprintf "unexpected float in length range: %s\n" s)

let mk_script nm pos lang params code prefix =
  let rule =
    String.concat "_" (Str.split (Str.regexp " ") !Ast0.rule_name) in
  let key = Printf.sprintf "%s_code_%s_0_%s" prefix rule (snd nm) in
  let code = String.concat " " code in
  let lang' = id2name lang in
  let code' =
    if lang' = "ocaml" then
      let (file, line) = pos in
      Printf.sprintf "\n# %d \"%s\"\n%s" line file code
    else code in
  let params =
    List.map
      (fun (rl, nm) ->
        let mv = lookup rl nm in
        ((rl, nm), mv))
      (List.rev params) in
  (key, lang', params, pos, code')
