(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Detects subtrees that are all minus/plus and nodes that are "binding
context nodes".  The latter is a node whose structure and immediate tokens
are the same in the minus and plus trees, and such that for every child,
the set of context nodes in the child subtree is the same in the minus and
plus subtrees. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module U = Unparse_ast0

(* --------------------------------------------------------------------- *)
(* Generic access to code *)

let set_mcodekind x mcodekind =
  match x with
    Ast0.DotsExprTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsInitTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsParamTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsStmtTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsDeclTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsFieldTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsEnumDeclTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsCaseTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DotsDefParamTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.IdentTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.ExprTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.AssignOpTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.BinaryOpTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.ArgExprTag(d) | Ast0.TestExprTag(d) ->
      failwith "not possible - iso only"
  | Ast0.TypeCTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.ParamTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.DeclTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.FieldTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.EnumDeclTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.InitTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.StmtTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.ForInfoTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.CaseLineTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.StringFragmentTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.AttributeTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.AttrArgTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.TopTag(d) -> Ast0.set_mcodekind d mcodekind
  | Ast0.IsoWhenTag(_) -> failwith "only within iso phase"
  | Ast0.IsoWhenTTag(_) -> failwith "only within iso phase"
  | Ast0.IsoWhenFTag(_) -> failwith "only within iso phase"
  | Ast0.MetaPosTag(p) -> failwith "invisible at this stage"
  | Ast0.HiddenVarTag(p) -> failwith "hiddenvar only within iso phase"
  | Ast0.WhenTag _ -> failwith "whentag only within iso phase"

let set_index x index =
  match x with
    Ast0.DotsExprTag(d) -> Ast0.set_index d index
  | Ast0.DotsInitTag(d) -> Ast0.set_index d index
  | Ast0.DotsParamTag(d) -> Ast0.set_index d index
  | Ast0.DotsStmtTag(d) -> Ast0.set_index d index
  | Ast0.DotsDeclTag(d) -> Ast0.set_index d index
  | Ast0.DotsFieldTag(d) -> Ast0.set_index d index
  | Ast0.DotsEnumDeclTag(d) -> Ast0.set_index d index
  | Ast0.DotsCaseTag(d) -> Ast0.set_index d index
  | Ast0.DotsDefParamTag(d) -> Ast0.set_index d index
  | Ast0.IdentTag(d) -> Ast0.set_index d index
  | Ast0.ExprTag(d) -> Ast0.set_index d index
  | Ast0.AssignOpTag(d) -> Ast0.set_index d index
  | Ast0.BinaryOpTag(d) -> Ast0.set_index d index
  | Ast0.ArgExprTag(d) | Ast0.TestExprTag(d) ->
      failwith "not possible - iso only"
  | Ast0.TypeCTag(d) -> Ast0.set_index d index
  | Ast0.ParamTag(d) -> Ast0.set_index d index
  | Ast0.InitTag(d) -> Ast0.set_index d index
  | Ast0.DeclTag(d) -> Ast0.set_index d index
  | Ast0.FieldTag(d) -> Ast0.set_index d index
  | Ast0.EnumDeclTag(d) -> Ast0.set_index d index
  | Ast0.StmtTag(d) -> Ast0.set_index d index
  | Ast0.ForInfoTag(d) -> Ast0.set_index d index
  | Ast0.CaseLineTag(d) -> Ast0.set_index d index
  | Ast0.StringFragmentTag(d) -> Ast0.set_index d index
  | Ast0.AttributeTag(d) -> Ast0.set_index d index
  | Ast0.AttrArgTag(d) -> Ast0.set_index d index
  | Ast0.TopTag(d) -> Ast0.set_index d index
  | Ast0.IsoWhenTag(_) -> failwith "only within iso phase"
  | Ast0.IsoWhenTTag(_) -> failwith "only within iso phase"
  | Ast0.IsoWhenFTag(_) -> failwith "only within iso phase"
  | Ast0.MetaPosTag(p) -> failwith "invisible at this stage"
  | Ast0.HiddenVarTag(p) -> failwith "hiddenvar only within iso phase"
  | Ast0.WhenTag _ -> failwith "whentag only within iso phase"

let get_index = function
    Ast0.DotsExprTag(d) -> Index.expression_dots d
  | Ast0.DotsInitTag(d) -> Index.initialiser_dots d
  | Ast0.DotsParamTag(d) -> Index.parameter_dots d
  | Ast0.DotsStmtTag(d) -> Index.statement_dots d
  | Ast0.DotsDeclTag(d) -> Index.declaration_dots d
  | Ast0.DotsFieldTag(d) -> Index.field_dots d
  | Ast0.DotsEnumDeclTag(d) -> Index.enum_decl_dots d
  | Ast0.DotsCaseTag(d) -> Index.case_line_dots d
  | Ast0.DotsDefParamTag(d) -> Index.define_param_dots d
  | Ast0.IdentTag(d) -> Index.ident d
  | Ast0.ExprTag(d) -> Index.expression d
  | Ast0.AssignOpTag(d) -> Index.assignOp d
  | Ast0.BinaryOpTag(d) -> Index.binaryOp d
  | Ast0.ArgExprTag(d) | Ast0.TestExprTag(d) ->
      failwith "not possible - iso only"
  | Ast0.TypeCTag(d) -> Index.typeC d
  | Ast0.ParamTag(d) -> Index.parameterTypeDef d
  | Ast0.InitTag(d) -> Index.initialiser d
  | Ast0.DeclTag(d) -> Index.declaration d
  | Ast0.FieldTag(d) -> Index.field d
  | Ast0.EnumDeclTag(d) -> Index.enum_decl d
  | Ast0.StmtTag(d) -> Index.statement d
  | Ast0.ForInfoTag(d) -> Index.forinfo d
  | Ast0.CaseLineTag(d) -> Index.case_line d
  | Ast0.StringFragmentTag(d) -> Index.string_fragment d
  | Ast0.AttributeTag(d) -> Index.attribute d
  | Ast0.AttrArgTag(d) -> Index.attr_arg d
  | Ast0.TopTag(d) -> Index.top_level d
  | Ast0.IsoWhenTag(_) -> failwith "only within iso phase"
  | Ast0.IsoWhenTTag(_) -> failwith "only within iso phase"
  | Ast0.IsoWhenFTag(_) -> failwith "only within iso phase"
  | Ast0.MetaPosTag(p) -> failwith "invisible at this stage"
  | Ast0.HiddenVarTag(p) -> failwith "hiddenvar only within iso phase"
  | Ast0.WhenTag _ -> failwith "whentag only within iso phase"

(* --------------------------------------------------------------------- *)
(* Collect the line numbers of the plus code.  This is used for disjunctions.
It is not completely clear why this is necessary, but it seems like an easy
fix for whatever is the problem that is discussed in disj_cases *)

let plus_lines = ref ([] : int list)

let insert n =
  let rec loop = function
      [] -> [n]
    | x::xs ->
	match compare n x with
	  1 -> x::(loop xs)
	| 0 -> x::xs
	| -1 -> n::x::xs
	| _ -> failwith "not possible" in
  plus_lines := loop !plus_lines

let find n min max =
  let rec loop = function
      [] -> (min,max)
    | [x] -> if n < x then (min,x) else (x,max)
    | x1::x2::rest ->
	if n < x1
	then (min,x1)
	else if n > x1 && n < x2 then (x1,x2) else loop (x2::rest) in
  loop !plus_lines

let collect_plus_lines top =
  plus_lines := [];
  let bind x y = () in
  let option_default = () in
  let donothing r k e = k e in
  let mcode (_,_,info,mcodekind,_,_) =
    match mcodekind with
      Ast0.PLUS _ -> insert info.Ast0.pos_info.Ast0.line_start
    | _ -> () in
  let statement r k s =
    let mcode info bef = mcode ((),(),info,bef,(),-1) in
    match Ast0.unwrap s with
	(* cases for everything with extra mcode *)
      | Ast0.Decl((info,bef),_) ->
	  bind (mcode info bef) (k s)
      |	Ast0.FunDecl((info,bef),_,_,_,_,_,_,_,_,_,(ainfo,aft)) ->
	  bind (mcode info bef) (bind (k s) (mcode ainfo aft))
      | Ast0.IfThen(_,_,_,_,_,(info,aft,adj))
      | Ast0.IfThenElse(_,_,_,_,_,_,_,(info,aft,adj))
      | Ast0.Iterator(_,_,_,_,_,(info,aft,adj))
      | Ast0.While(_,_,_,_,_,(info,aft,adj))
      | Ast0.For(_,_,_,_,_,_,_,_,(info,aft,adj)) ->
	  bind (k s) (mcode info aft)
      |	_ -> k s in
  let fn =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing statement donothing donothing donothing
      donothing donothing donothing in
  fn.VT0.combiner_rec_top_level top

(* --------------------------------------------------------------------- *)

type kind =
    Neutral | AllMarked of Ast.count | NotAllMarked (* marked means + or - *)

(* --------------------------------------------------------------------- *)
(* The first part analyzes each of the minus tree and the plus tree
separately *)

(* ints are unique token indices (offset field) *)
type node =
    Token (* tokens *) of kind * int (* unique index *) * Ast0.mcodekind *
	int list (* context tokens *)
  | Recursor (* children *) of kind *
	int list (* indices of all tokens at the level below *) *
	Ast0.mcodekind list (* tokens at the level below *) *
	int list
  | Bind (* neighbors *) of kind *
	int list (* indices of all tokens at current level *) *
	Ast0.mcodekind list (* tokens at current level *) *
	int list (* indices of all tokens at the level below *) *
	Ast0.mcodekind list (* tokens at the level below *)
	* int list list

let kind2c = function
    Neutral -> "neutral"
  | AllMarked _ -> "allmarked"
  | NotAllMarked -> "notallmarked"

let node2c = function
    Token(k,_,_,_) -> Printf.sprintf "token %s\n" (kind2c k)
  | Recursor(k,_,_,_) -> Printf.sprintf "recursor %s\n" (kind2c k)
  | Bind(k,_,_,_,_,_) -> Printf.sprintf "bind %s\n" (kind2c k)

(* goal: detect negative in both tokens and recursors, or context only in
tokens *)
let bind c1 c2 =
  let lub = function
      (k1,k2) when k1 = k2 -> k1
    | (Neutral,AllMarked c) -> AllMarked c
    | (AllMarked c,Neutral) -> AllMarked c
    | _ -> NotAllMarked in
  match (c1,c2) with
    (* token/token *)
    (* there are tokens at this level, so ignore the level below *)
    (Token(k1,i1,t1,l1),Token(k2,i2,t2,l2)) ->
      Bind(lub(k1,k2),[i1;i2],[t1;t2],[],[],[l1;l2])

    (* token/recursor *)
    (* there are tokens at this level, so ignore the level below *)
  | (Token(k1,i1,t1,l1),Recursor(k2,_,_,l2)) ->
      Bind(lub(k1,k2),[i1],[t1],[],[],[l1;l2])
  | (Recursor(k1,_,_,l1),Token(k2,i2,t2,l2)) ->
      Bind(lub(k1,k2),[i2],[t2],[],[],[l1;l2])

    (* token/bind *)
    (* there are tokens at this level, so ignore the level below *)
  | (Token(k1,i1,t1,l1),Bind(k2,i2,t2,_,_,l2)) ->
      Bind(lub(k1,k2),i1::i2,t1::t2,[],[],l1::l2)
  | (Bind(k1,i1,t1,_,_,l1),Token(k2,i2,t2,l2)) ->
      Bind(lub(k1,k2),i1@[i2],t1@[t2],[],[],l1@[l2])

    (* recursor/bind *)
  | (Recursor(k1,bi1,bt1,l1),Bind(k2,i2,t2,bi2,bt2,l2)) ->
      Bind(lub(k1,k2),i2,t2,bi1@bi2,bt1@bt2,l1::l2)
  | (Bind(k1,i1,t1,bi1,bt1,l1),Recursor(k2,bi2,bt2,l2)) ->
      Bind(lub(k1,k2),i1,t1,bi1@bi2,bt1@bt2,l1@[l2])

    (* recursor/recursor and bind/bind - not likely to ever occur *)
  | (Recursor(k1,bi1,bt1,l1),Recursor(k2,bi2,bt2,l2)) ->
      Bind(lub(k1,k2),[],[],bi1@bi2,bt1@bt2,[l1;l2])
  | (Bind(k1,i1,t1,bi1,bt1,l1),Bind(k2,i2,t2,bi2,bt2,l2)) ->
      Bind(lub(k1,k2),i1@i2,t1@t2,bi1@bi2,bt1@bt2,l1@l2)


let option_default = (*Bind(Neutral,[],[],[],[],[])*)
  Recursor(Neutral,[],[],[])

let contains_added_strings info =
  let unsafe l =
    List.exists
      (fun (str,_) ->
	match str with
	  (Ast.Noindent s | Ast.Indent s) -> not (Stdcompat.String.trim s = "")
	| Ast.Space _ -> true (* adds a thing with space around it *))
      l in
  (* If this is true, eg if (x) return 0; will be broken up into
     two rule elems.  Not sure why that is needed, but surely it is not needed
     when it is just whitespace that is added. *)
  unsafe info.Ast0.strings_before || unsafe info.Ast0.strings_after

let mcode (_,_,info,mcodekind,pos,_) =
  let offset = info.Ast0.pos_info.Ast0.offset in
  match mcodekind with
    Ast0.MINUS(_) -> Token(AllMarked Ast.ONE,offset,mcodekind,[])
  | Ast0.PLUS c -> Token(AllMarked c,offset,mcodekind,[])
  | Ast0.CONTEXT(_) -> Token(NotAllMarked,offset,mcodekind,[offset])
  | _ -> failwith "not possible"

let neutral_mcode (_,_,info,mcodekind,pos,_) =
  let offset = info.Ast0.pos_info.Ast0.offset in
  match mcodekind with
    Ast0.MINUS(_) -> Token(Neutral,offset,mcodekind,[])
  | Ast0.PLUS _ -> Token(Neutral,offset,mcodekind,[])
  | Ast0.CONTEXT(_) -> Token(Neutral,offset,mcodekind,[offset])
  | _ -> failwith "not possible"

(* neutral for context; used for mcode in bef aft nodes that don't represent
anything if they don't contain some information *)
let nc_mcode (_,_,info,mcodekind,pos,_) =
  (* distinguish from the offset of some real token *)
  let offset = (-1) * info.Ast0.pos_info.Ast0.offset in
  match mcodekind with
    Ast0.MINUS(_) -> Token(AllMarked Ast.ONE,offset,mcodekind,[])
  | Ast0.PLUS c -> Token(AllMarked c,offset,mcodekind,[])
  | Ast0.CONTEXT(_) ->
      (* Unlike the other mcode cases, we drop the offset from the context
	 offsets.  This is because we don't know whether the term this is
	 associated with is - or context.  In any case, the context offsets are
	 used for identification, and this invisible node should not be needed
	 for this purpose. *)
      if contains_added_strings info
      then
	(* can we have ++ for strings? *)
	Token(NotAllMarked,offset,mcodekind,[])
      else Token(Neutral,offset,mcodekind,[])
  | _ -> failwith "not possible"

let is_context = function Ast0.CONTEXT(_) -> true | _ -> false

let union_all l = List.fold_left Common.union_set [] l

(* is minus is true when we are processing minus code that might be
intermingled with plus code.  it is used in disj_cases *)
let classify is_minus all_marked table code =
  let mkres builder k il tl bil btl l e =
    (match k with
      AllMarked count ->
	Ast0.set_mcodekind e (all_marked count) (* definitive *)
    | _ ->
      let check_index il tl =
	if List.for_all is_context tl
	then
	  (let e1 = builder e in
	  let index = (get_index e1)@il in
	  try
	    let _ = Hashtbl.find table index in
	    failwith
	      (Printf.sprintf "line %d: index %s already used\n"
		 (Ast0.get_info e).Ast0.pos_info.Ast0.line_start
		 (String.concat " " (List.map string_of_int index)))
	  with Not_found -> Hashtbl.add table index (e1,l)) in
      if il = [] then check_index bil btl else check_index il tl);
    if il = []
    then Recursor(k, bil, btl, union_all l)
    else Recursor(k, il, tl, union_all l) in

  let compute_result builder e = function
      Bind(k,il,tl,bil,btl,l) -> mkres builder k il tl bil btl l e
    | Token(k,il,tl,l) -> mkres builder k [il] [tl] [] [] [l] e
    | Recursor(k,bil,btl,l) -> mkres builder k [] [] bil btl [l] e in

  let make_not_marked = function
      Bind(k,il,tl,bil,btl,l) -> Bind(NotAllMarked,il,tl,bil,btl,l)
    | Token(k,il,tl,l) -> Token(NotAllMarked,il,tl,l)
    | Recursor(k,bil,btl,l) -> Recursor(NotAllMarked,bil,btl,l) in

  let do_nothing builder r k e = compute_result builder e (k e) in

  let disj_cases disj starter code fn ender =
    (* neutral_mcode used so starter and ender don't have an affect on
       whether the code is considered all plus/minus, but so that they are
       consider in the index list, which is needed to make a disj with
       something in one branch and nothing in the other different from code
       that just has the something (starter/ender enough, mids not needed
       for this).  Cannot agglomerate + code over | boundaries, because two -
       cases might have different + code, and don't want to put the + code
       together into one unit. *)
    let make_not_marked =
      if is_minus
      then
	(let min = Ast0.get_line disj in
	let max = Ast0.get_line_end disj in
	let (plus_min,plus_max) = find min (min-1) (max+1) in
	if max > plus_max then make_not_marked else (function x -> x))
      else make_not_marked in
    bind (neutral_mcode starter)
      (bind (List.fold_right bind
	       (List.map make_not_marked (List.map fn code))
	       option_default)
	 (neutral_mcode ender)) in

  (* no whencode in plus tree so have to drop it *)
  (* need special cases for dots, nests, and disjs *)
  let ident r k e =
    compute_result Ast0.ident e
      (match Ast0.unwrap e with
	Ast0.DisjId(starter,id_list,_,ender)
      | Ast0.ConjId(starter,id_list,_,ender) ->
	  disj_cases e starter id_list r.VT0.combiner_rec_ident ender
      |	_ -> k e) in

  let expression r k e =
    compute_result Ast0.expr e
      (match Ast0.unwrap e with
	Ast0.NestExpr(starter,exp,ender,whencode,multi) ->
	  k (Ast0.rewrap e (Ast0.NestExpr(starter,exp,ender,None,multi)))
      | Ast0.Edots(dots,whencode) ->
	  k (Ast0.rewrap e (Ast0.Edots(dots,None)))
      | Ast0.DisjExpr(starter,expr_list,_,ender)
      | Ast0.ConjExpr(starter,expr_list,_,ender) ->
	  disj_cases e starter expr_list r.VT0.combiner_rec_expression ender
      |	_ -> k e) in


  (* not clear why we have the next cases, since DisjDecl and
  as it only comes from isos *)
  (* actually, DisjDecl now allowed in source struct decls *)
  let declaration r k e =
    compute_result Ast0.decl e
      (match Ast0.unwrap e with
	Ast0.DisjDecl(starter,decls,_,ender)
      | Ast0.ConjDecl(starter,decls,_,ender) ->
	  disj_cases e starter decls r.VT0.combiner_rec_declaration ender
	(* Need special cases for the following so that the type will be
	   considered as a unit, rather than distributed around the
	   declared variable.  This needs to be done because of the call to
	   compute_result, ie the processing of each term should make a
	   side-effect on the complete term structure as well as collecting
	   some information about it.  So we have to visit each complete
	   term structure.  In (all?) other such cases, we visit the terms
	   using rebuilder, which just visits the subterms, rather than
	   reordering their components. *)
      |	Ast0.Init(stg,ty,id,attr,eq,ini,sem) ->
	  bind (match stg with Some stg -> mcode stg | _ -> option_default)
	    (bind (r.VT0.combiner_rec_typeC ty)
	       (bind (r.VT0.combiner_rec_ident id)
                  (bind
                     (List.fold_right bind
                       (List.map r.VT0.combiner_rec_attribute attr)
			option_default)
		     (bind (mcode eq)
			(bind (r.VT0.combiner_rec_initialiser ini)
			   (mcode sem))))))
      | Ast0.UnInit(stg,ty,id,attr,sem) ->
	  bind (match stg with Some stg -> mcode stg | _ -> option_default)
	    (bind (r.VT0.combiner_rec_typeC ty)
	       (bind (r.VT0.combiner_rec_ident id)
                  (bind
                     (List.fold_right bind
                       (List.map r.VT0.combiner_rec_attribute attr)
			option_default)
		     (mcode sem))))
      |	_ -> k e) in

  let field r k e =
    compute_result Ast0.field e
      (match Ast0.unwrap e with
	Ast0.DisjField(starter,decls,_,ender)
      | Ast0.ConjField(starter,decls,_,ender) ->
	  disj_cases e starter decls r.VT0.combiner_rec_field ender
      | Ast0.Fdots(dots,whencode) ->
	  k (Ast0.rewrap e (Ast0.Fdots(dots,None)))
      | Ast0.Field(ty,id,bf,sem) ->
	  let bitfield (_c, e) = r.VT0.combiner_rec_expression e in
	  bind (r.VT0.combiner_rec_typeC ty)
	    (bind (Common.default option_default r.VT0.combiner_rec_ident id)
	       (bind (Common.default option_default bitfield bf) (mcode sem)))
      |	_ -> k e) in

  let enum_decl r k e =
    compute_result Ast0.enum_decl e
      (match Ast0.unwrap e with
	Ast0.Enum(name,Some(eq,eval)) ->
          bind (r.VT0.combiner_rec_ident name)
            (bind (mcode eq) (r.VT0.combiner_rec_expression eval))
      | Ast0.EnumDots(dots,whencode) ->
	  k (Ast0.rewrap e (Ast0.EnumDots(dots,None)))
      | _ -> k e) in

  let param r k e =
    compute_result Ast0.param e
      (match Ast0.unwrap e with
	Ast0.Param(ty,Some id,attr) ->
	  (* needed for the same reason as in the Init and UnInit cases *)
	  bind (r.VT0.combiner_rec_typeC ty)
           (bind (r.VT0.combiner_rec_ident id)
              (List.fold_right bind
                 (List.map r.VT0.combiner_rec_attribute attr)
                 option_default))
      |	_ -> k e) in

  let typeC r k e =
    compute_result Ast0.typeC e
      (match Ast0.unwrap e with
	Ast0.DisjType(starter,types,_,ender)
      | Ast0.ConjType(starter,types,_,ender) ->
	  disj_cases e starter types r.VT0.combiner_rec_typeC ender
      |	_ -> k e) in

  let initialiser r k i =
    compute_result Ast0.ini i
      (match Ast0.unwrap i with
	Ast0.Idots(dots,whencode) ->
	  k (Ast0.rewrap i (Ast0.Idots(dots,None)))
      |	_ -> k i) in

  let case_line r k e =
    compute_result Ast0.case_line e
      (match Ast0.unwrap e with
	Ast0.DisjCase(starter,case_list,_,ender) ->
	  disj_cases e starter case_list r.VT0.combiner_rec_case_line ender
      |	_ -> k e) in

  let statement r k s =
    compute_result Ast0.stmt s
      (match Ast0.unwrap s with
	Ast0.Nest(started,stm_dots,ender,whencode,multi) ->
	  k (Ast0.rewrap s (Ast0.Nest(started,stm_dots,ender,[],multi)))
      | Ast0.Dots(dots,whencode) ->
	  k (Ast0.rewrap s (Ast0.Dots(dots,[])))
      | Ast0.Disj(starter,statement_dots_list,_,ender)
      | Ast0.Conj(starter,statement_dots_list,_,ender) ->
	  disj_cases s starter statement_dots_list
	    r.VT0.combiner_rec_statement_dots
	    ender
	(* cases for everything with extra mcode *)
      | Ast0.Decl((info,bef),_) ->
	  bind (nc_mcode ((),(),info,bef,(),-1)) (k s)
      | Ast0.FunDecl((info,bef),_,_,_,_,_,_,_,_,_,(ainfo,aft)) ->
	  (* not sure that the use of start is relevant here *)
	  let a1 = nc_mcode ((),(),info,bef,(),-1) in
	  let a2 = nc_mcode ((),(),ainfo,aft,(),-1) in
	  let b = k s in
	  bind a1 (bind b a2)
      (* For these, the info of the aft mcode is derived from the else
	 branch.  These might not correspond for a context if, eg if
	 only the else branch is replaced.  Thus we take instead the
	 info of the starting keyword.  In a context case, these will be
	 the same on the - and + sides.  All that is used as an offset,
	 and it is only used as a key, so this is safe to do. For an
         iterator, we take the left parenthesis, which should have the
	 same property. *)
      | Ast0.IfThen(start,_,_,_,_,(info,aft,adj))
      | Ast0.IfThenElse(start,_,_,_,_,_,_,(info,aft,adj))
      | Ast0.Iterator(_,start,_,_,_,(info,aft,adj))
      | Ast0.While(start,_,_,_,_,(info,aft,adj))
      | Ast0.For(start,_,_,_,_,_,_,_,(info,aft,adj)) ->
	  let mcode_info (_,_,info,_,_,_) = info in
	  bind (k s) (nc_mcode ((),(),mcode_info start,aft,(),adj))
      |	_ -> k s) in

  let string_fragment r k s =
    compute_result Ast0.string_fragment s (k s) in

  let do_top builder r k e = compute_result builder e (k e) in

  let combiner =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      (do_nothing Ast0.dotsExpr) (do_nothing Ast0.dotsInit)
      (do_nothing Ast0.dotsParam) (do_nothing Ast0.dotsStmt)
      (do_nothing Ast0.dotsDecl) (do_nothing Ast0.dotsField)
      (do_nothing Ast0.dotsEnumDecl) (do_nothing Ast0.dotsCase)
      (do_nothing Ast0.dotsDefParam)
      ident expression (do_nothing Ast0.assignOp) (do_nothing Ast0.binaryOp)
      typeC initialiser param declaration field enum_decl
      statement (do_nothing Ast0.forinfo) case_line string_fragment
      (do_nothing Ast0.attr) (do_nothing Ast0.attr_arg) (do_top Ast0.top) in
  combiner.VT0.combiner_rec_top_level code

(* --------------------------------------------------------------------- *)
(* Traverse the hash tables and find corresponding context nodes that have
the same context children *)

(* this is just a sanity check - really only need to look at the top-level
   structure *)
let equal_mcode (_,_,info1,_,_,_) (_,_,info2,_,_,_) =
  info1.Ast0.pos_info.Ast0.offset = info2.Ast0.pos_info.Ast0.offset

let assignOp_equal_mcode op1 op2 =
  match (Ast0.unwrap op1, Ast0.unwrap op2) with
    Ast0.SimpleAssign op1', Ast0.SimpleAssign op2' -> equal_mcode op1' op2'
  | Ast0.OpAssign op1', Ast0.OpAssign op2' -> equal_mcode op1' op2'
  | Ast0.MetaAssign(mv1,_,_), Ast0.MetaAssign(mv2,_,_) -> equal_mcode mv1 mv2
  | _ -> false

let binaryOp_equal_mcode op1 op2 =
  match (Ast0.unwrap op1, Ast0.unwrap op2) with
    Ast0.Arith op1', Ast0.Arith op2' -> equal_mcode op1' op2'
  | Ast0.Logical op1', Ast0.Logical op2' -> equal_mcode op1' op2'
  | Ast0.MetaBinary(mv1,_,_), Ast0.MetaBinary(mv2,_,_) -> equal_mcode mv1 mv2
  | _ -> false

let equal_option e1 e2 =
  match (e1,e2) with
    (Some x, Some y) -> equal_mcode x y
  | (None, None) -> true
  | _ -> false

let dots fn d1 d2 =
  List.length (Ast0.unwrap d1) = List.length (Ast0.unwrap d2)

let equal_attr_arg a1 a2 =
  match (Ast0.unwrap a1, Ast0.unwrap a2) with
    (Ast0.AttrName(name1),Ast0.AttrName(name2)) ->
      equal_mcode name1 name2
  | (Ast0.MetaAttr(name1,_,_),Ast0.MetaAttr(name2,_,_)) ->
      equal_mcode name1 name2
  | _ -> false

let equal_attribute a1 a2 =
  match (Ast0.unwrap a1, Ast0.unwrap a2) with
    (Ast0.Attribute(attr1),Ast0.Attribute(attr2)) ->
      equal_attr_arg attr1 attr2

let equal_ident i1 i2 =
  match (Ast0.unwrap i1,Ast0.unwrap i2) with
    (Ast0.Id(name1),Ast0.Id(name2)) -> equal_mcode name1 name2
  | (Ast0.MetaId(name1,_,_,_),Ast0.MetaId(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.MetaFunc(name1,_,_),Ast0.MetaFunc(name2,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.MetaLocalFunc(name1,_,_),Ast0.MetaLocalFunc(name2,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.DisjId(starter1,_,mids1,ender1),
     Ast0.DisjId(starter2,_,mids2,ender2))
  | (Ast0.ConjId(starter1,_,mids1,ender1),
     Ast0.ConjId(starter2,_,mids2,ender2)) ->
      equal_mcode starter1 starter2 &&
      List.for_all2 equal_mcode mids1 mids2 &&
      equal_mcode ender1 ender2
  | (Ast0.OptIdent(_),Ast0.OptIdent(_)) -> true
  | _ -> false

let rec equal_expression e1 e2 =
  match (Ast0.unwrap e1,Ast0.unwrap e2) with
    (Ast0.Ident(_),Ast0.Ident(_)) -> true
  | (Ast0.Constant(const1),Ast0.Constant(const2)) -> equal_mcode const1 const2
  | (Ast0.StringConstant(lq1,const1,rq1,isWchar1),Ast0.StringConstant(lq2,const2,rq2,isWchar2))->
      equal_mcode lq1 lq2 && equal_mcode rq1 rq2 && isWchar1 = isWchar2
  | (Ast0.FunCall(_,lp1,_,rp1),Ast0.FunCall(_,lp2,_,rp2)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.Assignment(_,op1,_,_),Ast0.Assignment(_,op2,_,_)) ->
      assignOp_equal op1 op2
  | (Ast0.Sequence(_,op1,_),Ast0.Sequence(_,op2,_)) ->
      equal_mcode op1 op2
  | (Ast0.CondExpr(_,why1,_,colon1,_),Ast0.CondExpr(_,why2,_,colon2,_)) ->
      equal_mcode why1 why2 && equal_mcode colon1 colon2
  | (Ast0.Postfix(_,op1),Ast0.Postfix(_,op2)) -> equal_mcode op1 op2
  | (Ast0.Infix(_,op1),Ast0.Infix(_,op2)) -> equal_mcode op1 op2
  | (Ast0.Unary(_,op1),Ast0.Unary(_,op2)) -> equal_mcode op1 op2
  | (Ast0.Binary(_,op1,_),Ast0.Binary(_,op2,_)) -> binaryOp_equal op1 op2
  | (Ast0.Paren(lp1,_,rp1),Ast0.Paren(lp2,_,rp2)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.ArrayAccess(_,lb1,_,rb1),Ast0.ArrayAccess(_,lb2,_,rb2)) ->
      equal_mcode lb1 lb2 && equal_mcode rb1 rb2
  | (Ast0.RecordAccess(_,pt1,_),Ast0.RecordAccess(_,pt2,_)) ->
      equal_mcode pt1 pt2
  | (Ast0.RecordPtAccess(_,ar1,_),Ast0.RecordPtAccess(_,ar2,_)) ->
      equal_mcode ar1 ar2
  | (Ast0.Cast(lp1,_,ar1,rp1,_),Ast0.Cast(lp2,_,ar2,rp2,_)) ->
      equal_mcode lp1 lp2 &&
      (List.length ar1) = (List.length ar2) &&
      List.for_all2 equal_attribute ar1 ar2 &&
      equal_mcode rp1 rp2
  | (Ast0.SizeOfExpr(szf1,_),Ast0.SizeOfExpr(szf2,_)) ->
      equal_mcode szf1 szf2
  | (Ast0.SizeOfType(szf1,lp1,_,rp1),Ast0.SizeOfType(szf2,lp2,_,rp2)) ->
      equal_mcode szf1 szf2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.TypeExp(_),Ast0.TypeExp(_)) -> true
  | (Ast0.Constructor(lp1,_,rp1,_),Ast0.Constructor(lp2,_,rp2,_)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.MetaErr(name1,_,_),Ast0.MetaErr(name2,_,_))
  | (Ast0.MetaExpr(name1,_,_,_,_,_),Ast0.MetaExpr(name2,_,_,_,_,_))
  | (Ast0.MetaExprList(name1,_,_,_),Ast0.MetaExprList(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.EComma(cm1),Ast0.EComma(cm2)) -> equal_mcode cm1 cm2
  | (Ast0.DisjExpr(starter1,_,mids1,ender1),
     Ast0.DisjExpr(starter2,_,mids2,ender2))
  | (Ast0.ConjExpr(starter1,_,mids1,ender1),
     Ast0.ConjExpr(starter2,_,mids2,ender2)) ->
       equal_mcode starter1 starter2 &&
       List.for_all2 equal_mcode mids1 mids2 &&
       equal_mcode ender1 ender2
  | (Ast0.NestExpr(starter1,_,ender1,_,m1),
     Ast0.NestExpr(starter2,_,ender2,_,m2)) ->
      equal_mcode starter1 starter2 && equal_mcode ender1 ender2 && m1 = m2
  | (Ast0.Edots(dots1,_),Ast0.Edots(dots2,_)) -> equal_mcode dots1 dots2
  | (Ast0.OptExp(_),Ast0.OptExp(_)) -> true
  | _ -> false

and assignOp_equal op1 op2 =
  match (Ast0.unwrap op1, Ast0.unwrap op2) with
    | Ast0.SimpleAssign o1, Ast0.SimpleAssign o2 -> equal_mcode o1 o2
    | Ast0.OpAssign o1, Ast0.OpAssign o2 -> equal_mcode o1 o2
    | Ast0.MetaAssign(mv1,_,_), Ast0.MetaAssign(mv2,_,_) ->
      equal_mcode mv1 mv2
    | _ -> false
and binaryOp_equal op1 op2 =
  match (Ast0.unwrap op1, Ast0.unwrap op2) with
    | Ast0.Arith o1, Ast0.Arith o2 -> equal_mcode o1 o2
    | Ast0.Logical o1, Ast0.Logical o2 -> equal_mcode o1 o2
    | Ast0.MetaBinary(mv1,_,_), Ast0.MetaBinary(mv2,_,_) ->
      equal_mcode mv1 mv2
    | _ -> false

let equal_typeC t1 t2 =
  match (Ast0.unwrap t1,Ast0.unwrap t2) with
    (Ast0.ConstVol(cv1,_),Ast0.ConstVol(cv2,_)) -> equal_mcode cv1 cv2
  | (Ast0.BaseType(ty1,stringsa),Ast0.BaseType(ty2,stringsb)) ->
      List.for_all2 equal_mcode stringsa stringsb
  | (Ast0.Signed(sign1,_),Ast0.Signed(sign2,_)) ->
      equal_mcode sign1 sign2
  | (Ast0.Pointer(_,star1),Ast0.Pointer(_,star2)) ->
      equal_mcode star1 star2
  | (Ast0.ParenType(lp1,_,rp1),Ast0.ParenType(lp2,_,rp2)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.FunctionType(_,lp1,_,rp1),Ast0.FunctionType(_,lp2,_,rp2)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.Array(_,lb1,_,rb1),Ast0.Array(_,lb2,_,rb2)) ->
      equal_mcode lb1 lb2 && equal_mcode rb1 rb2
  | (Ast0.Decimal(dec1,lp1,_,comma1,_,rp1),
     Ast0.Decimal(dec2,lp2,_,comma2,_,rp2)) ->
       equal_mcode dec1 dec2 && equal_mcode lp1 lp2 &&
       equal_option comma1 comma2 && equal_mcode rp1 rp2
  | (Ast0.EnumName(kind1,_),Ast0.EnumName(kind2,_)) ->
      equal_mcode kind1 kind2
  | (Ast0.EnumDef(_,lb1,_,rb1),Ast0.EnumDef(_,lb2,_,rb2)) ->
       let tru1 = equal_mcode lb1 lb2 in
       let tru2 = equal_mcode rb1 rb2 in
       tru1 && tru2
  | (Ast0.StructUnionName(kind1,_),Ast0.StructUnionName(kind2,_)) ->
      equal_mcode kind1 kind2
  | (Ast0.StructUnionDef(_,lb1,_,rb1),
     Ast0.StructUnionDef(_,lb2,_,rb2)) ->
       equal_mcode lb1 lb2 && equal_mcode rb1 rb2
  | (Ast0.TypeOfExpr(tf1,lp1,_,rp1),Ast0.TypeOfExpr(tf2,lp2,_,rp2)) ->
      equal_mcode tf1 tf2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.TypeOfType(tf1,lp1,_,rp1),Ast0.TypeOfType(tf2,lp2,_,rp2)) ->
      equal_mcode tf1 tf2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.TypeName(name1),Ast0.TypeName(name2)) -> equal_mcode name1 name2
  | (Ast0.AutoType(auto1),Ast0.AutoType(auto2)) -> equal_mcode auto1 auto2
  | (Ast0.MetaType(name1,_,_),Ast0.MetaType(name2,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.DisjType(starter1,_,mids1,ender1),
     Ast0.DisjType(starter2,_,mids2,ender2)) ->
       equal_mcode starter1 starter2 &&
       List.for_all2 equal_mcode mids1 mids2 &&
       equal_mcode ender1 ender2
  | (Ast0.ConjType(starter1,_,mids1,ender1),
     Ast0.ConjType(starter2,_,mids2,ender2)) ->
       equal_mcode starter1 starter2 &&
       List.for_all2 equal_mcode mids1 mids2 &&
       equal_mcode ender1 ender2
  | (Ast0.OptType(_),Ast0.OptType(_)) -> true
  | _ -> false

let equal_fninfo x y =
  match (x,y) with
    (Ast0.FStorage(s1),Ast0.FStorage(s2)) -> equal_mcode s1 s2
  | (Ast0.FType(_),Ast0.FType(_)) -> true
  | (Ast0.FInline(i1),Ast0.FInline(i2)) -> equal_mcode i1 i2
  | (Ast0.FAttr(i1),Ast0.FAttr(i2)) -> equal_attribute i1 i2
  | _ -> false

let equal_declaration d1 d2 =
  match (Ast0.unwrap d1,Ast0.unwrap d2) with
    (Ast0.MetaDecl(name1,_,_),Ast0.MetaDecl(name2,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.Init(stg1,_,_,attr1,eq1,_,sem1),
     Ast0.Init(stg2,_,_,attr2,eq2,_,sem2)) ->
      equal_option stg1 stg2 &&
      (List.length attr1) = (List.length attr2) &&
      List.for_all2 equal_attribute attr1 attr2 &&
      equal_mcode eq1 eq2 && equal_mcode sem1 sem2
  | (Ast0.UnInit(stg1,_,_,attr1,sem1),Ast0.UnInit(stg2,_,_,attr2,sem2)) ->
      equal_option stg1 stg2 &&
      (List.length attr1) = (List.length attr2) &&
      List.for_all2 equal_attribute attr1 attr2 &&
      equal_mcode sem1 sem2
  | (Ast0.FunProto(fninfo1,name1,lp1,p1,va1,rp1,sem1),
     Ast0.FunProto(fninfo2,name2,lp2,p2,va2,rp2,sem2)) ->
       let equal_varargs va1 va2 = match (va1,va2) with
         | None, None -> true
         | Some (c1, e1), Some (c2, e2) ->
           equal_mcode c1 c2 && equal_mcode e1 e2
         | _ -> false in
       (List.length fninfo1) = (List.length fninfo2) &&
       List.for_all2 equal_fninfo fninfo1 fninfo2 &&
       equal_mcode lp1 lp2 && equal_varargs va1 va2 &&
       equal_mcode rp1 rp2 && equal_mcode sem1 sem2
  | (Ast0.MacroDecl(stg1,nm1,lp1,_,rp1,attr1,sem1),
     Ast0.MacroDecl(stg2,nm2,lp2,_,rp2,attr2,sem2)) ->
      equal_option stg1 stg2 &&
      (List.length attr1) = (List.length attr2) &&
      List.for_all2 equal_attribute attr1 attr2 &&
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2 && equal_mcode sem1 sem2
  | (Ast0.MacroDeclInit(stg1,nm1,lp1,_,rp1,eq1,_,sem1),
     Ast0.MacroDeclInit(stg2,nm2,lp2,_,rp2,eq2,_,sem2)) ->
       equal_option stg1 stg2 &&
       equal_mcode lp1 lp2 && equal_mcode rp1 rp2 && equal_mcode eq1 eq2
	 && equal_mcode sem1 sem2
  | (Ast0.TyDecl(_,attr1,sem1),Ast0.TyDecl(_,attr2,sem2)) ->
       (List.length attr1) = (List.length attr2) &&
       List.for_all2 equal_attribute attr1 attr2 && equal_mcode sem1 sem2
  | (Ast0.OptDecl(_),Ast0.OptDecl(_)) -> true
  | (Ast0.DisjDecl(starter1,_,mids1,ender1),
     Ast0.DisjDecl(starter2,_,mids2,ender2))
  | (Ast0.ConjDecl(starter1,_,mids1,ender1),
     Ast0.ConjDecl(starter2,_,mids2,ender2)) ->
       equal_mcode starter1 starter2 &&
       List.for_all2 equal_mcode mids1 mids2 &&
       equal_mcode ender1 ender2
  | _ -> false

let equal_field d1 d2 =
  match (Ast0.unwrap d1,Ast0.unwrap d2) with
     (Ast0.MetaField(name1,_,_),Ast0.MetaField(name2,_,_))
  | (Ast0.MetaFieldList(name1,_,_,_),Ast0.MetaFieldList(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.Field(_,_,_bf1,sem1),Ast0.Field(_,_,_bf2,sem2)) ->
      equal_mcode sem1 sem2
  | (Ast0.Fdots(dots1,_),Ast0.Fdots(dots2,_)) -> equal_mcode dots1 dots2
  | (Ast0.OptField(_),Ast0.OptField(_)) -> true
  | (Ast0.DisjField(starter1,_,mids1,ender1),
     Ast0.DisjField(starter2,_,mids2,ender2))
  | (Ast0.ConjField(starter1,_,mids1,ender1),
     Ast0.ConjField(starter2,_,mids2,ender2)) ->
       equal_mcode starter1 starter2 &&
       List.for_all2 equal_mcode mids1 mids2 &&
       equal_mcode ender1 ender2
  | _ -> false

let equal_enum_decl d1 d2 =
  match (Ast0.unwrap d1,Ast0.unwrap d2) with
    (Ast0.Enum(name1,enum_val1),Ast0.Enum(name2,enum_val2)) ->
      equal_ident name1 name2 &&
      (match enum_val1,enum_val2 with
        None,None -> true
      | Some (eq1,val1),Some(eq2,val2) ->
         equal_mcode eq1 eq2 && equal_expression val1 val2
      | _ -> false)
  | (Ast0.EnumComma(cm1),Ast0.EnumComma(cm2)) -> equal_mcode cm1 cm2
  | (Ast0.EnumDots(dots1,_),Ast0.EnumDots(dots2,_)) -> equal_mcode dots1 dots2
  | _ -> false

let equal_designator d1 d2 =
  match (d1,d2) with
    (Ast0.DesignatorField(dot1,_),Ast0.DesignatorField(dot2,_)) ->
      equal_mcode dot1 dot2
  | (Ast0.DesignatorIndex(lb1,_,rb1),Ast0.DesignatorIndex(lb2,_,rb2)) ->
      (equal_mcode lb1 lb2) && (equal_mcode rb1 rb2)
  | (Ast0.DesignatorRange(lb1,_,dots1,_,rb1),
     Ast0.DesignatorRange(lb2,_,dots2,_,rb2)) ->
       (equal_mcode lb1 lb2) && (equal_mcode dots1 dots2) &&
       (equal_mcode rb1 rb2)
  | _ -> false

let equal_initialiser i1 i2 =
  match (Ast0.unwrap i1,Ast0.unwrap i2) with
    (Ast0.MetaInit(name1,_,_),Ast0.MetaInit(name2,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.MetaInitList(name1,_,_,_),Ast0.MetaInitList(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.InitExpr(_),Ast0.InitExpr(_)) -> true
  | (Ast0.InitList(lb1,_,rb1,o1),Ast0.InitList(lb2,_,rb2,o2)) ->
      (* can't compare orderedness, because this can differ between -
	 and + code *)
      (equal_mcode lb1 lb2) && (equal_mcode rb1 rb2)
  | (Ast0.InitGccExt(designators1,eq1,_),
     Ast0.InitGccExt(designators2,eq2,_)) ->
       (List.for_all2 equal_designator designators1 designators2) &&
       (equal_mcode eq1 eq2)
  | (Ast0.InitGccName(_,eq1,_),Ast0.InitGccName(_,eq2,_)) ->
      equal_mcode eq1 eq2
  | (Ast0.IComma(cm1),Ast0.IComma(cm2)) -> equal_mcode cm1 cm2
  | (Ast0.Idots(d1,_),Ast0.Idots(d2,_)) -> equal_mcode d1 d2
  | (Ast0.OptIni(_),Ast0.OptIni(_)) -> true
  | _ -> false

let equal_parameterTypeDef p1 p2 =
  match (Ast0.unwrap p1,Ast0.unwrap p2) with
    (Ast0.VoidParam(_,ar1),Ast0.VoidParam(_,ar2)) ->
      (List.length ar1) = (List.length ar2) &&
      List.for_all2 equal_attribute ar1 ar2
  | (Ast0.Param(_,_,ar1),Ast0.Param(_,_,ar2)) ->
      (List.length ar1) = (List.length ar2) &&
      List.for_all2 equal_attribute ar1 ar2
  | (Ast0.MetaParam(name1,_,_),Ast0.MetaParam(name2,_,_))
  | (Ast0.MetaParamList(name1,_,_,_),Ast0.MetaParamList(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.PComma(cm1),Ast0.PComma(cm2)) -> equal_mcode cm1 cm2
  | (Ast0.Pdots(dots1),Ast0.Pdots(dots2)) -> equal_mcode dots1 dots2
  | (Ast0.OptParam(_),Ast0.OptParam(_)) -> true
  | _ -> false

let equal_statement s1 s2 =
  match (Ast0.unwrap s1,Ast0.unwrap s2) with
    (Ast0.FunDecl(_,fninfo1,_,lp1,_,_,rp1,lbrace1,_,rbrace1,_),
     Ast0.FunDecl(_,fninfo2,_,lp2,_,_,rp2,lbrace2,_,rbrace2,_)) ->
       (List.length fninfo1) = (List.length fninfo2) &&
       List.for_all2 equal_fninfo fninfo1 fninfo2 &&
       equal_mcode lp1 lp2 && equal_mcode rp1 rp2 &&
       equal_mcode lbrace1 lbrace2 && equal_mcode rbrace1 rbrace2
  | (Ast0.Decl(_,_),Ast0.Decl(_,_)) -> true
  | (Ast0.Seq(lbrace1,_,rbrace1),Ast0.Seq(lbrace2,_,rbrace2)) ->
      equal_mcode lbrace1 lbrace2 && equal_mcode rbrace1 rbrace2
  | (Ast0.ExprStatement(_,sem1),Ast0.ExprStatement(_,sem2)) ->
      equal_mcode sem1 sem2
  | (Ast0.IfThen(iff1,lp1,_,rp1,_,_),Ast0.IfThen(iff2,lp2,_,rp2,_,_)) ->
      equal_mcode iff1 iff2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.IfThenElse(iff1,lp1,_,rp1,_,els1,_,_),
     Ast0.IfThenElse(iff2,lp2,_,rp2,_,els2,_,_)) ->
       equal_mcode iff1 iff2 &&
	 equal_mcode lp1 lp2 && equal_mcode rp1 rp2 && equal_mcode els1 els2
  | (Ast0.While(whl1,lp1,_,rp1,_,_),Ast0.While(whl2,lp2,_,rp2,_,_)) ->
      equal_mcode whl1 whl2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.Do(d1,_,whl1,lp1,_,rp1,sem1),Ast0.Do(d2,_,whl2,lp2,_,rp2,sem2)) ->
      equal_mcode whl1 whl2 && equal_mcode d1 d2 &&
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2 && equal_mcode sem1 sem2
  | (Ast0.For(fr1,lp1,first1,_,sem21,_,rp1,_,_),
     Ast0.For(fr2,lp2,first2,_,sem22,_,rp2,_,_)) ->
       let first =
	 match (Ast0.unwrap first1,Ast0.unwrap first2) with
	   (Ast0.ForExp(_,sem1),Ast0.ForExp(_,sem2)) ->
	     equal_mcode sem1 sem2
	 | (Ast0.ForDecl _,Ast0.ForDecl _) -> true
	 | _ -> false in
       equal_mcode fr1 fr2 && equal_mcode lp1 lp2 &&
       first && equal_mcode sem21 sem22 &&
       equal_mcode rp1 rp2
  | (Ast0.Iterator(nm1,lp1,_,rp1,_,_),Ast0.Iterator(nm2,lp2,_,rp2,_,_)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.Switch(switch1,lp1,_,rp1,lb1,_,_,rb1),
     Ast0.Switch(switch2,lp2,_,rp2,lb2,_,_,rb2)) ->
       equal_mcode switch1 switch2 && equal_mcode lp1 lp2 &&
       equal_mcode rp1 rp2 && equal_mcode lb1 lb2 &&
       equal_mcode rb1 rb2
  | (Ast0.Break(br1,sem1),Ast0.Break(br2,sem2)) ->
      equal_mcode br1 br2 && equal_mcode sem1 sem2
  | (Ast0.Continue(cont1,sem1),Ast0.Continue(cont2,sem2)) ->
      equal_mcode cont1 cont2 && equal_mcode sem1 sem2
  | (Ast0.Label(_,dd1),Ast0.Label(_,dd2)) ->
      equal_mcode dd1 dd2
  | (Ast0.Goto(g1,_,sem1),Ast0.Goto(g2,_,sem2)) ->
      equal_mcode g1 g2 && equal_mcode sem1 sem2
  | (Ast0.Return(ret1,sem1),Ast0.Return(ret2,sem2)) ->
      equal_mcode ret1 ret2 && equal_mcode sem1 sem2
  | (Ast0.ReturnExpr(ret1,_,sem1),Ast0.ReturnExpr(ret2,_,sem2)) ->
      equal_mcode ret1 ret2 && equal_mcode sem1 sem2
  | (Ast0.Exec(exec1,lang1,_,sem1),Ast0.Exec(exec2,lang2,_,sem2)) ->
      equal_mcode exec1 exec2 && equal_mcode lang1 lang2 &&
      equal_mcode sem1 sem2
  | (Ast0.MetaStmt(name1,_,_),Ast0.MetaStmt(name2,_,_))
  | (Ast0.MetaStmtList(name1,_,_,_),Ast0.MetaStmtList(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.Disj(starter1,_,mids1,ender1),Ast0.Disj(starter2,_,mids2,ender2))
  | (Ast0.Conj(starter1,_,mids1,ender1),Ast0.Conj(starter2,_,mids2,ender2)) ->
      equal_mcode starter1 starter2 &&
      List.for_all2 equal_mcode mids1 mids2 &&
      equal_mcode ender1 ender2
  | (Ast0.Nest(starter1,_,ender1,_,m1),Ast0.Nest(starter2,_,ender2,_,m2)) ->
      equal_mcode starter1 starter2 && equal_mcode ender1 ender2 && m1 = m2
  | (Ast0.Exp(_),Ast0.Exp(_)) -> true
  | (Ast0.TopExp(_),Ast0.TopExp(_)) -> true
  | (Ast0.Ty(_),Ast0.Ty(_)) -> true
  | (Ast0.TopId(_),Ast0.TopId(_)) -> true
  | (Ast0.TopInit(_),Ast0.TopInit(_)) -> true
  | (Ast0.Dots(d1,_),Ast0.Dots(d2,_)) -> equal_mcode d1 d2
  | (Ast0.Include(inc1,name1),Ast0.Include(inc2,name2)) ->
      equal_mcode inc1 inc2 && equal_mcode name1 name2
  | (Ast0.MetaInclude(inc1,name1),Ast0.MetaInclude(inc2,name2)) ->
      equal_mcode inc1 inc2
  | (Ast0.Undef(def1,_),Ast0.Undef(def2,_)) ->
      equal_mcode def1 def2
  | (Ast0.Define(def1,_,_,_),Ast0.Define(def2,_,_,_)) ->
      equal_mcode def1 def2
  | (Ast0.Pragma(prg1,_,_),Ast0.Pragma(prg2,_,_)) ->
      equal_mcode prg1 prg2
  | (Ast0.OptStm(_),Ast0.OptStm(_)) -> true
  | _ -> false

let equal_case_line c1 c2 =
  match (Ast0.unwrap c1,Ast0.unwrap c2) with
    (Ast0.Default(def1,colon1,_),Ast0.Default(def2,colon2,_)) ->
      equal_mcode def1 def2 && equal_mcode colon1 colon2
  | (Ast0.Case(case1,_,colon1,_),Ast0.Case(case2,_,colon2,_)) ->
      equal_mcode case1 case2 && equal_mcode colon1 colon2
  | (Ast0.DisjCase(starter1,_,mids1,ender1),
     Ast0.DisjCase(starter2,_,mids2,ender2)) ->
       equal_mcode starter1 starter2 &&
       List.for_all2 equal_mcode mids1 mids2 &&
       equal_mcode ender1 ender2
  | (Ast0.OptCase(_),Ast0.OptCase(_)) -> true
  | _ -> false

let equal_define_param d1 d2 =
  match (Ast0.unwrap d1,Ast0.unwrap d2) with
    (Ast0.DParam _,Ast0.DParam _) -> true
  | (Ast0.MetaDParamList(name1,_,_,_),Ast0.MetaDParamList(name2,_,_,_)) ->
      equal_mcode name1 name2
  | (Ast0.DPComma cm1,Ast0.DPComma cm2) -> equal_mcode cm1 cm2
  | (Ast0.DPdots d1,Ast0.DPdots d2) -> equal_mcode d1 d2
  | (Ast0.OptDParam(_),Ast0.OptDParam(_)) -> true
  | _ -> false

let equal_top_level t1 t2 =
  match (Ast0.unwrap t1,Ast0.unwrap t2) with
    (Ast0.NONDECL(_),Ast0.NONDECL(_)) -> true
  | (Ast0.FILEINFO(old_file1,new_file1),Ast0.FILEINFO(old_file2,new_file2)) ->
      equal_mcode old_file1 old_file2 && equal_mcode new_file1 new_file2
  | (Ast0.CODE(_),Ast0.CODE(_)) -> true
  | (Ast0.ERRORWORDS(_),Ast0.ERRORWORDS(_)) -> true
  | _ -> false

let root_equal e1 e2 =
  match (e1,e2) with
    (Ast0.DotsExprTag(d1),Ast0.DotsExprTag(d2)) -> dots equal_expression d1 d2
  | (Ast0.DotsParamTag(d1),Ast0.DotsParamTag(d2)) ->
      dots equal_parameterTypeDef d1 d2
  | (Ast0.DotsStmtTag(d1),Ast0.DotsStmtTag(d2)) -> dots equal_statement d1 d2
  | (Ast0.DotsDeclTag(d1),Ast0.DotsDeclTag(d2)) -> dots equal_declaration d1 d2
  | (Ast0.DotsFieldTag(d1),Ast0.DotsFieldTag(d2)) -> dots equal_field d1 d2
  | (Ast0.DotsEnumDeclTag(d1),Ast0.DotsEnumDeclTag(d2)) ->
      dots equal_field d1 d2
  | (Ast0.DotsCaseTag(d1),Ast0.DotsCaseTag(d2)) -> dots equal_case_line d1 d2
  | (Ast0.DotsDefParamTag(d1),Ast0.DotsDefParamTag(d2)) ->
      dots equal_define_param d1 d2
  | (Ast0.IdentTag(i1),Ast0.IdentTag(i2)) -> equal_ident i1 i2
  | (Ast0.ExprTag(e1),Ast0.ExprTag(e2)) -> equal_expression e1 e2
  | (Ast0.AssignOpTag(e1),Ast0.AssignOpTag(e2)) -> assignOp_equal e1 e2
  | (Ast0.BinaryOpTag(e1),Ast0.BinaryOpTag(e2)) -> binaryOp_equal e1 e2
  | (Ast0.ArgExprTag(d),_) -> failwith "not possible - iso only"
  | (Ast0.TypeCTag(t1),Ast0.TypeCTag(t2)) -> equal_typeC t1 t2
  | (Ast0.ParamTag(p1),Ast0.ParamTag(p2)) -> equal_parameterTypeDef p1 p2
  | (Ast0.InitTag(d1),Ast0.InitTag(d2)) -> equal_initialiser d1 d2
  | (Ast0.DeclTag(d1),Ast0.DeclTag(d2)) -> equal_declaration d1 d2
  | (Ast0.FieldTag(d1),Ast0.FieldTag(d2)) -> equal_field d1 d2
  | (Ast0.EnumDeclTag(d1),Ast0.EnumDeclTag(d2)) -> equal_enum_decl d1 d2
  | (Ast0.StmtTag(s1),Ast0.StmtTag(s2)) -> equal_statement s1 s2
  | (Ast0.TopTag(t1),Ast0.TopTag(t2)) -> equal_top_level t1 t2
  | (Ast0.IsoWhenTag(_),_) | (_,Ast0.IsoWhenTag(_))
  | (Ast0.IsoWhenTTag(_),_) | (_,Ast0.IsoWhenTTag(_))
  | (Ast0.IsoWhenFTag(_),_) | (_,Ast0.IsoWhenFTag(_)) ->
      failwith "only within iso phase"
  | _ -> false

let default_context _ =
  Ast0.CONTEXT(ref(Ast.NOTHING,
		   Ast0.default_token_info,Ast0.default_token_info))

let traverse minus_table plus_table =
  Hashtbl.iter
    (function key ->
      function (e,l) ->
	try
	  let (plus_e,plus_l) = Hashtbl.find plus_table key in
	  if root_equal e plus_e &&
	    List.for_all (function x -> x)
	      (List.map2 Common.equal_set l plus_l)
	  then
	    let i = Ast0.fresh_index() in
	    (set_index e i; set_index plus_e i;
	     set_mcodekind e (default_context());
	     set_mcodekind plus_e (default_context()))
	with Not_found -> ())
    minus_table

(* --------------------------------------------------------------------- *)
(* contextify the whencode *)

let contextify_all =
  let bind x y = () in
  let option_default = () in
  let mcode x = () in
  let donothing r k e = Ast0.set_mcodekind e (default_context()); k e in

  V0.flat_combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

let contextify_whencode =
  let bind x y = () in
  let option_default = () in

  let expression r k e =
    k e;
    match Ast0.unwrap e with
      Ast0.NestExpr(_,_,_,Some (_,_,whencode),_)
    | Ast0.Edots(_,Some (_,_,whencode)) ->
	contextify_all.VT0.combiner_rec_expression whencode
    | _ -> () in

  let initialiser r k i =
    match Ast0.unwrap i with
      Ast0.Idots(dots,Some (_,_,whencode)) ->
	contextify_all.VT0.combiner_rec_initialiser whencode
    | _ -> k i in

  let whencode = function
      Ast0.WhenNot (_,_,sd) ->
	contextify_all.VT0.combiner_rec_statement_dots sd
    | Ast0.WhenAlways (_,_,s) -> contextify_all.VT0.combiner_rec_statement s
    | Ast0.WhenModifier _ -> ()
    | Ast0.WhenNotTrue(_,_,e) -> contextify_all.VT0.combiner_rec_expression e
    | Ast0.WhenNotFalse(_,_,e) -> contextify_all.VT0.combiner_rec_expression e
  in

  let statement r k (s : Ast0.statement) =
    k s;
    match Ast0.unwrap s with
      Ast0.Nest(_,_,_,whn,_)
    | Ast0.Dots(_,whn) -> List.iter whencode whn
    | _ -> () in

  let combiner =
    V0.combiner bind option_default
      {V0.combiner_functions with
	VT0.combiner_exprfn = expression;
	VT0.combiner_initfn = initialiser;
	VT0.combiner_stmtfn = statement} in
  combiner.VT0.combiner_rec_top_level

(* --------------------------------------------------------------------- *)

(* the first int list is the tokens in the node, the second is the tokens
in the descendants *)
let minus_table =
  (Hashtbl.create(50) : (int list, Ast0.anything * int list list) Hashtbl.t)
let plus_table =
  (Hashtbl.create(50) : (int list, Ast0.anything * int list list) Hashtbl.t)

let iscode t =
  match Ast0.unwrap t with
    Ast0.NONDECL(_) -> true
  | Ast0.FILEINFO(_) -> true
  | Ast0.ERRORWORDS(_) -> false
  | Ast0.CODE(_) -> true
  | Ast0.TOPCODE(_)
  | Ast0.OTHER(_) -> failwith "unexpected top level code"

(* ------------------------------------------------------------------- *)
(* alignment of minus and plus *)

let concat = function
    [] -> []
  | [s] -> [s]
  | l ->
      let rec loop = function
	  [] -> []
	| x::rest ->
	    (match Ast0.unwrap x with
	      Ast0.NONDECL(s) -> let stms = loop rest in s::stms
	    | Ast0.CODE(ss) ->
		let stms = loop rest in
		(Ast0.unwrap ss)@stms
	    | _ -> failwith "plus code is being discarded") in
      let res =
	Compute_lines.compute_statement_dots_lines false
	  (Ast0.rewrap (List.hd l) (loop l)) in
      [Ast0.rewrap res (Ast0.CODE res)]

let collect_up_to m plus =
  let minfo = Ast0.get_info m in
  let mend = minfo.Ast0.pos_info.Ast0.logical_end in
  let rec loop = function
      [] -> ([],[])
    | p::plus ->
	let pinfo = Ast0.get_info p in
	let pstart = pinfo.Ast0.pos_info.Ast0.logical_start in
	if pstart > mend
	then ([],p::plus)
	else let (plus,rest) = loop plus in (p::plus,rest) in
  let (plus,rest) = loop plus in
  (concat plus,rest)

let realign minus plus =
  let rec loop = function
      ([],_) -> failwith "not possible, some context required"
    | ([m],p) -> ([m],concat p)
    | (m::minus,plus) ->
	let (p,plus) = collect_up_to m plus in
	let (minus,plus) = loop (minus,plus) in
	(m::minus,p@plus) in
  loop (minus,plus)

(* ------------------------------------------------------------------- *)
(* check compatible: check that at the top level the minus and plus code is
of the same kind.  Could go further and make the correspondence between the
code between ...s. *)

let isonly f l = match Ast0.unwrap l with [s] -> f s | _ -> false

let isall f l = List.for_all (isonly f) l
let isany f l = List.exists (isonly f) l

let rec is_exp s =
  match Ast0.unwrap s with
    Ast0.Exp(e) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_exp stmts
  | Ast0.Conj(_,s::_,_,_) ->
      (* the parser ensures that if the Conj matches a
	 statement, the statement is in the first position *)
      isonly is_exp s
  | _ -> false

let rec is_ty s =
  match Ast0.unwrap s with
    Ast0.Ty(e) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_ty stmts
  | Ast0.Conj(_,s::_,_,_) ->
      (* the parser ensures that if the Conj matches a
	 statement, the statement is in the first position *)
      isonly is_ty s
  | _ -> false

let rec is_init s =
  match Ast0.unwrap s with
    Ast0.TopInit(e) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_init stmts
  | Ast0.Conj(_,stmts,_,_) -> isany is_init stmts
  | _ -> false

let rec is_decl s =
  match Ast0.unwrap s with
    Ast0.Decl(_,e) -> true
  | Ast0.FunDecl(_,_,_,_,_,_,_,_,_,_,_) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_decl stmts
  | Ast0.Conj(_,stmts,_,_) -> isany is_decl stmts
  | _ -> false

let rec is_fndecl s =
  match Ast0.unwrap s with
    Ast0.FunDecl(_,_,_,_,_,_,_,_,_,_,_) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_fndecl stmts
  | Ast0.Conj(_,stmts,_,_) -> isany is_fndecl stmts
  | _ -> false

let rec is_toplevel s =
  match Ast0.unwrap s with
    Ast0.Decl(_,e) -> true
  | Ast0.FunDecl(_,_,_,_,_,_,_,_,_,_,_) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_toplevel stmts
  | Ast0.Conj(_,stmts,_,_) -> isany is_toplevel stmts
  | Ast0.ExprStatement(Some fc,_) ->
      (match Ast0.unwrap fc with
	Ast0.FunCall(_,_,_,_) -> true
      |	_ -> false)
  | Ast0.Include(_,_) -> true
  | Ast0.Undef(_,_) -> true
  | Ast0.Pragma(_,_,_) -> true
  | Ast0.Define(_,_,_,_) -> true
  | _ -> false

(* consider code and topcode to be the same; difference handled
in top_level.ml *)
let check_compatible m p =
  let fail _ =
    failwith
      (Printf.sprintf
	 "incompatible minus and plus code starting on lines %d and %d"
	 (Ast0.get_line m) (Ast0.get_line p)) in
  match (Ast0.unwrap m, Ast0.unwrap p) with
    (Ast0.NONDECL(decl1),Ast0.NONDECL(decl2)) ->
      if not (is_decl decl1 && is_decl decl2)
      then fail()
  | (Ast0.NONDECL(decl1),Ast0.CODE(code2)) ->
      (* This is probably the only important case.  We don't want to
	 replace top-level declarations by arbitrary code. *)
      let v1 = is_decl decl1 in
      let v2 = List.for_all is_toplevel (Ast0.unwrap code2) in
      if !Flag.make_hrule = None && v1 && not v2
      then fail()
  | (Ast0.CODE(code1),Ast0.NONDECL(decl2)) ->
      let v1 = List.for_all is_toplevel (Ast0.unwrap code1) in
      let v2 = is_decl decl2 in
      if v1 && not v2
      then fail()
  | (Ast0.CODE(code1),Ast0.CODE(code2)) ->
      let v1 = isonly is_init code1 in
      let v2a = isonly is_init code2 in
      let v2b = isonly is_exp code2 in
      if v1
      then (if not (v2a || v2b) then fail())
      else
	let testers = [is_exp;is_ty] in
	List.iter
	  (function tester ->
	    let v1 = isonly tester code1 in
	    let v2 = isonly tester code2 in
	    if (v1 && not v2) || (!Flag.make_hrule = None && v2 && not v1)
	    then fail())
	  testers;
	let v1 = isonly is_fndecl code1 in
	let v2 = List.for_all is_toplevel (Ast0.unwrap code2) in
	if !Flag.make_hrule = None && v1 && not v2
	then fail()
  | (Ast0.FILEINFO(_,_),Ast0.FILEINFO(_,_)) -> ()
  | (Ast0.OTHER(_),Ast0.OTHER(_)) -> ()
  | _ -> fail()

(* can't just remove expressions or types, not sure if all cases are needed. *)
let check_complete m =
   match Ast0.unwrap m with
     Ast0.NONDECL(code) ->
       if is_exp code || is_ty code
       then
	 failwith
	   (Printf.sprintf "invalid minus starting on line %d"
	      (Ast0.get_line m))
   | Ast0.CODE(code) ->
       if isonly is_exp code || isonly is_ty code
       then
	 failwith
           (Printf.sprintf "invalid minus starting on line %d"
              (Ast0.get_line m))
   | _ -> ()

(* ------------------------------------------------------------------- *)

(* returns a list of corresponding minus and plus trees *)
let context_neg minus plus =
  Hashtbl.clear minus_table;
  Hashtbl.clear plus_table;
  List.iter contextify_whencode minus;
  let (minus,plus) = realign minus plus in
  let rec loop = function
      ([],[]) -> []
    | ([],l) ->
	failwith (Printf.sprintf "%d plus things remaining" (List.length l))
    | (minus,[]) ->
	List.iter check_complete minus;
	plus_lines := [];
	let _ =
	  List.map
	    (function m ->
	      classify true
		(function _ ->
		  Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)))
		minus_table m)
	    minus in
	[]
    | (((m::minus) as mall),((p::plus) as pall)) ->
	let minfo = Ast0.get_info m in
	let pinfo = Ast0.get_info p in
	let mstart = minfo.Ast0.pos_info.Ast0.logical_start in
	let mend = minfo.Ast0.pos_info.Ast0.logical_end in
	let pstart = pinfo.Ast0.pos_info.Ast0.logical_start in
	let pend = pinfo.Ast0.pos_info.Ast0.logical_end in
	if (iscode m || iscode p) &&
	  (mend + 1 = pstart || pend + 1 = mstart || (* adjacent *)
	   (mstart <= pstart && mend >= pstart) ||
	   (pstart <= mstart && pend >= mstart)) (* overlapping or nested *)
	then
	  begin
	    (* ensure that the root of each tree has a unique index,
	       although it might get overwritten if the node is a context
	       node *)
	    let i = Ast0.fresh_index() in
	    Ast0.set_index m i; Ast0.set_index p i;
	    check_compatible m p;
	    collect_plus_lines p;
	    let _ =
	      classify true
		(function _ ->
		  Ast0.MINUS(ref(Ast.NOREPLACEMENT,Ast0.default_token_info)))
		minus_table m in
	    let _ = classify false (function c -> Ast0.PLUS c) plus_table p in
	    traverse minus_table plus_table;
	    (m,p)::loop(minus,plus)
	  end
	else
	  if not(iscode m || iscode p)
	  then loop(minus,plus)
	  else
	    if mstart < pstart
	    then
	      begin
		plus_lines := [];
		let _ =
		  classify true
		    (function _ ->
		      Ast0.MINUS(ref(Ast.NOREPLACEMENT,
				     Ast0.default_token_info)))
		    minus_table m in
		loop(minus,pall)
	      end
	    else loop(mall,plus) in
  loop(minus,plus)
