(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Computes starting and ending logical lines for statements and
expressions.  every node gets an index as well. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Result *)

(* This is a horrible hack.  We need to have a special treatment for the code
inside a nest, and this is to avoid threading that information around
everywhere *)
let in_nest_count = ref 0
let check_attachable v = if !in_nest_count > 0 then false else v

let mkres x e left right =
  let lstart = Ast0.get_info left in
  let lend = Ast0.get_info right in
  let pos_info =
    { Ast0.line_start = lstart.Ast0.pos_info.Ast0.line_start;
      Ast0.line_end = lend.Ast0.pos_info.Ast0.line_end;
      Ast0.logical_start = lstart.Ast0.pos_info.Ast0.logical_start;
      Ast0.logical_end = lend.Ast0.pos_info.Ast0.logical_end;
      Ast0.column = lstart.Ast0.pos_info.Ast0.column;
      Ast0.offset = lstart.Ast0.pos_info.Ast0.offset;} in
  let info =
    { Ast0.pos_info = pos_info;
      Ast0.whitespace = lstart.Ast0.whitespace;
      (* not clear that the next two lines serve any purpose *)
      Ast0.attachable_start = check_attachable lstart.Ast0.attachable_start;
      Ast0.attachable_end = check_attachable lend.Ast0.attachable_end;
      Ast0.mcode_start = lstart.Ast0.mcode_start;
      Ast0.mcode_end = lend.Ast0.mcode_end;
      (* only for tokens, not inherited upwards *)
      Ast0.strings_before = []; Ast0.strings_after = [];
      Ast0.isSymbolIdent = false; } in
  {x with Ast0.node = e; Ast0.info = info}

(* This looks like it is there to allow distribution of plus code
over disjunctions.  But this doesn't work with single_statement, as the
plus code has not been distributed to the place that it expects.  So the
only reasonably easy solution seems to be to disallow distribution. *)
(* inherit attachable is because single_statement doesn't work well when +
code is attached outside an or, but this has to be allowed after
isomorphisms have been introduced.  So only set it to true then, or when we
know that the code involved cannot contain a statement, ie it is a
declaration. *)
let inherit_attachable = ref false
let mkmultires x e left right (astart,start_mcodes) (aend,end_mcodes) =
  let lstart = Ast0.get_info left in
  let lend = Ast0.get_info right in
  let pos_info =
    { Ast0.line_start = lstart.Ast0.pos_info.Ast0.line_start;
      Ast0.line_end = lend.Ast0.pos_info.Ast0.line_end;
      Ast0.logical_start = lstart.Ast0.pos_info.Ast0.logical_start;
      Ast0.logical_end = lend.Ast0.pos_info.Ast0.logical_end;
      Ast0.column = lstart.Ast0.pos_info.Ast0.column;
      Ast0.offset = lstart.Ast0.pos_info.Ast0.offset; } in
  let info =
    { Ast0.pos_info = pos_info;
      Ast0.whitespace = lstart.Ast0.whitespace;
      Ast0.attachable_start =
      check_attachable (if !inherit_attachable then astart else false);
      Ast0.attachable_end =
      check_attachable (if !inherit_attachable then aend else false);
      Ast0.mcode_start = start_mcodes;
      Ast0.mcode_end = end_mcodes;
      (* only for tokens, not inherited upwards *)
      Ast0.strings_before = []; Ast0.strings_after = [];
      Ast0.isSymbolIdent = false; } in
  {x with Ast0.node = e; Ast0.info = info}

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let promote_mcode (_,_,info,mcodekind,_,_) =
  let new_info =
    {info with
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind]} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

let set_mcode_info (a,b,_,c,d,e) info = (a,b,info,c,d,e)

let promote_mcode_plus_one (_,_,info,mcodekind,_,_) =
  let new_pos_info =
    {info.Ast0.pos_info with
      Ast0.line_start = info.Ast0.pos_info.Ast0.line_start + 1;
      Ast0.logical_start = info.Ast0.pos_info.Ast0.logical_start + 1;
      Ast0.line_end = info.Ast0.pos_info.Ast0.line_end + 1;
      Ast0.logical_end = info.Ast0.pos_info.Ast0.logical_end + 1; } in
  let new_info =
    {info with
      Ast0.pos_info = new_pos_info;
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind]} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

let promote_to_statement stm mcodekind =
  let info = Ast0.get_info stm in
  let new_pos_info =
    {info.Ast0.pos_info with
      Ast0.logical_start = info.Ast0.pos_info.Ast0.logical_end;
      Ast0.line_start = info.Ast0.pos_info.Ast0.line_end; } in
  let new_info =
    {info with
      Ast0.pos_info = new_pos_info;
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind];
      Ast0.attachable_start = check_attachable true;
      Ast0.attachable_end = check_attachable true} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

let promote_to_statement_start stm mcodekind =
  let info = Ast0.get_info stm in
  let new_pos_info =
    {info.Ast0.pos_info with
      Ast0.logical_end = info.Ast0.pos_info.Ast0.logical_start;
      Ast0.line_end = info.Ast0.pos_info.Ast0.line_start; } in
  ({info with
     Ast0.pos_info = new_pos_info;
     Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind];
     Ast0.attachable_start = check_attachable true;
     Ast0.attachable_end = check_attachable true;
     Ast0.strings_after = []},
   Ast0.set_info stm {info with Ast0.strings_before = []})

let promote_to_statement_end stm mcodekind =
  let info = Ast0.get_info stm in
  let new_pos_info =
    {info.Ast0.pos_info with
      Ast0.logical_start = info.Ast0.pos_info.Ast0.logical_end;
      Ast0.line_start = info.Ast0.pos_info.Ast0.line_end; } in
  let new_info =
    {info with
     Ast0.pos_info = new_pos_info;
     Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind];
     Ast0.attachable_start = check_attachable true;
     Ast0.attachable_end = check_attachable true;
     Ast0.strings_before = []} in
  (new_info,
   {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind},
   Ast0.set_info stm {info with Ast0.strings_after = []})

let promote_to_statement_end_mcode (a,ar,info,mc,al,adj) mcodekind =
  let new_pos_info =
    {info.Ast0.pos_info with
      Ast0.logical_end = info.Ast0.pos_info.Ast0.logical_start;
      Ast0.line_end = info.Ast0.pos_info.Ast0.line_start; } in
  let new_info =
    {info with
     Ast0.pos_info = new_pos_info;
     Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind];
     Ast0.attachable_start = check_attachable true;
     Ast0.attachable_end = check_attachable true;
     Ast0.strings_before = []} in
  (new_info,
   {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind},
   (a,ar,{info with Ast0.strings_after = []},mc,al,adj))

(* mcode is good by default *)
let bad_mcode (t,a,info,mcodekind,pos,adj) =
  let new_info =
    {info with
      Ast0.attachable_start = check_attachable false;
      Ast0.attachable_end = check_attachable false} in
  (t,a,new_info,mcodekind,pos,adj)

let normal_mcode (t,a,info,mcodekind,pos,adj) =
  let new_info =
    if !in_nest_count > 0
    then
      {info with
	Ast0.attachable_start = check_attachable false;
	Ast0.attachable_end = check_attachable false}
    else info in
  (t,a,new_info,mcodekind,pos,adj)

let get_all_start_info l =
  (List.for_all (function x -> (Ast0.get_info x).Ast0.attachable_start) l,
   List.concat (List.map (function x -> (Ast0.get_info x).Ast0.mcode_start) l))

let get_all_end_info l =
  (List.for_all (function x -> (Ast0.get_info x).Ast0.attachable_end) l,
   List.concat (List.map (function x -> (Ast0.get_info x).Ast0.mcode_end) l))

(* --------------------------------------------------------------------- *)
(* Dots *)

(* for the logline classification and the mcode field, on both sides, skip
over initial minus dots, as they don't contribute anything *)
let dot_list is_dots fn = function
    [] -> failwith "dots should not be empty"
  | l ->
      let get_node l fn =
	let first = List.hd l in
	let chosen =
	  match (is_dots first, l) with (true,_::x::_) -> x | _ -> first in
	(* get the logline decorator and the mcodekind of the chosen node *)
	fn (Ast0.get_info chosen) in
      let forward = List.map fn l in
      let backward = List.rev forward in
      let (first_attachable,first_mcode) =
	get_node forward
	  (function x -> (x.Ast0.attachable_start,x.Ast0.mcode_start)) in
      let (last_attachable,last_mcode) =
	get_node backward
	  (function x -> (x.Ast0.attachable_end,x.Ast0.mcode_end)) in
      let first = List.hd forward in
      let last = List.hd backward in
      let first_info =
	{ (Ast0.get_info first) with
	  Ast0.attachable_start = check_attachable first_attachable;
	  Ast0.mcode_start = first_mcode } in
      let last_info =
	{ (Ast0.get_info last) with
	  Ast0.attachable_end = check_attachable last_attachable;
	  Ast0.mcode_end = last_mcode } in
      let first = Ast0.set_info first first_info in
      let last = Ast0.set_info last last_info in
      (forward,first,last)

let dots is_dots prev fn d =
  match (prev,Ast0.unwrap d) with
    (Some prev,[]) -> mkres d [] prev prev
  | (None,[]) ->
      Ast0.set_info d
	{(Ast0.get_info d)
	with
	  Ast0.attachable_start = check_attachable false;
	  Ast0.attachable_end = check_attachable false}
  | (_,x) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d l lstart lend

(* --------------------------------------------------------------------- *)
(* Disjunctions *)

let do_disj e starter xs mids ender processor rebuilder =
  let starter = bad_mcode starter in
  let xs = List.map processor xs in
  let mids = List.map bad_mcode mids in
  let ender = bad_mcode ender in
  mkmultires e (rebuilder starter xs mids ender)
    (promote_mcode starter) (promote_mcode ender)
    (get_all_start_info xs) (get_all_end_info xs)

(* --------------------------------------------------------------------- *)
(* Identifier *)

(* for #define name, with no value, to compute right side *)
let mkidres a b c d r = (mkres a b c d,r)

let rec full_ident i =
  match Ast0.unwrap i with
    Ast0.Id(nm) ->
      let nm = normal_mcode nm in
      let name = promote_mcode nm in
      mkidres i (Ast0.Id(nm)) name name (Some name)
  | Ast0.MetaId(nm,a,b,c) ->
      let nm = normal_mcode nm in
      let name = promote_mcode nm in
      mkidres i (Ast0.MetaId(nm,a,b,c)) name name (Some name)
  | Ast0.MetaFunc(nm,a,b) ->
      let nm = normal_mcode nm in
      let name = promote_mcode nm in
      mkidres i (Ast0.MetaFunc(nm,a,b)) name name (Some name)
  | Ast0.MetaLocalFunc(nm,a,b) ->
      let nm = normal_mcode nm in
      let name = promote_mcode nm in
      mkidres i (Ast0.MetaLocalFunc(nm,a,b)) name name (Some name)
  | Ast0.DisjId(starter,ids,mids,ender) ->
      let res =
	do_disj i starter ids mids ender ident
	  (fun starter ids mids ender ->
	    Ast0.DisjId(starter,ids,mids,ender)) in
      (res,None)
  | Ast0.ConjId(starter,ids,mids,ender) ->
      let res =
	do_disj i starter ids mids ender ident
	  (fun starter ids mids ender ->
	    Ast0.ConjId(starter,ids,mids,ender)) in
      (res,None)
  | Ast0.OptIdent(id) ->
      let (id,r) = full_ident id in mkidres i (Ast0.OptIdent(id)) id id r
  | Ast0.AsIdent(id,asid) ->
      let (id,r) = full_ident id in mkidres i (Ast0.AsIdent(id,asid)) id id r
and ident i = let (id,_) = full_ident i in id

(* --------------------------------------------------------------------- *)
(* Expression *)

let is_exp_dots e =
  match Ast0.unwrap e with
    Ast0.Edots(_,_) -> true
  | _ -> false

let is_str_dots e =
  match Ast0.unwrap e with
    Ast0.Strdots(_) -> true
  | _ -> false

let assignOp op =
  let (newop, promoted) = match Ast0.unwrap op with
    | Ast0.SimpleAssign op0 ->
      let op1 = normal_mcode op0 in
      let op2 = promote_mcode op1 in
      ( (Ast0.SimpleAssign op1), op2)
    | Ast0.OpAssign op0 ->
      let op1 = normal_mcode op0 in
      let op2 = promote_mcode op1 in
      ( (Ast0.OpAssign op1), op2)
    | Ast0.MetaAssign (mv0, c, pure) ->
      let mv1 = normal_mcode mv0 in
      let mv2 = promote_mcode mv1 in
      ( Ast0.MetaAssign(mv1, c, pure), mv2) in
  mkres op newop promoted promoted

let binaryOp op =
  let (newop, promoted) = match Ast0.unwrap op with
    | Ast0.Arith op0 ->
      let op1 = normal_mcode op0 in
      let op2 = promote_mcode op1 in
      ( (Ast0.Arith op1), op2)
    | Ast0.Logical op0 ->
      let op1 = normal_mcode op0 in
      let op2 = promote_mcode op1 in
      ( (Ast0.Logical op1), op2)
    | Ast0.MetaBinary (mv0, c, pure) ->
      let mv1 = normal_mcode mv0 in
      let mv2 = promote_mcode mv1 in
      ( Ast0.MetaBinary(mv1, c, pure), mv2) in
  mkres op newop promoted promoted

let rec expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) ->
      let id = ident id in
      mkres e (Ast0.Ident(id)) id id
  | Ast0.Constant(const) ->
      let const = normal_mcode const in
      let ln = promote_mcode const in
      mkres e (Ast0.Constant(const)) ln ln
  | Ast0.StringConstant(lq,str,rq,isWchar) ->
      let lq = normal_mcode lq in
      let str =
	dots is_str_dots (Some(promote_mcode lq)) string_fragment str in
      let rq = normal_mcode rq in
      mkres e (Ast0.StringConstant(lq,str,rq,isWchar))
	(promote_mcode lq) (promote_mcode rq)
  | Ast0.FunCall(fn,lp,args,rp) ->
      let fn = expression fn in
      let lp = normal_mcode lp in
      let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
      let rp = normal_mcode rp in
      mkres e (Ast0.FunCall(fn,lp,args,rp)) fn (promote_mcode rp)
  | Ast0.Assignment(left,op,right,simple) ->
      let left = expression left in
      let op = assignOp op in
      let right = expression right in
      mkres e (Ast0.Assignment(left,op,right,simple)) left right
  | Ast0.Sequence(left,op,right) ->
      let left = expression left in
      let op = normal_mcode op in
      let right = expression right in
      mkres e (Ast0.Sequence(left,op,right)) left right
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let exp1 = expression exp1 in
      let why = normal_mcode why in
      let exp2 = get_option expression exp2 in
      let colon = normal_mcode colon in
      let exp3 = expression exp3 in
      mkres e (Ast0.CondExpr(exp1,why,exp2,colon,exp3)) exp1 exp3
  | Ast0.Postfix(exp,op) ->
      let exp = expression exp in
      let op = normal_mcode op in
      mkres e (Ast0.Postfix(exp,op)) exp (promote_mcode op)
  | Ast0.Infix(exp,op) ->
      let exp = expression exp in
      let op = normal_mcode op in
      mkres e (Ast0.Infix(exp,op)) (promote_mcode op) exp
  | Ast0.Unary(exp,op) ->
      let exp = expression exp in
      let op = normal_mcode op in
      mkres e (Ast0.Unary(exp,op)) (promote_mcode op) exp
  | Ast0.Binary(left,op,right) ->
      let left = expression left in
      let op = binaryOp op in
      let right = expression right in
      mkres e (Ast0.Binary(left,op,right)) left right
  | Ast0.Nested(left,op,right) ->
      let left = expression left in
      let op = binaryOp op in
      let right = expression right in
      mkres e
        (Ast0.Nested(left,op,right)) left right
  | Ast0.Paren(lp,exp,rp) ->
      let lp = normal_mcode lp in
      let rp = normal_mcode rp in
      mkres e (Ast0.Paren(lp,expression exp,rp))
	(promote_mcode lp) (promote_mcode rp)
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      let exp1 = expression exp1 in
      let lb = normal_mcode lb in
      let exp2 = expression exp2 in
      let rb = normal_mcode rb in
      mkres e (Ast0.ArrayAccess(exp1,lb,exp2,rb)) exp1 (promote_mcode rb)
  | Ast0.RecordAccess(exp,pt,field) ->
      let exp = expression exp in
      let pt = normal_mcode pt in
      let field = ident field in
      mkres e (Ast0.RecordAccess(exp,pt,field)) exp field
  | Ast0.RecordPtAccess(exp,ar,field) ->
      let exp = expression exp in
      let ar = normal_mcode ar in
      let field = ident field in
      mkres e (Ast0.RecordPtAccess(exp,ar,field)) exp field
  | Ast0.Cast(lp,ty,attr,rp,exp) ->
      let lp = normal_mcode lp in
      let exp = expression exp in
      let attr = List.map attribute attr in
      let rp = normal_mcode rp in
      mkres e (Ast0.Cast(lp,typeC ty,attr,rp,exp)) (promote_mcode lp) exp
  | Ast0.SizeOfExpr(szf,exp) ->
      let szf = normal_mcode szf in
      let exp = expression exp in
      mkres e (Ast0.SizeOfExpr(szf,exp)) (promote_mcode szf) exp
  | Ast0.SizeOfType(szf,lp,ty,rp) ->
      let szf = normal_mcode szf in
      let lp = normal_mcode lp in
      let rp = normal_mcode rp in
      mkres e (Ast0.SizeOfType(szf,lp,typeC ty,rp))
        (promote_mcode szf)  (promote_mcode rp)
  | Ast0.TypeExp(ty) ->
      let ty = typeC ty in mkres e (Ast0.TypeExp(ty)) ty ty
  | Ast0.Constructor(lp,ty,rp,init) ->
      let lp = normal_mcode lp in
      let init = initialiser init in
      let rp = normal_mcode rp in
      mkres e (Ast0.Constructor(lp,typeC ty,rp,init)) (promote_mcode lp) init
  | Ast0.MetaErr(name,a,b) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres e (Ast0.MetaErr(name,a,b)) ln ln
  | Ast0.MetaExpr(name,a,b,c,d,f) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres e (Ast0.MetaExpr(name,a,b,c,d,f)) ln ln
  | Ast0.MetaExprList(name,a,b,c) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres e (Ast0.MetaExprList(name,a,b,c)) ln ln
  | Ast0.EComma(cm) ->
      let cm = normal_mcode cm in
      let ln = promote_mcode cm in
      mkres e (Ast0.EComma(cm)) ln ln
  | Ast0.DisjExpr(starter,exps,mids,ender) ->
      do_disj e starter exps mids ender expression
	(fun starter exps mids ender -> Ast0.DisjExpr(starter,exps,mids,ender))
  | Ast0.ConjExpr(starter,exps,mids,ender) ->
      do_disj e starter exps mids ender expression
	(fun starter exps mids ender -> Ast0.ConjExpr(starter,exps,mids,ender))
  | Ast0.NestExpr(starter,exp_dots,ender,whencode,multi) ->
      (* See explanation on Nest *)
	let wrapper f =
	  match Ast0.get_mcode_mcodekind starter with
	    Ast0.MINUS _ ->
	      in_nest_count := !in_nest_count + 1;
	      let res = f() in
	      in_nest_count := !in_nest_count - 1;
	      res
	  | _ -> f() in
      let exp_dots =
	wrapper (function _ -> dots is_exp_dots None expression exp_dots) in
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      mkres e (Ast0.NestExpr(starter,exp_dots,ender,whencode,multi))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.Edots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Edots(dots,whencode)) ln ln
  | Ast0.OptExp(exp) ->
      let exp = expression exp in
      mkres e (Ast0.OptExp(exp)) exp exp
  | Ast0.AsExpr(exp,asexp) ->
      let exp = expression exp in
      mkres e (Ast0.AsExpr(exp,asexp)) exp exp
  | Ast0.AsSExpr(exp,asexp) ->
      let exp = expression exp in
      mkres e (Ast0.AsSExpr(exp,asexp)) exp exp

and expression_dots x = dots is_exp_dots None expression x

and string_fragment e =
  match Ast0.unwrap e with
    Ast0.ConstantFragment(str) ->
      let str = normal_mcode str in
      let ln = promote_mcode str in
      mkres e (Ast0.ConstantFragment(str)) ln ln
  | Ast0.FormatFragment(pct,fmt) ->
      let pct = normal_mcode pct in
      let ln = promote_mcode pct in
      let fmt = string_format fmt in
      mkres e (Ast0.FormatFragment(pct,fmt)) ln fmt
  | Ast0.Strdots dots ->
      let dots = normal_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Strdots dots) ln ln
  | Ast0.MetaFormatList(pct,name,cstr,lenname) ->
      (* not sure what to do about the following comment... *)
      (* pct is particularly bad in this case, because it is ignored in
	 the matching process.  The metavariable matches the complete format
	 specification, including the % *)
      let pct = normal_mcode pct in
      let ln1 = promote_mcode pct in
      let name = normal_mcode name in
      let ln2 = promote_mcode name in
      mkres e (Ast0.MetaFormatList(pct,name,cstr,lenname)) ln1 ln2

and string_format e =
  match Ast0.unwrap e with
    Ast0.ConstantFormat(str) ->
      let str = normal_mcode str in
      let ln = promote_mcode str in
      mkres e (Ast0.ConstantFormat str) ln ln
  | Ast0.MetaFormat(name,constraints) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres e (Ast0.MetaFormat(name,constraints)) ln ln

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) ->
      let cv = normal_mcode cv in
      let ty = typeC ty in
      mkres t (Ast0.ConstVol(cv,ty)) (promote_mcode cv) ty
  | Ast0.BaseType(ty,strings) ->
      let strings = List.map normal_mcode strings in
      let first = List.hd strings in
      let last = List.hd (List.rev strings) in
      mkres t (Ast0.BaseType(ty,strings))
	(promote_mcode first) (promote_mcode last)
  | Ast0.Signed(sgn,None) ->
      let sgn = normal_mcode sgn in
      mkres t (Ast0.Signed(sgn,None)) (promote_mcode sgn) (promote_mcode sgn)
  | Ast0.Signed(sgn,Some ty) ->
      let sgn = normal_mcode sgn in
      let ty = typeC ty in
      mkres t (Ast0.Signed(sgn,Some ty)) (promote_mcode sgn) ty
  | Ast0.Pointer(ty,star) ->
      let ty = typeC ty in
      let star = normal_mcode star in
      mkres t (Ast0.Pointer(ty,star)) ty (promote_mcode star)
  | Ast0.ParenType(lp,ty,rp) ->
      let lp = normal_mcode lp in
      let rp = normal_mcode rp in
      let ty = typeC ty in
      mkres t (Ast0.ParenType(lp,ty,rp)) ty (promote_mcode rp)
  | Ast0.FunctionType(ty,lp,params,rp) ->
      let ty = typeC ty in
      let lp = normal_mcode lp in
      let params = parameter_list (Some(promote_mcode lp)) params in
      let rp = normal_mcode rp in
      mkres t (Ast0.FunctionType(ty,lp,params,rp)) ty (promote_mcode rp)
  | Ast0.Array(ty,lb,size,rb) ->
      let ty = typeC ty in
      let lb = normal_mcode lb in
      let rb = normal_mcode rb in
      mkres t (Ast0.Array(ty,lb,get_option expression size,rb))
	ty (promote_mcode rb)
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      let dec = normal_mcode dec in
      let lp = normal_mcode lp in
      let length = expression length in
      let comma = get_option normal_mcode comma in
      let precision_opt = get_option expression precision_opt in
      let rp = normal_mcode rp in
      mkres t (Ast0.Decimal(dec,lp,length,comma,precision_opt,rp))
	(promote_mcode dec) (promote_mcode rp)
  | Ast0.EnumName(kind,Some name) ->
      let kind = normal_mcode kind in
      let name = ident name in
      mkres t (Ast0.EnumName(kind,Some name)) (promote_mcode kind) name
  | Ast0.EnumName(kind,None) ->
      let kind = normal_mcode kind in
      let mc = promote_mcode kind in
      mkres t (Ast0.EnumName(kind,None)) mc mc
  | Ast0.EnumDef(ty,lb,ids,rb) ->
      let ty = typeC ty in
      let lb = normal_mcode lb in
      let ids =
        dots is_enum_decl_dots (Some(promote_mcode lb)) enum_decl ids in
      let rb = normal_mcode rb in
      mkres t (Ast0.EnumDef(ty,lb,ids,rb)) ty (promote_mcode rb)
  | Ast0.StructUnionName(kind,Some name) ->
      let kind = normal_mcode kind in
      let name = ident name in
      mkres t (Ast0.StructUnionName(kind,Some name)) (promote_mcode kind) name
  | Ast0.StructUnionName(kind,None) ->
      let kind = normal_mcode kind in
      let mc = promote_mcode kind in
      mkres t (Ast0.StructUnionName(kind,None)) mc mc
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      let ty = typeC ty in
      let lb = normal_mcode lb in
      let decls =
	dots is_field_dots (Some(promote_mcode lb)) field decls in
      let rb = normal_mcode rb in
      mkres t (Ast0.StructUnionDef(ty,lb,decls,rb)) ty (promote_mcode rb)
  | Ast0.TypeOfExpr(tf,lp,exp,rp) ->
      let tf = normal_mcode tf in
      let lp = normal_mcode lp in
      let exp = expression exp in
      let rp = normal_mcode rp in
      mkres t (Ast0.TypeOfExpr(tf,lp,exp,rp))
	(promote_mcode tf) (promote_mcode rp)
  | Ast0.TypeOfType(tf,lp,ty,rp) ->
      let tf = normal_mcode tf in
      let lp = normal_mcode lp in
      let rp = normal_mcode rp in
      mkres t (Ast0.TypeOfType(tf,lp,typeC ty,rp))
        (promote_mcode tf) (promote_mcode rp)
  | Ast0.TypeName(name) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres t (Ast0.TypeName(name)) ln ln
  | Ast0.AutoType(auto) ->
      let auto = normal_mcode auto in
      let la = promote_mcode auto in
      mkres t (Ast0.AutoType(auto)) la la
  | Ast0.MetaType(name,cstr,a) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres t (Ast0.MetaType(name,cstr,a)) ln ln
  | Ast0.DisjType(starter,types,mids,ender) ->
      do_disj t starter types mids ender typeC
	(fun starter types mids ender ->
	  Ast0.DisjType(starter,types,mids,ender))
  | Ast0.ConjType(starter,types,mids,ender) ->
      do_disj t starter types mids ender typeC
	(fun starter types mids ender ->
	  Ast0.ConjType(starter,types,mids,ender))
  | Ast0.OptType(ty) ->
      let ty = typeC ty in mkres t (Ast0.OptType(ty)) ty ty
  | Ast0.AsType _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and declaration d =
  match Ast0.unwrap d with
    Ast0.MetaDecl(name,a,b) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres d (Ast0.MetaDecl(name,a,b)) ln ln
  | Ast0.Init(stg,ty,id,attr,eq,exp,sem) ->
      let ty = typeC ty in
      let id = ident id in
      let attr = List.map attribute attr in
      let eq = normal_mcode eq in
      let exp = initialiser exp in
      let sem = normal_mcode sem in
      (match stg with
	None ->
	  mkres d (Ast0.Init(stg,ty,id,attr,eq,exp,sem)) ty (promote_mcode sem)
      | Some x ->
	  let stg = Some (normal_mcode x) in
	  mkres d (Ast0.Init(stg,ty,id,attr,eq,exp,sem))
	    (promote_mcode x) (promote_mcode sem))
  | Ast0.UnInit(stg,ty,id,attr,sem) ->
      let ty = typeC ty in
      let id = ident id in
      let attr = List.map attribute attr in
      let sem = normal_mcode sem in
      (match stg with
	None ->
	  mkres d (Ast0.UnInit(stg,ty,id,attr,sem)) ty (promote_mcode sem)
      | Some x ->
	  let stg = Some (normal_mcode x) in
	  mkres d (Ast0.UnInit(stg,ty,id,attr,sem))
	    (promote_mcode x) (promote_mcode sem))
  | Ast0.FunProto(fninfo,name,lp1,params,va1,rp1,sem) ->
      let fninfo =
	List.map
	  (function Ast0.FType(ty) -> Ast0.FType(typeC ty) | x -> x)
	  fninfo in
      let name = ident name in
      let lp1 = normal_mcode lp1 in
      let params = parameter_list (Some(promote_mcode lp1)) params in
      let va1 = match va1 with
        | None -> None
        | Some (c1,e1) -> Some (normal_mcode c1, normal_mcode e1) in
      let rp1 = normal_mcode rp1 in
      let sem = normal_mcode sem in
      let res = Ast0.FunProto(fninfo,name,lp1,params,va1,rp1,sem) in
      let right = promote_mcode sem in
      (match fninfo with
	  [] -> mkres d res name right
	| Ast0.FStorage(stg)::_ -> mkres d res (promote_mcode stg) right
	| Ast0.FType(ty)::_ -> mkres d res ty right
	| Ast0.FInline(inline)::_ -> mkres d res (promote_mcode inline) right
	| Ast0.FAttr(attr)::_ -> mkres d res attr right)
  | Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
      let name = ident name in
      let lp = normal_mcode lp in
      let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
      let rp = normal_mcode rp in
      let attr = List.map attribute attr in
      let sem = normal_mcode sem in
      (match stg with
	None ->
	  mkres d (Ast0.MacroDecl(None,name,lp,args,rp,attr,sem))
	    name (promote_mcode sem)
      | Some x ->
	  let stg = Some (normal_mcode x) in
	  mkres d (Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem))
	    (promote_mcode x) (promote_mcode sem))
  | Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
      let name = ident name in
      let lp = normal_mcode lp in
      let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
      let rp = normal_mcode rp in
      let eq = normal_mcode eq in
      let ini = initialiser ini in
      let sem = normal_mcode sem in
      (match stg with
	None ->
	  mkres d (Ast0.MacroDeclInit(None,name,lp,args,rp,eq,ini,sem))
	    name (promote_mcode sem)
      | Some x ->
	  let stg = Some (normal_mcode x) in
	  mkres d (Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem))
	    (promote_mcode x) (promote_mcode sem))
  | Ast0.TyDecl(ty,attr,sem) ->
      let ty = typeC ty in
      let attr = List.map attribute attr in
      let sem = normal_mcode sem in
      mkres d (Ast0.TyDecl(ty,attr,sem)) ty (promote_mcode sem)
  | Ast0.Typedef(stg,ty,id,sem) ->
      let stg = normal_mcode stg in
      let ty = typeC ty in
      let id = typeC id in
      let sem = normal_mcode sem in
      mkres d (Ast0.Typedef(stg,ty,id,sem))
	(promote_mcode stg) (promote_mcode sem)
  | Ast0.DisjDecl(starter,decls,mids,ender) ->
      do_disj d starter decls mids ender declaration
	(fun starter decls mids ender ->
	  Ast0.DisjDecl(starter,decls,mids,ender))
  | Ast0.ConjDecl(starter,decls,mids,ender) ->
      do_disj d starter decls mids ender declaration
	(fun starter decls mids ender ->
	  Ast0.ConjDecl(starter,decls,mids,ender))
  | Ast0.OptDecl(decl) ->
      let decl = declaration decl in
      mkres d (Ast0.OptDecl(declaration decl)) decl decl
  | Ast0.AsDecl _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and is_field_dots s =
  match Ast0.unwrap s with
    Ast0.Fdots(_,_) -> true
  | _ -> false

and field d =
  match Ast0.unwrap d with
     Ast0.MetaField(name,a,b) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres d (Ast0.MetaField(name,a,b)) ln ln
  | Ast0.MetaFieldList(name,a,b,c) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres d (Ast0.MetaFieldList(name,a,b,c)) ln ln
  | Ast0.Field(ty,id,bf,sem) ->
      let ty = typeC ty in
      let id = Common.map_option ident id in
      let bitfield (c, e) = (normal_mcode c, expression e) in
      let bf = Common.map_option bitfield bf in
      let sem = normal_mcode sem in
      mkres d (Ast0.Field(ty,id,bf,sem)) ty (promote_mcode sem)
  | Ast0.DisjField(starter,decls,mids,ender) ->
      do_disj d starter decls mids ender field
	(fun starter decls mids ender ->
	  Ast0.DisjField(starter,decls,mids,ender))
  | Ast0.ConjField(starter,decls,mids,ender) ->
      do_disj d starter decls mids ender field
	(fun starter decls mids ender ->
	  Ast0.ConjField(starter,decls,mids,ender))
  | Ast0.OptField(decl) ->
      let decl = field decl in
      mkres d (Ast0.OptField(field decl)) decl decl
  | Ast0.Fdots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres d (Ast0.Fdots(dots,whencode)) ln ln

and is_enum_decl_dots d =
  match Ast0.unwrap d with
    Ast0.EnumDots(_) -> true
  | _ -> false

and enum_decl d =
  match Ast0.unwrap d with
     Ast0.Enum(name,enum_val) ->
      let name = ident name in
      let eval (a, b) = (normal_mcode a, expression b) in
      let enum_val = get_option eval enum_val in
      mkres d (Ast0.Enum(name,enum_val)) name name
  | Ast0.EnumComma(cm) ->
      let cm = normal_mcode cm in
      let ln = promote_mcode cm in
      mkres d (Ast0.EnumComma(cm)) ln ln
  | Ast0.EnumDots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres d (Ast0.EnumDots(dots,whencode)) ln ln


(* --------------------------------------------------------------------- *)
(* Initializer *)

and is_init_dots i =
  match Ast0.unwrap i with
    Ast0.Idots(_,_) -> true
  | _ -> false

and initialiser i =
  match Ast0.unwrap i with
    Ast0.MetaInit(name,a,b) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres i (Ast0.MetaInit(name,a,b)) ln ln
  | Ast0.MetaInitList(name,a,b,c) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres i (Ast0.MetaInitList(name,a,b,c)) ln ln
  | Ast0.InitExpr(exp) ->
      let exp = expression exp in
      mkres i (Ast0.InitExpr(exp)) exp exp
  | Ast0.InitList(lb,initlist,rb,ordered) ->
      let lb = normal_mcode lb in
      let initlist =
	dots is_init_dots (Some(promote_mcode lb)) initialiser initlist in
      let rb = normal_mcode rb in
      mkres i (Ast0.InitList(lb,initlist,rb,ordered))
	(promote_mcode lb) (promote_mcode rb)
  | Ast0.InitGccExt(designators,eq,ini) ->
      let (delims,designators) = (* non empty due to parsing *)
	List.split (List.map designator designators) in
      let eq = normal_mcode eq in
      let ini = initialiser ini in
      mkres i (Ast0.InitGccExt(designators,eq,ini))
	(promote_mcode (List.hd delims)) ini
  | Ast0.InitGccName(name,eq,ini) ->
      let name = ident name in
      let eq = normal_mcode eq in
      let ini = initialiser ini in
      mkres i (Ast0.InitGccName(name,eq,ini)) name ini
  | Ast0.IComma(cm) ->
      let cm = normal_mcode cm in
      let ln = promote_mcode cm in
      mkres i (Ast0.IComma(cm)) ln ln
  | Ast0.Idots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres i (Ast0.Idots(dots,whencode)) ln ln
  | Ast0.OptIni(ini) ->
      let ini = initialiser ini in
      mkres i (Ast0.OptIni(ini)) ini ini
  | Ast0.AsInit _ -> failwith "not possible"

and designator = function
    Ast0.DesignatorField(dot,id) ->
      let dot = normal_mcode dot in
      (dot,Ast0.DesignatorField(dot,ident id))
  | Ast0.DesignatorIndex(lb,exp,rb) ->
      let lb = normal_mcode lb in
      let rb = normal_mcode rb in
      (lb,Ast0.DesignatorIndex(lb,expression exp,rb))
  | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
      let lb = normal_mcode lb in
      let dots = normal_mcode dots in
      let rb = normal_mcode rb in
      (lb,Ast0.DesignatorRange(lb,expression min,dots,expression max,rb))

and initialiser_list prev = dots is_init_dots prev initialiser

(* for export *)
and initialiser_dots x = dots is_init_dots None initialiser x

and attribute attr =
  match Ast0.unwrap attr with
    Ast0.Attribute(a) ->
      let ln = attr_arg a in
      mkres attr (Ast0.Attribute(a)) ln ln
  | Ast0.GccAttribute(attr_,lp1,lp2,arg,rp1,rp2) ->
      let attr_ = normal_mcode attr_ in
      let lp1 = normal_mcode lp1 in
      let lp2 = normal_mcode lp2 in
      let arg = attr_arg arg in
      let rp1 = normal_mcode rp1 in
      let rp2 = normal_mcode rp2 in
      let ln1 = promote_mcode attr_ in
      let ln2 = promote_mcode rp2 in
      mkres attr (Ast0.GccAttribute(attr_,lp1,lp2,arg,rp1,rp2)) ln1 ln2

and attr_arg arg =
  match Ast0.unwrap arg with
    Ast0.AttrName(a) ->
      let ln = promote_mcode a in
      mkres arg (Ast0.AttrName(a)) ln ln
  | Ast0.MetaAttr(name,a,b) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres arg (Ast0.MetaAttr(name,a,b)) ln ln

(* --------------------------------------------------------------------- *)
(* Parameter *)

and is_param_dots p =
  match Ast0.unwrap p with
    Ast0.Pdots(_) -> true
  | _ -> false

and parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty,attr) ->
      let attr = List.map attribute attr in
      let ty = typeC ty in
      (match attr with
        [] -> mkres p (Ast0.VoidParam(ty,attr)) ty ty
      | l ->
          let lattr = List.hd (List.rev l) in
          mkres p (Ast0.VoidParam(ty,attr)) ty lattr)
  | Ast0.Param(ty,Some id,attr) ->
      let id = ident id in
      let ty = typeC ty in
      let attr = List.map attribute attr in
      (match attr with
        [] -> mkres p (Ast0.Param(ty,Some id,attr)) ty id
      | l ->
          let lattr = List.hd (List.rev l) in
          mkres p (Ast0.Param(ty,Some id,attr)) ty lattr)
  | Ast0.Param(ty,None,attr) ->
      let attr = List.map attribute attr in
      let ty = typeC ty in
      (match attr with
        [] -> mkres p (Ast0.Param(ty,None,attr)) ty ty
      | l ->
          let lattr = List.hd (List.rev l) in
          mkres p (Ast0.Param(ty,None,attr)) ty lattr)
  | Ast0.MetaParam(name,a,b) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres p (Ast0.MetaParam(name,a,b)) ln ln
  | Ast0.MetaParamList(name,a,b,c) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres p (Ast0.MetaParamList(name,a,b,c)) ln ln
  | Ast0.PComma(cm) ->
      let cm = normal_mcode cm in
      let ln = promote_mcode cm in
      mkres p (Ast0.PComma(cm)) ln ln
  | Ast0.Pdots(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres p (Ast0.Pdots(dots)) ln ln
  | Ast0.OptParam(param) ->
      let res = parameterTypeDef param in
      mkres p (Ast0.OptParam(res)) res res
  | Ast0.AsParam _ -> failwith "not possible"

and parameter_list prev = dots is_param_dots prev parameterTypeDef

(* for export *)
let parameter_dots x = dots is_param_dots None parameterTypeDef x

(* --------------------------------------------------------------------- *)

let is_define_param_dots s =
  match Ast0.unwrap s with
    Ast0.DPdots(_) -> true
  | _ -> false

let rec define_param p =
  match Ast0.unwrap p with
    Ast0.DParam(id) ->
      let id = ident id in mkres p (Ast0.DParam(id)) id id
  | Ast0.MetaDParamList(name,a,b,c) ->
      let name = normal_mcode name in
      let ln = promote_mcode name in
      mkres p (Ast0.MetaDParamList(name,a,b,c)) ln ln
  | Ast0.DPComma(cm) ->
      let cm = normal_mcode cm in
      let ln = promote_mcode cm in
      mkres p (Ast0.DPComma(cm)) ln ln
  | Ast0.DPdots(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres p (Ast0.DPdots(dots)) ln ln
  | Ast0.OptDParam(dp) ->
      let res = define_param dp in
      mkres p (Ast0.OptDParam(res)) res res

let define_parameters x id =
  match Ast0.unwrap x with
    Ast0.NoParams -> (x,id) (* no info, should be ignored *)
  | Ast0.DParams(lp,dp,rp) ->
      let lp = normal_mcode lp in
      let dp = dots is_define_param_dots None define_param dp in
      let rp = normal_mcode rp in
      let l = promote_mcode lp in
      let r = promote_mcode rp in
      (mkres x (Ast0.DParams(lp,dp,rp)) l r, r)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let is_stm_dots s =
  match Ast0.unwrap s with
    Ast0.Dots(_,_) -> true
  | _ -> false

let is_ec_dots e =
  match Ast0.unwrap e with
    Ast0.ExecDots(_) -> true
  | _ -> false

let rec statement s =
  let res =
    match Ast0.unwrap s with
      Ast0.Decl((info,bef),decl) ->
	let decl = declaration decl in
	let (leftinfo,decl) = promote_to_statement_start decl bef in
	let leftinfo =
	  (* for function prototypes, which may have information placed here
	     before calling Compute_lines *)
	  {leftinfo with Ast0.strings_before = info.Ast0.strings_before} in
	mkres s (Ast0.Decl((leftinfo,bef),decl)) decl decl
    | Ast0.Seq(lbrace,body,rbrace) ->
	let lbrace = normal_mcode lbrace in
	let body =
	  dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
	let rbrace = normal_mcode rbrace in
	mkres s (Ast0.Seq(lbrace,body,rbrace))
	  (promote_mcode lbrace) (promote_mcode rbrace)
    | Ast0.ExprStatement(Some exp,sem) ->
	let exp = expression exp in
	let sem = normal_mcode sem in
	mkres s (Ast0.ExprStatement(Some exp,sem)) exp (promote_mcode sem)
    | Ast0.ExprStatement(None,sem) ->
	let sem = normal_mcode sem in
	let promoted_sem = promote_mcode sem in
	mkres s (Ast0.ExprStatement(None,sem)) promoted_sem promoted_sem
    | Ast0.IfThen(iff,lp,exp,rp,branch,(_,aft,adj)) ->
	let iff = normal_mcode iff in
	let lp = normal_mcode lp in
	let exp = expression exp in
	let rp = normal_mcode rp in
	let branch = statement branch in
	let (rightinfo,right,branch) = promote_to_statement_end branch aft in
	mkres s
	  (Ast0.IfThen(iff,lp,exp,rp,branch,(rightinfo,aft,adj)))
	  (promote_mcode iff) right
    | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(_,aft,adj)) ->
	let iff = normal_mcode iff in
	let lp = normal_mcode lp in
	let exp = expression exp in
	let rp = normal_mcode rp in
	let branch1 = statement branch1 in
	let els = normal_mcode els in
	let branch2 = statement branch2 in
	let (rightinfo,right,branch2) = promote_to_statement_end branch2 aft in
	mkres s
	  (Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,
	    (rightinfo,aft,adj)))
	  (promote_mcode iff) right
    | Ast0.While(wh,lp,exp,rp,body,(_,aft,adj)) ->
	let wh = normal_mcode wh in
	let lp = normal_mcode lp in
	let exp = expression exp in
	let rp = normal_mcode rp in
	let body = statement body in
	let (rightinfo,right,body) = promote_to_statement_end body aft in
	mkres s (Ast0.While(wh,lp,exp,rp,body,(rightinfo,aft,adj)))
	  (promote_mcode wh) right
    | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
	let d = normal_mcode d in
	let body = statement body in
	let wh = normal_mcode wh in
	let lp = normal_mcode lp in
	let exp = expression exp in
	let rp = normal_mcode rp in
	mkres s (Ast0.Do(d,body,wh,lp,exp,rp,sem))
	  (promote_mcode d) (promote_mcode sem)
    | Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,(_,aft,adj)) ->
	let fr = normal_mcode fr in
	let lp = normal_mcode lp in
	let first =
	  match Ast0.unwrap first with
	    Ast0.ForExp(None,sem1) ->
	      let sem1 = normal_mcode sem1 in
	      mkres first (Ast0.ForExp(None,sem1))
		(promote_mcode sem1) (promote_mcode sem1)
	  | Ast0.ForExp(Some exp1,sem1) ->
	      let exp1 = expression exp1 in
	      let sem1 = normal_mcode sem1 in
	      mkres first (Ast0.ForExp(Some exp1,sem1))
		exp1 (promote_mcode sem1)
	  | Ast0.ForDecl((_,bef),decl) ->
	      let decl = declaration decl in
	      let (leftinfo,decl) = promote_to_statement_start decl bef in
	      mkres first (Ast0.ForDecl ((leftinfo,bef),decl)) decl decl in
	let exp2 = get_option expression exp2 in
	let sem2 = normal_mcode sem2 in
	let exp3 = get_option expression exp3 in
	let rp = normal_mcode rp in
	let body = statement body in
	let (rightinfo,right,body) = promote_to_statement_end body aft in
	mkres s (Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,
			  (rightinfo,aft,adj)))
	  (promote_mcode fr) right
    | Ast0.Iterator(nm,lp,args,rp,body,(_,aft,adj)) ->
	let nm = ident nm in
	let lp = normal_mcode lp in
	let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
	let rp = normal_mcode rp in
	let body = statement body in
	let (rightinfo,right,body) = promote_to_statement_end body aft in
	mkres s
	  (Ast0.Iterator(nm,lp,args,rp,body,(rightinfo,aft,adj)))
	  nm right
    | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
	let switch = normal_mcode switch in
	let lp = normal_mcode lp in
	let exp = expression exp in
	let rp = normal_mcode rp in
	let lb = normal_mcode lb in
	let decls =
	  dots is_stm_dots (Some(promote_mcode lb))
	    statement decls in
	let cases =
	  dots (function _ -> false)
	    (if Ast0.unwrap decls = []
	    then (Some(promote_mcode lb))
	    else None (* not sure this is right, but not sure the case can
			 arise either *))
	    case_line cases in
	let rb = normal_mcode rb in
	mkres s
	  (Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb))
	  (promote_mcode switch) (promote_mcode rb)
    | Ast0.Break(br,sem) ->
	let br = normal_mcode br in
	let sem = normal_mcode sem in
	mkres s (Ast0.Break(br,sem)) (promote_mcode br) (promote_mcode sem)
    | Ast0.Continue(cont,sem) ->
	let cont = normal_mcode cont in
	let sem = normal_mcode sem in
	mkres s (Ast0.Continue(cont,sem))
	  (promote_mcode cont) (promote_mcode sem)
    | Ast0.Label(l,dd) ->
	let l = ident l in
	let dd = normal_mcode dd in
	mkres s (Ast0.Label(l,dd)) l (promote_mcode dd)
    | Ast0.Goto(goto,id,sem) ->
	let goto = normal_mcode goto in
	let id = ident id in
	let sem = normal_mcode sem in
	mkres s (Ast0.Goto(goto,id,sem))
	  (promote_mcode goto) (promote_mcode sem)
    | Ast0.Return(ret,sem) ->
	let ret = normal_mcode ret in
	let sem = normal_mcode sem in
	mkres s (Ast0.Return(ret,sem)) (promote_mcode ret) (promote_mcode sem)
    | Ast0.ReturnExpr(ret,exp,sem) ->
	let ret = normal_mcode ret in
	let exp = expression exp in
	let sem = normal_mcode sem in
	mkres s (Ast0.ReturnExpr(ret,exp,sem))
	  (promote_mcode ret) (promote_mcode sem)
    | Ast0.Exec(exec,lang,code,sem) ->
	let exec = normal_mcode exec in
	let lang = normal_mcode lang in
	let code = dots is_ec_dots (Some(promote_mcode lang)) exec_code code in
	let sem = normal_mcode sem in
	mkres s (Ast0.Exec(exec,lang,code,sem))
	  (promote_mcode exec) (promote_mcode sem)
    | Ast0.MetaStmt(name,a,b) ->
	let name = normal_mcode name in
	let ln = promote_mcode name in
	mkres s (Ast0.MetaStmt(name,a,b)) ln ln
    | Ast0.MetaStmtList(name,a,b,c) ->
	(* This becomes ..., which becomes a metavar.  The metavar, with
	   any attachments that this has, is then iterated over the code.
	   If we attach anything here, then it will be duplicated for each
	   statement in the list. *)
	let name = bad_mcode name in
	let ln = promote_mcode name in
	mkres s (Ast0.MetaStmtList(name,a,b,c)) ln ln
    | Ast0.Exp(exp) ->
	let exp = expression exp in
	mkres s (Ast0.Exp(exp)) exp exp
    | Ast0.TopExp(exp) ->
	let exp = expression exp in
	mkres s (Ast0.TopExp(exp)) exp exp
    | Ast0.Ty(ty) ->
	let ty = typeC ty in
	mkres s (Ast0.Ty(ty)) ty ty
    | Ast0.TopId(id) ->
	let id = ident id in
	mkres s (Ast0.TopId(id)) id id
    | Ast0.TopInit(init) ->
	let init = initialiser init in
	mkres s (Ast0.TopInit(init)) init init
    | Ast0.Disj(starter,rule_elem_dots_list,mids,ender) ->
	let starter = bad_mcode starter in
	let mids = List.map bad_mcode mids in
	let ender = bad_mcode ender in
	let rec loop prevs = function
	    [] -> []
	  | stm::stms ->
	      (dots is_stm_dots (Some(promote_mcode_plus_one(List.hd prevs)))
		 statement stm)::
	      (loop (List.tl prevs) stms) in
	let elems = loop (starter::mids) rule_elem_dots_list in
	mkmultires s (Ast0.Disj(starter,elems,mids,ender))
	  (promote_mcode starter) (promote_mcode ender)
	  (get_all_start_info elems) (get_all_end_info elems)
    | Ast0.Conj(starter,rule_elem_dots_list,mids,ender) ->
	let starter = bad_mcode starter in
	let mids = List.map bad_mcode mids in
	let ender = bad_mcode ender in
	let rec loop prevs = function
	    [] -> []
	  | stm::stms ->
	      (dots is_stm_dots (Some(promote_mcode_plus_one(List.hd prevs)))
		 statement stm)::
	      (loop (List.tl prevs) stms) in
	let elems = loop (starter::mids) rule_elem_dots_list in
	mkmultires s (Ast0.Conj(starter,elems,mids,ender))
	  (promote_mcode starter) (promote_mcode ender)
	  (get_all_start_info elems) (get_all_end_info elems)
    | Ast0.Nest(starter,rule_elem_dots,ender,whencode,multi) ->
	let starter = bad_mcode starter in
	let ender = bad_mcode ender in
	let wrapper f =
	  match Ast0.get_mcode_mcodekind starter with
	    Ast0.MINUS _ ->
	      (* if minus, then all nest code has to be minus.  This is
		 checked at the token level, in parse_cocci.ml.  All nest code
		 is also unattachable.  We strip the minus annotations from
		 the nest code because in the CTL another metavariable will
		 take care of removing all the code matched by the nest.
		 Without stripping the minus annotations, we would get a
		 double transformation.  Perhaps there is a more elegant
		 way to do this in the CTL, but it is not easy, because of
		 the interaction with the whencode and the implementation of
		 plus *)
	      in_nest_count := !in_nest_count + 1;
	      let res = f() in
	      in_nest_count := !in_nest_count - 1;
	      res
	  | _ -> f() in
	let rule_elem_dots =
	  wrapper
	    (function _ -> dots is_stm_dots None statement rule_elem_dots) in
	mkres s (Ast0.Nest(starter,rule_elem_dots,ender,whencode,multi))
	  (promote_mcode starter) (promote_mcode ender)
    | Ast0.Dots(dots,whencode) ->
	let dots = bad_mcode dots in
	let ln = promote_mcode dots in
	mkres s (Ast0.Dots(dots,whencode)) ln ln
    | Ast0.FunDecl((_,bef),fninfo,name,lp,params,va,rp,lbrace,body,rbrace,
		   (_,aft)) ->
	let fninfo =
	  List.map
	    (function Ast0.FType(ty) -> Ast0.FType(typeC ty) | x -> x)
	    fninfo in
	let name = ident name in
	let lp = normal_mcode lp in
	let params = parameter_list (Some(promote_mcode lp)) params in
	let rp = normal_mcode rp in
	let lbrace = normal_mcode lbrace in
	let body =
	  dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
	let rbrace = normal_mcode rbrace in
	let (leftinfo,fninfo,name) = leftfninfo fninfo name bef in
	let (rightinfo,right,rbrace) =
	  promote_to_statement_end_mcode rbrace aft in
      (* pretend it is one line before the start of the function, so that it
	 will catch things defined at top level.  We assume that these will not
	 be defined on the same line as the function.  This is a HACK.
	 A better approach would be to attach top_level things to this node,
	 and other things to the node after, but that would complicate
	 insert_plus, which doesn't distinguish between different mcodekinds *)
	let res =
	  Ast0.FunDecl((leftinfo,bef),fninfo,name,lp,params,va,rp,lbrace,
		       body,rbrace,(rightinfo,aft)) in
      (* have to do this test again, because of typing problems - can't save
	 the result, only use it *)
	(match fninfo with
	  [] -> mkres s res name right
	| Ast0.FStorage(stg)::_ -> mkres s res (promote_mcode stg) right
	| Ast0.FType(ty)::_ -> mkres s res ty right
	| Ast0.FInline(inline)::_ -> mkres s res (promote_mcode inline) right
	| Ast0.FAttr(attr)::_ -> mkres s res attr right)

    | Ast0.Include(inc,stm) ->
	let inc = normal_mcode inc in
	let stm = normal_mcode stm in
	mkres s (Ast0.Include(inc,stm)) (promote_mcode inc) (promote_mcode stm)
    | Ast0.MetaInclude(inc,s) ->
	let inc = normal_mcode inc in
	let s = expression s in
	mkres s (Ast0.MetaInclude(inc,s)) (promote_mcode inc) s

    | Ast0.Undef(def,id) ->
	let def = normal_mcode def in
	let id = ident id in
	mkres s (Ast0.Undef(def,id)) (promote_mcode def) id
    | Ast0.Define(def,id,params,body) ->
	let def = normal_mcode def in
	let (id,right) = full_ident id in
	(match right with
	  None -> failwith "no disj id for #define"
	| Some right ->
	    let (params,prev) = define_parameters params right in
	    let body = dots is_stm_dots (Some prev) statement body in
	    mkres s (Ast0.Define(def,id,params,body)) (promote_mcode def) body)
    | Ast0.Pragma(prg,id,body) ->
	let prg = normal_mcode prg in
	let id = ident id in
	let body = pragmainfo body in
	mkres s (Ast0.Pragma(prg,id,body)) (promote_mcode prg) body
    | Ast0.OptStm(stm) ->
	let stm = statement stm in mkres s (Ast0.OptStm(stm)) stm stm
    | Ast0.AsStmt(stm,asstm) ->
	let stm = statement stm in mkres s (Ast0.AsStmt(stm,asstm)) stm stm in
  Ast0.set_dots_bef_aft res
    (match Ast0.get_dots_bef_aft res with
      Ast0.NoDots -> Ast0.NoDots
    | Ast0.AddingBetweenDots s ->
	Ast0.AddingBetweenDots(statement s)
    | Ast0.DroppingBetweenDots s ->
	Ast0.DroppingBetweenDots(statement s))

and leftfninfo fninfo name bef = (* cases on what is leftmost *)
  match fninfo with
    [] ->
      let (leftinfo,name) = promote_to_statement_start name bef in
      (leftinfo,[],name)
  | Ast0.FStorage(stg)::rest ->
      let (leftinfo,stginfo) =
	promote_to_statement_start (promote_mcode stg) bef in
      (leftinfo,
       Ast0.FStorage(set_mcode_info stg (Ast0.get_info stginfo))::rest,
       name)
  | Ast0.FType(ty)::rest ->
      let (leftinfo,ty) = promote_to_statement_start ty bef in
      (leftinfo,Ast0.FType(ty)::rest,name)
  | Ast0.FInline(inline)::rest ->
      let (leftinfo,inlinfo) =
	promote_to_statement_start (promote_mcode inline) bef in
      (leftinfo,
       Ast0.FInline(set_mcode_info inline (Ast0.get_info inlinfo))::rest,
       name)
  | Ast0.FAttr(attr)::rest ->
      let attr = attribute attr in
      let (leftinfo,attr) =
	promote_to_statement_start attr bef in
      (leftinfo,Ast0.FAttr(attr)::rest,name)

and pragmainfo pi =
  match Ast0.unwrap pi with
    Ast0.PragmaString(s) ->
      let s = bad_mcode s in
      let ln = promote_mcode s in
      mkres pi (Ast0.PragmaString(s)) ln ln
  | Ast0.PragmaDots(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres pi (Ast0.PragmaDots(dots)) ln ln

and case_line c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) ->
      let def = normal_mcode def in
      let colon = normal_mcode colon in
      let code = dots is_stm_dots (Some(promote_mcode colon)) statement code in
      mkres c (Ast0.Default(def,colon,code)) (promote_mcode def) code
  | Ast0.Case(case,exp,colon,code) ->
      let case = normal_mcode case in
      let exp = expression exp in
      let colon = normal_mcode colon in
      let code = dots is_stm_dots (Some(promote_mcode colon)) statement code in
      mkres c (Ast0.Case(case,exp,colon,code)) (promote_mcode case) code
  | Ast0.DisjCase(starter,case_lines,mids,ender) ->
      do_disj c starter case_lines mids ender case_line
	(fun starter case_lines mids ender ->
	  Ast0.DisjCase(starter,case_lines,mids,ender))
  | Ast0.OptCase(case) ->
      let case = case_line case in mkres c (Ast0.OptCase(case)) case case

and exec_code e =
  match Ast0.unwrap e with
    Ast0.ExecEval(colon,id) ->
      let colon = normal_mcode colon in
      let id = expression id in
      mkres e (Ast0.ExecEval(colon,id)) (promote_mcode colon) id
  | Ast0.ExecToken(tok) ->
      let tok = normal_mcode tok in
      let ln = promote_mcode tok in
      mkres e (Ast0.ExecToken tok) ln ln
  | Ast0.ExecDots(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.ExecDots dots) ln ln

and statement_dots x = dots is_stm_dots None statement x

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level t =
  match Ast0.unwrap t with
    Ast0.FILEINFO(old_file,new_file) -> t
  | Ast0.NONDECL(stmt) ->
      let stmt = statement stmt in mkres t (Ast0.NONDECL(stmt)) stmt stmt
  | Ast0.CODE(rule_elem_dots) ->
      let rule_elem_dots = dots is_stm_dots None statement rule_elem_dots in
      mkres t (Ast0.CODE(rule_elem_dots)) rule_elem_dots rule_elem_dots
  | Ast0.ERRORWORDS(exps) -> t
  | Ast0.OTHER(_) | Ast0.TOPCODE(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let compute_lines attachable_or x =
  in_nest_count := 0;
  inherit_attachable := attachable_or;
  List.map top_level x

let compute_statement_lines attachable_or x =
  in_nest_count := 0;
  inherit_attachable := attachable_or;
  statement x

let compute_statement_dots_lines attachable_or x =
  in_nest_count := 0;
  inherit_attachable := attachable_or;
  statement_dots x
