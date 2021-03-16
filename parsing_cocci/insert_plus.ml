(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* The error message "no available token to attach to" often comes in an
argument list of unbounded length.  In this case, one should move a comma so
that there is a comma after the + code. *)

(* Start at all of the corresponding BindContext nodes in the minus and
plus trees, and traverse their children.  We take the same strategy as
before: collect the list of minus/context nodes/tokens and the list of plus
tokens, and then merge them. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module CN = Context_neg

let empty_isos = ref false

let get_option f = function
    None -> []
  | Some x -> f x

(* --------------------------------------------------------------------- *)
(* Collect root and all context nodes in a tree *)

let collect_context e =
  let bind x y = x @ y in
  let option_default = [] in

  let mcode _ = [] in

  let donothing builder r k e =
    match Ast0.get_mcodekind e with
      Ast0.CONTEXT(_) -> (builder e) :: (k e)
    | _ -> k e in

(* special case for everything that contains whencode, so that we skip over
it *)
  let expression r k e =
    donothing Ast0.expr r k
      (Ast0.rewrap e
	 (match Ast0.unwrap e with
	   Ast0.NestExpr(starter,exp,ender,whencode,multi) ->
	     Ast0.NestExpr(starter,exp,ender,None,multi)
	 | Ast0.Edots(dots,whencode) -> Ast0.Edots(dots,None)
	 | e -> e)) in

  let initialiser r k i =
    donothing Ast0.ini r k
      (Ast0.rewrap i
	 (match Ast0.unwrap i with
	   Ast0.Idots(dots,whencode) -> Ast0.Idots(dots,None)
	 | i -> i)) in

  let statement r k s =
    donothing Ast0.stmt r k
      (Ast0.rewrap s
	 (match Ast0.unwrap s with
	   Ast0.Nest(started,stm_dots,ender,whencode,multi) ->
	     Ast0.Nest(started,stm_dots,ender,[],multi)
	 | Ast0.Dots(dots,whencode) -> Ast0.Dots(dots,[])
	 | s -> s)) in

  let topfn r k e = Ast0.TopTag(e) :: (k e) in

  let res =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      (donothing Ast0.dotsExpr) (donothing Ast0.dotsInit)
      (donothing Ast0.dotsParam) (donothing Ast0.dotsStmt)
      (donothing Ast0.dotsDecl) (donothing Ast0.dotsField)
      (donothing Ast0.dotsEnumDecl) (donothing Ast0.dotsCase)
      (donothing Ast0.dotsDefParam)
      (donothing Ast0.ident) expression  (donothing Ast0.assignOp)
      (donothing Ast0.binaryOp)
      (donothing Ast0.typeC) initialiser
      (donothing Ast0.param) (donothing Ast0.decl)
      (donothing Ast0.field) (donothing Ast0.enum_decl) statement
      (donothing Ast0.forinfo) (donothing Ast0.case_line)
      (donothing Ast0.string_fragment) (donothing Ast0.attr) (donothing Ast0.attr_arg)
      topfn in
  res.VT0.combiner_rec_top_level e

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* collect the possible join points, in order, among the children of a
BindContext.  Dots are not allowed.  Nests and disjunctions are no problem,
because their delimiters take up a line by themselves *)

(* An Unfavored token is one that is in a BindContext node; using this causes
  the node to become Neither, meaning that isomorphisms can't be applied *)
(* Toplevel is for the bef token of a function declaration and is for
attaching top-level definitions that should come before the complete
declaration *)
type minus_join_point = Favored | Unfavored | Toplevel | Decl

(* Maps the index of a node to the indices of the mcodes it contains *)
let root_token_table = (Hashtbl.create(51) : (int, int list) Hashtbl.t)

let create_root_token_table minus =
  Hashtbl.iter
    (function tokens ->
      function (node,_) ->
	let rec key n =
	  match n with
	    Ast0.DotsExprTag(d) -> Ast0.get_index d
	  | Ast0.DotsInitTag(d) -> Ast0.get_index d
	  | Ast0.DotsParamTag(d) -> Ast0.get_index d
	  | Ast0.DotsStmtTag(d) -> Ast0.get_index d
	  | Ast0.DotsDeclTag(d) -> Ast0.get_index d
	  | Ast0.DotsFieldTag(d) -> Ast0.get_index d
	  | Ast0.DotsEnumDeclTag(d) -> Ast0.get_index d
	  | Ast0.DotsCaseTag(d) -> Ast0.get_index d
	  | Ast0.DotsDefParamTag(d) -> Ast0.get_index d
	  | Ast0.IdentTag(d) -> Ast0.get_index d
	  | Ast0.ExprTag(d) -> Ast0.get_index d
	  | Ast0.AssignOpTag d -> Ast0.get_index d
	  | Ast0.BinaryOpTag d -> Ast0.get_index d
	  | Ast0.ArgExprTag(d) | Ast0.TestExprTag(d) ->
	      failwith "not possible - iso only"
	  | Ast0.TypeCTag(d) -> Ast0.get_index d
	  | Ast0.ParamTag(d) -> Ast0.get_index d
	  | Ast0.InitTag(d) -> Ast0.get_index d
	  | Ast0.DeclTag(d) -> Ast0.get_index d
	  | Ast0.FieldTag(d) -> Ast0.get_index d
	  | Ast0.EnumDeclTag(d) -> Ast0.get_index d
	  | Ast0.StmtTag(d) -> Ast0.get_index d
	  | Ast0.ForInfoTag(d) -> Ast0.get_index d
	  | Ast0.CaseLineTag(d) -> Ast0.get_index d
	  | Ast0.StringFragmentTag(d) -> Ast0.get_index d
	  | Ast0.AttributeTag(d) -> Ast0.get_index d
	  | Ast0.AttrArgTag(d) -> Ast0.get_index d
	  | Ast0.TopTag(d) -> Ast0.get_index d
	  | Ast0.IsoWhenTag(_) -> failwith "only within iso phase"
	  | Ast0.IsoWhenTTag(_) -> failwith "only within iso phase"
	  | Ast0.IsoWhenFTag(_) -> failwith "only within iso phase"
	  | Ast0.MetaPosTag(p) -> failwith "not in plus code"
	  | Ast0.HiddenVarTag(p) -> failwith "only within iso phase"
	  | Ast0.WhenTag(_,_,w) -> key w
	in
	Hashtbl.add root_token_table (key node) tokens)
    CN.minus_table;
  List.iter
    (function r ->
      let index = Ast0.get_index r in
      try let _ = Hashtbl.find root_token_table index in ()
      with Not_found -> Hashtbl.add root_token_table index [])
    minus

let collect_minus_join_points root =
  let root_index = Ast0.get_index root in
  let unfavored_tokens = Hashtbl.find root_token_table root_index in
  let bind x y = x @ y in
  let option_default = [] in

  let mcode (x,_,info,mcodekind,_,_) =
    if List.mem (info.Ast0.pos_info.Ast0.offset) unfavored_tokens
    then [(Unfavored,info,mcodekind)]
    else [(Favored,info,mcodekind)] in

  let unfavored_mcode (x,_,info,mcodekind,_,_) =
    [(Unfavored,info,mcodekind)] in

  let do_nothing r k e =
    let info = Ast0.get_info e in
    let index = Ast0.get_index e in
    match Ast0.get_mcodekind e with
      (Ast0.MINUS(_)) as mc -> [(Favored,info,mc)]
    | (Ast0.CONTEXT(_)) as mc when not(index = root_index) ->
	(* was unfavored, not sure why *)
      [(Favored,info,mc)]
    | _ -> k e in

(* don't want to attach to the outside of DOTS, because metavariables can't
bind to that; not good for isomorphisms *)

  let dots f k d =
    let multibind l =
      let rec loop = function
	  [] -> option_default
	| [x] -> x
	| x::xs -> bind x (loop xs) in
      loop l in

    let l = Ast0.unwrap d in
    multibind (List.map f l) in

  let edots r k d =
    let l =
      match Ast0.unwrap d with
      | [] -> []
      | el ->
          (* transform an expression into a tuple (bool, expression)
           * with the boolean indicating if the previous expression is
           * optional (dots or expression list)*)
          let map l =
            (* loop processes the reversed list, leading acc to be in the right
             * order while having a simpler pattern matching than if the list
             * was in its orginal order *)
            let rec loop acc = function
              | [] -> acc
              | e::[] -> (false, e)::acc
              | curr::(prev::_ as l) ->
                  let prev_is_optional =
                    match Ast0.unwrap prev with
                      | Ast0.Edots _
                      | Ast0.MetaExprList _ -> true
                      | _ -> false
                  in
                  let acc = (prev_is_optional, curr)::acc in
                  loop acc l in
            loop [] (List.rev l) in
          map el in
    dots
      (fun (prev_is_optional, e) ->
        match (prev_is_optional, Ast0.unwrap e) with
        | (true, Ast0.EComma(comma)) ->
            (* Only unfavor the comma if the previous expression can be absent
             * from the match *)
            unfavored_mcode comma
        | _ ->
            r.VT0.combiner_rec_expression e)
      k (Ast0.rewrap d l) in
  let idots r k d =
    dots
      (function i ->
	match Ast0.unwrap i with
	  Ast0.IComma(comma) -> unfavored_mcode comma
	| _ -> r.VT0.combiner_rec_initialiser i)
      k d in
  let pdots r k d =
    dots
      (function p ->
	match Ast0.unwrap p with
	  Ast0.PComma(comma) -> unfavored_mcode comma
	| _ -> r.VT0.combiner_rec_parameter p)
  k d in
  let dpdots r k d =
    dots
      (function p ->
	match Ast0.unwrap p with
	  Ast0.DPComma(comma) -> unfavored_mcode comma
	| _ -> r.VT0.combiner_rec_define_param p)
  k d in
  let enumdots r k d =
    dots
      (function p ->
	match Ast0.unwrap p with
	  Ast0.EnumComma(comma) -> unfavored_mcode comma
	| _ -> r.VT0.combiner_rec_enumdecl p)
  k d in

  let sdots r k d = dots r.VT0.combiner_rec_statement k d in
  let ddots r k d = dots r.VT0.combiner_rec_declaration k d in
  let fdots r k d = dots r.VT0.combiner_rec_field k d in
  let cdots r k d = dots r.VT0.combiner_rec_case_line k d in

  (* a case for everything that has a Opt *)

  let statement r k s =
    (*
    let redo_branched res (ifinfo,aftmc) =
      let redo fv info mc rest =
	let new_info = {info with Ast0.attachable_end = false} in
	List.rev ((Favored,ifinfo,aftmc)::(fv,new_info,mc)::rest) in
      match List.rev res with
	[(fv,info,mc)] ->
	  (match mc with
	    Ast0.MINUS(_) | Ast0.CONTEXT(_) ->
		(* even for -, better for isos not to integrate code after an
		   if into the if body.
		   but the problem is that this can extend the region in
		   which a variable is bound, because a variable bound in the
		   aft node would seem to have to be live in the whole if,
		   whereas we might like it to be live in only one branch.
		   ie ideally, if we can keep the minus code in the right
		   order, we would like to drop it as close to the bindings
		   of its free variables.  This could be anywhere in the minus
		   code.  Perhaps we would like to do this after the
		   application of isomorphisms, though.
		*)
	      redo fv info mc []
	  | _ -> res)
      | (fv,info,mc)::rest ->
	  (match mc with
	    Ast0.CONTEXT(_) -> redo fv info mc rest
	  | _ -> res)
      | _ -> failwith "unexpected empty code" in *)
    match Ast0.unwrap s with
 (*     Ast0.IfThen(_,_,_,_,_,aft)
    | Ast0.IfThenElse(_,_,_,_,_,_,_,aft)
    | Ast0.While(_,_,_,_,_,aft)
    | Ast0.For(_,_,_,_,_,_,_,_,aft)
    | Ast0.Iterator(_,_,_,_,_,aft) ->
	redo_branched (do_nothing r k s) aft*)
    | Ast0.FunDecl((info,bef),fninfo,name,lp,params,va,rp,lbrace,body,rbrace,
		   (aftinfo,aft)) ->
	(Toplevel,info,bef)::(k s)@[(Toplevel,aftinfo,aft)]
    | Ast0.Decl((info,bef),decl) ->
	(Decl,info,bef)::(k s)
    | Ast0.Nest(starter,stmt_dots,ender,whencode,multi) ->
	mcode starter @ r.VT0.combiner_rec_statement_dots stmt_dots @
	mcode ender
    | Ast0.Dots(d,whencode) -> mcode d (* ignore whencode *)
    | Ast0.OptStm s ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_statement s
    | _ -> do_nothing r k s in

  let forinfo r k s =
    match Ast0.unwrap s with
      Ast0.ForDecl((info,bef),decl) ->
	(Decl,info,bef)::(k s)
    | _ -> k s in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	mcode starter @
	r.VT0.combiner_rec_expression_dots expr_dots @ mcode ender
    | Ast0.Edots(d,whencode) -> mcode d (* ignore whencode *)
    | Ast0.OptExp e ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_expression e
    | _ -> do_nothing r k e in

  let ident r k e =
    match Ast0.unwrap e with
      Ast0.OptIdent i ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_ident i
    | _ -> do_nothing r k e in

  let typeC r k e =
    match Ast0.unwrap e with
      Ast0.OptType t ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_typeC t
    | _ -> do_nothing r k e in

  let decl r k e =
    match Ast0.unwrap e with
      Ast0.OptDecl d ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_declaration d
    | _ -> do_nothing r k e in

  let field r k e =
    match Ast0.unwrap e with
      Ast0.OptField d ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_field d
    | _ -> do_nothing r k e in

  let initialiser r k e =
    match Ast0.unwrap e with
      Ast0.Idots(d,whencode) -> mcode d (* ignore whencode *)
    | Ast0.OptIni i ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_initialiser i
    | _ -> do_nothing r k e in

  let param r k e =
    match Ast0.unwrap e with
      Ast0.OptParam p ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_parameter p
    | _ -> do_nothing r k e in

  let case_line r k e =
    match Ast0.unwrap e with
      Ast0.OptCase c ->
	(* put the + code on the thing, not on the opt *)
	r.VT0.combiner_rec_case_line c
    | _ -> do_nothing r k e in

  let do_top r k (e: Ast0.top_level) = k e in

  V0.flat_combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    edots idots pdots sdots ddots fdots enumdots cdots dpdots
    ident expression do_nothing do_nothing
    typeC initialiser param decl field do_nothing statement forinfo
    case_line do_nothing do_nothing do_nothing do_top


let call_collect_minus context_nodes :
    (int * (minus_join_point * Ast0.info * Ast0.mcodekind) list) list =
  List.map
    (function e ->
      let rec f' = (function
	Ast0.DotsExprTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_expression_dots e)
      | Ast0.DotsInitTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_initialiser_list e)
      | Ast0.DotsParamTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_parameter_list e)
      | Ast0.DotsStmtTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_statement_dots e)
      | Ast0.DotsDeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_declaration_dots e)
      | Ast0.DotsFieldTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_field_dots e)
      | Ast0.DotsEnumDeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_enum_decl_dots e)
      | Ast0.DotsCaseTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_case_line_dots e)
      | Ast0.DotsDefParamTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_define_param_dots e)
      | Ast0.IdentTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_ident e)
      | Ast0.ExprTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_expression e)
      | Ast0.AssignOpTag op ->
	  (Ast0.get_index op,
	   (collect_minus_join_points op).VT0.combiner_rec_assignOp op)
      | Ast0.BinaryOpTag op ->
	  (Ast0.get_index op,
	   (collect_minus_join_points op).VT0.combiner_rec_binaryOp op)
      | Ast0.ArgExprTag(e) | Ast0.TestExprTag(e) ->
	  failwith "not possible - iso only"
      | Ast0.TypeCTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_typeC e)
      | Ast0.ParamTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_parameter e)
      | Ast0.InitTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_initialiser e)
      | Ast0.DeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_declaration e)
      | Ast0.FieldTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_field e)
      | Ast0.EnumDeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_enumdecl e)
      | Ast0.StmtTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_statement e)
      | Ast0.ForInfoTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_forinfo e)
      | Ast0.StringFragmentTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_string_fragment e)
      | Ast0.AttributeTag(e) ->
          (Ast0.get_index e,
           (collect_minus_join_points e).VT0.combiner_rec_attribute e)
      | Ast0.AttrArgTag(e) ->
          (Ast0.get_index e,
           (collect_minus_join_points e).VT0.combiner_rec_attr_arg e)
      | Ast0.CaseLineTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_case_line e)
      | Ast0.TopTag(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).VT0.combiner_rec_top_level e)
      | Ast0.IsoWhenTag(_) -> failwith "only within iso phase"
      | Ast0.IsoWhenTTag(_) -> failwith "only within iso phase"
      | Ast0.IsoWhenFTag(_) -> failwith "only within iso phase"
      | Ast0.MetaPosTag(p) -> failwith "not in plus code"
      | Ast0.HiddenVarTag(p) -> failwith "only within iso phase"
      | Ast0.WhenTag(_,_,w) -> f' w) in f' e)
    context_nodes

(* result of collecting the join points should be sorted in nondecreasing
   order by line *)
let verify l =
  let get_info = function
      (Favored,info,_) | (Unfavored,info,_) | (Toplevel,info,_)
    | (Decl,info,_) -> info in
  let token_start_line    x = (get_info x).Ast0.pos_info.Ast0.logical_start in
  let token_end_line        x = (get_info x).Ast0.pos_info.Ast0.logical_end in
  let token_real_start_line x = (get_info x).Ast0.pos_info.Ast0.line_start in
  let token_real_end_line   x = (get_info x).Ast0.pos_info.Ast0.line_end in
  List.iter
    (function
	(index,((_::_) as l1)) ->
	  let _ =
	    List.fold_left
	      (function (prev,real_prev) ->
		function cur ->
		  let ln = token_start_line cur in
		  if ln < prev
		  then
		    failwith
		      (Printf.sprintf
			 "error in collection of - tokens: line %d less than line %d"
			 (token_real_start_line cur) real_prev);
		  (token_end_line cur,token_real_end_line cur))
	      (token_end_line (List.hd l1), token_real_end_line (List.hd l1))
	      (List.tl l1) in
	  ()
      |	_ -> ()) (* dots, in eg f() has no join points *)
    l

let process_minus minus =
  create_root_token_table minus;
  let res =
    List.concat
      (List.map
	 (function x ->
	   let res = call_collect_minus (collect_context x) in
	   verify res;
	   res)
	 minus) in
  Stdcompat.Hashtbl.reset root_token_table;
  res

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* collect the plus tokens *)

let mk_structUnion x      = Ast.StructUnionTag x
let mk_sign x             = Ast.SignTag x
let mk_ident x            = Ast.IdentTag (Ast0toast.ident x)
let mk_expression x       = Ast.ExpressionTag (Ast0toast.expression x)
let mk_constant x         = Ast.ConstantTag x
let mk_unaryOp x          = Ast.UnaryOpTag x
let mk_assignOp x         = Ast.AssignOpTag (Ast0toast.assignOp x)
let mk_simpleAssignOp x   = Ast.SimpleAssignOpTag x
let mk_opAssignOp x       = Ast.OpAssignOpTag x
let mk_fixOp x            = Ast.FixOpTag x
let mk_binaryOp x         = Ast.BinaryOpTag (Ast0toast.binaryOp x)
let mk_arithOp x          = Ast.ArithOpTag x
let mk_logicalOp x        = Ast.LogicalOpTag x
let mk_arithOp x          = Ast.ArithOpTag x
let mk_logicalOp x        = Ast.LogicalOpTag x
let mk_declaration x      = Ast.DeclarationTag (Ast0toast.declaration x)
let mk_field x            = Ast.FieldTag (Ast0toast.field x)
let mk_enum_decl x        = Ast.EnumDeclTag (Ast0toast.enum_decl x)
let mk_topdeclaration x   = Ast.DeclarationTag (Ast0toast.declaration x)
let mk_storage x          = Ast.StorageTag x
let mk_inc_file x         = Ast.IncFileTag x
let mk_statement x        = Ast.StatementTag (Ast0toast.statement x)
let mk_forinfo x          = Ast.ForInfoTag (Ast0toast.forinfo x)
let mk_case_line x        = Ast.CaseLineTag (Ast0toast.case_line x)
let mk_string_fragment x  = Ast.StringFragmentTag (Ast0toast.string_fragment x)
let mk_attribute x        = Ast.AttributeTag (Ast0toast.attribute x)
let mk_attr_arg x         = Ast.AttrArgTag (Ast0toast.attr_arg x)
let mk_const_vol x        = Ast.ConstVolTag x
let mk_token x info       = Ast.Token (x,Some info)
let mk_meta (_,x) info    = Ast.Token (x,Some info)
let mk_code x             = Ast.Code (Ast0toast.top_level x)

let mk_exprdots x  = Ast.ExprDotsTag (Ast0toast.expression_dots x)
let mk_paramdots x = Ast.ParamDotsTag (Ast0toast.parameter_list x)
let mk_stmtdots x  = Ast.StmtDotsTag (Ast0toast.statement_dots x)
let mk_decldots x  = Ast.AnnDeclDotsTag (Ast0toast.declaration_dots x)
let mk_fielddots x  = Ast.AnnFieldDotsTag (Ast0toast.field_dots x)
let mk_enumdecldots x  = Ast.EnumDeclDotsTag (Ast0toast.enum_decl_dots x)
let mk_casedots x  = failwith "+ case lines not supported"
let mk_defpardots x= Ast.DefParDotsTag (Ast0toast.define_param_dots x)
let mk_typeC x     = Ast.FullTypeTag (Ast0toast.typeC false x)
let mk_init x      = Ast.InitTag (Ast0toast.initialiser x)
let mk_param x     = Ast.ParamTag (Ast0toast.parameterTypeDef x)

let collect_plus_nodes root =
  let root_index = Ast0.get_index root in

  let bind x y = x @ y in
  let option_default = [] in

  let extract_strings info =
    let adjust_info =
      {info with Ast0.strings_before = [];  Ast0.strings_after = []} in
    let extract = function
	[] -> []
      |	strings_before ->
	  let (_,first) = List.hd strings_before in
	  let (_,last) = List.hd (List.rev strings_before) in
	  let new_pos_info =
	    {Ast0.line_start = first.Ast0.line_start;
	      Ast0.line_end = last.Ast0.line_start;
	      Ast0.logical_start = first.Ast0.logical_start;
	      Ast0.logical_end = last.Ast0.logical_start;
	      Ast0.column = first.Ast0.column;
	      Ast0.offset = first.Ast0.offset} in
	  let new_info = {adjust_info with Ast0.pos_info = new_pos_info} in
	  let string = List.map (function (s,_) -> s) strings_before in
	  [(new_info,Ast.ONE(*?*),Ast.Directive (string))] in
    let bef = extract info.Ast0.strings_before in
    let aft = extract info.Ast0.strings_after in
    (bef,aft) in

  let mcode fn (term,_,info,mcodekind,_,_) =
    match mcodekind with
      Ast0.PLUS c -> [(info,c,fn term)]
    | Ast0.CONTEXT _ -> let (bef,aft) = extract_strings info in bef@aft
    | _ -> [] in

  let imcode fn (term,_,info,mcodekind,_,_) =
    match mcodekind with
      Ast0.PLUS c -> [(info,c,fn term (Ast0toast.convert_info info))]
    | Ast0.CONTEXT _ -> let (bef,aft) = extract_strings info in bef@aft
    | _ -> [] in

  let info (i,_,_) = let (bef,aft) = extract_strings i in bef@aft in
  let pre_info (i,_) = let (bef,aft) = extract_strings i in bef@aft in

  let do_nothing_extra bef aft fn r k e =
    match Ast0.get_mcodekind e with
      (Ast0.CONTEXT(_)) when not(Ast0.get_index e = root_index) -> []
    | Ast0.PLUS c -> [(Ast0.get_info e,c,fn e)]
    | _ -> bef@(k e)@aft in
  let do_nothing fn r k e = do_nothing_extra [] [] fn r k e in

  (* case for everything that is just a wrapper for a simpler thing *)
  (* case for things with bef aft *)
  let stmt r k e =
    match Ast0.unwrap e with
      Ast0.Exp(exp) -> r.VT0.combiner_rec_expression exp
    | Ast0.TopExp(exp) -> r.VT0.combiner_rec_expression exp
    | Ast0.Ty(ty) -> r.VT0.combiner_rec_typeC ty
    | Ast0.TopId(id) -> r.VT0.combiner_rec_ident id
    | Ast0.TopInit(init) -> r.VT0.combiner_rec_initialiser init
    | Ast0.Decl(bef,decl) ->
	 do_nothing_extra (pre_info bef) [] mk_statement r k e
    | Ast0.FunDecl(bef,fi,name,lp,params,va,rp,lbrace,body,rbrace,aft) ->
	do_nothing_extra (pre_info bef) (pre_info aft) mk_statement r k e
    | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) ->
	do_nothing_extra [] (info aft) mk_statement r k e
    | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	do_nothing_extra [] (info aft) mk_statement r k e
    | Ast0.While(whl,lp,exp,rp,body,aft) ->
	do_nothing_extra [] (info aft) mk_statement r k e
    | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft) ->
	do_nothing_extra [] (info aft)mk_statement r k e
    | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	do_nothing_extra [] (info aft) mk_statement r k e
    | _ -> do_nothing mk_statement r k e in

  (* statementTag is preferred, because it indicates that one statement is
  replaced by one statement, in single_statement *)
  let stmt_dots r k e =
    match Ast0.unwrap e with
      [s] -> r.VT0.combiner_rec_statement s
    | _ -> do_nothing mk_stmtdots r k e in

  let toplevel r k e =
    match Ast0.unwrap e with
      Ast0.NONDECL(s) -> r.VT0.combiner_rec_statement s
    | Ast0.CODE(sdots) -> r.VT0.combiner_rec_statement_dots sdots
    | _ -> do_nothing mk_code r k e in

  let initdots r k e = k e in

  V0.flat_combiner bind option_default
    (imcode mk_meta) (imcode mk_token) (mcode mk_constant)
    (mcode mk_simpleAssignOp) (mcode mk_opAssignOp)
    (mcode mk_fixOp)
    (mcode mk_unaryOp) (mcode mk_arithOp)
    (mcode mk_logicalOp) (mcode mk_const_vol)
    (mcode mk_sign) (mcode mk_structUnion)
    (mcode mk_storage) (mcode mk_inc_file)
    (do_nothing mk_exprdots) initdots
    (do_nothing mk_paramdots) stmt_dots (do_nothing mk_decldots)
    (do_nothing mk_fielddots) (do_nothing mk_enumdecldots)
    (do_nothing mk_casedots) (do_nothing mk_defpardots)
    (do_nothing mk_ident) (do_nothing mk_expression)
    (do_nothing mk_assignOp) (do_nothing mk_binaryOp)
    (do_nothing mk_typeC) (do_nothing mk_init) (do_nothing mk_param)
    (do_nothing mk_declaration) (do_nothing mk_field)
    (do_nothing mk_enum_decl)
    stmt (do_nothing mk_forinfo) (do_nothing mk_case_line)
    (do_nothing mk_string_fragment) (do_nothing mk_attribute)
    (do_nothing mk_attr_arg)
    toplevel

let call_collect_plus context_nodes :
    (int * (Ast0.info * Ast.count * Ast.anything) list) list =
  List.map
    (function e ->
      let rec f' = (function
	Ast0.DotsExprTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_expression_dots e)
      | Ast0.DotsInitTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_initialiser_list e)
      | Ast0.DotsParamTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_parameter_list e)
      | Ast0.DotsStmtTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_statement_dots e)
      | Ast0.DotsDeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_declaration_dots e)
      | Ast0.DotsFieldTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_field_dots e)
      | Ast0.DotsEnumDeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_enum_decl_dots e)
      | Ast0.DotsCaseTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_case_line_dots e)
      | Ast0.DotsDefParamTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_define_param_dots e)
      | Ast0.IdentTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_ident e)
      | Ast0.ExprTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_expression e)
      | Ast0.AssignOpTag(op) ->
	  (Ast0.get_index op,
	   (collect_plus_nodes op).VT0.combiner_rec_assignOp op)
      | Ast0.BinaryOpTag(op) ->
	  (Ast0.get_index op,
	   (collect_plus_nodes op).VT0.combiner_rec_binaryOp op)
      | Ast0.ArgExprTag(_) | Ast0.TestExprTag(_) ->
	  failwith "not possible - iso only"
      | Ast0.TypeCTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_typeC e)
      | Ast0.InitTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_initialiser e)
      | Ast0.ParamTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_parameter e)
      | Ast0.DeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_declaration e)
      | Ast0.FieldTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_field e)
      | Ast0.EnumDeclTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_enumdecl e)
      | Ast0.StmtTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_statement e)
      | Ast0.ForInfoTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_forinfo e)
      | Ast0.CaseLineTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_case_line e)
      | Ast0.StringFragmentTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_string_fragment e)
      | Ast0.AttributeTag(e) ->
          (Ast0.get_index e,
           (collect_plus_nodes e).VT0.combiner_rec_attribute e)
      | Ast0.AttrArgTag(e) ->
          (Ast0.get_index e,
           (collect_plus_nodes e).VT0.combiner_rec_attr_arg e)
      | Ast0.TopTag(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).VT0.combiner_rec_top_level e)
      | Ast0.IsoWhenTag(_) -> failwith "only within iso phase"
      | Ast0.IsoWhenTTag(_) -> failwith "only within iso phase"
      | Ast0.IsoWhenFTag(_) -> failwith "only within iso phase"
      | Ast0.MetaPosTag(p) -> failwith "not visible here"
      | Ast0.HiddenVarTag(_) -> failwith "only within iso phase"
      | Ast0.WhenTag(_,_,w) -> f' w) in f' e)
    context_nodes

(* The plus fragments are converted to a list of lists of lists.
Innermost list: Elements have type anything.  For any pair of successive
elements, n and n+1, the ending line of n is the same as the starting line
of n+1.
Middle lists: For any pair of successive elements, n and n+1, the ending
line of n is one less than the starting line of n+1.
Outer list: For any pair of successive elements, n and n+1, the ending
line of n is more than one less than the starting line of n+1. *)

let logstart info = info.Ast0.pos_info.Ast0.logical_start
let logend info = info.Ast0.pos_info.Ast0.logical_end

let redo info start finish =
  let new_pos_info =
    {info.Ast0.pos_info with
      Ast0.logical_start = start;
      Ast0.logical_end = finish} in
  {info with Ast0.pos_info = new_pos_info}

let find_neighbors (index,l) :
    int * (Ast0.info * Ast.count * (Ast.anything list list)) list =
  let rec loop = function
      [] -> []
    | (i,c,x)::rest ->
	(match loop rest with
	  ((i1,c1,(x1::rest_inner))::rest_middle)::rest_outer ->
	    let finish1 = logend i in
	    let start2 = logstart i1 in
	    if finish1 = start2
	    then
	      ((if not (c = c1) then failwith "inconsistent + code");
	      ((redo i (logstart i) (logend i1),c,(x::x1::rest_inner))
	       ::rest_middle)
	      ::rest_outer)
	    else if finish1 + 1 = start2
	    then ((i,c,[x])::(i1,c1,(x1::rest_inner))::rest_middle)::rest_outer
	    else
	      [(i,c,[x])]::((i1,c1,(x1::rest_inner))::rest_middle)::rest_outer
	| _ -> [[(i,c,[x])]]) (* rest must be [] *) in
  let res =
    List.map
      (function l ->
	let (start_info,start_count,_) = List.hd l in
	let (end_info,end_count,_) = List.hd (List.rev l) in
	(if not (start_count = end_count) then failwith "inconsistent + code");
	(redo start_info (logstart start_info) (logend end_info),
	 start_count,
	 List.map (function (_,_,x) -> x) l))
      (loop l) in
  (index,res)

let process_plus plus :
    (int * (Ast0.info * Ast.count * Ast.anything list list) list) list =
  List.concat
    (List.map
       (function x ->
	 List.map find_neighbors (call_collect_plus (collect_context x)))
       plus)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* merge *)
(*
let merge_one = function
    (m1::m2::minus_info,p::plus_info) ->
      if p < m1, then
         attach p to the beginning of m1.bef if m1 is Good, fail if it is bad
      if p > m1 && p < m2, then consider the following possibilities, in order
         m1 is Good and favored: attach to the beginning of m1.aft
         m2 is Good and favored: attach to the beginning of m2.bef; drop m1
         m1 is Good and unfavored: attach to the beginning of m1.aft
         m2 is Good and unfavored: attach to the beginning of m2.bef; drop m1
         also flip m1.bef if the first where > m1
         if we drop m1, then flip m1.aft first
      if p > m2
         m2 is Good and favored: attach to the beginning of m2.aft; drop m1
*)

(* end of first argument < start/end of second argument *)
let less_than_start info1 info2 =
  info1.Ast0.pos_info.Ast0.logical_end < info2.Ast0.pos_info.Ast0.logical_start
let less_than_end info1 info2 =
  info1.Ast0.pos_info.Ast0.logical_end < info2.Ast0.pos_info.Ast0.logical_end
let greater_than_end info1 info2 =
  info1.Ast0.pos_info.Ast0.logical_start > info2.Ast0.pos_info.Ast0.logical_end
let good_start info = info.Ast0.attachable_start
let good_end info = info.Ast0.attachable_end

let toplevel = function Toplevel -> true | Favored | Unfavored | Decl -> false
let decl = function Decl -> true | Favored | Unfavored | Toplevel -> false
let favored = function Favored -> true | Unfavored | Toplevel | Decl -> false

let top_code =
  List.for_all
    (List.for_all (function Ast.Code _ | Ast.Directive _ -> true | _ -> false))

let storage_code =
  List.for_all
    (List.for_all (function Ast.StorageTag _ -> true | _ -> false))

(* The following is probably not correct.  The idea is to detect what
should be placed completely before the declaration.  So type/storage
related things do not fall into this category, and complete statements do
fall into this category.  But perhaps other things should be in this
category as well, such as { or ;? *)
let predecl_code =
  let tester = function
      (* the following should definitely be true *)
      Ast.DeclarationTag _
    | Ast.StatementTag _
    | Ast.Rule_elemTag _
    | Ast.StmtDotsTag _
    | Ast.Code _
    | Ast.Directive _ -> true
      (* the following should definitely be false *)
    | Ast.FullTypeTag _ | Ast.BaseTypeTag _ | Ast.StructUnionTag _
    | Ast.SignTag _
    | Ast.StorageTag _ | Ast.ConstVolTag _ | Ast.TypeCTag _ -> false
      (* not sure about the rest *)
    | _ -> false in
  List.for_all (List.for_all tester)

let pr = Printf.sprintf

let insert thing thinginfo into intoinfo =
  let get_last l = let l = List.rev l in (List.rev(List.tl l),List.hd l) in
  let get_first l = (List.hd l,List.tl l) in
  let thing_start = thinginfo.Ast0.pos_info.Ast0.logical_start in
  let thing_end = thinginfo.Ast0.pos_info.Ast0.logical_end in
  let thing_offset = thinginfo.Ast0.pos_info.Ast0.offset in
  let into_start = intoinfo.Ast0.tline_start in
  let into_end = intoinfo.Ast0.tline_end in
  let into_left_offset = intoinfo.Ast0.left_offset in
  let into_right_offset = intoinfo.Ast0.right_offset in
  if thing_end < into_start && thing_start < into_start
  then (thing@into,
	{{intoinfo with Ast0.tline_start = thing_start}
	with Ast0.left_offset = thing_offset})
  else if thing_end = into_start && thing_offset < into_left_offset
  then
    let (prev,last) = get_last thing in
    let (first,rest) = get_first into in
    (prev@[last@first]@rest,
     {{intoinfo with Ast0.tline_start = thing_start}
     with Ast0.left_offset = thing_offset})
  else if thing_start > into_end && thing_end > into_end
  then (into@thing,
	{{intoinfo with Ast0.tline_end = thing_end}
	with Ast0.right_offset = thing_offset})
  else if thing_start = into_end && thing_offset > into_right_offset
  then
    let (first,rest) = get_first thing in
    let (prev,last) = get_last into in
    (prev@[last@first]@rest,
     {{intoinfo with Ast0.tline_end = thing_end}
     with Ast0.right_offset = thing_offset})
  else
    begin
      Printf.printf "thing start %d thing end %d into start %d into end %d\n"
	thing_start thing_end into_start into_end;
      Printf.printf "thing offset %d left offset %d right offset %d\n"
	thing_offset into_left_offset into_right_offset;
      Pretty_print_cocci.print_anything "" thing;
      Pretty_print_cocci.print_anything "" into;
      failwith "can't figure out where to put the + code"
    end

let init thing info =
  (thing,
   {Ast0.tline_start = info.Ast0.pos_info.Ast0.logical_start;
     Ast0.tline_end = info.Ast0.pos_info.Ast0.logical_end;
     Ast0.left_offset = info.Ast0.pos_info.Ast0.offset;
     Ast0.right_offset = info.Ast0.pos_info.Ast0.offset})

let it2c = function Ast.ONE -> "one" | Ast.MANY -> "many"

let attachbefore (infop,c,p) = function
    Ast0.MINUS(replacements) ->
      let (repl,ti) = !replacements in
      (match repl with
 	Ast.NOREPLACEMENT ->
	  let (bef,ti) = init p infop in
	  replacements := (Ast.REPLACEMENT(bef,c),ti)
      | Ast.REPLACEMENT(repl,it) ->
	  let it = Ast.lub_count it c in
	  let (bef,ti) = insert p infop repl ti in
	  replacements := (Ast.REPLACEMENT(bef,it),ti))
  | Ast0.CONTEXT(neighbors) ->
      let (repl,ti1,ti2) = !neighbors in
      (match repl with
	Ast.BEFORE(bef,it) ->
	  let (bef,ti1) = insert p infop bef ti1 in
	  let it = Ast.lub_count it c in
	  neighbors := (Ast.BEFORE(bef,it),ti1,ti2)
      |	Ast.AFTER(aft,it) ->
	  let (bef,ti1) = init p infop in
	  let it = Ast.lub_count it c in
	  neighbors := (Ast.BEFOREAFTER(bef,aft,it),ti1,ti2)
      |	Ast.BEFOREAFTER(bef,aft,it) ->
	  let (bef,ti1) = insert p infop bef ti1 in
	  let it = Ast.lub_count it c in
	  neighbors := (Ast.BEFOREAFTER(bef,aft,it),ti1,ti2)
      |	Ast.NOTHING ->
	  let (bef,ti1) = init p infop in
	  neighbors := (Ast.BEFORE(bef,c),ti1,ti2))
  | _ -> failwith "not possible for attachbefore"

let attachafter (infop,c,p) = function
    Ast0.MINUS(replacements) ->
       let (repl,ti) = !replacements in
       (match repl with
 	Ast.NOREPLACEMENT ->
	  let (aft,ti) = init p infop in
	  replacements := (Ast.REPLACEMENT(aft,c),ti)
      | Ast.REPLACEMENT(repl,it) ->
	  let it = Ast.lub_count it c in
	  let (aft,ti) = insert p infop repl ti in
	  replacements := (Ast.REPLACEMENT(aft,it),ti))
  | Ast0.CONTEXT(neighbors) ->
      let (repl,ti1,ti2) = !neighbors in
      (match repl with
	Ast.BEFORE(bef,it) ->
	  let (aft,ti2) = init p infop in
	  let it = Ast.lub_count it c in
	  neighbors := (Ast.BEFOREAFTER(bef,aft,it),ti1,ti2)
      |	Ast.AFTER(aft,it) ->
	  let (aft,ti2) = insert p infop aft ti2 in
	  let it = Ast.lub_count it c in
	  neighbors := (Ast.AFTER(aft,it),ti1,ti2)
      |	Ast.BEFOREAFTER(bef,aft,it) ->
	  let (aft,ti2) = insert p infop aft ti2 in
	  let it = Ast.lub_count it c in
	  neighbors := (Ast.BEFOREAFTER(bef,aft,it),ti1,ti2)
      |	Ast.NOTHING ->
	  let (aft,ti2) = init p infop in
	  neighbors := (Ast.AFTER(aft,c),ti1,ti2))
  | _ -> failwith "not possible for attachbefore"

let attach_all_before ps m =
  List.iter (function x -> attachbefore x m) ps

let attach_all_after ps m =
  List.iter (function x -> attachafter x m) ps

let split_at_end info ps =
  let split_point =  info.Ast0.pos_info.Ast0.logical_end in
  List.partition
    (function (info,_,_) -> info.Ast0.pos_info.Ast0.logical_end < split_point)
    ps

let allminus = function
    Ast0.MINUS(_) -> true
  | _ -> false

let rec before_m1 ((f1,infom1,m1) as x1) ((f2,infom2,m2) as x2) rest = function
    [] -> ()
  | (((infop,_,pcode) as p) :: ps) as all ->
      if less_than_start infop infom1 ||
	(allminus m1 && less_than_end infop infom1) (* account for trees *)
      then
	if toplevel f1
	then
	  if storage_code pcode
	  then before_m2 x2 rest all (* skip fake token for storage *)
	  else (attachbefore p m1; before_m1 x1 x2 rest ps)
	else
	  if good_start infom1
	  then (attachbefore p m1; before_m1 x1 x2 rest ps)
	  else
	    failwith
	      (pr "%d: no available token to attach to"
		 infop.Ast0.pos_info.Ast0.line_start)
      else after_m1 x1 x2 rest all

and after_m1 ((f1,infom1,m1) as x1) ((f2,infom2,m2) as x2) rest = function
    [] -> ()
  | (((infop,count,pcode) as p) :: ps) as all ->
      (* if the following is false, then some + code is stuck in the middle
	 of some context code (m1).  could drop down to the token level.
	 this might require adjustments in ast0toast as well, when + code on
	 expressions is dropped down to + code on expressions.  it might
	 also break some invariants on which iso depends, particularly on
	 what it can infer from something being CONTEXT with no top-level
	 modifications.  for the moment, we thus give an error, asking the
	 user to rewrite the semantic patch. *)
      if greater_than_end infop infom1 || is_minus m1 || !empty_isos
      then
	if less_than_start infop infom2
	then
	  if predecl_code pcode && good_end infom1 && decl f1
	  then (attachafter p m1; after_m1 x1 x2 rest ps)
	  else if predecl_code pcode && good_start infom2 && decl f2
	  then before_m2 x2 rest all
	  else if top_code pcode && good_end infom1 && toplevel f1
	  then (attachafter p m1; after_m1 x1 x2 rest ps)
	  else if top_code pcode && good_start infom2 && toplevel f2
	  then before_m2 x2 rest all
	  else if good_end infom1 && favored f1
	  then (attachafter p m1; after_m1 x1 x2 rest ps)
	  else if good_start infom2 && favored f2
	  then before_m2 x2 rest all
	  else if good_end infom1
	  then (attachafter p m1; after_m1 x1 x2 rest ps)
	  else if good_start infom2
	  then before_m2 x2 rest all
	  else
	    failwith
	      (pr "%d: no available token to attach to"
		 infop.Ast0.pos_info.Ast0.line_start)
	else after_m2 x2 rest all
      else
	begin
	  Printf.printf "between: p start %d p end %d m1 start %d m1 end %d m2 start %d m2 end %d\n"
	    infop.Ast0.pos_info.Ast0.line_start
	    infop.Ast0.pos_info.Ast0.line_end
	    infom1.Ast0.pos_info.Ast0.line_start
	    infom1.Ast0.pos_info.Ast0.line_end
	    infom2.Ast0.pos_info.Ast0.line_start
	    infom2.Ast0.pos_info.Ast0.line_end;
	  Pretty_print_cocci.print_anything "" pcode;
	  failwith
	    "The semantic patch is structured in a way that may give bad results with isomorphisms.  Please try to rewrite it by moving + code out from -/context terms."
	end

(* not sure this is safe.  if have iso problems, consider changing this
to always return false *)
and is_minus = function
    Ast0.MINUS _ -> true
  | _ -> false

and before_m2 ((f2,infom2,m2) as x2) rest
    (p : (Ast0.info * Ast.count * Ast.anything list list) list) =
  match (rest,p) with
    (_,[]) -> ()
  | ([],((infop,_,_)::_)) ->
      let (bef_m2,aft_m2) = split_at_end infom2 p in (* bef_m2 isn't empty *)
      if good_start infom2
      then (attach_all_before bef_m2 m2; after_m2 x2 rest aft_m2)
      else
	failwith
	  (pr "%d: no available token to attach to"
	     infop.Ast0.pos_info.Ast0.line_start)
  | (m::ms,_) -> before_m1 x2 m ms p

and after_m2 ((f2,infom2,m2) as x2) rest
    (p : (Ast0.info * Ast.count * Ast.anything list list) list) =
  match (rest,p) with
    (_,[]) -> ()
  | ([],((infop,_,_)::_)) ->
      if good_end infom2
      then attach_all_after p m2
      else
	failwith
	  (pr "%d: no available token to attach to"
	     infop.Ast0.pos_info.Ast0.line_start)
  | (m::ms,_) -> after_m1 x2 m ms p

let merge_one : (minus_join_point * Ast0.info * 'a) list *
    (Ast0.info * Ast.count * Ast.anything list list) list -> unit =
      function (m,p) ->
  (*
  Printf.printf "minus code\n";
  List.iter
    (function (_,info,_) ->
      Printf.printf
	"start %d end %d real_start %d real_end %d attachable start %b attachable end %b\n"
	info.Ast0.pos_info.Ast0.logical_start
	info.Ast0.pos_info.Ast0.logical_end
	info.Ast0.pos_info.Ast0.line_start
	info.Ast0.pos_info.Ast0.line_end
	info.Ast0.attachable_start
	info.Ast0.attachable_end)
    m;
  Printf.printf "plus code\n";
  List.iter
    (function (info,_,p) ->
      Printf.printf "start %d end %d real_start %d real_end %d\n"
	info.Ast0.pos_info.Ast0.logical_start
	info.Ast0.pos_info.Ast0.logical_end
	info.Ast0.pos_info.Ast0.line_end
	info.Ast0.pos_info.Ast0.line_end;
      Pretty_print_cocci.print_anything "" p;
      Format.print_newline())
    p;
  Printf.printf "end of plus code\n";
  *)
  match (m,p) with
    (_,[]) -> ()
  | (m1::m2::restm,p) -> before_m1 m1 m2 restm p
  | ([m],p) -> before_m2 m [] p
  | ([],_) -> failwith "minus tree ran out before the plus tree"

let merge minus_list plus_list =
  (*
  Printf.printf "minus list %s\n"
    (String.concat " "
       (List.map (function (x,_) -> string_of_int x) minus_list));
  Printf.printf "plus list %s\n"
    (String.concat " "
       (List.map (function (x,_) -> string_of_int x) plus_list));
  *)
  List.iter
    (function (index,minus_info) ->
      let plus_info = List.assoc index plus_list in
      merge_one (minus_info,plus_info))
    minus_list

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Need to check that CONTEXT nodes have nothing attached to their tokens.
If they do, they become MIXED *)

let reevaluate_contextness =
   let bind = (@) in
   let option_default = [] in

   let mcode (_,_,_,mc,_,_) =
     match mc with
       Ast0.CONTEXT(mc) -> let (ba,_,_) = !mc in [ba]
     | _ -> [] in

   let pre_info (_,mc) =
     match mc with
       Ast0.CONTEXT(mc) -> let (ba,_,_) = !mc in [ba]
     | _ -> [] in

   let info (_,mc,_) =
     match mc with
       Ast0.CONTEXT(mc) -> let (ba,_,_) = !mc in [ba]
     | _ -> [] in

   let donothing_extra extra r k e =
     match Ast0.get_mcodekind e with
       Ast0.CONTEXT(mc) ->
	 let updates = extra @ (k e) in
	 if List.exists (function Ast.NOTHING -> false | _ -> true) updates
	 then Ast0.set_mcodekind e (Ast0.MIXED(mc));
	 []
     | _ -> let _ = k e in [] in
   let donothing r k e = donothing_extra [] r k e in

   (* a case for everything with bef or aft *)
   let stmt r k e =
     match Ast0.unwrap e with
       Ast0.Decl(bef,decl) ->
	 donothing_extra (pre_info bef) r k e
     | Ast0.FunDecl(bef,fi,name,lp,params,va,rp,lbrace,body,rbrace,aft) ->
	 donothing_extra ((pre_info bef) @ (pre_info aft)) r k e
     | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) ->
	 donothing_extra (info aft) r k e
     | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	 donothing_extra (info aft) r k e
     | Ast0.While(whl,lp,exp,rp,body,aft) ->
	 donothing_extra (info aft) r k e
     | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft) ->
	 donothing_extra (info aft) r k e
     | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	 donothing_extra (info aft) r k e
     | _ -> donothing r k e in

  let res =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing stmt
      donothing
      donothing donothing
      donothing donothing donothing in
  res.VT0.combiner_rec_top_level

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

let insert_plus minus plus ei =
  empty_isos := ei;
  let minus_stream = process_minus minus in
  let plus_stream = process_plus plus in
  merge minus_stream plus_stream;
  List.iter (function x -> let _ =  reevaluate_contextness x in ()) minus
