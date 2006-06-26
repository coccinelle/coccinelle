(* Start at all of the corresponding BindContext nodes in the minus and
plus trees, and traverse their children.  We take the same strategy as
before: collect the list of minus/context nodes/tokens and the list of plus
tokens, and then merge them. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module CN = Context_neg

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

  let dotsExpr x = CN.DotsExpr x in
  let dotsParam x = CN.DotsParam x in
  let dotsStmt x = CN.DotsStmt x in
  let ident x = CN.Ident x in
  let expr x = CN.Expr x in
  let typeC x = CN.TypeC x in
  let param x = CN.Param x in
  let decl x = CN.Decl x in
  let stmt x = CN.Stmt x in

  let topfn r k e = CN.Top(e) :: (k e) in

  let res =
    V0.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      (donothing dotsExpr) (donothing dotsParam) (donothing dotsStmt)
      (donothing ident) (donothing expr) (donothing typeC)
      (donothing param) (donothing decl) (donothing stmt) topfn in
  res.V0.combiner_top_level e

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* collect the possible join points, in order, among the children of a
BindContext.  Dots are not allowed.  Nests and disjunctions are no problem,
because their delimiters take up a line by themselves *)

(* An Unfavored token is one that is in a BindContext node; using this causes
  the node to become Neither, meaning that isomorphisms can't be applied *)
type minus_join_point = Favored | Unfavored

(* Maps the index of a node to the indices of the mcodes it contains *)
let root_token_table = (Hashtbl.create(50) : (int, int list) Hashtbl.t)

let create_root_token_table minus =
  Hashtbl.iter
    (function tokens ->
      function (node,_) ->
	let key =
	  match node with
	    CN.DotsExpr(d) -> Ast0.get_index d
	  | CN.DotsParam(d) -> Ast0.get_index d
	  | CN.DotsStmt(d) -> Ast0.get_index d
	  | CN.Ident(d) -> Ast0.get_index d
	  | CN.Expr(d) -> Ast0.get_index d
	  | CN.TypeC(d) -> Ast0.get_index d
	  | CN.Param(d) -> Ast0.get_index d
	  | CN.Decl(d) -> Ast0.get_index d
	  | CN.Stmt(d) -> Ast0.get_index d
	  | CN.Top(d) -> Ast0.get_index d in
	Hashtbl.add root_token_table key tokens)
    CN.minus_table;
  List.iter
    (function (t,info,index,mcodekind) ->
      try let _ = Hashtbl.find root_token_table !index in ()
      with Not_found -> Hashtbl.add root_token_table !index [])
    minus

let collect_minus_join_points root =
  let root_index = Ast0.get_index root in
  let unfavored_tokens = Hashtbl.find root_token_table root_index in
  let bind x y = x @ y in
  let option_default = [] in

  let mcode (_,_,info,mcodekind) =
    if List.mem (info.Ast0.offset) unfavored_tokens
    then [(Unfavored,info,mcodekind)]
    else [(Favored,info,mcodekind)] in

  let do_nothing r k ((_,info,index,mcodekind) as e) =
    match !mcodekind with
      (Ast0.MINUS(_)) as mc -> [(Favored,info,mc)]
    | (Ast0.CONTEXT(_)) as mc when not(!index = root_index) ->
	[(Unfavored,info,mc)]
    | _ -> k e in

  let do_top r k (e: Ast0.top_level) = k e in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    do_nothing do_nothing do_nothing
    do_nothing do_nothing do_nothing do_nothing do_nothing
    do_nothing do_top


let call_collect_minus context_nodes :
    (int * (minus_join_point * Ast0.info * Ast0.mcodekind) list) list =
  List.map
    (function
	CN.DotsExpr(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_expression_dots e)
      | CN.DotsParam(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_parameter_list e)
      | CN.DotsStmt(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_statement_dots e)
      | CN.Ident(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_ident e)
      | CN.Expr(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_expression e)
      | CN.TypeC(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_typeC e)
      | CN.Param(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_parameter e)
      | CN.Decl(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_declaration e)
      | CN.Stmt(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_statement e)
      | CN.Top(e) ->
	  (Ast0.get_index e,
	   (collect_minus_join_points e).V0.combiner_top_level e))
    context_nodes

(* result of collecting the join points should be sorted in nondecreasing
   order by line *)
let verify l =
  let token_start_line = function
      (Favored,info,_) | (Unfavored,info,_) -> info.Ast0.logical_start in
  let token_end_line = function
      (Favored,info,_) | (Unfavored,info,_) -> info.Ast0.logical_end in
  List.iter
    (function
	(index,((_::_) as l1)) ->
	  let _ =
	    List.fold_left
	      (function prev ->
		function cur ->
		  let ln = token_start_line cur in
		  if ln < prev
		  then failwith "error in collection of - tokens\n";
		  token_end_line cur)
	      (token_end_line (List.hd l1))
	      (List.tl l1) in
	  ()
      |	_ -> ()) (* dots, in eg f() has no join points *)
    l

let process_minus minus =
  create_root_token_table minus;
  List.concat
    (List.map
       (function x ->
	 let res = call_collect_minus (collect_context x) in
	 verify res;
	 res)
       minus)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* collect the plus tokens *)

let mk_baseType x         = Ast.BaseTypeTag x
let mk_structUnion x      = Ast.StructUnionTag x
let mk_sign x             = Ast.SignTag x
let mk_ident x            = Ast.IdentTag (Ast0toast.ident x)
let mk_expression x       = Ast.ExpressionTag (Ast0toast.expression x)
let mk_constant x         = Ast.ConstantTag x
let mk_unaryOp x          = Ast.UnaryOpTag x
let mk_assignOp x         = Ast.AssignOpTag x
let mk_fixOp x            = Ast.FixOpTag x
let mk_binaryOp x         = Ast.BinaryOpTag x
let mk_arithOp x          = Ast.ArithOpTag x
let mk_logicalOp x        = Ast.LogicalOpTag x
let mk_declaration x      = Ast.DeclarationTag (Ast0toast.declaration x)
let mk_storage x          = Ast.StorageTag x
let mk_statement x        = Ast.StatementTag (Ast0toast.statement x)
let mk_const_vol x        = Ast.ConstVolTag x
let mk_token x            = Ast.Token x
let mk_code x             = Ast.Code (Ast0toast.top_level x)

let mk_exprdots x  = Ast.ExprDotsTag (Ast0toast.expression_dots x)
let mk_paramdots x = Ast.ParamDotsTag (Ast0toast.parameter_list x)
let mk_stmtdots x  = Ast.StmtDotsTag (Ast0toast.statement_dots x)
let mk_typeC x     = Ast.FullTypeTag (Ast0toast.typeC x)
let mk_param x     = Ast.ParamTag (Ast0toast.parameterTypeDef x)

let collect_plus_nodes root =
  let root_index = Ast0.get_index root in

  let bind x y = x @ y in
  let option_default = [] in

  let mcode fn (term,_,info,mcodekind) =
    match mcodekind with Ast0.PLUS -> [(info,fn term)] | _ -> [] in

  let do_nothing fn r k ((term,info,index,mcodekind) as e) =
    match !mcodekind with
      (Ast0.CONTEXT(_)) when not(!index = root_index) -> []
    | Ast0.PLUS -> [(info,fn e)]
    | _ -> k e in

  V0.combiner bind option_default
    (mcode mk_token) (mcode mk_constant) (mcode mk_assignOp) (mcode mk_fixOp)
    (mcode mk_unaryOp) (mcode mk_binaryOp) (mcode mk_const_vol)
    (mcode mk_baseType) (mcode mk_sign) (mcode mk_structUnion)
    (mcode mk_storage)
    (do_nothing mk_exprdots) (do_nothing mk_paramdots) (do_nothing mk_stmtdots)
    (do_nothing mk_ident) (do_nothing mk_expression)
    (do_nothing mk_typeC) (do_nothing mk_param) (do_nothing mk_declaration)
    (do_nothing mk_statement) (do_nothing mk_code)

let call_collect_plus context_nodes :
    (int * (Ast0.info * Ast.anything) list) list =
  List.map
    (function
	CN.DotsExpr(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_expression_dots e)
      | CN.DotsParam(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_parameter_list e)
      | CN.DotsStmt(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_statement_dots e)
      | CN.Ident(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_ident e)
      | CN.Expr(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_expression e)
      | CN.TypeC(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_typeC e)
      | CN.Param(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_parameter e)
      | CN.Decl(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_declaration e)
      | CN.Stmt(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_statement e)
      | CN.Top(e) ->
	  (Ast0.get_index e,
	   (collect_plus_nodes e).V0.combiner_top_level e))
    context_nodes

(* The plus fragments are converted to a list of lists of lists.
Innermost list: Elements have type anything.  For any pair of successive
elements, n and n+1, the ending line of n is the same as the starting line
of n+1.
Middle lists: For any pair of successive elements, n and n+1, the ending
line of n is one less than the starting line of n+1.
Outer list: For any pair of successive elements, n and n+1, the ending
line of n is more than one less than the starting line of n+1. *)

let logstart info = info.Ast0.logical_start
let logend info = info.Ast0.logical_end

let rec find_neighbors (index,l) :
    int * (Ast0.info * (Ast.anything list list)) list =
  let rec loop = function
      [] -> []
    | (i,x)::rest ->
	(match loop rest with
	  ((i1,(x1::rest_inner))::rest_middle)::rest_outer ->
	    let finish1 = logend i in
	    let start2 = logstart i1 in
	    if finish1 = start2
	    then ((i,(x::x1::rest_inner))::rest_middle)::rest_outer
	    else if finish1 + 1 = start2
	    then ((i,[x])::(i1,(x1::rest_inner))::rest_middle)::rest_outer
	    else [(i,[x])]::((i1,(x1::rest_inner))::rest_middle)::rest_outer
	| _ -> [[(i,[x])]]) (* rest must be [] *) in
  let res =
    List.map
      (function
	  ((info,_)::_) as l -> (info,List.map (function (_,x) -> x) l)
	| _ -> failwith "not possible")
      (loop l) in
  (index,res)

let process_plus plus :
    (int * (Ast0.info * Ast.anything list list) list) list =
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
         m1 is Good and favored: attach to the beginning of m1.aft; drop m1
         m2 is Good and favored: attach to the beginning of m2.bef
         m1 is Good and unfavored: attach to the beginning of m1.aft; drop m1
         m2 is Good and unfavored: attach to the beginning of m2.bef
         also flip m1.bef if the first where > m1
         if we drop m1, then flip m1.aft first
      if p > m2
         m2 is Good and favored: attach to the beginning of m2.aft; drop m1
*)

(* end of first argument < start/end of second argument *)
let less_than_start info1 info2 =
  info1.Ast0.logical_end < info2.Ast0.logical_start
let less_than_end info1 info2 =
  info1.Ast0.logical_end < info2.Ast0.logical_end
let good_start info = info.Ast0.attachable_start
let good_end info = info.Ast0.attachable_end

let favored = function Favored -> true | Unfavored -> false

let pr = Printf.sprintf

let attachbefore (infop,p) m =
  let p = List.rev p in
  let pln = infop.Ast0.logical_start in
  match m with
    Ast0.MINUS(replacements) ->
      (match !replacements with
	([],ti) ->
	  replacements := (p,{Ast0.tline_start=pln;Ast0.tline_end=pln})
      |	(repl,ti) -> replacements := (p@repl,{ti with Ast0.tline_end=pln}))
  | Ast0.CONTEXT(neighbors) ->
      let (repl,ti1,ti2) = !neighbors in
      let new_ti1 = {ti1 with Ast0.tline_end=pln} in
      (match repl with
	Ast.BEFORE(bef) -> neighbors := (Ast.BEFORE(p@bef),new_ti1,ti2)
      |	Ast.AFTER(_) | Ast.BEFOREAFTER(_,_) ->
	  failwith "attachbefore: should not occur"
      |	Ast.NOTHING ->
	  neighbors :=
	    (Ast.BEFORE(p),{new_ti1 with Ast0.tline_start=pln},ti2))
  | _ -> failwith "not possible for attachbefore"

let attachafter (infop,p) m =
  let p = List.rev p in
  let pln =  infop.Ast0.logical_start in
  match m with
    Ast0.MINUS(replacements) ->
      (match !replacements with
	([],ti) ->
	  replacements := (p,{Ast0.tline_start=pln;Ast0.tline_end=pln})
      |	(repl,ti) -> replacements := (p@repl,{ti with Ast0.tline_end=pln}))
  | Ast0.CONTEXT(neighbors) ->
      let (repl,ti1,ti2) = !neighbors in
      let new_ti2 = {ti2 with Ast0.tline_end=pln} in
      (match repl with
	Ast.BEFORE(bef) ->
	  neighbors :=
	    (Ast.BEFOREAFTER(bef,p),ti1,{new_ti2 with Ast0.tline_start=pln})
      |	Ast.AFTER(aft) -> neighbors := (Ast.AFTER(p@aft),ti1,new_ti2)
      | Ast.BEFOREAFTER(bef,aft) ->
	  neighbors := (Ast.BEFOREAFTER(bef,p@aft),ti1,new_ti2)
      |	Ast.NOTHING ->
	  neighbors :=
	    (Ast.AFTER(p),ti1,{new_ti2 with Ast0.tline_start=pln}))
  | _ -> failwith "not possible for attachafter"

let flip_before = function (* only flip if a CONTEXT node *)
    Ast0.MINUS(replacements) -> () (* do nothing *)
  | Ast0.CONTEXT(neighbors) ->
      let (repl,ti1,ti2) = !neighbors in
      (match repl with
	Ast.BEFORE(bef) -> neighbors := (Ast.BEFORE(List.rev bef),ti1,ti2)
      |	Ast.AFTER(_) | Ast.BEFOREAFTER(_,_) ->
	  failwith "flip_before: should not occur"
      |	Ast.NOTHING -> ())
  | _ -> failwith "not possible for flip_before"
    
let flip_after = function (* flip for both CONTEXT and MINUS *)
    Ast0.MINUS(replacements) ->
      let (repl,ti) = !replacements in
      replacements := (List.rev repl,ti)
  | Ast0.CONTEXT(neighbors) ->
      let (repl,ti1,ti2) = !neighbors in
      (match repl with
	Ast.BEFORE(bef) -> ()
      |	Ast.AFTER(aft) -> neighbors := (Ast.AFTER(List.rev aft),ti1,ti2)
      |	Ast.BEFOREAFTER(bef,aft) ->
	  neighbors := (Ast.BEFOREAFTER(bef,List.rev aft),ti1,ti2)
      |	Ast.NOTHING -> ())
  | _ -> failwith "not possible for flip_after"

let attach_all_before ps m =
  List.iter (function x -> attachbefore x m) ps; flip_before m

let attach_all_after ps m =
  List.iter (function x -> attachafter x m) ps; flip_after m

let split_at_end info ps =
  let split_point =  info.Ast0.logical_end in
  List.partition
    (function (info,_) -> info.Ast0.logical_end < split_point)
    ps

let rec before_m1 ((f1,infom1,m1) as x1) ((f2,infom2,m2) as x2) rest = function
    [] -> let _ = flip_before m1 in ()
  | (((infop,_) as p) :: ps) as all ->
      if less_than_end infop infom1 (* account for trees *)
      then
	if good_start infom1
	then (attachbefore p m1; before_m1 x1 x2 rest ps)
	else
	  failwith
	    (pr "%d: no available token to attach to" infop.Ast0.line_start)
      else (flip_before m1; after_m1 x1 x2 rest all)

and after_m1 ((f1,infom1,m1) as x1) ((f2,infom2,m2) as x2) rest = function
    [] -> let _ = flip_after m1 in ()
  | (((infop,_) as p) :: ps) as all ->
      if less_than_start infop infom2
      then
	if good_end infom1 && favored f1
	then (attachafter p m1; after_m1 x1 x2 rest ps)
	else if good_start infom2 && favored f2
	then (flip_after m1; before_m2 x2 rest all)
	else if good_end infom1
	then (attachafter p m1; after_m1 x1 x2 rest ps)
	else if good_start infom2
	then (flip_after m1; before_m2 x2 rest all)
	else
	  failwith
	    (pr "%d: no available token to attach to" infop.Ast0.line_start)
      else (flip_after m1; after_m2 x2 rest all)

and before_m2 ((f2,infom2,m2) as x2) rest
    (p : (Ast0.info * Ast.anything list list) list) =
  match (rest,p) with
    (_,[]) -> ()
  | ([],((infop,_)::_)) ->
      let (bef_m2,aft_m2) = split_at_end infom2 p in (* bef_m2 isn't empty *)
      if good_start infom2
      then (attach_all_before bef_m2 m2; after_m2 x2 rest aft_m2)
      else
	failwith
	  (pr "%d: no available token to attach to" infop.Ast0.line_start)
  | (m::ms,_) -> before_m1 x2 m ms p

and after_m2 ((f2,infom2,m2) as x2) rest
    (p : (Ast0.info * Ast.anything list list) list) =
  match (rest,p) with
    (_,[]) -> ()
  | ([],((infop,_)::_)) ->
      if good_end infom2
      then attach_all_after p m2
      else
	failwith
	  (pr "%d: no available token to attach to" infop.Ast0.line_start)
  | (m::ms,_) -> after_m1 x2 m ms p

let merge_one : (minus_join_point * Ast0.info * 'a) list *
    (Ast0.info * Ast.anything list list) list -> unit =
  function
    (_,[]) -> ()
  | (m1::m2::restm,p) -> before_m1 m1 m2 restm p
  | ([m],p) -> before_m2 m [] p
  | ([],_) -> failwith "minus tree ran out before the plus tree"

let merge minus_list plus_list =
  List.iter
    (function (index,minus_info) ->
      let plus_info = List.assoc index plus_list in
      merge_one (minus_info,plus_info))
    minus_list

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

let insert_plus minus plus =
  let minus_stream = process_minus minus in
  let plus_stream = process_plus plus in
  merge minus_stream plus_stream
