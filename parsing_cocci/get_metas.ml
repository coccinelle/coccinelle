(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* --------------------------------------------------------------------- *)
(* creates AsExpr, etc *)
(* @ attached metavariables can only be associated with positions, so nothing
to do for them *)

(* Why doesn't this use the Ast0 visitor? *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

let map_split f l = List.split(List.map f l)

let rewrap x (n,e) = (n,Ast0.rewrap x e)

let mcode x =
  let (nonpos,ispos) =
    List.partition (function Ast0.MetaPosTag _ -> false | _ -> true)
      (Ast0.get_pos x) in
  (nonpos,Ast0.set_pos ispos x)

let option_default = []

let bind l1 l2 =
  let oldnames = List.map Ast0.meta_pos_name l2 in
  List.fold_left
    (function prev -> function e1 ->
      if List.mem (Ast0.meta_pos_name e1) oldnames then prev else e1::prev)
    l2 l1

let multibind l =
  let rec loop = function
      [] -> option_default
    | [x] -> x
    | x::xs -> bind x (loop xs) in
  loop l

let map_split_bind f l =
  let (n,e) = List.split(List.map f l) in (multibind n,e)

let dots fn d = rewrap d (map_split_bind fn (Ast0.unwrap d))

let ident r k i =
  let (metas,i) = k i in
  List.fold_left
    (function (other_metas,id) ->
      function
	  Ast0.IdentTag(id_meta) ->
	    (other_metas,Ast0.rewrap id (Ast0.AsIdent(id,id_meta)))
	| x -> (x::other_metas,id))
    ([],i) metas

and expression r k e =
  let (metas,e) = k e in
  List.fold_left
    (function (other_metas,exp) ->
      function
	  Ast0.ExprTag(exp_meta) ->
	    (other_metas,Ast0.rewrap exp (Ast0.AsExpr(exp,exp_meta)))
	| Ast0.IdentTag(id_meta) ->
	    (other_metas,
	     Ast0.rewrap exp
	       (Ast0.AsExpr(exp,Ast0.rewrap exp (Ast0.Ident(id_meta)))))
	| Ast0.StmtTag(stm_meta) ->
	    (other_metas, Ast0.rewrap exp (Ast0.AsSExpr(exp,stm_meta)))
	| Ast0.DeclTag(decl_meta) ->
	    let decl_meta =
	      Ast0.rewrap exp
		(Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),decl_meta)) in
	    (other_metas, Ast0.rewrap exp (Ast0.AsSExpr(exp,decl_meta)))
	| x -> (x::other_metas,exp))
    ([],e) metas

and typeC r k t =
  let (metas,t) = k t in
  List.fold_left
    (function (other_metas,ty) ->
      function
	  Ast0.TypeCTag(ty_meta) ->
	    (other_metas,Ast0.rewrap ty (Ast0.AsType(ty,ty_meta)))
	| x -> (x::other_metas,ty))
    ([],t) metas

and declaration r k d =
  let (metas,d) = k d in
  List.fold_left
    (function (other_metas,decl) ->
      function
	  Ast0.DeclTag(decl_meta) ->
	    (other_metas,Ast0.rewrap decl (Ast0.AsDecl(decl,decl_meta)))
	| x -> (x::other_metas,decl))
    ([],d) metas

and initialiser r k i =
  let (metas,i) = k i in
  List.fold_left
    (function (other_metas,init) ->
      function
	  Ast0.InitTag(init_meta) ->
	    (other_metas,Ast0.rewrap init (Ast0.AsInit(init,init_meta)))
	| x -> (x::other_metas,init))
    ([],i) metas

and param r k p =
  match Ast0.unwrap p with
    Ast0.MetaParamList(name,lenname,cstr,pure) ->
      let (metas,p) =
       rewrap p
         (let (n,name) = mcode name in
         (n,Ast0.MetaParamList(name,lenname,cstr,pure))) in
      List.fold_left
       (function (other_metas,id) ->
         function
             ((Ast0.ExprTag(exp_meta)) as x) ->
               (match Ast0.unwrap exp_meta with
                 Ast0.MetaExprList _ ->
                   (other_metas,Ast0.rewrap p (Ast0.AsParam(p,exp_meta)))
               | _ -> (x::other_metas,id))
           | x -> (x::other_metas,id))
       ([],p) metas
  | _ -> k p

and statement r k s =
  let (metas,s) = k s in
  List.fold_left
    (function (other_metas,stmt) ->
      function
	  Ast0.StmtTag(stmt_meta) ->
	    (other_metas,Ast0.rewrap stmt (Ast0.AsStmt(stmt,stmt_meta)))
	| x -> (x::other_metas,stmt))
    ([],s) metas

let res =
  V0.combiner_rebuilder bind option_default
    {V0.crmcode=mcode} {V0.crdonothing=(fun r k e -> k e)}
    ~ident:ident ~expr:expression ~ty:typeC ~init:initialiser
    ~param:param ~decl:declaration ~stmt:statement ()

let do_process fn line_getter t =
  match fn t with
    ([],code) -> code
  | (l,_) ->
      failwith
	(Printf.sprintf "%s contains unattached metavariables: %s"
	   (line_getter t)
	   (String.concat ", "
	      (List.map
		 (function nm ->
		   let (r,n) = Ast0.unwrap_mcode nm in r^"."^n)
		 (List.map Ast0.meta_pos_name l))))

let process =
  let line t = Printf.sprintf "rule starting on line %d" (Ast0.get_line t) in
  List.map (do_process res.VT0.top_level line)
let process_anything x = do_process res.VT0.anything (fun _ -> "term") x
