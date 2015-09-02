module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* Add commas in init lists or exp lists, if needed.  This must be done
before the adjacency calculation so that the commas get the right
adjacency numbers.  This is needed for correct formatting in unparse_c.ml *)

(* commas in dotted lists, here due to polymorphism restrictions *)

let add_comma is_comma is_dots make_comma itemlist =
  match List.rev (Ast0.unwrap itemlist) with
    [] -> itemlist
(* Not sure if comma is needed if the list is just ...; leave it there for
now. See list_matcher in cocci_vs_c.ml in first try_matches case. *)
(*      | [e] when is_dots e -> itemlist*)
  |  e::es ->
      if is_comma e
      then itemlist
      else
        let comma =
          match Ast0.get_mcodekind e with
            Ast0.MINUS(_) -> (Ast0.make_minus_mcode ",")
          |  _ -> (Ast0.make_mcode ",") in
	Ast0.rewrap itemlist
          (List.rev (Ast0.rewrap e (make_comma comma) :: (e::es)))

let add_exp_comma =
  add_comma
    (function x -> match Ast0.unwrap x with Ast0.EComma _ -> true | _ -> false)
    (function x -> match Ast0.unwrap x with Ast0.Edots _  -> true | _ -> false)
    (function x -> Ast0.EComma x)

and add_init_comma =
  add_comma
    (function x -> match Ast0.unwrap x with Ast0.IComma _ -> true | _ -> false)
    (function x -> match Ast0.unwrap x with Ast0.Idots _  -> true | _ -> false)
    (function x -> Ast0.IComma x)

(* --------------------------------------------------------------------- *)
(* special cases for terms that contain comma-separated lists where the
trailing comma is allowed but not required *)

let base_typeC r k t =
  let t = k t in
  match Ast0.unwrap t with
    Ast0.EnumDef(ty,lb,ids,rb) ->
      let ids = add_exp_comma ids in
      Ast0.rewrap t (Ast0.EnumDef(ty,lb,ids,rb))
  | _ -> t

let initialiser r k i =
  let i = k i in
  match Ast0.unwrap i with
    Ast0.InitList(lb,initlist,rb,ordered) ->
      let initlist = add_init_comma initlist in
      Ast0.rewrap i (Ast0.InitList(lb,initlist,rb,ordered))
  | _ -> i

let process p =
  let fn =
    V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_tyfn = base_typeC;
	VT0.rebuilder_initfn = initialiser} in
  List.map fn.VT0.rebuilder_rec_top_level p
