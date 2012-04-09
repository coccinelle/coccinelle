(******************************************************************************
 *                             Sexplib                                        *
 *                                                                            *
 * Copyright (C) 2005- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
 *                                                                            *
 * This file is derived from file "pa_tywith.ml" of version 0.45 of the       *
 * library "Tywith".                                                          *
 *                                                                            *
 * Tywith is Copyright (C) 2004, 2005 by                                      *
 *                                                                            *
 *    Martin Sandin  <msandin@hotmail.com>                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

(* Pa_sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open StdLabels
open MoreLabels

open Printf

open Camlp4
open PreCast

module Gen = Pa_type_conv.Gen

(* Utility functions *)

let mk_rev_bindings loc fps =
  let coll (i, bindings, patts, vars) fp =
    let name = "v" ^ string_of_int i in
    let var_expr = <:expr@loc< $lid:name$ >> in
    let expr =
      match fp with
      | `Fun fun_expr -> <:expr@loc< $fun_expr$ $var_expr$ >>
      | `Match matchings -> <:expr@loc< match $var_expr$ with [ $matchings$ ] >>
    in
    let patt = <:patt@loc< $lid:name$ >> in
    let bindings = <:binding@loc< $patt$ = $expr$ and $bindings$ >> in
    i - 1, bindings, patt :: patts, var_expr :: vars
  in
  let n = List.length fps in
  let _, bindings, patts, expr =
    List.fold_left ~f:coll ~init:(n, Ast.BiNil loc, [], []) fps
  in
  bindings, patts, expr

let mk_bindings loc fps = mk_rev_bindings loc (List.rev fps)

let unroll_cnv_fp loc var = function
  | `Fun fun_expr -> <:expr@loc< $fun_expr$ $var$ >>
  | `Match matchings -> <:expr@loc< match $var$ with [ $matchings$ ] >>

let unroll_fun_matches loc fp1 fp2 =
  match fp1, fp2 with
  | `Fun fun_expr1, `Fun fun_expr2 ->
      <:expr@loc< $fun_expr1$ $fun_expr2$ >>
  | `Fun fun_expr, `Match matching ->
      <:expr@loc< $fun_expr$ (fun [ $matching$ ]) >>
  | _ -> assert false  (* impossible *)

let rec sig_of_tds cnv = function
  | Ast.TyDcl (loc, type_name, tps, rhs, cl) -> cnv loc type_name tps rhs cl
  | Ast.TyAnd (loc, tp1, tp2) ->
      <:sig_item@loc< $sig_of_tds cnv tp1$; $sig_of_tds cnv tp2$ >>
  | _ -> assert false  (* impossible *)


(* Generators for S-expressions *)

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct

  let rec sig_of_td__loop acc = function
    | [] ->
        let loc = Ast.loc_of_ctyp acc in
        <:ctyp@loc< $acc$ -> Sexplib.Sexp.t >>
    | tp :: tps ->
        let tp = Gen.drop_variance_annotations tp in
        let loc = Ast.loc_of_ctyp tp in
        let sexp_of = sig_of_td__loop <:ctyp@loc< $acc$ $tp$ >> tps in
        <:ctyp@loc< ( $tp$ -> Sexplib.Sexp.t ) -> $sexp_of$ >>

  let sig_of_td loc type_name tps _rhs _cl =
    let sexp_of = sig_of_td__loop <:ctyp@loc< $lid:type_name$ >> tps in
    <:sig_item@loc< value $lid: "sexp_of_" ^ type_name$ : $sexp_of$ >>

  let mk_sig tds = <:sig_item< $sig_of_tds sig_of_td tds$ >>

  let () = Pa_type_conv.add_sig_generator "sexp_of" mk_sig

  let mk_sig_exn = function
    | <:ctyp@loc< $uid:_$ >> | <:ctyp@loc< $uid:_$ of $_$ >> ->
        <:sig_item@loc< >>
    | tp -> Gen.error tp ~fn:"mk_sig_exn" ~msg:"unknown type"

  let () = Pa_type_conv.add_sig_generator ~is_exn:true "sexp" mk_sig_exn
end


(* Generates the signature for type conversion from S-expressions *)
module Sig_generate_of_sexp = struct

  let rec is_polymorphic_variant = function
    | <:ctyp< private $tp$ >> -> is_polymorphic_variant tp
    | <:ctyp< ( $tup:_$ ) >>
    | <:ctyp< $_$ -> $_$ >>
    | <:ctyp< { $_$ } >>
    | <:ctyp< [ $_$ ] >> -> `Surely_not
    | <:ctyp< [< $_$ ] >> | <:ctyp< [> $_$ ] >>
    | <:ctyp< [= $_$ ] >> -> `Definitely
    | <:ctyp< '$_$ >>
    | <:ctyp< $_$ $_$ >>
    | <:ctyp< $id:_$ >>
    | <:ctyp< >> -> `Maybe
    | <:ctyp< $tp1$ == $tp2$ >> ->
        begin match is_polymorphic_variant tp1 with
        | (`Surely_not | `Definitely) as res -> res
        | `Maybe -> is_polymorphic_variant tp2 end
    | tp -> Gen.unknown_type tp "Sig_generate_of_sexp.is_polymorphic_variant"

  let rec sig_of_td__loop acc = function
    | [] ->
        let loc = Ast.loc_of_ctyp acc in
        <:ctyp@loc< Sexplib.Sexp.t -> $acc$ >>
    | tp :: tps ->
        let tp = Gen.drop_variance_annotations tp in
        let loc = Ast.loc_of_ctyp tp in
        let of_sexp = sig_of_td__loop <:ctyp@loc< $acc$ $tp$ >> tps in
        <:ctyp@loc< ( Sexplib.Sexp.t -> $tp$ ) -> $of_sexp$ >>

  let sig_of_td with_poly loc type_name tps rhs _cl =
    let of_sexp = sig_of_td__loop <:ctyp@loc< $lid:type_name$ >> tps in
    let of_sexp_item =
      <:sig_item@loc< value $lid: type_name ^ "_of_sexp"$ : $of_sexp$; >>
    in
    match with_poly, is_polymorphic_variant rhs with
    | true, `Surely_not ->
        Gen.error rhs ~fn:"Sig_generate_of_sexp.sig_of_td"
          ~msg:"sexp_poly annotation \
            but type is surely not a polymorphic variant"
    | false, (`Surely_not | `Maybe) -> of_sexp_item
    | (true | false), `Definitely | true, `Maybe ->
        <:sig_item@loc<
          $of_sexp_item$;
          value $lid: type_name ^ "_of_sexp__"$ : $of_sexp$;
        >>

  let mk_sig with_poly tds =
    <:sig_item< $sig_of_tds (sig_of_td with_poly) tds$ >>

  let () = Pa_type_conv.add_sig_generator "of_sexp" (mk_sig false)
  let () = Pa_type_conv.add_sig_generator "of_sexp_poly" (mk_sig true)
end


(* Generates the signature for type conversion to S-expressions *)
module Sig_generate = struct
  let () =
    Pa_type_conv.add_sig_generator "sexp" (fun tds ->
      let loc = Ast.loc_of_ctyp tds in
      <:sig_item@loc<
        $Sig_generate_sexp_of.mk_sig tds$;
        $Sig_generate_of_sexp.mk_sig false tds$
      >>)

  let () =
    Pa_type_conv.add_sig_generator "sexp_poly" (fun tds ->
      let loc = Ast.loc_of_ctyp tds in
      <:sig_item@loc<
        $Sig_generate_sexp_of.mk_sig tds$;
        $Sig_generate_of_sexp.mk_sig true tds$
      >>)
end


(* Generator for converters of OCaml-values to S-expressions *)
module Generate_sexp_of = struct
  let mk_abst_call loc tn rev_path =
    <:expr@loc<
      $id:Gen.ident_of_rev_path loc (("sexp_of_" ^ tn) :: rev_path)$
    >>

  (* Conversion of type paths *)
  let sexp_of_path_fun loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call loc tn rev_path
    | [] -> assert false  (* impossible *)

  (* Conversion of types *)
  let rec sexp_of_type = function
    | <:ctyp@loc< sexp_opaque $_$ >> ->
        `Fun <:expr@loc< Sexplib.Conv.sexp_of_opaque >>
    | <:ctyp@loc< $tp1$ $tp2$ >> -> `Fun (sexp_of_appl_fun loc tp1 tp2)
    | <:ctyp< ( $tup:tp$ ) >> -> sexp_of_tuple tp
    | <:ctyp@loc< '$parm$ >> -> `Fun <:expr@loc< $lid:"_of_" ^ parm$ >>
    | <:ctyp@loc< $id:id$ >> -> `Fun (sexp_of_path_fun loc id)
    | <:ctyp@loc< $_$ -> $_$ >> as arrow ->
        `Fun <:expr@loc< fun (_f : $arrow$) ->
          Sexplib.Conv.sexp_of_fun Pervasives.ignore >>
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> -> sexp_of_variant row_fields
    | <:ctyp< ! $parms$ . $poly_tp$ >> -> sexp_of_poly parms poly_tp
    | tp -> Gen.unknown_type tp "sexp_of_type"

  (* Conversion of polymorphic types *)
  and sexp_of_appl_fun loc tp1 tp2 =
    match sexp_of_type tp1, sexp_of_type tp2 with
    | `Fun fun_expr1, `Fun fun_expr2 -> <:expr@loc< $fun_expr1$ $fun_expr2$ >>
    | `Fun fun_expr, `Match matching ->
        <:expr@loc< $fun_expr$ (fun [ $matching$ ]) >>
    | _ -> assert false  (* impossible *)


  (* Conversion of tuples *)
  and sexp_of_tuple tp =
    let loc = Ast.loc_of_ctyp tp in
    let fps = List.map ~f:sexp_of_type (Ast.list_of_ctyp tp []) in
    let bindings, patts, vars = mk_bindings loc fps in
    let in_expr = <:expr@loc< Sexplib.Sexp.List $Gen.mk_expr_lst loc vars$ >> in
    let expr = <:expr@loc< let $bindings$ in $in_expr$ >> in
    `Match <:match_case@loc< ( $tup:Ast.paCom_of_list patts$ ) -> $expr$ >>


  (* Conversion of variant types *)

  and mk_cnv_expr tp =
    let loc = Ast.loc_of_ctyp tp in
    match sexp_of_type tp with
    | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
    | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>

  and sexp_of_variant row_fields =
    let rec loop = function
      | <:ctyp@loc< $tp1$ | $tp2$ >> ->
          <:match_case@loc< $loop tp1$ | $loop tp2$ >>
      | <:ctyp@loc< `$cnstr$ >> ->
          <:match_case@loc< `$cnstr$ -> Sexplib.Sexp.Atom $str:cnstr$ >>
      | <:ctyp@loc< `$cnstr$ of sexp_list $tp$>> ->
        let cnv_expr =
          match sexp_of_type tp with
          | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
          | `Match matchings ->
              <:expr@loc< fun el -> match el with [ $matchings$ ] >>
        in
        <:match_case@loc<
          `$cnstr$ l ->
             Sexplib.Sexp.List
               [ Sexplib.Sexp.Atom $str:cnstr$ ::
                   Sexplib.Conv.list_map $cnv_expr$ l]
        >>
      | <:ctyp@loc< `$cnstr$ of $tps$ >> ->
          let fps = List.map ~f:sexp_of_type (Ast.list_of_ctyp tps []) in
          let bindings, patts, vars = mk_bindings loc fps in
          let cnstr_expr = <:expr@loc< Sexplib.Sexp.Atom $str:cnstr$ >> in
          let expr =
            <:expr@loc<
              let $bindings$ in
              Sexplib.Sexp.List $Gen.mk_expr_lst loc (cnstr_expr :: vars)$
            >>
          in
          <:match_case@loc< `$cnstr$ $Ast.paSem_of_list patts$ -> $expr$ >>
      | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
      | <:ctyp< [= $row_fields$ ] >> -> loop row_fields
      | <:ctyp@loc< $tp1$ $tp2$ >> ->
          let id_path = Gen.get_appl_path loc tp1 in
          let call = sexp_of_appl_fun loc tp1 tp2 in
          <:match_case@loc< #$id_path$ as v -> $call$ v >>
      | <:ctyp@loc< $id:id$ >> | <:ctyp@loc< #$id:id$ >> ->
          let call =
            match Gen.get_rev_id_path id [] with
            | tn :: rev_path -> mk_abst_call loc tn rev_path
            | [] -> assert false  (* impossible *)
          in
          <:match_case@loc< #$id$ as v -> $call$ v >>
      | tp -> Gen.unknown_type tp "sexp_of_variant"
    in
    `Match (loop row_fields)


  (* Polymorphic record fields *)

  and sexp_of_poly parms tp =
    let loc = Ast.loc_of_ctyp tp in
    let bindings =
      let mk_binding parm =
        <:binding@loc< $lid:"_of_" ^ parm$ = Sexplib.Conv.sexp_of_opaque >>
      in
      List.map ~f:mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match sexp_of_type tp with
    | `Fun fun_expr -> `Fun <:expr@loc< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case@loc<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Conversion of sum types *)

  let rec branch_sum = function
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
        <:match_case@loc< $branch_sum tp1$ | $branch_sum tp2$ >>
    | <:ctyp@loc< $uid:cnstr$ >> ->
        <:match_case@loc< $uid:cnstr$ -> Sexplib.Sexp.Atom $str:cnstr$ >>
    | <:ctyp@loc< $uid:cnstr$ of sexp_list $tp$>> ->
        let cnv_expr =
          match sexp_of_type tp with
          | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
          | `Match matchings ->
              <:expr@loc< fun el -> match el with [ $matchings$ ] >>
        in
        <:match_case@loc<
          $uid:cnstr$ l ->
             Sexplib.Sexp.List
               [Sexplib.Sexp.Atom $str:cnstr$ ::
                   Sexplib.Conv.list_map $cnv_expr$ l]
        >>
    | <:ctyp@loc< $uid:cnstr$ of $tps$ >> ->
        let fps = List.map ~f:sexp_of_type (Ast.list_of_ctyp tps []) in
        let cnstr_expr = <:expr@loc< Sexplib.Sexp.Atom $str:cnstr$ >> in
        let bindings, patts, vars = mk_bindings loc fps in
        let patt =
          match patts with
          | [patt] -> patt
          | _ -> <:patt@loc< ( $tup:Ast.paCom_of_list patts$ ) >>
        in
        <:match_case@loc<
          $uid:cnstr$ $patt$ ->
            let $bindings$ in
            Sexplib.Sexp.List $Gen.mk_expr_lst loc (cnstr_expr :: vars)$
        >>
    | tp -> Gen.unknown_type tp "branch_sum"

  let sexp_of_sum alts = `Match (branch_sum alts)


  (* Conversion of record types *)

  let mk_rec_patt loc patt name =
    let p = <:patt@loc< $lid:name$ = $lid:"v_" ^ name$ >> in
    <:patt@loc< $patt$; $p$ >>

  let sexp_of_default_field patt expr name tp sexp_of empty =
    let loc = Ast.loc_of_ctyp tp in
    let patt = mk_rec_patt loc patt name in
    let cnv_expr =
      match sexp_of_type tp with
      | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
      | `Match matchings ->
          <:expr@loc< fun el -> match el with [ $matchings$ ] >>
    in
    let expr =
      let v_name = <:expr@loc< $lid: "v_" ^ name$ >> in
      <:expr@loc<
        let bnds =
          if $v_name$ = $empty$ then bnds
          else
            let arg = $sexp_of$ $cnv_expr$ $v_name$ in
            let bnd =
              Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
            in
            [ bnd :: bnds ]
        in
        $expr$
      >>
    in
    patt, expr

  let sexp_of_record flds_ctyp =
    let flds = Ast.list_of_ctyp flds_ctyp [] in
    let rec coll (patt, expr) = function
      | <:ctyp@loc< $lid:name$ : mutable sexp_option $tp$ >>
      | <:ctyp@loc< $lid:name$ : sexp_option $tp$ >> ->
          let patt = mk_rec_patt loc patt name in
          let vname = <:expr@loc< v >> in
          let cnv_expr = unroll_cnv_fp loc vname (sexp_of_type tp) in
          let expr =
            <:expr@loc<
              let bnds =
                match $lid:"v_" ^ name$ with
                [ None -> bnds
                | Some v ->
                    let arg = $cnv_expr$ in
                    let bnd =
                      Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
                    in
                    [ bnd :: bnds ] ]
              in
              $expr$
            >>
          in
          patt, expr
      | <:ctyp@loc< $lid:name$ : mutable sexp_bool >>
      | <:ctyp@loc< $lid:name$ : sexp_bool >> ->
          let patt = mk_rec_patt loc patt name in
          let expr =
            <:expr@loc<
              let bnds =
                if $lid:"v_" ^ name$ then
                  let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$] in
                  [ bnd :: bnds ]
                else bnds
              in
              $expr$
            >>
          in
          patt, expr
      | <:ctyp@loc< $lid:name$ : mutable sexp_list $tp$ >>
      | <:ctyp@loc< $lid:name$ : sexp_list $tp$ >> ->
          sexp_of_default_field
            patt expr name tp <:expr@loc< sexp_of_list >> <:expr@loc< [] >>
      | <:ctyp@loc< $lid:name$ : mutable sexp_array $tp$ >>
      | <:ctyp@loc< $lid:name$ : sexp_array $tp$ >> ->
          sexp_of_default_field
            patt expr name tp <:expr@loc< sexp_of_array >> <:expr@loc< [||] >>
      | <:ctyp@loc< $lid:name$ : mutable $tp$ >>
      | <:ctyp@loc< $lid:name$ : $tp$ >> ->
          let patt = mk_rec_patt loc patt name in
          let vname = <:expr@loc< $lid:"v_" ^ name$ >> in
          let cnv_expr = unroll_cnv_fp loc vname  (sexp_of_type tp) in
          let expr =
            <:expr@loc<
              let arg = $cnv_expr$ in
              let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg] in
              let bnds = [ bnd :: bnds ] in
              $expr$
            >>
          in
          patt, expr
      | _ -> assert false  (* impossible *)
    in
    let loc = Ast.loc_of_ctyp flds_ctyp in
    let init_expr = <:expr@loc< Sexplib.Sexp.List bnds >> in
    let patt, expr =
      List.fold_left ~f:coll ~init:(<:patt@loc<>>, init_expr) flds
    in
    `Match
      <:match_case@loc<
        { $patt$ } ->
          let bnds = [] in
          $expr$
      >>


  (* Empty type *)
  let sexp_of_nil loc = `Fun <:expr@loc< fun _v -> assert False >>


  (* Generate code from type definitions *)

  let sexp_of_td loc type_name tps rhs =
    let body =
      let rec loop tp =
        Gen.switch_tp_def tp
          ~alias:(fun (_ : Loc.t) tp -> sexp_of_type tp)
          ~sum:(fun (_ : Loc.t) tp -> sexp_of_sum tp)
          ~record:(fun (_ : Loc.t) tp -> sexp_of_record tp)
          ~variants:(fun (_ : Loc.t) tp -> sexp_of_variant tp)
          ~mani:(fun (_ : Loc.t) _tp1 tp2 -> loop tp2)
          ~nil:sexp_of_nil
      in
      match loop rhs with
      | `Fun fun_expr ->
          (* Prevent violation of value restriction and problems with
             recursive types by eta-expanding function definitions *)
          <:expr@loc< fun [ v -> $fun_expr$ v ] >>
      | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>
    in
    let mk_pat id = <:patt@loc< $lid:id$ >> in
    let patts =
      List.map tps
        ~f:(fun ty -> <:patt@loc< $lid:"_of_" ^ Gen.get_tparam_id ty$>>)
    in
    let bnd = mk_pat ("sexp_of_" ^ type_name) in
    <:binding@loc< $bnd$ = $Gen.abstract loc patts body$ >>

  let rec sexp_of_tds = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        sexp_of_td loc type_name tps rhs
    | Ast.TyAnd (loc, tp1, tp2) ->
        <:binding@loc< $sexp_of_tds tp1$ and $sexp_of_tds tp2$ >>
    | _ -> assert false  (* impossible *)

  let sexp_of tds =
    let binding, recursive, loc =
      match tds with
      | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
          sexp_of_td loc type_name tps rhs,
          Gen.type_is_recursive type_name rhs, loc
      | Ast.TyAnd (loc, _, _) as tds -> sexp_of_tds tds, true, loc
      | _ -> assert false  (* impossible *)
    in
    if recursive then <:str_item@loc< value rec $binding$ >>
    else <:str_item@loc< value $binding$ >>

  (* Add code generator to the set of known generators *)
  let () = Pa_type_conv.add_generator "sexp_of" sexp_of

  let string_of_ident id =
    let str_lst = Gen.get_rev_id_path id [] in
    String.concat ~sep:"." str_lst

  let sexp_of_exn tp =
    let get_full_cnstr cnstr = Pa_type_conv.get_conv_path () ^ "." ^ cnstr in
    let expr =
      match tp with
      | <:ctyp@loc< $uid:cnstr$ >> ->
          <:expr@loc<
            Sexplib.Exn_magic.register $uid:cnstr$ $str:get_full_cnstr cnstr$
          >>
      | <:ctyp@loc< $uid:cnstr$ of $tps$ >> ->
          let ctyps = Ast.list_of_ctyp tps [] in
          let fps = List.map ~f:sexp_of_type ctyps in
          let sexp_converters =
            List.map fps ~f:(function
            | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
            | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>)
          in
          let _, patts, vars = mk_bindings loc fps in
          let register_name = sprintf "register%d" (List.length fps) in
          let make_exc =
            let var_args =
              match vars with
              | [var] -> var
              | _ -> <:expr@loc< $tup:Ast.exCom_of_list vars$ >>
            in
            Gen.abstract loc patts <:expr@loc< $uid:cnstr$ $var_args$ >>
          in
          let call =
            let partial =
              <:expr@loc<
                Sexplib.Exn_magic.$lid:register_name$
                  $make_exc$ $str:get_full_cnstr cnstr$
              >>
            in
            Gen.apply loc partial sexp_converters
          in
          <:expr@loc< $call$ >>
      | tp -> Gen.unknown_type tp "sexp_of_exn"
    in
    let loc = Ast.loc_of_ctyp tp in
    <:str_item@loc< value () = $expr$ >>

  let () = Pa_type_conv.add_generator ~is_exn:true "sexp" sexp_of_exn
end


(* Generator for converters of S-expressions to OCaml-values *)
module Generate_of_sexp = struct
  let mk_abst_call loc tn ?(internal = false) rev_path =
    let tns = tn ^ "_of_sexp" in
    let tns_suff = if internal then tns ^ "__" else tns in
    <:expr@loc< $id:Gen.ident_of_rev_path loc (tns_suff :: rev_path)$ >>

  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    <:match_case@loc< Sexplib.Conv_error.No_variant_match _ -> $expr$ >>

  let is_wildcard = function [_] -> true | _ -> false

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc match_last matches =
    if match_last || is_wildcard matches then
      match matches with
      | <:match_case< $_$ -> $expr$ >> :: _ -> expr
      | _ -> assert false  (* impossible *)
    else <:expr@loc< match atom with [ $list:matches$ ] >>

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches loc rev_els call =
    let coll_structs acc (loc, cnstr) =
      <:match_case@loc<
        $str:cnstr$ -> Sexplib.Conv_error.$lid:call$ _tp_loc _sexp
      >> :: acc
    in
    let exc_no_variant_match =
      <:match_case@loc<
        _ -> Sexplib.Conv_error.no_variant_match _tp_loc _sexp
      >>
    in
    List.fold_left ~f:coll_structs ~init:[exc_no_variant_match] rev_els

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let rec split_row_field (atoms, structs, ainhs, sinhs as acc) = function
    | <:ctyp@loc< `$cnstr$ >> ->
        let tpl = loc, cnstr in
        (
          tpl :: atoms,
          structs,
          `A tpl :: ainhs,
          sinhs
        )
    | <:ctyp@loc< `$cnstr$ of $tps$ >> ->
        (
          atoms,
          (loc, cnstr) :: structs,
          ainhs,
          `S (loc, cnstr, tps) :: sinhs
        )
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        List.fold_left ~f:split_row_field
          ~init:acc (Ast.list_of_ctyp row_fields [])
    | <:ctyp< $_$ $_$ >>
    | (<:ctyp< $id:_$ >> | <:ctyp< #$id:_$ >>) as inh ->
        let iinh = `I inh in
        (
          atoms,
          structs,
          iinh :: ainhs,
          iinh :: sinhs
        )
    | tp -> Gen.unknown_type tp "split_row_field"

  (* Conversion of type paths *)
  let path_of_sexp_fun loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call loc tn rev_path
    | [] -> assert false  (* no empty paths *)

  (* Conversion of types *)
  let rec type_of_sexp = function
    | <:ctyp@loc< sexp_opaque $_$ >> ->
        `Fun <:expr@loc< Sexplib.Conv.opaque_of_sexp >>
    | <:ctyp@loc< sexp_option >> ->
        `Fun <:expr@loc< fun a_of_sexp v -> Some (a_of_sexp v) >>
    | <:ctyp@loc< sexp_list >> ->
        `Fun <:expr@loc< fun a_of_sexp v ->
          Sexplib.Conv.list_of_sexp a_of_sexp v >>
    | <:ctyp@loc< sexp_array >> ->
        `Fun <:expr@loc< fun a_of_sexp v ->
          Sexplib.Conv.array_of_sexp a_of_sexp v >>
    | <:ctyp@loc< $tp1$ $tp2$ >> ->
        let fp1 = type_of_sexp tp1 in
        let fp2 = type_of_sexp tp2 in
        `Fun (unroll_fun_matches loc fp1 fp2)
    | <:ctyp< ( $tup:tp$ ) >> -> tuple_of_sexp tp
    | <:ctyp@loc< '$parm$ >> -> `Fun <:expr@loc< $lid:"_of_" ^ parm$ >>
    | <:ctyp@loc< $id:id$ >> -> `Fun (path_of_sexp_fun loc id)
    | <:ctyp@loc< $_$ -> $_$ >> as arrow ->
        `Fun <:expr@loc< fun sexp ->
          (Sexplib.Conv.fun_of_sexp sexp : $arrow$) >>
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        variant_of_sexp ?full_type:None row_fields
    | <:ctyp< ! $parms$ . $poly_tp$ >> -> poly_of_sexp parms poly_tp
    | tp -> Gen.unknown_type tp "type_of_sexp"

  (* Conversion of tuples *)
  and tuple_of_sexp tps =
    let fps = List.map ~f:type_of_sexp (Ast.list_of_ctyp tps []) in
    let loc = Ast.loc_of_ctyp tps in
    let bindings, patts, vars = mk_bindings loc fps in
    let n = string_of_int (List.length fps) in
    `Match
      <:match_case@loc<
          Sexplib.Sexp.List $Gen.mk_patt_lst loc patts$ ->
            let $bindings$ in
            ( $tup:Ast.exCom_of_list vars$ )
        | sexp ->
            Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc $int:n$ sexp
      >>

  (* Generate internal call *)
  and mk_internal_call = function
    | <:ctyp@loc< $id:id$ >> | <:ctyp@loc< #$id:id$ >> ->
        let call =
          match Gen.get_rev_id_path id [] with
          | tn :: rev_path -> mk_abst_call loc tn ~internal:true rev_path
          | [] -> assert false  (* impossible *)
        in
        call
    | <:ctyp@loc< $tp1$ $tp2$ >> ->
        let fp1 = `Fun (mk_internal_call tp1) in
        let fp2 = type_of_sexp tp2 in
        unroll_fun_matches loc fp1 fp2
    | _ -> assert false  (* impossible *)

  (* Generate code for matching included variant types *)
  and handle_variant_inh full_type match_last other_matches inh =
    let loc = Ast.loc_of_ctyp inh in
    let fun_expr = mk_internal_call inh in
    let match_exc =
      handle_no_variant_match loc (
        handle_variant_match_last loc match_last other_matches) in
    let new_other_matches =
      [
        <:match_case@loc<
          _ -> try ($fun_expr$ _sexp :> $full_type$) with [ $match_exc$ ]
        >>
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs =
    let coll (other_matches, match_last) = function
      | `A (loc, cnstr) ->
          let new_match = <:match_case@loc< $str:cnstr$ -> `$cnstr$ >> in
          new_match :: other_matches, false
      | `I inh ->
          handle_variant_inh full_type match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_structs "ptag_no_args"
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs in
    handle_variant_match_last loc match_last match_atoms_inhs


  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match ~is_variant cnstr tps =
    let loc = Ast.loc_of_ctyp tps in
    let cnstr vars_expr =
      if is_variant then <:expr@loc< `$cnstr$ $vars_expr$ >>
      else <:expr@loc< $uid:cnstr$ $vars_expr$ >>
    in
    match tps with
    | <:ctyp@loc< sexp_list $tp$ >> ->
      let cnv =
        match type_of_sexp tp with
        | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
        | `Match matchings ->
            <:expr@loc< fun el -> match el with [ $matchings$ ] >>
      in
      cnstr <:expr@loc< Sexplib.Conv.list_map ($cnv$) sexp_args >>
    | _ ->
        let fps = List.map ~f:type_of_sexp (Ast.list_of_ctyp tps []) in
        let bindings, patts, vars = mk_bindings loc fps in
        let good_arg_match =
          let vars_expr =
            match vars with
            | [var_expr] -> var_expr
            | _ -> <:expr@loc< ( $tup:Ast.exCom_of_list vars$ ) >>
          in
          cnstr vars_expr
        in
        let handle_exc =
          if is_variant then "ptag_incorrect_n_args"
          else "stag_incorrect_n_args"
        in
        <:expr@loc<
          match sexp_args with
            [ $Gen.mk_patt_lst loc patts$ -> let $bindings$ in $good_arg_match$
            | _ -> Sexplib.Conv_error.$lid:handle_exc$ _tp_loc _tag _sexp ]
        >>

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, cnstr, tps) ->
          has_structs_ref := true;
          let expr = mk_cnstr_args_match ~is_variant:true cnstr tps in
          let new_match =
            <:match_case@loc< ($str:cnstr$ as _tag) -> $expr$ >>
          in
          new_match :: other_matches, false
      | `I inh ->
          handle_variant_inh full_type match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_atoms "ptag_no_args"
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    (
      handle_variant_match_last loc match_last match_structs_inhs,
      !has_structs_ref
    )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag loc full_type row_fields =
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:split_row_field ~init:([], [], [], []) row_fields
    in
    let match_struct, has_structs =
      mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms in
    let maybe_sexp_args_patt =
      if has_structs then <:patt@loc< sexp_args >>
      else <:patt@loc< _ >>
    in
    <:match_case@loc<
        Sexplib.Sexp.Atom atom as _sexp ->
          $mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs$
      | Sexplib.Sexp.List
          [Sexplib.Sexp.Atom atom :: $maybe_sexp_args_patt$] as _sexp ->
            $match_struct$
      | Sexplib.Sexp.List [Sexplib.Sexp.List _ :: _] as sexp ->
          Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp
      | Sexplib.Sexp.List [] as sexp ->
          Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp
    >>

  (* Generate matching code for variants *)
  and variant_of_sexp ?full_type row_tp =
    let loc = Ast.loc_of_ctyp row_tp in
    let row_fields = Ast.list_of_ctyp row_tp [] in
    let is_contained, full_type =
      match full_type with
      | None -> true, <:ctyp@loc< [= $row_tp$ ] >>
      | Some full_type -> false, full_type
    in
    let top_match =
      match row_fields with
      | (<:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >>) as inh :: rest ->
          let rec loop inh row_fields =
            let call =
              <:expr@loc< ( $mk_internal_call inh$ sexp :> $full_type$ ) >>
            in
            match row_fields with
            | [] -> call
            | h :: t ->
                let expr =
                  match h with
                  | <:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >> -> loop h t
                  | _ ->
                     let rftag_matches =
                       handle_variant_tag loc full_type row_fields
                     in
                     <:expr@loc< match sexp with [ $rftag_matches$ ] >>
                in
                <:expr@loc<
                  try $call$ with
                  [ $handle_no_variant_match loc expr$ ]
                >>
          in
          <:match_case@loc< sexp -> $loop inh rest$ >>
      | _ :: _ -> handle_variant_tag loc full_type row_fields
      | [] -> assert false  (* impossible *)
    in
    if is_contained then
      `Fun
        <:expr@loc<
          fun sexp ->
            try match sexp with [ $top_match$ ]
            with
            [ Sexplib.Conv_error.No_variant_match (_tp_loc, sexp) ->
                Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
            ]
        >>
    else `Match top_match

  and poly_of_sexp parms tp =
    let loc = Ast.loc_of_ctyp tp in
    let bindings =
      let mk_binding parm =
        <:binding@loc<
          $lid:"_of_" ^ parm$ =
            fun sexp -> Sexplib.Conv_error.record_poly_field_value _tp_loc sexp
        >>
      in
      List.map ~f:mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match type_of_sexp tp with
    | `Fun fun_expr -> `Fun <:expr@loc< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case@loc<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let rec mk_good_sum_matches = function
    | <:ctyp@loc< $uid:cnstr$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case@loc<
          Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$) -> $uid:cnstr$
        >>
    | <:ctyp@loc< $uid:cnstr$ of $tps$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case@loc<
          (Sexplib.Sexp.List
            [Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$ as _tag) ::
              sexp_args] as _sexp) ->
                $mk_cnstr_args_match ~is_variant:false cnstr tps$
        >>
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
        <:match_case@loc<
            $mk_good_sum_matches tp1$
          | $mk_good_sum_matches tp2$
        >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let rec mk_bad_sum_matches = function
    | <:ctyp@loc< $uid:cnstr$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case@loc<
          Sexplib.Sexp.List
            [Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$) :: _] as sexp ->
              Sexplib.Conv_error.stag_no_args _tp_loc sexp
        >>
    | <:ctyp@loc< $uid:cnstr$ of $_$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case@loc<
          Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$) as sexp ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        >>
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
        <:match_case@loc<
            $mk_bad_sum_matches tp1$
          | $mk_bad_sum_matches tp2$
        >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for sum types *)
  let sum_of_sexp alts =
    let loc = Ast.loc_of_ctyp alts in
    `Match
      <:match_case@loc<
          $mk_good_sum_matches alts$
        | $mk_bad_sum_matches alts$
        | Sexplib.Sexp.List [Sexplib.Sexp.List _ :: _] as sexp ->
            Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp
        | Sexplib.Sexp.List [] as sexp ->
            Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp
        | sexp -> Sexplib.Conv_error.unexpected_stag _tp_loc sexp
      >>


  (* Record conversions *)

  (* Generate code for extracting record fields *)
  let mk_extract_fields tp =
    let rec loop no_args args = function
      | <:ctyp< $tp1$; $tp2$ >> ->
          let no_args, args = loop no_args args tp2 in
          loop no_args args tp1
      | <:ctyp@loc< $lid:nm$ : mutable sexp_bool >>
      | <:ctyp@loc< $lid:nm$ : sexp_bool>> ->
          let no_args =
            <:match_case@loc<
                $str:nm$ ->
                  if $lid:nm ^ "_field"$.val then
                    duplicates.val := [ field_name :: duplicates.val ]
                  else $lid:nm ^ "_field"$.val := True
              | $no_args$
            >>
          in
          no_args, args
      | <:ctyp@loc< $lid:nm$ : mutable sexp_option $tp$ >>
      | <:ctyp@loc< $lid:nm$ : sexp_option $tp$ >>
      | <:ctyp@loc< $lid:nm$ : mutable $tp$ >>
      | <:ctyp@loc< $lid:nm$ : $tp$ >> ->
          let unrolled =
            unroll_cnv_fp loc <:expr@loc< _field_sexp >> (type_of_sexp tp)
          in
          let args =
            <:match_case@loc<
                $str:nm$ ->
                  match $lid:nm ^ "_field"$.val with
                  [ None ->
                      let fvalue = $unrolled$ in
                      $lid:nm ^ "_field"$.val := Some fvalue
                  | Some _ ->
                      duplicates.val := [ field_name :: duplicates.val ] ]
              | $args$
            >>
          in
          no_args, args
        | _ -> assert false  (* impossible *)
    in
    let handle_extra =
      let loc = Ast.loc_of_ctyp tp in
      <:match_case@loc<
        _ ->
          if Sexplib.Conv.record_check_extra_fields.val then
            extra.val := [ field_name :: extra.val ]
          else ()
      >>
    in
    loop handle_extra handle_extra tp

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result has_poly flds =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop (res_tpls, bi_lst, good_patts as acc) = function
        | <:ctyp@loc< $lid:nm$ : $tp$ >> ->
            let fld = <:expr@loc< $lid:nm ^ "_field"$.val >> in
            let new_bi_lst, new_good_patts =
              match tp with
              | <:ctyp@loc< sexp_bool >> |  <:ctyp@loc< mutable sexp_bool >>
              | <:ctyp@loc< sexp_option $_$ >>
              | <:ctyp@loc< mutable sexp_option $_$ >>
              | <:ctyp@loc< sexp_list $_$ >>
              | <:ctyp@loc< mutable sexp_list $_$ >>
              | <:ctyp@loc< sexp_array $_$ >>
              | <:ctyp@loc< mutable sexp_array $_$ >> ->
                  bi_lst, <:patt@loc< $lid:nm ^ "_value"$ >> :: good_patts
              | _ ->
                  let loc = Ast.loc_of_ctyp tp in
                  has_nonopt_fields := true;
                  (
                    <:expr@loc<
                      (Pervasives.(=) $fld$ None, $str:nm$) >> :: bi_lst,
                    <:patt@loc< Some $lid:nm ^ "_value"$ >> :: good_patts
                  )
            in
            (
              <:expr@loc< $fld$ >> :: res_tpls,
              new_bi_lst,
              new_good_patts
            )
        | <:ctyp< $tp1$; $tp2$ >> -> loop (loop acc tp2) tp1
        | _ -> assert false  (* impossible *)
      in
      loop ([], [], []) flds
    in
    let loc = Ast.loc_of_ctyp flds in
    let match_good_expr =
      if has_poly then
        let rec loop acc = function
          | <:ctyp< $tp1$; $tp2$ >> -> loop (loop acc tp2) tp1
          | <:ctyp@loc< $lid:nm$ : $_$ >> ->
              <:expr@loc< $lid:nm ^ "_value"$ >> :: acc
          | _ -> assert false  (* impossible *)
        in
        match loop [] flds with
        | [match_good_expr] -> match_good_expr
        | match_good_exprs ->
            <:expr@loc< $tup:Ast.exCom_of_list match_good_exprs$ >>
      else
        let rec loop = function
          | <:ctyp@loc< $tp1$; $tp2$ >> ->
              <:rec_binding@loc< $loop tp1$; $loop tp2$ >>
          | <:ctyp@loc< $lid:nm$ : sexp_list $_$ >> ->
              <:rec_binding@loc<
                $lid:nm$ =
                  match $lid:nm ^ "_value"$ with
                  [ None -> [] | Some v -> v ]
              >>
          | <:ctyp@loc< $lid:nm$ : sexp_array $_$ >> ->
              <:rec_binding@loc<
                $lid:nm$ =
                  match $lid:nm ^ "_value"$ with
                  [ None -> [||] | Some v -> v ]
              >>
          | <:ctyp@loc< $lid:nm$ : $_$ >> ->
              <:rec_binding@loc< $lid:nm$ = $lid:nm ^ "_value"$ >>
          | _ -> assert false  (* impossible *)
        in
        <:expr@loc< { $loop flds$ } >>
    in
    let expr, patt =
      match res_tpls, good_patts with
      | [res_expr], [res_patt] -> res_expr, res_patt
      | _ ->
          <:expr@loc< $tup:Ast.exCom_of_list res_tpls$ >>,
          <:patt@loc< $tup:Ast.paCom_of_list good_patts$ >>
    in
    if !has_nonopt_fields then
      <:expr@loc<
        match $expr$ with
        [ $patt$ -> $match_good_expr$
        | _ ->
            Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
              $Gen.mk_expr_lst loc bi_lst$
        ]
      >>
    else <:expr@loc< match $expr$ with [ $patt$ -> $match_good_expr$ ] >>

  (* Generate code for converting record fields *)
  let mk_cnv_fields has_poly flds =
    let field_refs =
      let rec loop = function
        | <:ctyp@loc< $tp1$; $tp2$ >> ->
            <:binding@loc< $loop tp1$ and $loop tp2$ >>
        | <:ctyp@loc< $lid:nm$ : sexp_bool >> ->
            <:binding@loc< $lid:nm ^ "_field"$ = ref False >>
        | <:ctyp@loc< $lid:nm$ : $_$ >> ->
            <:binding@loc< $lid:nm ^ "_field"$ = ref None >>
        | _ -> assert false  (* impossible *)
      in
      loop flds
    in
    let mc_no_args_fields, mc_fields_with_args = mk_extract_fields flds in
    let loc = Ast.loc_of_ctyp flds in
    <:expr@loc<
      let $field_refs$ and duplicates = ref [] and extra = ref [] in
      let rec iter = fun
        [ [
            Sexplib.Sexp.List
              [(Sexplib.Sexp.Atom field_name); _field_sexp] ::
            tail
          ] ->
            do {
              match field_name with
              [ $mc_fields_with_args$ ];
              iter tail }
        | [Sexplib.Sexp.List [(Sexplib.Sexp.Atom field_name)] :: tail] ->
            do {
              match field_name with
              [ $mc_no_args_fields$ ];
              iter tail }
        | [sexp :: _] ->
            Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
        | [] -> () ]
      in
      do {
        iter field_sexps;
        if Pervasives.(<>) duplicates.val [] then
          Sexplib.Conv_error.record_duplicate_fields
            _tp_loc duplicates.val sexp
        else if Pervasives.(<>) extra.val [] then
          Sexplib.Conv_error.record_extra_fields _tp_loc extra.val sexp
        else $mk_handle_record_match_result has_poly flds$
      }
    >>

  let rec is_poly = function
    | <:ctyp< $_$ : ! $_$ . $_$ >> -> true
    | <:ctyp< $flds1$; $flds2$ >> -> is_poly flds1 || is_poly flds2
    | _ -> false

  (* Generate matching code for records *)
  let record_of_sexp flds =
    let loc = Ast.loc_of_ctyp flds in
    let handle_fields =
      let has_poly = is_poly flds in
      let cnv_fields = mk_cnv_fields has_poly flds in
      if has_poly then
        let is_singleton_ref = ref true in
        let patt =
          let rec loop = function
            | <:ctyp@loc< $tp1$; $tp2$ >> ->
                is_singleton_ref := false;
                <:patt@loc< $loop tp1$, $loop tp2$ >>
            | <:ctyp@loc< $lid:nm$ : $_$ >> -> <:patt@loc< $lid:nm$ >>
            | _ -> assert false  (* impossible *)
          in
          let patt = loop flds in
          if !is_singleton_ref then patt
          else <:patt@loc< $tup:patt$ >>
        in
        let record_def =
          let rec loop = function
            | <:ctyp@loc< $tp1$; $tp2$ >> ->
                <:rec_binding@loc< $loop tp1$; $loop tp2$ >>
            | <:ctyp@loc< $lid:nm$ : $_$ >> ->
                <:rec_binding@loc< $lid:nm$ = $lid:nm$ >>
            | _ -> assert false  (* impossible *)
          in
          loop flds
        in
        <:expr@loc<
          let $patt$ = $cnv_fields$ in
          { $record_def$ }
        >>
      else cnv_fields
    in
    `Match
      <:match_case@loc<
          Sexplib.Sexp.List field_sexps as sexp -> $handle_fields$
        | Sexplib.Sexp.Atom _ as sexp ->
            Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
      >>


  (* Empty type *)
  let nil_of_sexp loc =
    `Fun <:expr@loc< fun sexp -> Sexplib.Conv_error.empty_type _tp_loc sexp >>


  (* Generate code from type definitions *)

  let rec is_poly_call = function
    | <:expr< $f$ $_$ >> -> is_poly_call f
    | <:expr< $lid:name$ >> -> name.[0] = '_' && name.[1] = 'o'
    | _ -> false

  let td_of_sexp loc type_name tps rhs =
    let is_alias_ref = ref false in
    let handle_alias tp =
      is_alias_ref := true;
      type_of_sexp tp
    in
    let coll_args tp param =
      <:ctyp@loc< $tp$ $Gen.drop_variance_annotations param$ >>
    in
    let full_type =
      List.fold_left ~f:coll_args ~init:<:ctyp@loc< $lid:type_name$ >> tps
    in
    let is_variant_ref = ref false in
    let handle_variant row_fields =
      is_variant_ref := true;
      variant_of_sexp ~full_type row_fields
    in
    let body =
      let rec loop tp =
        Gen.switch_tp_def tp
          ~alias:(fun (_ : Loc.t) tp -> handle_alias tp)
          ~sum:(fun (_ : Loc.t) tp -> sum_of_sexp tp)
          ~record:(fun (_ : Loc.t) tp -> record_of_sexp tp)
          ~variants:(fun (_ : Loc.t) tp -> handle_variant tp)
          ~mani:(fun (_ : Loc.t) _tp1 tp2 -> loop tp2)
          ~nil:nil_of_sexp
      in
      match loop rhs with
      | `Fun fun_expr ->
          (* Prevent violation of value restriction and problems with
             recursive types by eta-expanding function definitions *)
          <:expr@loc< fun [ t -> $fun_expr$ t ] >>
      | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>
    in
    let internal_name = type_name ^ "_of_sexp__" in
    let arg_patts, arg_exprs =
      List.split (
        List.map ~f:(function tp ->
            let name = "_of_" ^ Gen.get_tparam_id tp in
            <:patt@loc< $lid:name$ >>, <:expr@loc< $lid:name$ >>
          )
          tps)
    in
    let with_poly_call = !is_alias_ref && is_poly_call body in
    let internal_fun_body =
      let full_type_name =
        sprintf "%s.%s" (Pa_type_conv.get_conv_path ()) type_name
      in
      if with_poly_call then
        Gen.abstract loc arg_patts
          <:expr@loc<
            fun sexp ->
              Sexplib.Conv_error.silly_type $str:full_type_name$ sexp
          >>
      else
        <:expr@loc<
          let _tp_loc = $str:full_type_name$ in
          $Gen.abstract loc arg_patts body$
        >>
    in
    let pre_external_fun_body =
      let internal_call =
        let internal_expr = <:expr@loc< $lid:internal_name$ >> in
        <:expr@loc< $Gen.apply loc internal_expr arg_exprs$ sexp >>
      in
      let no_variant_match_mc =
        <:match_case@loc<
          Sexplib.Conv_error.No_variant_match (_tp_loc, sexp) ->
            Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
        >>
      in
      if with_poly_call then
        <:expr@loc< try $body$ sexp with [ $no_variant_match_mc$ ] >>
      (* Type alias may refer to variant, therefore same handling here! *)
      else if !is_variant_ref || !is_alias_ref then
        <:expr@loc< try $internal_call$ with [ $no_variant_match_mc$ ] >>
      else internal_call
    in
    let internal_binding =
      <:binding@loc< $lid:internal_name$ = $internal_fun_body$ >>
    in
    let external_fun_patt = <:patt@loc< $lid:type_name ^ "_of_sexp"$ >> in
    let external_fun_body =
      Gen.abstract loc arg_patts
        <:expr@loc< fun sexp -> $pre_external_fun_body$ >>
    in
    let external_binding =
      <:binding@loc< $external_fun_patt$ = $external_fun_body$ >>
    in
    internal_binding, external_binding

  let rec tds_of_sexp acc = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        td_of_sexp loc type_name tps rhs :: acc
    | Ast.TyAnd (_, tp1, tp2) -> tds_of_sexp (tds_of_sexp acc tp2) tp1
    | _ -> assert false  (* impossible *)

  (* Generate code from type definitions *)
  let of_sexp = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        let internal_binding, external_binding =
          td_of_sexp loc type_name tps rhs
        in
        let recursive = Gen.type_is_recursive type_name rhs in
        if recursive then
          <:str_item@loc<
            value rec $internal_binding$
            and $external_binding$
          >>
        else
          <:str_item@loc<
            value $internal_binding$;
            value $external_binding$
          >>
    | Ast.TyAnd (loc, _, _) as tds ->
        let two_bindings = tds_of_sexp [] tds in
        let bindings =
          List.map ~f:(fun (b1, b2) -> <:binding@loc< $b1$ and $b2$ >>)
            two_bindings
        in
        <:str_item@loc< value rec $list:bindings$ >>
    | _ -> assert false  (* impossible *)

  (* Add code generator to the set of known generators *)
  let () = Pa_type_conv.add_generator "of_sexp" of_sexp
end

module Quotations = struct
  let of_sexp_quote loc _loc_name_opt cnt_str =
    Pa_type_conv.set_conv_path_if_not_set loc;
    let ctyp = Gram.parse_string Syntax.ctyp_quot loc cnt_str in
    let fp = Generate_of_sexp.type_of_sexp ctyp in
    let body =
      match fp with
      | `Fun fun_expr -> <:expr@loc< $fun_expr$ sexp >>
      | `Match matchings -> <:expr@loc< match sexp with [$matchings$]  >>
    in
    let full_type_name =
      sprintf "%s line %i: %s"
        (Pa_type_conv.get_conv_path ()) (Loc.start_line loc) cnt_str
    in
    <:expr@loc<
      fun [ sexp ->
        let _tp_loc = $str:full_type_name$ in
        $body$ ]
      >>

  let () =
    Syntax.Quotation.add "of_sexp" Syntax.Quotation.DynAst.expr_tag
      of_sexp_quote

 let sexp_of_quote loc _loc_name_opt cnt_str =
   Pa_type_conv.set_conv_path_if_not_set loc;
   let ctyp = Gram.parse_string Syntax.ctyp_quot loc cnt_str in
   Generate_sexp_of.mk_cnv_expr ctyp

  let () =
    Syntax.Quotation.add "sexp_of" Syntax.Quotation.DynAst.expr_tag
      sexp_of_quote
end

(* Add "of_sexp" and "sexp_of" as "sexp" to the set of generators *)
let () =
  Pa_type_conv.add_generator
    "sexp"
    (fun tds ->
      let loc = Ast.loc_of_ctyp tds in
      <:str_item@loc<
        $Generate_of_sexp.of_sexp tds$; $Generate_sexp_of.sexp_of tds$
      >>
    )
