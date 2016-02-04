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


# 0 "./cocci_vs_c.mli"
(*****************************************************************************)
(* Cocci vs C *)
(*****************************************************************************)

(* This module was introduced to factorize code between
 * pattern.ml and transformation.ml. In both cases we need
 * to "compare" a piece of C with a piece of Cocci, and depending
 * if we want just to pattern or transform, we perform different
 * actions on the tokens. So, the common code is in this module
 * and the module specific actions are in pattern.ml and transformation.ml.
 *
 * We could have used a visitor approach as in visitor_c but I prefer
 * this time to use a functor. The specific actions are passed
 * via a module to the functor.
 *
 * If the functor is too complex too understand, you can look at
 * the comments in pattern.ml and transformation.ml to look at
 * how it was done before, which may help to understand how
 * it is done now.
 *
 * You can also look at the papers on parser combinators in haskell
 * (cf a pearl by meijer in ICFP) to understand our monadic
 * approach to matching/unifying.
 *)


(* should be used as less as possible. Most of the time the code in
 * cocci_vs_c should be the same if we pattern or transform *)
type mode = PatternMode | TransformMode

(* used in both pattern and transform, in envf *)
val equal_metavarval :
  Ast_c.metavar_binding_kind -> Ast_c.metavar_binding_kind -> bool

(* for inherited metavariables.  no declaration link on expressions *)
val equal_inh_metavarval :
  Ast_c.metavar_binding_kind -> Ast_c.metavar_binding_kind -> bool

(*****************************************************************************)
(* The parameter of the functor (the specific actions) *)
(*****************************************************************************)


module type PARAM =
  sig
    type tin
    type 'a tout

    (* a matcher between 'a' and 'b'  take 'a' and 'b' in parameter,
     * and "something" (tin; a state that is threaded across calls),
     * and return a new 'a' and 'b' encapsulated in "something" (tout)
     *)
    type ('a, 'b) matcher = 'a -> 'b -> tin -> ('a * 'b) tout

    val mode : mode

    (* -------------------------------------------------------------------- *)
    (* The monadic combinators *)
    (* -------------------------------------------------------------------- *)

    (* it kinds of take a matcher in parameter, and another matcher,
     * and returns a matcher, so =~ matcher -> matcher -> matcher
     *)
    val ( >>= ) :
      (tin -> ('a * 'b) tout) ->
      ('a -> 'b -> tin -> ('c * 'd) tout) ->
      tin -> ('c * 'd) tout

    val return : 'a * 'b -> tin -> ('a * 'b) tout
    val fail : tin -> ('a * 'b) tout

    val ( >||> ) :  (tin -> 'a tout) -> (tin -> 'a tout) -> tin -> 'a tout
    val ( >|+|> ) : (tin -> 'a tout) -> (tin -> 'a tout) -> tin -> 'a tout
    val ( >&&> ) :  (tin -> bool) -> (tin -> 'a tout) -> tin -> 'a tout

    (* -------------------------------------------------------------------- *)
    (* Tokens tagging *)
    (* -------------------------------------------------------------------- *)
    val tokenf :     ('a Ast_cocci.mcode,  Ast_c.info) matcher
    val tokenf_mck : (Ast_cocci.mcodekind, Ast_c.info) matcher

    (* -------------------------------------------------------------------- *)
    (* Distr_f functions, to tag a range of tokens *)
    (* -------------------------------------------------------------------- *)

    val distrf_e :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.expression) matcher

    val distrf_args :
      (Ast_cocci.meta_name Ast_cocci.mcode,
      (Ast_c.argument, Ast_c.il) Common.either list)
      matcher

    val distrf_type :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.fullType) matcher

    val distrf_params :
      (Ast_cocci.meta_name Ast_cocci.mcode,
      (Ast_c.parameterType, Ast_c.il) Common.either list)
      matcher
    val distrf_param :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.parameterType) matcher

    val distrf_ini :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.initialiser) matcher
    val distrf_inis :
      (Ast_cocci.meta_name Ast_cocci.mcode,
       (Ast_c.initialiser, Ast_c.il) Common.either list) matcher
    val distrf_decl :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.declaration) matcher
    val distrf_field :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.field) matcher

    val distrf_node :
      (Ast_cocci.meta_name Ast_cocci.mcode, Control_flow_c.node) matcher

    val distrf_fragments :
      (Ast_cocci.meta_name Ast_cocci.mcode,
       (Ast_c.string_fragment, Ast_c.il) Common.either list) matcher
    val distrf_format :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.string_format) matcher

    val distrf_define_params :
      (Ast_cocci.meta_name Ast_cocci.mcode,
      (string Ast_c.wrap, Ast_c.il) Common.either list)
      matcher

    val distrf_pragmainfo :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.pragmainfo) matcher
    val distrf_ident_list :
      (Ast_cocci.meta_name Ast_cocci.mcode,
       (Ast_c.name, Ast_c.il) Common.either list) matcher

    val distrf_enum_fields :
      (Ast_cocci.meta_name Ast_cocci.mcode,
       (Ast_c.oneEnumType, Ast_c.il) Common.either list) matcher

    val distrf_struct_fields :
      (Ast_cocci.meta_name Ast_cocci.mcode, Ast_c.field list)
      matcher

    val distrf_cst :
      (Ast_cocci.meta_name Ast_cocci.mcode,
      (Ast_c.constant, string) Common.either Ast_c.wrap)
      matcher

    (* -------------------------------------------------------------------- *)
    (* Modifying nested expression and nested types, with Exp and Ty *)
    (* -------------------------------------------------------------------- *)

    val cocciExp :
      (Ast_cocci.expression, Ast_c.expression) matcher ->
      (Ast_cocci.expression, Control_flow_c.node) matcher

    val cocciExpExp :
      Ast_cocci.mcodekind ->
      (Ast_cocci.expression, Ast_c.expression) matcher ->
      (Ast_cocci.expression, Ast_c.expression) matcher

    val cocciTy :
      (Ast_cocci.fullType, Ast_c.fullType) matcher ->
      (Ast_cocci.fullType, Control_flow_c.node) matcher

    val cocciInit :
      (Ast_cocci.initialiser, Ast_c.initialiser) matcher ->
      (Ast_cocci.initialiser, Control_flow_c.node) matcher

    (* -------------------------------------------------------------------- *)
    (* Environment manipulation. Extract info from tin, the "something" *)
    (* -------------------------------------------------------------------- *)
    val envf :
      Ast_cocci.keep_binding ->
      Ast_cocci.inherited ->
      Ast_cocci.meta_name Ast_cocci.mcode * Ast_c.metavar_binding_kind *
	  (* pos info, if needed *)
	  (unit -> Common.filename * string * Ast_c.posl * Ast_c.posl) ->
      (unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val check_idconstraint :
      ('a -> 'b -> bool) -> 'a -> 'b ->
	(unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val check_constraints_ne :
      ('a, 'b) matcher -> 'a list -> 'b ->
	(unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val all_bound : Ast_cocci.meta_name list -> tin -> bool


    val optional_storage_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val optional_qualifier_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val value_format_flag: (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val optional_declarer_semicolon_flag :
	(bool -> tin -> 'x tout) -> (tin -> 'x tout)

  end


(*****************************************************************************)
(* The functor itself *)
(*****************************************************************************)

module COCCI_VS_C :
  functor (X : PARAM) ->
    sig
      type ('a, 'b) matcher = 'a -> 'b -> X.tin -> ('a * 'b) X.tout

      val rule_elem_node : (Ast_cocci.rule_elem, Control_flow_c.node) matcher

      val expression :     (Ast_cocci.expression, Ast_c.expression)   matcher

      (* there are far more functions in this functor but they do not have
       * to be exported
       *)

    end


