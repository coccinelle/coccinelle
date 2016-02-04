(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2009, University of Urbana Champaign.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

module T = Token_c


(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* A trimmed down version of my comment_annotater of CComment. In CComment
 * I was also trying to associate the comment to the relevant entity, not
 * just the closest token (e.g. a function comment is not placed next to the
 * identifier of the function but before its return type or storage).
 *)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_comment_or_space_or_stuff tok =
  Token_helpers.is_not_in_ast tok && Token_helpers.is_origin tok

(* coupling with token_helpers.is_not_in_ast, and of course with tokens_c.ml *)
let convert_relevant_tokens x =
  assert (Token_helpers.is_origin x);

  match x with
  | Parser_c.TCommentSpace info ->
      Token_c.TCommentSpace, (Ast_c.parse_info_of_info info)
  | Parser_c.TCommentNewline info ->
      Token_c.TCommentNewline, (Ast_c.parse_info_of_info info)

  | Parser_c.TComment info ->
      Token_c.TComment, (Ast_c.parse_info_of_info info)

  (* the passed tokens because of our limited handling of cpp *)
  | Parser_c.TCommentCpp(cppcommentkind, info) ->
      Token_c.TCommentCpp cppcommentkind, (Ast_c.parse_info_of_info info)

  | _ -> raise (Impossible 61)


(*****************************************************************************)
(* Main entry *)
(*****************************************************************************)

(* right now we just add comment-like and origin-tok tokens,
 * as explained in token_c.ml.
 *
 * This simplified comment_annotater (compared to CComment) is really
 * simple as the tokens and the Ast_c.info in the asts actually share
 * the same refs.
 * So, modifying fields in the tokens will also modify the info in
 * the ast. Sometimes side effects simplify programming ...
 * We use similar tricks in unparse_c.ml. So really the asts argument
 * is not needed.
 *
 * ex: C1 C2 T1 T2 C3 C4 T3 C5 T4.
 *  => infoT1(-C1C2,+), infoT2(-,+C3C4), infoT3(-C3C4,+C5), infoT4(-C5,+)
 *)

(*
let (agglomerate_either:
 ('a, 'a) Common.either list -> ('a list, 'a list) Common.either list) = fun xs ->
  raise Todo

let (span_and_pack:
 ('a -> ('a, 'a) Common.either) -> 'a list ->
      ('a list, 'a list) Common.either list) = fun f_either xs ->
  let xs' = List.map f_either xs in
  agglomerate_either xs'
*)


(* the asts is not really used, we do all via side effect on the tokens,
 * which share the info reference with the elements in the ast.
 *)
let annotate_program toks asts =
   (* Common.exclude_but_keep_attached gather all comments before a
    * token and then associates to this token those comments. Note that
    * if reverse the list of tokens then this function can also be used
    * to gather all the comments after a token :)
    *)

   (* before phase *)
   let toks_with_before =
     Common.exclude_but_keep_attached is_comment_or_space_or_stuff
       toks
   in

  (* after phase. trick: reverse the tokens and reuse previous func *)
   let toks_with_after =
     List.fold_left (* comes out reversed *)
       (function prev -> function (x,l) -> (x,List.rev l) :: prev) []
       (Common.exclude_but_keep_attached is_comment_or_space_or_stuff
          (List.rev toks))
   in

  (* merge *)
   assert(List.length toks_with_after =|= List.length toks_with_before);

  List.iter2 (fun (t1, before) (t2, after) ->

    assert(t1 =*= t2);

    let before' = before +> List.map convert_relevant_tokens in
    let after' = after  +> List.map convert_relevant_tokens in

    let info = Token_helpers.info_of_tok t1 in
    info.Ast_c.comments_tag :=
      { Ast_c.mbefore = before';
        Ast_c.mafter = after';
        mbefore2 = [];
        mafter2 = [];
      };

    )
    toks_with_before toks_with_after;
   (* modified via side effect. I return it just to have a
    * clean signature.
    *)
   asts
