(* Yoann Padioleau, Julia Lawall
 *
 * Copyright (C) 2012, INRIA.
 * Copyright (C) 2010, 2011, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007, 2008, 2009 Ecole des Mines de Nantes and DIKU
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *
 *
 * Modifications by Julia Lawall for better newline handling.
 *)
open Common

open Ast_c

module TH = Token_helpers


(* should keep comments and directives in between adjacent deleted terms,
but not comments and directives within deleted terms.  should use the
labels found in the control-flow graph *)



(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_unparsing

(*****************************************************************************)
(* Types used during the intermediate phases of the unparsing *)
(*****************************************************************************)

type token1 =
  | Fake1 of info
  | T1 of Parser_c.token

(* The cocci_tag of the token should always be a NOTHING. The mark of
 * the token can only be OriginTok or ExpandedTok. Why not get rid of
 * token and get something simpler ? because we need to know if the
 * info is a TCommentCpp or TCommentSpace, etc for some of the further
 * analysis so easier to keep with the token.
 *
 * This type contains the whole information. Have all the tokens with this
 * type.
 *)
type min =
    Min of (int list (* match numbers from witness trees *) *
	      Ast_cocci.adjacency (* adjacency information *))
  | Ctx

type token2 =
  | T2 of Parser_c.token * min *
          int option (* orig index, abstracting away comments and space *)
  | Fake2 of min
  | Cocci2 of string * int (* line *) * int (* lcol *) * int (* rcol *)
	* Unparse_cocci.nlhint option
  | C2 of string
  | Comma of string
  | Indent_cocci2
  | Unindent_cocci2 of bool (* true for permanent, false for temporary *)
  | EatSpace2

(* not used yet *)
type token3 =
  | T3 of Parser_c.token
  | Cocci3 of string
  | C3 of string


(* similar to the tech in parsing_hack *)
type token_extended = {
  tok2 : token2;
  str  : string;
  idx: int option; (* to know if 2 tokens were consecutive in orig file *)
  mutable new_tokens_before : token2 list;
  mutable remove : bool;
}


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let info_of_token1 t =
  match t with
  | Fake1 info -> info
  | T1 tok -> TH.info_of_tok tok

let print_token1 = function
    T1 tok -> TH.str_of_tok tok
  | Fake1 info -> "fake"

let str_of_token2 = function
  | T2 (t,_,_) -> TH.str_of_tok t
  | Fake2 _ -> ""
  | Cocci2 (s,_,_,_,_) -> s
  | C2 s -> s
  | Comma s -> s
  | Indent_cocci2 -> ""
  | Unindent_cocci2 _ -> ""
  | EatSpace2 -> ""

let print_token2 = function
  | T2 (t,b,_) ->
      let t_str =
	match t with
	| Parser_c.TCommentSpace _ -> " sp "
	| Parser_c.TCommentNewline _ -> " nl "
	| Parser_c.TCommentCpp _ -> " cp "
	| Parser_c.TCommentMisc _ -> " misc "
	| Parser_c.TComment _ -> " comment "
	| _ -> "" in
      let b_str =
	match b with
	  Min (index,adj) ->
	    Printf.sprintf "-%d[%s]"
	      (match adj with Ast_cocci.ADJ n -> n | _ -> -1)
	      (String.concat " " (List.map string_of_int index))
	| Ctx -> "" in
      "T2:"^b_str^t_str^TH.str_of_tok t
  | Fake2 b ->
      let b_str =
	match b with
	  Min (index,adj) ->
	    Printf.sprintf "-%d[%s]"
	      (match adj with Ast_cocci.ADJ n -> n | _ -> -1)
	      (String.concat " " (List.map string_of_int index))
	| Ctx -> "" in
      b_str^"fake"
  | Cocci2 (s,_,lc,rc,_) -> Printf.sprintf "Cocci2:%d:%d%s" lc rc s
  | C2 s -> "C2:"^s
  | Comma s -> "Comma:"^s
  | Indent_cocci2 -> "Indent"
  | Unindent_cocci2 _ -> "Unindent"
  | EatSpace2 -> "EatSpace"

let simple_print_all_tokens1 l =
  List.iter (function x -> Printf.printf "|%s| " (print_token1 x)) l;
  Printf.printf "\n"

let simple_print_all_tokens2 l =
  List.iter (function x -> Printf.printf "|%s| " (print_token2 x)) l;
  Printf.printf "\n"

let str_of_token3 = function
  | T3 t -> TH.str_of_tok t
  | Cocci3 s | C3 s -> s



let mk_token_extended x =
  let origidx =
    match x with
    | T2 (_,_, idx) -> idx
    | _ -> None
  in
  { tok2 = x;
    str = str_of_token2 x;
    idx = origidx;
    new_tokens_before = [];
    remove = false;
  }

let rebuild_tokens_extented toks_ext =
  let _tokens = ref [] in
  toks_ext +> List.iter (fun tok ->
    tok.new_tokens_before +> List.iter (fun x -> push2 x _tokens);
    if not tok.remove then push2 tok.tok2 _tokens;
  );
  let tokens = List.rev !_tokens in
  (tokens +> List.map mk_token_extended)


let mcode_contain_plus = function
  | Ast_cocci.CONTEXT (_,Ast_cocci.NOTHING) -> false
  | Ast_cocci.CONTEXT _ -> true
(* patch: when need full coccinelle transformation *)
  | Ast_cocci.MINUS (_,_,_,Ast_cocci.NOREPLACEMENT) -> false
  | Ast_cocci.MINUS (_,_,_,Ast_cocci.REPLACEMENT _) -> true(*REPL is not empty*)
  | Ast_cocci.PLUS _ -> raise Impossible

let contain_plus info =
  let mck = Ast_c.mcode_of_info info in
  mcode_contain_plus mck

(*****************************************************************************)
(* Last fix on the ast *)
(*****************************************************************************)

(* Because of the ugly trick to handle initialiser, I generate fake ','
 * for the last initializer element, but if there is nothing around it,
 * I don't want in the end to print it.
 *)

let remove_useless_fakeInfo_struct program =
  let bigf = { Visitor_c.default_visitor_c_s with
    Visitor_c.kini_s = (fun (k,bigf) ini ->
      match k ini with
      | InitList args, ii ->
          (match ii with
          | [_i1;_i2] -> ini
          | [i1;i2;iicommaopt] ->
              if (not (contain_plus iicommaopt)) && (not (contain_plus i2))
                 && (Ast_c.is_fake iicommaopt)
                 (* sometimes the guy put a normal iicommaopt *)
              then InitList args, [i1;i2]
              else InitList args, [i1;i2;iicommaopt]
          | [i1;i2;iicommaopt;end_comma_opt] ->
	      (* only in #define. end_comma_opt canot be fake *)
	      (* not sure if this will be considered ambiguous with a previous
		 case? *)
              if (not (contain_plus iicommaopt)) && (not (contain_plus i2))
                 && (Ast_c.is_fake iicommaopt)
                 (* sometimes the guy put a normal iicommaopt *)
              then InitList args, [i1;i2;end_comma_opt]
              else InitList args, [i1;i2;iicommaopt;end_comma_opt]
          | _ -> raise Impossible
          )
      | x -> x
    )
  } in
  Visitor_c.vk_toplevel_s bigf program


(*****************************************************************************)
(* Tokens1 generation *)
(*****************************************************************************)

let get_fakeInfo_and_tokens celem toks =
  let toks_in  = ref toks in
  let toks_out = ref [] in

  (* todo? verify good order of position ? *)
  let pr_elem info =
    match Ast_c.pinfo_of_info info with
    | FakeTok _ ->
        Common.push2 (Fake1 info) toks_out
    | OriginTok _ | ExpandedTok _ ->
        (* get the associated comments/space/cppcomment tokens *)
        let (before, x, after) =
	  !toks_in +> Common.split_when (fun tok ->
	    info =*= TH.info_of_tok tok)
        in
        assert(info =*= TH.info_of_tok x);
        (*old: assert(before +> List.for_all (TH.is_comment)); *)
        before +> List.iter (fun x ->
          if not (TH.is_comment x)
          then pr2 ("WEIRD: not a comment:" ^ TH.str_of_tok x)
          (* case such as  int asm d3("x"); not yet in ast *)
        );
        before +> List.iter (fun x -> Common.push2 (T1 x) toks_out);
        push2 (T1 x) toks_out;
        toks_in := after;
    | AbstractLineTok _ ->
        (* can be called on type info when for instance use -type_c *)
        if !Flag_parsing_c.pretty_print_type_info
        then Common.push2 (Fake1 info) toks_out
        else raise Impossible (* at this stage *)
  in

  let pr_space _ = () in (* use the spacing that is there already *)

  Pretty_print_c.pp_program_gen pr_elem pr_space celem;

  if not (null !toks_in)
  then failwith "WEIRD: unparsing not finished";

  List.rev !toks_out

(* Fake nodes that have BEFORE code or are - should be moved over any subsequent
whitespace and newlines, but not any comments, to get as close to the affected
code as possible.  Similarly, fake nodes that have AFTER code should be moved
backwards.  No fake nodes should have both before and after code. *)

let displace_fake_nodes toks =
  let is_fake = function Fake1 _ -> true | _ -> false in
  let is_whitespace = function
      T1(Parser_c.TCommentSpace _)
      (* patch: cocci    *)
    | T1(Parser_c.TCommentNewline _) -> true
    | _ -> false in
  let rec loop toks =
    let fake_info =
      try Some (Common.split_when is_fake toks)
      with Not_found -> None in
    match fake_info with
      Some(bef,((Fake1 info) as fake),aft) ->
	(match !(info.cocci_tag) with
        | Some x ->
          (match x with
	    (Ast_cocci.CONTEXT(_,Ast_cocci.BEFORE _),_)
	  | (Ast_cocci.MINUS(_,_,_,Ast_cocci.REPLACEMENT _),_) ->
	    (* move the fake node forwards *)
	      let (whitespace,rest) = Common.span is_whitespace aft in
	      bef @ whitespace @ fake :: (loop rest)
	  | (Ast_cocci.CONTEXT(_,Ast_cocci.AFTER _),_) ->
	    (* move the fake node backwards *)
	      let revbef = List.rev bef in
	      let (revwhitespace,revprev) = Common.span is_whitespace revbef in
	      let whitespace = List.rev revwhitespace in
	      let prev = List.rev revprev in
	      prev @ fake :: (loop (whitespace @ aft))
	  | (Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING),_) ->
	      bef @ fake :: (loop aft)
	  | (Ast_cocci.CONTEXT(_,Ast_cocci.BEFOREAFTER _),_) ->
	      failwith "fake node should not be before-after"
	  | _ -> bef @ fake :: (loop aft) (* old: was removed when have simpler yacfe *)
        )
        | None ->
            bef @ fake :: (loop aft)
        )
    | None -> toks
    | _ -> raise Impossible in
  loop toks

(*****************************************************************************)
(* Tokens2 generation *)
(*****************************************************************************)

let comment2t2 = function
    (Token_c.TCommentCpp
       (* not sure iif the following list is exhaustive or complete *)
       (Token_c.CppAttr|Token_c.CppMacro|Token_c.CppPassingCosWouldGetError),
     (info : Token_c.info)) ->
      C2(info.Common.str)
  | (Token_c.TCommentCpp x,(info : Token_c.info)) ->
      C2("\n"^info.Common.str^"\n")
  | x -> failwith (Printf.sprintf "unexpected comment %s" (Common.dump x))

let expand_mcode toks =
  let toks_out = ref [] in

  let index = ref 0 in

  let add_elem t minus =
    match t with
    | Fake1 info ->
        let str = Ast_c.str_of_info info in
        if str =$= ""
        then push2 (Fake2 minus) toks_out
	(* fx the fake "," at the end of a structure or enum.
	   no idea what other fake info there can be... *)
	else push2 (Comma str) toks_out


    | T1 tok ->
	(*let (a,b) = !((TH.info_of_tok tok).cocci_tag) in*)
        (* no tag on expandedTok ! *)
	let modified = function
	    None -> false
	  | Some (Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING),l) -> false
	  | _ -> true in
        (if (TH.is_expanded tok &&
	     modified !((TH.info_of_tok tok).cocci_tag)
            (*!((TH.info_of_tok tok).cocci_tag) <> Ast_c.emptyAnnot*))
	then
	  failwith
	    (Printf.sprintf
	       "expanded token %s on line %d is either modified or stored in a metavariable"
	       (TH.str_of_tok tok) (TH.line_of_tok tok)));

        let tok' = tok +> TH.visitor_info_of_tok (fun i ->
          { i with cocci_tag = ref Ast_c.emptyAnnot; }
        ) in

        let optindex =
          if TH.is_origin tok && not (TH.is_real_comment tok)
          then begin
              incr index;
              Some !index
          end
          else None
        in

        push2 (T2 (tok', minus, optindex)) toks_out
  in

  let expand_info t =
    let (mcode,env) =
      Ast_c.mcode_and_env_of_cocciref ((info_of_token1 t).cocci_tag) in

    let pr_cocci s ln col rcol hint =
      push2 (Cocci2(s,ln,col,rcol,hint)) toks_out  in
    let pr_c info =
      (match Ast_c.pinfo_of_info info with
	Ast_c.AbstractLineTok _ ->
	  push2 (C2 (Ast_c.str_of_info info)) toks_out
      |	Ast_c.FakeTok (s,_) ->
	  push2 (C2 s) toks_out
      |	_ ->
	  Printf.fprintf stderr "line: %s\n" (Common.dump info);
	  failwith "not an abstract line");
      (!(info.Ast_c.comments_tag)).Ast_c.mafter +>
      List.iter (fun x -> Common.push2 (comment2t2 x) toks_out) in

    let pr_barrier ln col = (* marks a position, used around C code *)
      push2 (Cocci2("",ln,col,col,None)) toks_out  in
    let pr_nobarrier ln col = () in (* not needed for linux spacing *)

    let pr_cspace _ = push2 (C2 " ") toks_out in

    let pr_space _ = () (* rely on add_space in cocci code *) in
    let pr_arity _ = () (* not interested *) in

    let indent _   = push2 Indent_cocci2 toks_out in
    let unindent x = push2 (Unindent_cocci2 x) toks_out in
    let eat_space _   = push2 EatSpace2 toks_out in

    let args_pp =
      (env, pr_cocci, pr_c, pr_cspace,
       (match !Flag_parsing_c.spacing with
	 Flag_parsing_c.SMPL -> pr_space | _ -> pr_cspace),
       pr_arity,
       (match !Flag_parsing_c.spacing with
	 Flag_parsing_c.SMPL -> pr_barrier | _ -> pr_nobarrier),
       indent, unindent, eat_space) in

    (* old: when for yacfe with partial cocci:
     *    add_elem t false;
    *)

    (* patch: when need full coccinelle transformation *)
    let unparser = Unparse_cocci.pp_list_list_any args_pp false in
    match mcode with
    | Ast_cocci.MINUS (_,inst,adj,any_xxs) ->
        (* Why adding ? because I want to have all the information, the whole
         * set of tokens, so I can then process and remove the
         * is_between_two_minus for instance *)
        add_elem t (Min (inst,adj));
	(match any_xxs with
	  Ast_cocci.NOREPLACEMENT -> ()
	| Ast_cocci.REPLACEMENT(any_xxs,_) ->
            unparser any_xxs Unparse_cocci.InPlace)
    | Ast_cocci.CONTEXT (_,any_befaft) ->
        (match any_befaft with
        | Ast_cocci.NOTHING ->
            add_elem t Ctx
        | Ast_cocci.BEFORE (xxs,_) ->
            unparser xxs Unparse_cocci.Before;
            add_elem t Ctx
        | Ast_cocci.AFTER (xxs,_) ->
            add_elem t Ctx;
            unparser xxs Unparse_cocci.After;
        | Ast_cocci.BEFOREAFTER (xxs, yys, _) ->
            unparser xxs Unparse_cocci.Before;
            add_elem t Ctx;
            unparser yys Unparse_cocci.After;
        )
    | Ast_cocci.PLUS _ -> raise Impossible

  in

  toks +> List.iter expand_info;
  List.rev !toks_out


(*****************************************************************************)
(* Tokens2 processing, filtering, adjusting *)
(*****************************************************************************)

let is_space = function
  | T2(Parser_c.TCommentSpace _,_b,_i) -> true  (* only whitespace *)
  | _ -> false

let is_newline = function
  | T2(Parser_c.TCommentNewline _,_b,_i) -> true
  | _ -> false

let is_whitespace = function
  | (T2 (t,_b,_i)) ->
      (match t with
      | Parser_c.TCommentSpace _ -> true  (* only whitespace *)
      | Parser_c.TCommentNewline _ (* newline plus whitespace *) -> true
      | _ -> false
      )
  | _ -> false

let is_minusable_comment = function
  | (T2 (t,_b,_i)) ->
      (match t with
      | Parser_c.TCommentSpace _   (* only whitespace *)
      (* patch: coccinelle *)
      | Parser_c.TCommentNewline _ (* newline plus whitespace *) -> true
      | Parser_c.TComment _ when !Flag_parsing_c.keep_comments -> false
      | Parser_c.TComment _
      | Parser_c.TCommentCpp (Token_c.CppAttr, _)
      | Parser_c.TCommentCpp (Token_c.CppMacro, _)
      | Parser_c.TCommentCpp (Token_c.CppIfDirective _, _)
      | Parser_c.TCommentCpp (Token_c.CppDirective, _) (* result was false *)
        -> true

      | Parser_c.TCommentMisc _
      | Parser_c.TCommentCpp (Token_c.CppPassingCosWouldGetError, _)
        -> false

      | _ -> false
      )
  | _ -> false

let is_minusable_comment_nocpp = function
  | (T2 (t,_b,_i)) ->
      (match t with
      | Parser_c.TCommentSpace _   (* only whitespace *)
      (* patch: coccinelle *)
      | Parser_c.TCommentNewline _ (* newline plus whitespace *) -> true
      | Parser_c.TComment _ when !Flag_parsing_c.keep_comments -> false
      | Parser_c.TComment _ -> true
      | Parser_c.TCommentCpp (Token_c.CppAttr, _)
      | Parser_c.TCommentCpp (Token_c.CppMacro, _)
      | Parser_c.TCommentCpp (Token_c.CppIfDirective _, _)
      | Parser_c.TCommentCpp (Token_c.CppDirective, _)
        -> false

      | Parser_c.TCommentMisc _
      | Parser_c.TCommentCpp (Token_c.CppPassingCosWouldGetError, _)
        -> false

      | _ -> false
      )
  | _ -> false

let all_coccis = function
    Cocci2 _ | C2 _ | Comma _ | Indent_cocci2 | Unindent_cocci2 _
  | EatSpace2 -> true
  | _ -> false

(*previously gave up if the first character was a newline, but not clear why*)
let is_minusable_comment_or_plus x = is_minusable_comment x or all_coccis x

let set_minus_comment adj = function
  | T2 (t,Ctx,idx) ->
      let str = TH.str_of_tok t in
      (match t with
      | Parser_c.TCommentSpace _
(* patch: coccinelle *)
      | Parser_c.TCommentNewline _ -> ()

      | Parser_c.TComment _
      | Parser_c.TCommentCpp (Token_c.CppAttr, _)
      | Parser_c.TCommentCpp (Token_c.CppMacro, _)
      | Parser_c.TCommentCpp (Token_c.CppIfDirective _, _)
      | Parser_c.TCommentCpp (Token_c.CppDirective, _)
        ->
          pr2 (Printf.sprintf "%d: ERASING_COMMENTS: %s"
		 (TH.line_of_tok t) str)
      | _ -> raise Impossible
      );
      T2 (t, Min adj, idx)
(* patch: coccinelle *)
  | T2 (t,Min adj,idx) as x -> x
  | Fake2 _ as x -> x
  | _ -> raise Impossible

(* don't touch ifdefs, done after *)
let set_minus_comment_or_plus adj = function
    Cocci2 _ | C2 _ | Comma _ | Indent_cocci2 | Unindent_cocci2 _
  | EatSpace2 as x -> x
  | x -> set_minus_comment adj x

let drop_minus xs =
  xs +> Common.exclude (function
    | T2 (t,Min adj,_) -> true
    | _ -> false
  )

let drop_expanded xs =
  xs +> Common.exclude (function
    | T2 (t,_,_) when TH.is_expanded t -> true
    | _ -> false
  )

let drop_fake xs =
  xs +> Common.exclude (function
    | Fake2 _ -> true
    | _ -> false
  )

let remove_minus_and_between_and_expanded_and_fake xs =

  (* get rid of expanded tok *)
  let xs = drop_expanded xs in

  let minus_or_comment = function
      T2(_,Min adj,_) -> true
    | x -> is_minusable_comment x in

  let minus_or_comment_nocpp = function
      T2(_,Min adj,_) -> true
    | x -> is_minusable_comment_nocpp x in

  let common_adj (index1,adj1) (index2,adj2) =
    let same_adj = (* same adjacency info *)
      match (adj1,adj2) with
	(Ast_cocci.ADJ adj1,Ast_cocci.ADJ adj2) -> adj1 = adj2
      | (Ast_cocci.ALLMINUS,_) | (_,Ast_cocci.ALLMINUS) -> true in
    same_adj &&
    (* non-empty intersection of witness trees *)
    not ((Common.inter_set index1 index2) = []) in

  (* new idea: collects regions not containing non-space context code
     if two adjacent adjacent minus tokens satisfy common_adj then delete
     all spaces, comments etc between them
     if two adjacent minus tokens do not satisfy common_adj only delete
     the spaces between them if there are no comments, etc.
     if the region contain no plus code and is both preceded and followed
     by a newline, delete the initial newline. *)

  let rec adjust_around_minus = function
      [] -> []
    | (T2(Parser_c.TCommentNewline c,_b,_i) as x)::
      ((Fake2(Min adj1) | T2(_,Min adj1,_)) as t1)::xs ->
	let (minus_list,rest) = Common.span not_context (t1::xs) in
	let contains_plus = List.exists is_plus minus_list in
	let x =
	  match List.rev minus_list with
	    (T2(Parser_c.TCommentNewline c,_b,_i))::rest
	    when List.for_all minus_or_comment minus_list ->
	      set_minus_comment_or_plus adj1 x
	  | _ -> x in
	x :: adjust_within_minus contains_plus minus_list @
	adjust_around_minus rest
    | ((Fake2(Min adj1) | T2(_,Min adj1,_)) as t1)::xs ->
	let (minus_list,rest) = Common.span not_context (t1::xs) in
	let contains_plus = List.exists is_plus minus_list in
	adjust_within_minus contains_plus minus_list @ adjust_around_minus rest
    | x::xs ->
	x :: adjust_around_minus xs
  and adjust_within_minus cp (* contains plus *) = function
      ((Fake2(Min adj1) | T2(_,Min adj1,_)) as t1)::xs ->
	let not_minus = function T2(_,Min _,_) -> false | _ -> true in
	let (not_minus_list,rest) = Common.span not_minus xs in
	t1 ::
	(match rest with
	  ((Fake2(Min adj2) | T2(_,Min adj2,_)) as t2)::xs
	  when common_adj adj1 adj2 ->
	    (List.map (set_minus_comment_or_plus adj1) not_minus_list)
	    @ (adjust_within_minus cp (t2::xs))
	| ((Fake2(Min adj2) | T2(_,Min adj2,_)) as t2)::xs ->
	    if not cp && List.for_all is_whitespace not_minus_list
	    then
	      (List.map (set_minus_comment_or_plus adj1) not_minus_list)
	      @ (adjust_within_minus cp (t2::xs))
	    else
	      not_minus_list @ (adjust_within_minus cp (t2::xs))
	| _ ->
	    if cp
	    then xs
	    else
	      (* remove spaces after removed stuff, eg a comma after a
		 function argument *)
	      (let (spaces,rest) = Common.span is_space xs in
	      (List.map (set_minus_comment_or_plus adj1) spaces)
	      @ rest))
    | xs -> failwith "should always start with minus"
  and not_context = function
      (T2(_,Ctx,_) as x) when not (is_minusable_comment x) -> false
    | _ -> true
  and is_plus = function
      C2 _ | Comma _ | Cocci2 _ -> true
    | _ -> false in

  let xs = adjust_around_minus xs in

  (* get rid of fake tok *)
  let xs = drop_fake xs in

  (* this drops blank lines after a brace introduced by removing code *)
  let minus_or_comment_nonl = function
      T2(_,Min adj,_) -> true
    | T2(Parser_c.TCommentNewline _,_b,_i) -> false
    | x -> is_minusable_comment x in

  let rec adjust_after_brace = function
      [] -> []
    | ((T2(_,Ctx,_)) as x)::((T2(_,Min adj,_)::_) as xs)
       when str_of_token2 x =$= "{" ->
	 let (between_minus,rest) = Common.span minus_or_comment_nonl xs in
	 let is_whitespace = function
	     T2(Parser_c.TCommentSpace _,_b,_i)
	     (* patch: cocci    *)
	   | T2(Parser_c.TCommentNewline _,_b,_i) -> true
	   | _ -> false in
	 let (newlines,rest) = Common.span is_whitespace rest in
	 let (drop_newlines,last_newline) =
	   let rec loop = function
	       [] -> ([],[])
	     | ((T2(Parser_c.TCommentNewline _,_b,_i)) as x) :: rest ->
		 (List.rev rest,[x])
	     | x::xs ->
		 let (drop_newlines,last_newline) = loop xs in
		 (drop_newlines,x::last_newline) in
	   loop (List.rev newlines) in
	 x::between_minus@(List.map (set_minus_comment adj) drop_newlines)@
	 last_newline@
	 adjust_after_brace rest
    | x::xs -> x::adjust_after_brace xs in

  let xs = adjust_after_brace xs in

  (* search backwards from context } over spaces until reaching a newline.
     then go back over all minus code until reaching some context or + code.
     get rid of all intervening spaces, newlines, and comments
     input is reversed *)
  let rec adjust_before_brace = function
      [] -> []
    | ((T2(t,Ctx,_)) as x)::xs when str_of_token2 x =$= "}" or is_newline x ->
	let (outer_spaces,rest) = Common.span is_space xs in
	x :: outer_spaces @
	(match rest with
	  ((T2 (Parser_c.TCommentNewline _,Ctx,_i)) as h) ::
	  (* the rest of this code is the same as from_newline below
	     but merging them seems to be error prone... *)
	  ((T2 (t, Min adj, idx)) as m) :: rest ->
	    let (spaces,rest) = Common.span minus_or_comment_nocpp rest in
	    h :: m ::
	    (List.map (set_minus_comment adj) spaces) @
	    (adjust_before_brace rest)
	| _ -> adjust_before_brace rest)
    | x::xs -> x :: (adjust_before_brace xs) in

  let from_newline = function
      ((T2 (t, Min adj, idx)) as m) :: rest ->
	let (spaces,rest) = Common.span minus_or_comment_nocpp rest in
	m ::
	(List.map (set_minus_comment adj) spaces) @
	(adjust_before_brace rest)
    | ((T2 (t0, Ctx, idx0)) as m0) :: ((T2 (t, Min adj, idx)) as m) :: rest
	when TH.str_of_tok t0 = "" ->
	  (* This is for the case of a #define that is completely deleted,
	     because a #define has a strange EOL token at the end.
	     We hope there i no other kind of token that is represented by
	     "", but it seems like changing the kind of token might break
	     the end of entity recognition in the C parser.
	     See parsing_hacks.ml *)
	  let (spaces,rest) = Common.span minus_or_comment_nocpp rest in
	  m0 :: m ::
	  (List.map (set_minus_comment adj) spaces) @
	  (adjust_before_brace rest)
    | rest -> adjust_before_brace rest in

  let xs = List.rev (from_newline (List.rev xs)) in

  let cleanup_ifdefs toks =
  (* TODO: these functions are horrid, but using tokens caused circularity *)
    let is_ifdef = function
	T2((Parser_c.TCommentCpp
	      (Token_c.CppIfDirective Token_c.IfDef, _)),m,idx) -> true
      | T2((Parser_c.TCommentCpp
	      (Token_c.CppIfDirective Token_c.IfDef0, _)),m,idx) -> true
      | t -> false in
    let is_else = function
	T2((Parser_c.TCommentCpp
	      (Token_c.CppIfDirective Token_c.Else, _)),m,idx) -> true
      | _ -> false in
    let is_endif = function
	T2((Parser_c.TCommentCpp
	      (Token_c.CppIfDirective Token_c.Endif, _)),m,idx) -> true
      | _ -> false in
    let add t = function
	l::rest -> (t::l)::rest
      |	_ -> failwith "not possible" in
    let rec parse_ifdef acc_keywords acc_code stack = function
	[] -> (None,acc_keywords,acc_code)
      | t::rest when is_else t ->
	  (match stack with
	    [] -> parse_ifdef (t::acc_keywords) ([]::acc_code) stack rest
	  | _ -> parse_ifdef acc_keywords (add t acc_code) stack rest)
      | t::rest when is_endif t ->
	  (match stack with
	    [] -> ((Some (t,rest)),acc_keywords,acc_code)
	  | _::stack -> parse_ifdef acc_keywords (add t acc_code) stack rest)
      | t::rest when is_ifdef t ->
	  parse_ifdef acc_keywords (add t acc_code) (()::stack) rest
      | t::rest -> parse_ifdef acc_keywords (add t acc_code) stack rest in
    let unminus = function
	T2 (t,Min adj,idx) -> T2 (t,Ctx,idx)
      | x -> x in
    let is_minus = function
	T2 (t,Min adj,idx) -> true
      | x -> false in
    let rec loop = function
	[] -> []
      | t::rest when is_ifdef t ->
	  let (ender,acc_keywords,acc_code) =
	    parse_ifdef [t] [[]] [] rest in
	  let acc_code = List.map loop acc_code in
	  let merge = (* args reversed *)
	    List.fold_left2
	      (fun prev kwd code -> kwd :: (List.rev code) @ prev)
	      [] in
	  (match ender with
	    None -> merge (List.map unminus acc_keywords) acc_code
	  | Some(endif,rest) ->
	      let rest = loop rest in
	      if List.for_all is_minus (endif::acc_keywords)
	      then (merge acc_keywords acc_code) @ (endif :: rest)
	      else
		(merge (List.map unminus acc_keywords) acc_code) @
		((unminus endif) :: rest))
      | x::xs -> x :: loop xs in
    loop toks in
      
  let xs = cleanup_ifdefs xs in
  let xs = drop_minus xs in
  xs

(* things that should not be followed by space - boundary between SmPL
   code and C code *)
let adjust_eat_space toks =
  let rec loop = function
      [] -> []
    | EatSpace2 :: x :: rest when is_space x -> loop rest
    | EatSpace2 :: rest -> loop rest
    | x :: xs -> x :: loop xs in
  loop toks

(* normally, in C code, a semicolon is not preceded by a space or newline *)
let adjust_before_semicolon toks =
  let toks = List.rev toks in
  let rec search_semic = function
      [] -> []
    | ((T2(_,Ctx,_)) as x)::xs | ((Cocci2 _) as x)::xs ->
	if List.mem (str_of_token2 x) [";";")";","]
	then x :: search_semic (search_minus false xs)
	else x :: search_semic xs
    | x::xs -> x :: search_semic xs
  and search_minus seen_minus xs =
    let (spaces, rest) = Common.span is_space xs in
    (* only delete spaces if something is actually deleted *)
    match rest with
      ((T2(_,Min _,_)) as a)::rerest -> a :: search_minus true rerest
    | _ -> if seen_minus then rest else xs in
  List.rev (search_semic toks)

(* normally, in C code, a ( is not followed by a space or newline *)
let adjust_after_paren toks =
  let rec search_paren = function
      [] -> []
    | ((T2(_,Ctx,_)) as x)::xs | ((Cocci2 _) as x)::xs ->
	if List.mem (str_of_token2 x) ["("] (* other things? *)
	then x :: search_paren(search_minus false xs)
	else x :: search_paren xs
    | x::xs -> x :: search_paren xs
  and search_minus seen_minus xs =
    let (spaces, rest) = Common.span is_whitespace xs in
    (* only delete spaces if something is actually deleted *)
    match rest with
      ((T2(_,Min _,_)) as a)::rerest -> (* minus *)
	a :: search_minus true rerest
    | ((T2(_,Ctx,_)) as a)::rerest when seen_minus && str_of_token2 a = "," ->
	(* comma after ( will be deleted, so consider it as minus code
	   already *)
	a :: search_minus true rerest
    | _ -> if seen_minus then rest else xs in (* drop trailing space *)
  search_paren toks

(* this is for the case where braces are added around an if branch *)
let paren_then_brace toks =
  let rec search_paren = function
      [] -> []
    | ((T2(_,Ctx,_)) as x)::xs ->
	if List.mem (str_of_token2 x) [")"]
	then x :: search_paren(search_plus xs)
	else x :: search_paren xs
    | x::xs -> x :: search_paren xs
  and search_plus xs =
    let (spaces, rest) = Common.span is_whitespace xs in
    match rest with
      (* move the brace up to the previous line *)
      ((Cocci2("{",_,_,_,_)) as x) :: (((Cocci2 _) :: _) as rest) ->
	(C2 " ") :: x :: spaces @ rest
    | _ -> xs in
  search_paren toks

let is_ident_like s = s ==~ Common.regexp_alpha

let rec drop_space_at_endline = function
    [] -> []
  | [x] -> [x]
  | (C2 " ") ::
    ((((T2(Parser_c.TCommentSpace _,Ctx,_)) | Cocci2("\n",_,_,_,_) |
    (T2(Parser_c.TCommentNewline _,Ctx,_))) :: _) as rest) ->
      (* when unparse_cocci doesn't know whether space is needed *)
      drop_space_at_endline rest
  | ((T2(Parser_c.TCommentSpace _,Ctx,_i)) as a)::rest ->
      let (outer_spaces,rest) = Common.span is_space rest in
      let minus_or_comment_or_space_nocpp = function
	  T2(_,Min adj,_) -> true
	| (T2(Parser_c.TCommentSpace _,Ctx,_i)) -> true
	| (T2(Parser_c.TCommentNewline _,Ctx,_i)) -> false
	| x -> false in
      let (minus,rest) = Common.span minus_or_comment_or_space_nocpp rest in
      let fail _ = a :: outer_spaces @ minus @ (drop_space_at_endline rest) in
      if List.exists (function T2(_,Min adj,_) -> true | _ -> false) minus
      then
	match rest with
	  ((T2(Parser_c.TCommentNewline _,Ctx,_i)) as a)::rest ->
	    (* drop trailing spaces *)
	    minus@a::(drop_space_at_endline rest)
	| _ -> fail()
      else fail()
  | a :: rest ->
      a :: drop_space_at_endline rest

(* if a removed ( is between two tokens, then add a space *)
let rec paren_to_space = function
    [] -> []
  | [x] -> [x]
  | [x;y] -> [x;y]
  | ((T2(_,Ctx,_)) as a)::((T2(t,Min _,_)) as b)::((T2(_,Ctx,_)) as c)::rest
    when not (is_whitespace a) && TH.str_of_tok t = "(" ->
      a :: b :: (C2 " ") :: (paren_to_space (c :: rest))
  | a :: rest -> a :: (paren_to_space rest)

let rec add_space xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | (Cocci2(sx,lnx,_,rcolx,_) as x)::((Cocci2(sy,lny,lcoly,_,_)) as y)::xs
    when !Flag_parsing_c.spacing = Flag_parsing_c.SMPL &&
      not (lnx = -1) && lnx = lny && not (rcolx = -1) && rcolx < lcoly ->
	(* this only works within a line.  could consider whether
	   something should be done to add newlines too, rather than
	   printing them explicitly in unparse_cocci. *)
	x::C2 (String.make (lcoly-rcolx) ' ')::add_space (y::xs)
  | (Cocci2(sx,lnx,_,rcolx,_) as x)::((Cocci2(sy,lny,lcoly,_,_)) as y)::xs
    when !Flag_parsing_c.spacing = Flag_parsing_c.SMPL &&
      not (lnx = -1) && lnx < lny && not (rcolx = -1) ->
	(* this only works within a line.  could consider whether
	   something should be done to add newlines too, rather than
	   printing them explicitly in unparse_cocci. *)
	x::C2 (String.make (lny-lnx) '\n')::
	C2 (String.make (lcoly-1) ' '):: (* -1 is for the + *)
	add_space (y::xs)
  | ((T2(_,Ctx,_)) as x)::((Cocci2 _) as y)::xs -> (* add space on boundary *)
      let sx = str_of_token2 x in
      let sy = str_of_token2 y in
      if is_ident_like sx && (is_ident_like sy or List.mem sy ["="])
      then x::C2 " "::(add_space (y::xs))
      else x::(add_space (y::xs))
  | x::y::xs -> (* not boundary, not sure if it is possible *)
      let sx = str_of_token2 x in
      let sy = str_of_token2 y in
      if is_ident_like sx && is_ident_like sy
      then x::C2 " "::(add_space (y::xs))
      else x::(add_space (y::xs))

(* A fake comma is added at the end of an unordered initlist or a enum
decl, if the initlist or enum doesn't already end in a comma.  This is only
needed if there is + code, ie if we see Cocci after it in the code sequence *)

let rec drop_end_comma = function
    [] -> []
  | [x] -> [x]
  | ((Comma ",") as x) :: rest ->
      let (newlines,rest2) = Common.span is_whitespace rest in
      (match rest2 with
	(Cocci2 _) :: _ -> x :: drop_end_comma rest
      |	_ -> drop_end_comma rest)
  | x :: xs -> x :: drop_end_comma xs

(* The following only works for the outermost function call.  Stack records
the column of all open parentheses.  Space_cell contains the most recent
comma in the outermost function call.  The goal is to decide whether this
should be followed by a space or a newline and indent. *)
let add_newlines toks tabbing_unit =
  let create_indent n =
    let (tu,tlen) = 
      match tabbing_unit with
	Some ("\t",_) -> ("\t",8)
      | Some ("",_) -> ("\t",8) (* not sure why... *)
      | Some (s,_) -> (s,String.length s) (* assuming only spaces *)
      |	None -> ("\t",8) in
    let rec loop seen =
      if seen + tlen <= n
      then tu ^ loop (seen + tlen)
      else String.make (n-seen) ' ' in
    loop 0 in
  let check_for_newline count x = function
      Some (start,space_cell) when count > Flag_parsing_c.max_width ->
	space_cell := "\n"^(create_indent x);
	Some (x + (count - start))
    | _ -> None in
  (* the following is for strings that may contain newline *)
  let string_length s count =
    let l = list_of_string s in
    List.fold_left
      (function count ->
	function
	    '\t' -> count + 8
	  | '\n' -> 0
	  | c -> count + 1)
      count l in
  let rec loop info count = function
      [] -> []
    | ((T2(tok,_,_)) as a)::xs ->
	a :: loop info (string_length (TH.str_of_tok tok) count) xs
    | ((Cocci2(s,line,lcol,rcol,hint)) as a)::xs ->
	let (stack,space_cell) = info in
	let rest =
	  match hint with
	    None -> loop info (string_length s count) xs
	  | Some Unparse_cocci.StartBox ->
	      let count = string_length s count in
	      loop (count::stack,space_cell) count xs
	  | Some Unparse_cocci.EndBox ->
	      let count = string_length s count in
	      (match stack with
		[x] ->
		  (match check_for_newline count x space_cell with
		    Some count -> loop ([],None) count xs
		  | None -> loop ([],None) count xs)
	      | _ -> loop (List.tl stack,space_cell) count xs)
	  | Some (Unparse_cocci.SpaceOrNewline sp) ->
	      let count = string_length s (count + 1 (*space*)) in
	      (match stack with
		[x] ->
		  (match check_for_newline count x space_cell with
		    Some count -> loop (stack,Some (x,sp)) count xs
		  | None -> loop (stack,Some (count,sp)) count xs)
	      | _ -> loop info count xs) in
	a :: rest
    | ((C2(s)) as a)::xs -> a :: loop info (string_length s count) xs
    | ((Comma(s)) as a)::xs -> a :: loop info (string_length s count) xs
    | Fake2 _ :: _ | Indent_cocci2 :: _
    | Unindent_cocci2 _::_ | EatSpace2::_ ->
	failwith "unexpected fake, indent, unindent, or eatspace" in
  let redo_spaces prev = function
      Cocci2(s,line,lcol,rcol,Some (Unparse_cocci.SpaceOrNewline sp)) ->
        C2 !sp :: Cocci2(s,line,lcol,rcol,None) :: prev
    | t -> t::prev in
  (match !Flag_parsing_c.spacing with
    Flag_parsing_c.SMPL -> toks
  | _ -> List.rev (List.fold_left redo_spaces [] (loop ([],None) 0 toks)))

(* When insert some new code, because of a + in a SP, we must add this
 * code at the right place, with the good indentation. So each time we
 * encounter some spacing info, with some newline, we maintain the
 * current indentation level used.
 *
 * TODO problems: not accurate. ex: TODO
 *
 * TODO: if in #define region, should add a \ \n
 *)
let new_tabbing2 space =
  (list_of_string space)
    +> List.rev
    +> Common.take_until (fun c -> c =<= '\n')
    +> List.rev
    +> List.map string_of_char
    +> String.concat ""

let new_tabbing a =
  Common.profile_code "C unparsing.new_tabbing" (fun () -> new_tabbing2 a)


let rec adjust_indentation xs =

  let _current_tabbing = ref ([] : string list) in
  let tabbing_unit = ref None in

  let string_of_list l = String.concat "" (List.map string_of_char l) in

  (* try to pick a tabbing unit for the plus code *)
  let adjust_tabbing_unit old_tab new_tab =
    if !tabbing_unit =*= None && String.length new_tab > String.length old_tab
    then
      let old_tab = list_of_string old_tab in
      let new_tab = list_of_string new_tab in
      let rec loop = function
	  ([],new_tab) ->
	    tabbing_unit := Some(string_of_list new_tab,List.rev new_tab)
	| (_,[]) -> failwith "not possible"
	| (o::os,n::ns) -> loop (os,ns) in (* could check for equality *)
      loop (old_tab,new_tab) in

(*
  let remtab tu current_tab =
    let current_tab = List.rev(list_of_string current_tab) in
    let rec loop = function
	([],new_tab) -> string_of_list (List.rev new_tab)
      |	(_,[]) -> (-*weird; tabbing unit used up more than the current tab*-)
        ""
      |	(t::ts,n::ns) when t =<= n -> loop (ts,ns)
      |	(_,ns) -> (-* mismatch; remove what we can *-)
	  string_of_list (List.rev ns) in
    loop (tu,current_tab) in
*)

  let rec find_first_tab started = function
      [] -> ()
    | ((T2 (tok,_,_)) as x)::xs when str_of_token2 x =$= "{" ->
	find_first_tab true xs
(* patch: coccinelle *)
    | ((T2 (Parser_c.TCommentNewline s, _, _)) as x)::_
      when started ->
	let s = str_of_token2 x +> new_tabbing in
	tabbing_unit := Some (s,List.rev (list_of_string s))
    | x::xs -> find_first_tab started xs in
  find_first_tab false xs;

  let rec balanced ct = function
      [] -> ct >= 0
    | ((T2(tok,_,_)) as x)::xs ->
	(match str_of_token2 x with
	  "(" -> balanced (ct+1) xs
	| ")" -> balanced (ct-1) xs
	| _ -> balanced ct xs)
    | x::xs -> balanced ct xs in

  let update_tabbing started s x =
    let old_tabbing = !_current_tabbing in
    str_of_token2 x +> new_tabbing +> (fun s -> _current_tabbing := [s]);
    (* only trust the indentation after the first { *)
    if started
    then
      adjust_tabbing_unit
	(String.concat "" old_tabbing)
	(String.concat "" !_current_tabbing) in

  let rec aux started xs =
    match xs with
    | [] ->  []
(* patch: coccinelle *)
    | ((T2 (Parser_c.TCommentNewline s,_,_)) as x)::
      Unindent_cocci2(false)::xs ->
	update_tabbing started s x;
        (C2 "\n")::aux started xs
    | (Cocci2("\n",_,_,_,_))::Unindent_cocci2(false)::xs ->
        (C2 "\n")::aux started xs
    | ((T2 (tok,_,_)) as x)::(T2 (Parser_c.TCommentNewline s, _, _))::
      ((Cocci2 ("{",_,_,_,_)) as a)::xs
      when started && str_of_token2 x =$= ")" ->
	(* to be done for if, etc, but not for a function header *)
	x::(C2 " ")::a::(aux started xs)
    | ((T2 (Parser_c.TCommentNewline s, _, _)) as x)::xs
      when
	balanced 0 (fst(Common.span (function x -> not(is_newline x)) xs)) ->
	update_tabbing started s x;
	let coccis_rest = Common.span all_coccis xs in
	(match coccis_rest with
	  (_::_,((T2 (tok,_,_)) as y)::_) when str_of_token2 y =$= "}" ->
	    (* the case where cocci code has been added before a close } *)
	    x::aux started (Indent_cocci2::xs)
        | _ -> x::aux started xs)
    | Indent_cocci2::((Cocci2(sy,lny,lcoly,_,_)) as y)::xs
      when !Flag_parsing_c.spacing = Flag_parsing_c.SMPL ->
	let tu = String.make (lcoly-1) ' ' in
	_current_tabbing := tu::(!_current_tabbing);
	C2 (tu)::aux started (y::xs)
    | Indent_cocci2::xs ->
	(match !tabbing_unit with
	  None -> aux started xs
	| Some (tu,_) ->
	    _current_tabbing := tu::(!_current_tabbing);
	     (* can't be C2, for later phases *)
	     Cocci2 (tu,-1,-1,-1,None)::aux started xs)
    | Unindent_cocci2(permanent)::xs ->
	(match !_current_tabbing with
	  [] -> aux started xs
	| _::new_tabbing ->
            let s = String.concat "" new_tabbing in
	    _current_tabbing := new_tabbing;
	    Cocci2 (s,-1,-1,-1,None)::aux started xs)
    (* border between existing code and cocci code *)
    | ((T2 (tok,_,_)) as x)::((Cocci2("\n",_,_,_,_)) as y)::xs
      when str_of_token2 x =$= "{" ->
	x::aux true (y::Indent_cocci2::xs)
    | ((Cocci2 _) as x)::((T2 (tok,_,_)) as y)::xs
      when str_of_token2 y =$= "}" ->
	x::aux started (Unindent_cocci2 true::y::xs)
    (* starting the body of the function *)
    | ((T2 (tok,_,_)) as x)::xs when str_of_token2 x =$= "{" ->  x::aux true xs
    | ((Cocci2("{",_,_,_,_)) as a)::xs -> a::aux true xs
    | ((Cocci2("\n",_,_,_,_)) as x)::xs ->
            (* dont inline in expr because of weird eval order of ocaml *)
        let s = String.concat "" !_current_tabbing in
        (* can't be C2, for later phases *)
        x::Cocci2 (s,-1,-1,-1,None)::aux started xs
    | x::xs -> x::aux started xs in
  (aux false xs,!tabbing_unit)


let rec find_paren_comma = function
  | [] -> ()

  (* do nothing if was like this in original file *)
  | ({ str = "("; idx = Some p1 } as _x1)::({ str = ","; idx = Some p2} as x2)
    ::xs when p2 =|= p1 + 1 ->
      find_paren_comma (x2::xs)

  | ({ str = ","; idx = Some p1 } as _x1)::({ str = ","; idx = Some p2} as x2)
    ::xs when p2 =|= p1 + 1 ->
      find_paren_comma (x2::xs)

  | ({ str = ","; idx = Some p1 } as _x1)::({ str = ")"; idx = Some p2} as x2)
    ::xs when p2 =|= p1 + 1 ->
      find_paren_comma (x2::xs)

  (* otherwise yes can adjust *)
  | ({ str = "(" } as _x1)::({ str = ","} as x2)::xs ->
      x2.remove <- true;
      find_paren_comma (x2::xs)
  | ({ str = "," } as x1)::({ str = ","} as x2)::xs ->
      x1.remove <- true;
      find_paren_comma (x2::xs)

  | ({ str = "," } as x1)::({ str = ")"} as x2)::xs ->
      x1.remove <- true;
      find_paren_comma (x2::xs)

  | x::xs ->
      find_paren_comma xs


let fix_tokens toks =
  let toks = toks +> List.map mk_token_extended in

  let cleaner = toks +> Common.exclude (function
    | {tok2 = T2 (t,_,_)} -> TH.is_real_comment t (* I want the ifdef *)
    | _ -> false
  ) in
  find_paren_comma cleaner;

  let toks = rebuild_tokens_extented toks in
  toks +> List.map (fun x -> x.tok2)



(*****************************************************************************)
(* Final unparsing (and debugging support) *)
(*****************************************************************************)

(* for debugging *)
type kind_token2 = KFake | KCocci | KC | KExpanded | KOrigin

let kind_of_token2 = function
  | Fake2 _ -> KFake
  | Cocci2 _ -> KCocci
  | C2 _ -> KC
  | Comma _ -> KC
  | T2 (t,_,_) ->
      (match TH.pinfo_of_tok t with
      | ExpandedTok _ -> KExpanded
      | OriginTok _ -> KOrigin
      | FakeTok _ -> raise Impossible (* now a Fake2 *)
      | AbstractLineTok _ -> raise Impossible (* now a KC *)
      )
  | Unindent_cocci2 _ | Indent_cocci2 | EatSpace2 -> raise Impossible

let end_mark = "!"

let start_mark = function
  | KFake -> "!F!"
  | KCocci -> "!S!"
  | KC -> "!A!"
  | KExpanded -> "!E!"
  | KOrigin -> ""

let print_all_tokens2 pr xs =
  if !Flag_parsing_c.debug_unparsing
  then
    let current_kind = ref KOrigin in
    xs +> List.iter (fun t ->
      let newkind = kind_of_token2 t in
      if newkind =*= !current_kind
      then pr (str_of_token2 t)
      else begin
        pr (end_mark);
        pr (start_mark newkind);
        pr (str_of_token2 t);
        current_kind := newkind
      end
    );
  else
    xs +> List.iter (fun x -> pr (str_of_token2 x))




(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* old: PPviatok was made in the beginning to allow to pretty print a
 * complete C file, including a modified C file by transformation.ml,
 * even if we don't handle yet in pretty_print_c.ml, ast_to_flow (and
 * maybe flow_to_ast) all the cases. Indeed we don't need to do some
 * fancy stuff when a function was not modified at all. Just need to
 * print the list of token as-is. But now pretty_print_c.ml handles
 * almost everything so maybe less useful. Maybe PPviatok allows to
 * optimize a little the pretty printing.
 *
 * update: now have PPviastr which goes even faster than PPviatok, so
 * PPviatok has disappeared.
 *)

type ppmethod = PPnormal | PPviastr




(* The pp_program function will call pretty_print_c.ml with a special
 * function to print the leaf components, the tokens. When we want to
 * print a token, we need to print also maybe the space and comments that
 * were close to it in the original file (and that was omitted during the
 * parsing phase), and honor what the cocci-info attached to the token says.
 * Maybe we will not print the token if it's a MINUS-token, and maybe we will
 * print it and also print some cocci-code attached in a PLUS to it.
 * So we will also maybe call unparse_cocci. Because the cocci-code may
 * contain metavariables, unparse_cocci will in fact sometimes call back
 * pretty_print_c (which will this time don't call back again unparse_cocci)
 *)

let pp_program2 xs outfile  =
  Common.with_open_outfile outfile (fun (pr,chan) ->
    let pr s =
      if !Flag_parsing_c.debug_unparsing
      then begin pr2_no_nl s; flush stderr end
      else pr s
        (* flush chan; *)
        (* Common.pr2 ("UNPARSING: >" ^ s ^ "<"); *)
    in

    xs +> List.iter (fun ((e,(str, toks_e)), ppmethod) ->
      (* here can still work on ast *)
      let e = remove_useless_fakeInfo_struct e in

      match ppmethod with
      | PPnormal ->
          (* now work on tokens *)
          (* phase1: just get all the tokens, all the information *)
          assert(toks_e +> List.for_all (fun t ->
	    TH.is_origin t or TH.is_expanded t
          ));
          let toks = get_fakeInfo_and_tokens e toks_e in
	  let toks = displace_fake_nodes toks in
          (* assert Origin;ExpandedTok;Faketok *)
          let toks = expand_mcode toks in

          (* assert Origin;ExpandedTok; + Cocci + C (was AbstractLineTok)
           * and no tag information, just NOTHING. *)

	  let toks =
	    if !Flag.sgrep_mode2
	    then
	      (* nothing else to do for sgrep *)
	      drop_expanded(drop_fake(drop_minus toks))
	    else
              (* phase2: can now start to filter and adjust *)
	       (let (toks,tu) = adjust_indentation toks in
	      let toks = adjust_eat_space toks in
	      let toks = adjust_before_semicolon toks in(*before remove minus*)
	      let toks = adjust_after_paren toks in(*also before remove minus*)
	      let toks = drop_space_at_endline toks in
	      let toks = paren_to_space toks in
	      let toks = drop_end_comma toks in
	      let toks = remove_minus_and_between_and_expanded_and_fake toks in
              (* assert Origin + Cocci + C and no minus *)
	      let toks = add_space toks in
	      let toks = add_newlines toks tu in
	      let toks = paren_then_brace toks in
              let toks = fix_tokens toks in
	       toks) in

          (* in theory here could reparse and rework the ast! or
           * apply some SP. Not before cos julia may have generated
           * not parsable file. Need do unparsing_tricks call before being
           * ready to reparse. *)
          print_all_tokens2 pr toks;

      | PPviastr -> pr str
    )
  )

let pp_program a b =
  Common.profile_code "C unparsing" (fun () -> pp_program2 a b)


let pp_program_default xs outfile =
  let xs' = xs +> List.map (fun x -> x, PPnormal) in
  pp_program xs' outfile
