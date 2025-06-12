(* Yoann Padioleau, Julia Lawall
 *
 * Copyright (C) 2012-2015, Inria.
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

module TH = Token_helpers

(* should keep comments and directives in between adjacent deleted terms,
but not comments and directives within deleted terms.  should use the
labels found in the control-flow graph *)

let default_indent = ref "\t"

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = mk_pr2_wrappers Flag_parsing_c.verbose_unparsing

(*****************************************************************************)
(* Types used during the intermediate phases of the unparsing *)
(*****************************************************************************)

type token1 =
  | Fake1 of Ast_c.info * Ast_c.befaft
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
  | Min of (int list (* match numbers from witness trees *) *
            Ast_cocci.adjacency (* adjacency information *))
  | Ctx

type token2 =
  | T2 of Parser_c.token * min
        * int option (* orig index, abstracting away comments and space *)
        * Unparse_cocci.nlhint option
  | Fake2 of Ast_c.info * min
  | Cocci2 of string * int (* line *) * int (* lcol *) * int (* rcol *)
            * Unparse_cocci.nlhint option
  | C2 of string * Unparse_cocci.nlhint option
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
type token_extended =
  { tok2 : token2;
    str  : string;
    idx  : int option; (* to know if 2 tokens were consecutive in orig file *)
    mutable new_tokens_before : token2 list;
    mutable remove : bool;
  }


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let info_of_token1 t =
  match t with
  | Fake1(info,_) -> info
  | T1 tok -> TH.info_of_tok tok

let print_token1 = function
  | T1 tok -> TH.str_of_tok tok
  | Fake1 _ -> "fake"

let str_of_token2 = function
  | T2 (t,_,_,_) -> TH.str_of_tok t
  | Cocci2 (s,_,_,_,_)
  | C2 (s,_)
  | Comma s -> s
  | Fake2 _
  | Indent_cocci2
  | Unindent_cocci2 _
  | EatSpace2 -> ""

let adj2c = function
    Ast_cocci.ADJ(adj) -> string_of_int adj.Ast_cocci.counter
  | _ -> "AM"

let print_token2 = function
  | T2 (t,b,_,_) ->
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
      | Min (index,adj) ->
        Printf.sprintf "-.%s[%s]" (adj2c adj)
          (String.concat " " (List.map string_of_int index))
      | Ctx -> "" in
    (*let d_str =
      let info = TH.info_of_tok t in
      match !(info.Ast_c.danger) with
	Ast_c.DangerStart -> ":DS:"
      | Ast_c.DangerEnd -> ":DE:"
      | Ast_c.Danger -> ":D:"
      | Ast_c.NoDanger -> ":ND:" in*)
    "T2:"^b_str^t_str(*^d_str*)^TH.str_of_tok t
  | Fake2 (info,b) ->
    let b_str =
      match b with
      | Min (index,adj) ->
        Printf.sprintf "-%s[%s]" (adj2c adj)
          (String.concat " " (List.map string_of_int index))
      | Ctx -> "" in
    (*let d_str =
      match !(info.Ast_c.danger) with
	Ast_c.DangerStart -> ":DS:"
      | Ast_c.DangerEnd -> ":DE:"
      | Ast_c.Danger -> ":D:"
      | Ast_c.NoDanger -> ":ND:" in*)
    b_str(*^d_str*)^"fake"
  | Cocci2 (s,_,lc,rc,_) -> Printf.sprintf "Cocci2:%d:%d%s" lc rc s
  | C2 (s,_) -> "C2:"^s
  | Comma s -> "Comma:"^s
  | Indent_cocci2 -> "Indent"
  | Unindent_cocci2 true -> "Unindent"
  | Unindent_cocci2 false -> "Unindent-false"
  | EatSpace2 -> "EatSpace"

let str_of_token3 = function
  | T3 t -> TH.str_of_tok t
  | Cocci3 s | C3 s -> s

let simple_print_all_tokens pr s l =
  Printf.printf "%s\n" s;
  List.iter (function x -> Printf.printf "|%s| " (pr x)) l;
  Printf.printf "\n"
let simple_print_all_tokens1 = simple_print_all_tokens print_token1
let simple_print_all_tokens2 = simple_print_all_tokens print_token2
let simple_print_all_tokens3 = simple_print_all_tokens str_of_token3

let mk_token_extended x =
  let origidx =
    match x with
    | T2 (_,_,idx,_) -> idx
    | _ -> None in
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
    if not tok.remove
    then push2 tok.tok2 _tokens);
  let tokens = List.rev !_tokens in
  (tokens +> List.map mk_token_extended)


let mcode_contain_plus = function
  | Ast_cocci.CONTEXT (_,Ast_cocci.NOTHING) -> false
  | Ast_cocci.CONTEXT _ -> true
  (* patch: when need full coccinelle transformation *)
  | Ast_cocci.MINUS (_,_,_,Ast_cocci.NOREPLACEMENT) -> false
  | Ast_cocci.MINUS (_,_,_,Ast_cocci.REPLACEMENT _) -> true(*REPL is not empty*)
  | Ast_cocci.PLUS _ -> raise (Impossible 132)

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
      | Ast_c.InitList args, ii ->
        (match ii with
        | [_;_] -> ini
        | i1 :: i2 :: iicommaopt :: tl when
           (not (contain_plus iicommaopt))
        && (not (contain_plus i2))
        && (Ast_c.is_fake iicommaopt) ->
          (* sometimes the guy put a normal iicommaopt *)
          Ast_c.InitList args, (i1 :: i2 :: tl)
        | ii -> Ast_c.InitList args, ii
        )
      | Ast_c.InitListNoBrace args, [iicommaopt] when
          (not (contain_plus iicommaopt))
            && (Ast_c.is_fake iicommaopt) ->
              (* sometimes the guy put a normal iicommaopt *)
              Ast_c.InitListNoBrace args, []
      | x -> x)
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
    | Ast_c.FakeTok(_,_,befaft) ->
      push2 (Fake1(info,befaft)) toks_out
    | Ast_c.OriginTok _ | Ast_c.ExpandedTok _ ->
      (* get the associated comments/space/cppcomment tokens *)
      let (before, x, after) =
        !toks_in +> split_when (fun tok ->
          info = TH.info_of_tok tok)
      in
      assert(info = TH.info_of_tok x);
      (*old: assert(before +> List.for_all (TH.is_comment)); *)
      before +> List.iter (fun x ->
        if not (TH.is_comment x)
        then
	  pr2 (Printf.sprintf "%s:%d: WEIRD: not a comment: %s"
		 (TH.file_of_tok x) (TH.line_of_tok x) (TH.str_of_tok x))
        (* case such as  int asm d3("x"); not yet in ast *)
        );
      before +> List.iter (fun x -> push2 (T1 x) toks_out);

      push2 (T1 x) toks_out;
      toks_in := after;
    | Ast_c.AbstractLineTok _ ->
      (* can be called on type info when for instance use -type_c *)
      if !Flag_parsing_c.pretty_print_type_info
      then push2 (Fake1(info,Ast_c.After)) toks_out
      else raise (Impossible 134) (* at this stage *)
  in

  let pr_space _ = () in (* use the spacing that is there already *)

  let printed_toks = ref [] in
  let pr_get_elem tok = push2 tok printed_toks in

  Pretty_print_c.pp_program_gen ~pr_elem:pr_get_elem ~pr_space:pr_space celem;

  (* sort tokens when possible *)
  let is_origin info =
    match Ast_c.pinfo_of_info info with
    | Ast_c.OriginTok _ -> true
    | _ -> false in (* don't know if the others have proper positions *)
  let rec tok_group = function
      [] -> []
    | tok::rest ->
	let (nonorig,origrest) =
	  Common.span (function tok -> not (is_origin tok)) rest in
	(tok::nonorig)::(tok_group origrest) in
  let (front,rest) =
    match tok_group (List.rev !printed_toks) with
      front::rest when not(is_origin (List.hd front)) (*might not be orig*)
	  -> (front,rest)
    | all -> ([],all) in
  let tcompare t1 t2 = compare (Ast_c.info_to_fixpos t1) (Ast_c.info_to_fixpos t2) in
  let collected_by_file =
    let rec loop cur curfile acc = function
	x::xs ->
	  let f = Ast_c.file_of_info x in
	  if f = curfile
	  then loop (x::cur) curfile acc xs
	  else
	    let cur =
	      if curfile = ""
	      then List.rev cur
	      else (List.sort tcompare cur) in
	    loop [x] f (cur :: acc) xs
      | [] ->
	  let cur =
	    if curfile = ""
	    then List.rev cur
	    else (List.sort tcompare cur) in
	  List.concat(List.rev(cur :: acc)) in
    List.map (loop [] "" []) rest in
  let printed_toks = List.concat (front :: collected_by_file) in
  List.iter pr_elem printed_toks;

  if  (!toks_in <> [])
  then failwith "WEIRD: unparsing not finished";

  List.rev !toks_out

(* Fake nodes that have BEFORE code or are - should be moved over any
subsequent whitespace and newlines, but not the last newline, to get as
close to the affected code as possible.  Similarly, fake nodes that have
AFTER code should be moved backwards.  No fake nodes should have both
before and after code. *)

let displace_fake_nodes toks =
  let is_fake = function Fake1(_,Ast_c.Before) -> true | _ -> false in
  let is_whitespace_or_noncol0_comment = function
    | T1(Parser_c.TCommentSpace _)
    (* patch: cocci    *)
    | T1(Parser_c.TCommentNewline _)
    | T1(Parser_c.TCommentCpp _) -> true
    | T1(Parser_c.TComment i) -> true
    | _ -> false in
  let rec loop toks =
    let fake_info =
      try Some (split_when is_fake toks)
      with Not_found -> None in
    match fake_info with
    | Some(bef,((Fake1(info,Ast_c.Before)) as fake),aft) ->
	(* move the fake node forwards *)
        let (whitespace,rest) = span is_whitespace_or_noncol0_comment aft in
        bef @ whitespace @ fake :: (loop rest)
    | Some _ | None -> toks in
  loop toks

(*****************************************************************************)
(* Tokens2 generation *)
(*****************************************************************************)

(* This preserves the whitespace information stored with the match of a
metavariable.  The problem is that we don't know whether this will be used
at the end of a line, in which case a newline would be added by existing
mechanisms, or in the middle of some code, in which case any newlines found
around the comments have to be preserved.  So if there is a comment, we
keep everything, and cleanup later in add_space (C2 newline followed by
Cocci2 or T2 newline) *)
let rec comment2t2 infos =
  let has_comment =
    List.exists
      (function
	  (Token_c.TCommentCpp _,_) -> true
	| _ -> false)
      infos in
  if has_comment
  then
    let rec loop = function
	[] -> []
      | (_,info)::rest -> C2(info.Common.str,None) :: loop rest in
    loop infos
  else []

let expand_mcode toks =
  let toks_out = ref [] in

  let index = ref 0 in

  let add_elem t minus =
    match t with
    | Fake1(info,_) ->
      let str = Ast_c.str_of_info info in
      let isminus = match minus with Min _ -> true | Ctx -> false in
      (* don't add fake string if the thing should be removed *)
      if str = "" || isminus
      then push2 (Fake2 (info,minus)) toks_out
      (* fx the fake "," at the end of a structure or enum.
      no idea what other fake info there can be... *)
      else push2 (Comma str) toks_out

    | T1 tok ->
      (*let (a,b) = !((TH.info_of_tok tok).cocci_tag) in*)
      (* no tag on expandedTok ! *)
      let modified = function
        | None -> false
        | Some (Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING),l) -> false
        | _ -> true in
      (if TH.is_expanded tok &&
        modified !((TH.info_of_tok tok).Ast_c.cocci_tag)
        (*!((TH.info_of_tok tok).cocci_tag) <> Ast_c.emptyAnnot*)
      then
        failwith
          (Printf.sprintf
            "expanded token %s on line %d is either modified or stored in a metavariable"
            (TH.str_of_tok tok) (TH.line_of_tok tok)));

      let tok' = tok +> TH.visitor_info_of_tok (fun i ->
        { i with Ast_c.cocci_tag = ref Ast_c.emptyAnnot; }
      ) in

      let optindex =
        if TH.is_origin tok && not (TH.is_real_comment tok)
        then
          begin
            incr index;
            Some !index
          end
        else None
      in

      push2 (T2 (tok', minus, optindex, None)) toks_out
  in

  let expand_info t =
    let (mcode,env) =
      Ast_c.mcode_and_env_of_cocciref ((info_of_token1 t).Ast_c.cocci_tag) in

    let pr_cocci s ln col rcol hint =
      push2 (Cocci2 (s,ln,col,rcol,hint)) toks_out  in
    let pr_c info =
      (match Ast_c.pinfo_of_info info with
      |	Ast_c.FakeTok (s,_,_) ->
        push2 (C2 (s,None)) toks_out
      |	_ ->
        push2 (C2 (Ast_c.str_of_info info,None)) toks_out
      );
      (* why nothing for mbefore? *)
      (Ast_c.get_comments_after info) +> comment2t2 +>
      List.iter (fun x -> push2 x toks_out) in

    let pr_barrier ln col = (* marks a position, used around C code *)
      push2 (Cocci2 ("",ln,col,col,None)) toks_out in
    let pr_nobarrier ln col = () in (* not needed for linux spacing *)

    let pr_cspace _ = push2 (C2 (" ",None)) toks_out in

    let pr_space _ = () (* rely on add_space in cocci code *) in
    let pr_arity _ = () (* not interested *) in

    let indent _ = push2 Indent_cocci2 toks_out in
    let unindent x = push2 (Unindent_cocci2 x) toks_out in
    let eat_space _ = push2 EatSpace2 toks_out in

    let args_pp =
      (env, pr_cocci, pr_c, pr_cspace,
        (match !Flag_parsing_c.spacing with
        | Flag_parsing_c.SMPL -> pr_space | _ -> pr_cspace),
        pr_arity,
        (match !Flag_parsing_c.spacing with
        | Flag_parsing_c.SMPL -> pr_barrier | _ -> pr_nobarrier),
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
      | Ast_cocci.NOREPLACEMENT -> ()
      | Ast_cocci.REPLACEMENT(any_xxs,_) ->
	  (* second argument indicates whether newline is added for statements
	     if a fake token is being replaced, there should be a newline,
	     hence use before *)
	  (match t with
	    Fake1(info,_) when !(info.Ast_c.danger) = Ast_c.DangerStart ->
	      unparser any_xxs Unparse_cocci.Before
          | _ -> unparser any_xxs Unparse_cocci.InPlace)
      )
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
    | Ast_cocci.PLUS _ -> raise (Impossible 136)
  in

  toks +> List.iter expand_info;
  List.rev !toks_out


(*****************************************************************************)
(* Tokens2 processing, filtering, adjusting *)
(*****************************************************************************)

let is_space = function
  | T2(Parser_c.TCommentSpace _,_b,_i,_h) -> true (* only whitespace *)
  | _ -> false

let is_passed = function
  | T2(Parser_c.TCommentCpp (Token_c.CppPassingCosWouldGetError,_),_b,_i,_h) -> true
  | T2(Parser_c.TCommentCpp (Token_c.CppMacro,_),_b,_i,_h) -> true
  | _ -> false

let is_comment = function
  | T2(Parser_c.TComment _,_b,_i,_h) -> true (* only whitespace *)
  | _ -> false

let is_comment_or_space = function
  | T2(Parser_c.TCommentSpace _,_b,_i,_h) -> true (* only whitespace *)
  | T2(Parser_c.TComment _,_b,_i,_h) -> true (* only whitespace *)
  | _ -> false

let is_slash_slash t =
  Str.string_match (Str.regexp_string "//") (str_of_token2 t) 0

(* // is unsafe, because it captures what follows it *)
let is_safe_comment_or_space = function
  | T2(Parser_c.TCommentSpace _,_b,_i,_h) -> true (* only whitespace *)
  | (T2(Parser_c.TComment _,_b,_i,_h)) as t -> not(is_slash_slash t)
  | _ -> false

let is_added_space = function
  | C2(" ",_) -> true (* only whitespace *)
  | _ -> false

let is_added_whitespace =
  function
      C2 (s,_) | Cocci2(s,_,_,_,_)
      when s = "" || (List.mem (String.get s 0) [' ';'\n']) -> true
    | _ -> false

let is_newline_or_comment = function
  | T2(Parser_c.TCommentNewline _,_b,_i,_h) -> true
  | T2(Parser_c.TComment _,_b,_i,_h) -> true (* only whitespace *)
  | _ -> false

let is_newline = function
  | T2(Parser_c.TCommentNewline _,_b,_i,_h) -> true
  | T2(c,_,_,_) -> TH.is_eom c
  | _ -> false

let c2newline = function
    C2(s,_) -> s <> "" && String.get s 0 = '\n' (* has a newline *)
	&& String.length (Stdcompat.String.trim s) = 1 (* just a newline *)
  | _ -> false

let cocci2newline = function
    Cocci2(s,_,_,_,_) -> s <> "" && String.get s 0 = '\n' (* has a newline *)
	&& String.length (Stdcompat.String.trim s) = 1 (* just a newline *)
  | _ -> false

let contains_newline = List.exists is_newline

let existing_or_added_newline = function
    T2((Parser_c.TCommentNewline _),Ctx,_i,_h) -> true
  | C2("",_) -> false
  | C2(s,_) -> String.get s 0 = '\n' (* may have whitespace after *)
  | Cocci2(s,_,_,_,_) ->
      (try let _ = Str.search_forward (Str.regexp "\n") s 0 in true
      with Not_found -> false)
  | _ -> false

let generated_newline_space_or_min = function
  | (T2(Parser_c.TComment _,_b,_i,_h)) as t -> not(is_slash_slash t) (* only whitespace *)
  | C2("",_) -> true
  | T2 (_, Min _, _, _) -> true
  | t -> existing_or_added_newline t

let is_fake2 = function Fake2 _ -> true | _ -> false
let is_comma = function Comma _ -> true | _ -> false

let is_whitespace x =
  is_space x || is_newline_or_comment x

let is_minusable_comment = function
  | (T2 (t,_b,_i,_h)) ->
    (match t with
    | Parser_c.TCommentSpace _   (* only whitespace *)
    (* patch: coccinelle *)
    | Parser_c.TCommentNewline _ (* newline plus whitespace *) -> true
    | Parser_c.TComment _ when !Flag_parsing_c.keep_comments -> false
    | Parser_c.TComment _
    | Parser_c.TCommentCpp (Token_c.CppAttr, _)
    | Parser_c.TCommentCpp (Token_c.CppMacro, _) -> true
    | Parser_c.TCommentCpp (Token_c.CppIfDirective _, _) -> true
    | Parser_c.TCommentCpp (Token_c.CppDirective, _) -> true
    (*
    | Parser_c.TCommentMisc _
    | Parser_c.TCommentCpp (Token_c.CppPassingCosWouldGetError, _) ->
      false
    *)
    | _ -> false
    )
  | _ -> false

let is_minusable_comment_nonl = function
  | (T2 (t,_b,_i,_h)) ->
    (match t with
    | Parser_c.TCommentSpace _ -> true (* only whitespace *)
    (* patch: coccinelle *)
    | Parser_c.TComment _ when !Flag_parsing_c.keep_comments -> false
    | Parser_c.TComment _
    | Parser_c.TCommentCpp (Token_c.CppAttr, _)
    | Parser_c.TCommentCpp (Token_c.CppMacro, _) -> true
    | Parser_c.TCommentCpp (Token_c.CppIfDirective _, _) -> true
    | Parser_c.TCommentCpp (Token_c.CppDirective, _) -> true
    (*
    | Parser_c.TCommentMisc _
    | Parser_c.TCommentCpp (Token_c.CppPassingCosWouldGetError, _) ->
      false
    *)
    | _ -> false
    )
  | _ -> false

let is_minusable_comment_nocpp = function
  | (T2 (t,_b,_i,_h)) ->
    (match t with
    | Parser_c.TCommentSpace _   (* only whitespace *)
    (* patch: coccinelle *)
    | Parser_c.TCommentNewline _ (* newline plus whitespace *) -> true
    | Parser_c.TComment _ when !Flag_parsing_c.keep_comments -> false
    | Parser_c.TComment _ -> true
    | _ -> false
    )
  | _ -> false

let all_coccis = function
  | Cocci2 _ | C2 _ | Comma _ | Indent_cocci2
  | Unindent_cocci2 _ | EatSpace2 -> true
  | _ -> false

let set_minus_comment adj = function
    (T2 (Parser_c.TComment _,Ctx,idx,hint)) as x
    when !Flag_parsing_c.keep_comments -> x
  | T2 (t,Ctx,idx,hint) ->
    let str = TH.str_of_tok t in
    (match t with
    | Parser_c.TCommentSpace _
    (* patch: coccinelle *)
    | Parser_c.TCommentNewline _ -> ()

    | Parser_c.TComment _
    | Parser_c.TCommentCpp (Token_c.CppAttr, _)
    | Parser_c.TCommentCpp (Token_c.CppMacro, _)
    | Parser_c.TCommentCpp (Token_c.CppIfDirective _, _)
    | Parser_c.TCommentCpp (Token_c.CppDirective, _) ->
      pr2 (Printf.sprintf "%d: ERASING_COMMENTS: %s"
        (TH.line_of_tok t) str)
    | _ ->
	if str <> "" (* allow removing an empty token at the end of a #define *)
	then raise (Impossible 137)
    );
    T2 (t, Min adj, idx, hint)
    (* patch: coccinelle *)
  | T2 (t, Min adj, idx, hint) as x -> x
  | Fake2 _ as x -> x
  | _ -> raise (Impossible 138)

let is_minus = function
  | T2 (_, Min _, _, _) -> true
  | _ -> false

let is_expanded = function
    T2 (t,_,_,_) when TH.is_expanded t -> true
  | _ -> false

let remove_minus_and_between_and_expanded_and_fake1 xs =
  (* get rid of expanded tok *)
  let xs = xs +> exclude is_expanded in

  let common_adj (index1,adj1) (index2,adj2) =
    let same_adj = (* same adjacency info *)
      match (adj1,adj2) with
      | (Ast_cocci.ADJ adj1,Ast_cocci.ADJ adj2) ->
	  adj1.Ast_cocci.counter = adj2.Ast_cocci.counter &&
	  not adj1.Ast_cocci.ender
      | (Ast_cocci.ALLMINUS,_) | (_,Ast_cocci.ALLMINUS) -> true in
    same_adj &&
    (* non-empty intersection of witness trees *)
    not ((inter_set index1 index2) = []) in

  (* if two minus tokens have the same adjacency, delete the non-newline whitespace
     in between. newlines require knowing whether there is added code, and will be
     dealt with later *)
  let rec adjust_around_minus = function
    | ((Fake2(_,Min adj1) | T2(_,Min adj1,_,_)) as t1)::xs ->
	let (space_or_plus,rest) =
	  Common.span (fun x -> is_whitespace x || is_passed x || all_coccis x) xs in
	let set_minus x = if not (all_coccis x) then set_minus_comment adj1 x else x in
	(match rest with
	  (Fake2(_,Min adj2) | T2(_,Min adj2,_,_))::xs2
	  when common_adj adj1 adj2 ->
	    t1 ::
	    List.map set_minus space_or_plus @
	    adjust_around_minus rest
	| [] -> t1 :: List.map set_minus space_or_plus
	| _ ->  t1 :: space_or_plus @ adjust_around_minus rest)
    | x::xs -> x :: adjust_around_minus xs
    | [] -> [] in

  let xs = adjust_around_minus xs in

  (* get rid of fake tok *)
  let xs = xs +> exclude is_fake2 in

  let rec line_end_removed = function
      x::rest when is_newline x && not(is_minus x) ->
	let onlyspaces x =
	  is_whitespace x && not (is_newline x) && not (is_minus x) in
	(* skip spaces *)
	let (spaces,rest) = Common.span onlyspaces rest in
	(* skip minus *)
	let (minus_list,rest) =
	  Common.span (fun x -> is_minus x && not (is_newline x)) rest in
	if minus_list <> []
	then
	  (* skip more spaces *)
	  let (prespaces,rest) = Common.span onlyspaces rest in
	  let minusify =
	    List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) in
	  x :: spaces @ minus_list @ minusify prespaces @
	  line_end_removed rest
	else x :: spaces @ line_end_removed rest
    | x::xs -> x :: line_end_removed xs
    | [] -> [] in

  let xs = List.rev(line_end_removed (List.rev xs)) in

  (* drop trailing spaces, working forwards *)
  let rec inner_region_removed = function
      ((T2(_,Ctx,_,_)) as x) :: ((T2(_,Min _,_,_)) as y) :: rest ->
        let (minus_list,rest) = Common.span is_minus (y::rest) in
	let minusify =
	  List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) in
	let rest = inner_region_removed rest in
	let (spaces,rest) =
	  Common.span
	    (fun x ->
	      is_whitespace x && not(is_minus x) &&
	      not(is_newline x))
	    rest in
	x :: minus_list @ minusify spaces @ rest
    | x::xs -> x :: inner_region_removed xs
    | [] -> [] in

  let xs = inner_region_removed xs in

  (* whole line removed,
  drop initial newline and preceeding comments, input is reversed *)
  let rec line_removed = function
      ((T2(c,Ctx,_,_)) as x) :: rest when is_newline x ->
	let (minus_list,rest) =
	  Common.span
	    (fun x -> is_space x || is_minus x)
	    rest in
	let contains_minus = List.exists is_minus minus_list in
	if contains_minus
	then
	  let minusify =
	    List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) in
	  let rest = line_removed rest in
	  match rest with
	    ((T2(c,Ctx,_,_)) as y) :: _ when is_newline y ->
	      let (spaces,rest) =
		Common.span is_whitespace rest in
	      let fwdspaces = List.rev spaces in
	      let (keeper,todrop) =
		Common.span (fun x -> not(is_newline x)) fwdspaces in
	      let keeper = List.rev keeper in
	      let todrop = List.rev todrop in
	      x :: minusify minus_list @ minusify todrop @ keeper @ rest
	  | [] -> x :: minusify minus_list
	  | _ -> x :: minus_list @ rest
	else x :: minus_list @ line_removed rest
    | x:: xs -> x :: line_removed xs
    | [] -> [] in

  let xs = List.rev (line_removed (List.rev xs)) in

  (* special case for the very beginning of the file that
     has no preceeding newlines *)
  let start_removed = function
      ((T2(c,Min _,_,_) :: _) as xs) ->
        let (minus_list,rest) = Common.span is_minus xs in
	let minusify =
	  List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) in
	  let (spaces,rest) =
	    Common.span
	      (fun x ->
		is_whitespace x && not(is_minus x) &&
		not(is_newline x) && not(TH.is_eom c))
	      rest in
	  minus_list @ minusify spaces @ rest
    | xs -> xs in

  let xs = start_removed xs in

  (* When we remove some lines at the beginning of a block, we should remove
     all surrounding blank lines.  Likewise when we remove lines at the end
     of a function *)
  let obrace = function
      (T2(t,Ctx,_,_)) as x -> List.mem (str_of_token2 x) ["{";"<:"]
    | _ -> false in

  (* remove newly blank lines *)
  let rec adjust_after_brace = function
      x::xs when obrace x ->
	(* keep initial spaces and minus code *)
	let (spaces,rest) =
	  Common.span
	    (fun tok -> not(is_minus tok) && (is_newline tok || is_comment_or_space tok))
	    xs in
	let (remline,rest) = Common.span is_minus rest in
	let front = spaces @ remline in
	(match rest with
	  (T2 (Parser_c.TCommentNewline _,Ctx,_i,_h)) :: _ when remline <> [] ->
	    let (newlines,rest) =
	      Common.span (fun x -> not (is_minus x) && is_newline x) rest in
	    (match List.rev newlines with
	      firstnewline::restnewlines ->
		let newlines =
		  List.rev(firstnewline :: List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) restnewlines) in
		x :: front @ newlines @ (adjust_after_brace rest)
	    | _ -> failwith "not possible")
	| _ -> x :: front @ adjust_after_brace rest)
    | x::xs -> x :: adjust_after_brace xs
    | [] -> [] in

  let xs = adjust_after_brace xs in

  (* search backwards from context } over spaces until reaching a newline.
     then go back over all minus code until reaching some context or + code.
     get rid of all intervening spaces, newlines, and comments that are alone
     on a line. input is reversed *)

  let cbrace = function
      (T2(t,Ctx,_,_)) as x -> List.mem (str_of_token2 x) ["}";":>"]
    | _ -> false in

  (* remove newly blank lines *)
  let entirely_removed xs =
    let (minlines,xs) = Common.span is_minus xs in
    minlines <> [] &&
    (match xs with
      (T2 (Parser_c.TCommentNewline _,_,_i,_h))::_ -> true
    | _ -> false) in

  let rec adjust_before_brace = function
      x::xs when cbrace x ->
	(* collect trailing newlines *)
	let (trailing,before) =
	  Common.span (fun x -> not(is_minus x) && is_newline x) xs in
	if entirely_removed before
	then
	  match trailing with
	    keep::negate ->
	      let trailing =
		keep :: List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) negate in
	      x :: trailing @ (adjust_before_brace before)
	  | _ -> x :: trailing @ (adjust_before_brace before)
	else x :: trailing @ (adjust_before_brace before)
    | x::xs -> x :: adjust_before_brace xs
    | [] -> [] in

  let revxs = adjust_before_brace (List.rev xs) in

  (* remove all blank lines before a removed newline *)
  let rec from_inner_newline = function
    | ((T2 (Parser_c.TCommentNewline _,Min adj,_i,_h)) as m) :: rest ->
	let (spaces,rest) =
	  Common.span (fun x -> is_newline x || is_minusable_comment_nocpp x) rest in
	let (keepers,spaces) =
	  Common.span (fun x -> not(is_newline x)) (List.rev spaces) in
	let keepers = List.rev keepers in
	let spaces = List.rev spaces in
	m ::
	(List.map (set_minus_comment adj) spaces) @
	keepers @ (from_inner_newline rest)
    | x::rest -> x :: from_inner_newline rest
    | [] -> [] in

  let xs = List.rev (from_inner_newline revxs) in

  (* when something is entirely removed, remove the starting newlines *)
  let remove_starting_newlines xs =
    (* before is comments trailing from the previous function *)
    let (before,xs) =
      Common.span (fun x -> not(is_newline x)) xs in
    let is_nothing = function (* trailing character in a #define *)
	T2 (t0,Ctx, idx0,h0) -> TH.str_of_tok t0 = ""
      | _ -> false in
    if List.for_all (fun x -> is_minus x || is_newline x || is_nothing x || is_minusable_comment_nocpp x) xs
    then before @ List.map (set_minus_comment ([],Ast_cocci.ALLMINUS)) xs
    else before @ xs in

  let xs = remove_starting_newlines xs in

  let cleanup_ifdefs toks =
    (* TODO: these functions are horrid, but using tokens caused circularity *)
    let is_ifdef = function
      | T2((Parser_c.TCommentCpp
        (Token_c.CppIfDirective Token_c.IfDef, _)),m,idx,_) -> true
      | T2((Parser_c.TCommentCpp
        (Token_c.CppIfDirective Token_c.IfDef0, _)),m,idx,_) -> true
      | t -> false in
    let is_else = function
      | T2((Parser_c.TCommentCpp
        (Token_c.CppIfDirective Token_c.Else, _)),m,idx,_) -> true
      | _ -> false in
    let is_endif = function
      | T2((Parser_c.TCommentCpp
        (Token_c.CppIfDirective Token_c.Endif, _)),m,idx,_) -> true
      | _ -> false in
    let add t = function
      | l::rest -> (t::l)::rest
      |	_ -> failwith "not possible" in
    let rec parse_ifdef acc_keywords acc_code stack = function
      | [] -> (None,acc_keywords,acc_code)
      | t::rest when is_else t ->
        (match stack with
        | [] -> parse_ifdef (t::acc_keywords) ([]::acc_code) stack rest
        | _ -> parse_ifdef acc_keywords (add t acc_code) stack rest
        )
      | t::rest when is_endif t ->
        (match stack with
        | [] -> ((Some (t,rest)),acc_keywords,acc_code)
        | _::stack -> parse_ifdef acc_keywords (add t acc_code) stack rest
        )
      | t::rest when is_ifdef t ->
        parse_ifdef acc_keywords (add t acc_code) (()::stack) rest
      | t::rest -> parse_ifdef acc_keywords (add t acc_code) stack rest in
    let unminus = function
      | T2 (t,Min adj,idx,hint) -> T2 (t,Ctx,idx,hint)
      | x -> x in
    let rec loop = function
      | [] -> []
      | t::rest when is_ifdef t ->
        let (ender,acc_keywords,acc_code) =
          parse_ifdef [t] [[]] [] rest in
        let acc_code = List.map loop acc_code in
        let merge = (* args reversed *)
          List.fold_left2
            (fun prev kwd code -> kwd :: (List.rev code) @ prev)
            [] in
        (match ender with
        | None -> merge (List.map unminus acc_keywords) acc_code
        | Some(endif,rest) ->
          let rest = loop rest in
          if List.for_all is_minus (endif :: acc_keywords)
          then (merge acc_keywords acc_code) @ (endif :: rest)
          else
            (merge (List.map unminus acc_keywords) acc_code) @
            ((unminus endif) :: rest)
        )
      | x::xs -> x :: loop xs in
    loop toks in

  cleanup_ifdefs xs

let remove_minus_and_between_and_expanded_and_fake2 xs =
  xs +> exclude is_minus

(* things that should not be followed by space - boundary between SmPL
code and C code *)
let adjust_eat_space toks =
  let rec loop = function
    | [] -> []
    | EatSpace2 :: x :: rest when is_space x || is_added_space x -> loop rest
    | EatSpace2 :: rest -> loop rest
    | x :: xs -> x :: loop xs in
  loop toks

(* normally, in C code, a semicolon is not preceded by a space or newline *)
let adjust_before_semicolon toks =
  let toks = List.rev toks in
  let rec search_semic = function
    | [] -> []
    | ((T2(_,Ctx,_,_) | Cocci2 _) as x)::xs
      when List.mem (str_of_token2 x) [";";")";","] ->
      x :: search_semic (search_minus false xs)
    | x::xs -> x :: search_semic xs
  and search_minus seen_minus xs =
    (* drop spaces added by cocci, eg after attribute *)
    let (_, xs) = span is_added_space xs in
    let (spaces, rest) = span is_space xs in
    (* only delete spaces if something is actually deleted *)
    match rest with
    | ((T2(_,Min _,_,_)) as a)::rerest -> a :: search_minus true rerest
    | _ -> if seen_minus then rest else xs in
  List.rev (search_semic toks)

(* normally, in C code, a ( is not followed by a space or newline *)
let adjust_after_paren toks =
  let rec search_paren = function
    | [] -> []
    | ((T2(_,Ctx,_,_) | Cocci2 _) as x)::xs
      when List.mem (str_of_token2 x) ["("] (* other things? *) ->
      x :: search_paren (search_minus false xs)
    | x::xs -> x :: search_paren xs
  and search_minus seen_minus xs =
    let (spaces, rest) = span is_whitespace xs in
    (* only delete spaces if something is actually deleted *)
    match rest with
    | ((T2(_,Min _,_,_)) as a)::rerest -> (* minus *)
      a :: search_minus true rerest
    | ((T2(_,Ctx,_,_)) as a)::rerest
      when seen_minus && str_of_token2 a = "," ->
      (* comma after ( will be deleted, so consider it as minus code
      already *)
      a :: search_minus true rerest
    | _ -> if seen_minus then rest else xs in (* drop trailing space *)
  search_paren toks

(* Danger is related to tokens that are shared between multiple AST
representations, in particular for multidecls.  This attempts to clean up
after any transformations that have been made.  It may not work in all
cases... *)
let check_danger toks =
  let get_danger = function
      T2(tok,_,_,_) ->
	let info = TH.info_of_tok tok in
	Some !(info.Ast_c.danger)
    | Fake2(info,_) ->
	Some !(info.Ast_c.danger)
    | _ -> None in
  let is_danger tok =
    match get_danger tok with
      Some Ast_c.Danger -> true
    | _ -> false in
  let isnt_danger_end tok =
    match get_danger tok with
      Some Ast_c.DangerEnd -> false
    | _ -> true in
  let is_comma tok = (str_of_token2 tok) = "," in
  let removed_or_comma = function
      T2(_,Min _,_,_) -> true
    | (T2(tok,Ctx,_,_)) as x ->
	TH.str_of_tok tok = "," || is_whitespace x
    | Fake2(info,_) -> true
    | C2 _ | Cocci2 _ -> true (* ignore added code *)
    | x -> false in
  let rec undanger_untouched toks =
    (* check that each entry before or after a comma contains at least
       one context token. combined with safe for multi constraints, that
       means that the rule can only have changed the type *)
    let ctx =
      function
	  (T2(_,Ctx,_,_) as t) when not(is_danger t) -> not (is_whitespace t)
	| _ -> false in
    let safe = function [] -> true | toks -> List.exists ctx toks in
    let res =
      try Some (Common.split_when is_comma toks)
      with Not_found -> None in
    match res with
      Some (bef,_,aft) -> safe bef && undanger_untouched aft
    | None -> safe toks in
  let drop_danger_commas toks =
    (* convert to minus a context comma that is at the end of minused
       nondangers or spaces, preceded by a danger of any sort *)
    let isnt_danger_or_end tok =
      match (get_danger tok) with
	Some Ast_c.DangerEnd -> false
      | Some Ast_c.Danger -> false
      | _ -> is_minus tok || is_comment_or_space tok || is_newline tok in
    let rec loop = function
	[] -> []
      |	x::xs ->
	  match get_danger x with
	    Some Ast_c.Danger ->
	      let (nodanger,rest) = span isnt_danger_or_end xs in
	      (match (nodanger,rest) with
		(_,[]) -> x::xs
	      | ([],_) -> x:: loop xs (* still in danger region *)
	      |	(_,y::ys) ->
		  (match (y,get_danger y) with
		    (T2(tok,Ctx,a,b), Some Ast_c.Danger) when is_comma y ->
		      let rec find_minus = function
			  [] -> None
			| (T2(_,Min m,_,_)) :: _
			| (Fake2(_,Min m)) :: _ -> Some m
			| x::xs -> find_minus xs in
                      (match find_minus (List.rev nodanger) with
			Some m ->
			  x::nodanger@(loop(T2(tok,Min m,a,b)::ys))
		      |	None -> failwith "no way to minus")
		  | _ -> x::loop xs))
	  | _ -> x :: loop xs in
    loop toks in
  let drop_last_danger_comma toks = (* avoid comma before ; if not all gone *)
    let indanger_and_isminus_or_space tok =
      match (get_danger tok) with
	Some Ast_c.DangerStart -> false
      | _ -> is_minus tok || is_comment_or_space tok || is_newline tok in
    let rec loop = function
	[] -> []
      |	x::xs ->
	  (match get_danger x with
	    Some Ast_c.DangerEnd ->
	      let (removed,rest) = span indanger_and_isminus_or_space xs in
	      (match rest with
		[] -> x::xs
	      |	y::ys ->
		  (match (y,get_danger y) with
		    (T2(tok,Ctx,a,b), Some Ast_c.Danger) when is_comma y ->
		      let rec find_minus = function
			  [] -> None
			| (T2(_,Min m,_,_)) :: _
			| (Fake2(_,Min m)) :: _ -> Some m
			| x::xs -> find_minus xs in
                      (match find_minus removed with
			Some m ->
			  x :: removed @ (T2(tok,Min m,a,b)) :: loop ys
		      |	None -> failwith "no way to minus")
		  | _ -> x::loop xs))
	  | _ -> x :: loop xs) in
    (* reverse to find danger end first, and then work backwards *)
    List.rev (loop (List.rev toks)) in
  (* the following four functions are for unminusing the type if it is still
     needed *)
  let not_danger tok =
    match get_danger tok with
      Some Ast_c.Danger -> false
    | _ -> true in
  let nonspace_danger tok =
    match get_danger tok with
      Some Ast_c.Danger -> true
    | Some Ast_c.NoDanger -> is_space tok || is_newline_or_comment tok
    | _ -> false in
  let unminus tok =
    match (tok,get_danger tok) with
      (T2(tok,Min _,a,b),Some Ast_c.Danger) -> T2(tok,Ctx,a,b)
    | _ -> tok in
  let unminus_initial_danger toks =
    let (front,rest) = span not_danger toks in
    let (dangers,rest) = span nonspace_danger rest in
    front @ (List.map unminus dangers) @ rest in
  let unminus_danger_end = function
      T2(tok,Min _,a,b) -> T2(tok,Ctx,a,b)
    | x -> x in
  let reminus_danger_end m = function
      (* remove space inside a danger that can be removed, but is not
	 allminus, because some code is attached ; *)
      (T2(tok,Ctx,a,b)) as t when is_whitespace t ->
	T2(tok,Min m,a,b)
    | t -> t in
  let rec search_danger = function
      [] -> []
    | x::xs ->
	match get_danger x with
	  Some Ast_c.DangerStart ->
	    let (danger,rest) = span isnt_danger_end (x::xs) in
	    (match rest with
	      de::rest ->
		(match get_danger de with
		  Some Ast_c.DangerEnd ->
		    if List.for_all removed_or_comma (de::danger)
			(* everything removed *)
		    then
		      (* drop spaces and final newline *)
		      let m =
			match de with
			  T2(tok,Min m,a,b) -> m
			| _ -> failwith "should be min" in
		      let (nl,rest) =
			match rest with
			  ((T2(tok,Ctx,a,b)) as t)::rest when is_newline t ->
			    ([T2(tok,Min m,a,b)],rest)
			| _ -> ([],rest) in
		      (List.map (reminus_danger_end m) danger)
		      @ de :: (search_danger rest)
		    else if undanger_untouched (danger@[de])
			(* nothing removed, type changed *)
		    then danger @ de :: (search_danger rest)
		    else
		      (* some things removed, not others, unminus the type *)
		      (* if next token is added, will need a newline *)
		      let rest_with_nl =
			match rest with
			  t::_ when all_coccis t ->
			    (Cocci2("\n",-1,-1,-1,None)) :: rest
			|_ -> rest in
		      drop_last_danger_comma
			((unminus_initial_danger danger) @
			 [(unminus_danger_end de)]) @
		      (search_danger rest_with_nl)
		| _ -> failwith "missing danger end")
	    | _ -> failwith "missing danger end")
	| _ -> x :: search_danger xs in
  search_danger (drop_danger_commas toks)

(* // should not be followed by a non-newline existing token *)
let fix_slash_slash toks =
  let rec loop acc = function
      t0::rest when is_slash_slash t0 ->
	(match Common.drop_while is_minus rest with
	  [] -> loop (t0::acc) rest
	| t1::_ when existing_or_added_newline t1 -> loop (t0::acc) rest
	| _ -> loop (C2("\n",None)::(t0::acc)) rest)
    | x::xs -> loop (x::acc) xs
    | [] -> List.rev acc in
  loop [] toks

(* this is for the case where braces are added around an if branch
because of a change inside the branch *)
let minusify = function
    T2(t,_,i,h) -> T2(t,Min([],Ast_cocci.ALLMINUS),i,h)
  | _ -> failwith "not possible" (* see is_newline, below *)

let paren_then_brace toks =
  let rec search_paren = function
    | [] -> []
    | ((T2(_,Ctx,_,_)) as x)::xs
      when List.mem (str_of_token2 x) [")";"else"] ->
      x :: search_paren (search_plus xs)
    | x::xs -> x :: search_paren xs
  and search_plus xs =
    let (spaces, rest) = span generated_newline_space_or_min xs in
    match rest with
    | ((Cocci2(lb,_,_,_,_)) as x) :: ((Cocci2 (s,_,_,_,_)) as a) :: after
      when List.mem lb ["{";"<:"] && s <> "" && String.get s 0 = '\n' ->
	(* move the brace up to the previous line *)
	(* if there is a newline with indentation just before the {,
	   then we want to move the { before that, to benefit from the
	   correct indentation *)
	(match Common.drop_while is_minus (List.rev spaces) with
	  [] -> (C2 (" ",None)) :: rest
	| t1 :: _ ->
	    if existing_or_added_newline t1
	    then (C2 (" ",None)) :: x :: spaces @ after (* don't need a *)
	    else (C2 (" ",None)) :: x :: a :: spaces @ after)
    | _ -> xs in
  search_paren toks

let newline_before_else toks =
  let rec loop = function (* reversed toks *)
      ((Cocci2("else",ln,lcol,rcol,hint)) as e)::rest ->
	let (minus_space_list,rest) =
	  Common.span (fun x -> is_minus x || is_space x) rest in
	(match rest with
	  (T2(Parser_c.TCommentNewline _,Ctx,_,_)) :: _ ->
	    e :: minus_space_list @ loop rest
	| (T2(Parser_c.TCBrace _,Ctx,_,_)) :: _ ->
	    e :: minus_space_list @ loop rest
	| ((T2 _) as x) :: rest ->
	    e :: minus_space_list @ (C2("\n",None)) :: x :: loop rest
	| _ -> e :: minus_space_list @ loop rest)
    | x :: xs -> x :: loop xs
    | [] -> [] in
  List.rev(loop(List.rev toks))

let is_ident_like s = s ==~ regexp_alpha
let is_int_like s = s ==~ regexp_int

let isop s =
  List.mem s
    ["=";"+";"-";"*";"/";"%";"^";"+=";"-=";"*=";"/=";"%=";"^=";":";"?";
      "==";"!=";"<";">";"<=";">=";
      "<?";">?";"<<";">>";"<?=";">?=";"<<=";">>=";
      "&&";"||";"&";"|"]

let space_after = function
    [] -> true
  | x::xs -> is_newline x || is_space x || is_added_whitespace x      

(* comment2c2 includes comments/newlines/ifdefs after tokens, but
we don't want them at the end of something.
Find a noncomment C2 token followed by comment C2 tokens, followed
by context T2 whitespace or Cocci2 whitespace *)
let cleanup_comment_trailers toks =
  let nonc2_whitespace toks =
    let min_or_space t = is_minus t || is_space t in
    match Common.drop_while min_or_space toks with
      t::_ -> is_newline t || cocci2newline t
    | _ -> false in
  let c2whitespace = function
      C2 (s,_) ->
	s = ""
      || List.mem (String.get s 0) ['\n';' ';'\t';'#']
      || (String.length s > 1 && List.mem (String.sub s 0 2) ["//";"/*"])
      | _ -> false in
  let notnl = function
      C2 (s,_) -> not(s <> "" && String.get s 0 = '\n')
    | _ -> failwith "impossible" in
  let rec loop acc = function
      [] -> List.rev acc
    | ((C2 _) as t)::xs when not(c2whitespace t) ->
	let (ws,rest) = Common.span c2whitespace xs in
	let (keep,drop) = Common.span notnl ws in
	let ok = nonc2_whitespace rest in
	if ok || drop = []
	then loop ((List.rev (t :: keep)) @ acc) rest
	else
	  (* keep a newline *)
	  loop ((List.rev (t :: (keep @ [List.hd drop]))) @ acc) rest
    | x::xs -> loop (x::acc) xs in
  loop [] toks

let rec drop_space_at_endline = function
  | [] -> []
  | [x] -> [x]
  | (C2 (" ",_)) ::
    ((Cocci2(str,_,_,_,_) :: _) as rest)
    when not (str = "") && String.get str 0 = '\n' ->
    (* when unparse_cocci doesn't know whether space is needed *)
    drop_space_at_endline rest
  | (C2 (" ",_)) ::
    ((((T2(Parser_c.TCommentSpace _,Ctx,_,_)) |
    (T2(Parser_c.TCommentNewline _,Ctx,_,_))) :: _) as rest) ->
    (* when unparse_cocci doesn't know whether space is needed *)
    drop_space_at_endline rest
  | ((T2(Parser_c.TCommentSpace _,Ctx,_i,_h)) as a)::rest ->
    let (outer_spaces,rest) = span is_space rest in
    let match_till_context_nl = function
      | (T2(Parser_c.TCommentNewline _,_,_i,_) :: _) -> false
      | (T2(_,Min adj,_,_) :: _) -> true
      | x -> false in
    if match_till_context_nl rest
    then
      let minus_or_comment_or_space_nocpp = function
        | (T2(Parser_c.TCommentNewline _,Ctx,_i,_)) -> false
        | T2(_,Min adj,_,_) -> true
        | (T2(Parser_c.TCommentSpace _,Ctx,_i,_)) -> true
        | x -> false in
      let (minus,rest) = span minus_or_comment_or_space_nocpp rest in
      let fail _ = a :: outer_spaces @ minus @ (drop_space_at_endline rest) in
      if List.exists is_minus minus
      then
        match rest with
        | ((T2(Parser_c.TCommentNewline _,Ctx,_i,_h)) as a)::rest ->
          (* drop trailing spaces *)
          minus @ a :: (drop_space_at_endline rest)
        | _ -> fail ()
      else fail ()
    else a :: outer_spaces @ (drop_space_at_endline rest)
  | a :: rest ->
    a :: drop_space_at_endline rest

(* if a removed ( is between two tokens, but not just after (,
   then add a space *)
let rec paren_to_space = function
  | [] -> []
  | [x] -> [x]
  | [x;y] -> [x;y]
  | ((T2(t1,Ctx,_,_)) as a)::
    ((T2(t2,Min _,_,_)) as b)::
    ((T2(_,Ctx,_,_)) as c)::rest
    when not (is_whitespace a) && not(TH.str_of_tok t1 = "(") &&
      TH.str_of_tok t2 = "(" ->
    a :: b :: (C2 (" ",None)) :: (paren_to_space (c :: rest))
  | a :: rest -> a :: (paren_to_space rest)

let rec add_space xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | (Cocci2(sx,lnx,_,rcolx,_) as x)::((Cocci2(sy,lny,lcoly,_,_)) as y)::xs
    when !Flag_parsing_c.spacing = Flag_parsing_c.SMPL &&
    not (lnx = -1) && not (rcolx = -1) && lnx = lny && rcolx < lcoly ->
    (* this only works within a line.  could consider whether
    something should be done to add newlines too, rather than
    printing them explicitly in unparse_cocci. *)
    x::C2 (String.make (lcoly-rcolx) ' ', None)::add_space (y::xs)
  | (Cocci2(sx,lnx,_,rcolx,_) as x)::((Cocci2(sy,lny,lcoly,_,_)) as y)::xs
    when !Flag_parsing_c.spacing = Flag_parsing_c.SMPL &&
    not (lnx = -1) && not (rcolx = -1) && lnx < lny ->
    (* this only works within a line.  could consider whether
    something should be done to add newlines too, rather than
    printing them explicitly in unparse_cocci. *)
    x::C2 (String.make (lny-lnx) '\n', None)::
    C2 (String.make (lcoly-1) ' ', None):: (* -1 is for the + *)
    add_space (y::xs)
  | ((T2(_,Ctx,_,_)) as x)::(((Cocci2 _) | C2 _) as y)::xs
    when str_of_token2 x = "," && not(is_added_whitespace y) ->
      x::C2(" ",None)::(add_space (y::xs))
  | ((T2(_,Ctx,_,_)) as x)::(((Cocci2 _) | C2 _) as y)::xs
  | (((Cocci2 _) | C2 _) as x)::((T2(_,Ctx,_,_)) as y)::xs ->
    (* add space on boundary *)
    let sx = str_of_token2 x in
    let sy = str_of_token2 y in
    if (is_ident_like sx || List.mem sx [")";"]"]) &&
      (is_ident_like sy || (isop sy && space_after xs) || is_int_like sy)
    then x::C2(" ",None)::(add_space (y::xs))
    else x::(add_space (y::xs))
  | ((T2(_,Ctx,_,_)) as x)::((T2(_,Ctx,_,_)) as y)::xs -> (* don't touch *)
      x :: (add_space (y :: xs))
  | x::y::xs -> (* not boundary, not sure if it is possible *)
    let sx = str_of_token2 x in
    let sy = str_of_token2 y in
    if is_ident_like sx && is_ident_like sy
    then x::C2(" ",None)::(add_space (y::xs))
    else x::(add_space (y::xs))

(* A fake comma is added at the end of an unordered initlist or a enum
decl, if the initlist or enum doesn't already end in a comma.  This is only
needed if there is + code, ie if we see Cocci after it in the code sequence *)

let rec drop_end_comma = function
  | [] -> []
  | [x] -> [x]
  | ((Comma ",") as x) :: rest ->
    let (newlines,rest2) = span is_whitespace rest in
    (match rest2 with
    | (Cocci2 _) :: _ -> x :: drop_end_comma rest
    | (C2 _) :: _ -> x :: drop_end_comma rest
    |	_ -> drop_end_comma rest
    )
  | x :: xs -> x :: drop_end_comma xs

(* add_newlines only works for the outermost function call.  Stack records
the column of all open parentheses.  Space_cell contains the most recent
comma in the outermost function call.  The goal is to decide whether this
should be followed by a space or a newline and indent. *)
let string_length s count =
  (* drops both space_cell and seen_cocci if there is a newline *)
  let l = list_of_string s in
  List.fold_left
    (function (count,nl) ->
      function
        | '\t' -> (count + 8,nl)
        | '\n' -> (0,true)
        | c -> (count + 1,nl))
    (count,false) l
  (*don't care about seen cocci - know no newline is possible, or don't care*)
let simple_string_length s count = fst(string_length s count)

let scan_past_define l =
  let is_newline = function
      T2(Parser_c.TCommentNewline _,_b,_i,_h) -> true
    | C2 ("\n",_) -> true
    | _ -> false in
  let rec loop = function
      [] -> ([],[])
    | x::xs when str_of_token2 x = "\\" ->
	let (notnewline,nl,after) =
	  xs +> split_when is_newline in
	let (before,after) = loop after in
	(notnewline @ nl :: before, after)
    | x::xs when is_newline x -> ([x],xs)
    | x::xs ->
	let (before,after) = loop xs in
	(x::before,after) in
  loop l

(* This is the last thing that happens, so there is no more minus code at
this point.  The cases are as follows:

1. Comma then space.
2. Comma then newline.
3. Comma then something else.
4. Newline with no preceding comma.
5. Start or end token, ie = ( ) { }.
6. Anything else.

If the position of anythng has to move, then there can be movement on the
rest of the current line.  If a newline adds less space than the indentation
at the top of the stack, then the space should be adjusted to what is indicated
at the top of the stack, unless the next character is ) or }.  Indentation
is done like on the previous line.  An overload at a comma causes the previous
space to be replaced by a newline.

Stack contains an indentation level and a tabbing unit. *)

let add_newlines toks tabbing_unit =
  let iscomma instring tok = not instring && str_of_token2 tok = "," in
  let isquote tok = str_of_token2 tok = "\"" in
  let isspace tok = str_of_token2 tok = " " in
  let isnewline tok =
    not(is_comment tok) &&
    let s = str_of_token2 tok in
    (* backslash newline, not a real newline *)
    try let _ = Str.search_forward (Str.regexp "\\\\[ \t]*\n") s 0 in false
    with Not_found ->
      try let _ = Str.search_forward (Str.regexp "\n") s 0 in true
      with Not_found -> false in
  let iscocci = function Cocci2 _ | C2 _ -> true | _ -> false in
  let nonempty = function [] -> false | _ -> true in
  (* the following is for strings that may contain newline or tabs *)
  let create_indent n =
    let (tu,tlen) =
      match tabbing_unit with
      | Some "\t" -> ("\t",8)
	| None | Some "" ->
	  if !Flag_parsing_c.indent = 0
	  then ("\t",8) (* not sure why "" case... *)
	  else (!default_indent,!Flag_parsing_c.indent)
      | Some s -> (* assuming only tabs or spaces *)
	  (s,simple_string_length s 0) in
    let rec loop seen =
      if seen + tlen <= n
      then tu ^ loop (seen + tlen)
      else String.make (n-seen) ' ' in
    loop 0 in
  let update_previous_space stack space_cell count s =
    let count = count + String.length s in
    match (stack,space_cell) with
      ([(indent, tu)], Some (start,space_cell)) ->
	if count > !Flag_parsing_c.max_width
	then
	  let indent =
	    match tu with
	      Some tu -> space_cell := "\n"^tu; indent
	    | None -> space_cell := "\n"^(create_indent indent); indent in
	  count - start + indent
	else count
    | _ -> count in
  let make_space_cell count sp =
    let count = count + String.length sp in
    let sp = ref sp in
    let a = C2(",",Some (Unparse_cocci.SpaceOrNewline sp)) in
    (a,Some (count,sp)) in
  let ender = function
      [] -> false
    | Cocci2(s,line,lcol,rcol,Some Unparse_cocci.EndBox)::_ -> true
    | t::_ -> List.mem (str_of_token2 t) [")";"}";":>"] in
  let get_indent stack t2 seen_cocci rest_toks =
    match stack with
      (indent_count,indent_string)::rest_stack ->
	let (new_indent,rest) =
	  let rpieces =
	    List.rev (Str.split_delim (Str.regexp "\n") (str_of_token2 t2)) in
	  match rpieces with
            indent::rest -> (indent,String.concat "\n" (List.rev rest))
	  |  [] -> ("","") (* no indentation seems desired *) in
	let new_indent_count = simple_string_length new_indent 0 in
	if new_indent_count = 0
	then (* no indentation wanted; leave it that way *)
	  (* false is not ideal, because it means that for subsequent lines
	     we will have forgotten any previous changes. *)
	  (stack,indent_count,t2,false)
	else
	if seen_cocci && not (ender rest_toks)
	then
	  (match indent_string with
	    Some indent_string -> (*if we have picked something, then use it*)
	      (stack,indent_count,C2(rest^"\n"^indent_string,None),
	       not (new_indent_count = indent_count))
	  | None ->
	      if new_indent_count >= indent_count
	      then
		((new_indent_count,Some new_indent)::rest_stack,
		 new_indent_count, t2, false)
	      else
		let new_indent_string = create_indent indent_count in
		((indent_count,Some new_indent_string)::rest_stack,
		 indent_count, C2(rest^"\n"^new_indent_string,None), true))
	else
	  ((new_indent_count,Some new_indent)::rest_stack,
	   new_indent_count, t2, false)
    | _ -> failwith "should not be called with an empty stack" in
  let start_box stack count s =
    let inside_count = String.length s + count in
    (* using 0 should cause it to stay with what there is already *)
    let stack_count = if s = "(" then inside_count else 0 in
    (inside_count,(stack_count,None)::stack) in
  let end_box stack space_cell count seen_cocci s =
    (* this assumes that start_box and end_box are matched, but this is not
    necessarily the case, if ( is modified and ) is context code *)
    match stack with
      [_] ->
	let newcount =
	  if seen_cocci
	  then update_previous_space stack space_cell count s
	  else count + String.length s in
	(newcount,[],None,false)
    | [] -> (count + String.length s, [], None, false)
    | x::xs ->
	(count + String.length s, xs, space_cell, seen_cocci) in
  let rec loop stack instring space_cell count seen_cocci seeneq =
    function x ->
      match x with
    | [] -> []
    | t1::rest when isquote t1 ->
	t1 :: loop stack (not instring) space_cell count seen_cocci seeneq rest
    | t1::rest
      when str_of_token2 t1 = "#define" ->
	(* don't want to add newlines in a #define *)
	let (def,rest) = scan_past_define rest in
	let nl = List.hd(List.rev def) in
	(match stack with
	  [] ->
	    let newcount = simple_string_length (str_of_token2 nl) 0 in
	    t1 :: def @ loop stack instring space_cell newcount false false rest
	| _ ->
	    let def = List.rev(List.tl(List.rev def)) in
	    let (stack,newcount,nl,newseencocci) =
	      get_indent stack nl seen_cocci rest in
	    t1 :: def @ nl ::
	    (loop stack instring space_cell newcount newseencocci false rest))
    | ((Cocci2(s,line,lcol,rcol,Some Unparse_cocci.StartBox)) as a)::xs ->
        let (newcount,newstack) = start_box stack count s in
        a :: loop newstack instring space_cell newcount true false xs
    | ((Cocci2(s,line,lcol,rcol,Some Unparse_cocci.EndBox)) as a)::xs ->
        let (newcount,newstack,newspacecell,newseencocci) =
          end_box stack space_cell count seen_cocci s in
        a :: loop newstack instring newspacecell newcount newseencocci false xs
    | t1 :: t2 :: rest
      when nonempty stack && iscomma instring t1 && isnewline t2 ->
	let seen_cocci = seen_cocci || iscocci t1 || iscocci t2 in
	let (stack,newcount,t2,newseencocci) =
	  get_indent stack t2 seen_cocci rest in
	let t1 =
	  match t1 with
	    Cocci2(s,line,lcol,rcol,Some(Unparse_cocci.SpaceOrNewline sp)) ->
	      (* no need for extra space, because we have a newline *)
	      Cocci2(s,line,lcol,rcol,None)
	  | t1 -> t1 in
	(match stack with
	  [_] ->
	    let _count =
	      if seen_cocci
	      then update_previous_space stack space_cell count ","
	      else count+1 in
	    t1::t2::loop stack instring None newcount newseencocci false rest
	| _ -> t1::t2::loop stack instring None newcount newseencocci false rest)
    | t1 :: t2 :: rest
      when iscomma instring t1 && isnewline t2 ->
	(* nothing to do in this case *)
	let t1 =
	  match t1 with
	    Cocci2(s,line,lcol,rcol,Some(Unparse_cocci.SpaceOrNewline sp)) ->
	      (* no need for extra space, because we have a newline *)
	      Cocci2(s,line,lcol,rcol,None)
	  | t1 -> t1 in
	(* 0 for second arg of string_length - because of nl, value doesn't
	   matter *)
	let (newcount,sawnl) = string_length (str_of_token2 t2) 0 in
	let newseencocci = seen_cocci && (not sawnl) in
	t1::t2::loop stack instring None newcount newseencocci false rest
    | ((Cocci2(s,line,lcol,rcol,Some(Unparse_cocci.SpaceOrNewline sp))) as a)::
      xs ->
	let xs =
	  match xs with
	    t1::xs when isspace t1 -> xs
	  | _ -> xs in
	let count = update_previous_space stack space_cell count s in
	let count_after_space = count + 1 in
	let space_cell =
	  match stack with
	    [_] -> Some(count_after_space,sp)
	  | _ -> space_cell in
	a :: loop stack instring space_cell count_after_space true false xs
    | t1 :: t2 :: rest when nonempty stack && iscomma instring t1 && isspace t2 ->
	let seen_cocci = seen_cocci || iscocci t1 || iscocci t2 in
	let space_sz = simple_string_length (str_of_token2 t2) 0 in
	(match stack with
	  [_] ->
	    let count =
	      if seen_cocci
	      then update_previous_space stack space_cell count ","
	      else count+1 in
	    let (t1,space_cell) = make_space_cell count " " in
	    t1::
	    loop stack instring space_cell (count+space_sz) seen_cocci false rest
	| _ ->
	    t1::t2::
	    loop stack instring space_cell (count+space_sz+1) seen_cocci false rest)
    | t1 :: rest
      when nonempty stack && iscomma instring t1 ->
	let seen_cocci = seen_cocci || iscocci t1 in
	(match stack with
	  [_] ->
	    let count =
	      if seen_cocci
	      then update_previous_space stack space_cell count ","
	      else count+1 in
	    let (t1,space_cell) = make_space_cell count "" in
	    t1 :: loop stack instring space_cell (count) seen_cocci false rest
	| _ -> t1 :: loop stack instring space_cell (count+1) seen_cocci false rest)
    | t1 :: rest
      when nonempty stack && isnewline t1 ->
	let seen_cocci = seen_cocci || iscocci t1 in
	let (stack,newcount,t1,newseencocci) =
	  get_indent stack t1 seen_cocci rest in
	(match stack with
	  [_] ->
	    let _count =
	      if seen_cocci
	      then update_previous_space stack space_cell count ""
	      else count+1 in
	    t1 :: loop stack instring None newcount newseencocci false rest
	| _ -> t1 :: loop stack instring None newcount newseencocci false rest)
    | (C2(s1,_)) :: (C2(" ",_)) :: (((C2(s2,_)) :: _) as xs)
      when (not (s1 = "")) && (not (s2 = "")) &&
	(* not perfect, because only finds the string string case *)
	(String.get s1 0 = '\"') && (String.get s2 0 = '\"') ->
        let count =
	  update_previous_space stack space_cell count s1 in
	let count_after_space = count + 1 in
	let sp = ref " " in
	let a = C2(s1,Some(Unparse_cocci.SpaceOrNewline sp)) in
	let space_cell =
          match stack with
            [_] -> Some(count_after_space,sp)
          | _ -> space_cell in
	a ::
	loop stack instring space_cell count_after_space true false xs
    | Fake2 _ :: _ | Indent_cocci2 :: _
    | Unindent_cocci2 _::_ | EatSpace2::_ ->
      failwith "unexpected fake, indent, unindent, or eatspace"
    | a::xs ->
      (match str_of_token2 a with
      | "=" -> a :: loop stack instring space_cell (count+1) seen_cocci true xs
      | "(" as s ->
        let (newcount,newstack) = start_box stack count s in
        a :: loop newstack instring space_cell newcount seen_cocci false xs
      | ")" as s ->
        let (newcount,newstack,newspacecell,newseencocci) =
          end_box stack space_cell count seen_cocci s in
        a :: loop newstack instring newspacecell newcount newseencocci false xs
      | s when List.mem s ["{";"<:"] && seeneq ->
	  let (spaces_after,_) = span is_whitespace xs in
	  let (newcount,nl) =
	    List.fold_left
              (function (prev,info) ->
		function
		  | (T2(tok,_b,_i,_h)) ->
		      string_length (TH.str_of_tok tok) prev
		  | _ -> failwith "not possible")
              (count,false) spaces_after in
	  let s = if nl then "" else s in
          let (newcount,newstack) = start_box stack count s in
          a :: loop newstack instring space_cell newcount seen_cocci false xs
      | s when List.mem s ["{";"<:"] && not (stack = []) ->
        (* [] case means statement braces *)
        let (newcount,newstack) = start_box stack count s in
        a :: loop newstack instring space_cell newcount seen_cocci false xs
      | s when List.mem s ["}";":>"] && not (stack = []) ->
        (* [] case means statement braces *)
        let (newcount,newstack,newspacecell,newseencocci) =
          end_box stack space_cell count seen_cocci s in
        a :: loop newstack instring newspacecell newcount newseencocci false xs
      | s ->
	  let count = simple_string_length s count in
	  let seeneq = seeneq && is_whitespace a in
	  let seen_cocci = seen_cocci || (iscocci a && nonempty stack) in
	  a :: loop stack instring space_cell count seen_cocci seeneq xs) in
  let mkc2 = function "" -> [] | sp -> [C2 (sp,None)] in
  let redo_spaces prev = function
    | Cocci2(s,line,lcol,rcol,Some (Unparse_cocci.SpaceOrNewline sp)) ->
      mkc2 !sp @ Cocci2(s,line,lcol,rcol,None) :: prev
    | T2(tok,min,idx,Some (Unparse_cocci.SpaceOrNewline sp)) ->
      mkc2 !sp @ T2(tok,min,idx,None) :: prev
    | C2(s,Some (Unparse_cocci.SpaceOrNewline sp)) ->
      mkc2 !sp @ C2(s,None) :: prev
    | t -> t::prev in
  (match !Flag_parsing_c.spacing with
  | Flag_parsing_c.SMPL -> toks
  | _ ->
      let preres = loop [] false None 0 false false toks in
      List.rev (List.fold_left redo_spaces [] preres)
  )

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
  list_of_string space
    +> List.rev
    +> take_until (fun c -> c = '\n')
    +> List.rev
    +> List.map string_of_char
    +> String.concat ""

let new_tabbing a =
  profile_code "C unparsing.new_tabbing" (fun () -> new_tabbing2 a)

(* ------------------------------------------------------------------------ *)

type info =
    CtxNL of string * int (*depthmin*) * int (*depthplus*) * int (*inparen*)
  | MinNL of string * int (*depthmin*) * int (*depthplus*) * int (*inparen*)
  | PlusNL of int (* depthplus *) * int (* inparen *)
  | Other of int | Drop | Unindent | Unindent1
  | Label (* label is for a newline that should not be taken into account
	     to compute indentation; it might be before a label, a #, or
	     just an empty line *)

let newlineNL = function
    CtxNL _ -> true
  | PlusNL _ -> true
  | Label -> true
  | _ -> false

(* ------------------------------------------------------------------------- *)
(* preparsing *)

type op = PlusOnly | MinusOnly | Both | Neither

let minplus = function
    Ctx -> Both
  | Min _ -> MinusOnly

type parsed_tokens = NL of string | Tok of string | Ind of token2

let parse_token tok =
  match tok with
    T2((Parser_c.TCommentNewline s) as t,a,_,_) ->
      let s = TH.str_of_tok t in
      (match Str.split_delim (Str.regexp "\n") s with
	[before;after] -> (NL after, minplus a)
      |	_ -> (Tok (str_of_token2 tok), minplus a))
  | T2(_,a,_,_) -> (Tok (str_of_token2 tok), minplus a)
  | C2("\n",_) -> (NL "", PlusOnly)
  | C2 (s,_) -> (Tok s, PlusOnly)
  | Cocci2("\n",_,_,_,_) -> (NL "", PlusOnly)
  | Cocci2(s,_,_,_,_) -> (Tok s, PlusOnly)
  | Indent_cocci2 | Unindent_cocci2 _ -> (Ind tok, PlusOnly)
  | _ -> (Tok (str_of_token2 tok), Neither)

(* ------------------------------------------------------------------------- *)

let add1 op (am,ap) =
  match op with
    PlusOnly -> (am,ap+1)
  | MinusOnly -> (am+1,ap)
  | Both -> (am+1,ap+1)
  | Neither -> (am,ap)

let accadd1 op (am,ap) =
  match op with
    PlusOnly -> (am,0::ap)
  | MinusOnly -> (0::am,ap)
  | Both -> (0::am,0::ap)
  | Neither -> (am,ap)

let add op (am,ap) (bm,bp) =
  match op with
    PlusOnly -> (am,ap+bp)
  | MinusOnly -> (am+bm,ap)
  | Both -> (am+bm,ap+bp)
  | Neither -> (am,ap)

let sub1 op (am,ap) =
  let sub1 = function 0 -> 0 | n -> n-1 in
  match op with
    PlusOnly -> (am,sub1 ap)
  | MinusOnly -> (sub1 am,ap)
  | Both -> (sub1 am,sub1 ap)
  | Neither -> (am,ap)

let accsub1 op (am,ap) =
  let tl = function x::xs -> xs | [] -> [] in
  match op with
    PlusOnly -> (am,tl ap)
  | MinusOnly -> (tl am,ap)
  | Both -> (tl am,tl ap)
  | Neither -> (am,ap)

let subtract op (am,ap) (bm,bp) =
  let subtract a b = max 0 (a - b) in
  match op with
    PlusOnly -> (am,subtract ap bp)
  | MinusOnly -> (subtract am bm,ap)
  | Both -> (subtract am bm,subtract ap bp)
  | Neither -> (am,ap)

let skip_unlike_me op xs is_whitespace =
  let rec loop = function
      [] -> []
    | x::xs when is_whitespace x -> loop xs
    | ((T2 (_,Ctx,_,_)) :: _) as xs -> xs
    | ((T2 (_,Min _,_,_)) :: _) as xs when op = MinusOnly || op = Both -> xs
    | (((Cocci2 _)::_) | ((C2 _)::_)) as xs
      when op = PlusOnly || op = Both -> xs
    | (Indent_cocci2::_) as xs when op = PlusOnly || op = Both -> xs
    | (Unindent_cocci2 _::_) as xs when op = PlusOnly || op = Both -> xs
    | _::xs -> loop xs in
  loop xs

let open_brace op xs =
  let is_whitespace t = is_whitespace t || is_added_whitespace t in
  match skip_unlike_me op xs is_whitespace with
    [] -> false
  | t::_ -> List.mem (str_of_token2 t) ["{";"<:";";";","]

let notelse op xs =
  not
    (let is_whitespace t = is_whitespace t || is_added_whitespace t in
    match skip_unlike_me op xs is_whitespace with
      [] -> false
    | t::_ -> (str_of_token2 t) = "else")

let close_brace op xs =
  let is_whitespace t = is_whitespace t || is_added_whitespace t in
  match skip_unlike_me op xs is_whitespace with
    [] -> false
  | t::_ -> List.mem (str_of_token2 t) ["}";":>"]

let is_nl op xs =
  let is_whitespace t = is_space t || is_added_space t in
  match skip_unlike_me op xs is_whitespace with
    [] -> false
  | T2(Parser_c.TCommentNewline _,_b,_i,_h)::_ -> true
  | C2 ("\n",_)::_ | Cocci2("\n",_,_,_,_)::_ ->
      true (*not sure if cocci2 is needed*)
  | Indent_cocci2 :: _ -> true
  | Unindent_cocci2 _ :: _ -> true
  | _ -> false

let is_pragma t =
  let str = str_of_token2 t in
  match str with
    "" -> false
  | _ -> String.get str 0 = '#'

let is_access t =
  let str = str_of_token2 t in
  List.mem str ["private";"protected";"public"]

let is_label op xs =
  let is_whitespace t = is_whitespace t || is_added_whitespace t in
  match skip_unlike_me op xs is_whitespace with
    [] -> false
  | t::_ when is_pragma t -> true
  | t::_ when is_access t -> false
  | _::rest ->
      (match skip_unlike_me op rest is_whitespace with
	t::_ when str_of_token2 t = ":" -> true
      | _ -> false)

let adjust_by_function getter op k q vl xs =
  match op with
    MinusOnly | PlusOnly ->
      let fn = if getter op xs then k else q in
      fn op vl
  | Both ->
      let vl1 =
	let fn = if getter MinusOnly xs then k else q in
	fn MinusOnly vl in
      let fn = if getter PlusOnly xs then k else q in
      fn PlusOnly vl1
  | _ -> vl

let getval (am,ap) = function
    PlusOnly -> ap
  | MinusOnly -> am
  | Both -> am (* no idea, pick - arbitrarily *)
  | Neither -> am

let adjust_by_op fn (am,ap) = function
    PlusOnly -> (am,fn ap)
  | MinusOnly -> (fn am,ap)
  | Both -> (fn am,fn ap)
  | Neither -> (am,ap)

let drop_zeroes op accumulator xs =
  let drop_zeroes l =
    let (_,rest) = span (function x -> x = 0) l in
    rest in
  adjust_by_function notelse op
    (fun o a -> adjust_by_op drop_zeroes a o) accsub1 accumulator xs
  (*adjust_by_op drop_zeroes accumulator op*)

let add1top op accumulator =
  let add1 = function x::xs -> (x+1)::xs | _ -> [] in
  adjust_by_op add1 accumulator op

let sub1top op accumulator =
  let sub1 = function x::xs -> (max 0 (x-1))::xs | _ -> [] in
  adjust_by_op sub1 accumulator op

let assign bop =
  List.mem bop
    ["=";"+=";"-=";"*=";"/=";"%=";"&=";"|=";"^=";">?=";"<?=";"<<=";">>=";
      "enum"]

type token_effect_state =
    { dmin : int;
      dplus : int;
      inparens : int * int;
      inassn : int * int;
      inbrace : int * int;
      instruct : int * int;
      accumulator : int list * int list }

let token_effect tok state xs =
  let info = parse_token tok in
  match info with
    (Tok ")",op)
    when getval state.inparens op <= 1 && getval state.inassn op = 0 &&
      getval state.inbrace op > 0 ->
      let nopen_brace a b = not (open_brace a b) in
      let do_nothing a b = b in
      let accumulator =
	adjust_by_function nopen_brace op accadd1 do_nothing state.accumulator xs in
      (Other 1,
       { state with inparens = (0,0); inassn = (0,0); accumulator = accumulator })
  | (Tok "else",op) ->
      (* is_nl is for the case where the next statement is on the same line
	 as the else *)
      let nopen_brace a b =
	let res = not (open_brace a b) && (is_nl a b) in
	res in
      let do_nothing a b = b in
      let accumulator =
	adjust_by_function nopen_brace op accadd1 do_nothing state.accumulator xs in
      (Other 1,
       { state with inparens = (0,0); inassn = (0,0); accumulator = accumulator })
  | (Tok "{",op) | (Tok "<:",op) ->
      let (dmin,dplus) = add1 op (state.dmin,state.dplus) in
      let accumulator = add1top op state.accumulator in
      let instruct =
	if getval state.inassn op > 0
	then add1 op state.instruct
	else state.instruct in
      (Other 2,
       { state with
	 dmin = dmin;
	 dplus = dplus;
	 inassn = (0,0);
	 inbrace = add1 op state.inbrace;
	 instruct = instruct;
	 accumulator = accumulator })
  | (Tok "}",op) | (Tok ":>",op) ->
      let (dmin,dplus) = sub1 op (state.dmin,state.dplus) in
      let accumulator = sub1top op state.accumulator in
      (Other 3,
       { state with
	 dmin = dmin;
	 dplus = dplus;
	 inassn = (0,0);
	 inbrace = sub1 op state.inbrace;
	 instruct = sub1 op state.instruct;
	 accumulator = drop_zeroes op accumulator xs })
  | (Tok(";"|"{}"),op) (* {} is generated when removing an if branch *)
    when getval state.inparens op = 0 && getval state.inassn op <= 1 ->
      (Other 4,
       { state with inassn = (0,0); accumulator = drop_zeroes op state.accumulator xs })
  | (Tok(","),op)
    when getval state.inparens op = 0 && getval state.inassn op <= 1 &&
      getval state.instruct op > 0 ->
      (* in a structure initializer, so a comma is a terminator *)
      (Other 10,
       { state with inassn = (0,0);
	 accumulator = drop_zeroes op state.accumulator xs })
  | (Tok ";",op) ->
      (Other 5,{ state with inassn = sub1 op state.inassn })
  | (Tok bop,op)
    when assign bop && getval state.inparens op + getval state.inassn op = 0 ->
      (Other 6,{ state with inassn = add1 op (0,0) })
  | (Tok "(",op) -> (Other 7,{ state with inparens = add1 op state.inparens })
  | (Tok ")",op) -> (Other 8,{ state with inparens = sub1 op state.inparens })
  | (Ind Indent_cocci2,op) -> (Drop,state)
  | (Ind (Unindent_cocci2 true),op) -> (Drop,state)
  | (Ind (Unindent_cocci2 false),op) -> (Unindent,state)
  | (Tok "case",op) -> (Unindent1,state)
  | (NL after,op) ->
      if is_label Both xs
      then (* ignore indentation *)
	(Label,state)
      else
	let inp = getval state.inparens op in
	let ina = getval state.inassn op in
	let rebuilder min plus =
	  match op with
	    Both -> CtxNL(after,min,plus,inp+ina)
	  | MinusOnly -> MinNL(after,min,plus,inp+ina)
	  | PlusOnly -> PlusNL(plus,inp+ina)
	  | _ -> failwith "not possible" in
	let numacc =
	  (List.length (fst state.accumulator), List.length (snd state.accumulator)) in
	let (admin,adplus) =
	  adjust_by_function close_brace op
	    (fun op x -> add op (sub1 op x) numacc)
	    (fun op x -> add op x numacc)
	    (state.dmin,state.dplus) xs in
	(rebuilder admin adplus,state)
  | (_,op) -> (Other 9,state)

let parse_indentation xs =
  let xs =
    match xs with
      (Unindent_cocci2 false)::xs ->
	(* Drop unindent at the very beginning; no need for prior nl *)
	xs
    | _ -> xs in
  let rec loop n state = function
      [] -> []
    | (x::xs) as l ->
	let (front,x,xs) =
	  let (newlines,rest) = span is_whitespace l in
	  match List.rev newlines with
	    nl::whitespace -> (List.rev whitespace, nl, rest)
	  | [] -> ([],x,xs) in
	let (res,state) = token_effect x state xs in
	let front =
	  let rec loop n = function
	      [] -> []
	    | x::xs ->
		(* Label is better than other, because it is recognized
		   as being like a newline *)
		(n,Label,x) :: loop (n+1) xs in
	  loop n front in
	front @ ((n+List.length front),res,x) :: loop (n+1) state xs in
  let state =
    { dmin = 0;
      dplus = 0;
      inparens = (0,0);
      inassn = (0,0);
      inbrace = (0,0);
      instruct = (0,0);
      accumulator = ([],[]) } in
  loop 1 state xs

exception NoInfo

let get_tabbing_unit shorter longer =
  let old_tab = list_of_string shorter in
  let new_tab = list_of_string longer in
  let rec get_diff n l1 l2 =
    match (l1,l2) with
      ([],xs) -> String.sub longer n ((String.length longer)-n)
    | (x::xs,y::ys) -> if x = y then get_diff (n+1) xs ys else raise NoInfo
    | _ -> failwith "not possible" in
  try Some (get_diff 0 old_tab new_tab)
  with NoInfo -> None

let update_indent tok indent =
  match tok with
    Cocci2("\n",ln,lcol,rcol,nlhint) ->
      Cocci2(("\n"^indent),ln,lcol,rcol,nlhint)
  | C2("\n",_) -> C2("\n"^indent,None)
  | _ -> failwith "bad newline"

let update_entry map depth inparens n indent =
  let others =
    List.filter
      (function ((d,i),_) -> not((depth,inparens) = (d,i)))
      map in
  ((depth,inparens),(n,indent)) :: others

let update_map_min n spaces tabbing_unit past_minus_map depthmin dmin
    inparens retab =
  let past_minus_map = (* gc *)
    List.filter (function ((_,ip),_) -> ip <= inparens) past_minus_map in
  let new_tabbing_unit =
    if retab
    then
      try
	let (_,oldspaces) = List.assoc (dmin,inparens) past_minus_map in
	if depthmin < dmin (* we have outdented *)
	then get_tabbing_unit spaces oldspaces
	else if depthmin = dmin + 1 (* we have indented *)
	then get_tabbing_unit oldspaces spaces
	else tabbing_unit
      with _ -> None
    else None in
  let new_map = update_entry past_minus_map depthmin inparens n spaces in
  (new_tabbing_unit,new_map)

let times before n tabbing_unit ctr =
  (if n < 0 then failwith (Printf.sprintf "n is %d\n" n));
  let tabbing_unit =
    match tabbing_unit with None -> !default_indent | Some tu -> tu in
  let rec loop = function
      0 -> before
    | n -> (loop (n-1)) ^ tabbing_unit in
  loop n

(* adds to the front *)
let times_before after n tabbing_unit ctr =
  (if n < 0 then failwith (Printf.sprintf "n is %d\n" n));
  let tabbing_unit =
    match tabbing_unit with None -> !default_indent | Some tu -> tu in
  let rec loop = function
      0 -> after
    | n -> tabbing_unit ^ (loop (n-1)) in
  loop n

(* drops from the front *)
let untimes_before cur n tabbing_unit ctr =
  (if n < 0 then failwith (Printf.sprintf "n is %d\n" n));
  let tabbing_unit =
    match tabbing_unit with None -> !default_indent | Some tu -> tu in
  let cur = List.rev(Common.list_of_string cur) in
  let tu = Common.list_of_string tabbing_unit in
  let rec loop cur tu =
    match (cur,tu) with
      (x,[]) -> Common.string_of_chars (List.rev x)
    | ([],_) -> ""
    | (x::xs,y::ys) ->
	if x = y
	then loop xs ys
	else if x = ' ' && y = '\t'
	then loop (x::xs) (' '::' '::' '::' '::' '::' '::' '::' '::ys)
	else if x = '\t' && y = ' '
	then loop (' '::' '::' '::' '::' '::' '::' '::' '::xs) (y::ys)
	else (* give up, no idea what to do *)
	  Common.string_of_chars (List.rev cur) in
  loop cur tu

(* Probably doesn't do a good job of parens.  Code in parens may be aligned
by tabbing unit or may have extra space specific to the position of the
parentheses.  Don't seem inheritable.  TODO... *)
let plus_search_in_maps n depth inparens past_minmap minmap tu t =
  let get_answer fail map1 map2 =
    match (map1,map2) with
      (None,None) -> fail()
    | (Some(_,indent),None) | (None,Some(_,indent)) -> update_indent t indent
    | (Some(n1,indent1),Some(n2,indent2)) ->
	let d1 = abs(n - n1) in
	let d2 = abs(n2 - n) in
	let indent = if d1 < d2 then indent1 else indent2 in
	update_indent t indent in
  let find_recent map =
    List.fold_left
      (function (((pdepth,_),(pn,pindent)) as prev) ->
	function (((cdepth,_),(cn,cindent)) as cur) ->
	  if cdepth < depth && cdepth >= pdepth then cur else prev)
      ((-1,-1),(-1,"")) map in
  let fail2 _ =
    (* should we consider inparens here??? *)
    let depth = depth + inparens in
    let ((brecent,_),(bn,bindent)) = find_recent past_minmap in
    let ((arecent,_),(an,aindent)) = find_recent minmap in
    match (brecent,arecent) with
      (-1,-1) -> update_indent t (times "" depth tu 1)
    | (_,-1) -> update_indent t (times bindent (depth - brecent) tu 2)
    | (-1,_) -> update_indent t (times aindent (depth - arecent) tu 3)
    | (_,_) ->
	let d1 = depth - brecent in
	let d2 = depth - arecent in
	if d1 < d2
	then update_indent t (times bindent d1 tu 4)
	else if d2 < d1
	then update_indent t (times aindent d2 tu 5)
	else
	  let n1 = abs(n - bn) in
	  let n2 = abs(an - n) in
	  let indent = if n1 < n2 then bindent else aindent in
	  update_indent t (times indent d2 tu 6) in
  let map1 =
    try Some(List.assoc (depth,inparens) past_minmap) with _ -> None in
  let map2 =
    try Some(List.assoc (depth,inparens) minmap) with _ -> None in
  get_answer fail2 map1 map2

let context_search_in_maps n depth samedelta inparens past_minmap minmap tu t =
  let findn map =
    if not samedelta
    then None
    else
      (* favor just shifing left *)
      try
	Some(List.find (function ((_,ip),(n1,_)) -> ip = inparens && n = n1)
	       map)
      with Not_found -> None in
  let before = findn past_minmap in
  let after = findn minmap in (* should be the same... *)
  (if not (before = after)
  then failwith "inconsistent maps for ctx info");
  match before with
    Some ((old_depth,_),(_,indent)) ->
      if depth < old_depth
      then update_indent t (untimes_before indent (old_depth-depth) tu 1)
      else
	if depth > old_depth
	then update_indent t (times_before indent (depth-old_depth) tu 1)
	else update_indent t indent
  | None ->
      (* parens must have changed, fall back on plus_search *)
      plus_search_in_maps n depth inparens past_minmap minmap tu t

(* Add newlines where needed around unindents.  Lets adjust_indentation
adjust them if needed. *)
let newlines_for_unindents xs =
  let is_ctxnl =
    function T2(Parser_c.TCommentNewline _,_b,_i,_h) -> true | _ -> false in
  let is_plusnl =
    function C2("\n",_) | Cocci2("\n",_,_,_,_) -> true | _ -> false in
  let is_nl x = is_ctxnl x || is_plusnl x in
  let isempty =
    function Cocci2(s,_,_,_,_) -> s = "" | _ -> false in
  let rec loop = function
      [] -> []
    | (Unindent_cocci2 false)::x::nl::rest ->
	x :: loop (nl::rest)
    | ctxnl::(Unindent_cocci2 false)::x::plusnl::rest
      when is_ctxnl ctxnl && is_plusnl plusnl ->
	plusnl::(Unindent_cocci2 false)::x::loop (ctxnl::rest)
    | ctxnl::(Unindent_cocci2 false)::x::rest when is_ctxnl ctxnl ->
	(C2("\n",None))::(Unindent_cocci2 false)::x::loop (ctxnl::rest)
    | plusnl1::(Unindent_cocci2 false)::x::nl2::rest
      when is_plusnl plusnl1 && is_nl nl2 ->
	plusnl1::(Unindent_cocci2 false)::x::loop (nl2::rest)
    | plusnl::(Unindent_cocci2 false)::x::[] when is_plusnl plusnl ->
	plusnl::(Unindent_cocci2 false)::x::[]
    | plusnl::(Unindent_cocci2 false)::x::rest when is_plusnl plusnl ->
	if not(isempty x)
	then
	  (* x can be empty when a newline is in the + code *)
	  plusnl::(Unindent_cocci2 false)::x::loop (C2("\n",None)::rest)
	else
	  plusnl::(Unindent_cocci2 false)::x::loop rest
    | y::(Unindent_cocci2 false)::x::nl::rest when is_nl nl ->
	y::C2("\n",None)::(Unindent_cocci2 false)::x::loop (nl::rest)
    | y::(Unindent_cocci2 false)::x::rest ->
	y::C2("\n",None)::(Unindent_cocci2 false)::x::C2("\n",None)::rest
    | x::rest -> x::loop rest in
  loop xs

(* needed because token_effect doesn't see context *)
let rec clear_unindent1 = function
    [] -> []
  | (n,PlusNL(depth,inparens),t)::(n1,Unindent1,t1)::rest ->
      (n,PlusNL(depth-1,inparens),t)::(n1,Other 0,t1)::(clear_unindent1 rest)
  | (n,CtxNL(spaces,depthmin,depthplus,inparens),t)::(n1,Unindent1,t1)::rest ->
      (n,CtxNL(spaces,depthmin-1,depthplus-1,inparens),t)::
      (n1,Other 0,t1)::(clear_unindent1 rest)
  | (n,MinNL(spaces,depthmin,depthplus,inparens),t)::(n1,Unindent1,t1)::rest ->
      (n,MinNL(spaces,depthmin-1,depthplus-1,inparens),t)::
      (n1,Other 0,t1)::(clear_unindent1 rest)
  | (n,Unindent1,t)::rest -> (n,Other 0,t)::(clear_unindent1 rest)
  | x::rest -> x :: (clear_unindent1 rest)

let adjust_indentation xs =
  let xs = newlines_for_unindents xs in
  let toks = parse_indentation xs in
  let toks = clear_unindent1 toks in
  let rec loop tabbing_unit past_minmap dmin dplus =
    function
	[] -> (tabbing_unit,past_minmap,[])
      |	(n,PlusNL(depth,inparens),t)::(_,Unindent,_)::(_,_,x)::rest ->
	  let (out_tu,minmap,res) =
	    loop tabbing_unit past_minmap dmin dplus rest in
	  (out_tu,minmap,t::x::res)
      |	(_,Unindent,_)::rest -> loop tabbing_unit past_minmap dmin dplus rest
      |	(_,Unindent1,_)::rest -> failwith "removed by clear_unindent1"
      | (n,CtxNL(spaces,depthmin,depthplus,inparens),t)::rest ->
	  let (tabbing_unit,past_minmap) =
	    update_map_min n spaces tabbing_unit past_minmap
	      depthmin dmin inparens true in
	  let (out_tu,minmap,res) =
	    loop tabbing_unit past_minmap depthmin depthplus rest in
	  let (_,minmap) =
	    update_map_min n spaces tabbing_unit minmap
	      depthmin dmin inparens false in
	  let t =
	    if not (depthmin = depthplus) (*&& is_cocci rest*)
	    then
	      context_search_in_maps n depthplus
		((depthmin - depthplus) = (dmin - dplus))
		inparens past_minmap minmap
		tabbing_unit (C2("\n",None))
	    else t in
	  (out_tu,minmap,t::res)
      | (n,MinNL(spaces,depthmin,depthplus,inparens),t)::rest ->
	  let (tabbing_unit,past_minmap) =
	    update_map_min n spaces tabbing_unit past_minmap
	      depthmin dmin inparens true in
	  let (out_tu,minmap,res) =
	    loop tabbing_unit past_minmap depthmin depthplus rest in
	  let (_,minmap) =
	    update_map_min n spaces tabbing_unit minmap
	      depthmin dmin inparens false in
	  (out_tu,minmap,t::res)
      | (n,PlusNL(depth,inparens),t)::rest ->
	  let dont_touch = (* double newline *)
	    let after_min =
	      Common.drop_while (fun (_,_,t) -> is_minus t || is_space t) rest in
	    match after_min with
	      (_,t,_)::_ -> newlineNL t
	    | _ -> false in
	  if dont_touch
	  then
	    let (out_tu,minmap,res) =
	      loop tabbing_unit past_minmap dmin dplus rest in
	    (out_tu,minmap,t::res)
	  else
	    let (out_tu,minmap,res) =
	      loop tabbing_unit past_minmap dmin depth rest in
	    let newtok =
	      plus_search_in_maps n depth inparens past_minmap minmap
		tabbing_unit t in
	    (out_tu, minmap, newtok::res)
      | (n,(Other _|Label),t)::rest ->
	  let (out_tu,minmap,res) =
	    loop tabbing_unit past_minmap dmin dplus rest in
	  (out_tu,minmap,t::res)
      | (n,Drop,t)::rest ->
	  let (out_tu,minmap,res) =
	    loop tabbing_unit past_minmap dmin dplus rest in
	  (out_tu,minmap,res) in
  let nulmap = [((0,0),(0,""))] in
  let (out_tu,_,res) = loop None nulmap 0 0 toks in
  (res,out_tu)

(* ------------------------------------------------------------------------ *)

let rec find_paren_comma = function
  | [] -> ()

  (* do nothing if was like this in original file *)
  | { str = "("; idx = Some p1 } :: ({ str = ","; idx = Some p2} :: _ as xs)
  | { str = ","; idx = Some p1 } :: ({ str = ","; idx = Some p2} :: _ as xs)
  | { str = ","; idx = Some p1 } :: ({ str = ")"; idx = Some p2} :: _ as xs)
    when p2 = p1 + 1 ->
    find_paren_comma xs

  (* otherwise yes can adjust *)
  | { str = "(" } :: (({ str = ","} as rem) :: _ as xs)
  | ({ str = "," } as rem) :: ({ str = ","} :: _ as xs)
  | ({ str = "," } as rem) :: ({ str = ")"} :: _ as xs) ->
    rem.remove <- true;
    find_paren_comma xs

  | x::xs ->
    find_paren_comma xs

let fix_tokens toks =
  let toks = toks +> List.map mk_token_extended in

  let cleaner = toks +> exclude (function
    | {tok2 = T2 (t,_,_,_)} -> TH.is_real_comment t (* I want the ifdef *)
    | {tok2 = C2(" ",_)} -> true (* added by redo_spaces *)
    | _ -> false
  ) in
  find_paren_comma cleaner;
  let toks = rebuild_tokens_extented toks in

  toks +> List.map (fun x -> x.tok2)

(* if we have to remove a '}' that is alone on a line, remove the line too *)
let drop_line toks =
  let rec space_until_newline toks =
    match toks with
    | (T2(_, Min _, _, _) as hd) :: tl ->
	let (drop, tl) = space_until_newline tl in
	(drop, hd :: tl)
    | hd :: tl when is_space hd ->
	space_until_newline tl
    | Fake2 _ :: tl ->
	space_until_newline tl
    | hd :: tl when is_newline hd ->
	(true, toks)
    | _ ->
	(false, toks) in
  let rec loop toks =
    match toks with
    | (T2(_, Min _, _, _) as x) :: tl
      when List.mem (str_of_token2 x) ["}";":>"] ->
	let (drop, tl) = space_until_newline tl in
	(drop, x :: tl)
    | hd :: tl when is_whitespace hd ->
	let (drop, tl) = loop tl in
	if drop then
	  (true, tl)
	else
	  (false, toks)
    | _ -> (false, toks) in
  let rec find toks =
    let (_, toks) = loop toks in
    match toks with
    | [] -> []
    | hd :: tl ->
	hd :: find tl in
  find toks

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
  | T2 (t,_,_,_) ->
    (match TH.pinfo_of_tok t with
    | Ast_c.ExpandedTok _ -> KExpanded
    | Ast_c.OriginTok _ -> KOrigin
    | Ast_c.FakeTok _ -> raise (Impossible 139) (* now a Fake2 *)
    | Ast_c.AbstractLineTok _ -> raise (Impossible 140) (* now a KC *)
    )
  | Unindent_cocci2 _ | Indent_cocci2 | EatSpace2 -> raise (Impossible 141)

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
      if newkind = !current_kind
      then pr (str_of_token2 t)
      else
        begin
          pr (end_mark);
          pr (start_mark newkind);
          pr (str_of_token2 t);
          current_kind := newkind
        end
    );
  else
    let to_whitespace s =
      let len = String.length s in
      let rec translate n =
	if n = len
	then ()
	else
	  begin
	    (match String.get s n with
            | '\t' -> pr "\t"
	    | '\r' -> pr "\r"
	    | '\n' -> pr "\n"
            | _ -> pr " ");
	    translate (n+1)
	  end in
      translate 0 in
    let hiding_level = ref 0 in
    let handle_token t =
      let s = str_of_token2 t in
      let hide_current =
        match t with
        | T2 (t,_,_,_) ->
          let i = TH.info_of_tok t in
          (match Ast_c.get_annot_info i Token_annot.Exclude_start with
          | None   -> ()
          | Some _ -> hiding_level := !hiding_level + 1
          );
          let hide_current = !hiding_level > 0 in
          (match Ast_c.get_annot_info i Token_annot.Exclude_end with
          | None   -> ()
          | Some _ -> hiding_level := max (!hiding_level - 1) 0
          );
          hide_current
        | _ -> !hiding_level > 0 in
      if hide_current then to_whitespace s else pr s in
    xs +> List.iter handle_token





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
  with_open_outfile outfile (fun (pr,chan) ->
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
        (*assert(toks_e +> List.for_all (fun t ->
          TH.is_origin t || TH.is_expanded t
        ));*)
        let toks = get_fakeInfo_and_tokens e toks_e in
        let toks = displace_fake_nodes toks in
        (* assert Origin;ExpandedTok;Faketok *)
        let toks = expand_mcode toks in

        (* assert Origin;ExpandedTok; + Cocci + C (was AbstractLineTok)
         * and no tag endparen, just NOTHING. *)

        let toks =
          if !Flag.sgrep_mode2
          then
            (* nothing else to do for sgrep *)
	    toks +>
	    exclude
	      (fun x -> is_expanded x || is_comma x || is_fake2 x || is_minus x)
          else
            begin
	      (* phase2: can now start to filter and adjust *)
	      let toks = cleanup_comment_trailers toks in
	      let toks = check_danger toks in
	      let toks = fix_slash_slash toks in
	      let toks = paren_then_brace toks in
	      let toks = newline_before_else toks in
	      (* have to annotate droppable spaces early, so that can create
		 the right minus and plus maps in adjust indentation.  For
		 the same reason, cannot actually remove the minus tokens. *)
	      let toks = drop_line toks in
              let toks = remove_minus_and_between_and_expanded_and_fake1 toks in
              let (toks,tu) = adjust_indentation toks in
              let toks = adjust_eat_space toks in
              let toks = adjust_before_semicolon toks in(*before remove minus*)
              let toks = adjust_after_paren toks in(*also before remove minus*)
              let toks = paren_to_space toks in
              let toks = drop_end_comma toks in
              let toks = drop_space_at_endline toks in
              let toks = remove_minus_and_between_and_expanded_and_fake2 toks in
              (* assert Origin + Cocci + C and no minus *)
              let toks = add_space toks in
              let toks = fix_tokens toks in
              let toks = add_newlines toks tu in
              toks
            end in

        (* in theory here could reparse and rework the ast! or
         * apply some SP. Not before cos julia may have generated
         * not parsable file. Need do unparsing_tricks call before
	 * being ready to reparse. *)
        print_all_tokens2 pr toks;

      | PPviastr -> pr str
    )
  )

let pp_program a b =
  (if !Flag_parsing_c.indent > 0
  then default_indent := String.make !Flag_parsing_c.indent ' ');
  profile_code "C unparsing" (fun () -> pp_program2 a b)


let pp_program_default xs outfile =
  (if !Flag_parsing_c.indent > 0
  then default_indent := String.make !Flag_parsing_c.indent ' ');
  let xs' = xs +> List.map (fun x -> x, PPnormal) in
  pp_program xs' outfile
