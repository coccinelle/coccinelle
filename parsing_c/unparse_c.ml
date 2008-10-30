(* Copyright (C) 2006, 2007, 2008 Yoann Padioleau
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
type token2 = 
  | T2 of Parser_c.token * bool (* minus *) * 
          int option (* orig index, abstracting away comments and space *)
  | Fake2
  | Cocci2 of string
  | C2 of string
  | Indent_cocci2
  | Unindent_cocci2

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

let str_of_token2 = function
  | T2 (t,_,_) -> TH.str_of_tok t
  | Fake2 -> ""
  | Cocci2 s -> s
  | C2 s -> s
  | Indent_cocci2 -> ""
  | Unindent_cocci2 -> ""

let print_token2 = function
  | T2 (t,b,_) -> "T2:"^(if b then "-" else "")^TH.str_of_tok t
  | Fake2 -> ""
  | Cocci2 s -> "Cocci2:"^s
  | C2 s -> "C2:"^s
  | Indent_cocci2 -> "Indent"
  | Unindent_cocci2 -> "Unindent"

let print_all_tokens2 l =
  List.iter (function x -> Printf.printf "%s " (print_token2 x)) l;
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
  | Ast_cocci.MINUS (_,[]) -> false
  | Ast_cocci.MINUS (_,x::xs) -> true
  | Ast_cocci.PLUS -> raise Impossible

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
        let (before, x, after) = !toks_in +> Common.split_when (fun tok -> 
	  info =*= TH.info_of_tok tok)
        in
        assert(info = TH.info_of_tok x);
        (*old: assert(before +> List.for_all (TH.is_comment)); *)
        before +> List.iter (fun x -> 
          if not (TH.is_comment x)
          then pr2 ("WIERD: not a comment:" ^ TH.str_of_tok x)
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
  then failwith "WIERD: unparsing not finished";

  List.rev !toks_out

(* Fake nodes that have BEFORE code should be moved over any subsequent
whitespace and newlines, but not any comments, to get as close to the affected
code as possible.  Similarly, face nodes that have AFTER code should be moved
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
	  (Ast_cocci.CONTEXT(_,Ast_cocci.BEFORE _),_) ->
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
    | None -> toks
    | _ -> raise Impossible in
  loop toks

(*****************************************************************************)
(* Tokens2 generation *)
(*****************************************************************************)

let expand_mcode toks = 
  let toks_out = ref [] in

  let index = ref 0 in

  let add_elem t minus = 
    match t with
    | Fake1 info -> 
        let str = Ast_c.str_of_info info in
        if str = ""
        then push2 (Fake2) toks_out
        (* perhaps the fake ',' *)
        else push2 (C2 str) toks_out
          
  
    | T1 tok -> 
        (* no tag on expandedTok ! *)
        assert(not (TH.is_expanded tok && 
            !((TH.info_of_tok tok).cocci_tag) <> Ast_c.emptyAnnot));

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
    let (mcode,env) = !((info_of_token1 t).cocci_tag) in

    let pr_cocci s = 
      push2 (Cocci2 s) toks_out 
    in
    let pr_c info = 
      match Ast_c.pinfo_of_info info with
	Ast_c.AbstractLineTok _ -> push2 (C2 (Ast_c.str_of_info info)) toks_out
      |	Ast_c.FakeTok (s,_) -> push2 (C2 s) toks_out
      |	_ ->
	  Printf.printf "line: %s\n" (Common.dump info);
	  failwith "not an abstract line" in

    let pr_space _ = push2 (C2 " ") toks_out in

    let indent _   = push2 Indent_cocci2 toks_out in
    let unindent _ = push2 Unindent_cocci2 toks_out in

    let args_pp = (env, pr_cocci, pr_c, pr_space, indent, unindent) in

    (* old: when for yacfe with partial cocci: 
     *    add_elem t false; 
    *)

    (* patch: when need full coccinelle transformation *)
    match mcode with
    | Ast_cocci.MINUS (_,any_xxs) -> 
        (* Why adding ? because I want to have all the information, the whole
         * set of tokens, so I can then process and remove the 
         * is_between_two_minus for instance *)
        add_elem t true;
        Unparse_cocci.pp_list_list_any args_pp any_xxs Unparse_cocci.InPlace
    | Ast_cocci.CONTEXT (_,any_befaft) -> 
        (match any_befaft with
        | Ast_cocci.NOTHING -> 
            add_elem t false
        | Ast_cocci.BEFORE xxs -> 
            Unparse_cocci.pp_list_list_any args_pp xxs Unparse_cocci.Before;
            add_elem t false
        | Ast_cocci.AFTER xxs -> 
            add_elem t false;
            Unparse_cocci.pp_list_list_any args_pp xxs Unparse_cocci.After;
        | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
            Unparse_cocci.pp_list_list_any args_pp xxs Unparse_cocci.Before;
            add_elem t false;
            Unparse_cocci.pp_list_list_any args_pp yys Unparse_cocci.After;
        )
    | Ast_cocci.PLUS -> raise Impossible

  in

  toks +> List.iter expand_info;
  List.rev !toks_out
        

(*****************************************************************************)
(* Tokens2 processing, filtering, adjusting *)
(*****************************************************************************)

let is_minusable_comment = function
  | T2 (t,_b,_i) -> 
      (match t with
      | Parser_c.TCommentSpace _   (* only whitespace *)
      (* patch: coccinelle *)      
      | Parser_c.TCommentNewline _ (* newline plus whitespace *)
      | Parser_c.TComment _ 
      | Parser_c.TCommentCpp (Ast_c.CppAttr, _) 
      | Parser_c.TCommentCpp (Ast_c.CppMacro, _) 
        -> true

      | Parser_c.TCommentMisc _ 
      | Parser_c.TCommentCpp (Ast_c.CppDirective, _)
      | Parser_c.TCommentCpp (Ast_c.CppPassingCosWouldGetError, _)
        -> false

      | _ -> false
      )
  | _ -> false 

let all_coccis = function
    Cocci2 _ | C2 _ | Indent_cocci2 | Unindent_cocci2 -> true
  | _ -> false

let is_minusable_comment_or_plus = function
(* patch: coccinelle *)
    T2(Parser_c.TCommentNewline _,_b,_i) -> false
  | x -> is_minusable_comment x or all_coccis x

let set_minus_comment = function
  | T2 (t,false,idx) -> 
      let str = TH.str_of_tok t in
      (match t with
      | Parser_c.TCommentSpace _
(* patch: coccinelle *)      
      | Parser_c.TCommentNewline _ -> ()

      | Parser_c.TComment _ 
      | Parser_c.TCommentCpp (Ast_c.CppAttr, _) 
      | Parser_c.TCommentCpp (Ast_c.CppMacro, _) 
        -> 
          pr2 ("ERASING_COMMENTS: " ^ str)
      | _ -> raise Impossible
      );
      T2 (t, true, idx)
(* patch: coccinelle *)   
  | T2 (Parser_c.TCommentNewline _,true,idx) as x -> x
  | _ -> raise Impossible

let set_minus_comment_or_plus = function
    Cocci2 _ | C2 _ | Indent_cocci2 | Unindent_cocci2 as x -> x
  | x -> set_minus_comment x
      

let remove_minus_and_between_and_expanded_and_fake xs =

  (* get rid of exampled and fake tok *)
  let xs = xs +> Common.exclude (function 
    | T2 (t,_,_) when TH.is_expanded t -> true
    | Fake2 -> true

    | _ -> false
  )
  in

  (*This drops the space before each completely minused block (no plus code).*)
  let rec adjust_before_minus = function
      [] -> []
(* patch: coccinelle  *)
    | (T2(Parser_c.TCommentNewline c,_b,_i) as x)::((T2(_,true,_)::_) as xs) ->
	let minus_or_comment = function
	    T2(_,true,_) -> true
	  | T2(Parser_c.TCommentNewline _,_b,_i) -> false
	  | x -> is_minusable_comment x in
	let (between_minus,rest) = Common.span minus_or_comment xs in
	(match rest with
	  [] -> (set_minus_comment x) :: between_minus
	| T2(Parser_c.TCommentNewline _,_b,_i)::_ ->
	    (set_minus_comment x) :: between_minus @
	    (adjust_before_minus rest)
	| _ -> x :: between_minus @ (adjust_before_minus rest))
    | x::xs -> x::adjust_before_minus xs in

  let xs = adjust_before_minus xs in

  (* this deals with any stuff that is between the minused code, eg
     spaces, comments, attributes, etc. *)
  (* The use of is_minusable_comment_or_plus and set_minus_comment_or_plus
     is because the + code can end up anywhere in the middle of the - code;
     it is not necessarily to the far left *)
  let rec adjust_between_minus xs = 
    match xs with
    | [] -> []
    | (T2 (t1,true,idx1))::xs -> 

        let (between_comments, rest) =
	  Common.span is_minusable_comment_or_plus xs in
        (match rest with
        | [] -> [(T2 (t1, true,idx1))]

        | (T2 (t2, true,idx2))::rest ->
             (T2 (t1, true,idx1))::
               (List.map set_minus_comment_or_plus between_comments @
               adjust_between_minus ((T2 (t2, true, idx2))::rest))
        | x::xs -> 
             (T2 (t1, true, idx1))::
               (between_comments @ adjust_between_minus (x::xs))
        )

    | x::xs -> x::adjust_between_minus xs in

  let xs = adjust_between_minus xs in

  let xs = xs +> Common.exclude (function
    | T2 (t,true,_) -> true
    | _ -> false
  ) in
  xs


let is_ident_like s = s ==~ Common.regexp_alpha

let rec add_space xs = 
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> 
      let sx = str_of_token2 x in
      let sy = str_of_token2 y in
      if is_ident_like sx && is_ident_like sy 
      then x::C2 " "::(add_space (y::xs))
      else x::(add_space (y::xs))



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
    +> Common.take_until (fun c -> c = '\n')
    +> List.rev
    +> List.map string_of_char
    +> String.concat ""

let new_tabbing a = 
  Common.profile_code "C unparsing.new_tabbing" (fun () -> new_tabbing2 a)


let rec adjust_indentation xs = 
  let _current_tabbing = ref "" in
  let tabbing_unit = ref None in

  let string_of_list l = String.concat "" (List.map string_of_char l) in

  (* try to pick a tabbing unit for the plus code *)
  let adjust_tabbing_unit old_tab new_tab =
    if !tabbing_unit = None && String.length new_tab > String.length old_tab
    then
      let old_tab = list_of_string old_tab in
      let new_tab = list_of_string new_tab in
      let rec loop = function
	  ([],new_tab) ->
	    tabbing_unit := Some(string_of_list new_tab,List.rev new_tab)
	| (_,[]) -> failwith "not possible"
	| (o::os,n::ns) -> loop (os,ns) in (* could check for equality *)
      loop (old_tab,new_tab) in

  let remtab tu current_tab =
    let current_tab = List.rev(list_of_string current_tab) in
    let rec loop = function
	([],new_tab) -> string_of_list (List.rev new_tab)
      |	(_,[]) -> "" (*wierd; tabbing unit used up more than the current tab*)
      |	(t::ts,n::ns) when t = n -> loop (ts,ns)
      |	(_,ns) -> (* mismatch; remove what we can *)
	  string_of_list (List.rev ns) in
    loop (tu,current_tab) in

  let rec find_first_tab started = function
      [] -> ()
    | ((T2 (tok,_,_)) as x)::xs when str_of_token2 x = "{" ->
	find_first_tab true xs
(* patch: coccinelle *)
    | ((T2 (Parser_c.TCommentNewline s, _, _)) as x)::_
      when started ->
	let s = str_of_token2 x +> new_tabbing in
	tabbing_unit := Some (s,List.rev (list_of_string s))
    | x::xs -> find_first_tab started xs in
  find_first_tab false xs;

  let rec aux started xs = 
    match xs with
    | [] ->  []
(* patch: coccinelle *)
    | ((T2 (tok,_,_)) as x)::(T2 (Parser_c.TCommentNewline s, _, _))::
      (Cocci2 "{")::xs when started && str_of_token2 x = ")" ->
	(* to be done for if, etc, but not for a function header *)
	x::(Cocci2 " {")::(aux started xs)
    | ((T2 (Parser_c.TCommentNewline s, _, _)) as x)::xs ->
	let old_tabbing = !_current_tabbing in 
        str_of_token2 x +> new_tabbing +> (fun s -> _current_tabbing := s);
	(* only trust the indentation after the first { *)
	(if started then adjust_tabbing_unit old_tabbing !_current_tabbing);
	let coccis_rest = Common.span all_coccis xs in
	(match coccis_rest with
	  (_::_,((T2 (tok,_,_)) as y)::_) when str_of_token2 y = "}" ->
	    (* the case where cocci code has been added before a close } *)
	    x::aux started (Indent_cocci2::xs)
        | _ -> x::aux started xs)
    | Indent_cocci2::xs ->
	(match !tabbing_unit with
	  None -> aux started xs
	| Some (tu,_) ->
	    _current_tabbing := (!_current_tabbing)^tu;
	    Cocci2 (tu)::aux started xs)
    | Unindent_cocci2::xs ->
	(match !tabbing_unit with
	  None -> aux started xs
	| Some (_,tu) ->
	    _current_tabbing := remtab tu (!_current_tabbing);
	    aux started xs)
    (* border between existing code and cocci code *)
    | ((T2 (tok,_,_)) as x)::((Cocci2 "\n") as y)::xs
      when str_of_token2 x = "{" ->
	x::aux true (y::Indent_cocci2::xs)
    | ((Cocci2 _) as x)::((T2 (tok,_,_)) as y)::xs
      when str_of_token2 y = "}" ->
	x::aux started (y::Unindent_cocci2::xs)
    (* starting the body of the function *)
    | ((T2 (tok,_,_)) as x)::xs when str_of_token2 x = "{" ->  x::aux true xs
    | (Cocci2 "{")::xs -> (Cocci2 "{")::aux true xs
    | ((Cocci2 "\n") as x)::xs -> 
            (* dont inline in expr because of wierd eval order of ocaml *)
        let s = !_current_tabbing in 
        x::Cocci2 (s)::aux started xs
    | x::xs -> x::aux started xs in
  aux false xs


let rec find_paren_comma = function
  | [] -> ()

  (* do nothing if was like this in original file *)
  | ({ str = "("; idx = Some p1 } as _x1)::({ str = ","; idx = Some p2} as x2)
    ::xs when p2 = p1 + 1 -> 
      find_paren_comma (x2::xs)

  | ({ str = ","; idx = Some p1 } as _x1)::({ str = ","; idx = Some p2} as x2)
    ::xs when p2 = p1 + 1 -> 
      find_paren_comma (x2::xs)

  | ({ str = ","; idx = Some p1 } as _x1)::({ str = ")"; idx = Some p2} as x2)
    ::xs when p2 = p1 + 1 -> 
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
  | Fake2 -> KFake
  | Cocci2 _ -> KCocci
  | C2 _ -> KC
  | T2 (t,_,_) ->
      (match TH.pinfo_of_tok t with
      | ExpandedTok _ -> KExpanded
      | OriginTok _ -> KOrigin
      | FakeTok _ -> raise Impossible (* now a Fake2 *)
      | AbstractLineTok _ -> raise Impossible (* now a KC *)
      )
  | Unindent_cocci2 | Indent_cocci2 -> raise Impossible
  
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
	  
          (* phase2: can now start to filter and adjust *)
          let toks = adjust_indentation toks in
          let toks = remove_minus_and_between_and_expanded_and_fake toks in
          (* assert Origin + Cocci + C and no minus *)
          let toks = add_space toks in
          let toks = fix_tokens toks in

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
