(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
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

module TH = Token_helpers
module TV = Token_views_c
module LP = Lexer_parser

module Stat = Parsing_stat

open Ast_c
open Parser_c

(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)

let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_parsing

let pr2_cpp s =
  if !Flag_parsing_c.debug_cpp
  then Common.pr2_once ("CPP-" ^ s)


let msg_gen cond is_known printer s =
  if cond
  then
    if not (!Flag_parsing_c.filter_msg)
    then printer s
    else
      if not (is_known s)
      then printer s

(* In the following, there are some hardcoded names of types or macros
 * but they are not used by our heuristics! They are just here to
 * enable to detect false positive by printing only the typedef/macros
 * that we don't know yet. If we print everything, then we can easily
 * get lost with too much verbose tracing information. So those
 * functions "filter" some messages. So our heuristics are still good,
 * there is no more (or not that much) hardcoded linux stuff.
 *)

let is_known_typdef =
    (fun s ->
      (match s with
      | "u_char"   | "u_short"  | "u_int"  | "u_long"
      | "u8" | "u16" | "u32" | "u64"
      | "s8"  | "s16" | "s32" | "s64"
      | "__u8" | "__u16" | "__u32"  | "__u64" | "bool"
        -> true

      | "acpi_handle"
      | "acpi_status"
        -> true

      | "FILE"
      | "DIR"
        -> true

      | s when s =~ ".*_t$" -> true
      | _ -> false
      )
    )

(* note: cannot use partial application with let msg_typedef =
 * because it would compute msg_typedef at compile time when
 * the flag debug_typedef is always false
 *)
let msg_typedef s ii n =
  incr Stat.nTypedefInfer;
  msg_gen (!Flag_parsing_c.debug_typedef)
    is_known_typdef
    (fun s ->
      pr2_cpp
	(Printf.sprintf "TYPEDEF: promoting:(%d) %s on line %d" n s
	   (Ast_c.line_of_info ii))
	(*(Printf.sprintf "TYPEDEF: promoting: %s on line %d" s
	   (Ast_c.line_of_info ii))*)
    )
    s

let msg_maybe_dangereous_typedef s =
  if not (is_known_typdef s)
  then
    pr2
      ("PB MAYBE: dangerous typedef inference, maybe not a typedef: " ^ s)



let msg_declare_macro s =
  incr Stat.nMacroDecl;
  msg_gen (!Flag_parsing_c.debug_cpp)
    (fun s ->
      (match s with
      | "DECLARE_MUTEX" | "DECLARE_COMPLETION"  | "DECLARE_RWSEM"
      | "DECLARE_WAITQUEUE" | "DECLARE_WAIT_QUEUE_HEAD"
      | "DEFINE_SPINLOCK" | "DEFINE_TIMER"
      | "DEVICE_ATTR" | "CLASS_DEVICE_ATTR" | "DRIVER_ATTR"
      | "SENSOR_DEVICE_ATTR"
      | "LIST_HEAD"
      | "DECLARE_WORK"  | "DECLARE_TASKLET"
      | "PORT_ATTR_RO" | "PORT_PMA_ATTR"
      | "DECLARE_BITMAP"

          -> true
 (*
      | s when s =~ "^DECLARE_.*" -> true
      | s when s =~ ".*_ATTR$" -> true
      | s when s =~ "^DEFINE_.*" -> true
 *)

      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found declare-macro: " ^ s))
    s


let msg_foreach s =
  incr Stat.nIteratorHeuristic;
  pr2_cpp ("MACRO: found foreach: " ^ s)


(* ??
let msg_debug_macro s =
  pr2_cpp ("MACRO: found debug-macro: " ^ s)
*)


let msg_macro_noptvirg s =
  incr Stat.nMacroStmt;
  pr2_cpp ("MACRO: found macro with param noptvirg: " ^ s)

let msg_macro_toplevel_noptvirg s =
  incr Stat.nMacroStmt;
  pr2_cpp ("MACRO: found toplevel macro noptvirg: " ^ s)

let msg_macro_noptvirg_single s =
  incr Stat.nMacroStmt;
  pr2_cpp ("MACRO: found single-macro noptvirg: " ^ s)




let msg_macro_higher_order s =
  incr Stat.nMacroHigherOrder;
  msg_gen (!Flag_parsing_c.debug_cpp)
    (fun s ->
      (match s with
      | "DBGINFO"
      | "DBGPX"
      | "DFLOW"
        -> true
      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found higher order macro : " ^ s))
    s


let msg_stringification s =
  incr Stat.nMacroString;
  msg_gen (!Flag_parsing_c.debug_cpp)
    (fun s ->
      (match s with
      | "REVISION"
      | "UTS_RELEASE"
      | "SIZE_STR"
      | "DMA_STR"
        -> true
      (* s when s =~ ".*STR.*" -> true  *)
      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found string-macro " ^ s))
    s

let msg_stringification_params s =
  incr Stat.nMacroString;
  pr2_cpp ("MACRO: string-macro with params : " ^ s)



let msg_apply_known_macro s =
  incr Stat.nMacroExpand;
  pr2_cpp ("MACRO: found known macro = " ^ s)

let msg_apply_known_macro_hint s =
  incr Stat.nMacroHint;
  pr2_cpp ("MACRO: found known macro hint = " ^ s)




let msg_ifdef_bool_passing is_ifdef_positif =
  incr Stat.nIfdefZero; (* of Version ? *)
  if is_ifdef_positif
  then pr2_cpp "commenting parts of a #if 1 or #if LINUX_VERSION"
  else pr2_cpp "commenting a #if 0 or #if LINUX_VERSION or __cplusplus"


let msg_ifdef_mid_something () =
  incr Stat.nIfdefExprPassing;
  pr2_cpp "found ifdef-mid-something"

let msg_ifdef_funheaders () =
  incr Stat.nIfdefFunheader;
  ()

let msg_ifdef_cparen_else () =
  incr Stat.nIfdefPassing;
  pr2_cpp("found ifdef-cparen-else")


let msg_attribute s =
  incr Stat.nMacroAttribute;
  pr2_cpp("ATTR:" ^ s)



(*****************************************************************************)
(* The regexp and basic view definitions *)
(*****************************************************************************)

(* opti: better to built then once and for all, especially regexp_foreach *)

let regexp_macro = Str.regexp
  "^[A-Z_][A-Z_0-9]*$"

(* linuxext: *)
let regexp_annot = Str.regexp
  "^__.*$"

(* linuxext: *)
let regexp_declare = Str.regexp
  ".*DECLARE.*"

(* linuxext: *)
let regexp_foreach = Str.regexp_case_fold
  ".*\\(for_?each\\|for_?all\\|iterate\\|loop\\|walk\\|scan\\|each\\|for\\)"

let regexp_typedef = Str.regexp
  ".*_t$"

let false_typedef = [
  "printk";
  ]


let ok_typedef s = not (List.mem s false_typedef)

let not_annot s =
  not (s ==~ regexp_annot)
let is_macro s =
  s ==~ regexp_macro
let not_macro s =
  not (is_macro s)

let is_macro_paren s rest = (* likely macro call *)
  is_macro s &&
    match rest with
      TOPar _::TOPar _::_ -> true
    | _ -> false


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* the pair is the status of '()' and '{}', ex: (-1,0)
 * if too much ')' and good '{}'
 * could do for [] too ?
 * could do for ','   if encounter ',' at "toplevel", not inside () or {}
 * then if have ifdef, then certainly can lead to a problem.
 *)
let count_open_close_stuff_ifdef_clause :TV.ifdef_grouped list -> (int * int) =
 fun xs ->
   let cnt_paren, cnt_brace = ref 0, ref 0 in
   xs +> TV.iter_token_ifdef (fun x ->
     (match x.TV.tok with
     | x when TH.is_opar x  -> incr cnt_paren
     | TOBrace _ -> incr cnt_brace
     | x when TH.is_cpar x  -> decr cnt_paren
     | TCBrace _ -> decr cnt_brace
     | _ -> ()
     )
   );
   !cnt_paren, !cnt_brace

let sequencible xs =
  let last = ref None in
  let cpp = ref false in
  xs +> List.iter
    (TV.iter_token_ifdef
       (fun x ->
	 (match x.TV.tok with
	   TDefine _ | TUndef _ | TOParDefine _ | TIdentDefine _ | TPrePragma _
	 | TPragma _ | TInclude _ | TIncludeStart _ | TIncludeFilename _
	 | TCppDirectiveOther _ (*occurs?*) -> cpp := true
	 | _ -> ());
	 last := Some x));
  !cpp ||
  (match !last with
    Some t ->
      (match t.TV.tok with
	TCBrace _ | TOBrace _ | TPtVirg _ -> true
      | TCPar i ->
	  (match i.pinfo with
	    (* for macrostatements, not sure if it works *)
	    ExpandedTok _ -> true
	  | _ -> false)
      | _ -> false)
  | None -> true)

(* ------------------------------------------------------------------------- *)
let forLOOKAHEAD = 30


(* look if there is a '{' just after the closing ')', and handling the
 * possibility to have nested expressions inside nested parenthesis
 *
 * todo: use indentation instead of premier(statement) ?
 *)
let is_really_foreach xs =
  let rec is_foreach_aux = function
    | [] -> false, []
    | TCPar _::TOBrace _::xs -> true, xs
      (* the following attempts to handle the cases where there is a
       * single statement in the body of the loop.  undoubtedly more
       * cases are needed.
       * todo: premier(statement) - suivant(funcall)
       *)
    | TCPar _::TIdent _::xs -> true, xs
    | TCPar _::Tif _::xs -> true, xs
    | TCPar _::Twhile _::xs -> true, xs
    | TCPar _::Tfor _::xs -> true, xs
    | TCPar _::Tswitch _::xs -> true, xs
    | TCPar _::Treturn _::xs -> true, xs
    | TCPar _::TInc _::xs -> true, xs
    | TCPar _::TDec _::xs -> true, xs


    | TCPar _::xs -> false, xs
    | TOPar _::xs ->
        let (_, xs') = is_foreach_aux xs in
        is_foreach_aux xs'
    | x::xs -> is_foreach_aux xs
  in
  is_foreach_aux xs +> fst


(* ------------------------------------------------------------------------- *)
let set_ifdef_token_parenthize_info cnt x =
    match x with
    | TIfdef (_, tag, _)
    | TIfdefelse (tag, _)
    | TIfdefelif (_, tag, _)
    | TEndif (tag, _)

    | TIfdefBool (_, tag, _)
    | TIfdefMisc (_, tag, _)
    | TIfdefVersion (_, tag, _) ->
	tag := Some cnt;

    | _ -> raise (Impossible 89)



let ifdef_paren_cnt = ref 0


let rec set_ifdef_parenthize_info xs =
  xs +> List.iter (function
  | TV.NotIfdefLine xs -> ()
  | TV.Ifdefbool (_, xxs, info_ifdef)
  | TV.Ifdef (xxs, info_ifdef) ->

      incr ifdef_paren_cnt;
      let total_directives = List.length info_ifdef in

      info_ifdef +> List.iter (fun x ->
        set_ifdef_token_parenthize_info (!ifdef_paren_cnt, total_directives)
          x.TV.tok);
      xxs +> List.iter set_ifdef_parenthize_info
  )


(*****************************************************************************)
(* The parsing hack for #define *)
(*****************************************************************************)

(* To parse macro definitions I need to do some tricks
 * as some information can be get only at the lexing level. For instance
 * the space after the name of the macro in '#define foo (x)' is meaningful
 * but the grammar can not get this information. So define_ident below
 * look at such space and generate a special TOpardefine. In a similar
 * way macro definitions can contain some antislash and newlines
 * and the grammar need to know where the macro ends (which is
 * a line-level and so low token-level information). Hence the
 * function 'define_line' below and the TDefEol.
 *
 * update: TDefEol is handled in a special way at different places,
 * a little bit like EOF, especially for error recovery, so this
 * is an important token that should not be retagged!
 *
 *
 * ugly hack, a better solution perhaps would be to erase TDefEOL
 * from the Ast and list of tokens in parse_c.
 *
 * note: I do a +1 somewhere, it's for the unparsing to correctly sync.
 *
 * note: can't replace mark_end_define by simply a fakeInfo(). The reason
 * is where is the \n TCommentSpace. Normally there is always a last token
 * to synchronize on, either EOF or the token of the next toplevel.
 * In the case of the #define we got in list of token
 * [TCommentSpace "\n"; TDefEOL] but if TDefEOL is a fakeinfo then we will
 * not synchronize on it and so we will not print the "\n".
 * A solution would be to put the TDefEOL before the "\n".
 * (jll: tried to do this, see the comment "Put end of line..." below)
 *
 * todo?: could put a ExpandedTok for that ?
 *)
let mark_end_define ii =
  let ii' =
    { Ast_c.pinfo = Ast_c.OriginTok { (Ast_c.parse_info_of_info ii) with
        Common.str = "";
        Common.charpos = Ast_c.pos_of_info ii + 1
      };
      cocci_tag = ref Ast_c.emptyAnnot;
      annots_tag = Token_annot.empty;
      comments_tag = ref Ast_c.emptyComments;
      danger = ref Ast_c.NoDanger;
    }
  in
  TDefEOL (ii')

(* put the TDefEOL at the right place *)
let rec define_line_1 acc = function
  | [] -> List.rev acc
  | token::tokens ->
    begin match token with
      | TDefine ii | TUndef ii ->
        let line = Ast_c.line_of_info ii in
        define_line_2 (token::acc) line ii tokens
      | TPrePragma(_,_,_,_,_,ii) ->
          (* need final ii for PrePragma, to match end of line *)
	  let (str,ii) = List.hd(List.rev ii) in
          let line = Ast_c.line_of_info ii in
          define_line_2 (token::acc) line ii tokens
      | TCppEscapedNewline ii ->
        pr2 ("SUSPICIOUS: a \\ character appears outside of a #define at");
        pr2 (Ast_c.strloc_of_info ii);
        define_line_1 ((TCommentSpace ii) :: acc) tokens
      | _ -> define_line_1 (token::acc) tokens
    end

and define_line_2 acc line lastinfo = function
  | [] ->
      (* should not happened, should meet EOF before *)
      pr2 "PB: WEIRD";
      List.rev ((mark_end_define lastinfo)::acc)
  | token::tokens ->
      let line' = TH.line_of_tok token in
      let info = TH.info_of_tok token in
      (match token with
      | EOF ii -> define_line_1 (token::(mark_end_define lastinfo)::acc) tokens
      | TCppEscapedNewline ii ->
          if (line' <> line) then pr2 "PB: WEIRD: not same line number";
          define_line_2 ((TCommentSpace ii)::acc) (line+1) info tokens
      | TCommentNewline ii ->
	  define_line_1 (token::(mark_end_define lastinfo)::acc) tokens
      | _ ->
          define_line_2
            (token::acc) (end_line_of_tok line' token) info tokens)

(* for a comment, the end line is not the same as line_of_tok *)
and end_line_of_tok default = function
    (TComment _) as t ->
      let line_fragments =
	Str.split_delim (Str.regexp "\n") (TH.str_of_tok t) in
      (match List.rev line_fragments with
	_::front_fragments ->
	  (* no need for backslash at the end of these lines... *)
	  TH.line_of_tok t + (List.length front_fragments)
      |	_ -> failwith (Printf.sprintf "bad comment: %d" (TH.line_of_tok t)))
  |  t -> default

let rec define_ident acc = function
  | [] -> List.rev acc
  | token::tokens ->
    (match token with
    | TUndef ii ->
	let acc = token::acc in
	(match tokens with
	  TCommentSpace i1::TIdent (s,i2)::xs ->
            define_ident ((TIdentDefine (s,i2))::(TCommentSpace i1)::acc) xs
	| _ ->
            pr2 "WEIRD: weird #undef body";
            define_ident acc tokens
      )
    | TDefine ii ->
	let acc = token::acc in
	(match tokens with
	| TCommentSpace i1::TIdent (s,i2)::TOPar (i3)::xs ->
          (* Change also the kind of TIdent to avoid bad interaction
           * with other parsing_hack tricks. For instant if keep TIdent then
           * the stringication algo can believe the TIdent is a string-macro.
           * So simpler to change the kind of the ident too.
           *)
          (* if TOParDefine sticked to the ident, then
           * it's a macro-function. Change token to avoid ambiguity
           * between #define foo(x)  and   #define foo   (x)
           *)
	    let acc = (TCommentSpace i1) :: acc in
	    let acc = (TIdentDefine (s,i2)) :: acc in
	    let acc = (TOParDefine i3) :: acc in
            define_ident acc xs

	| TCommentSpace i1::TIdent (s,i2)::xs ->
	    let acc = (TCommentSpace i1) :: acc in
	    let acc = (TIdentDefine (s,i2)) :: acc in
            define_ident acc xs

      (* bugfix: ident of macro (as well as params, cf below) can be tricky
       * note, do we need to subst in the body of the define ? no cos
       * here the issue is the name of the macro, as in #define inline,
       * so obviously the name of this macro will not be used in its
       * body (it would be a recursive macro, which is forbidden).
       *)

	| TCommentSpace i1::t::xs
	  when TH.str_of_tok t ==~ Common.regexp_alpha
	  ->
            let s = TH.str_of_tok t in
            let ii = TH.info_of_tok t in
	    pr2 (spf "remapping: %s to an ident in macro name" s);
            let acc = (TCommentSpace i1) :: acc in
	    let acc = (TIdentDefine (s,ii)) :: acc in
            define_ident acc xs

	| TCommentSpace _::_::xs
	| xs ->
            pr2 "WEIRD: weird #define body";
            define_ident acc xs
      )
    | TPrePragma(prag,wss1,ident,iinfo,wss2,rest) ->
	let acc = (TPragma prag) :: acc in
	let acc = (TCommentSpace wss1) :: acc in
	let acc = (TIdent(ident,iinfo)) :: acc in
	let acc =
	  if Ast_c.str_of_info wss2 = ""
	  then acc
	  else (TCommentSpace wss2) :: acc in
	let acc =
	  List.fold_left
	    (fun acc (rest,rinfo) ->
	      (TPragmaString(rest,rinfo)) :: acc)
	    acc rest in
	define_ident acc tokens
    | _ ->
	let acc = token::acc in
	define_ident acc tokens
    )



let fix_tokens_define2 xs =
  define_ident [] (define_line_1 [] xs)

let fix_tokens_define a =
  Common.profile_code "C parsing.fix_define" (fun () -> fix_tokens_define2 a)


(* ------------------------------------------------------------------------- *)
(* Other parsing hacks related to cpp, Include/Define hacks *)
(* ------------------------------------------------------------------------- *)

(* Sometimes I prefer to generate a single token for a list of things in the
 * lexer so that if I have to passed them, like for passing TInclude then
 * it's easy. Also if I don't do a single token, then I need to
 * parse the rest which may not need special stuff, like detecting
 * end of line which the parser is not really ready for. So for instance
 * could I parse a #include <a/b/c/xxx.h> as 2 or more tokens ? just
 * lex #include ? so then need recognize <a/b/c/xxx.h> as one token ?
 * but this kind of token is valid only after a #include and the
 * lexing and parsing rules are different for such tokens so not that
 * easy to parse such things in parser_c.mly. Hence the following hacks.
 *
 * less?: maybe could get rid of this like I get rid of some of fix_define.
 *)

(* helpers *)

(* used to generate new token from existing one *)
let new_info posadd str ii =
  { Ast_c.pinfo =
      Ast_c.OriginTok { (Ast_c.parse_info_of_info ii) with
        charpos = Ast_c.pos_of_info ii + posadd;
        str     = str;
        column = Ast_c.col_of_info ii + posadd;
      };
    (* must generate a new ref each time, otherwise share *)
    cocci_tag = ref Ast_c.emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag = ref Ast_c.emptyComments;
    danger = ref Ast_c.NoDanger;
   }


let rec comment_until_defeol xs =
  match xs with
  | [] ->
      (* job not done in Cpp_token_c.define_parse ? *)
      failwith "can't find end of define token TDefEOL"
  | x::xs ->
      (match x with
      | Parser_c.TDefEOL i ->
          Parser_c.TCommentCpp (Token_c.CppDirective, TH.info_of_tok x)
          ::xs
      | _ ->
          let x' =
            (* bugfix: otherwise may lose a TComment token *)
            if TH.is_real_comment x
            then x
            else Parser_c.TCommentCpp (Token_c.CppPassingNormal (*good?*), TH.info_of_tok x)
          in
          x'::comment_until_defeol xs
      )

let drop_until_defeol xs =
  List.tl
    (Common.drop_until (function Parser_c.TDefEOL _ -> true | _ -> false) xs)



(* ------------------------------------------------------------------------- *)
(* returns a pair (replaced token, list of next tokens) *)
(* ------------------------------------------------------------------------- *)

let tokens_include (info, includes, filename, inifdef) =
  Parser_c.TIncludeStart (Ast_c.rewrap_str includes info, inifdef),
  [Parser_c.TIncludeFilename
      (filename, (new_info (String.length includes) filename info))
  ]




(*****************************************************************************)
(* CPP handling: macros, ifdefs, macros defs  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* special skip_start skip_end handling *)
(* ------------------------------------------------------------------------- *)

(* note: after this normally the token list should not contain any more the
 * TCommentSkipTagStart and End tokens.
 *)
let rec commentize_skip_start_to_end xs =
  match xs with
  | [] -> ()
  | x::xs ->
      (match x with
      | {TV.tok = TCommentSkipTagStart info} ->
          (try
            let (before, x2, after) =
              xs +> Common.split_when (function
              | {TV.tok = TCommentSkipTagEnd _ } -> true
              | _ -> false
              )
            in
            let topass = x::x2::before in
            topass +> List.iter (fun tok ->
              TV.set_as_comment Token_c.CppPassingExplicit tok
            );
            commentize_skip_start_to_end after
          with Not_found ->
            failwith "could not find end of skip_start special comment"
          )
      | {TV.tok = TCommentSkipTagEnd info} ->
            failwith "found skip_end comment but no skip_start"
      | _ ->
          commentize_skip_start_to_end xs
      )




(* ------------------------------------------------------------------------- *)
(* ifdef keeping/passing *)
(* ------------------------------------------------------------------------- *)

(* #if 0, #if 1,  #if LINUX_VERSION handling *)
let rec find_ifdef_bool xs =
  xs +> List.iter (function
  | TV.NotIfdefLine _ -> ()
  | TV.Ifdefbool (is_ifdef_positif, xxs, info_ifdef_stmt) ->

      msg_ifdef_bool_passing is_ifdef_positif;

      (match xxs with
      | [] -> raise (Impossible 90)
      | firstclause::xxs ->
          info_ifdef_stmt +>
            List.iter (TV.save_as_comment (fun x ->
                                    Token_c.CppIfDirective x));

          if is_ifdef_positif
          then xxs +> List.iter
                (TV.iter_token_ifdef (TV.set_as_comment Token_c.CppPassingNormal))
          else begin
            firstclause +>
              TV.iter_token_ifdef (TV.set_as_comment Token_c.CppPassingNormal);
            (match List.rev xxs with
            (* keep only last *)
            | last::startxs ->
                startxs +> List.iter
                  (TV.iter_token_ifdef (TV.set_as_comment Token_c.CppPassingNormal))
            | [] -> (* not #else *) ()
            );
          end
      );

  | TV.Ifdef (xxs, info_ifdef_stmt) -> xxs +> List.iter find_ifdef_bool
  )

(* Rules that fix input tokens. *)
type fix_tokens_rule = token list (* input tokens *)
                        -> (token list * token list) option
                           (* fixed tokens, remaining tokens *)

(* #ifdef A if e S1 else #endif S2 *)
let fix_tokens_ifdef_endif :fix_tokens_rule = fun xs ->
  match TH.match_cpp_simple_ifdef_endif xs with
  | Some (tok_ifdef,body_ifdef,tok_endif,rest_endif) ->
    let (whites1,body_ifdef') = span TH.is_just_comment_or_space body_ifdef in
    begin match (TH.match_simple_if_else body_ifdef') with
    | Some (tok_if,body_if,tok_else,rest_else)
      when List.for_all TH.is_just_comment_or_space rest_else ->
        let tok_ifdefif = TUifdef (TH.info_of_tok tok_ifdef) in
        let tok_ifendif = TUendif (TH.info_of_tok tok_endif) in
        let fixed_toks = (tok_ifdefif :: body_ifdef) @ [tok_ifendif] in
        Some (fixed_toks,rest_endif)
    | __else__ -> None
    end
  | None -> None

(* #ifdef A if e S1 else #else S2 #endif S3 *)
let fix_tokens_ifdef_else_endif :fix_tokens_rule = fun xs ->
  match TH.match_cpp_simple_ifdef_else_endif xs with
  | Some (tok_ifdef,body_ifdef,tok_else,body_else,tok_endif,rest_endif) ->
    let (whites1,body_ifdef') = span TH.is_just_comment_or_space body_ifdef in
    begin match (TH.match_simple_if_else body_ifdef') with
    | Some (tok_if,body_if,tok_if_else,rest_else)
      when List.for_all TH.is_just_comment_or_space rest_else ->
        let tok_ifdefif = TUifdef (TH.info_of_tok tok_ifdef) in
        let tok_elseu = TUelseif (TH.info_of_tok tok_else) in
        let tok_endifu = TUendif (TH.info_of_tok tok_endif) in
        let fixed_toks = (tok_ifdefif :: body_ifdef)
                          @ (tok_elseu :: body_else) @ [tok_endifu] in
        Some (fixed_toks,rest_endif)
    | __else__ -> None
    end
  | None -> None

(* Note [Nasty Undisciplined Cpp]
 *
 * This function handles variability patterns that cannot be easily handled
 * by cpp_ifdef_statementize, aka "nasty undisciplined uses of cpp" for the
 * lay person. These are uses of cpp #ifdef that do not cover a full syntactic
 * element. Even though the possibilities are infinite, there are maybe a ten
 * patterns that one sees in practice.
 *
 * __Ideally__, we would like to handle these patterns with a AST-to-AST
 * rewriting pass after parsing, in the same spirit as cpp_ifdef_statementize.
 * But such nasty uses of cpp are handled by the parser by commenting out the
 * code, and that makes difficult to handle it after parsing.
 *
 * Thus, __pragmatically__, if we find an occurrence of these patterns we mark
 * it by replacing regular Cpp tokens by undisciplined Cpp tokens, before
 * parsing. The pattern is finally recognized by the parser. Since we introduce
 * these new tokens, the new parsing rules do not introduce shift/reduce
 * conflicts.
 *
 * For each new pattern to be supported we need:
 * - a helper match_cpp_* in Token_helpers,
 * - a fix_tokens_rule that must be added to fix_tokens_ifdef, and
 * - a rule in the parser.
 *
 * /Iago
 *)
let rec fix_tokens_ifdef (xs :token list) :token list =
  let skip_tok () = match xs with
    | [] -> []
    | x::xs -> x :: fix_tokens_ifdef xs
    in
  match fix_tokens_ifdef_endif xs with
  | Some (fixed,xs') -> fixed @ fix_tokens_ifdef xs'
  | None ->
  match fix_tokens_ifdef_else_endif xs with
  | Some (fixed,xs') -> fixed @ fix_tokens_ifdef xs'
  | None ->
      skip_tok()

let thresholdIfdefSizeMid = 6

(* infer ifdef involving not-closed expressions/statements *)
let rec find_ifdef_mid xs =
  xs +> List.iter (function
  | TV.NotIfdefLine _ -> ()
  | TV.Ifdef (xxs, info_ifdef_stmt) ->
      (match xxs with
      | [] -> raise (Impossible 91)
      | [first] -> ()
      | first::second::rest ->
          (* don't analyse big ifdef *)
          if xxs +> List.for_all
            (fun xs -> List.length xs <= thresholdIfdefSizeMid) &&
            (* don't want nested ifdef *)
            xxs +> List.for_all (fun xs ->
              xs +> List.for_all
                (function TV.NotIfdefLine _ -> true | _ -> false)
            )

          then
            let counts = xxs +> List.map count_open_close_stuff_ifdef_clause in
            let cnt1, cnt2 = List.hd counts in
            if (not (sequencible xxs)) || (* require end with ; or } *)
	        (cnt1 <> 0 || cnt2 <> 0 &&
		counts +> List.for_all (fun x -> x = (cnt1, cnt2)))
              (*
                if counts +> List.exists (fun (cnt1, cnt2) ->
                cnt1 <> 0 || cnt2 <> 0
                )
              *)
            then begin
              msg_ifdef_mid_something();
              (* keep only first, treat the rest as comment *)
              info_ifdef_stmt +> List.iter
                  (TV.save_as_comment (function x -> Token_c.CppIfDirective x));
              (second::rest) +>
	      List.iter
		(TV.iter_token_ifdef (TV.set_as_comment Token_c.CppPassingCosWouldGetError));
            end

      );
      List.iter find_ifdef_mid xxs

  (* no need complex analysis for ifdefbool *)
  | TV.Ifdefbool (_, xxs, info_ifdef_stmt) ->
      List.iter find_ifdef_mid xxs


  )


let thresholdFunheaderLimit = 4

(* ifdef defining alternate function header, type *)
let rec find_ifdef_funheaders = function
  | [] -> ()
  | TV.NotIfdefLine _::xs -> find_ifdef_funheaders xs

  (* ifdef-funheader if ifdef with 2 lines and a '{' in next line *)
  | TV.Ifdef
      ([(TV.NotIfdefLine (({TV.col = 0} as _xline1)::line1))::ifdefblock1;
        (TV.NotIfdefLine (({TV.col = 0} as xline2)::line2))::ifdefblock2
      ], info_ifdef_stmt
      )
    ::TV.NotIfdefLine (({TV.tok = TOBrace i; TV.col = 0})::line3)
    ::xs
   when List.length ifdefblock1 <= thresholdFunheaderLimit &&
        List.length ifdefblock2 <= thresholdFunheaderLimit
    ->
      find_ifdef_funheaders xs;

      msg_ifdef_funheaders ();
      info_ifdef_stmt +>
      List.iter (TV.save_as_comment (fun x -> Token_c.CppIfDirective x));
      let all_toks = [xline2] @ line2 in
      all_toks +> List.iter (TV.set_as_comment Token_c.CppPassingCosWouldGetError) ;
      ifdefblock2 +> TV.iter_token_ifdef (TV.set_as_comment Token_c.CppPassingCosWouldGetError);

  (* ifdef with nested ifdef *)
  | TV.Ifdef
      ([[TV.NotIfdefLine (({TV.col = 0} as _xline1)::line1)];
        [TV.Ifdef
            ([[TV.NotIfdefLine (({TV.col = 0} as xline2)::line2)];
              [TV.NotIfdefLine (({TV.col = 0} as xline3)::line3)];
            ], info_ifdef_stmt2
            )
        ]
      ], info_ifdef_stmt
      )
    ::TV.NotIfdefLine (({TV.tok = TOBrace i; TV.col = 0})::line4)
    ::xs
    ->
      find_ifdef_funheaders xs;

      msg_ifdef_funheaders ();
      info_ifdef_stmt  +>
      List.iter (TV.save_as_comment (fun x -> Token_c.CppIfDirective x));
      info_ifdef_stmt2 +>
      List.iter (TV.save_as_comment (fun x -> Token_c.CppIfDirective x));
      let all_toks = [xline2;xline3] @ line2 @ line3 in
      all_toks +> List.iter (TV.set_as_comment Token_c.CppPassingCosWouldGetError);

 (* ifdef with elseif *)
  | TV.Ifdef
      ([[TV.NotIfdefLine (({TV.col = 0} as _xline1)::line1)];
        [TV.NotIfdefLine (({TV.col = 0} as xline2)::line2)];
        [TV.NotIfdefLine (({TV.col = 0} as xline3)::line3)];
      ], info_ifdef_stmt
      )
    ::TV.NotIfdefLine (({TV.tok = TOBrace i; TV.col = 0})::line4)
    ::xs
    ->
      find_ifdef_funheaders xs;

      msg_ifdef_funheaders ();
      info_ifdef_stmt +>
      List.iter (TV.save_as_comment (fun x -> Token_c.CppIfDirective x));
      let all_toks = [xline2;xline3] @ line2 @ line3 in
      all_toks +> List.iter (TV.set_as_comment Token_c.CppPassingCosWouldGetError)

  (* recurse *)
  | TV.Ifdef (xxs,info_ifdef_stmt)::xs
  | TV.Ifdefbool (_, xxs,info_ifdef_stmt)::xs ->
      List.iter find_ifdef_funheaders xxs;
      find_ifdef_funheaders xs



(* ?? *)
let adjust_inifdef_include xs =
  let is_ifndef_nop xxs = function
      x::xs ->
	(match x.TV.tok with
	  Parser_c.TIfdef (Ast_c.Gifndef vr,_,_) ->
	    (match xxs with
	      ((TV.NotIfdefLine (def::nm::_))::_)::_ ->
		(match (def.TV.tok,nm.TV.tok) with
		  (Parser_c.TDefine _,TIdentDefine(dnm,_)) -> vr = dnm
		| _ -> false)
	    | _ -> false)
	| _ -> false)
    | _ -> false in
  xs +> List.iter (function
  | TV.NotIfdefLine _ -> ()
  | TV.Ifdef (xxs, info_ifdef_stmt) | TV.Ifdefbool (_, xxs, info_ifdef_stmt)
      when is_ifndef_nop xxs info_ifdef_stmt
    -> () (* ifndef followed by define of same variable, often in .h *)
  | TV.Ifdef (xxs, info_ifdef_stmt) | TV.Ifdefbool (_, xxs, info_ifdef_stmt) ->
      xxs +> List.iter (TV.iter_token_ifdef (fun tokext ->
        match tokext.TV.tok with
        | Parser_c.TInclude (s1, s2, inifdef_ref, ii) ->
            inifdef_ref := true;
        | _ -> ()
      ));
  )







let find_ifdef_cparen_else xs =
  let rec aux xs =
  xs +> List.iter (function
  | TV.NotIfdefLine _ -> ()
  | TV.Ifdef (xxs, info_ifdef_stmt) ->
      (match xxs with
      | [] -> raise (Impossible 92)
      | [first] -> ()
      | first::second::rest ->

         (* found a closing ')' just after the #else *)

          (* Too bad ocaml does not support better list pattern matching
           * a la Prolog-III where can match the end of lists.
           *)
          let condition =
            if List.length first = 0 then false
            else
              let last_line = Common.last first in
              match last_line with
              | TV.NotIfdefLine xs ->
                  if List.length xs = 0 then false
                  else
                    let last_tok = Common.last xs in
                    TH.is_cpar last_tok.TV.tok
              | TV.Ifdef _ | TV.Ifdefbool _ -> false
          in
          if condition then begin
            msg_ifdef_cparen_else();
            (* keep only first, treat the rest as comment *)
            info_ifdef_stmt +>
	    List.iter (TV.save_as_comment (fun x -> Token_c.CppIfDirective x));
            (second::rest) +> List.iter
              (TV.iter_token_ifdef (TV.set_as_comment Token_c.CppPassingCosWouldGetError));
          end

      );
      List.iter aux xxs

  (* no need complex analysis for ifdefbool *)
  | TV.Ifdefbool (_, xxs, info_ifdef_stmt) ->
      List.iter aux xxs
  )
  in aux xs


(* ------------------------------------------------------------------------- *)
(* cpp-builtin part2, macro, using standard.h or other defs *)
(* ------------------------------------------------------------------------- *)

(* now in cpp_token_c.ml *)

(* ------------------------------------------------------------------------- *)
(* stringification *)
(* ------------------------------------------------------------------------- *)

let rec find_string_macro_paren xs =
  match xs with
  | [] -> ()
  | TV.Parenthised(xxs, info_parens)::xs ->
      xxs +> List.iter (fun xs ->
        if xs +> List.exists
          (function TV.PToken({TV.tok = (TString _| TMacroString _)}) -> true | _ -> false) &&
          xs +> List.for_all
          (function TV.PToken({TV.tok = (TString _| TMacroString _)}) | TV.PToken({TV.tok = TIdent _}) ->
            true | _ -> false)
        then
          xs +> List.iter (fun tok ->
            match tok with
            | TV.PToken({TV.tok = TIdent (s,_)} as id) ->

                msg_stringification s;
                id.TV.tok <- TMacroString (s, TH.info_of_tok id.TV.tok);
            | _ -> ()
          )
        else
          find_string_macro_paren xs
      );
      find_string_macro_paren xs
  | TV.PToken(tok)::xs ->
      find_string_macro_paren xs


(* ------------------------------------------------------------------------- *)
(* format strings *)
(* ------------------------------------------------------------------------- *)

(* can't just expand all strings, because string followed by another string
will turn into a MultiString. *)
let fix_tokens_strings toks =
  let comments x = TH.is_comment x in
  let strings_and_comments =
    function TString _ | TMacroString _ -> true | x -> TH.is_comment x in
  let can_be_string =
    function TString _ | TMacroString _ -> true | x -> false in
  let rec skip acc fn = function
      x :: xs when fn x -> skip (x :: acc) fn xs
    | xs -> (List.rev acc, xs)
  (* tail recursive for very large parsing units *)
  and out_strings acc = function
      [] -> List.rev acc
    | a :: rest ->
      if can_be_string a
      then
        let (front,rest) = skip [] comments rest in
        (match rest with
          b :: rest when can_be_string b ->
            let (front2,rest) = skip [] strings_and_comments rest in
	    let new_acc =
	      (List.rev front2) @ b :: (List.rev front) @ a :: acc in
            out_strings new_acc rest
        | _ ->
            (match a with
              TString(str_isW,info) ->
		let str = Parse_string_c.parse_string str_isW info in
		let new_acc = (List.rev front) @ (List.rev str) @ acc in
		out_strings new_acc rest
            | _ ->
		let new_acc = (List.rev front) @ (a :: acc) in
		out_strings new_acc rest))
      else out_strings (a::acc) rest in
  out_strings [] toks

(* ------------------------------------------------------------------------- *)
(* macro2 *)
(* ------------------------------------------------------------------------- *)

(* don't forget to recurse in each case *)
let rec find_macro_paren xs =
  match xs with
  | [] -> ()

  (* attribute *)
  | TV.PToken ({TV.tok = TattributeNoarg ii} as id)
    ::xs
     ->
      pr2_cpp (Printf.sprintf "MACRO: attributenoarg %s detected "
		 (Ast_c.str_of_info ii));
      TV.set_as_comment Token_c.CppAttr id;
      find_macro_paren xs
  | TV.PToken ({TV.tok = TIdent _})::TV.PToken ({TV.tok = TIdent _})
    ::TV.PToken ({TV.tok = TIdent (s,ii)} as id)
    ::TV.Parenthised (xxs,info_parens)
    ::(TV.PToken {TV.tok = TPtVirg _} | TV.PToken {TV.tok = TEq _})
    ::xs when (LP.current_context () = LP.InTopLevel &&
	      s ==~ regexp_annot) ->
      msg_attribute s;
      id.TV.tok <- TMacroAttrArgs (s,ii);
      find_macro_paren xs
  | TV.PToken ({TV.tok = TCCro _})::TV.PToken ({TV.tok = TIdent (s,ii)} as id)
    ::TV.Parenthised (xxs,info_parens)
    ::(TV.PToken {TV.tok = TPtVirg _} | TV.PToken {TV.tok = TEq _})
    ::xs when (LP.current_context () = LP.InTopLevel &&
	      s ==~ regexp_annot) ->
      msg_attribute s;
      id.TV.tok <- TMacroAttrArgs (s,ii);
      find_macro_paren xs
  | TV.PToken ({TV.tok = TMacroAttr (s,ii)} as attr)
    ::TV.Parenthised (xxs,info_parens)
    ::(TV.PToken {TV.tok = TPtVirg _} | TV.PToken {TV.tok = TEq _} | TV.PToken {TV.tok = TOBrace _})
    ::xs
     ->
      attr.TV.tok <- TMacroAttrArgs (s,ii);
      find_macro_paren xs

(*
  (* attribute cpp, __xxx id *)
  | TV.PToken ({TV.tok = TIdent (s,i1)} as id)
    ::TV.PToken ({TV.tok = TIdent (s2, i2)} as id2)
    ::xs when s ==~ regexp_annot
     ->
      msg_attribute s;
      id.TV.tok <- TMacroAttr (s, i1);
      find_macro_paren ((TV.PToken id2)::xs); (* recurse also on id2 ? *)

  (* attribute cpp, id __xxx *)
  | TV.PToken ({TV.tok = TIdent (s,i1)} as _id)
    ::TV.PToken ({TV.tok = TIdent (s2, i2)} as id2)
    ::xs when s2 ==~ regexp_annot && (not (s ==~ regexp_typedef))
     ->
      msg_attribute s2;
      id2.TV.tok <- TMacroAttr (s2, i2);
      find_macro_paren xs

  | TV.PToken ({TV.tok = (Tstatic _ | Textern _)} as tok1)
    ::TV.PToken ({TV.tok = TIdent (s,i1)} as attr)
    ::xs when s ==~ regexp_annot
    ->
      pr2_cpp ("storage attribute: " ^ s);
      attr.TV.tok <- TMacroAttrStorage (s,i1);
      (* recurse, may have other storage attributes *)
      find_macro_paren (TV.PToken (tok1)::xs)

*)

  (* stringification
   *
   * the order of the matching clause is important
   *
   *)

  (* string macro with params, before case *)
  | TV.PToken ({TV.tok = (TString _| TMacroString _)})::TV.PToken ({TV.tok = TIdent (s,_)} as id)
    ::TV.Parenthised (xxs, info_parens)
    ::xs ->

      msg_stringification_params s;
      id.TV.tok <- TMacroString (s, TH.info_of_tok id.TV.tok);
      [TV.Parenthised (xxs, info_parens)] +>
        TV.iter_token_paren (TV.set_as_comment Token_c.CppMacro);
      find_macro_paren xs

  (* after case *)
  | TV.PToken ({TV.tok = TIdent (s,_)} as id)
    ::TV.Parenthised (xxs, info_parens)
    ::TV.PToken ({TV.tok = (TString _ | TMacroString _)})
    ::xs ->

      msg_stringification_params s;
      id.TV.tok <- TMacroString (s, TH.info_of_tok id.TV.tok);
      [TV.Parenthised (xxs, info_parens)] +>
        TV.iter_token_paren (TV.set_as_comment Token_c.CppMacro);
      find_macro_paren xs


  (* for the case where the string is not inside a funcall, but
   * for instance in an initializer.
   *)

  (* string macro variable, before case *)
  | TV.PToken ({TV.tok = (TString _ | TMacroString _)})::TV.PToken ({TV.tok = TIdent (s,_)} as id)
      ::xs when !Flag.c_plus_plus = Flag.Off ->

      msg_stringification s;
      id.TV.tok <- TMacroString (s, TH.info_of_tok id.TV.tok);
      find_macro_paren xs

  (* after case *)
  | TV.PToken ({TV.tok = TIdent (s,_)} as id)
      ::TV.PToken ({TV.tok = (TString _ | TMacroString _)})
      ::xs ->

      msg_stringification s;
      id.TV.tok <- TMacroString (s, TH.info_of_tok id.TV.tok);
      find_macro_paren xs

  (* iterator *)
  | TV.PToken ({TV.tok = TDefine _})::
    TV.PToken ({TV.tok = TIdentDefine (s,_)})::
    TV.Parenthised _::
    ((TV.PToken ({TV.tok = (Tfor _ | TMacroIterator _)})::
    TV.Parenthised _::
    TV.PToken ({TV.tok = TDefEOL _})::_) as xs) ->
      Data.add_special_name s Data.Iterator;
      find_macro_paren xs (* no need to recurse on define header *)

  (* recurse *)
  | (TV.PToken x)::xs -> find_macro_paren xs
  | (TV.Parenthised (xxs, info_parens))::xs ->
      xxs +> List.iter find_macro_paren;
      find_macro_paren xs





(* don't forget to recurse in each case *)
let rec find_macro_lineparen prev_line_end xs =
  match xs with
  | [] -> ()

  (* linuxext: ex: static [const] DEVICE_ATTR(); *)
  | (TV.Line
        (
          [TV.PToken ({TV.tok = Tstatic _});
           TV.PToken ({TV.tok = TIdent (s,_)} as macro);
           TV.Parenthised (xxs,info_parens);
           TV.PToken ({TV.tok = TPtVirg _});
          ]
        ))
    ::xs
    when (s ==~ regexp_macro) ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      find_macro_lineparen true (xs)

  (* the static const case *)
  | (TV.Line
        (
          [TV.PToken ({TV.tok = Tstatic _});
           TV.PToken ({TV.tok = Tconst _} as const);
           TV.PToken ({TV.tok = TIdent (s,_)} as macro);
           TV.Parenthised (xxs,info_parens);
           TV.PToken ({TV.tok = TPtVirg _});
          ]
            (*as line1*)

        ))
    ::xs
    when (s ==~ regexp_macro) ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      (* need retag this const, otherwise ambiguity in grammar
         21: shift/reduce conflict (shift 121, reduce 137) on Tconst
  	 decl2 : Tstatic . TMacroDecl TOPar argument_list TCPar ...
	 decl2 : Tstatic . Tconst TMacroDecl TOPar argument_list TCPar ...
	 storage_class_spec : Tstatic .  (137)
      *)
      const.TV.tok <- TMacroDeclConst (TH.info_of_tok const.TV.tok);

      find_macro_lineparen true (xs)


  (* same but without trailing ';'
   *
   * I do not put the final ';' because it can be on a multiline and
   * because of the way mk_line is coded, we will not have access to
   * this ';' on the next line, even if next to the ')' *)
  | (TV.Line
        ([TV.PToken ({TV.tok = Tstatic _});
          TV.PToken ({TV.tok = TIdent (s,_)} as macro);
          TV.Parenthised (xxs,info_parens);
        ]
        ))
    ::xs
    when s ==~ regexp_macro ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      find_macro_lineparen true (xs)


  (* on multiple lines *)
  | (TV.Line
        (
          (TV.PToken ({TV.tok = Tstatic _})::[]
          )))
    ::(TV.Line
          (
            [TV.PToken ({TV.tok = TIdent (s,_)} as macro);
             TV.Parenthised (xxs,info_parens);
             TV.PToken ({TV.tok = TPtVirg _});
            ]
          )
        )
    ::xs
    when (s ==~ regexp_macro) ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      find_macro_lineparen true (xs)


  | (TV.Line (* initializer case *)
        (
          TV.PToken ({TV.tok = Tstatic _}) ::
           TV.PToken ({TV.tok = TIdent (s,_)} as macro) ::
           TV.Parenthised (xxs,info_parens) ::
           TV.PToken ({TV.tok = TEq _}) :: rest
        ))
    ::xs
    when (s ==~ regexp_macro) ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      (* continue with the rest of the line *)
      find_macro_lineparen false ((TV.Line(rest))::xs)


  | (TV.Line (* multi-line initializer case *)
        (
          (TV.PToken ({TV.tok = Tstatic _})::[]
          )))
    ::(TV.Line
        (
          TV.PToken ({TV.tok = Tstatic _}) ::
           TV.PToken ({TV.tok = TIdent (s,_)} as macro) ::
           TV.Parenthised (xxs,info_parens) ::
           TV.PToken ({TV.tok = TEq _}) :: rest
        ))
    ::xs
    when (s ==~ regexp_macro) ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      (* continue with the rest of the line *)
      find_macro_lineparen false ((TV.Line(rest))::xs)


  (* linuxext: ex: DECLARE_BITMAP();
   *
   * Here I use regexp_declare and not regexp_macro because
   * Sometimes it can be a FunCallMacro such as DEBUG(foo());
   * Here we don't have the preceding 'static' so only way to
   * not have positive is to restrict to .*DECLARE.* macros.
   *
   * but there is a grammar rule for that, so don't need this case anymore
   * unless the parameter of the DECLARE_xxx are weird and can not be mapped
   * on a argument_list
   *)

  | (TV.Line
        ([TV.PToken ({TV.tok = TIdent (s,_)} as macro);
          TV.Parenthised (xxs,info_parens);
          TV.PToken ({TV.tok = TPtVirg _});
        ]
        ))
    ::xs
    when (s ==~ regexp_declare) ->

      msg_declare_macro s;
      let info = TH.info_of_tok macro.TV.tok in
      Data.add_special_name s Data.Declarer;
      macro.TV.tok <- TMacroDecl (Ast_c.str_of_info info, info);

      find_macro_lineparen true (xs)


  (* toplevel macros.
   * module_init(xxx)
   *
   * Could also transform the TIdent in a TMacroTop but can have false
   * positive, so easier to just change the TCPar and so just solve
   * the end-of-stream pb of ocamlyacc
   *)
  | (TV.Line
        ([TV.PToken ({TV.tok = TIdent (s,ii); TV.col = col1; where = ctx} as _macro);
          TV.Parenthised (xxs,info_parens);
        ] as _line1
        ))
    ::xs when col1 = 0
    ->
      let condition =
        (* to reduce number of false positive *)
        (match xs with
        | (TV.Line (TV.PToken ({TV.col = col2 } as other)::restline2))::_ ->
            TH.is_eof other.TV.tok || (col2 = 0 &&
             (match other.TV.tok with
             | TOBrace _ -> false (* otherwise would match funcdecl *)
             | TCBrace _ when ctx <> TV.InFunction -> false
             | TPtVirg _
             | TDotDot _
               -> false
             | tok when TH.is_binary_operator tok -> false

             | _ -> true
             )
            )
        | _ -> false
        )
      in
      if condition
      then begin

          msg_macro_toplevel_noptvirg s;
          (* just to avoid the end-of-stream pb of ocamlyacc  *)
          let tcpar = Common.last info_parens in
          tcpar.TV.tok <- TCParEOL (TH.info_of_tok tcpar.TV.tok);

          (*macro.TV.tok <- TMacroTop (s, TH.info_of_tok macro.TV.tok);*)

        end;

       find_macro_lineparen true (xs)



  (* macro with parameters
   * ex: DEBUG()
   *     return x;
   *)
  | (TV.Line
        ([TV.PToken ({TV.tok = TIdent (s,ii); TV.col = col1; where = ctx} as macro);
          (TV.Parenthised (xxs,info_parens) as args);
        ] as _line1
        ))
    ::(TV.Line
          (TV.PToken ({TV.col = col2 } as other)::restline2
          ) as line2)
    ::xs
    when List.mem ctx [TV.InFunction;TV.InStruct;TV.NoContext]
    (* when s ==~ regexp_macro *)
    ->
      (* This can give a false positive for K&R functions if the function
         name is in the same column as the first parameter declaration. *)
      let condition =
        (col1 = col2 &&
            (match other.TV.tok with
            | TOBrace _ -> false (* otherwise would match funcdecl *)
            | TCBrace _ when ctx <> TV.InFunction -> false
            | TPtVirg _
            | TDotDot _
                -> false
            | tok when TH.is_binary_operator tok -> false

            | _ -> true
            )
        )
        ||
        (col2 <= col1 &&
              (match other.TV.tok, restline2 with
              | TCBrace _, _ when ctx = TV.InFunction -> true
              | Treturn _, _ -> true
              | Tif _, _ -> true
              | Telse _, _ -> true

              (* case of label, usually put in first line *)
              | TIdent _, (TV.PToken ({TV.tok = TDotDot _}))::_ ->
                  true


              | _ -> false
              )
          )

      in

      if condition
      then
        if col1 = 0 then ()
        else begin
          msg_macro_noptvirg s;
	  (match ctx with
	    TV.InStruct ->
              macro.TV.tok <- TMacroDecl (s, TH.info_of_tok macro.TV.tok);
              [args] +>
              TV.iter_token_paren (TV.set_as_comment Token_c.CppMacro)
	  | TV.InFunction | TV.NoContext ->
	      let contains_semicolon data =
		let res = ref false in
		TV.iter_token_paren
		  (function t ->
		    match t.TV.tok with
		      TPtVirg _ -> res := true
		    | _ -> ())
		  data;
		!res in
	      if contains_semicolon [args]
	      then
		begin
		  macro.TV.tok <- TMacroIdStmt (s, TH.info_of_tok macro.TV.tok);
		  [args] +>
		  TV.iter_token_paren (TV.set_as_comment Token_c.CppMacro)
		end
              else macro.TV.tok <- TMacroStmt (s, TH.info_of_tok macro.TV.tok)
	  | _ -> failwith "macro: not possible");

        end;

      find_macro_lineparen true (line2::xs)

  (* linuxext:? single macro
   * ex: LOCK
   *     foo();
   *     UNLOCK
   *
   * todo: factorize code with previous rule ?
   *)
  | (TV.Line
        ([TV.PToken ({TV.tok = TIdent (s,ii); TV.col = col1; where = ctx} as macro);
        ] as _line1
        ))
    ::(TV.Line
          (TV.PToken ({TV.col = col2 } as other)::restline2
          ) as line2)
    ::xs when ctx = TV.InFunction (* MacroStmt doesn't make sense otherwise *)
	      && prev_line_end ->
    (* when s ==~ regexp_macro *)

      let condition =
        (col1 = col2 &&
            col1 <> 0 && (* otherwise can match typedef of fundecl*)
            (match other.TV.tok with
            | TPtVirg _ -> false
            | TOr _ -> false
            | TCBrace _ when ctx <> TV.InFunction -> false
            | tok when TH.is_binary_operator tok -> false

            | _ -> true
            )) ||
          (col2 <= col1 &&
              (match other.TV.tok with
              | TCBrace _ when ctx = TV.InFunction -> true
              | Treturn _ -> true
              | Tif _ -> true
              | Telse _ -> true
              | _ -> false
              ))
      in

      if condition
      then begin
        msg_macro_noptvirg_single s;
        macro.TV.tok <- TMacroIdStmt (s, TH.info_of_tok macro.TV.tok);
      end;
      find_macro_lineparen true (line2::xs)

  | (TV.Line line)::xs ->
      let prev_line_end =
	match List.rev line with
	  (TV.PToken {TV.tok = TPtVirg _} | TV.PToken {TV.tok = TOBrace _}
	| TV.PToken {TV.tok = TCBrace _} | TV.PToken {TV.tok = TDotDot _}) :: _ ->
	    true
	| _ -> false in
      find_macro_lineparen prev_line_end xs



(* ------------------------------------------------------------------------- *)
(* define tobrace init *)
(* ------------------------------------------------------------------------- *)

let find_define_init_brace_paren xs =
 let rec aux xs =
  match xs with
  | [] -> ()

  (* mainly for firefox *)
  | (TV.PToken {TV.tok = TDefine _})
    ::(TV.PToken {TV.tok = TIdentDefine (s,_)})
    ::(TV.PToken ({TV.tok = TOBrace i1} as tokbrace))
    ::(TV.PToken tok2)
    ::(TV.PToken tok3)
    ::xs ->
      let is_init =
        match tok2.TV.tok, tok3.TV.tok with
        | TInt _, TComma _ -> true
        | TString _, TComma _ -> true
        | TIdent _, TComma _ -> true
        | _ -> false

      in
      if is_init
      then begin
        pr2_cpp("found define initializer: " ^s);
        tokbrace.TV.tok <- TOBraceDefineInit i1;
      end;

      aux xs

  (* mainly for linux, especially in sound/ *)
  | (TV.PToken {TV.tok = TDefine _})
    ::(TV.PToken {TV.tok = TIdentDefine (s,_)})
    ::(TV.Parenthised(xxx, info_parens))
    ::(TV.PToken ({TV.tok = TOBrace i1} as tokbrace))
    ::(TV.PToken tok2)
    ::(TV.PToken tok3)
    ::xs ->
      let is_init =
        match tok2.TV.tok, tok3.TV.tok with
        | TInt _, TComma _ -> true
        | TDot _, TIdent _ -> true
        | TIdent _, TComma _ -> true
        | _ -> false

      in
      if is_init
      then begin
        pr2_cpp("found define initializer with param: " ^ s);
        tokbrace.TV.tok <- TOBraceDefineInit i1;
      end;

      aux xs



  (* recurse *)
  | (TV.PToken x)::xs -> aux xs
  | (TV.Parenthised (xxs, info_parens))::xs ->
      (* not need for tobrace init:
       *  xxs +> List.iter aux;
       *)
      aux xs
 in
 aux xs


(* ------------------------------------------------------------------------- *)
(* action *)
(* ------------------------------------------------------------------------- *)

(* obsolete now with macro expansion ? get some regression if comment.
 * todo: if do bad decision here, then it can influence other phases
 * and make it hard to parse. So maybe when have a parse error, should
 * undo some of the guess those heuristics have done, and restore
 * the original token value.
 *)

let rec find_actions = function
  | [] -> ()

  | TV.PToken ({TV.tok = TIdent (s,ii)})
    ::TV.Parenthised (xxs,info_parens)
    ::xs ->
      find_actions xs;
      xxs +> List.iter find_actions;
      let modified = find_actions_params xxs in
      if modified
      then msg_macro_higher_order s

  | x::xs ->
      find_actions xs

and find_actions_params xxs =
  xxs +> List.fold_left (fun acc xs ->
    let toks = TV.tokens_of_paren xs in
    if toks +> List.exists (fun x -> TH.is_statement x.TV.tok)
      (* undo:  && List.length toks > 1
       * good for sparse, not good for linux
       *)
    then begin
      xs +> TV.iter_token_paren (fun x ->
        if TH.is_eof x.TV.tok
        then
          (* certainly because paren detection had a pb because of
           * some ifdef-exp. Do similar additional checking than
           * what is done in TV.set_as_comment.
           *)
          pr2 "PB: weird, I try to tag an EOF token as an action"
        else
          (* cf tests-bis/no_cpar_macro.c *)
          if TH.is_eom x.TV.tok
          then
            pr2 "PB: weird, I try to tag an EOM token as an action"
          else
            x.TV.tok <- TAction (TH.info_of_tok x.TV.tok);
      );
      true (* modified *)
    end
    else acc
  ) false



(* ------------------------------------------------------------------------- *)
(* main fix cpp function *)
(* ------------------------------------------------------------------------- *)

let filter_cpp_stuff xs =
  List.filter
    (function x ->
      (match x.TV.tok with
        | tok when TH.is_comment tok -> false
        (* don't want drop the define, or if drop, have to drop
         * also its body otherwise the line heuristics may be lost
         * by not finding the TDefine in column 0 but by finding
         * a TDefineIdent in a column > 0
         *)
        | Parser_c.TDefine _ -> true
        | tok when TH.is_cpp_instruction tok -> false
        | _ -> true
        ))
    xs

let insert_virtual_positions l =
  let strlen x = String.length (Ast_c.str_of_info x) in
  let rec loop prev offset acc = function
      [] -> List.rev acc
    | x::xs ->
	let ii = TH.info_of_tok x in
	let inject pi =
	  TH.visitor_info_of_tok (function ii -> Ast_c.rewrap_pinfo pi ii) x in
	match Ast_c.pinfo_of_info ii with
	  Ast_c.OriginTok pi ->
	    let prev = Ast_c.parse_info_of_info ii in
	    loop prev (strlen ii) (x::acc) xs
	| Ast_c.ExpandedTok (pi,_) ->
            let x' = inject (Ast_c.ExpandedTok (pi,(prev,offset))) in
	    loop prev (offset + (strlen ii)) (x'::acc) xs
	| Ast_c.FakeTok (s,_,befaft) ->
            let x' = inject (Ast_c.FakeTok (s,(prev,offset),befaft)) in
	    loop prev (offset + (strlen ii)) (x'::acc) xs
	| Ast_c.AbstractLineTok _ -> failwith "abstract not expected" in
  let rec skip_fake = function
    | [] -> []
    | x::xs ->
	let ii = TH.info_of_tok x in
	match Ast_c.pinfo_of_info ii with
	| Ast_c.OriginTok pi ->
	    let prev = Ast_c.parse_info_of_info ii in
            let res = loop prev (strlen ii) [] xs  in
            x::res
	| _ -> x::skip_fake xs in
  skip_fake l

(* ------------------------------------------------------------------------- *)

let fix_tokens_cpp2 ~macro_defs err_pos tokens =
  let tokens2 = ref (tokens +> Common.acc_map TV.mk_token_extended) in

  begin
    (* the order is important, if you put the action heuristic first,
     * then because of ifdef, can have not closed paren
     * and so may believe that higher order macro
     * and it will eat too much tokens. So important to do
     * first the ifdef.
     *
     * I recompute multiple times cleaner cos the mutable
     * can have be changed and so may have more comments
     * in the token original list.
     *
     *)

    commentize_skip_start_to_end !tokens2;

    (* ifdef *)
    let cleaner = !tokens2 +> List.filter (fun x ->
      (* is_comment will also filter the TCommentCpp created in
       * commentize_skip_start_to_end *)
      not (TH.is_comment x.TV.tok) (* could filter also #define/#include *)
    ) in
    let ifdef_grouped = TV.mk_ifdef cleaner in
    set_ifdef_parenthize_info ifdef_grouped;

    find_ifdef_funheaders ifdef_grouped;
    find_ifdef_bool       ifdef_grouped;
    find_ifdef_mid        ifdef_grouped;
    (* change order ? maybe cparen_else heuristic make some of the funheaders
     * heuristics irrelevant ?
     *)
    find_ifdef_cparen_else        ifdef_grouped;
    adjust_inifdef_include ifdef_grouped;


    (* macro 1 *)
    let cleaner = !tokens2 +> filter_cpp_stuff in

    let paren_grouped = TV.mk_parenthised  cleaner in
    Cpp_token_c.apply_macro_defs
      ~msg_apply_known_macro
      ~msg_apply_known_macro_hint
      macro_defs err_pos paren_grouped;
    (* because the before field is used by apply_macro_defs *)
    tokens2 := TV.rebuild_tokens_extented !tokens2;

    (* tagging contextual info (InFunc, InStruct, etc). Better to do
     * that after the "ifdef-simplification" phase.
     *)
    let cleaner = !tokens2 +> List.filter (fun x ->
      not (TH.is_comment x.TV.tok) (* could filter also #define/#include *)
    ) in

    let brace_grouped = TV.mk_braceised cleaner in
    TV.set_context_tag   brace_grouped;

    (* macro *)
    let cleaner = !tokens2 +> filter_cpp_stuff in

    let paren_grouped      = TV.mk_parenthised  cleaner in
    let line_paren_grouped = TV.mk_line_parenthised paren_grouped in
    find_define_init_brace_paren paren_grouped;
    find_string_macro_paren paren_grouped;
    find_macro_lineparen    true line_paren_grouped;
    find_macro_paren        paren_grouped;


    (* obsolete: actions ? not yet *)
    let cleaner = !tokens2 +> filter_cpp_stuff in
    let paren_grouped = TV.mk_parenthised  cleaner in
    find_actions  paren_grouped;

    insert_virtual_positions (!tokens2 +> Common.acc_map (fun x -> x.TV.tok))
  end

let time_hack1 ~macro_defs a =
  Common.profile_code_exclusif "HACK" (fun () -> fix_tokens_cpp2 ~macro_defs a)

let fix_tokens_cpp ~macro_defs err_pos a =
  Common.profile_code "C parsing.fix_cpp"
    (fun () -> time_hack1 ~macro_defs err_pos a)



let can_be_on_top_level tl =
  match tl with
  | Tstruct _
  | Ttypedef _
  | TDefine _
  | TIfdef _
  | TIfdefelse _
  | TIfdefelif _
  | TIfdefBool _
  | TIfdefMisc _
  | TIfdefVersion _
  | TEndif _ -> true
  | _ -> false


(*****************************************************************************)
(* Lexing with lookahead *)
(*****************************************************************************)

(* Why using yet another parsing_hack technique ? The fix_xxx where do
 * some pre-processing on the full list of tokens is not enough ?
 * No cos sometimes we need more contextual info, and even if
 * set_context() tries to give some contextual info, it's not completely
 * accurate so the following code give yet another alternative, yet another
 * chance to transform some tokens.
 *
 * todo?: maybe could try to get rid of this technique. Maybe a better
 * set_context() would make possible to move this code using a fix_xx
 * technique.
 *
 * LALR(k) trick. We can do stuff by adding cases in lexer_c.mll, but
 * it is more general to do it via my LALR(k) tech. Because here we can
 * transform some token give some context information. So sometimes it
 * makes sense to transform a token in one context, sometimes not, and
 * lex can not provide us this context information. Note that the order
 * in the pattern matching in lookahead is important. Do not cut/paste.
 *
 * Note that in next there is only "clean" tokens, there is no comment
 * or space tokens. This is done by the caller.
 *
 *)

open Lexer_parser (* for the fields of lexer_hint type *)

let not_struct_enum = function
  | (Parser_c.Tstruct _ | Parser_c.Tunion _ | Parser_c.Tenum _)::_ -> false
  | (Parser_c.TIdent _)::
    (Parser_c.Tstruct _ | Parser_c.Tunion _ | Parser_c.Tenum _)::_ -> false
  | _ -> true

let is_struct_enum = function
  | (Parser_c.TIdent _)::
    (Parser_c.Tstruct _ | Parser_c.Tunion _ | Parser_c.Tenum _)::_ -> true
  | _ -> false

let not_opar = function
    TOPar _ -> false
  | _ -> true

let not_pragma = function
    (Parser_c.TPragma _)::_ -> false
  | _ -> true

let pointer ?(followed_by=fun _ -> true)
    ?(followed_by_more=fun _ -> true) ts =
  let rec loop ts =
    match ts with
    | TMul _ :: rest -> loop rest
    | TAnd _ :: rest when !Flag.c_plus_plus <> Flag.Off -> loop rest
    | TAndLog _ :: rest when !Flag.c_plus_plus <> Flag.Off -> loop rest
    | t :: ts' -> followed_by t && followed_by_more ts'
    | [] -> failwith "unexpected end of token stream" in
  match ts with
  | TMul _ :: rest -> loop rest
  | TAnd _ :: rest when !Flag.c_plus_plus <> Flag.Off -> loop rest
  | TAndLog _ :: rest when !Flag.c_plus_plus <> Flag.Off -> loop rest
  | _ -> false

let ident = function
    TIdent _ -> true
  | TMacroStmt _ -> true
  | TMacroIdStmt _ -> true
  | _ -> false

let is_type = function
  | TypedefIdent _
  | Tvoid _
  | Tchar _
  | Tfloat _
  | Tdouble _

  | Tsize_t _
  | Tssize_t _
  | Tptrdiff_t _

  | Tint _
  | Tlong _
  | Tshort _ -> true
  | _ -> false

let is_type_qualif = function
  | Tconst _
  | Tvolatile _ -> true
  | _ -> false

let is_storage_spec = function
  | Tstatic _
  | Tregister _
  | Textern _
  | Tauto _ -> true
  | _ -> false

let rec is_idents ?(followed_by=fun _ -> true) ts =
  let rec loop l =
    match l with
    | x::xs when ident x -> loop xs
    | x::xs -> followed_by x
    | [] -> failwith "unexpected end of token stream" in
  match ts with
  | x::xs when ident x -> loop xs
  | x::xs -> followed_by x
  | _ -> false

let is_cparen = function (TCPar _) -> true | _ -> false
let is_oparen = function (TOPar _) -> true | _ -> false

let rec not_has_type_before f xs =
  match xs with
  | [] -> raise (Impossible 666)
  | x :: xs ->
      if f x then
	true
      else if is_type x then
	false
      else
	not_has_type_before f xs

(* This function is inefficient, because it will look over a K&R header,
or function prototype multiple times.  At least when we see a , and are in a
parameter list, we know we will eventually see a close paren, and it
should come fairly soon. *)
let k_and_r l =
  let l1 = drop_until is_cparen l in
  match l1 with
    (TCPar _) :: (TOCro _) :: _ -> false
  | (TCPar _) :: _ -> true
  | _ -> false

(* (a)(b) is ambiguous, because (a) could be a function name or a cast.
At this point, we just see an ident for a; we don't know if it is eg a local
variable.  This function sees at least if b is the only argument, ie there
are no commas at top level *)
let paren_before_comma l =
  let rec loop level = function
      [] -> false
    | (TComma _)::_ when level = 1 -> false
    | (TCPar _)::_ when level = 1 -> true
    | (TCPar _)::rest -> loop (level-1) rest
    | (TOPar _)::rest -> loop (level+1) rest
    | x::rest -> loop level rest in
  loop 0 l

let lookahead2 ~pass next before =
  match (next, before) with
  | t::_, (TOPar il)::TIdent(s,i2)::(Tstatic _|Tconst _|Textern _)::_
    when is_macro s && (LP.current_context() = LP.InParameter) ->
      (* swap context, not really in a parameter list *)
      LP.pop_context(); LP.push_context (LP.current_context());
      t

  (* c++ hacks *)
  | TIdent(s,i1)::_,TEq _ :: _ :: Ttypename _ :: _
      when !Flag.c_plus_plus <> Flag.Off ->
	msg_typedef s i1 40; LP.add_typedef_root s i1;
	TypedefIdent (s,i1)
  | TIdent(s,i1)::TTemplateStart i2::_,_
      when !Flag.c_plus_plus <> Flag.Off &&
	(List.mem (LP.current_context()) [LP.InParameter;LP.InStruct]) ->
	msg_typedef s i1 41; LP.add_typedef_root s i1;
	TypedefIdent (s,i1)
  | TIdent(s,i1)::(Tclass i2|Tstruct i2|Toperator i2)::_,_
      when !Flag.c_plus_plus <> Flag.Off ->
	msg_typedef s i1 42; LP.add_typedef_root s i1;
	TypedefIdent (s,i1)
  | TIdent(s,i1)::TOPar i2::x::_,_
      when !Flag.c_plus_plus <> Flag.Off &&
	(match x with TMul _ -> false (* fun ptr *) | _ -> true) ->
	TIdent (s,i1)
  (* yy xx(   and in function *)
  | TOPar i1::_,              TIdent(s,i2)::(TypedefIdent _|TTemplateEnd _|TautoType _)::_
      when !Flag.c_plus_plus <> Flag.Off &&
	(List.mem (LP.current_context ()) [LP.InFunction;LP.InStruct]) ->
        pr2_cpp("constructed_object: "  ^s);
        TOParCplusplusInit i1
  | TOPar i1::_,              TIdent(s,i2)::ptr
      when !Flag.c_plus_plus <> Flag.Off
	  && pointer ~followed_by:(function TypedefIdent _ -> true | _ -> false) ptr
	  && (LP.current_context () = (LP.InFunction)) ->
        pr2_cpp("constructed_object: "  ^s);
        TOParCplusplusInit i1
  | TypedefIdent(s,i)::TOPar i1::_,_
      when !Flag.c_plus_plus <> Flag.Off && (LP.current_context () = (LP.InFunction)) ->
	TIdent(s,i)

  | TIdent(s,i1)::rest,(TOPar _ | TComma _ | TTemplateStart _)::_
      when
	let arg_ender =
	  function TCPar _ | TComma _ | TTemplateEnd _ -> true | _ -> false in
	pointer ~followed_by:arg_ender rest ->
	msg_typedef s i1 180; LP.add_typedef_root s i1;
	TypedefIdent (s,i1)

  (*-------------------------------------------------------------*)
  (* typedef inference, parse_typedef_fix3 *)
  (*-------------------------------------------------------------*)
  (* xx xx *)
  | (TIdent(s,i1)::TIdent(s2,i2)::_ , _) when not_struct_enum before && s = s2
      && ok_typedef s
      (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
    ->
      (* parse_typedef_fix3:
       *    acpi_object		acpi_object;
       * etait mal parsé, car pas le temps d'appeler dt()  dans le type_spec.
       * Le parser en interne a deja appelé le prochain token pour pouvoir
       * decider des choses.
       *  => special case in lexer_heuristic, again
       *)
      if !Flag_parsing_c.debug_typedef
      then pr2 ("TYPEDEF: disable typedef cos special case: " ^ s);


      LP.disable_typedef();
      msg_typedef s i1 1; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

	(* christia *)

	(* xx xx *)
  | (TypedefIdent (s1, i1) :: _ , TypedefIdent (s2, i2) :: _) ->
      TIdent (s1, i1)

	(* xx * xx *)
  | (TypedefIdent (s1, i1) :: _ , ptr)
    when pointer ~followed_by:(function TypedefIdent _ -> true | _ -> false)  ptr  ->
      TIdent (s1, i1)

	(* extern "_" tt *)
  | ((TString ((s, _), i1) | TMacroString (s, i1)) :: _ , Textern _ :: _)
    when !Flag.c_plus_plus <> Flag.Off ->
	  TCommentCpp (Token_c.CppDirective, i1)

	(* ) const { *)
  | (Tconst i1 :: TOBrace _ :: _ , TCPar _ :: _)
    when !Flag.c_plus_plus <> Flag.Off ->
	  TCommentCpp (Token_c.CppDirective, i1)

  (* const ident const: ident must be a type *)
  | (TIdent (s, i1)::Tconst _::_,  Tconst _::_)
      when not_struct_enum before
      && ok_typedef s
        ->
      msg_typedef s i1 38; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

	(* xx const tt *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::type_::_  , _)
    when not_struct_enum before
	&& is_type type_ ->
	  TCommentCpp (Token_c.CppDirective, i1)

	(* xx struct *)
  | (TIdent (s, i1)::Tstruct _::_  , _)
    when not_struct_enum before ->
	  TCommentCpp (Token_c.CppDirective, i1)

	(* xx tt *)
  | (TIdent (s, i1)::type_::_  , _)
    when not_struct_enum before
	&& is_type type_
	&& s ==~ regexp_annot ->
      msg_attribute s;
      TMacroAttr (s,i1)

        (* tt xx yy ( : xx is an annot *)
(*| (TIdent (s, i1)::TIdent (s2, i2)::TOPar _::_, seen::_)
    when LP.current_context () = LP.InTopLevel
	&& (is_struct_enum before || is_type seen)
	&& s ==~ regexp_annot ->
	  TCommentCpp (Token_c.CppMacro, i1)
*)
        (* tt * xx yy ( : xx is an annot *)
  | (TIdent (s, i1)::TIdent (s2, i2)::TOPar _::_, ptr)
    when LP.current_context () = LP.InTopLevel
	&& pointer ptr
	&& s ==~ regexp_annot ->
      msg_attribute s;
      TMacroAttr (s,i1)

	(* tt xx yy; : yy is an annot *)
  | (TIdent (s, i1)::(TPtVirg _|TEq _)::_, TIdent (s2, i2)::type_::rest)
    when (is_struct_enum (type_::rest)
	|| is_type type_)
	&& s ==~ regexp_annot ->
      msg_attribute s;
      TMacroAttr (s,i1)

	(* tt * xx yy; : yy is an annot *)
  | (TIdent (s, i1)::(TPtVirg _|TEq _)::_, TIdent (s2, i2)::ptr)
    when pointer ptr
	&& s ==~ regexp_annot ->
      msg_attribute s;
      TMacroAttr (s,i1)

	(* tt xx yy; : yy is an annot, so xx is an ident *)
  | (TIdent (s, i1)::TIdent (s2, i2)::(TPtVirg _|TEq _)::_, seen::_)
    when (is_struct_enum before
	|| is_type seen || pointer before)
	&& not(s ==~ regexp_annot)
	&& s2 ==~ regexp_annot ->
	  TIdent (s, i1)
(*
	(* tt xx yy *)
  | (TIdent (s, i1)::TIdent (s2, i2)::_  , seen::_)
    when not_struct_enum before
	&& is_type seen ->
	  if is_macro s2 then
	    TIdent (s, i1)
	  else
	    TCommentCpp (Token_c.CppDirective, i1) *)

	(* tt xx yy *)
  | (TIdent (s, i1)::rest, _)
    when not_struct_enum before
	&& is_idents
           ~followed_by:
             (function x ->
                is_type x || is_storage_spec x || is_type_qualif x) rest
        && s ==~ regexp_annot ->
      msg_attribute s;
      TMacroAttr (s,i1)

  | (TIdent (s2, i2)::_  , TIdent (s, i1)::seen::_)
    when not_struct_enum before
	&& is_macro s2 && is_type seen ->
	  TCommentCpp (Token_c.CppDirective, i2)

	(* tt xx * *)
  | (TIdent (s, i1)::ptr  , seen::_)
    when not_struct_enum before
	&& pointer ptr && is_type seen ->
	  TCommentCpp (Token_c.CppDirective, i1)

	(* tt * xx yy *)
  | (TIdent (s, i1)::TIdent(s2, i2)::_  , ptr)
    when not_struct_enum before
	&& pointer ptr ->
	  if is_macro s2 then
	    TIdent (s, i1)
	  else
	    TCommentCpp (Token_c.CppDirective, i1)

	(* tt * xx yy *)
  | (TIdent(s2, i2)::_  , TIdent (s, i1)::ptr)
    when not_struct_enum before
	&& is_macro s2 && pointer ptr ->
	  TCommentCpp (Token_c.CppDirective, i2)

        (* exception to next rule *)
  | (TIdent(s2, i2)::TOPar _ :: _ , TIdent(s, i1)::seen::_)
    when not_struct_enum before
	&& is_macro s2 && is_type seen ->
	  TIdent(s2, i2)
	(* tt xx yy *)
  | (TIdent(s2, i2)::_  , TIdent(s, i1)::seen::_)
    when not_struct_enum before
	&& is_macro s2 && is_type seen ->
	  TCommentCpp (Token_c.CppDirective, i2)

  (*  xx * yy      AND  in paramdecl *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false) ptr
	&& ok_typedef s ->
      msg_typedef s i1 14; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  xx * yy      AND  in for range *)
  | (TIdent (s, i1)::ptr , TOPar _ :: Tfor _ :: _)
    when !Flag.c_plus_plus <> Flag.Off &&
         pointer ~followed_by:(function TIdent _ -> true | _ -> false) ptr &&
        ok_typedef s ->
      msg_typedef s i1 140; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx MM ( *)
  | (TIdent (s, i1)::TIdent (s2, i2)::TOPar _::_  , type_::_) when not_struct_enum before
      && ok_typedef s && is_macro s2 && is_type type_
        ->
	  TIdent (s, i1)

  (* xx yy zz : xx is a macro *)
  | (TIdent (s, i1)::TIdent (s2, i2)::TIdent(_,_)::_ , _)
    when not_struct_enum before
	&& ok_typedef s2
	&& is_known_typdef s2
        -> TCommentCpp(Token_c.CppMacro, i1)

  (* xx yy zz : xx is a typedef ident *)
  | (TIdent (s, i1)::TIdent (s2, i2)::TIdent(_,_)::_ , _)
    when not_struct_enum before
	&& ok_typedef s
        ->
      msg_typedef s i1 2; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx yy is a typedef ident *)
  | (TIdent (s, i1)::TQualId _::_ , _)
    when not_struct_enum before
	&& ok_typedef s
        ->
      msg_typedef s i1 212; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx yy * zz : xx is a macro *)
  | (TIdent (s, i1)::TIdent (s2, i2)::ptr , _)
    when pointer ~followed_by:(function TIdent _ -> true | _ -> false) ptr
	&& not_struct_enum before
	&& ok_typedef s2
	&& is_known_typdef s2
        -> TCommentCpp(Token_c.CppMacro, i1)

  (* xx yy * zz : xx is a typedef ident *)
  | (TIdent (s, i1)::TIdent (s2, i2)::ptr , _)
    when pointer ~followed_by:(function TIdent _ -> true | _ -> false) ptr
	&& not_struct_enum before
	&& ok_typedef s
        ->
      msg_typedef s i1 200; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx yy *)
  | (TIdent (s, i1)::TIdent (s2, i2)::rest  , _) when not_struct_enum before
	&& ok_typedef s && not (is_macro_paren s2 rest)
        ->
         (* && not_annot s2 BUT lead to false positive*)
      msg_typedef s i1 201; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (* xx inline *)
  | (TIdent (s, i1)::Tinline i2::_  , _) when not_struct_enum before
      && ok_typedef s
      ->

      msg_typedef s i1 3; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (* [,(] xx [,)] AND param decl *)
  | (TIdent (s, i1)::(((TComma _|TCPar _)::_) as rest) ,
     ((TComma _ |TOPar _)::_ as bef))
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && not (!Flag_parsing_c.prevent_kr) && LP.is_kr_possible()
      && k_and_r rest
      && not_has_type_before is_cparen rest
      && not_has_type_before is_oparen bef
      ->

	TKRParam(s,i1)

  (* for function prototypes *)
  | (TIdent (s, i1)::((TCPar _)::x::_) , (TComma _ |TOPar _)::_ )
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s && not_opar x (* ensure it is not the name of a fnptr *)
      ->

	msg_typedef s i1 4; LP.add_typedef_root s i1;
	TypedefIdent (s, i1)

  | (TIdent (s, i1)::((TComma _)::_) , (TComma _ |TOPar _)::_ )
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s
      ->

	msg_typedef s i1 4; LP.add_typedef_root s i1;
	TypedefIdent (s, i1)

  (* xx* [,)] *)
  (* specialcase:  [,(] xx* [,)] *)
  | (TIdent (s, i1)::ptr , (*(TComma _|TOPar _)::*)_ )
    when pointer ~followed_by:(function TComma _ |TCPar _ -> true | _ -> false) ptr
	&& not_struct_enum before
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
    ->

      msg_typedef s i1 5; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (* xx** [,)] *)
  (* specialcase:  [,(] xx** [,)] *)
  | (TIdent (s, i1)::TMul _::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
    ->

      msg_typedef s i1 6; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)



  (* xx const *   USELESS because of next rule ? *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::TMul _::_ , _ )
      when not_struct_enum before
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
      ->

      msg_typedef s i1 7; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx const *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::_ , _ )
      when not_struct_enum before
      && ok_typedef s
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      ->

      msg_typedef s i1 8; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (* xx * const *)
  | (TIdent (s, i1)::ptr , _ )
      when pointer ~followed_by:(function Tconst _ | Tvolatile _ | Trestrict _ -> true | _ -> false) ptr
	  && not_struct_enum before
	  && ok_typedef s
      ->

      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)

      msg_typedef s i1 9; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (* ( const xx)  *)
  | (TIdent (s, i1)::TCPar _::_,  (Tconst _ | Tvolatile _|Trestrict _)::TOPar _::_) when
      ok_typedef s ->

      msg_typedef s i1 10; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)



  (* ( xx ) [sizeof, ~] *)
  | (TIdent (s, i1)::TCPar _::(Tsizeof _|TTilde _)::_ ,     TOPar _::_ )
    when not_struct_enum before
      && ok_typedef s
    ->

      msg_typedef s i1 11; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* [(,] xx [   AND parameterdeclaration *)
  | (TIdent (s, i1)::TOCro _::_, (TComma _ |TOPar _)::_)
      when (LP.current_context() = LP.InParameter)
      && ok_typedef s
     ->

      msg_typedef s i1 12; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*------------------------------------------------------------*)
  (* if 'x*y' maybe an expr, maybe just a classic multiplication *)
  (* but if have a '=', or ','   I think not *)
  (*------------------------------------------------------------*)

  (* static xx * yy  *)
  | (TIdent (s, i1)::ptr ,
     (Tregister _|Tstatic _  |Tvolatile _|Tconst _|Trestrict _)::_) when
      pointer ~followed_by:(function TIdent _ -> true | _ -> false) ptr
	 && ok_typedef s
        ->

      msg_typedef s i1 13; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  TODO  xx * yy ; AND in start of compound element  *)


  (*  xx * yy,      AND  in paramdecl *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TComma _ :: _ -> true | _ -> false) ptr
	&& ok_typedef s
      ->

      msg_typedef s i1 14; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (*  xx * yy ;     AND in Toplevel, except when have = before  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , TEq _::_) ->
      TIdent (s, i1)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TPtVirg _ :: _ -> true | _ -> false) ptr
	&& (LP.is_top_or_struct (LP.current_context ()))
      ->

      msg_typedef s i1 15; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  xx * yy ,     AND in Toplevel  *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before && (LP.current_context () = LP.InTopLevel)
	&& ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TComma _ :: _ -> true | _ -> false) ptr
      ->

      msg_typedef s i1 16; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  xx * yy (     AND in Toplevel  *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before
	&& (LP.is_top_or_struct (LP.current_context ()))
	&& ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TOPar _ :: _ -> true | _ -> false) ptr
      ->

      msg_typedef s i1 17; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx * yy [ *)
  (* todo? enough ? cos in struct def we can have some expression ! *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before
	&& (LP.is_top_or_struct (LP.current_context ()))
	&& ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TOCro _ :: _ -> true | _ -> false) ptr
      ->

      msg_typedef s i1 18;  LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* u16: 10; in struct *)
  | (TIdent (s, i1)::TDotDot _::_ , (TOBrace _ | TPtVirg _)::_)
    when       (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s
      ->

      msg_typedef s i1 19;  LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


    (*  why need TOPar condition as stated in preceding rule ? really needed ? *)
    (*   YES cos at toplevel can have some expression !! for instance when *)
    (*   enter in the dimension of an array *)
    (*
      | (TIdent s::TMul::TIdent s2::_ , _)
      when (take_safe 1 !passed_tok <> [Tstruct] &&
      (take_safe 1 !passed_tok <> [Tenum]))
      &&
      !LP._lexer_hint = Some LP.Toplevel ->
      msg_typedef s 20; LP.add_typedef_root s i1;
      TypedefIdent s
     *)

  (*  xx * yy =  *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before
	&& ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TEq _ :: _ -> true | _ -> false) ptr
      ->

      msg_typedef s i1 21; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (*  xx * yy)      AND in paramdecl *)
  | (TIdent (s, i1)::ptr , _)
    when not_struct_enum before && (LP.current_context () = LP.InParameter)
	&& ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TCPar _ :: _ -> true | _ -> false) ptr
        ->

      msg_typedef s i1 22; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (*  xx * yy; *) (* wrong ? *)
  | (TIdent (s, i1)::ptr ,
     (TOBrace _| TPtVirg _)::_) when not_struct_enum before
	&& ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TPtVirg _ :: _ -> true | _ -> false) ptr
        ->

      msg_typedef s i1 23;  LP.add_typedef_root s i1;
      msg_maybe_dangereous_typedef s;
      TypedefIdent (s, i1)


  (*  xx * yy,  and ';' before xx *) (* wrong ? *)
  | (TIdent (s, i1)::ptr ,
     (TOBrace _| TPtVirg _)::_) when
      ok_typedef s
	&& pointer ~followed_by:(function TIdent _ -> true | _ -> false)
              ~followed_by_more:(function TComma _ :: _ -> true | _ -> false) ptr
    ->

      msg_typedef s i1 24; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (* xx_t * yy *)
  | (TIdent (s, i1)::ptr , _)
      when s ==~ regexp_typedef && not_struct_enum before
        (* struct user_info_t sometimes *)
	&& ok_typedef s
        && pointer ~followed_by:(function TIdent _ -> true | _ -> false) ptr
        ->

      msg_typedef s i1 25; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  xx ** yy *)  (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before
	&& (LP.current_context() = LP.InParameter)
	&& ok_typedef s
    ->

      msg_typedef s i1 26; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  xx ** yy *)  (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , (TOBrace _ | TPtVirg _)::_)
    when not_struct_enum before
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
	&& ok_typedef s
	(* christia : this code catches 'a * *b' which is wrong
	 *)
    ->

      msg_typedef s i1 26; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* xx *** yy *)
  | (TIdent (s, i1)::TMul _::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before
      && ok_typedef s
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      ->

      msg_typedef s i1 27; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (*  xx ** ) *)
  | (TIdent (s, i1)::TMul _::TMul _::TCPar _::_ , _)
    when not_struct_enum before
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
      ->

      msg_typedef s i1 28; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)



  (* ----------------------------------- *)
  (* old: why not do like for other rules and start with TIdent ?
   * why do TOPar :: TIdent :: ..., _  and not TIdent :: ...,  TOPAr::_ ?
   * new: prefer now start with TIdent because otherwise the add_typedef_root
   * may have no effect if in second pass or if have disable the add_typedef.
   *)

  (*  (xx) yy *)
  | (TIdent (s, i1)::TCPar i2::
     (TIdent (_,i3)|TInt (_,i3)|TChar (_,i3)|TString (_,i3)|TFloat (_,i3))::_ ,
    (TOPar info)::x::_)
    when not (TH.is_stuff_taking_parenthized x) (* &&
      Ast_c.line_of_info i2 = Ast_c.line_of_info i3 - why useful?
      *)
      && ok_typedef s
      && not (ident x) (* possible K&R declaration *)
      ->

      msg_typedef s i1 29; LP.add_typedef_root s i1;
      (*TOPar info*)
      TypedefIdent (s, i1)


  (* (xx) (    yy)
   * but false positif: typedef int (xxx_t)(...), so do specialisation below.
   *)
  (*
  | (TIdent (s, i1)::TCPar _::TOPar _::_ , (TOPar info)::x::_)
    when not (TH.is_stuff_taking_parenthized x)
      && ok_typedef s
        ->
      msg_typedef s 30; LP.add_typedef_root s i1;
      (* TOPar info *)
      TypedefIdent (s, i1)
  *)
  (* special case:  = (xx) (    yy) *)
  | (TIdent (s, i1)::TCPar _::((TOPar _::_) as rest) ,
    (TOPar info)::(TEq _ |TEqEq _)::_)
    when ok_typedef s && paren_before_comma rest
        ->

      msg_typedef s i1 31; LP.add_typedef_root s i1;
      (* TOPar info *)
      TypedefIdent (s, i1)


  (*  (xx * ) yy *)
  | (TIdent (s, i1)::ptr, (TOPar info)::_)
    when ok_typedef s
	&& pointer ~followed_by:(function TCPar _ -> true | _ -> false)
              ~followed_by_more:(function TIdent _ :: _ -> true | _ -> false) ptr
        ->

      msg_typedef s i1 32; LP.add_typedef_root s i1;
      (*TOPar info*)
      TypedefIdent (s,i1)


  (* (xx){ ... }  constructor *)
  | (TIdent (s, i1)::TCPar _::TOBrace _::_ , TOPar _::x::_)
      when (*s ==~ regexp_typedef && *)
	!Flag.c_plus_plus = Flag.Off (* in C++ can be an initializer list *)
	&& not (TH.is_stuff_taking_parenthized x)
        && ok_typedef s
        ->

      msg_typedef s i1 33; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


        (* can have sizeof on expression
           | (Tsizeof::TOPar::TIdent s::TCPar::_,   _) ->
           msg_typedef s; LP.add_typedef_root s i1;
           Tsizeof
         *)


  (* ----------------------------------- *)
  (* x ( *y )(params),  function pointer *)
  | (TIdent (s, i1)::TOPar _::TMul _::TIdent _::TCPar _::TOPar _::_,  _)
      when not_struct_enum before
      && ok_typedef s
        ->

      msg_typedef s i1 34; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* x* ( *y )(params),  function pointer 2 *)
  | (TIdent (s, i1)::TMul _::TOPar _::TMul _::TIdent _::TCPar _::TOPar _::_,  _)
      when not_struct_enum before
      && ok_typedef s
        ->

      msg_typedef s i1 35; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* x ( *const y )(params),  function pointer *)
  (* This means that the function pointer is constant *)
  | (TIdent (s, i1)::TOPar _::TMul _::Tconst _::TIdent _::TCPar _::TOPar _::_,
     _)
      when not_struct_enum before
      && ok_typedef s
        ->

      msg_typedef s i1 36; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)

  (* x* ( *const y )(params),  function pointer 2 *)
  | (TIdent (s, i1)::TMul _::TOPar _::TMul _::Tconst _::TIdent _::TCPar _::
     TOPar _::_,  _)
      when not_struct_enum before
      && ok_typedef s
        ->

      msg_typedef s i1 37; LP.add_typedef_root s i1;
      TypedefIdent (s, i1)


  (*-------------------------------------------------------------*)
  (* CPP *)
  (*-------------------------------------------------------------*)
  | ((TIfdef (_,_,ii) |TIfdefelse (_,ii) |TIfdefelif (_,_,ii) |TEndif (_,ii) |
      TIfdefBool (_,_,ii)|TIfdefMisc(_,_,ii)|TIfdefVersion(_,_,ii))
        as x)
    ::_, _
      ->

      (*
      if not !Flag_parsing_c.ifdef_to_if
      then TCommentCpp (Ast_c.CppIfDirective, ii)
      else
      *)
      (* not !LP._lexer_hint.toplevel *)
      if !Flag_parsing_c.ifdef_directive_passing
        || (pass >= 2)
      then begin

        if (LP.current_context () = LP.InInitializer)
        then begin
          pr2_cpp "In Initializer passing"; (* cheat: don't count in stat *)
          incr Stat.nIfdefInitializer;
        end else begin
          pr2_cpp("IFDEF: or related inside function. I treat it as comment");
          incr Stat.nIfdefPassing;
        end;
        let x =
          match x with
            TIfdef _ | TIfdefMisc _ | TIfdefVersion _ -> Token_c.IfDef
          | TIfdefBool _ -> Token_c.IfDef0
          | TIfdefelse _ | TIfdefelif _ -> Token_c.Else
          | TEndif _ -> Token_c.Endif
          | _ -> Token_c.Other in (* not possible here *)
        TCommentCpp (Token_c.CppIfDirective x, ii)
        end
      else x

  | (TUndef (ii) as x)::_, _
      ->
        if (pass >= 2)
        then begin
          pr2_cpp("UNDEF: I treat it as comment");
          TCommentCpp (Token_c.CppDirective, ii)
        end
        else x

  | (TCppDirectiveOther (ii) as x)::_, _
      ->
        if (pass >= 2)
        then begin
          pr2_cpp ("OTHER directive: I treat it as comment");
          TCommentCpp (Token_c.CppDirective, ii)
        end
        else x

   (* If ident contain a for_each, then certainly a macro. But to be
    * sure should look if there is a '{' after the ')', but it requires
    * to count the '('. Because this can be expensive, we do that only
    * when the token contains "for_each".
    *)
  | (TIdent (s, i1)::TOPar _::rest, _)
     when not (LP.current_context () = LP.InTopLevel)
      (* otherwise a function such as static void loopback_enable(int i) {
       * will be considered as a loop
       *)
        ->
	  let res =
	    try Some(Data.get_special_name s)
	    with _ -> None in
	  (match res with
	    Some Data.Iterator -> TMacroIterator (s, i1)
	  | _ ->
	      if s ==~ regexp_foreach &&
		is_really_foreach (Common.take_safe forLOOKAHEAD rest)
	      then
		begin
		  msg_foreach s;
		  Data.add_special_name s Data.Iterator;
		  TMacroIterator (s, i1)
		end
	      else TIdent (s, i1))

(*  | (TIdent(s1,i1)::(TPtVirg(ii2)|TEq(ii2))::rest,
     TIdent(s2,i2)::TIdent(s3,i3)::_)
      when (LP.current_context () = LP.InTopLevel &&
	s1 ==~ regexp_annot) ->
	  msg_attribute s1;
	  TMacroAttr (s1, i1)
*)

  (* after an array reference and before ; or = *)
  | (TIdent(s1,i1)::(TPtVirg(ii2)|TEq(ii2))::rest,TCCro(i2)::_)
      when LP.current_context () = LP.InTopLevel &&
	s1 ==~ regexp_annot ->
	  msg_attribute s1;
	  TMacroAttr (s1, i1)

(*
  | (TIdent(s1,i1)::TOBrace(ii2)::rest,TCPar(i2)::_)
      when LP.current_context () = LP.InTopLevel &&
	s1 ==~ regexp_annot ->
	  msg_attribute s1;
	  TMacroAttr (s1, i1) *)

  | (TMacroAttr(s1,i1)::(TPtVirg(ii2)|TEq(ii2)|TOBrace(ii2))::rest,_)
      ->
	  TMacroAttr (s1, i1)

  | (TMacroAttr(s1,i1)::TOPar(ii2)::t::rest,_)
      when match t with TMul(_) -> false | _ -> true ->
	  TMacroAttrArgs (s1, i1)

(*  (* christia: here insert support for macros on top level *)
  | TIdent (s, ii) :: tl :: _, _ when
    can_be_on_top_level tl && LP.current_context () = InTopLevel ->
      pr2_cpp ("'" ^ s ^ "' looks like a macro, I treat it as comment");
      TCommentCpp (Token_c.CppDirective, ii)
*)

(*-------------------------------------------------------------*)
 | v::xs, _ -> v
 | _ -> raise (Impossible 93)

let lookahead ~pass a b =
  Common.profile_code "C parsing.lookahead" (fun () -> lookahead2 ~pass a b)

(*****************************************************************************)
(* Ifdef-statementize *)
(*****************************************************************************)

(*
 * cpp_ifdef_statementize: again better to separate concern and in parser
 *  just add the directives in a flat way (IfdefStmt) and later do more
 *  processing and transform them in a tree with some IfdefStmt2.
 *)

let is_ifdef_and_same_tag (tag : Ast_c.matching_tag)
                          (x : Ast_c.statement_sequencable)
                          :bool
  = match x with
  | IfdefStmt (IfdefDirective ((_, tag2),_)) ->
      tag = tag2
  | StmtElem _ | CppDirectiveStmt _ -> false
  | IfdefStmt2 _ -> raise (Impossible 77)

(* What if I skipped in the parser only some of the ifdef elements
 * of the same tag. Once I passed one, I should pass all of them and so
 * at least should detect here that one tag is not "valid". Maybe in the parser
 * can return or marked some tags as "partially_passed_ifdef_tag".
 * Maybe could do in ast_c a MatchingTag of int * bool ref (* one_was_passed *)
 * where the ref will be shared by the ifdefs with the same matching tag
 * indice. Or simply count  the number of directives with the same tag and
 * put this information in the tag. Hence the total_with_this_tag below.
 *)
let should_ifdefize (tag,ii) ifdefs_directives _xxs =
  let IfdefTag (_tag, total_with_this_tag) = tag in

  if total_with_this_tag <> List.length ifdefs_directives
  then begin
    let strloc = Ast_c.strloc_of_info (List.hd ii) in
    pr2 (spf "CPPASTC: can not ifdefize ifdef at %s" strloc);
    pr2 "CPPASTC: some of its directives were passed";
    false
  end else
    (* todo? put more condition ? don't ifdefize declaration ? *)
    true

(* return a triple, (ifdefs directive * grouped xs * remaining sequencable)
 * XXX1 XXX2 elsif YYY1 else ZZZ1 endif WWW1 WWW2
 * => [elsif, else, endif], [XXX1 XXX2; YYY1; ZZZ1], [WWW1 WWW2]
 *)
let group_ifdef : Ast_c.matching_tag
                  -> Ast_c.statement_sequencable list
                  -> Ast_c.ifdef_directive list
                      * Ast_c.statement_sequencable list list
                      * Ast_c.statement_sequencable list
  = fun tag xs ->
  let (xxs, remaining) = group_by_post (is_ifdef_and_same_tag tag) xs in

  (* TODO: Should replace this with a proper assert.
   * - Iago Abal
   *)
  xxs +> List.map (fun (_,x) ->
    match x with
    | IfdefStmt y -> y
    | StmtElem _ | CppDirectiveStmt _ | IfdefStmt2 _ -> raise (Impossible 78)
  ),
  xxs +> List.map fst,
  remaining


let cpp_ifdef_statementize (ast :toplevel list) :toplevel list =
  Visitor_c.vk_program_s { Visitor_c.default_visitor_c_s with
    Visitor_c.kstatementseq_list_s = (fun (k, bigf) xs ->
      let rec aux : statement_sequencable list -> statement_sequencable list
        = function
        | [] -> []
        | stseq::xs ->
            (match stseq with
            | StmtElem st ->
                Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs
            | CppDirectiveStmt directive ->
                Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs
            | IfdefStmt ifdef ->
                (match ifdef with
                | IfdefDirective ((Ast_c.Ifdef _,tag),ii) ->

                    let (restifdefs, xxs, xs') = group_ifdef tag xs in
                    if should_ifdefize (tag,ii) (ifdef::restifdefs) xxs
                    then
                      let res = IfdefStmt2 (ifdef::restifdefs, xxs) in
                      Visitor_c.vk_statement_sequencable_s bigf res::aux xs'
                    else
                      Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs

                | IfdefDirective (((IfdefElseif _|IfdefElse|IfdefEndif),b),ii) ->
                    pr2 "weird: first directive is not a ifdef";
                    (* maybe not weird, just that should_ifdefize
                     * returned false *)
                    Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs
                )

            | IfdefStmt2 (ifdef, xxs) ->
                failwith "already applied cpp_ifdef_statementize"
            )
      in
      aux xs
    );
  } ast

let fix_comma_init ii toks =
  let postfakeInfo pii  =
    let (max,min) =  Lib_parsing_c.max_min_ii_by_pos pii in
    let max_pi = Ast_c.get_info (fun x -> x) max in
    let vp = ({str="";charpos=max_pi.Common.charpos;line=max_pi.Common.line;
		column=max_pi.Common.column;file=max_pi.Common.file},
	      String.length max_pi.Common.str) in
    { pinfo = FakeTok ("",vp,Ast_c.After);
      cocci_tag = ref Ast_c.emptyAnnot;
      annots_tag = Token_annot.empty;
      comments_tag = ref Ast_c.emptyComments;
      danger = ref Ast_c.NoDanger;
    } in
  match toks with
    (TOPar _::_) as rest ->
      let rec iloop acc n = function
	  [] -> rest (* ran out *)
	| x::xs ->
	    match x with
	      TOPar _ -> iloop (x::acc) (n+1) xs
	    | TCPar ii ->
		if n = 1
		then
		  List.fold_left
		    (fun prev cur -> cur :: prev)
		    (x :: TNoComma (postfakeInfo [ii]) :: xs) acc
		else iloop (x::acc) (n-1) xs
	    | _ -> iloop (x::acc) n xs in
      iloop [] 0 rest
  | _ -> TNoComma (postfakeInfo [ii]) :: toks

(* ----------------------------------------------------------------------- *)
(* Changes specific to C++ *)

let convert_templates toks =
  let tokens2 = toks +> Common.acc_map TV.mk_token_extended in
  let rebuild = ref false in
  let top1 stack pdepth tdepth =
    match stack with
      (tok,tok_pdepth,tok_tdepth)::rest ->
	pdepth = tok_pdepth && tdepth = tok_tdepth + 1
    | _ -> false in
  let top2 stack pdepth tdepth =
    match stack with
      (tok1,tok1_pdepth,tok1_tdepth)::(tok2,tok2_pdepth,tok2_tdepth)::rest ->
	pdepth = tok1_pdepth && pdepth = tok2_pdepth &&
	tdepth = tok1_tdepth + 1 && tdepth = tok2_tdepth + 2
    | _ -> false in
  let top3 stack pdepth tdepth =
    match stack with
      (tok1,tok1_pdepth,tok1_tdepth)::(tok2,tok2_pdepth,tok2_tdepth)::
      (tok3,tok3_pdepth,tok3_tdepth)::rest ->
	pdepth = tok1_pdepth && pdepth = tok2_pdepth && pdepth = tok3_pdepth &&
	tdepth = tok1_tdepth + 1 && tdepth = tok2_tdepth + 2 && tdepth = tok3_tdepth + 3
    | _ -> false in
  let success a repl b i2 c i3 xs =
    b.TV.tok <- TTemplateStart i2;
    c.TV.tok <- i3;
    let comment_or_space x =
      let tok = x.TV.tok in
      TH.is_just_comment_or_space tok in
    let tmp = drop_while comment_or_space xs in
    (match tmp, repl with
      {TV.tok = TOPar(_)} :: xs, _ -> ()
    | _, Some (s,i1) -> a.TV.tok <- TypedefIdent(s,i1)
    | _ -> ()) in
  let rec loop stack pdepth tdepth = function
    [] -> ()
  | {TV.tok = TOPar _} :: xs
  | {TV.tok = TOCro _} :: xs
  | {TV.tok = TOBrace _} :: xs ->
      loop stack (pdepth+1) tdepth xs
  | {TV.tok = TCPar _} :: xs
  | {TV.tok = TCCro _} :: xs
  | {TV.tok = TCBrace _} :: xs ->
      let new_pdepth = max 0 (pdepth-1) in
      let stack =
	List.filter
	  (* close paren before close > *)
	  (fun (info,pdepth,tdepth) -> pdepth <= new_pdepth)
	  stack in
      loop stack new_pdepth tdepth xs
  (* start point *)
  | (({TV.tok = (TIdent(s,i1)|TypedefIdent(s,i1))}) as a) :: (* no space *)
    (({TV.tok = TInf i2}) as b) :: rest ->
      loop (((a,Some(s,i1),b,i2),pdepth,tdepth)::stack) pdepth (tdepth+1) rest
  | (({TV.tok = (TIdent(s,i1)|TypedefIdent(s,i1))}) as a) :: sp ::
    (({TV.tok = TInf i2}) as b) :: ((notsp::_) as rest)
    (* allow one space or newline before < if none after *)
    when TH.is_space sp.TV.tok && not(TH.is_space notsp.TV.tok) ->
      loop (((a,Some(s,i1),b,i2),pdepth,tdepth)::stack) pdepth (tdepth+1) rest
  | (({TV.tok = Ttemplate(i1)}) as a) :: rest ->
      let (skipped,rest) =
	span (fun x -> let tok = x.TV.tok in TH.is_just_comment_or_space tok) rest in
      (match rest with
	(({TV.tok = TInf i2}) as b)::rest ->
	  loop (((a,None,b,i2),pdepth,tdepth)::stack) pdepth (tdepth+1) rest
      | _ -> loop stack pdepth tdepth rest) (* just move on, template type name<...>(...) *)
  (* one possible end point *)
  | (({TV.tok = TSup i3}) as c) :: rest when top1 stack pdepth tdepth ->
      let ((ident,repl,inf,i2),_,_) = List.hd stack in
      success ident repl inf i2 c (TTemplateEnd i3) rest;
      loop (List.tl stack) pdepth (tdepth-1) rest
  (* another possible end point, more constraining Shr case first *)
  | (({TV.tok = TShr i3}) as c) :: rest when top2 stack pdepth tdepth ->
      rebuild := true;
      let ((ident,repl,inf,i2),_,_) = List.hd stack in
      success ident repl inf i2 c (TTemplateEndTemplateEnd i3) rest;
      let ((ident,repl,inf,i2),_,_) = List.hd (List.tl stack) in
      success ident repl inf i2 c (TTemplateEndTemplateEnd i3) rest;
      loop (List.tl (List.tl stack)) pdepth (tdepth-2) rest
  (* another possible end point, Shr that is template + > *)
  | (({TV.tok = TShr i3}) as c) :: rest when top1 stack pdepth tdepth ->
      rebuild := true;
      let ((ident,repl,inf,i2),_,_) = List.hd stack in
      success ident repl inf i2 c (TTemplateEndSup i3) rest;
      loop (List.tl stack) pdepth (tdepth-1) rest
  (* another possible end point *)
  | (({TV.tok = TSup3 i3}) as c) :: rest when top3 stack pdepth tdepth ->
      rebuild := true;
      let ((ident,repl,inf,i2),_,_) = List.hd stack in
      success ident repl inf i2 c (TTemplateEndTemplateEndTemplateEnd i3) rest;
      let ((ident,repl,inf,i2),_,_) = List.hd (List.tl stack) in
      success ident repl inf i2 c (TTemplateEndTemplateEndTemplateEnd i3) rest;
      let ((ident,repl,inf,i2),_,_) = List.hd (List.tl (List.tl stack)) in
      success ident repl inf i2 c (TTemplateEndTemplateEndTemplateEnd i3) rest;
      loop (List.tl (List.tl (List.tl stack))) pdepth (tdepth-3) rest
  (* something else *)
  | _::rest -> loop stack pdepth tdepth rest in
  loop [] 0 0 tokens2;
  if !rebuild
  then
    let copy_pi pi str offset =
      { pi with Common.str = str;
	Common.charpos = pi.Common.charpos + offset;
	Common.column = pi.Common.column + offset } in
    let copy_t tok str offset =
      match tok with
	Ast_c.OriginTok pi ->
	  Ast_c.OriginTok(copy_pi pi str offset)
      | Ast_c.FakeTok _ -> failwith "should not be a fake tok"
      | Ast_c.ExpandedTok(pi,(vpi,voffset)) ->
	  Ast_c.ExpandedTok(copy_pi pi str offset,
			    (copy_pi vpi str offset,voffset+offset))
      | Ast_c.AbstractLineTok pi ->
	  Ast_c.AbstractLineTok(copy_pi pi str offset) in
    let copy_tok t str offset =
      { Ast_c.pinfo = copy_t t.Ast_c.pinfo str offset;
	Ast_c.cocci_tag = ref !(t.Ast_c.cocci_tag);
	Ast_c.comments_tag = ref !(t.Ast_c.comments_tag);
	Ast_c.annots_tag = t.Ast_c.annots_tag;
	Ast_c.danger = ref !(t.Ast_c.danger) } in
    List.rev
      (List.fold_left
	 (fun prev tok ->
	   match tok.TV.tok with
	     TTemplateEndSup i3 ->
	       let t1 = TTemplateEnd (copy_tok i3 ">" 0) in
	       let t2 = TSup (copy_tok i3 ">" 1) in
	       t2 :: t1 :: prev
	   | TTemplateEndTemplateEnd i3 ->
	       let t1 = TTemplateEnd (copy_tok i3 ">" 0) in
	       let t2 = TTemplateEnd (copy_tok i3 ">" 1) in
	       t2 :: t1 :: prev
	   | TTemplateEndTemplateEndTemplateEnd i3 ->
	       let t1 = TTemplateEnd (copy_tok i3 ">" 0) in
	       let t2 = TTemplateEnd (copy_tok i3 ">" 1) in
	       let t3 = TTemplateEnd (copy_tok i3 ">" 2) in
	       t3 :: t2 :: t1 :: prev
	   | x -> x :: prev)
	 [] tokens2)
  else Common.acc_map (fun x -> x.TV.tok) tokens2

(* if the sequence of :: ends with a left paren we have an expression,
   otherwise we have a type *)
let rec choose_qualtype toks =
  let skip_to_template_end a toks =
    let rec loop ct acc = function
	| ((TTemplateStart i1) as a)::rest ->
	    loop (ct+1) (a::acc) rest
	| ((TTemplateEnd i1) as a)::rest ->
	    let ct = ct - 1 in
	    if ct = 0
	    then (List.rev (a::acc),rest)
	    else loop ct (a::acc) rest
	| x::rest -> loop ct (x::acc) rest
	| [] ->
	    failwith
	      (Printf.sprintf "%s:%d: no template end"
		 (TH.file_of_tok a) (TH.line_of_tok a)) in
    let (cur,rest) = loop 1 [a] toks in
    (choose_qualtype cur,rest) in
  let skip_parens toks =
    let rec loop ct acc = function
	((TOPar i1) as a) :: rest ->
	  loop (ct+1) (a::acc) rest
      | ((TCPar i1) as a) :: rest ->
	  if ct = 1
	  then (a::acc,rest)
	  else loop (ct-1) (a::acc) rest
      | x::xs ->loop ct (x::acc) xs
      | [] -> ([],toks) in
    loop 0 [] toks in
  let revapp l acc =
    List.fold_left (fun prev x -> x :: prev) acc l in
  let mkinfo info = Ast_c.fakeBeforeInfo() in
  let rec loop seencolon seentemplate localacc acc = function
      ((TIdent(s,i1)|TypedefIdent(s,i1)) as a)::rest ->
	let (skipped,rest) = span TH.is_just_comment_or_space rest in
	(match rest with
	  ((TColonColon i1) as b)::rest ->
	    let (skipped2,rest) = span TH.is_just_comment_or_space rest in
	    loop true seentemplate (revapp skipped2 (b::(revapp skipped (a::localacc)))) acc rest
	| ((TTemplateStart i1) as b)::rest ->
	    let (skipped2, rest) = skip_to_template_end b rest in
	    let (skipped3,rest) = span TH.is_just_comment_or_space rest in
	    (match rest with
	      ((TColonColon i1) as c)::rest ->
		let (skipped4,rest) = span TH.is_just_comment_or_space rest in
		loop true seentemplate
		  (revapp skipped4 (c :: (revapp (skipped2@skipped3) (revapp skipped (a::localacc)))))
		  acc rest
	    | ((TOPar i2) as c)::rest ->
		let acc = localacc@TQualExp(mkinfo i1)::acc in
		loop false false [] (c :: (revapp (skipped2@skipped3) (revapp skipped (a::acc)))) rest
	    | rest ->
		let acc = localacc@TQualType(mkinfo i1)::acc in
		loop false false [] (revapp (skipped2@skipped3) (revapp skipped (a::acc))) rest)
	| ((TIdent _) as b)::rest when seencolon->
	    let acc = localacc@TQualType(mkinfo i1)::acc in
	    loop false false [] (b :: (revapp skipped (a::acc))) rest
	| ((TAnd _|TMul _) as b)::((x::_) as rest)
	  when ((skipped <> [] && (not(TH.is_just_comment_or_space x))) ||
	        (skipped = [] && (TH.is_just_comment_or_space x))) && seencolon ->
	    let acc = localacc@TQualType(mkinfo i1)::acc in
	    loop false false [] (b :: (revapp skipped (a::acc))) rest
	| _ ->
	    let acc =
	      if seencolon
	      then
		if seentemplate
		then localacc@TQualExp(mkinfo i1)::acc
		else localacc@TQualId(mkinfo i1)::acc
	      else localacc@acc in
	    loop false false [] (revapp skipped (a::acc)) rest)
    | ((Tasm ii) as a)::rest -> (* skip, to avoid uses of :: *)
	let (skipped,rest) = skip_parens rest in
	assert (seencolon = false);
	assert (seentemplate = false);
	assert (localacc = []);
	loop false false [] (skipped @ (a::acc)) rest
    | ((TColonColon i1) as a)::rest ->
	let (skipped,rest) = span TH.is_just_comment_or_space rest in
	loop true false (revapp skipped (a::localacc)) acc rest
    | ((Toperator i1) as a) :: op :: rest ->
	let acc =
	  if seencolon
	  then localacc@TQualExp(mkinfo i1)::acc
	  else localacc@acc in
	loop false false [] (op::a::acc) rest
    | ((Ttemplate i1) as a)::rest when seencolon ->
	let (skipped,rest) = span TH.is_just_comment_or_space rest in
	loop true false (revapp skipped (a::localacc)) acc rest
    | ((TTilde i1) as a)::rest when seencolon ->
	let (skipped,rest) = span TH.is_just_comment_or_space rest in
	loop true false (revapp skipped (a::localacc)) acc rest
    | ((TMul i1) as a)::rest ->
	let acc =
	  if seencolon
	  then (TIdent("*",i1))::localacc@TQualType(mkinfo i1)::acc
	  else a::localacc@acc in
	loop false false [] acc rest
    | x::xs ->
	if localacc <> []
	then
	  failwith
	    (Printf.sprintf
	       "%s:%d.%d: localacc should be empty at unexpected qualifier token"
	       (TH.file_of_tok x) (TH.line_of_tok x) (TH.col_of_tok x))
	else loop false false [] (x::acc) xs
    | [] ->
	if localacc <> []
	then failwith "localacc should be empty at end of stream"
	else List.rev acc in
  loop false false [] [] toks
