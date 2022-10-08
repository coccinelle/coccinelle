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

open Parser_c

(*****************************************************************************)
(* Is_xxx, categories *)
(*****************************************************************************)

(* could define a type  token_class = Comment | Ident | Operator | ...
 * update: now token_c can maybe do that.
 * but still, sometimes tokens belon to multiple classes. Could maybe
 * return then a set of classes.
 *)

let is_space = function
  | TCommentSpace _ -> true
  | TCommentNewline _ -> true
  | _ -> false

let is_whitespace = is_space

let is_just_comment_or_space = function
  | TComment _ -> true
  | TCommentSpace _ -> true
  | TCommentNewline _ -> true
  | _ -> false
let is_real_comment = is_just_comment_or_space

let is_just_comment = function
  | TComment _ -> true
  | _ -> false




let is_comment = function
  | TComment _
  | TCommentSpace _ | TCommentNewline _
  | TCommentCpp _
  | TCommentMisc _ -> true
  | _ -> false

(* coupling with comment_annotater_c.ml.
 * In fact more tokens than comments are not in the ast, but
 * they were usually temporally created by ocamllex and removed
 * in parsing_hacks.
*)
let is_not_in_ast = is_comment

let is_fake_comment = function
  | TCommentCpp _    | TCommentMisc _
      -> true
  | _ -> false

let is_not_comment x =
  not (is_comment x)


(* ---------------------------------------------------------------------- *)

let is_cpp_instruction = function
  | TInclude _
  | TDefine _  | TPrePragma _ | TPragma _
  | TIfdef _ | TIfdefelse _ | TIfdefelif _ | TEndif _
  | TIfdefBool _ | TIfdefMisc _ | TIfdefVersion _
  | TUndef _
  | TCppDirectiveOther _
      -> true
  | _ -> false

let is_cpp_else = function
  | TIfdefelse _ -> true
  | _            -> false

let is_cpp_endif = function
  | TEndif _ -> true
  | _        -> false

let is_gcc_token = function
  | Tasm _
  | Tinline _
  | Tattribute _
  | Ttypeof _
      -> true
  | _ -> false

let is_escaped_newline = function
  | TCppEscapedNewline _ -> true
  | _                    -> false


(* ---------------------------------------------------------------------- *)
let is_opar = function
  | TOPar _ | TOParDefine _ -> true
  | _ -> false

let is_cpar = function
  | TCPar _ | TCParEOL _ -> true
  | _ -> false


let is_obrace = function
  | TOBrace _ | TOBraceDefineInit _ -> true
  | _ -> false

let is_cbrace = function
  | TCBrace _ -> true
  | _ -> false




(* ---------------------------------------------------------------------- *)

(* end of file *)
let is_eof = function
  | EOF x -> true
  | _ -> false


(* end of macro *)
let is_eom = function
  | TDefEOL _ -> true
  | _ -> false

let is_else = function
  | Telse _ -> true
  | _       -> false

let is_if_or_else = function
  | Tif _ | Telse _
      -> true
  | _ -> false

let is_statement = function
  | Tfor _ | Tdo _ | Tif _ | Twhile _ | Treturn _
  | Tbreak _ | Telse _ | Tswitch _ | Tcase _ | Tcontinue _
  | Tgoto _
  | TPtVirg _
  | TMacroIterator _
      -> true
  | _ -> false

(* is_start_of_something is used in parse_c for error recovery, to find
 * a synchronisation token.
 *
 * Would like to put TIdent or TDefine, TIfdef but they can be in the
 * middle of a function, for instance with label:.
 *
 * Could put Typedefident but fired ? it would work in error recovery
 * on the already_passed tokens, which has been already gone in the
 * Parsing_hacks.lookahead machinery, but it will not work on the
 * "next" tokens. But because the namespace for labels is different
 * from namespace for ident/typedef, we can use the name for a typedef
 * for a label and so dangerous to put Typedefident at true here.
 *
 * Can look in parser_c.output to know what can be at toplevel
 * at the very beginning.
 *)

let is_start_of_something = function
  | Tchar _  | Tshort _ | Tint _ | Tdouble _ |  Tfloat _ | Tlong _
  | Tunsigned _ | Tsigned _ | Tvoid _ | Tsize_t _ | Tssize_t _ | Tptrdiff_t _
  | TautoType _
  | Tauto _ | Tregister _ | Textern _ | Tstatic _
  | Tconst _ | Tvolatile _
  | Ttypedef _
  | Tstruct _ | Tunion _ | Tcpp_struct _ | Tcpp_union _ | Tclass _ | Tenum _ | Tdecimal _
    -> true
  | _ -> false



let is_binary_operator = function
  | TOrLog _ | TAndLog _ |  TOr _ |  TXor _ |  TAnd _
  | TEqEq _ |  TNotEq _  | TInf _ |  TSup _ |  TInfEq _ |  TSupEq _
  | TShl _ | TShr _
  | TPlus _ |  TMinus _ |  TMul _ |  TDiv _ |  TMod _ | TMax _ | TMin _
        -> true
  | _ -> false

let is_stuff_taking_parenthized = function
  | Tif _
  | Twhile _
  | Tswitch _
  | Ttypeof _
  | TMacroIterator _
    -> true
  | _ -> false


(* used in the algorithms for "10 most problematic errors" *)
let is_ident_like = function
  | TIdent _
  | TKRParam _
  | TypedefIdent _
  | TIdentDefine  _
  | TDefParamVariadic _

  | TUnknown _

  | TMacroAttr _
  | TMacroAttrArgs _
  | TMacroStmt _
  | TMacroIdStmt _
  | TMacroString _
  | TMacroDecl _
  | TMacroDeclConst _
  | TMacroIterator _
      -> true

  | _ -> false

(*****************************************************************************)
(* Matching functions for 'Nasty Undisciplined Cpp' *)
(*****************************************************************************)

(* matches 'if ... else ...'
 * where there is no other if the '...'.
 *)
let match_simple_if_else
    : token list -> (token * token list * token * token list) option
  = function
  | (Tif _ as tok_if)::rest_if ->
    begin try
      let (body,tok_else,rest_else) = split_when is_else rest_if in
      if List.exists is_if_or_else body
         || List.exists is_if_or_else rest_else
         then None (* no nested if/else wanted *)
         else Some (tok_if,body,tok_else,rest_else)
    with Not_found -> None end
  | _ -> None

(* matches '#ifdef ... #endif ...' *)
let match_cpp_simple_ifdef_endif_aux
    : token list -> (token * token list * token * token list) option
  = function
  | (TIfdef _ as tok_ifdef)::rest_ifdef ->
    begin try
      let (body,tok_endif,rest_endif) = split_when is_cpp_endif rest_ifdef in
      Some (tok_ifdef,body,tok_endif,rest_endif)
    with Not_found -> None end
  | _ -> None

(* matches '#ifdef ...1 #endif ...2'
 * where there is no other #ifdef within '...1'
 *)
let match_cpp_simple_ifdef_endif (xs :token list)
      : (token * token list * token * token list) option
  = match match_cpp_simple_ifdef_endif_aux xs with
  | Some (tok_ifdef,body,tok_endif,rest_endif)
      when not(List.exists is_cpp_instruction body)
        (* no nested CPP wanted *)
      -> Some (tok_ifdef,body,tok_endif,rest_endif)
  | _ -> None

let match_cpp_simple_ifdef_else_endif (xs :token list)
      : (token * token list * token * token list * token * token list) option
  = match match_cpp_simple_ifdef_endif_aux xs with
  | Some (tok_ifdef,body,tok_endif,rest_endif) ->
    begin try
      let (body_if,tok_else,body_else) = split_when is_cpp_else body in
      if List.exists is_cpp_instruction body_if
         || List.exists is_cpp_instruction body_else
         then None (* no nested CPP wanted *)
         else Some (tok_ifdef,body_if,tok_else,body_else,tok_endif,rest_endif)
    with Not_found -> None end
  | _ -> None

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

(* Because ocamlyacc force us to do it that way. The ocamlyacc token
 * cannot be a pair of a sum type, it must be directly a sum type.
 *)
let info_of_tok = function
  | TString ((string, isWchar), i) -> i
  | TChar  ((string, isWchar), i) -> i
  | TFloat ((string, floatType), i) -> i
  | TDecimal ((string, n, p), i) -> i

  | TQuote (_,i) -> i
  | TPct i -> i
  | TFormat(str,i) -> i
  | TSubString(str,i) -> i

  | TAssign  (assignOp, ii) -> Common.tuple_of_list1 ii

  | TIdent  (s, i) -> i
  | TKRParam  (s, i) -> i
  | Tconstructorname  (s, i) -> i
  | TypedefIdent  (s, i) -> i

  | TInt  (s, i) -> i

  | TDefine (ii) -> ii
  | TInclude (includes, filename, inifdef, i1) ->     i1

  | TUndef (ii) -> ii
  | TPrePragma (ii,_,_,_,_,_) -> ii
  | TPragmaString (s,ii) -> ii
  | TPragma (ii) -> ii
  | TCppDirectiveOther (ii) -> ii

  | TIncludeStart (i1, inifdef) ->     i1
  | TIncludeFilename (s, i1) ->     i1

  | TDefEOL (i1) ->     i1
  | TOParDefine (i1) ->     i1
  | TIdentDefine  (s, i) -> i
  | TCppEscapedNewline (ii) -> ii
  | TDefParamVariadic (s, i1) ->     i1

  | TCppConcatOp (ii) -> ii

  | TOBraceDefineInit (i1) ->     i1

  | TUnknown             (i) -> i

  | TMacroIdentBuilder     (s, i) -> i
  | TMacroAttr             (s, i) -> i
  | TMacroAttrArgs         (s, i) -> i
  | TMacroStmt             (s, i) -> i
  | TMacroIdStmt           (s, i) -> i
  | TMacroString           (s, i) -> i
  | TMacroDecl             (s, i) -> i
  | TMacroDeclConst        (i) -> i
  | TMacroIterator         (s,i) -> i
(*  | TMacroTop            (s,i) -> i *)
  | TCParEOL (i1) ->     i1

  | TAction             (i) -> i

  | TComment             (i) -> i
  | TCommentSpace        (i) -> i
  | TCommentNewline      (i) -> i
  | TCommentCpp          (cppkind, i) -> i
  | TCommentMisc         (i) -> i

  | TCommentSkipTagStart (i) -> i
  | TCommentSkipTagEnd (i) -> i

  | TIfdef               (_, _, i) -> i
  | TIfdefelse           (_, i) -> i
  | TIfdefelif           (_, _, i) -> i
  | TEndif               (_, i) -> i
  | TIfdefBool           (b, _, i) -> i
  | TIfdefMisc           (b, _, i) -> i
  | TIfdefVersion        (b, _, i) -> i

  | TUifdef              (i) -> i
  | TUelseif             (i) -> i
  | TUendif              (i) -> i

  | TOPar                (i) -> i
  | TCPar                (i) -> i
  | TOBrace              (i) -> i
  | TCBrace              (i) -> i
  | TOCro                (i) -> i
  | TCCro                (i) -> i
  | TDot                 (i) -> i
  | TComma               (i) -> i
  | TNoComma             (i) -> i
  | TPtrOp               (i) -> i
  | TInc                 (i) -> i
  | TDec                 (i) -> i
  | TEq                  (i) -> i
  | TWhy                 (i) -> i
  | TTilde               (i) -> i
  | TBang                (i) -> i
  | TEllipsis            (i) -> i
  | TDotDot              (i) -> i
  | TPtVirg              (i) -> i
  | TOrLog               (i) -> i
  | TAndLog              (i) -> i
  | TOr                  (i) -> i
  | TXor                 (i) -> i
  | TAnd                 (i) -> i
  | TEqEq                (i) -> i
  | TNotEq               (i) -> i
  | TInf                 (i) -> i
  | TSup                 (i) -> i
  | TInfEq               (i) -> i
  | TSupEq               (i) -> i
  | TShl                 (i) -> i
  | TShr                 (i) -> i
  | TPlus                (i) -> i
  | TMinus               (i) -> i
  | TMul                 (i) -> i
  | TDiv                 (i) -> i
  | TMin                 (i) -> i
  | TMax                 (i) -> i
  | TMod                 (i) -> i
  | Tchar                (i) -> i
  | Tshort               (i) -> i
  | Tint                 (i) -> i
  | Tdouble              (i) -> i
  | Tfloat               (i) -> i
  | Tcomplex             (i) -> i
  | Tlong                (i) -> i
  | Tunsigned            (i) -> i
  | Tsigned              (i) -> i
  | Tvoid                (i) -> i
  | Tsize_t              (i) -> i
  | Tssize_t             (i) -> i
  | Tptrdiff_t           (i) -> i
  | TautoType            (i) -> i
  | Tauto                (i) -> i
  | Tregister            (i) -> i
  | Textern              (i) -> i
  | Tstatic              (i) -> i
  | Tconst               (i) -> i
  | Tvolatile            (i) -> i

  | Trestrict            (i) -> i

  | Tstruct              (i) -> i
  | Tenum                (i) -> i
  | Tdecimal             (i) -> i
  | Texec                (i) -> i
  | Ttemplate            (i) -> i
  | Ttypedef             (i) -> i
  | Tunion               (i) -> i
  | Tcpp_struct          (i) -> i
  | Tcpp_union           (i) -> i
  | Tclass               (i) -> i
  | Tbreak               (i) -> i
  | Telse                (i) -> i
  | Tswitch              (i) -> i
  | Tcase                (i) -> i
  | Tcontinue            (i) -> i
  | Tfor                 (i) -> i
  | Tdo                  (i) -> i
  | Tif                  (i) -> i
  | Twhile               (i) -> i
  | Treturn              (i) -> i
  | Tgoto                (i) -> i
  | Tdefault             (i) -> i
  | Tsizeof              (i) -> i
  | Tasm                 (i) -> i
  | Tattribute           (i) -> i
  | TattributeNoarg      (i) -> i
  | Tinline              (i) -> i
  | Ttypeof              (i) -> i
  | Tnew                 (i) -> i
  | Tdelete              (i) -> i
  | Tprivate             (i) -> i
  | Tprotected           (i) -> i
  | Tpublic              (i) -> i
  | Toperator            (i) -> i
  | TTemplateStart       (i) -> i
  | TTemplateEnd         (i) -> i
  | TTemplateEndSup      (i) -> i
  | TTemplateEndTemplateEnd (i) -> i
  | Tfinal               (i) -> i
  | Tdefined             (i) -> i
  | TOParCplusplusInit   (i) -> i

  | EOF                  (i) -> i
  | Tnamespace           (i) -> i
  | TTODO _ -> failwith "fake token, should not occur"



(* used by tokens to complete the parse_info with filename, line, col infos *)
let visitor_info_of_tok f = function
  | TString ((s, isWchar), i)  -> TString ((s, isWchar), f i)
  | TChar  ((s, isWchar), i)   -> TChar  ((s, isWchar), f i)
  | TFloat ((s, floatType), i) -> TFloat ((s, floatType), f i)
  | TDecimal ((s, n, p), i)    -> TDecimal ((s, n, p), f i)
  | TAssign  (assignOp, ii)     -> TAssign  (assignOp, List.map f ii)

  | TQuote ((str,isW),i) -> TQuote ((str,isW),f i)
  | TPct i -> TPct (f i)
  | TFormat(str,i) -> TFormat(str,f i)
  | TSubString(str,i) -> TSubString(str,f i)

  | TIdent  (s, i)         -> TIdent  (s, f i)
  | TKRParam(s, i)         -> TKRParam(s, f i)
  | Tconstructorname(s, i) -> Tconstructorname  (s, f i)
  | TypedefIdent  (s, i)   -> TypedefIdent  (s, f i)
  | TInt  (s, i)           -> TInt  (s, f i)

  | TDefine (i1) -> TDefine(f i1)

  | TUndef (i1) -> TUndef(f i1)
  | TPragma (i1) -> TPragma(f i1)
  | TPrePragma (i1,wss1,a,b,wss2,c) ->
      TPrePragma(f i1,f wss1,a,f b,f wss2,
		 List.map (fun (c,d) -> (c,f d)) c)
  | TPragmaString (s,ii) -> TPragmaString(s,f ii)
  | TCppDirectiveOther (i1) -> TCppDirectiveOther(f i1)

  | TInclude (includes, filename, inifdef, i1) ->
      TInclude (includes, filename, inifdef, f i1)

  | TIncludeStart (i1, inifdef) -> TIncludeStart (f i1, inifdef)
  | TIncludeFilename (s, i1) -> TIncludeFilename (s, f i1)

  | TCppEscapedNewline (i1) -> TCppEscapedNewline (f i1)
  | TDefEOL (i1) -> TDefEOL (f i1)

  | TCppConcatOp (ii) -> TCppConcatOp (f ii)

  | TOParDefine (i1) -> TOParDefine (f i1)
  | TIdentDefine  (s, i) -> TIdentDefine (s, f i)

  | TDefParamVariadic (s, i1) -> TDefParamVariadic (s, f i1)

  | TOBraceDefineInit (i1) -> TOBraceDefineInit (f i1)


  | TUnknown             (i) -> TUnknown                (f i)

  | TMacroIdentBuilder             (s, i) -> TMacroIdentBuilder (s, f i)
  | TMacroAttr           (s, i)   -> TMacroAttr            (s, f i)
  | TMacroAttrArgs       (s, i)   -> TMacroAttrArgs        (s, f i)
  | TMacroStmt           (s, i)   -> TMacroStmt            (s, f i)
  | TMacroIdStmt         (s, i)   -> TMacroIdStmt          (s, f i)
  | TMacroString         (s, i)   -> TMacroString          (s, f i)
  | TMacroDecl           (s, i)   -> TMacroDecl            (s, f i)
  | TMacroDeclConst      (i)      -> TMacroDeclConst       (f i)
  | TMacroIterator       (s, i)   -> TMacroIterator        (s, f i)
(*  | TMacroTop          (s,i)    -> TMacroTop             (s,f i) *)
  | TCParEOL (i) ->     TCParEOL (f i)


  | TAction               (i) -> TAction             (f i)

  | TComment             (i) -> TComment             (f i)
  | TCommentSpace        (i) -> TCommentSpace        (f i)
  | TCommentNewline      (i) -> TCommentNewline      (f i)
  | TCommentCpp          (cppkind, i) -> TCommentCpp (cppkind, f i)
  | TCommentMisc         (i) -> TCommentMisc         (f i)

  | TCommentSkipTagStart         (i) -> TCommentSkipTagStart         (f i)
  | TCommentSkipTagEnd         (i) -> TCommentSkipTagEnd         (f i)

  | TIfdef               (c, t, i) -> TIfdef            (c, t, f i)
  | TIfdefelse           (t, i) -> TIfdefelse           (t, f i)
  | TIfdefelif           (c, t, i) -> TIfdefelif        (c, t, f i)
  | TEndif               (t, i) -> TEndif               (t, f i)
  | TIfdefBool           (b, t, i) -> TIfdefBool        (b, t, f i)
  | TIfdefMisc           (b, t, i) -> TIfdefMisc        (b, t, f i)
  | TIfdefVersion        (b, t, i) -> TIfdefVersion     (b, t, f i)

  | TUifdef              (i) -> TUifdef              (f i)
  | TUelseif             (i) -> TUelseif             (f i)
  | TUendif              (i) -> TUendif              (f i)

  | TOPar                (i) -> TOPar                (f i)
  | TCPar                (i) -> TCPar                (f i)
  | TOBrace              (i) -> TOBrace              (f i)
  | TCBrace              (i) -> TCBrace              (f i)
  | TOCro                (i) -> TOCro                (f i)
  | TCCro                (i) -> TCCro                (f i)
  | TDot                 (i) -> TDot                 (f i)
  | TComma               (i) -> TComma               (f i)
  | TNoComma             (i) -> TNoComma             (f i)
  | TPtrOp               (i) -> TPtrOp               (f i)
  | TInc                 (i) -> TInc                 (f i)
  | TDec                 (i) -> TDec                 (f i)
  | TEq                  (i) -> TEq                  (f i)
  | TWhy                 (i) -> TWhy                 (f i)
  | TTilde               (i) -> TTilde               (f i)
  | TBang                (i) -> TBang                (f i)
  | TEllipsis            (i) -> TEllipsis            (f i)
  | TDotDot              (i) -> TDotDot              (f i)
  | TPtVirg              (i) -> TPtVirg              (f i)
  | TOrLog               (i) -> TOrLog               (f i)
  | TAndLog              (i) -> TAndLog              (f i)
  | TOr                  (i) -> TOr                  (f i)
  | TXor                 (i) -> TXor                 (f i)
  | TAnd                 (i) -> TAnd                 (f i)
  | TEqEq                (i) -> TEqEq                (f i)
  | TNotEq               (i) -> TNotEq               (f i)
  | TInf                 (i) -> TInf                 (f i)
  | TSup                 (i) -> TSup                 (f i)
  | TInfEq               (i) -> TInfEq               (f i)
  | TSupEq               (i) -> TSupEq               (f i)
  | TShl                 (i) -> TShl                 (f i)
  | TShr                 (i) -> TShr                 (f i)
  | TPlus                (i) -> TPlus                (f i)
  | TMinus               (i) -> TMinus               (f i)
  | TMul                 (i) -> TMul                 (f i)
  | TDiv                 (i) -> TDiv                 (f i)
  | TMin                 (i) -> TMin                 (f i)
  | TMax                 (i) -> TMax                 (f i)
  | TMod                 (i) -> TMod                 (f i)
  | Tchar                (i) -> Tchar                (f i)
  | Tshort               (i) -> Tshort               (f i)
  | Tint                 (i) -> Tint                 (f i)
  | Tdouble              (i) -> Tdouble              (f i)
  | Tfloat               (i) -> Tfloat               (f i)
  | Tcomplex             (i) -> Tcomplex             (f i)
  | Tlong                (i) -> Tlong                (f i)
  | Tunsigned            (i) -> Tunsigned            (f i)
  | Tsigned              (i) -> Tsigned              (f i)
  | Tvoid                (i) -> Tvoid                (f i)
  | Tsize_t              (i) -> Tsize_t              (f i)
  | Tssize_t             (i) -> Tssize_t             (f i)
  | Tptrdiff_t           (i) -> Tptrdiff_t           (f i)
  | TautoType            (i) -> TautoType            (f i)
  | Tauto                (i) -> Tauto                (f i)
  | Tregister            (i) -> Tregister            (f i)
  | Textern              (i) -> Textern              (f i)
  | Tstatic              (i) -> Tstatic              (f i)
  | Tconst               (i) -> Tconst               (f i)
  | Tvolatile            (i) -> Tvolatile            (f i)

  | Trestrict            (i) -> Trestrict            (f i)

  | Tstruct              (i) -> Tstruct              (f i)
  | Tenum                (i) -> Tenum                (f i)
  | Tdecimal             (i) -> Tdecimal             (f i)
  | Texec                (i) -> Texec                (f i)
  | Ttemplate            (i) -> Ttemplate            (f i)
  | Ttypedef             (i) -> Ttypedef             (f i)
  | Tunion               (i) -> Tunion               (f i)
  | Tcpp_struct          (i) -> Tcpp_struct          (f i)
  | Tcpp_union           (i) -> Tcpp_union           (f i)
  | Tclass               (i) -> Tclass               (f i)
  | Tbreak               (i) -> Tbreak               (f i)
  | Telse                (i) -> Telse                (f i)
  | Tswitch              (i) -> Tswitch              (f i)
  | Tcase                (i) -> Tcase                (f i)
  | Tcontinue            (i) -> Tcontinue            (f i)
  | Tfor                 (i) -> Tfor                 (f i)
  | Tdo                  (i) -> Tdo                  (f i)
  | Tif                  (i) -> Tif                  (f i)
  | Twhile               (i) -> Twhile               (f i)
  | Treturn              (i) -> Treturn              (f i)
  | Tgoto                (i) -> Tgoto                (f i)
  | Tdefault             (i) -> Tdefault             (f i)
  | Tsizeof              (i) -> Tsizeof              (f i)
  | Tasm                 (i) -> Tasm                 (f i)
  | Tattribute           (i) -> Tattribute           (f i)
  | TattributeNoarg      (i) -> TattributeNoarg      (f i)
  | Tinline              (i) -> Tinline              (f i)
  | Ttypeof              (i) -> Ttypeof              (f i)
  | Tnew                 (i) -> Tnew                 (f i)
  | Tdelete              (i) -> Tdelete              (f i)
  | Tprivate             (i) -> Tprivate             (f i)
  | Tprotected           (i) -> Tprotected           (f i)
  | Tpublic              (i) -> Tpublic              (f i)
  | Toperator            (i) -> Toperator            (f i)
  | TTemplateStart       (i) -> TTemplateStart       (f i)
  | TTemplateEnd         (i) -> TTemplateEnd         (f i)
  | TTemplateEndSup      (i) -> TTemplateEndSup      (f i)
  | TTemplateEndTemplateEnd (i) -> TTemplateEndTemplateEnd (f i)
  | Tfinal               (i) -> Tfinal               (f i)
  | Tdefined             (i) -> Tdefined             (f i)
  | TOParCplusplusInit   (i) -> TOParCplusplusInit   (f i)
  | EOF                  (i) -> EOF                  (f i)
  | Tnamespace           (i) -> Tnamespace           (f i)
  | TTODO _ -> failwith "fake token, should not occur"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let linecol_of_tok tok =
  let info = info_of_tok tok in
  Ast_c.line_of_info info, Ast_c.col_of_info info

let col_of_tok x = snd (linecol_of_tok x)
let line_of_tok x = fst (linecol_of_tok x)
let pos_of_tok x =  Ast_c.opos_of_info (info_of_tok x)
let str_of_tok x =  Ast_c.str_of_info (info_of_tok x)

let string_of_token = function
  | TUnknown _ -> "TUnknown"
  | TCommentSpace _ -> "TCommentSpace"
  | TCommentNewline _ -> "TCommentNewline"
  | TComment _ -> "TComment"
  | TInt _ -> "TInt"
  | TFloat _ -> "TFloat"
  | TChar _ -> "TChar"
  | TString _ -> "TString"
  | TQuote _ -> "TQuote"
  | TPct _ -> "TPct"
  | TFormat _ -> "TFormat"
  | TSubString _ -> "TSubString"
  | TDecimal _ -> "TDecimal"
  | TIdent _ -> "TIdent"
  | TKRParam _ -> "TKRParam"
  | Tconstructorname _ -> "Tconstructorname"
  | TypedefIdent _ -> "TypedefIdent"
  | TOPar _ -> "TOPar"
  | TCPar _ -> "TCPar"
  | TOBrace _ -> "TOBrace"
  | TCBrace _ -> "TCBrace"
  | TOCro _ -> "TOCro"
  | TCCro _ -> "TCCro"
  | TDot _ -> "TDot"
  | TComma _ -> "TComma"
  | TNoComma _ -> "TNoComma"
  | TPtrOp _ -> "TPtrOp"
  | TInc _ -> "TInc"
  | TDec _ -> "TDec"
  | TAssign _ -> "TAssign"
  | TEq _ -> "TEq"
  | TWhy _ -> "TWhy"
  | TTilde _ -> "TTilde"
  | TBang _ -> "TBang"
  | TEllipsis _ -> "TEllipsis"
  | TDotDot _ -> "TDotDot"
  | TPtVirg _ -> "TPtVirg"
  | TOrLog _ -> "TOrLog"
  | TAndLog _ -> "TAndLog"
  | TOr _ -> "TOr"
  | TXor _ -> "TXor"
  | TAnd _ -> "TAnd"
  | TEqEq _ -> "TEqEq"
  | TNotEq _ -> "TNotEq"
  | TInf _ -> "TInf"
  | TSup _ -> "TSup"
  | TInfEq _ -> "TInfEq"
  | TSupEq _ -> "TSupEq"
  | TShl _ -> "TShl"
  | TShr _ -> "TShr"
  | TPlus _ -> "TPlus"
  | TMinus _ -> "TMinus"
  | TMul _ -> "TMul"
  | TDiv _ -> "TDiv"
  | TMod _ -> "TMod"
  | TMax _ -> "TMax"
  | TMin _ -> "TMin"
  | Tchar _ -> "Tchar"
  | Tshort _ -> "Tshort"
  | Tint _ -> "Tint"
  | Tdouble _ -> "Tdouble"
  | Tfloat _ -> "Tfloat"
  | Tcomplex _ -> "Tcomplex"
  | Tlong _ -> "Tlong"
  | Tunsigned _ -> "Tunsigned"
  | Tsigned _ -> "Tsigned"
  | Tvoid _ -> "Tvoid"
  | Tsize_t _ -> "Tsize_t"
  | Tssize_t _ -> "Tssize_t"
  | Tptrdiff_t _ -> "Tptrdiff_t"
  | TautoType _ -> "Tauto"
  | Tauto _ -> "Tauto"
  | Tregister _ -> "Tregister"
  | Textern _ -> "Textern"
  | Tstatic _ -> "Tstatic"
  | Ttypedef _ -> "Ttypedef"
  | Tconst _ -> "Tconst"
  | Tvolatile _ -> "Tvolatile"
  | Tstruct _ -> "Tstruct"
  | Tunion _ -> "Tunion"
  | Tcpp_struct _ -> "Tcpp_struct"
  | Tcpp_union _ -> "Tcpp_union"
  | Tclass _ -> "Tclass"
  | Tenum _ -> "Tenum"
  | Tdecimal _ -> "Tdecimal"
  | Texec _ -> "Texec"
  | Ttemplate _ -> "Ttemplate"
  | Tbreak _ -> "Tbreak"
  | Telse _ -> "Telse"
  | Tswitch _ -> "Tswitch"
  | Tcase _ -> "Tcase"
  | Tcontinue _ -> "Tcontinue"
  | Tfor _ -> "Tfor"
  | Tdo _ -> "Tdo"
  | Tif _ -> "Tif"
  | Twhile _ -> "Twhile"
  | Treturn _ -> "Treturn"
  | Tgoto _ -> "Tgoto"
  | Tdefault _ -> "Tdefault"
  | Tsizeof _ -> "Tsizeof"
  | Tnew _ -> "Tnew"
  | Tdelete _ -> "Tdelete"
  | Tprivate _ -> "Tprivate"
  | Tprotected _ -> "Tprotected"
  | Tpublic _ -> "Tpublic"
  | Toperator _ -> "Toperator"
  | TTemplateStart _ -> "TTemplateStart"
  | TTemplateEnd _ -> "TTemplateEnd"
  | TTemplateEndSup _ -> "TTemplateEndSup"
  | TTemplateEndTemplateEnd _ -> "TTemplateEndTemplateEnd"
  | Tfinal _ -> "Tfinal"
  | Tdefined _ -> "Tdefined"
  | TOParCplusplusInit _ -> "TOParCplusplusInit"
  | Tnamespace _ -> "Tnamespace"
  | Trestrict _ -> "Trestrict"
  | Tasm _ -> "Tasm"
  | Tattribute _ -> "Tattribute"
  | TattributeNoarg _ -> "TattributeNoarg"
  | Tinline _ -> "Tinline"
  | Ttypeof _ -> "Ttypeof"
  | TDefine _ -> "TDefine"
  | TDefParamVariadic _ -> "TDefParamVariadic"
  | TCppEscapedNewline _ -> "TCppEscapedNewline"
  | TCppConcatOp _ -> "TCppConcatOp"
  | TOParDefine _ -> "TOParDefine"
  | TOBraceDefineInit _ -> "TOBraceDefineInit"
  | TIdentDefine _ -> "TIdentDefine"
  | TDefEOL _ -> "TDefEOL"
  | TInclude _ -> "TInclude"
  | TIncludeStart _ -> "TIncludeStart"
  | TIncludeFilename _ -> "TIncludeFilename"
  | TIfdef _ -> "TIfdef"
  | TIfdefelif _ -> "TIfdefelif"
  | TIfdefelse _ -> "TIfdefelse"
  | TEndif _ -> "TEndif"
  | TIfdefBool _ -> "TIfdefBool"
  | TIfdefMisc _ -> "TIfdefMisc"
  | TIfdefVersion _ -> "TIfdefVersion"
  | TUifdef _ -> "TUifdef"
  | TUelseif _ -> "TUelseif"
  | TUendif _ -> "TUendif"
  | TUndef _ -> "TUndef"
  | TPrePragma _ -> "TPrePragma"
  | TPragmaString _ -> "TPragmaString"
  | TPragma _ -> "TPragma"
  | TCppDirectiveOther _ -> "TCppDirectiveOther"
  | TMacroAttr _ -> "TMacroAttr"
  | TMacroAttrArgs _ -> "TMacroAttrArgs"
  | TMacroStmt _ -> "TMacroStmt"
  | TMacroIdStmt _ -> "TMacroIdStmt"
  | TMacroIdentBuilder _ -> "TMacroIdentBuilder"
  | TMacroString _ -> "TMacroString"
  | TMacroDecl _ -> "TMacroDecl"
  | TMacroDeclConst _ -> "TMacroDeclConst"
  | TMacroIterator _ -> "TMacroIterator"
  | TCommentSkipTagStart _ -> "TCommentSkipTagStart"
  | TCommentSkipTagEnd _ -> "TCommentSkipTagEnd"
  | TCParEOL _ -> "TCParEOL"
  | TAction _ -> "TAction"
  | TCommentMisc _ -> "TCommentMisc"
  | TCommentCpp _ -> "TCommentCpp"
  | EOF _ -> "EOF"
  | TTODO _ -> failwith "fake token, should not occur"

let file_of_tok x = Ast_c.file_of_info (info_of_tok x)
let pinfo_of_tok x = Ast_c.pinfo_of_info (info_of_tok x)

(* for a comment, the end line is not the same as line_of_tok *)
let end_line_of_tok = function
    (TComment _) as t ->
      let newlines =
	List.length (Str.split_delim (Str.regexp "\n") (str_of_tok t)) - 1 in
      line_of_tok t + newlines
  | t -> line_of_tok t

let is_origin x =
  match pinfo_of_tok x with Ast_c.OriginTok _ -> true | _ -> false
let is_expanded x =
  match pinfo_of_tok x with Ast_c.ExpandedTok _ -> true | _ -> false
let is_fake x =
  match pinfo_of_tok x with Ast_c.FakeTok _ -> true | _ -> false
let is_abstract x =
  match pinfo_of_tok x with Ast_c.AbstractLineTok _ -> true | _ -> false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let is_same_line_or_close line tok =
  line_of_tok tok = line ||
  line_of_tok tok = line - 1 ||
  line_of_tok tok = line - 2

(** Filter out CPP backslash-newlines (i.e. "\\\n") from a token stream.
 *
 * This helps parsing expressions in CPP directives using
 * [expression_of_string].
 *
 * E.g.
 *          #if defined(A) || \
 *                defined(B)
 *
 * @author Iago Abal
 *)
let filter_out_escaped_newline =
  List.filter (fun tok -> not (is_escaped_newline tok))
