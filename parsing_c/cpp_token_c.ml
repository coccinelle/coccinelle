(* Yoann Padioleau
 * 
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

open Parser_c
open Token_views_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* cpp functions working at the token level. Cf cpp_ast_c for cpp functions
 * working at the AST level (which is very unusual but makes sense in 
 * the coccinelle context for instance).
 *  
 * Note that as I use a single lexer  to work both at the C and cpp level
 * there are some inconveniences. 
 * For instance 'for' is a valid name for a macro parameter and macro 
 * body, but is interpreted in a special way by our single lexer, and 
 * so at some places where I expect a TIdent I need also to
 * handle special cases and accept Tfor, Tif, etc at those places.
 * 
 * There are multiple issues related to those keywords incorrect tokens.
 * Those keywords can be:
 *   - (1) in the name of the macro as  in  #define inline
 *   - (2) in a parameter of the macro as in #define foo(char)   char x;
 *   - (3) in an argument to a macro call as in   IDENT(if);
 * Case 1 is easy to fix in define_ident.
 * Case 2 is easy to fix in define_parse where detect such toks in 
 * the parameter and then replace their occurence in the body in a Tident.
 * Case 3 is only an issue when the expanded token is not really use 
 * as usual but use for instance in concatenation as in  a ## if
 * when expanded. In the case the grammar this time will not be happy
 * so this is also easy to fix in cpp_engine.
 * 
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.pr2 s


(*****************************************************************************)
(* Types *)
(*****************************************************************************)


(* ------------------------------------------------------------------------- *)
(* mimic standard.h *)
(* ------------------------------------------------------------------------- *)

type define_def = string * define_param * define_body 
 and define_param = 
   | NoParam
   | Params of string list
 and define_body = 
   | DefineBody of Parser_c.token list
   | DefineHint of parsinghack_hint

   and parsinghack_hint = 
     | HintIterator
     | HintDeclarator
     | HintMacroString
     | HintMacroStatement
     | HintAttribute
     | HintMacroIdentBuilder



(* cf also data/test.h *)
let assoc_hint_string = [
  "YACFE_ITERATOR"   , HintIterator;
  "YACFE_DECLARATOR" , HintDeclarator;
  "YACFE_STRING"     , HintMacroString;
  "YACFE_STATEMENT"  , HintMacroStatement;
  "YACFE_ATTRIBUTE"  , HintAttribute;
  "YACFE_IDENT_BUILDER"  , HintMacroIdentBuilder;

  "MACROSTATEMENT"   , HintMacroStatement; (* backward compatibility *)
]


let (parsinghack_hint_of_string: string -> parsinghack_hint option) = fun s -> 
  Common.assoc_option s assoc_hint_string

let (is_parsinghack_hint: string -> bool) = fun s -> 
  parsinghack_hint_of_string s <> None

let (token_from_parsinghack_hint: 
     (string * Ast_c.info) -> parsinghack_hint -> Parser_c.token) = 
 fun (s,ii) hint ->
   match hint with
   | HintIterator -> 
       Parser_c.TMacroIterator (s, ii)
   | HintDeclarator -> 
       Parser_c.TMacroDecl (s, ii)
   | HintMacroString -> 
       Parser_c.TMacroString (s, ii)
   | HintMacroStatement -> 
       Parser_c.TMacroStmt (s, ii)
   | HintAttribute -> 
       Parser_c.TMacroAttr (s, ii)
   | HintMacroIdentBuilder -> 
       Parser_c.TMacroIdentBuilder (s, ii)
  





(*****************************************************************************)
(* Expansion helpers *)
(*****************************************************************************)

(* In some cases we can have macros like IDENT(if) that expands to some 
 * 'int xxx_if(void)', but as the lexer will currently generate a Tif for 
 * the expanded code, that may not be accepted as a token after a ##
 * in the grammar. Hence this function to remap some tokens. This is because
 * we should not use a single lexer for both working at the C level and
 * cpp level.
 *)
let rec remap_keyword_tokens xs = 
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> 
      (match x, y with
      | Parser_c.TCppConcatOp _, Parser_c.TIdent _ -> 
          x::y::remap_keyword_tokens xs
      | Parser_c.TIdent _, Parser_c.TCppConcatOp _ -> 
          x::y::remap_keyword_tokens xs

      | Parser_c.TCppConcatOp (i1), _ -> 

          let s = TH.str_of_tok y in
          let ii = TH.info_of_tok y in
          if s ==~ Common.regexp_alpha
          then begin
            pr2 (spf "remaping: %s to an ident in expanded code" s);
            x::(Parser_c.TIdent (s, ii))::remap_keyword_tokens xs
          end
          else 
            x::y::remap_keyword_tokens xs

      | _, Parser_c.TCppConcatOp (i1) -> 
          let s = TH.str_of_tok x in
          let ii = TH.info_of_tok x in
          if s ==~ Common.regexp_alpha
          then begin
            pr2 (spf "remaping: %s to an ident in expanded code" s);
            (Parser_c.TIdent (s, ii))::remap_keyword_tokens (y::xs)
          end
          else 
            x::y::remap_keyword_tokens xs

      | _, _ -> 
          x::remap_keyword_tokens (y::xs)
      )
              
          

(* To expand the parameter of the macro. The env corresponds to the actual
 * code that is binded to the parameters of the macro.
 * Recurse ? fixpoint ? the expansion may also contain macro.
 * Or to macro expansion in a strict manner, that is process first
 * the parameters, expands macro in params, and then process enclosing
 * macro call.
 * 
 * note: do the concatenation job of a##b here ?
 * normally this should be done in the grammar. Here just expand
 * tokens. The only thing we handle here is we may have to remap
 * some tokens.
 * 
 * todo: handle stringification here ? if #n
 * 
 * todo? but could parsing_hacks then pass over the remapped tokens, 
 * for instance transform some of the back into some TypedefIdent
 * so cpp_engine may be fooled?
 *)
let rec (cpp_engine: (string , Parser_c.token list) assoc -> 
          Parser_c.token list -> Parser_c.token list) = 
 fun env xs ->
  xs +> List.map (fun tok -> 
    (* expand only TIdent ? no cos the parameter of the macro
     * can actually be some 'register' so may have to look for 
     * any tokens candidates for the expansion.
     * Only subtelity is maybe dont expand the TDefineIdent.
     * update: in fact now the caller (define_parse) will have done 
     * the job right and already replaced the macro parameter with a TIdent.
     *)
    match tok with
    | TIdent (s,i1) when List.mem_assoc s env -> Common.assoc s env
    | x -> [x]
  )
  +> List.flatten
  +> remap_keyword_tokens



(* ------------------------------------------------------------------------- *)
(* apply macro, using standard.h or other defs *)
(* ------------------------------------------------------------------------- *)

(* Thanks to this function many stuff are not anymore hardcoded in ocaml code.
 * At some point there were hardcoded in a standard.h file but now I 
 * can even generate them on the fly on demand when there is actually
 * a parsing problem.
 * 
 * No need to take care to not substitute the macro name itself
 * that occurs in the macro definition because the macro name is
 * after fix_token_define a TDefineIdent, no more a TIdent.
 *)

let rec apply_macro_defs 
 ~msg_apply_known_macro 
 ~msg_apply_known_macro_hint 
 defs xs = 
 let rec apply_macro_defs xs = 
  match xs with
  | [] -> ()

  (* old: "but could do more, could reuse same original token
   * so that have in the Ast a Dbg, not a MACROSTATEMENT"
   * 
   *   | PToken ({tok = TIdent (s,i1)} as id)::xs 
   *     when s = "MACROSTATEMENT" -> 
   * 
   *     msg_macro_statement_hint s;
   *     id.tok <- TMacroStmt(TH.info_of_tok id.tok);
   *     find_macro_paren xs
   * 
   *  let msg_macro_statement_hint s = 
   *    incr Stat.nMacroHint;
   *   ()
   * 
   *)

  (* recognized macro of standard.h (or other) *)
  | PToken ({tok = TIdent (s,i1)} as id)::Parenthised (xxs,info_parens)::xs 
      when Hashtbl.mem defs s -> 
      
      msg_apply_known_macro s;
      let (s, params, body) = Hashtbl.find defs s in

      (match params with
      | NoParam -> 
          pr2 ("WEIRD: macro without param used before parenthize: " ^ s);
          (* ex: PRINTP("NCR53C400 card%s detected\n" ANDP(((struct ... *)

          (match body with
          | DefineBody bodymacro -> 
              set_as_comment (Token_c.CppMacro) id;
              id.new_tokens_before <- bodymacro;
          | DefineHint hint -> 
              msg_apply_known_macro_hint s;
              id.tok <- token_from_parsinghack_hint (s,i1) hint;
          )
      | Params params -> 
          (match body with
          | DefineBody bodymacro -> 

              (* bugfix: better to put this that before the match body, 
               * cos our macrostatement hint can have variable number of
               * arguments and so it's ok if it does not match exactly
               * the number of arguments. *)
              if List.length params != List.length xxs
              then begin 
                pr2_once ("WEIRD: macro with wrong number of arguments: " ^ s);
                (* old: id.new_tokens_before <- bodymacro; *)

                (* update: if wrong number, then I just pass this macro *)
                [Parenthised (xxs, info_parens)] +> 
                  iter_token_paren (set_as_comment Token_c.CppMacro);
                set_as_comment Token_c.CppMacro id;

                ()
              end
              else 

                let xxs' = xxs +> List.map (fun x -> 
                  (tokens_of_paren_ordered x) +> List.map (fun x -> 
                    TH.visitor_info_of_tok Ast_c.make_expanded x.tok
                  )
                ) in
                id.new_tokens_before <-
                  (* !!! cpp expansion job here  !!! *)
                  cpp_engine (Common.zip params xxs') bodymacro;

                (* important to do that after have apply the macro, otherwise
                 * will pass as argument to the macro some tokens that
                 * are all TCommentCpp
                 *)
                [Parenthised (xxs, info_parens)] +> 
                  iter_token_paren (set_as_comment Token_c.CppMacro);
                set_as_comment Token_c.CppMacro id;

            | DefineHint (HintMacroStatement as hint) -> 
                (* important to do that after have apply the macro, otherwise
                 * will pass as argument to the macro some tokens that
                 * are all TCommentCpp
                 * 
                 * note: such macrostatement can have a variable number of
                 * arguments but here we don't care, we just pass all the
                 * parameters.
                 *)

                (match xs with
                | PToken ({tok = TPtVirg _} as id2)::_ -> 
                    pr2_once 
                      ("macro stmt with trailing ';', passing also ';' for: "^
                       s);
                    (* sometimes still want pass its params ... as in
                     *  DEBUGPOLL(static unsigned int prev_mask = 0);
                     *)

                    msg_apply_known_macro_hint s;
                    id.tok <- token_from_parsinghack_hint (s,i1) hint;
                    [Parenthised (xxs, info_parens)] +> 
                      iter_token_paren (set_as_comment Token_c.CppMacro);
                    set_as_comment Token_c.CppMacro id2;

                | _ ->
                    msg_apply_known_macro_hint s;
                    id.tok <- token_from_parsinghack_hint (s,i1) hint;
                    [Parenthised (xxs, info_parens)] +> 
                      iter_token_paren (set_as_comment Token_c.CppMacro);
                )
                

            | DefineHint hint -> 
                msg_apply_known_macro_hint s;
                id.tok <- token_from_parsinghack_hint (s,i1) hint;
            )
      );
      apply_macro_defs xs

  | PToken ({tok = TIdent (s,i1)} as id)::xs 
      when Hashtbl.mem defs s -> 

      msg_apply_known_macro s;
      let (_s, params, body) = Hashtbl.find defs s in

      (match params with
      | Params params -> 
          pr2 ("WEIRD: macro with params but no parens found: " ^ s);
          (* dont apply the macro, perhaps a redefinition *)
          ()
      | NoParam -> 
          (match body with
          | DefineBody [newtok] -> 
             (* special case when 1-1 substitution, we reuse the token *)
              id.tok <- (newtok +> TH.visitor_info_of_tok (fun _ -> 
                TH.info_of_tok id.tok))
          | DefineBody bodymacro -> 
              set_as_comment Token_c.CppMacro id;
              id.new_tokens_before <- bodymacro;
          | DefineHint hint -> 
                msg_apply_known_macro_hint s;
                id.tok <- token_from_parsinghack_hint (s,i1) hint;
          )
      );
      apply_macro_defs xs




  (* recurse *)
  | (PToken x)::xs -> apply_macro_defs xs 
  | (Parenthised (xxs, info_parens))::xs -> 
      xxs +> List.iter apply_macro_defs;
      apply_macro_defs xs
 in
 apply_macro_defs xs




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
      comments_tag = ref Ast_c.emptyComments;
    } 
  in
  TDefEOL (ii')

(* put the TDefEOL at the good place *)
let rec define_line_1 acc xs = 
  match xs with
  | [] -> List.rev acc
  | TDefine ii::xs ->
      let line = Ast_c.line_of_info ii in
      let acc = (TDefine ii) :: acc in
      define_line_2 acc line ii xs
  | TCppEscapedNewline ii::xs ->
      pr2 "WEIRD: a \\ outside a #define";
      let acc = (TCommentSpace ii) :: acc in
      define_line_1 acc xs
  | x::xs -> define_line_1 (x::acc) xs

and define_line_2 acc line lastinfo xs = 
  match xs with 
  | [] -> 
      (* should not happened, should meet EOF before *)
      pr2 "PB: WEIRD";   
      List.rev (mark_end_define lastinfo::acc)
  | x::xs -> 
      let line' = TH.line_of_tok x in
      let info = TH.info_of_tok x in

      (match x with
      | EOF ii -> 
	  let acc = (mark_end_define lastinfo) :: acc in
	  let acc = (EOF ii) :: acc in
          define_line_1 acc xs
      | TCppEscapedNewline ii -> 
          if (line' <> line) then pr2 "PB: WEIRD: not same line number";
	  let acc = (TCommentSpace ii) :: acc in
          define_line_2 acc (line+1) info xs
      | x -> 
          if line' =|= line
          then define_line_2 (x::acc) line info xs 
          else define_line_1 (mark_end_define lastinfo::acc) (x::xs)
      )

let rec define_ident acc xs = 
  match xs with
  | [] -> List.rev acc
  | TDefine ii::xs -> 
      let acc = TDefine ii :: acc in
      (match xs with
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
       
      | TCommentSpace i1::t::xs -> 

          let s = TH.str_of_tok t in
          let ii = TH.info_of_tok t in
          if s ==~ Common.regexp_alpha
          then begin
            pr2 (spf "remaping: %s to an ident in macro name" s);
	    let acc = (TCommentSpace i1) :: acc in
	    let acc = (TIdentDefine (s,ii)) :: acc in
            define_ident acc xs
          end
          else begin
            pr2 "WEIRD: weird #define body"; 
            define_ident acc xs
          end

      | _ -> 
          pr2 "WEIRD: weird #define body"; 
          define_ident acc xs
      )
  | x::xs ->
      let acc = x :: acc in
      define_ident acc xs
  


let fix_tokens_define2 xs = 
  define_ident [] (define_line_1 [] xs)

let fix_tokens_define a = 
  Common.profile_code "C parsing.fix_define" (fun () -> fix_tokens_define2 a)
      


(*****************************************************************************)
(* for the cpp-builtin, standard.h, part 0 *)
(*****************************************************************************)

let macro_body_to_maybe_hint body = 
  match body with
  | [] -> DefineBody body
  | [TIdent (s,i1)] -> 
      (match parsinghack_hint_of_string s with
      | Some hint -> DefineHint hint
      | None -> DefineBody body
      )
  | xs -> DefineBody body


let rec define_parse xs = 
  match xs with
  | [] -> []
  | TDefine i1::TIdentDefine (s,i2)::TOParDefine i3::xs -> 
      let (tokparams, _, xs) = 
        xs +> Common.split_when (function TCPar _ -> true | _ -> false) in
      let (body, _, xs) = 
        xs +> Common.split_when (function TDefEOL _ -> true | _ -> false) in
      let params = 
        tokparams +> Common.map_filter (function
        | TComma _ -> None
        | TIdent (s, _) -> Some s

        (* TODO *)
        | TDefParamVariadic (s, _) -> Some s 
        (* TODO *)
        | TEllipsis _ -> Some "..." 

        | x -> 
            (* bugfix: param of macros can be tricky *)
            let s = TH.str_of_tok x in
            if s ==~ Common.regexp_alpha
            then begin
              pr2 (spf "remaping: %s to a macro parameter" s);
              Some s
            end
            else 
              error_cant_have x
        ) in
      (* bugfix: also substitute to ident in body so cpp_engine will 
       * have an easy job.
       *)
      let body = body +> List.map (fun tok -> 
        match tok with
        | TIdent _ -> tok
        | _ -> 
            let s = TH.str_of_tok tok in
            let ii = TH.info_of_tok tok in 
            if s ==~ Common.regexp_alpha && List.mem s params
            then begin
              pr2 (spf "remaping: %s to an ident in macro body" s);
              TIdent (s, ii)
            end
            else tok
      ) +> List.map (TH.visitor_info_of_tok Ast_c.make_expanded) in
      let def = (s, (s, Params params, macro_body_to_maybe_hint body)) in
      def::define_parse xs

  | TDefine i1::TIdentDefine (s,i2)::xs -> 
      let (body, _, xs) = 
        xs +> Common.split_when (function TDefEOL _ -> true | _ -> false) in
      let body = body +> List.map 
        (TH.visitor_info_of_tok Ast_c.make_expanded) in
      let def = (s, (s, NoParam, macro_body_to_maybe_hint body)) in
      def::define_parse xs

  | TDefine i1::_ -> 
      pr2_gen i1;
      raise Impossible
  | x::xs -> define_parse xs 
      


let extract_cpp_define xs = 
  let cleaner = xs +> List.filter (fun x -> 
    not (TH.is_comment x)
  ) in
  define_parse cleaner
  



