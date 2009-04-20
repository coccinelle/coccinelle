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
(* Types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* cpp part 1 for standard.h *)
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
(* Helpers *)
(*****************************************************************************)

(* To expand the parameter of the macro. The env corresponds to the actual
 * code that is binded to the parameters of the macro.
 * TODO? recurse ? fixpoint ? the expansion may also contain macro.
 * Or to macro expansion in a strict manner, that is process first
 * the parameters, expands macro in params, and then process enclosing
 * macro call.
 *)
let rec (cpp_engine: (string , Parser_c.token list) assoc -> 
          Parser_c.token list -> Parser_c.token list) = 
 fun env xs ->
  xs +> List.map (fun tok -> 
    match tok with
    | TIdent (s,i1) when List.mem_assoc s env -> Common.assoc s env
    | x -> [x]
  )
  +> List.flatten



(* ------------------------------------------------------------------------- *)
(* cpp-builtin part2, macro, using standard.h or other defs *)
(* ------------------------------------------------------------------------- *)

(* Thanks to this function many stuff are not anymore hardcoded in ocaml code
 * (but they are now hardcoded in standard.h ...)
 *
 * 
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
                ()
              end
              else 

                let xxs' = xxs +> List.map (fun x -> 
                  (tokens_of_paren_ordered x) +> List.map (fun x -> 
                    TH.visitor_info_of_tok Ast_c.make_expanded x.tok
                  )
                ) in
                id.new_tokens_before <-
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
(* The #define tricks *)
(*****************************************************************************)

(* ugly hack, a better solution perhaps would be to erase TDefEOL 
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

      (* bugfix: ident of macro (as well as params, cf below) can be tricky *)
      (* todo, subst in body of define ? *)
      | TCommentSpace i1
        ::(Tinline i2|Tconst i2|Tvolatile i2|Tstatic i2
          |Tattribute i2
        )
        ::xs -> 
          let s2 = Ast_c.str_of_info i2 in
	  let acc = (TCommentSpace i1) :: acc in
	  let acc = (TIdentDefine (s2,i2)) :: acc in
          define_ident acc xs

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
        (* bugfix: param of macros can be tricky *)
        | TDefParamVariadic (s, _) -> Some s 
        (* TODO *)
        | TEllipsis _ -> Some "..." 
        | Tregister _ -> Some "register"
        | Tconst _ -> Some "const"
        | Tint _ -> Some "int"
        | Tsigned _ -> Some "signed"

        | x -> error_cant_have x
        ) in
      let body = body +> List.map 
        (TH.visitor_info_of_tok Ast_c.make_expanded) in
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
  



