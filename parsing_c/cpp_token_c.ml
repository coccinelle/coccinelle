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
 * there are some inconveniencies.
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
 * the parameter and then replace their occurrence in the body in a Tident.
 * Case 3 is only an issue when the expanded token is not really use
 * as usual but use for instance in concatenation as in  a ## if
 * when expanded. In the case the grammar this time will not be happy
 * so this is also easy to fix in cpp_engine.
 *
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_parsing

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* mimic standard.h *)
(* ------------------------------------------------------------------------- *)

type define_def = string * define_param * define_body
 and define_param =
   | NoParam
   | Params of define_arg list
 and define_arg = FixedArg of string | VariadicArg of string
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


(*****************************************************************************)
(* Parsing and helpers of hints  *)
(*****************************************************************************)

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
let (string_of_parsinghack_hint: parsinghack_hint -> string) = fun hint ->
  let assoc' = assoc_hint_string +> List.map (fun (a,b) -> (b,a) ) in
  Common.assoc hint assoc'



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


(* used in extract_macros for example *)
let string_of_define_def (s, params, body) =

  let s1 =
    match params with
    | NoParam ->
        spf "#define %s " s
    | Params xs ->
	let xs = List.map (function FixedArg s -> s | VariadicArg s -> s) xs in
        spf "#define %s(%s) " s (Common.join "," xs)
  in
  let s2 =
    match body with
    | DefineHint hint ->
        string_of_parsinghack_hint hint
    | DefineBody xs ->
        Common.join " " (xs +> List.map Token_helpers.str_of_tok)
  in
  s1 ^ s2


(*****************************************************************************)
(* Expansion helpers *)
(*****************************************************************************)

(* In some cases we can have macros like IDENT(if) that expands to some
 * 'int xxx_if(void)', but as the lexer will currently generate a Tif for
 * the expanded code, that may not be accepted as a token after a ##
 * in the grammar. Hence this function to remap some tokens. This is because
 * we should not use a single lexer for both working at the C level and
 * cpp level.
 *
 * update: it can also rename some TypedefIdent into TIdent, possibly
 * because of bad interaction with add_typedef_root in parsing_hacks.
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

      | Parser_c.TCppConcatOp (i1),   y ->
          let s = TH.str_of_tok y in
          let ii = TH.info_of_tok y in
          if s ==~ Common.regexp_alpha
          then begin
            pr2 (spf "remapping: %s to an ident in expanded code" s);
            x::(Parser_c.TIdent (s, ii))::remap_keyword_tokens xs
          end
          else
            x::y::remap_keyword_tokens xs

      | x, Parser_c.TCppConcatOp (i1) ->
          let s = TH.str_of_tok x in
          let ii = TH.info_of_tok x in
          if s ==~ Common.regexp_alpha
          then begin
            pr2 (spf "remapping: %s to an ident in expanded code" s);
            (Parser_c.TIdent (s, ii))::remap_keyword_tokens (y::xs)
          end
          else
            x::y::remap_keyword_tokens xs

      | _, _ ->
          x::remap_keyword_tokens (y::xs)
      )


(* works with agglomerate_concat_op_ident below *)
let rec get_ident_in_concat_op xs =
  match xs with
  | [] ->
      pr2 "weird: ident after ## operator not found";
      "", []
  | [x] ->
      (match x with
      | Parser_c.TIdent (s, i1) -> s, []
      | _ ->
          pr2 "weird: ident after ## operator not found";
          "", [x]
      )
  | x::y::xs ->
      (match x, y with
      | Parser_c.TIdent (s,i1), Parser_c.TCppConcatOp (i2) ->
          let (s2, rest) = get_ident_in_concat_op xs in
          s ^ s2, rest
      | Parser_c.TIdent (s, i1), _ ->
          s, (y::xs)
      | _ ->
          pr2 "weird: ident after ## operator not found";
          "", x::y::xs
      )

(* must be run after the expansion has been done for the parameter so
 * that all idents are actually ident, not macro parameter names.
 *)
let rec agglomerate_concat_op_ident xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::xs ->
      (* can we have ## id, and so ## as first token ? yes
       * but the semantic is different as it represents variadic
       * names so this must be handled elsewhere.
       *)
      (match x, y with
      | Parser_c.TIdent (s,i1), Parser_c.TCppConcatOp (i2) ->
          let (all_str_ident, rest_toks) =
            get_ident_in_concat_op xs
          in
          let new_s = s ^ all_str_ident in
          let i1' = Ast_c.rewrap_str new_s i1 in
          Parser_c.TIdent (new_s, i1')::agglomerate_concat_op_ident rest_toks
      | Parser_c.TCppConcatOp _, _ ->
          pr2 "weird, ## alone";
          x::agglomerate_concat_op_ident (y::xs)
      | _ ->
          x::agglomerate_concat_op_ident (y::xs)

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
let rec (cpp_engine:
          ?evaluate_concatop:bool ->
          (string , Parser_c.token list) assoc ->
          Parser_c.token list -> Parser_c.token list) =
 fun ?(evaluate_concatop=true) env xs ->
  xs +> List.map (fun tok ->
    (* expand only TIdent ? no cos the parameter of the macro
     * can actually be some 'register' so may have to look for
     * any tokens candidates for the expansion.
     * Only subtelity is maybe don't expand the TDefineIdent.
     *
     * update: in fact now the caller (define_parse) will have done
     * the job right and already replaced the macro parameter with a TIdent.
     *)
    match tok with
    | TIdent (s,i1) when List.mem_assoc s env -> 
	Common.assoc s env
    | x -> [x]
  )
  +> List.flatten
  +> remap_keyword_tokens
  +> (fun xs ->
       if evaluate_concatop
       then agglomerate_concat_op_ident xs
       else xs
  )



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
 ?evaluate_concatop
 ?(inplace_when_single=true)
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
	      let build_binder params xxs =
		let rec loop = function
		    ([],[]) -> Some (function [] -> [] | _ -> failwith "bad")
		  | ([],[[]]) -> Some (function [[]] -> [] | _ -> failwith "bad")
		  | ([],l) -> None
		  | ([(VariadicArg s)],l) ->
		      Some (function l -> List.map (function a -> (s,a)) l)
		  | ((VariadicArg _)::_,l) -> None
		  | ((FixedArg _)::_,[]) -> None
		  | ((FixedArg s)::rest,x::xs) ->
		      (match loop (rest,xs) with
			Some k ->
			  Some (function l -> (s,(List.hd l)) :: k (List.tl l))
		      |	None -> None) in
		loop (params, xxs) in
	      (match build_binder params xxs with
		None ->
                  pr2_once
		    ("WEIRD: macro with wrong number of arguments: " ^ s);
                  (* old: id.new_tokens_before <- bodymacro; *)
		  
                  (* update: if wrong number, then I just pass this macro *)
                  [Parenthised (xxs, info_parens)] +>
                  iter_token_paren (set_as_comment Token_c.CppMacro);
                  set_as_comment Token_c.CppMacro id
	      |	Some bind ->

                  let xxs' = xxs +> List.map (fun x ->
		    (tokens_of_paren_ordered x) +> List.map (fun x ->
		      TH.visitor_info_of_tok Ast_c.make_expanded x.tok
			)
		      ) in
                  id.new_tokens_before <-
                      (* !!! cpp expansion job here  !!! *)
		    cpp_engine ?evaluate_concatop
		      (bind xxs') bodymacro;

                      (* important to do that after have apply the macro,
			 otherwise will pass as argument to the macro some
			 tokens that are all TCommentCpp
                      *)
                  [Parenthised (xxs, info_parens)] +>
                  iter_token_paren (set_as_comment Token_c.CppMacro);
                  set_as_comment Token_c.CppMacro id)
		
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
      | Params _ ->
          pr2 ("WEIRD: macro with params but no parens found: " ^ s);
          (* don't apply the macro, perhaps a redefinition *)
          ()
      | NoParam ->
          (match body with
          (* bugfix: we prefer not using this special case when we come
           * from extract_macros context
           *)
          | DefineBody [newtok] when inplace_when_single ->
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
(* extracting define_def from a standard.h  *)
(*****************************************************************************)
(* was the cpp-builtin, standard.h, part 0 *)

let macro_body_to_maybe_hint body =
  match body with
  | [] -> DefineBody body
  | [TIdent (s,i1)] ->
      (match parsinghack_hint_of_string s with
      | Some hint -> DefineHint hint
      | None -> DefineBody body
      )
  | xs -> DefineBody body

exception Bad_param

let rec (define_parse: Parser_c.token list -> (string * define_def) list) =
 fun xs ->
  match xs with
  | [] -> []
  | TDefine i1::TIdentDefine (s,i2)::TOParDefine i3::xs ->
      (* note: the macro could be badly written and have no closing ')' for
       * its param, which would make us go too far away, but I don't think
       * it's important to handle such an error *)
      let def =
	try
	  let (tokparams, _, xs) =
            xs +> Common.split_when (function TCPar _ -> true | _ -> false) in
	  let (body, _, xs) =
            xs +> Common.split_when (function TDefEOL _ -> true | _ -> false) in
	  let params =
            tokparams +> Common.map_filter (function
              |	 TComma _ -> None
              |	 TIdent (s, _) -> Some (FixedArg s)

              (* TODO *)
              |	 TDefParamVariadic (s, _) -> Some (VariadicArg s)
              (* TODO *)
              |	 TEllipsis _ -> Some (VariadicArg "...")

              |	 x ->
              (* bugfix: param of macros can be tricky *)
                  let s = TH.str_of_tok x in
                  if s ==~ Common.regexp_alpha
                  then begin
                    pr2 (spf "remapping: %s to a macro parameter" s);
                    Some (FixedArg s)
                  end
                  else
                    begin
                      pr2 (spf "bad character %s in macro parameter list" s);
                      raise Bad_param
                    end) in
          (* bugfix: also substitute to ident in body so cpp_engine will
             * have an easy job.
          *)
	  let body = body +> List.map (fun tok ->
            match tok with
            | TIdent _ -> tok
            | _ ->
		let s = TH.str_of_tok tok in
		let ii = TH.info_of_tok tok in
		let params =
		  List.map
		    (function FixedArg s -> s | VariadicArg s -> s)
		    params in
		if s ==~ Common.regexp_alpha && List.mem s params
		then begin
		  pr2 (spf "remapping: %s to an ident in macro body" s);
		  TIdent (s, ii)
		end
		else tok) +>
	    List.map (TH.visitor_info_of_tok Ast_c.make_expanded) in
	  Some (s, (s, Params params, macro_body_to_maybe_hint body))
	with Bad_param -> None in
      (match def with
	Some def -> def::define_parse xs
      |	None -> define_parse xs)

  | TDefine i1::TIdentDefine (s,i2)::xs ->
      let (body, _, xs) =
        xs +> Common.split_when (function TDefEOL _ -> true | _ -> false) in
      let body = body +> List.map
        (TH.visitor_info_of_tok Ast_c.make_expanded) in
      let def = (s, (s, NoParam, macro_body_to_maybe_hint body)) in
      def::define_parse xs

  (* cf tests-bis/define_plus.c *)
  | TDefine i1::xs ->
      let line = Ast_c.line_of_info i1 in
      pr2 (spf "WEIRD: no ident in define at line %d" line);
      define_parse xs

  | x::xs -> define_parse xs



let extract_macros xs =
  let cleaner = xs +> List.filter (fun x ->
    not (TH.is_comment x)
  ) in
  define_parse cleaner


