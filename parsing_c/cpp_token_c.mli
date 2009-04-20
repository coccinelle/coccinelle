(*
 * Do stuff involving cpp macros, like expanding some macros, 
 * or try to parse well the body of the define by finding the end of
 * define virtual end-of-line token.
 *)

(* corresponds to what is in the yacfe configuration file (e.g. standard.h) *)
type define_def = string * define_param * define_body 
 and define_param = 
   | NoParam
   | Params of string list
 and define_body = 
   | DefineBody of Parser_c.token list
   | DefineHint of parsinghack_hint

   (* strongly corresponds to the TMacroXxx in the grammar and lexer and the
    * MacroXxx in the ast.
    *)
   and parsinghack_hint = 
     | HintIterator
     | HintDeclarator
     | HintMacroString
     | HintMacroStatement
     | HintAttribute
     | HintMacroIdentBuilder

(* This function work by side effect and may generate new tokens
 * in the new_tokens_before field of the token_extended in the
 * paren_grouped list. So don't forget to recall 
 * Token_views_c.rebuild_tokens_extented after this call, as well
 * as probably parsing_hacks.insert_virtual_positions as new tokens
 * are generated.
 *)
val apply_macro_defs: 
  msg_apply_known_macro:(string -> unit) ->
  msg_apply_known_macro_hint:(string -> unit) ->
  (string, define_def) Hashtbl.t ->
  Token_views_c.paren_grouped list -> unit

(* generate virtual end-of-line token *)
val fix_tokens_define : 
  Parser_c.token list -> Parser_c.token list

val extract_cpp_define : 
  Parser_c.token list -> (string, define_def) Common.assoc

