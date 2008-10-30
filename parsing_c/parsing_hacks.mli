open Common 

(* Try detect some cpp idioms so can parse as-is files by adjusting or
 * commenting some tokens. Parsing hack style. Sometime use indentation info,
 * sometimes do some kind of lalr(k) by finding patterns. Often try to
 * work on better token representation, like ifdef-paren-ized, brace-ized,
 * paren-ized, so can do easier pattern matching to more easily match
 * complex cpp idiom pattern. Also try to get context info such as
 * whether the token is in a initializer as some common patterns have different
 * use depending on context.
 * 
 * 
 * Example of cpp idioms:
 *  - if 0 for commenting stuff (not always code, sometimes just real comments)
 *  - ifdef old version
 *  - ifdef funheader
 *  - ifdef statements, ifdef expression, ifdef-mid
 *  - macro toplevel (with or without ptvirg)
 *  - macro foreach
 *  - macro higher order
 *  - macro declare
 *  - macro debug
 *  - macro no ptvirg
 *  - macro string, and macro function string taking param and ##
 *  - macro attribute
 * Cf the TMacroXxx in parser_c.mly and MacroXxx in ast_c.ml
 * 
 * Also try infer typedef.
 * 
 * Also do other stuff involving cpp like expanding some macros, 
 * or try parse well define body by finding the end of define virtual 
 * end-of-line token.
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

val _defs : (string, define_def) Hashtbl.t ref

(* can reset it *)
val ifdef_paren_cnt: int ref

val fix_tokens_define : Parser_c.token list -> Parser_c.token list
val extract_cpp_define : Parser_c.token list -> (string, define_def) assoc


val fix_tokens_cpp : Parser_c.token list -> Parser_c.token list

(* next stream tokens -> passed stream tokens -> final next token *)
val lookahead : 
  pass:int -> 
  Parser_c.token list -> Parser_c.token list -> Parser_c.token

