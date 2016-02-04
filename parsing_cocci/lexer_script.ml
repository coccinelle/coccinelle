(*
 * Copyright 2012-2014, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./lexer_script.ml"
# 1 "lexer_script.mll"
 
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme
let file = ref ""
let language = ref ""
let inc_line _ = Lexer_cocci.line := !Lexer_cocci.line + 1

# 13 "lexer_script.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\244\255\245\255\246\255\001\000\001\000\252\255\253\255\
    \254\255\004\000\015\000\008\000\251\255\009\000\002\000\254\255\
    \255\255\003\000\254\255\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\006\000\005\000\255\255\255\255\
    \255\255\000\000\000\000\008\000\255\255\007\000\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\009\000\000\000\000\000\000\000\255\255\255\255\000\000\000\000\
    \000\000\009\000\010\000\011\000\000\000\013\000\015\000\000\000\
    \000\000\018\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\006\000\006\000\006\000\006\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\255\255\011\000\011\000\011\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\010\000\016\000\000\000\255\255\002\000\
    \008\000\007\000\019\000\255\255\255\255\255\255\000\000\004\000\
    \013\000\011\000\000\000\255\255\000\000\000\000\011\000\011\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\011\000\000\000\
    \005\000\012\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\255\255\
    ";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\009\000\009\000\
    \009\000\009\000\011\000\013\000\255\255\255\255\255\255\255\255\
    \255\255\010\000\010\000\010\000\010\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\014\000\255\255\009\000\000\000\
    \000\000\000\000\017\000\009\000\009\000\009\000\255\255\000\000\
    \004\000\010\000\255\255\009\000\255\255\255\255\010\000\010\000\
    \010\000\255\255\255\255\255\255\255\255\255\255\010\000\255\255\
    \000\000\005\000\255\255\255\255\009\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\010\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\014\000\017\000\009\000\255\255\255\255\255\255\
    \011\000\013\000\255\255\255\255\255\255\255\255\255\255\010\000\
    ";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 20 "lexer_script.mll"
           ( TScriptData (tok lexbuf) )
# 120 "lexer_script.ml"

  | 1 ->
# 21 "lexer_script.mll"
        ( TScriptData (tok lexbuf) )
# 125 "lexer_script.ml"

  | 2 ->
# 22 "lexer_script.mll"
        ( TScriptData (tok lexbuf) )
# 130 "lexer_script.ml"

  | 3 ->
# 24 "lexer_script.mll"
      ( inc_line();
	let text = tok lexbuf in
	let text =
	  if !language = "ocaml"
	  then
	    Printf.sprintf "%s# %d \"%s\"%s"
	      text !Lexer_cocci.line !file text
	  else text in
	TScriptData text )
# 143 "lexer_script.ml"

  | 4 ->
# 33 "lexer_script.mll"
         ( TArobArob )
# 148 "lexer_script.ml"

  | 5 ->
# 34 "lexer_script.mll"
         ( TArob )
# 153 "lexer_script.ml"

  | 6 ->
# 35 "lexer_script.mll"
         ( TScriptData (tok lexbuf) )
# 158 "lexer_script.ml"

  | 7 ->
# 36 "lexer_script.mll"
                   ( token lexbuf )
# 163 "lexer_script.ml"

  | 8 ->
# 37 "lexer_script.mll"
                   ( token lexbuf )
# 168 "lexer_script.ml"

  | 9 ->
# 38 "lexer_script.mll"
         ( TScriptData (Printf.sprintf "\"%s\"" (string lexbuf)) )
# 173 "lexer_script.ml"

  | 10 ->
# 39 "lexer_script.mll"
         ( TScriptData (Printf.sprintf "'%s'" (cstring lexbuf)) )
# 178 "lexer_script.ml"

  | 11 ->
# 40 "lexer_script.mll"
         ( EOF )
# 183 "lexer_script.ml"

  | 12 ->
# 41 "lexer_script.mll"
      ( raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) )
# 188 "lexer_script.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 14
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 45 "lexer_script.mll"
                        ( "" )
# 199 "lexer_script.ml"

  | 1 ->
let
# 46 "lexer_script.mll"
          x
# 205 "lexer_script.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 46 "lexer_script.mll"
                        ( (String.make 1 x) ^ string lexbuf )
# 209 "lexer_script.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and cstring lexbuf =
    __ocaml_lex_cstring_rec lexbuf 17
and __ocaml_lex_cstring_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 49 "lexer_script.mll"
                        ( "" )
# 220 "lexer_script.ml"

  | 1 ->
let
# 50 "lexer_script.mll"
          x
# 226 "lexer_script.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 50 "lexer_script.mll"
                        ( (String.make 1 x) ^ cstring lexbuf )
# 230 "lexer_script.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_cstring_rec lexbuf __ocaml_lex_state

;;

