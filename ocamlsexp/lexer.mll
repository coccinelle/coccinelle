{
  (* File: lexer.mll

      Copyright (C) 2005-

        Jane Street Holding, LLC
        Author: Markus Mottl
        email: mmottl@janestcapital.com
        WWW: http://www.janestcapital.com/ocaml

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  *)

  (** Lexer: Lexer Specification for S-expressions *)

  open Printf
  open Lexing
  open Parser

  let char_for_backslash = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    | c   -> c

  let double_nl = "\013\010"

  let dec_code c1 c2 c3 =
    100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

  let hex_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48 in
    let d2 = Char.code c2 in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48 in
    val1 * 16 + val2

  let found_newline lexbuf diff =
    let curr_p = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        curr_p with
        pos_lnum = curr_p.pos_lnum + 1;
        pos_bol = max 1 (curr_p.pos_cnum - diff);
      }

  let get_lexeme_len lexbuf = lexbuf.lex_curr_pos - lexbuf.lex_start_pos
}

let newline = ('\010' | '\013' | "\013\010")
let space = [' ' '\009' '\012']
let whitespace = [' ' '\010' '\013' '\009' '\012']
let backslash_escapes = ['\\' '"' '\'' 'n' 't' 'b' 'r']

rule main buf = parse
  | newline { found_newline lexbuf 1; main buf lexbuf }
  | space+ { main buf lexbuf }
  | ';' [^ '\n' '\r']+ { main buf lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '"'
      {
        scan_string buf lexbuf;
        let str = Buffer.contents buf in
        Buffer.clear buf;
        STRING str
      }
  | ([^ ';' '(' ')' '"'] # whitespace)+ as str { STRING str }
  | eof { EOF }

and scan_string buf = parse
  | '"' { () }
  | '\\' ['\010' '\013'] [' ' '\009']*
      {
        let len = get_lexeme_len lexbuf in
        found_newline lexbuf (len - 2);
        scan_string buf lexbuf
      }
  | '\\' "\013\010" [' ' '\009']*
      {
        let len = get_lexeme_len lexbuf in
        found_newline lexbuf (len - 3);
        scan_string buf lexbuf
      }
  | '\\' (backslash_escapes as c)
      {
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf lexbuf
      }
  | '\\' (['0'-'9'] as c1) (['0'-'9'] as c2) (['0'-'9']  as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let pos = lexbuf.lex_curr_p in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Buffer.add_char buf (Char.chr v);
        scan_string buf lexbuf
      }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as c1) (['0'-'9' 'a'-'f' 'A'-'F'] as c2)
      {
        let v = hex_code c1 c2 in
        if v > 255 then (
          let pos = lexbuf.lex_curr_p in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\x%c%c'"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol - 3)
              c1 c2 in
          failwith msg);
        Buffer.add_char buf (Char.chr v);
        scan_string buf lexbuf
      }
  | '\\' (_ as c)
      {
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        scan_string buf lexbuf
      }
  | ['\010' '\013'] as c
      {
        found_newline lexbuf 1;
        Buffer.add_char buf c;
        scan_string buf lexbuf
      }
  | "\013\010"
      {
        found_newline lexbuf 2;
        Buffer.add_string buf double_nl;
        scan_string buf lexbuf
      }
  | [^ '\\' '"']+
      {
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;
        scan_string buf lexbuf
      }
  | eof { failwith "Sexplib.Lexer.scan_string: unterminated string" }

{
  let main ?buf =
    let buf =
      match buf with
      | None -> Buffer.create 64
      | Some buf -> Buffer.clear buf; buf
    in
    main buf
}
