(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Lexer/parser for sgen config file *)

{

(* ------------------------------------------------------------------------- *)
(* TYPES *)

(* the config syntax has pretty low complexity and is very lenient, so we just
 * interpret the tokens directly and skip the lexical analysis. *)

type attribute =
  | Org of string * string list | Report of string * string list

type part =
  | Description of string
  | Limitations of string list
  | Keywords of string
  | Confidence of string
  | Comments of string
  | Options of string
  | Authors of string list (* author, affiliation, license in one *)
  | Url of string
  | Rule of (string * string option) * attribute list

(* the table is for line numbers to produce more meaningful error messages *)
let table = ref (Array.make 0 (0,0))
exception Eof

let get_start_line lexbuf =
  match !table.(Lexing.lexeme_start lexbuf) with (l,_) -> l

let get_column lexbuf =
  match !table.(Lexing.lexeme_start lexbuf) with (_,c) -> c

let get_position_str lexbuf =
  "around line " ^ (string_of_int (get_start_line lexbuf))
  ^ ", column " ^ (string_of_int (get_column lexbuf))

let error prob lexbuf =
  let pos = get_position_str lexbuf in failwith ("Config error: " ^ prob ^ pos)

let illegal = error "Illegal syntax "

let split_list delim = Str.split (Str.regexp (" *"^delim^" *"))

}

(* ------------------------------------------------------------------------- *)
(* LEXING RULES *)

let sp = [' ' '\t']
let ws = ['\n' '\r' '\012' '\013' ' ' '\t']*
let notws = [^ '\n' '\r' '\012' '\013' ' ' '\t']*
let notnl = [^ '\n' '\r' '\012' '\013']*
let equal = (sp* "=" sp*)
let colon = (sp* ":" sp*)
let percent = (sp* "%" sp*)
let letter = ['A'-'Z' 'a'-'z' '_']
let number  = ['0'-'9']*
let cname = letter (letter | number)*

rule token = parse
  | ("description"|"d") equal (notnl as d) { Description d }
  | ("keywords"   |"k") equal (notnl as v) { Keywords v }
  | ("confidence" |"c") equal (notws as c) { Confidence c }
  | ("comments"   |"m") equal (notnl as m) { Comments m }
  | ("options"    |"o") equal (notnl as o) { Options o }
  | ("url"        |"u") equal (notnl as u) { Url u }
  | ("limitations"|"l") equal (notnl as l) { Limitations (split_list "|" l) }
  | ("author"|"authors"|"a") equal (notnl as a) { Authors (split_list "|" a) }

  (* for naming unnamed rules. e.g. 8:name = ... for the rule on line 8. *)
  | (number as oldrule) colon (cname as newrule) equal {
      Rule ((oldrule, Some newrule), cocci_rule lexbuf)
    }

  (* standard rules with org and report messages *)
  | (cname as rulenm) equal {
      Rule ((rulenm, None), cocci_rule lexbuf)
    }

  (* comments and whitespace are skipped *)
  | ws | "//" [^ '\n']* { token lexbuf }
  | "/*" { comment lexbuf }
  | eof { raise Eof}
  | _ { illegal lexbuf }

and comment = parse
  | "*/" {token lexbuf}
  | eof { error "unclosed comment, needs \"*/\"! " lexbuf}
  | _ {comment lexbuf }

and cocci_rule = parse
  | ws* "org" colon {
      let (a,b) = format_string lexbuf in Org (a,b) :: (cocci_rule lexbuf)
    }
  | ws* "report" colon {
      let (a,b) = format_string lexbuf in Report (a,b) :: (cocci_rule lexbuf)
    }
  | _ { [] }

(* possibly formatted org and report messages *)
and format_string = parse
  | "\"" (notnl as msg) "\"" percent "(" (notnl as lst) ")" {
      (msg, split_list "," lst)
    }
  | "\"" (notnl as msg) "\"" percent (notnl as lst) { (msg,[lst]) }
  | "\"" (notnl as msg) "\"" { (msg, []) }
  | _ { illegal lexbuf }
