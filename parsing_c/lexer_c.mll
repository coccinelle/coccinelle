{
(* Copyright (C) 2002-2008 Yoann Padioleau
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

open Ast_c (* to factorise tokens, OpAssign, ... *)

(*****************************************************************************)
(*
 * subtil: ocamllex use side effect on lexbuf, so must take care. 
 * For instance must do   
 * 
 *  let info = tokinfo lexbuf in 
 *  TComment (info +> tok_add_s (comment lexbuf)) 
 * 
 * and not 
 * 
 *   TComment (tokinfo lexbuf +> tok_add_s (comment lexbuf)) 
 * 
 * because of the "wierd" order of evaluation of OCaml.
 *
 * note: can't use Lexer_parser._lexer_hint here to do different
 * things, because now we call the lexer to get all the tokens
 * (tokens_all), and then we parse. So we can't have the _lexer_hint
 * info here. We can have it only in parse_c. For the same reason, the
 * typedef handling here is now useless. 
 *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !Flag_parsing_c.verbose_lexing 
  then Common.pr2 s

(*****************************************************************************)


exception Lexical of string

let tok     lexbuf  = Lexing.lexeme lexbuf

let tokinfo lexbuf  = 
  { 
    pinfo = Ast_c.OriginTok {
      Common.charpos = Lexing.lexeme_start lexbuf; 
      Common.str     = Lexing.lexeme lexbuf;
      (* info filled in a post-lexing phase *)
      Common.line = -1; 
      Common.column = -1; 
      Common.file = "";
    };
   (* must generate a new ref each time, otherwise share *)
    cocci_tag = ref Ast_c.emptyAnnot;
    comments_tag = ref Ast_c.emptyComments;
  }

let tok_add_s s ii = Ast_c.rewrap_str ((Ast_c.str_of_info ii) ^ s) ii
    

(* opti: less convenient, but using a hash is faster than using a match *)
let keyword_table = Common.hash_of_list [

  "void",   (fun ii -> Tvoid ii); 
  "char",   (fun ii -> Tchar ii);    
  "short",  (fun ii -> Tshort ii); 
  "int",    (fun ii -> Tint ii); 
  "long",   (fun ii -> Tlong ii); 
  "float",  (fun ii -> Tfloat ii); 
  "double", (fun ii -> Tdouble ii);  

  "unsigned", (fun ii -> Tunsigned ii);  
  "signed",   (fun ii -> Tsigned ii);
  
  "auto",     (fun ii -> Tauto ii);    
  "register", (fun ii -> Tregister ii);  
  "extern",   (fun ii -> Textern ii); 
  "static",   (fun ii -> Tstatic ii);

  "const",    (fun ii -> Tconst ii);
  "volatile", (fun ii -> Tvolatile ii); 
  
  "struct",  (fun ii -> Tstruct ii); 
  "union",   (fun ii -> Tunion ii); 
  "enum",    (fun ii -> Tenum ii);  
  "typedef", (fun ii -> Ttypedef ii);  
  
  "if",      (fun ii -> Tif ii);      
  "else",     (fun ii -> Telse ii); 
  "break",   (fun ii -> Tbreak ii);   
  "continue", (fun ii -> Tcontinue ii);
  "switch",  (fun ii -> Tswitch ii);  
  "case",     (fun ii -> Tcase ii);  
  "default", (fun ii -> Tdefault ii); 
  "for",     (fun ii -> Tfor ii);  
  "do",      (fun ii -> Tdo ii);      
  "while",   (fun ii -> Twhile ii);  
  "return",  (fun ii -> Treturn ii);
  "goto",    (fun ii -> Tgoto ii); 
  
  "sizeof", (fun ii -> Tsizeof ii);   


  (* gccext: cppext: linuxext: synonyms *)
  "asm",     (fun ii -> Tasm ii);
  "__asm__", (fun ii -> Tasm ii);
  "__asm",   (fun ii -> Tasm ii);

  "inline",     (fun ii -> Tinline ii);
  "__inline__", (fun ii -> Tinline ii);
  "__inline",   (fun ii -> Tinline ii);
  "INLINE",     (fun ii -> Tinline ii); 
  "_INLINE_",   (fun ii -> Tinline ii); 
  "__INLINE__",   (fun ii -> Tinline ii); 

  "__attribute__", (fun ii -> Tattribute ii);
  "__attribute", (fun ii -> Tattribute ii);

  "typeof", (fun ii -> Ttypeof ii);
  "__typeof__", (fun ii -> Ttypeof ii);

  "__signed__",     (fun ii -> Tsigned ii);

  "__const__",     (fun ii -> Tconst ii);
  "__const",     (fun ii -> Tconst ii);

  "__volatile__",  (fun ii -> Tvolatile ii); 
  "__volatile",    (fun ii -> Tvolatile ii);  
  
 ]

let error_radix s = 
  ("numeric " ^ s ^ " constant contains digits beyond the radix:")

}

(*****************************************************************************)
let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

(* not used for the moment *)
let punctuation = ['!' '"' '#' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':'
		   ';' '<' '=' '>' '?' '[' '\\' ']' '^' '{' '|' '}' '~']
let space = [' ' '\t' '\n' '\r' '\011' '\012' ]
let additionnal = [ ' ' '\b' '\t' '\011' '\n' '\r' '\007' ] 
(* 7 = \a = bell in C. this is not the only char allowed !! 
 * ex @ and $ ` are valid too 
 *)

let cchar = (letter | digit | punctuation | additionnal) 

let sp = [' ' '\t']+
let spopt = [' ' '\t']*

let dec = ['0'-'9']
let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal = ('0' | (['1'-'9'] dec*))
let octal   = ['0']        oct+
let hexa    = ("0x" |"0X") hex+ 


let pent   = dec+
let pfract = dec+
let sign = ['-' '+']
let exp  = ['e''E'] sign? dec+
let real = pent exp | ((pent? '.' pfract | pent '.' pfract? ) exp?)

let id = letter (letter | digit) *

(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  (* note: this lexer generate tokens for comments!! so can not give 
   * this lexer as-is to the parsing function. Must preprocess it, hence
   * use techniques like cur_tok ref in parse_c.ml
   *)

  | ['\n'] [' ' '\t' '\r' '\011' '\012' ]*
      (* starting a new line; the newline character followed by whitespace *)
      { TCommentNewline (tokinfo lexbuf) }
  | [' ' '\t' '\r' '\011' '\012' ]+  
      { TCommentSpace (tokinfo lexbuf) }
  | "/*" 
      { let info = tokinfo lexbuf in 
        let com = comment lexbuf in
        TComment(info +> tok_add_s com) 
      }


  (* C++ comment are allowed via gccext, but normally they are deleted by cpp.
   * So need this here only when dont call cpp before.
   *)
  | "//" [^'\r' '\n' '\011']*    { TComment (tokinfo lexbuf) } 

  (* ----------------------------------------------------------------------- *)
  (* cpp *)
  (* ----------------------------------------------------------------------- *)

  (* old:
   *   | '#'		{ endline lexbuf} // should be line, and not endline 
   *   and endline = parse  | '\n' 	{ token lexbuf}  
   *                        |	_	{ endline lexbuf} 
   *)

  (* todo?:
   *  have found a # #else  in "newfile-2.6.c",  legal ?   and also a  #/* ... 
   *    => just "#" -> token {lexbuf} (that is ignore)
   *  il y'a 1 #elif  sans rien  apres
   *  il y'a 1 #error sans rien  apres
   *  il y'a 2  mov dede, #xxx    qui genere du coup exn car
   *  entouré par des #if 0
   *  => make as for comment, call a comment_cpp that when #endif finish the
   *   comment and if other cpp stuff raise exn
   *  il y'a environ 10  #if(xxx)  ou le ( est collé direct
   *  il y'a des include"" et include<
   *  il y'a 1 ` (derriere un #ifndef linux)
   *)



  (* ---------------------- *)
  (* misc *)
  (* ---------------------- *)
      
   (* #pragma pack
    * #pragma GCC set_debug_pwd
    * #pragma alloc_text
    * #pragma options align=packed
    * #pragma options align=reset
    * #pragma options align=power
    * #pragma	pack(2)
    * etc
    *)
  | "#pragma" sp  [^'\n']* '\n'  
      { TCommentCpp (CppDirective, tokinfo lexbuf) }

  | "#" [' ' '\t']* "ident"   [' ' '\t']+  [^'\n']+ '\n' 

  | "#" [' ' '\t']* "line"   [' ' '\t']+  [^'\n']+ '\n' 

  | "#" [' ' '\t']* "error"   [' ' '\t']+  [^'\n']* '\n' 
  | "#" [' ' '\t']* "warning" [' ' '\t']+  [^'\n']* '\n'                     
  | "#" [' ' '\t']* "abort"   [' ' '\t']+  [^'\n']* '\n'

      { TCommentCpp (CppDirective, tokinfo lexbuf) }

  (* only after cpp, ex: # 1 "include/linux/module.h" 1 *)
  | "#" sp pent sp  '"' [^ '"']* '"' (spopt pent)*  spopt '\n'
      { TCommentCpp (CppDirective, tokinfo lexbuf) }


  (* in drivers/char/tpqic02.c, in old version of the kernel *)
  | "#" [' ' '\t']* "error"     { TCommentCpp (CppDirective,tokinfo lexbuf) }


  | "#" [' ' '\t']* '\n'        { TCommentCpp (CppDirective,tokinfo lexbuf) }


  (* ---------------------- *)
  (* #define, #undef *)
  (* ---------------------- *)

  (* the rest of the lexing/parsing of define is done in fix_tokens_define 
   * where we parse until a TCppEscapedNewline and generate a TDefEol
   *)
  | "#" [' ' '\t']* "define" { TDefine (tokinfo lexbuf) } 

  | "#" [' ' '\t']* "undef" [' ' '\t']+ id
      { let info = tokinfo lexbuf in 
        TCommentCpp (CppDirective,info +> tok_add_s (cpp_eat_until_nl lexbuf))
      }


  (* could generate separate token for #, ## and then exten grammar,
   * but there can be ident in many different places, in expression
   * but also in declaration, in function name. So having 3 tokens
   * for an ident does not work well with how we add info in
   * ast_c. So better to generate just one token, just one info,
   * even if have later to reanalyse those tokens and unsplit.
   *)

  | ((id as s)  "...")
      { TDefParamVariadic (s, tokinfo lexbuf) }

  (* cppext: string concatenation *)
  |  id   ([' ''\t']* "##" [' ''\t']* id)+ 
      { let info = tokinfo lexbuf in
        TIdent (tok lexbuf, info)
      }

  (* cppext: stringification *)
  |  "#" id  
      { let info = tokinfo lexbuf in
        TIdent (tok lexbuf, info)
      }

  (* cppext: gccext: ##args for variadic macro *)
  |  "##" [' ''\t']* id
      { let info = tokinfo lexbuf in
        TIdent (tok lexbuf, info)
      }

  (* only in cpp directives normally *)
  | "\\" '\n' { TCppEscapedNewline (tokinfo lexbuf) }


  (* ---------------------- *)
  (* #include *)
  (* ---------------------- *)

  (* The difference between a local "" and standard <> include is computed
   * later in parser_c.mly. So redo a little bit of lexing there. Ugly but
   * simpler to generate a single token here.  *)
  | (("#" [' ''\t']* "include" [' ' '\t']*) as includes) 
    (('"' ([^ '"']+) '"' | 
     '<' [^ '>']+ '>' | 
      ['A'-'Z''_']+ 
    ) as filename)
      {
        let info = tokinfo lexbuf in 
        TInclude (includes, filename, Ast_c.noInIfdef(), info)
      }

   (* linuxext: special_for_no_exn: in atm/ambassador.c *)
  | "#include UCODE(" [^'\n']+  '\n'    
      { TCommentCpp (CppDirective, tokinfo lexbuf) }


  (* ---------------------- *)
  (* #ifdef *)
  (* ---------------------- *)

  (* '0'+ because sometimes it is a #if 000 *)
  | "#" [' ' '\t']* "if" [' ' '\t']* '0'+           (* [^'\n']*  '\n' *)
      { let info = tokinfo lexbuf in 
        TIfdefBool (false, info +> tok_add_s (cpp_eat_until_nl lexbuf)) 
      }

  | "#" [' ' '\t']* "if" [' ' '\t']* '1'   [^'\n']*  '\n'
      { let info = tokinfo lexbuf in 
        TIfdefBool (true, info) 

      } 

  | "#" [' ' '\t']* "ifdef" [' ' '\t']* "__cplusplus"   [^'\n']*  '\n'
      { let info = tokinfo lexbuf in 
        TIfdefMisc (false, info) 
      }


  (* linuxext: different possible variations (we not manage all of them):
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0)
    #if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,2)
    #if LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0)
    #if LINUX_VERSION_CODE > KERNEL_VERSION(2,3,0)
    #if LINUX_VERSION_CODE < 0x020600
    #if LINUX_VERSION_CODE >= 0x2051c
    #if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
    #if !(LINUX_VERSION_CODE > KERNEL_VERSION(2,5,73)) 
    #if STREAMER_IOCTL && (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,20)  &&  LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0)
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,20) && \
    # if defined(MODULE) && LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,30)
    #if LINUX_VERSION_CODE > LinuxVersionCode(2,3,12)
    #elif LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,93)
    #ifndef LINUX_VERSION_CODE
    #if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,2,0) || \
    (LINUX_VERSION_CODE > ASC_LINUX_VERSION(2,3,0) && \
    LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,4,0))
    #if (KERNEL_VERSION(2,4,0) > LINUX_VERSION_CODE)
    #if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
    # if defined(MODULE) && LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,30)
    
  *)

  (* linuxext: *)
  | "#" spopt "if" sp "("?  "LINUX_VERSION_CODE" sp (">=" | ">") sp
      { let info = tokinfo lexbuf in 
        TIfdefVersion (true, info +> tok_add_s (cpp_eat_until_nl lexbuf)) 
      } 
  (* linuxext: *)
  | "#" spopt "if" sp "!" "("?  "LINUX_VERSION_CODE" sp (">=" | ">") sp
  | "#" spopt "if" sp ['(']?  "LINUX_VERSION_CODE" sp ("<=" | "<") sp
      
      { let info = tokinfo lexbuf in 
        TIfdefVersion (false, info +> tok_add_s (cpp_eat_until_nl lexbuf)) 
      } 


  (* can have some ifdef 0  hence the letter|digit even at beginning of word *)
  | "#" [' ''\t']* "ifdef"  [' ''\t']+ (letter|digit) ((letter|digit)*) [' ''\t']*  
      { TIfdef (tokinfo lexbuf) }
  | "#" [' ''\t']* "ifndef" [' ''\t']+ (letter|digit) ((letter|digit)*) [' ''\t']*  
      { TIfdef (tokinfo lexbuf) }
  | "#" [' ''\t']* "if" [' ' '\t']+                                           
      { let info = tokinfo lexbuf in 
        TIfdef (info +> tok_add_s (cpp_eat_until_nl lexbuf)) 
      }
  | "#" [' ' '\t']* "if" '('                
      { let info = tokinfo lexbuf in 
        TIfdef (info +> tok_add_s (cpp_eat_until_nl lexbuf))
      }

  | "#" [' ' '\t']* "elif" [' ' '\t']+ 
      { let info = tokinfo lexbuf in 
        TIfdefelif (info +> tok_add_s (cpp_eat_until_nl lexbuf)) 
      } 


  (* can have #endif LINUX *)
  | "#" [' ' '\t']* "endif" [^'\n']* '\n'    { TEndif     (tokinfo lexbuf) }
  (* can be at eof *)
  | "#" [' ' '\t']* "endif"                  { TEndif     (tokinfo lexbuf) }

  | "#" [' ' '\t']* "else" [' ' '\t' '\n']   { TIfdefelse (tokinfo lexbuf) }
  (* there is a file in 2.6 that have this *)
  | "##" [' ' '\t']* "else" [' ' '\t' '\n']  { TIfdefelse (tokinfo lexbuf) }




  (* ----------------------------------------------------------------------- *)
  (* C symbols *)
  (* ----------------------------------------------------------------------- *)
   (* stdC:
    ...   &&   -=   >=   ~   +   ;   ]    
    <<=   &=   ->   >>   %   ,   <   ^    
    >>=   *=   /=   ^=   &   -   =   {    
    !=    ++   <<   |=   (   .   >   |    
    %=    +=   <=   ||   )   /   ?   }    
        --   ==   !    *   :   [   
    recent addition:    <:  :>  <%  %> 
    only at processing: %:  %:%: # ##  
   *) 


  | '[' { TOCro(tokinfo lexbuf) }   | ']' { TCCro(tokinfo lexbuf) }
  | '(' { TOPar(tokinfo lexbuf)   } | ')' { TCPar(tokinfo lexbuf)   }
  | '{' { TOBrace(tokinfo lexbuf) } | '}' { TCBrace(tokinfo lexbuf) }

  | '+' { TPlus(tokinfo lexbuf) }   | '*' { TMul(tokinfo lexbuf) }     
  | '-' { TMinus(tokinfo lexbuf) }  | '/' { TDiv(tokinfo lexbuf) } 
  | '%' { TMod(tokinfo lexbuf) } 

  | "++"{ TInc(tokinfo lexbuf) }    | "--"{ TDec(tokinfo lexbuf) }

  | "="  { TEq(tokinfo lexbuf) } 

  | "-=" { TAssign (OpAssign Minus, (tokinfo lexbuf))} 
  | "+=" { TAssign (OpAssign Plus, (tokinfo lexbuf))} 
  | "*=" { TAssign (OpAssign Mul, (tokinfo lexbuf))}   
  | "/=" { TAssign (OpAssign Div, (tokinfo lexbuf))} 
  | "%=" { TAssign (OpAssign Mod, (tokinfo lexbuf))} 
  | "&=" { TAssign (OpAssign And, (tokinfo lexbuf))}  
  | "|=" { TAssign (OpAssign Or, (tokinfo lexbuf)) } 
  | "^=" { TAssign(OpAssign Xor, (tokinfo lexbuf))} 
  | "<<=" {TAssign (OpAssign DecLeft, (tokinfo lexbuf)) } 
  | ">>=" {TAssign (OpAssign DecRight, (tokinfo lexbuf))}

  | "==" { TEqEq(tokinfo lexbuf) }  | "!=" { TNotEq(tokinfo lexbuf) } 
  | ">=" { TSupEq(tokinfo lexbuf) } | "<=" { TInfEq(tokinfo lexbuf) } 
  | "<"  { TInf(tokinfo lexbuf) }   | ">"  { TSup(tokinfo lexbuf) }

  | "&&" { TAndLog(tokinfo lexbuf) } | "||" { TOrLog(tokinfo lexbuf) }
  | ">>" { TShr(tokinfo lexbuf) }    | "<<" { TShl(tokinfo lexbuf) }
  | "&"  { TAnd(tokinfo lexbuf) }    | "|" { TOr(tokinfo lexbuf) } 
  | "^"  { TXor(tokinfo lexbuf) }
  | "..." { TEllipsis(tokinfo lexbuf) }
  | "->"   { TPtrOp(tokinfo lexbuf) }  | '.'  { TDot(tokinfo lexbuf) }  
  | ','    { TComma(tokinfo lexbuf) }  
  | ";"    { TPtVirg(tokinfo lexbuf) }
  | "?"    { TWhy(tokinfo lexbuf) }    | ":"   { TDotDot(tokinfo lexbuf) } 
  | "!"    { TBang(tokinfo lexbuf) }   | "~"   { TTilde(tokinfo lexbuf) }

  | "<:" { TOCro(tokinfo lexbuf) } | ":>" { TCCro(tokinfo lexbuf) } 
  | "<%" { TOBrace(tokinfo lexbuf) } | "%>" { TCBrace(tokinfo lexbuf) }



  (* ----------------------------------------------------------------------- *)
  (* C keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* StdC: must handle at least name of length > 509, but can
   * truncate to 31 when compare and truncate to 6 and even lowerise
   * in the external linkage phase 
   *)
  | letter (letter | digit) *  
      { let info = tokinfo lexbuf in
        let s = tok lexbuf in
        Common.profile_code "C parsing.lex_ident" (fun () -> 
          match Common.optionise (fun () -> Hashtbl.find keyword_table s)
          with
          | Some f -> f info

           (* parse_typedef_fix. note: now this is no more useful, cos
            * as we use tokens_all, it first parse all as an ident and
            * later transform an indent in a typedef. so this job is
            * now done in parse_c.ml.
            * 
            *    if Lexer_parser.is_typedef s 
            *    then TypedefIdent (s, info)
            *    else TIdent (s, info)
            *)

          | None -> TIdent (s, info)
        )            
      }	

  (* ----------------------------------------------------------------------- *)
  (* C constant *)
  (* ----------------------------------------------------------------------- *)

  | "'"     
      { let info = tokinfo lexbuf in 
        let s = char lexbuf   in 
        TChar     ((s,   IsChar),  (info +> tok_add_s (s ^ "'"))) 
      }
  | '"'     
      { let info = tokinfo lexbuf in
        let s = string lexbuf in 
        TString   ((s,   IsChar),  (info +> tok_add_s (s ^ "\""))) 
      }
  (* wide character encoding, TODO L'toto' valid ? what is allowed ? *)
  | 'L' "'" 
      { let info = tokinfo lexbuf in 
        let s = char lexbuf   in 
        TChar     ((s,   IsWchar),  (info +> tok_add_s (s ^ "'"))) 
      } 
  | 'L' '"' 
      { let info = tokinfo lexbuf in 
        let s = string lexbuf in 
        TString   ((s,   IsWchar),  (info +> tok_add_s (s ^ "\""))) 
      }


  (* Take care of the order ? No because lex try the longest match. The
   * strange diff between decimal and octal constant semantic is not
   * understood too by refman :) refman:11.1.4, and ritchie.
   *)

  | (( decimal | hexa | octal) 
        ( ['u' 'U'] 
        | ['l' 'L']  
        | (['l' 'L'] ['u' 'U'])
        | (['u' 'U'] ['l' 'L'])
        | (['u' 'U'] ['l' 'L'] ['l' 'L'])
        | (['l' 'L'] ['l' 'L'])
        )?
    ) as x { TInt (x, tokinfo lexbuf) }


  | (real ['f' 'F']) as x { TFloat ((x, CFloat),      tokinfo lexbuf) }
  | (real ['l' 'L']) as x { TFloat ((x, CLongDouble), tokinfo lexbuf) }
  | (real as x)           { TFloat ((x, CDouble),     tokinfo lexbuf) }

  | ['0'] ['0'-'9']+  
      { pr2 ("LEXER: " ^ error_radix "octal" ^ tok lexbuf); 
        TCommentMisc (tokinfo lexbuf)
      }
  | ("0x" |"0X") ['0'-'9' 'a'-'z' 'A'-'Z']+ 
      { pr2 ("LEXER: " ^ error_radix "hexa" ^ tok lexbuf);
        TCommentMisc (tokinfo lexbuf)
      }


 (* !!! to put after other rules !!! otherwise 0xff
  * will be parsed as an ident.
  *)
  | ['0'-'9']+ letter (letter | digit) *  
      { pr2 ("LEXER: ZARB integer_string, certainly a macro:" ^ tok lexbuf);
        TIdent (tok lexbuf, tokinfo lexbuf)
      } 

(* gccext: http://gcc.gnu.org/onlinedocs/gcc/Binary-constants.html *)
(*
 | "0b" ['0'-'1'] { TInt (((tok lexbuf)<!!>(??,??)) +> int_of_stringbits) } 
 | ['0'-'1']+'b' { TInt (((tok lexbuf)<!!>(0,-2)) +> int_of_stringbits) } 
*)


  (*------------------------------------------------------------------------ *)
  | eof { EOF (tokinfo lexbuf +> Ast_c.rewrap_str "") }

  | _ 
      { 
        if !Flag_parsing_c.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }



(*****************************************************************************)
and char = parse
  | (_ as x)                                    "'"  { String.make 1 x }
  (* todo?: as for octal, do exception  beyond radix exception ? *)
  | (("\\" (oct | oct oct | oct oct oct)) as x  "'") { x }  
  (* this rule must be after the one with octal, lex try first longest
   * and when \7  we want an octal, not an exn.
   *)
  | (("\\x" ((hex | hex hex))) as x        "'")      { x }
  | (("\\" (_ as v))           as x        "'")
	{ 
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()  
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ -> 
              pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x
	} 
  | _ 
      { pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf
      }



(*****************************************************************************)

(* todo? factorise code with char ? but not same ending token so hard. *)
and string  = parse
  | '"'                                       { "" }
  | (_ as x)                                  { string_of_char x^string lexbuf}
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string lexbuf }
  | ("\\" (_ as v)) as x  
       { 
         (match v with (* Machine specific ? *)
         | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()  
         | 'f' -> () | 'a' -> ()
	 | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
         | 'e' -> () (* linuxext: ? *)

         (* old: "x" -> 10 gccext ? todo ugly, I put a fake value *)

         (* cppext:  can have   \ for multiline in string too *)
         | '\n' -> () 
         | _ -> pr2 ("LEXER: unrecognised symbol in string:"^tok lexbuf);
	 );
          x ^ string lexbuf
       }

 (* Bug if add following code, cos match also the '"' that is needed
  * to finish the string, and so go until end of file.
  *)
 (*
  | [^ '\\']+ 
    { let cs = lexbuf +> tok +> list_of_string +> List.map Char.code in
      cs ++ string lexbuf  
    }
  *)



(*****************************************************************************)

(* less: allow only char-'*' ? *)
and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | _  
      { let s = tok lexbuf in
        pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment lexbuf
      }



(*****************************************************************************)

(* cpp recognize C comments, so when #define xx (yy) /* comment \n ... */
 * then he has already erased the /* comment. So:
 * - dont eat the start of the comment otherwiseafterwards we are in the middle
 *   of a comment and so will problably get a parse error somewhere.
 * - have to recognize comments in cpp_eat_until_nl.
 *)

and cpp_eat_until_nl = parse
  (* bugfix: *)
  | "/*"          
      { let s = tok lexbuf in 
        let s2 = comment lexbuf in 
        let s3 = cpp_eat_until_nl lexbuf in 
        s ^ s2 ^ s3  
      } 
  | '\\' "\n" { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }

  | "\n"      { tok lexbuf } 
  (* noteopti: 
   * update: need also deal with comments chars now 
   *)
  | [^ '\n' '\\'      '/' '*'  ]+ 
     { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf } 
  | eof { pr2 "LEXER: end of file in cpp_eat_until_nl"; ""}
  | _   { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }  
