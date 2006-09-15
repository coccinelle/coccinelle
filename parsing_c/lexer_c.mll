{
open Common open Commonop

open Parser_c
open Lexer_parser

open Ast_c (* to factorise tokens, OpAssign, ... *)

(*****************************************************************************)
(* 
 * todo?: stdC: multibyte character ??  
 * todo: certains cas cpp hardcodés peuvent peut etre geré maintenant via
 *  ma lalr(k) technique.
 *
 * subtil: ocamllex use side effect on lexbuf, so must take care. 
 * For instance must do   
 *  let info = tokinfo lexbuf in 
 *  TComment (info +> tok_add_s (comment lexbuf)) 
 * and not 
 *   TComment (tokinfo lexbuf +> tok_add_s (comment lexbuf)) 
 * because of the "wierd" order of evaluation of OCaml.
 *
 * Note: Can't use Lexer_parser._lexer_hint here to do different things,
 * because now we call the lexer to get all the token (tokens_all), and then
 * we parse. So we can't have the _lexer_hint info here. We can have it only in
 * parse_c. For the same reason, the typedef handling here is useless.
 *)

exception Lexical of string

let tok     lexbuf  = Lexing.lexeme lexbuf
let tokinfo lexbuf  = { 
    charpos = Lexing.lexeme_start lexbuf; 
    str     = tok lexbuf  
  }, Ast_c.emptyAnnot

let tok_add_s s (info,annot) = {info with str = info.str ^ s}, annot

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



(*****************************************************************************)
rule token = parse

(* ------------------------------------------------------------------------- *)
(* spacing/comments *)
(* ------------------------------------------------------------------------- *)

 | [' ' '\t' '\n' '\r' '\011' '\012' ]+  { TCommentSpace (tokinfo lexbuf) }
 | "/*" { let i = tokinfo lexbuf in TComment (i +> tok_add_s (comment lexbuf))}


(* ------------------------------------------------------------------------- *)
(* cpp part 1 *)
(* ------------------------------------------------------------------------- *)

 (* old:
  *   | '#'		{ endline lexbuf} // should be line, and not endline 
  *   and endline = parse  | '\n' 	{ token lexbuf}  
  *                        |	_	{ endline lexbuf} 
  *)

  (* C++ comment, allowed in gccext,  but normally they are deleted by cpp.
   * So need this here only when dont call cpp before/  *)
  | "//" [^'\r''\n' '\011']*    { TComment (tokinfo lexbuf) } 


  | "#pragma pack" [^'\n']* '\n'                { TCommentCpp (tokinfo lexbuf)}
  | "#pragma GCC set_debug_pwd " [^'\n']* '\n'  { TCommentCpp (tokinfo lexbuf)}
  | "#pragma alloc_text" [^'\n']* '\n'          { TCommentCpp (tokinfo lexbuf)}


  (* todo?:
   *  have found a # #else  in "newfile-2.6.c",  legal ?   and also a  #/* ... 
   *    => just "#" -> token {lexbuf} (that is ignore)
   *  y'a 1 #elif  sans rien  apres
   *  y'a 1 #error sans rien  apres
   *  y'a 2  mov dede, #xxx    qui genere du coup exn car entouré par des #if 0
   *  => make as for comment, call a comment_cpp that when #endif finish the
   *   comment and if other cpp stuff raise exn
   *  y'a =~10  #if(xxx)  ou le ( est collé direct
   *  y'a des include"" et include<
   *  y'a 1 ` (derriere un #ifndef linux)
   *)

  | "#" [' ' '\t']*  "define" [' ' '\t']+ (letter (letter |digit)*)           
  | "#" [' ' '\t']*  "define" [' ' '\t']+ (letter (letter |digit)*) '(' [^ ')']* ')' 
      { let info = tokinfo lexbuf in 
        TDefine (info +> tok_add_s (cpp_eat_until_nl lexbuf)) }

  | "#" [' ' '\t']* "undef" [' ' '\t']+ (letter (letter |digit)*) [' ' '\t' '\n']    
      { TCommentCpp (tokinfo lexbuf) }


  | "#" [' ' '\t']* "error"   [' ' '\t']+  [^'\n']* '\n' 
  | "#" [' ' '\t']* "warning" [' ' '\t']+ [^'\n']* '\n'                        
  | "#" [' ' '\t']* "abort"   [' ' '\t']+ [^'\n']* '\n'                         
      { TCommentCpp (tokinfo lexbuf)} 

  | "#" [' ' '\t']* "error"                                             
      { TCommentCpp (tokinfo lexbuf)} (* in drivers/char/tpqic02.c *)

  | "#" [' ' '\t']* "include" [' ' '\t']* '"' ([^ '"']+) '"'  
  | "#" [' ' '\t']* "include" [' ' '\t']* '<' [^ '>']+ '>'                           
      { TCommentCpp (tokinfo lexbuf) }


  | "#" [' ' '\t']* "if" [' ' '\t']* "0" 
      { let info = tokinfo lexbuf in 
        TCommentCpp (info +> tok_add_s (cpp_comment_if_0 lexbuf)) }

  (* can have some ifdef 0  hence the letter|digit even at beginning of word *)
  | "#" [' ' '\t']* "ifdef"  [' ' '\t']+ (letter|digit) ((letter |digit)*) [' ' '\t']* 
     { TCommentCpp (tokinfo lexbuf) }
  | "#" [' ' '\t']* "ifndef" [' ' '\t']+ (letter|digit) ((letter |digit)*) [' ' '\t']* 
     { TCommentCpp (tokinfo lexbuf) }
  | "#" [' ' '\t']* "endif"  [' ' '\t' '\n']                                   
      { TCommentCpp (tokinfo lexbuf) }
  | "#" [' ' '\t']* "if" [' ' '\t']+                                           
      { let info = tokinfo lexbuf in 
        TCommentCpp (info +> tok_add_s (cpp_eat_until_nl lexbuf)) }
  | "#" [' ' '\t']* "if" '('                
      { let info = tokinfo lexbuf in 
        TCommentCpp (info +> tok_add_s (cpp_eat_until_nl lexbuf)) }
  | "#" [' ' '\t']* "elif" [' ' '\t']+ [^'\n']+  '\n'                          
      { TCommentCpp (tokinfo lexbuf) }
  | "#" [' ' '\t']* "else" [' ' '\t' '\n']                                     
      { TCommentCpp (tokinfo lexbuf) }

  (* there is a file in 2.6 that have this *)
  | "##" [' ' '\t']* "else" [' ' '\t' '\n'] { TCommentCpp (tokinfo lexbuf) }

  | "#" [' ' '\t']* "ident" [' ' '\t']+                                     
      { TCommentCpp (tokinfo lexbuf) }

  | "#" [' ' '\t']* '\n'
      { TCommentCpp (tokinfo lexbuf) }

   (* special_for_no_exn: in atm/ambassador.c *)
  | "#include UCODE(" [^'\n']+  '\n'
      { TCommentCpp (tokinfo lexbuf) }

  | "\\" '\n'
      { TCommentSpace (tokinfo lexbuf) }
 
 (* ---------------- *)

 (* struct def component. todo? generalize via LALR(k) tech by using a
  *  "in_struct"  context_info.
  *)
  | "ACPI_COMMON_OBJ_INFO;"        { TCommentAttrOrMacro (tokinfo lexbuf) }

  | "CS_OWNER" [' ' '\t']+ "CS_THIS_MODULE" { TCommentAttrOrMacro (tokinfo lexbuf) }


 (* misc *)
  | "__MODULE_STRING" '(' [^ ')']* ')' { TCommentAttrOrMacro (tokinfo lexbuf) }

  | "ACPI_MODULE_NAME" [' ' '\t']* "(" [^'\n']+ '\n' { TCommentAttrOrMacro (tokinfo lexbuf) }


 (* common macro of device driver *)
 | "I2C_CLIENT_INSMOD;"   { TCommentAttrOrMacro (tokinfo lexbuf) }


 (* kernel macro: (less needed now that allow type in arguments, and that allow
  *  mix decl and statement inside func)
  *)

  | "static" [' ' '\t']+ ((['A'-'Z'] (['A'-'Z' '_'] | digit) *) as id)  [' ' '\t']* "(" [^'\n' ';']+ ';' [' ' '\t']* '\n' { 
        (match id with
        | "DECLARE_MUTEX"
        | "DECLARE_COMPLETION"
        | "DECLARE_RWSEM"

        | "DECLARE_WAIT_QUEUE_HEAD"
        | "DEFINE_SPINLOCK"

        | "DEVICE_ATTR"
        | "CLASS_DEVICE_ATTR"
        | "SENSOR_DEVICE_ATTR"

        | "LIST_HEAD"

        | "DECLARE_WORK"
        | "DECLARE_TASKLET"

        | "PORT_ATTR_RO"
        | "PORT_PMA_ATTR"
          -> ()
        | s when s =~ "^DECLARE_.*" -> ()
        | s when s =~ ".*_ATTR$" -> ()
        | s when s =~ "^DEFINE_.*" -> ()

        | _ -> pr2 ("PassingMacro: " ^ id)
        ); 
        TCommentAttrOrMacro (tokinfo lexbuf) 
     }
   (* cos sometimes this one is on multiple line, so just transform it into an ident and then it looks like a funcall *)
   | "static" [' ' '\t']+ "DEVICE_ATTR" { TIdent (tok lexbuf, tokinfo lexbuf) }


  | "EXPORT_NO_SYMBOLS;" { TCommentAttrOrMacro (tokinfo lexbuf) }

  (* normally can be handled,  but often the module_exist does not have a trailing ;  :(  *)
  | "module_exit(" letter (letter | digit)* ")"  { TCommentAttrOrMacro (tokinfo lexbuf) }
  | "module_init(" letter (letter | digit)* ")"  { TCommentAttrOrMacro (tokinfo lexbuf) }

(*
"DECLARE_TASKLET" 
*)

  | "DECLARE_WAITQUEUE" [' ' '\t']* "(" [^'\n' ]+  '\n'       { TCommentAttrOrMacro (tokinfo lexbuf) }
  | "DECLARE_COMPLETION" [' ' '\t']* "(" [^'\n']+ '\n'        { TCommentAttrOrMacro (tokinfo lexbuf) }
  | "DECLARE_WAIT_QUEUE_HEAD" [' ' '\t']* "(" [^'\n' ]+  '\n' { TCommentAttrOrMacro (tokinfo lexbuf) }
  | "DECLARE_COMPLETION" [' ' '\t']* "(" [^'\n']+ '\n'        { TCommentAttrOrMacro (tokinfo lexbuf) }


(* ------------------------------------------------------------------------- *)
(* C symbols *)
(* ------------------------------------------------------------------------- *)
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
  | "^=" {TAssign(OpAssign Xor, (tokinfo lexbuf))} 
  | "<<=" {TAssign (OpAssign DecLeft, (tokinfo lexbuf)) } 
  | ">>=" {TAssign (OpAssign DecRight, (tokinfo lexbuf))}

  | "==" { TEqEq(tokinfo lexbuf) }   | "!=" { TNotEq(tokinfo lexbuf) } 
  | ">=" { TInfEq(tokinfo lexbuf) } | "<=" { TSupEq(tokinfo lexbuf) } 
  | "<" { TInf(tokinfo lexbuf) } | ">" {TSup(tokinfo lexbuf) }

  | "&&" { TAndLog(tokinfo lexbuf) } | "||" { TOrLog(tokinfo lexbuf) }
  | ">>" { TShr(tokinfo lexbuf) }    | "<<" { TShl(tokinfo lexbuf) }
  | "&"  { TAnd(tokinfo lexbuf) }    | "|" { TOr(tokinfo lexbuf) } 
  | "^" { TXor(tokinfo lexbuf) }
  | "..." { TEllipsis(tokinfo lexbuf) }
  | "->"   { TPtrOp(tokinfo lexbuf) }  | '.'  { TDot(tokinfo lexbuf) }  
  | ','    { TComma(tokinfo lexbuf) }  
  | ";"    { TPtVirg(tokinfo lexbuf) }
  | "?"    { TWhy(tokinfo lexbuf) }    | ":"   { TDotDot(tokinfo lexbuf) } 
  | "!"    { TBang(tokinfo lexbuf) }   | "~"   { TTilde(tokinfo lexbuf) }

  | "<:" { TOCro(tokinfo lexbuf) } | ":>" { TCCro(tokinfo lexbuf) } 
  | "<%" { TOBrace(tokinfo lexbuf) } | "%>" { TCBrace(tokinfo lexbuf) }
 

(* ------------------------------------------------------------------------- *)
(* C keywords and ident *)
(* ------------------------------------------------------------------------- *)

  | letter (letter | digit) * 
   (* StdC: must handle at least name of length > 509, but can truncate to 31 
      when compare and truncate 
      to 6 and even lowerise in the external linkage phase 
    *)
      { 
        let info = tokinfo lexbuf in
        match tok lexbuf with
	 | "char"   -> Tchar info    | "short" -> Tshort info  | "int"  -> Tint info  
	 | "double" -> Tdouble info  | "float" -> Tfloat info  | "long" -> Tlong info 
	 | "void"   -> Tvoid info 

	 | "unsigned" -> Tunsigned info  | "signed" -> Tsigned info
	       
	 | "auto"  -> Tauto info    | "register" -> Tregister info  
         | "extern" -> Textern info | "static" -> Tstatic info
	 | "const" -> Tconst info   | "volatile" -> Tvolatile info 

	 | "struct" -> Tstruct info  | "union" -> Tunion info 
         | "enum" -> Tenum info  
         | "typedef" -> Ttypedef info  
 
         | "if"   -> Tif info       | "else" -> Telse info 
	 | "break"    -> Tbreak info  | "continue" -> Tcontinue info
         | "switch" -> Tswitch info  | "case" -> Tcase info  
         | "default" -> Tdefault info 
	 | "for"  -> Tfor info  | "do"     -> Tdo info      
	 | "while"    -> Twhile info  
	 | "return"   -> Treturn info    |"goto" -> Tgoto info 
	 | "sizeof"   -> Tsizeof info   



       (* gccext: *)
       | "asm" -> Tasm info
       | "__asm__" -> Tasm info
       (* TODO  typeof, __typeof__  *)

       (* gccext: *)
       | "__attribute__" -> Tattribute info


(* ------------------------------------------------------------------------- *)
(* cpp part 2 *)
(* ------------------------------------------------------------------------- *)
 (* typedef *)
(* still needed ? my LALR(k) tech is not enough ? 
  | "u32" -> Tint (tokinfo lexbuf) 
  | "u16" -> Tint (tokinfo lexbuf) 
*)

 (* struct def component *)
  | "ACPI_STATE_COMMON"            -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "ACPI_PARSE_COMMON"            -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "ACPI_COMMON_DEBUG_MEM_HEADER" -> TCommentAttrOrMacro (tokinfo lexbuf) 

 (* attributes. could perhaps generalize via "__.*" *)
  | "__init"                       -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__exit"                       -> TCommentAttrOrMacro (tokinfo lexbuf) 

  | "__user"                       -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__iomem"                      -> TCommentAttrOrMacro (tokinfo lexbuf) 

  | "__initdata"                   -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__cacheline_aligned"          -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "____cacheline_aligned"          -> TCommentAttrOrMacro (tokinfo lexbuf) 



  | "__devinit"                    -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__devexit"                    -> TCommentAttrOrMacro (tokinfo lexbuf) 

  | "__devinitdata"                -> TCommentAttrOrMacro (tokinfo lexbuf) 

  | "__exitdata"                -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__cacheline_aligned_in_smp" -> TCommentAttrOrMacro (tokinfo lexbuf) 


  | "__ALIGNED__"                       -> TCommentAttrOrMacro (tokinfo lexbuf)  

  | "inline"                       -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__inline__"                   -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__volatile__"                 -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "__volatile"                 -> TCommentAttrOrMacro (tokinfo lexbuf) 
  | "asmlinkage"                   -> TCommentAttrOrMacro (tokinfo lexbuf) 

  | "INLINE"                       -> TCommentAttrOrMacro (tokinfo lexbuf)  
  | "_INLINE_"                       -> TCommentAttrOrMacro (tokinfo lexbuf)  


  | "STATIC"                       -> TCommentAttrOrMacro (tokinfo lexbuf)  


  | " __pmac"  -> TCommentAttrOrMacro (tokinfo lexbuf)  

 (* foreach like macro. here too maybe could generalize via ".*for_each.*" *)
 (* and have an heuristic with my lalr(k) technique *)
  | "list_for_each"                -> Twhile (tokinfo lexbuf) 
  | "list_for_each_safe"           -> Twhile (tokinfo lexbuf) 
  | "list_for_each_prev"           -> Twhile (tokinfo lexbuf) 
  | "list_for_each_entry"          -> Twhile (tokinfo lexbuf) 
  | "list_for_each_entry_safe"     -> Twhile (tokinfo lexbuf) 
  | "list_for_each_entry_continue" -> Twhile (tokinfo lexbuf) 
  | "list_for_each_entry_reverse"  -> Twhile (tokinfo lexbuf) 

  | "hlist_for_each_entry"         -> Twhile (tokinfo lexbuf) 
  | "hlist_for_each_entry_safe"    -> Twhile (tokinfo lexbuf) 

  | "list_for_each_rcu"    -> Twhile (tokinfo lexbuf) 
  | "list_for_each_continue_rcu" -> Twhile (tokinfo lexbuf) 
  | "for_each_cpu"                 -> Twhile (tokinfo lexbuf) 
  | "for_each_online_cpu"          -> Twhile (tokinfo lexbuf) 
  | "for_each_cpu_mask"            -> Twhile (tokinfo lexbuf) 
  | "for_each_process"             -> Twhile (tokinfo lexbuf) 
  | "gadget_for_each_ep"           -> Twhile (tokinfo lexbuf) 
  | "pci_for_each_dev"             -> Twhile (tokinfo lexbuf) 
  | "for_each_sbus"  -> Twhile (tokinfo lexbuf) 
  | "for_each_sbusdev" -> Twhile (tokinfo lexbuf) 
  | "for_each_rx" -> Twhile (tokinfo lexbuf) 
  | "for_each_esp" -> Twhile (tokinfo lexbuf) 
  | "for_each_online_fc_channel" -> Twhile (tokinfo lexbuf) 
  | "snd_pcm_group_for_each"       -> Twhile (tokinfo lexbuf) 
  | "bio_for_each_segment"         -> Twhile (tokinfo lexbuf) 
  | "__bio_for_each_segment"         -> Twhile (tokinfo lexbuf) 
  | "rq_for_each_bio" -> Twhile (tokinfo lexbuf) 

  | "for_all_sbusdev"             -> Twhile (tokinfo lexbuf) 
  | "FOR_EACH_QUEUED_ELEMENT" -> Twhile (tokinfo lexbuf) 
  | "ITERATE_RDEV"         -> Twhile (tokinfo lexbuf) 
  | "ITERATE_MDDEV"        -> Twhile (tokinfo lexbuf) 
  | "ITERATE_RDEV_PENDING" -> Twhile (tokinfo lexbuf) 
  | "ITERATE_RDEV_GENERIC" -> Twhile (tokinfo lexbuf) 

  | "for_each_online_node"             -> Twhile (tokinfo lexbuf) 

  (* higher order, debug like macro *)
  | "DBGINFO" -> THigherOrderMacro (tokinfo lexbuf) 
  | "DBGPX" -> THigherOrderMacro (tokinfo lexbuf) 
  | "DFLOW" -> THigherOrderMacro (tokinfo lexbuf) 
  (*  | "DBG" { THigherOrderMacro (tokinfo lexbuf) } *)
    (* old: | "DBG" [' ' '\t']* "(" '"' [^')' '"' ]+ '"' ')'       { TCommentAttrOrMacro (tokinfo lexbuf) } *)

  (* special higher order, debug like macro *)
  | "snd_magic_cast" -> THigherOrderExprExprStatement (tokinfo lexbuf) 
  | "IO_STATE" -> THigherOrderExprExprStatement (tokinfo lexbuf) 

  (* fait plus de dommage:  | "wait_event_lock_irq" { THigherOrderExprExprExprStatement (tokinfo lexbuf) } *)
  (* ASSERT,  same,  pas forcement une macro *)

  | "snd_assert"     -> THigherOrderExprStatement (tokinfo lexbuf) 
  | "snd_runtime_check"     -> THigherOrderExprStatement (tokinfo lexbuf) 

  (* control-flow extended macro *)
  | "TRACE_EXIT" -> Treturn (tokinfo lexbuf) 

  (* misc macro *)
  (* not needed anymore cos have extended  grammar *)

  (* string macro. normally handle quite well by hack_lexed, but sometimes
   * not enough, if have for instance the XX YY case, so at least add this
   * special case, so no more a XX YY but now a good "XX" YY *)
  | "KERN_INFO"   -> TString ((tok lexbuf, IsChar), tokinfo lexbuf) 
  | "KERN_ERR"    -> TString ((tok lexbuf, IsChar), tokinfo lexbuf) 
  | "KERN_CRIT"   -> TString ((tok lexbuf, IsChar), tokinfo lexbuf) 
  | "KERN_DEBUG"  -> TString ((tok lexbuf, IsChar), tokinfo lexbuf) 


  | s -> 
      ((if (s<!>0) = '_' then 
        ()(* warning "_ is often reserved for internal use by the compiler and libc\n" () *)
       );
        (* parse_typedef_fix*)
        (* note: now this is no more useful, cos as we use tokens_all, it first parse all as an ident 
           and later transform an indent in a typedef. so this job is now done in parse_c.ml
         *)
	if Lexer_parser.is_typedef s 
        then TypedefIdent (s, info) (*pr2 ("TYPEDEF:" ^ s);*) 
        else TIdent (s, info)       (*pr2 ("IDENT:" ^ s);*)  
       )
      }	

(* ------------------------------------------------------------------------- *)
(* C constant *)
(* ------------------------------------------------------------------------- *)

  | "'"     { let info = tokinfo lexbuf in let s = char lexbuf   in TChar     ((s,   IsChar),  (info +> tok_add_s (s ^ "'"))) }
  | '"'     { let info = tokinfo lexbuf in let s = string lexbuf in TString   ((s,   IsChar),  (info +> tok_add_s (s ^ "\""))) }
  (* wide character encoding, TODO L'toto' valid ? what is allowed ? *)
  | 'L' "'" { let info = tokinfo lexbuf in let s = char lexbuf   in TChar     ((s,   IsWchar),  (info +> tok_add_s (s ^ "'"))) } 
  | 'L' '"' { let info = tokinfo lexbuf in let s = string lexbuf in TString   ((s,   IsWchar),  (info +> tok_add_s (s ^ "\""))) }


  (* take care of the order  ? no cos lex try the longest match 
     the strange diff between decimal and octal constant semantic is not understood too by refman :) 
     refman:11.1.4, and ritchie
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
    { raise (Lexical "numeric octal constant contains digits beyond the radix") }
  | ("0x" |"0X") ['0'-'9' 'a'-'z' 'A'-'Z']+ 
      { 
        (* special_for_no_exn: *)
        pr2 "numeric hexa constant contains digits beyond the radix";
        TCommentAttrOrMacro (tokinfo lexbuf)
        (* raise (Lexical "numeric hexa constant contains digits beyond the radix") *)
      }


 (* special, !!! to put after other rules such  !! otherwise 0xff will be parsed as an ident  *)
  | ['0'-'9']+ letter (letter | digit) *  { 
       let info = tokinfo lexbuf in
       pr2 ("ZARB integer_string, certainly a macro:" ^ (fst info).str);
       TIdent (tok lexbuf, info)
     } 

(*  | ['0'-'1']+'b'      { TInt (((tok lexbuf)<!!>(0,-2)) +> int_of_stringbits) } *)


(* ------------------------------------------------------------------------- *)
  | eof { let (w,an) = tokinfo lexbuf in EOF ({w with Common.str = "EOF"},an) }

  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }



(*****************************************************************************)
and char = parse
  | (_ as x)                                    "'"  { String.make 1 x }
  | (("\\" (oct | oct oct | oct oct oct)) as x  "'") { x }     (* TODO, as for octal, do exception  beyond radix exception ? *)
  (* this rule must be after the one with octal, lex try first longest and when \7  we want an octal, not an exn *)
  | (("\\x" ((hex | hex hex))) as x        "'") { x }
  | (("\\" (_ as v)) as x                       "'")
	{ (match v with (* Machine specific ? *)
            | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()  | 'f' -> () | 'a' -> ()
	    | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
            | 'e' -> () (* linuxext: ? *)
	    | _ -> raise (Lexical ("unrecognised symbol:"^tok lexbuf))
	    );
          x
	} 
  (* toreput?: trigraph, cf less/ *)
  | _ { raise (Lexical ("unrecognised symbol:"^tok lexbuf)) }




(*****************************************************************************)
(* TODO factorise code with char *)
and string  = parse
  | '"'                                       { "" }
  | (_ as x)                                  { string_of_char x ^ string lexbuf }
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string lexbuf }
  | ("\\" (_ as v)) as x  
       { 
         (match v with (* Machine specific ? *)
         | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()  | 'f' -> () | 'a' -> ()
	 | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
         (*| "x" -> 10 (* gccext ? TODO ugly, I put a fake value *)*)
         | 'e' -> () (* linuxext: ? *)
         (* cppext:  can have   \ for multiline in string too *)
         | '\n' -> () 
         | _ -> raise (Lexical ("unrecognised symbol:"^tok lexbuf))
	 );
          x ^ string lexbuf
       }
 (* toreput?: trigraph, cf less/ *)
 (* bug if add that, cos match also the '"' that is needed to finish the string, and so go until end of file
  | [^ '\\']+ { let cs = lexbuf +> tok +> list_of_string +> List.map Char.code in  cs ++ string lexbuf  }
 *)
  | _ { raise (Lexical ("unrecognised symbol:"^tok lexbuf)) }




(*****************************************************************************)
(* todo?: allow only char-'*' *)
and comment = parse
  | "*/"     { tok lexbuf   }
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf (* noteopti: *) }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | _        { raise (Lexical ("unrecognised symbol:"^tok lexbuf)) }



(*****************************************************************************)
(*
 * cpp recognize C comments, so when   #define xx (yy) /* comment \n ... */
 * then he has already erased the /* comment.
 * => dont eat the start of the comment and then get afterwards in the middle
 *  of a comment (and so a parse error).
 * => have to recognize comments in cpp_eat_until_nl.
*)


and cpp_eat_until_nl = parse
  | "/*"                          { let s = tok lexbuf in let s2 = comment lexbuf in let s3 = cpp_eat_until_nl lexbuf in s ^ s2 ^ s3  } (* fix *)
  | "\n"                          { tok lexbuf } 
  | '\\' "\n"                     { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }
  | [^ '\n' '\\'      '/' '*'  ]+ { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf } (* need fix too *)
  | _                             { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }  



and cpp_comment_if_0 = parse
  | "#" [' ' '\t']* "else"     { tok lexbuf   }
  | "#" [' ' '\t']* "endif"    { tok lexbuf }

 (* introduced cos sometimes there is some ifdef in comments inside stuff
  * inside a #if 0 
  *)
  | "//" [^'\r''\n' '\011']*   { let s = tok lexbuf in s ^ cpp_comment_if_0 lexbuf }

 (* todo: could do a recursive call ? to allow ifdef inside if 0 *)
  | "#" [' ' '\t']* "ifdef"      
  | "#" [' ' '\t']* "ifndef"     
  | "#" [' ' '\t']* "if"         
  | "#" [' ' '\t']* "elif"       { 
    pr2 "GRAVE: a cpp instruction inside a #if 0";
    pr2 "GRAVE: j'arrete la, tant pis pour toi";
    tok lexbuf 
     (* raise (Lexical "GRAVE: a cpp instruction inside a #if 0")  *)
     }

 (* if you introduce new rule, dont forget to add the first symbol here,
  * otherwise the preceding rule will never fire, hidden by this gigantic
  * [^'#' '/']+
  *)
  | [^ '#' '/']+ { let s = tok lexbuf in s ^ cpp_comment_if_0 lexbuf (* noteopti: *) }
  | [ '#']   { let s = tok lexbuf in s ^ cpp_comment_if_0 lexbuf }
  | [ '/']   { let s = tok lexbuf in s ^ cpp_comment_if_0 lexbuf }
  | _        { raise (Lexical ("unrecognised symbol:"^tok lexbuf)) }
