{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme

let line = ref 1
let logical_line = ref 0

(* ---------------------------------------------------------------------- *)
(* control codes *)

(* Defined in data.ml
type line_type = MINUS | OPTMINUS | UNIQUEMINUS | PLUS | CONTEXT | UNIQUE | OPT
*)

let in_atat = ref false

let current_line_type = ref (D.CONTEXT,!line,!logical_line)
let get_current_line_type lexbuf =
  let (c,l,ll) = !current_line_type in (c,l,ll,Lexing.lexeme_start lexbuf)
let current_line_started = ref false

let reset_line _ =
  line := !line + 1;
  current_line_type := (D.CONTEXT,!line,!logical_line);
  current_line_started := false

let started_line = ref (-1)

let start_line seen_char =
  current_line_started := true;
  if seen_char && not(!line = !started_line)
  then
    begin
      started_line := !line;
      logical_line := !logical_line + 1
    end

let add_current_line_type x =
  match (x,!current_line_type) with
    (D.MINUS,(D.CONTEXT,ln,lln))  ->
      current_line_type := (D.MINUS,ln,lln)
  | (D.MINUS,(D.UNIQUE,ln,lln))   ->
      current_line_type := (D.UNIQUEMINUS,ln,lln)
  | (D.MINUS,(D.OPT,ln,lln))      ->
      current_line_type := (D.OPTMINUS,ln,lln)
  | (D.MINUS,(D.MULTI,ln,lln))      ->
      current_line_type := (D.MULTIMINUS,ln,lln)
  | (D.PLUS,(D.CONTEXT,ln,lln))   ->
      current_line_type := (D.PLUS,ln,lln)
  | (D.UNIQUE,(D.CONTEXT,ln,lln)) ->
      current_line_type := (D.UNIQUE,ln,lln)
  | (D.OPT,(D.CONTEXT,ln,lln))    ->
      current_line_type := (D.OPT,ln,lln)
  | (D.MULTI,(D.CONTEXT,ln,lln))    ->
      current_line_type := (D.MULTI,ln,lln)
  | _ -> raise (Lexical "invalid control character combination")

let check_minus_context_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) -> raise (Lexical ("invalid in a + context:"^s))
  | _ -> ()

let check_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) -> ()
  | _ -> raise (Lexical ("invalid in a nonempty context:"^s))

let check_arity_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) | (D.PLUS,_,_) | (D.UNIQUE,_,_) | (D.OPT,_,_) -> ()
  | _ -> raise (Lexical ("invalid in a nonempty context:"^s))

(* ---------------------------------------------------------------------- *)
(* identifiers, including metavariables *)

let metavariables =
  (Hashtbl.create(100) :
     (string, D.line_type * int * int * int -> token) Hashtbl.t)

let type_names =
  (Hashtbl.create(100) :
     (string, D.line_type * int * int * int -> token) Hashtbl.t)

let id_tokens lexbuf =
  let s = tok lexbuf in
  let linetype = get_current_line_type lexbuf in
  let in_meta = !Data.in_meta in
  let in_iso = !Data.in_iso in
  match s with
    "identifier" when in_meta -> check_arity_context_linetype s; TIdentifier
  | "text" when in_meta ->       check_arity_context_linetype s; TText
  | "type" when in_meta ->       check_arity_context_linetype s; TType
  | "parameter" when in_meta ->  check_arity_context_linetype s; TParameter
  | "constant"  when in_meta ->  check_arity_context_linetype s; TConstant
  | "expression" when in_meta -> check_arity_context_linetype s; TExpression
  | "statement" when in_meta ->  check_arity_context_linetype s; TStatement
  | "function"  when in_meta ->  check_arity_context_linetype s; TFunction
  | "local" when in_meta ->      check_arity_context_linetype s; TLocal
  | "list" when in_meta ->       check_arity_context_linetype s; Tlist
  | "fresh" when in_meta ->      check_arity_context_linetype s; TFresh
  | "typedef" when in_meta ->    check_arity_context_linetype s; TTypedef
  | "pure" when in_meta && in_iso ->
      check_arity_context_linetype s; TPure
  | "error" when in_meta ->      check_arity_context_linetype s; TError
  | "words" when in_meta ->      check_context_linetype s; TWords

  | "char" ->       Tchar     linetype
  | "short" ->      Tshort    linetype
  | "int" ->        Tint      linetype
  | "double" ->     Tdouble   linetype
  | "float" ->      Tfloat    linetype
  | "long" ->       Tlong     linetype
  | "void" ->       Tvoid     linetype
  | "struct" ->     Tstruct   linetype
  | "union" ->      Tunion    linetype
  | "unsigned" ->   Tunsigned linetype
  | "signed" ->     Tsigned   linetype
	
  | "auto"  ->      Tauto     linetype
  | "register" ->   Tregister linetype
  | "extern" ->     Textern   linetype
  | "static" ->     Tstatic   linetype

  | "const" ->      Tconst    linetype
  | "volatile" ->   Tvolatile linetype

  | "if" ->         TIf       linetype
  | "else" ->       TElse     linetype
  | "while" ->      TWhile    linetype
  | "do" ->         TDo       linetype
  | "for" ->        TFor      linetype
  | "switch" ->     TSwitch   linetype
  | "case" ->       TCase     linetype
  | "default" ->    TDefault  linetype
  | "return" ->     TReturn   linetype
  | "break" ->      TBreak    linetype
  | "continue" ->   TContinue linetype

  | "sizeof" ->     TSizeof   linetype

  | "Expression"     -> TIsoExpression
  | "Statement"      -> TIsoStatement
  | "Declaration"    -> TIsoDeclaration

  | s ->
      (try (Hashtbl.find metavariables s) linetype
      with
	Not_found ->
	  (try (Hashtbl.find type_names s) linetype
	  with Not_found -> TIdent (s,linetype)))

let mkassign op lexbuf =
  TAssign (Ast.OpAssign op, (get_current_line_type lexbuf))

let init _ =
  line := 1;
  logical_line := 0;
  in_atat := false;
  Data.clear_meta := (function _ -> Hashtbl.clear metavariables);
  Data.add_id_meta :=
    (function name -> function pure ->
      let fn clt = TMetaId(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_text_meta :=
    (function name -> function pure ->
      let fn clt = TMetaText(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_type_meta :=
    (function name -> function pure ->
      let fn clt = TMetaType(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_param_meta :=
    (function name -> function pure ->
      let fn clt = TMetaParam(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_paramlist_meta :=
    (function name -> function pure ->
      let fn clt = TMetaParamList(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_const_meta :=
    (function tyopt -> function name -> function pure -> 
      let fn clt = TMetaConst(name,pure,tyopt,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_err_meta :=
    (function name -> function pure ->
      let fn clt = TMetaErr(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_exp_meta :=
    (function tyopt -> function name -> function pure ->
      let fn clt = TMetaExp(name,pure,tyopt,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_explist_meta :=
    (function name -> function pure ->
      let fn clt = TMetaExpList(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_stm_meta :=
    (function name -> function pure ->
      let fn clt = TMetaStm(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_stmlist_meta :=
    (function name -> function pure ->
      let fn clt = TMetaStmList(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_func_meta :=
    (function name -> function pure ->
      let fn clt = TMetaFunc(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_local_func_meta :=
    (function name -> function pure ->
      let fn clt = TMetaLocalFunc(name,pure,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_type_name :=
    (function name ->
      let fn clt = TTypeId(name,clt) in
      Hashtbl.replace type_names name fn)

let drop_spaces s =
  let len = String.length s in
  let rec loop n =
    if n = len
    then n
    else
      if List.mem (String.get s n) [' ';'\t']
      then loop (n+1)
      else n in
  let start = loop 0 in
  String.sub s start (len - start)
}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

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


rule token = parse
  | [' ' '\t'  ]+             { start_line false; token lexbuf }
  | ['\n' '\r' '\011' '\012'] { reset_line(); token lexbuf }

  | "//" [^ '\n']* { start_line false; token lexbuf }

  | "@@" { start_line true; in_atat := not(!in_atat); TArobArob }

  | "WHEN" | "when"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen (get_current_line_type lexbuf) }

  | "..."
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis (get_current_line_type lexbuf) }

  | "ooo"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TCircles (get_current_line_type lexbuf) }

  | "***"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TStars (get_current_line_type lexbuf) }

  | "<..." { start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis (get_current_line_type lexbuf) }
  | "...>" { start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis (get_current_line_type lexbuf) }

  | "<ooo" { start_line true; check_context_linetype (tok lexbuf);
	     TOCircles (get_current_line_type lexbuf) }
  | "ooo>" { start_line true; check_context_linetype (tok lexbuf);
	     TCCircles (get_current_line_type lexbuf) }

  | "<***" { start_line true; check_context_linetype (tok lexbuf);
	     TOStars (get_current_line_type lexbuf) }
  | "***>" { start_line true; check_context_linetype (tok lexbuf);
	     TCStars (get_current_line_type lexbuf) }

  | "-" { if !current_line_started
	  then (start_line true; TMinus (get_current_line_type lexbuf))
          else (add_current_line_type D.MINUS; token lexbuf) }
  | "+" { if !current_line_started
	  then (start_line true; TPlus (get_current_line_type lexbuf))
          else if !in_atat
	  then TPlus0
          else (add_current_line_type D.PLUS; token lexbuf) }
  | "\\+" { if !current_line_started
	  then failwith "Illegal use of \\+"
          else if !in_atat
	  then TPlus0
          else (add_current_line_type D.MULTI; token lexbuf) }
  | "?" { if !current_line_started
	  then (start_line true; TWhy (get_current_line_type lexbuf))
          else if !in_atat
	  then TWhy0
          else (add_current_line_type D.OPT; token lexbuf) }
  | "!" { if !current_line_started
	  then (start_line true; TBang (get_current_line_type lexbuf))
          else if !in_atat
	  then TBang0
          else (add_current_line_type D.UNIQUE; token lexbuf) }
  | "(" { if !current_line_started
	  then (start_line true; TOPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 (get_current_line_type lexbuf))}
  | "\\(" { start_line true; TOPar0 (get_current_line_type lexbuf) }
  | "|" { if !current_line_started
	  then (start_line true; TOr (get_current_line_type lexbuf))
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 (get_current_line_type lexbuf))}
  | "\\|" { start_line true; TMid0 (get_current_line_type lexbuf) }
  | ")" { if !current_line_started
	  then (start_line true; TCPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 (get_current_line_type lexbuf))}
  | "\\)" { start_line true; TCPar0 (get_current_line_type lexbuf) }

  | '[' { start_line true; TOCro (get_current_line_type lexbuf) }
  | ']' { start_line true; TCCro (get_current_line_type lexbuf) }
  | '{' { start_line true; TOBrace (get_current_line_type lexbuf) }
  | '}' { start_line true; TCBrace (get_current_line_type lexbuf) }

  | "->"           { start_line true; TPtrOp (get_current_line_type lexbuf) }
  | '.'            { start_line true; TDot (get_current_line_type lexbuf) }
  | ','            { start_line true; TComma (get_current_line_type lexbuf) }
  | ";"            { start_line true; TPtVirg (get_current_line_type lexbuf) }

  
  | '*'            { start_line true;  TMul (get_current_line_type lexbuf) }     
  | '/'            { start_line true;  TDiv (get_current_line_type lexbuf) } 
  | '%'            { start_line true;  TMod (get_current_line_type lexbuf) } 
  
  | "++"           { start_line true;  TInc (get_current_line_type lexbuf) }    
  | "--"           { start_line true;  TDec (get_current_line_type lexbuf) }
  
  | "="            { start_line true; TEq (get_current_line_type lexbuf) } 
  
  | "-="           { start_line true; mkassign Ast.Minus lexbuf }
  | "+="           { start_line true; mkassign Ast.Plus lexbuf }
  
  | "*="           { start_line true; mkassign Ast.Mul lexbuf }
  | "/="           { start_line true; mkassign Ast.Div lexbuf }
  | "%="           { start_line true; mkassign Ast.Mod lexbuf }
  
  | "&="           { start_line true; mkassign Ast.And lexbuf }
  | "|="           { start_line true; mkassign Ast.Or lexbuf }
  | "^="           { start_line true; mkassign Ast.Xor lexbuf }
  
  | "<<="          { start_line true; mkassign Ast.DecLeft lexbuf }
  | ">>="          { start_line true; mkassign Ast.DecRight lexbuf }

  | ":"            { start_line true; TDotDot (get_current_line_type lexbuf) }
  
  | "=="           { start_line true; TEqEq   (get_current_line_type lexbuf) }   
  | "!="           { start_line true; TNotEq  (get_current_line_type lexbuf) } 
  | ">="           { start_line true; TInfEq  (get_current_line_type lexbuf) } 
  | "<="           { start_line true; TSupEq  (get_current_line_type lexbuf) } 
  | "<"            { start_line true; TInf    (get_current_line_type lexbuf) } 
  | ">"            { start_line true; TSup    (get_current_line_type lexbuf) }
  
  | "&&"           { start_line true; TAndLog (get_current_line_type lexbuf) } 
  | "||"           { start_line true; TOrLog  (get_current_line_type lexbuf) }
  
  | ">>"           { start_line true; TShr    (get_current_line_type lexbuf) }
  | "<<"           { start_line true; TShl    (get_current_line_type lexbuf) }
  
  | "&"            { start_line true; TAnd    (get_current_line_type lexbuf) }
  | "^"            { start_line true; TXor    (get_current_line_type lexbuf) }

  | "#" [' ' '\t']* "define"
      { start_line true; TDefine (get_current_line_type lexbuf) }
  | "#" [' ' '\t']* "include" [' ' '\t']* '"' [^ '"']+ '"'
      { TInclude
	  (let str = tok lexbuf in
	  let start = String.index str '"' in
	  let finish = String.rindex str '"' in
	  start_line true;
	  (String.sub str start (finish - start + 1),
	   (get_current_line_type lexbuf))) }
  | "#" [' ' '\t']* "include" [' ' '\t']* '<' [^ '>']+ '>'
      { TInclude
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (String.sub str start (finish - start + 1),
	   (get_current_line_type lexbuf)))}
  | "---" [^'\n']*
      { (if !current_line_started
      then failwith "--- must be at the beginning of the line");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) }
  | "+++" [^'\n']*
      { (if !current_line_started
      then failwith "--- must be at the beginning of the line");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) }

  | letter (letter | digit)*
      { start_line true; id_tokens lexbuf } 

  | "'" { start_line true;
	  TChar(char lexbuf,(get_current_line_type lexbuf)) }
  | '"' { start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) }
  | (real as x)    { start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) }
  | ((( decimal | hexa | octal) 
      ( ['u' 'U'] 
      | ['l' 'L']  
      | (['l' 'L'] ['u' 'U'])
      | (['u' 'U'] ['l' 'L'])
      | (['u' 'U'] ['l' 'L'] ['l' 'L'])
      | (['l' 'L'] ['l' 'L'])
      )?
    ) as x) { start_line true; TInt(x,(get_current_line_type lexbuf)) }

  | "<=>"          { TIso }
  | "=>"           { TRightIso }

  | eof            { EOF }

  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }


and char = parse
  | (_ as x) "'"                                     { String.make 1 x }
  | (("\\" (oct | oct oct | oct oct oct)) as x  "'") { x }
  | (("\\x" (hex | hex hex)) as x  "'")       { x }
  | (("\\" (_ as v)) as x "'")
	{ (match v with
            | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> ()
	    | 'r' -> ()  | 'f' -> () | 'a' -> ()
	    | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
            | 'e' -> ()
	    | _ -> raise (Lexical ("unrecognised symbol:"^tok lexbuf))
	    );
          x
	} 
  | _ { raise (Lexical ("unrecognised symbol:"^tok lexbuf)) }

and string  = parse
  | '"'                                       { "" }
  | (_ as x)                   { Common.string_of_char x ^ string lexbuf }
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string lexbuf }
  | ("\\" (_ as v)) as x  
       { 
         (match v with
         | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> () 
	 | 'f' -> () | 'a' -> ()
	 | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
         | 'e' -> ()
         | '\n' -> () 
         | _ -> raise (Lexical ("unrecognised symbol:"^tok lexbuf))
	 );
          x ^ string lexbuf
       }
  | _ { raise (Lexical ("unrecognised symbol:"^tok lexbuf)) }
