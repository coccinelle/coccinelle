{
open Parser_cocci
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
  (Hashtbl.create(100) : (string, D.line_type * int * int -> token) Hashtbl.t)

let id_tokens s =
  match s with
    "identifier" -> check_arity_context_linetype s; TIdentifier
  | "type" ->       check_arity_context_linetype s; TType
  | "parameter" ->  check_arity_context_linetype s; TParameter
  | "constant"  ->  check_arity_context_linetype s; TConstant
  | "expression" -> check_arity_context_linetype s; TExpression
  | "statement" ->  check_arity_context_linetype s; TStatement
  | "function"  ->  check_arity_context_linetype s; TFunction
  | "local" ->      check_arity_context_linetype s; TLocal
  | "list" ->       check_arity_context_linetype s; Tlist
  | "fresh" ->      check_arity_context_linetype s; TFresh
  | "error" ->      check_arity_context_linetype s; TError
  | "words" ->      check_context_linetype s; TWords

  | "char" ->       Tchar   !current_line_type
  | "short" ->      Tshort  !current_line_type
  | "int" ->        Tint    !current_line_type
  | "double" ->     Tdouble !current_line_type
  | "float" ->      Tfloat  !current_line_type
  | "long" ->       Tlong   !current_line_type
  | "void" ->       Tvoid   !current_line_type
  | "struct" ->     Tstruct !current_line_type
  | "union" ->      Tunion  !current_line_type
  | "unsigned" ->   Tunsigned !current_line_type
  | "signed" ->     Tsigned !current_line_type
	
  | "static" ->     Tstatic !current_line_type
  | "const" ->      Tconst !current_line_type
  | "volatile" ->   Tstatic !current_line_type

  | "if" ->         TIf     !current_line_type
  | "else" ->       TElse   !current_line_type
  | "while" ->      TWhile  !current_line_type
  | "do" ->         TDo     !current_line_type
  | "for" ->        TFor    !current_line_type
  | "return" ->     TReturn !current_line_type
  | s ->
      try (Hashtbl.find metavariables s) !current_line_type
      with Not_found -> TIdent (s,!current_line_type)

let mkassign op = TAssign (Ast.OpAssign op, !current_line_type)

let init _ =
  Data.add_id_meta :=
    (let fn name clt = TMetaId(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_type_meta :=
    (let fn name clt = TMetaType(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_param_meta :=
    (let fn name clt = TMetaParam(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_paramlist_meta :=
    (let fn name clt = TMetaParamList(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_const_meta :=
    (let fn tyopt name clt = TMetaConst(name,tyopt,clt) in
    (function tyopt -> function name -> 
      Hashtbl.replace metavariables name (fn tyopt name)));
  Data.add_err_meta :=
    (let fn name clt = TMetaErr(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_exp_meta :=
    (let fn tyopt name clt = TMetaExp(name,tyopt,clt) in
    (function tyopt -> function name ->
      Hashtbl.replace metavariables name (fn tyopt name)));
  Data.add_explist_meta :=
    (let fn name clt = TMetaExpList(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_stm_meta :=
    (let fn name clt = TMetaStm(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_stmlist_meta :=
    (let fn name clt = TMetaStmList(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_func_meta :=
    (let fn name clt = TMetaFunc(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)));
  Data.add_local_func_meta :=
    (let fn name clt = TMetaLocalFunc(name,clt) in
    (function name -> Hashtbl.replace metavariables name (fn name)))

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

  | "WHEN"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen !current_line_type }

  | "..."
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis !current_line_type }

  | "ooo"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TCircles !current_line_type }

  | "***"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TStars !current_line_type }

  | "<..." { start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis !current_line_type }
  | "...>" { start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis !current_line_type }

  | "<ooo" { start_line true; check_context_linetype (tok lexbuf);
	     TOCircles !current_line_type }
  | "ooo>" { start_line true; check_context_linetype (tok lexbuf);
	     TCCircles !current_line_type }

  | "<***" { start_line true; check_context_linetype (tok lexbuf);
	     TOStars !current_line_type }
  | "***>" { start_line true; check_context_linetype (tok lexbuf);
	     TCStars !current_line_type }

  | "-" { if !current_line_started
	  then TMinus !current_line_type
          else (add_current_line_type D.MINUS; token lexbuf) }
  | "+" { if !current_line_started
	  then TPlus !current_line_type
          else if !in_atat
	  then TPlus0
          else (add_current_line_type D.PLUS; token lexbuf) }
  | "\\+" { if !current_line_started
	  then failwith "Illegal use of \\+"
          else if !in_atat
	  then TPlus0
          else (add_current_line_type D.MULTI; token lexbuf) }
  | "?" { if !current_line_started
	  then TWhy !current_line_type
          else if !in_atat
	  then TWhy0
          else (add_current_line_type D.OPT; token lexbuf) }
  | "!" { if !current_line_started
	  then TBang !current_line_type
          else if !in_atat
	  then TBang0
          else (add_current_line_type D.UNIQUE; token lexbuf) }
  | "(" { if !current_line_started
	  then TOPar !current_line_type
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 !current_line_type)}
  | "|" { if !current_line_started
	  then TOr !current_line_type
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 !current_line_type)}
  | ")" { if !current_line_started
	  then TCPar !current_line_type
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 !current_line_type)}

  | '[' { start_line true; TOCro !current_line_type }
  | ']' { start_line true; TCCro !current_line_type }
  | '{' { start_line true; TOBrace !current_line_type }
  | '}' { start_line true; TCBrace !current_line_type }

  | "->"           { start_line true; TPtrOp !current_line_type }
  | '.'            { start_line true; TDot !current_line_type }
  | ','            { start_line true; TComma !current_line_type }
  | ";"            { start_line true; TPtVirg !current_line_type }

  
  | '*'            { start_line true;  TMul !current_line_type }     
  | '/'            { start_line true;  TDiv !current_line_type } 
  | '%'            { start_line true;  TMod !current_line_type } 
  
  | "++"           { start_line true;  TInc !current_line_type }    
  | "--"           { start_line true;  TDec !current_line_type }
  
  | "="            { start_line true; TEq !current_line_type } 
  
  | "-="           { start_line true; mkassign Ast.Minus }
  | "+="           { start_line true; mkassign Ast.Plus }
  
  | "*="           { start_line true; mkassign Ast.Mul }
  | "/="           { start_line true; mkassign Ast.Div }
  | "%="           { start_line true; mkassign Ast.Mod }
  
  | "&="           { start_line true; mkassign Ast.And }
  | "|="           { start_line true; mkassign Ast.Or }
  | "^="           { start_line true; mkassign Ast.Xor }
  
  | "<<="          { start_line true; mkassign Ast.DecLeft }
  | ">>="          { start_line true; mkassign Ast.DecRight }

  | ":"            { start_line true; TDotDot !current_line_type }
  
  | "=="           { start_line true; TEqEq   !current_line_type }   
  | "!="           { start_line true; TNotEq  !current_line_type } 
  | ">="           { start_line true; TInfEq  !current_line_type } 
  | "<="           { start_line true; TSupEq  !current_line_type } 
  | "<"            { start_line true; TInf    !current_line_type } 
  | ">"            { start_line true; TSup    !current_line_type }
  
  | "&&"           { start_line true; TAndLog !current_line_type } 
  | "||"           { start_line true; TOrLog  !current_line_type }
  
  | ">>"           { start_line true; TShr    !current_line_type }
  | "<<"           { start_line true; TShl    !current_line_type }
  
  | "&"            { start_line true; TAnd    !current_line_type }
  | "^"            { start_line true; TXor    !current_line_type }

  | "#" [' ' '\t']* "include" [' ' '\t']* '"' [^ '"']+ '"'
      { TInclude
	  (let str = tok lexbuf in
	  let start = String.index str '"' in
	  let finish = String.rindex str '"' in
	  start_line true;
	  (String.sub str start (finish - start + 1),!current_line_type)) }
  | "#" [' ' '\t']* "include" [' ' '\t']* '<' [^ '>']+ '>'
      { TInclude
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (String.sub str start (finish - start + 1),!current_line_type))}
  | "---" [^'\n']*
      { (if !current_line_started
      then failwith "--- must be at the beginning of the line");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   !current_line_type)) }
  | "+++" [^'\n']*
      { (if !current_line_started
      then failwith "--- must be at the beginning of the line");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   !current_line_type)) }

  | letter (letter | digit)*
      { start_line true; id_tokens (tok lexbuf) } 

  | "'"       { start_line true; TChar(char lexbuf,!current_line_type) }
  | '"'       { start_line true; TString(string lexbuf,!current_line_type) }
  | (real as x)    { start_line true; TFloat(x,!current_line_type) }
  | (decimal as x) { start_line true; TInt(x,!current_line_type) }


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
