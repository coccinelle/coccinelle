{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module P = Parse_aux
exception Lexical of string
let tok = Lexing.lexeme

let line = ref 1
let logical_line = ref 0

(* ---------------------------------------------------------------------- *)
(* control codes *)

(* Defined in data.ml
type line_type = MINUS | OPTMINUS | UNIQUEMINUS | PLUS | CONTEXT | UNIQUE | OPT
*)

let current_line_type = ref (D.CONTEXT,!line,!logical_line)

let prev_plus = ref false
let line_start = ref 0 (* offset of the beginning of the line *)
let get_current_line_type lexbuf =
  let (c,l,ll) = !current_line_type in
  let lex_start = Lexing.lexeme_start lexbuf in
  let preceeding_spaces =
    if !line_start < 0 then 0 else lex_start - !line_start in
  line_start := -1;
  prev_plus := (c = D.PLUS);
  (c,l,ll,lex_start,preceeding_spaces,[],[],Ast0.NoMetaPos)
let current_line_started = ref false
let col_zero = ref true

let reset_line lexbuf =
  line := !line + 1;
  current_line_type := (D.CONTEXT,!line,!logical_line);
  current_line_started := false;
  col_zero := true;
  line_start := Lexing.lexeme_start lexbuf + 1

let started_line = ref (-1)

let start_line seen_char =
  current_line_started := true;
  col_zero := false;
  (if seen_char && not(!line = !started_line)
  then
    begin
      started_line := !line;
      logical_line := !logical_line + 1
    end)

let pass_zero _ = col_zero := false

let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

let add_current_line_type x =
  match (x,!current_line_type) with
    (D.MINUS,(D.CONTEXT,ln,lln))  ->
      current_line_type := (D.MINUS,ln,lln)
  | (D.MINUS,(D.UNIQUE,ln,lln))   ->
      current_line_type := (D.UNIQUEMINUS,ln,lln)
  | (D.MINUS,(D.OPT,ln,lln))      ->
      current_line_type := (D.OPTMINUS,ln,lln)
  | (D.PLUS,(D.CONTEXT,ln,lln))   ->
      current_line_type := (D.PLUS,ln,lln)
  | (D.UNIQUE,(D.CONTEXT,ln,lln)) ->
      current_line_type := (D.UNIQUE,ln,lln)
  | (D.OPT,(D.CONTEXT,ln,lln))    ->
      current_line_type := (D.OPT,ln,lln)
  | _ -> lexerr "invalid control character combination" ""

let check_minus_context_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) -> lexerr "invalid in a + context: " s
  | _ -> ()

let check_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) -> ()
  | _ -> lexerr "invalid in a nonempty context: " s

let check_plus_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) -> ()
  | _ -> lexerr "invalid in a non + context: " s

let check_arity_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) | (D.PLUS,_,_) | (D.UNIQUE,_,_) | (D.OPT,_,_) -> ()
  | _ -> lexerr "invalid in a nonempty context: " s

let process_include start finish str =
  (match !current_line_type with
    (D.PLUS,_,_) ->
      (try
	let _ = Str.search_forward (Str.regexp "\\.\\.\\.") str start in
	lexerr "... not allowed in + include" ""
      with Not_found -> ())
  | _ -> ());
  String.sub str (start + 1) (finish - start - 1)

(* ---------------------------------------------------------------------- *)
type pm = PATCH | MATCH | UNKNOWN

let pm = ref UNKNOWN

let patch_or_match = function
    PATCH ->
      (match !pm with
	MATCH -> lexerr "- or + not allowed in the first column for a match" ""
      |	PATCH -> ()
      |	UNKNOWN -> Flag.sgrep_mode2 := false; pm := PATCH)
  | MATCH ->
      (match !pm with
	PATCH -> lexerr "* not allowed in the first column for a patch" ""
      |	MATCH -> ()
      |	UNKNOWN -> Flag.sgrep_mode2 := true; pm := MATCH)
  | _ -> failwith "unexpected argument"

(* ---------------------------------------------------------------------- *)
(* identifiers, including metavariables *)

let metavariables = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let all_metavariables =
  (Hashtbl.create(100) : (string,(string * (D.clt -> token)) list) Hashtbl.t)

let type_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let declarer_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let iterator_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let rule_names = (Hashtbl.create(100) : (string, unit) Hashtbl.t)

let check_var s linetype =
  let fail _ =
    if (!Data.in_prolog || !Data.in_rule_name) &&
      Str.string_match (Str.regexp "<.*>") s 0
    then TPathIsoFile s
    else
      try (Hashtbl.find metavariables s) linetype
      with Not_found ->
	(try (Hashtbl.find type_names s) linetype
	with Not_found ->
	  (try (Hashtbl.find declarer_names s) linetype
	  with Not_found ->
	    (try (Hashtbl.find iterator_names s) linetype
	    with Not_found -> TIdent (s,linetype)))) in
  if !Data.in_meta or !Data.in_rule_name
  then (try Hashtbl.find rule_names s; TRuleName s with Not_found -> fail())
  else fail()

let id_tokens lexbuf =
  let s = tok lexbuf in
  let linetype = get_current_line_type lexbuf in
  let in_rule_name = !Data.in_rule_name in
  let in_meta = !Data.in_meta in
  let in_iso = !Data.in_iso in
  let in_prolog = !Data.in_prolog in
  match s with
    "identifier" when in_meta -> check_arity_context_linetype s; TIdentifier
  | "type" when in_meta ->       check_arity_context_linetype s; TType
  | "parameter" when in_meta ->  check_arity_context_linetype s; TParameter
  | "constant"  when in_meta ->  check_arity_context_linetype s; TConstant
  | "generated" when in_rule_name && not (!Flag.make_hrule = None) ->
      check_arity_context_linetype s; TGenerated
  | "expression" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TExpression
  | "initialiser" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TInitialiser
  | "initializer" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TInitialiser
  | "idexpression" when in_meta ->
      check_arity_context_linetype s; TIdExpression
  | "statement" when in_meta ->  check_arity_context_linetype s; TStatement
  | "function"  when in_meta ->  check_arity_context_linetype s; TFunction
  | "local" when in_meta ->      check_arity_context_linetype s; TLocal
  | "list" when in_meta ->       check_arity_context_linetype s; Tlist
  | "fresh" when in_meta ->      check_arity_context_linetype s; TFresh
  | "typedef" when in_meta ->    check_arity_context_linetype s; TTypedef
  | "declarer" when in_meta ->   check_arity_context_linetype s; TDeclarer
  | "iterator" when in_meta ->   check_arity_context_linetype s; TIterator
  | "name" when in_meta ->       check_arity_context_linetype s; TName
  | "position" when in_meta ->   check_arity_context_linetype s; TPosition
  | "any" when in_meta ->        check_arity_context_linetype s; TPosAny
  | "pure" when in_meta && in_iso ->
      check_arity_context_linetype s; TPure
  | "context" when in_meta && in_iso ->
      check_arity_context_linetype s; TContext
  | "error" when in_meta ->      check_arity_context_linetype s; TError
  | "words" when in_meta ->      check_context_linetype s; TWords

  | "using" when in_rule_name || in_prolog ->  check_context_linetype s; TUsing
  | "disable" when in_rule_name ->  check_context_linetype s; TDisable
  | "extends" when in_rule_name -> check_context_linetype s; TExtends
  | "depends" when in_rule_name -> check_context_linetype s; TDepends
  | "on" when in_rule_name      -> check_context_linetype s; TOn
  | "ever" when in_rule_name    -> check_context_linetype s; TEver
  | "never" when in_rule_name   -> check_context_linetype s; TNever
  | "exists" when in_rule_name  -> check_context_linetype s; TExists
  | "forall" when in_rule_name  -> check_context_linetype s; TForall
  | "reverse" when in_rule_name -> check_context_linetype s; TReverse
  | "script" when in_rule_name -> check_context_linetype s; TScript

  | "char" ->       Tchar     linetype
  | "short" ->      Tshort    linetype
  | "int" ->        Tint      linetype
  | "double" ->     Tdouble   linetype
  | "float" ->      Tfloat    linetype
  | "long" ->       Tlong     linetype
  | "void" ->       Tvoid     linetype
  | "struct" ->     Tstruct   linetype
  | "union" ->      Tunion    linetype
  | "enum" ->       Tenum     linetype
  | "unsigned" ->   Tunsigned linetype
  | "signed" ->     Tsigned   linetype

  | "auto"  ->      Tauto     linetype
  | "register" ->   Tregister linetype
  | "extern" ->     Textern   linetype
  | "static" ->     Tstatic   linetype
  | "inline" ->     Tinline   linetype
  | "typedef" ->    Ttypedef  linetype

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
  | "goto" ->       TGoto     linetype

  | "sizeof" ->     TSizeof   linetype

  | "Expression"     -> TIsoExpression
  | "ArgExpression"  -> TIsoArgExpression
  | "TestExpression" -> TIsoTestExpression
  | "Statement"      -> TIsoStatement
  | "Declaration"    -> TIsoDeclaration
  | "Type"           -> TIsoType
  | "TopLevel"       -> TIsoTopLevel

  | s -> check_var s linetype

let mkassign op lexbuf =
  TAssign (Ast.OpAssign op, (get_current_line_type lexbuf))

let init _ =
  line := 1;
  logical_line := 0;
  prev_plus := false;
  line_start := 0;
  current_line_started := false;
  col_zero := true;
  pm := UNKNOWN;
  Data.in_rule_name := false;
  Data.in_meta := false;
  Data.in_prolog := false;
  Data.inheritable_positions := [];
  Hashtbl.clear all_metavariables;
  Hashtbl.clear Data.all_metadecls;
  Hashtbl.clear metavariables;
  Hashtbl.clear type_names;
  Hashtbl.clear rule_names;
  let get_name (_,x) = x in
  Data.add_id_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaId(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_type_meta :=
    (fun name pure ->
      let fn clt = TMetaType(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_init_meta :=
    (fun name pure ->
      let fn clt = TMetaInit(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_param_meta :=
    (function name -> function pure ->
      let fn clt = TMetaParam(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_paramlist_meta :=
    (function name -> function lenname -> function pure ->
      let fn clt = TMetaParamList(name,lenname,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_const_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaConst(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_err_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaErr(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_exp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_idexp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaIdExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_local_idexp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaLocalIdExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_explist_meta :=
    (function name -> function lenname -> function pure ->
      let fn clt = TMetaExpList(name,lenname,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_stm_meta :=
    (function name -> function pure ->
      let fn clt = TMetaStm(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_stmlist_meta :=
    (function name -> function pure ->
      let fn clt = TMetaStmList(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_func_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaFunc(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_local_func_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaLocalFunc(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_iterator_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaIterator(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_declarer_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaDeclarer(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_pos_meta :=
    (fun name constraints any ->
      let fn ((d,ln,_,_,_,_,_,_) as clt) =
	(if d = Data.PLUS
	then
	  failwith
	    (Printf.sprintf "%d: positions only allowed in minus code" ln));
	TMetaPos(name,constraints,any,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_type_name :=
    (function name ->
      let fn clt = TTypeId(name,clt) in
      Hashtbl.replace type_names name fn);
  Data.add_declarer_name :=
    (function name ->
      let fn clt = TDeclarerId(name,clt) in
      Hashtbl.replace declarer_names name fn);
  Data.add_iterator_name :=
    (function name ->
      let fn clt = TIteratorId(name,clt) in
      Hashtbl.replace iterator_names name fn);
  Data.init_rule := (function _ -> Hashtbl.clear metavariables);
  Data.install_bindings :=
    (function parent ->
      List.iter (function (name,fn) -> Hashtbl.add metavariables name fn)
	(Hashtbl.find all_metavariables parent))

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
  | ['\n' '\r' '\011' '\012'] { reset_line lexbuf; token lexbuf }

  | "//" [^ '\n']* { start_line false; token lexbuf }

  | "@@" { start_line true; TArobArob }
  | "@"  { pass_zero();
	   if !Data.in_rule_name or not !current_line_started
	   then (start_line true; TArob)
	   else (check_minus_context_linetype "@"; TPArob) }

  | "WHEN" | "when"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen (get_current_line_type lexbuf) }

  | "..."
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis (get_current_line_type lexbuf) }
(*
  | "ooo"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TCircles (get_current_line_type lexbuf) }

  | "***"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TStars (get_current_line_type lexbuf) }
*)
  | "<..." { start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis (get_current_line_type lexbuf) }
  | "...>" { start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis (get_current_line_type lexbuf) }
  | "<+..." { start_line true; check_context_linetype (tok lexbuf);
	     TPOEllipsis (get_current_line_type lexbuf) }
  | "...+>" { start_line true; check_context_linetype (tok lexbuf);
	     TPCEllipsis (get_current_line_type lexbuf) }
(*
  | "<ooo" { start_line true; check_context_linetype (tok lexbuf);
	     TOCircles (get_current_line_type lexbuf) }
  | "ooo>" { start_line true; check_context_linetype (tok lexbuf);
	     TCCircles (get_current_line_type lexbuf) }

  | "<***" { start_line true; check_context_linetype (tok lexbuf);
	     TOStars (get_current_line_type lexbuf) }
  | "***>" { start_line true; check_context_linetype (tok lexbuf);
	     TCStars (get_current_line_type lexbuf) }
*)
  | "-" { pass_zero();
	  if !current_line_started
	  then (start_line true; TMinus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.MINUS; token lexbuf) }
  | "+" { pass_zero();
	  if !current_line_started
	  then (start_line true; TPlus (get_current_line_type lexbuf))
          else if !Data.in_meta
	  then TPlus0
          else (patch_or_match PATCH;
		add_current_line_type D.PLUS; token lexbuf) }
  | "?" { pass_zero();
	  if !current_line_started
	  then (start_line true; TWhy (get_current_line_type lexbuf))
          else if !Data.in_meta
	  then TWhy0
          else (add_current_line_type D.OPT; token lexbuf) }
  | "!" { pass_zero();
	  if !current_line_started
	  then (start_line true; TBang (get_current_line_type lexbuf))
          else if !Data.in_meta
	  then TBang0
          else (add_current_line_type D.UNIQUE; token lexbuf) }
  | "(" { if not !col_zero
	  then (start_line true; TOPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 (get_current_line_type lexbuf))}
  | "\\(" { start_line true; TOPar0 (get_current_line_type lexbuf) }
  | "|" { if not (!col_zero)
	  then (start_line true; TOr(get_current_line_type lexbuf))
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 (get_current_line_type lexbuf))}
  | "\\|" { start_line true; TMid0 (get_current_line_type lexbuf) }
  | ")" { if not !col_zero
	  then (start_line true; TCPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 (get_current_line_type lexbuf))}
  | "\\)" { start_line true; TCPar0 (get_current_line_type lexbuf) }

  | '[' { start_line true; TOCro (get_current_line_type lexbuf)   }
  | ']' { start_line true; TCCro (get_current_line_type lexbuf)   }
  | '{' { start_line true; TOBrace (get_current_line_type lexbuf) }
  | '}' { start_line true; TCBrace (get_current_line_type lexbuf) }

  | "->"           { start_line true; TPtrOp (get_current_line_type lexbuf)  }
  | '.'            { start_line true; TDot (get_current_line_type lexbuf)    }
  | ','            { start_line true; TComma (get_current_line_type lexbuf)  }
  | ";"            { start_line true;
		     if !Data.in_meta
		     then TMPtVirg (* works better with tokens_all *)
		     else TPtVirg (get_current_line_type lexbuf) }


  | '*'            { pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS; token lexbuf) }
  | '/'            { start_line true;
		     TDmOp (Ast.Div,get_current_line_type lexbuf) }
  | '%'            { start_line true;
		     TDmOp (Ast.Mod,get_current_line_type lexbuf) }
  | '~'            { start_line true;  TTilde (get_current_line_type lexbuf) }

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
  | ">="           { start_line true;
		     TLogOp(Ast.SupEq,get_current_line_type lexbuf) }
  | "<="           { start_line true;
		     TLogOp(Ast.InfEq,get_current_line_type lexbuf) }
  | "<"            { start_line true;
		     TLogOp(Ast.Inf,get_current_line_type lexbuf) }
  | ">"            { start_line true;
		     TLogOp(Ast.Sup,get_current_line_type lexbuf) }

  | "&&"           { start_line true; TAndLog (get_current_line_type lexbuf) }
  | "||"           { start_line true; TOrLog  (get_current_line_type lexbuf) }

  | ">>"           { start_line true;
		     TShOp(Ast.DecRight,get_current_line_type lexbuf) }
  | "<<"           { start_line true;
		     TShOp(Ast.DecLeft,get_current_line_type lexbuf) }

  | "&"            { start_line true; TAnd    (get_current_line_type lexbuf) }
  | "^"            { start_line true; TXor(get_current_line_type lexbuf) }

  | ( ("#" [' ' '\t']*  "define" [' ' '\t']+))
    ( (letter (letter |digit)*) as ident)
      { start_line true;
	let (arity,line,lline,offset,col,strbef,straft,pos) as lt =
	  get_current_line_type lexbuf in
	let off = String.length "#define " in
	(* -1 in the code below because the ident is not at the line start *)
	TDefine
	  (lt,
	   check_var ident
	     (arity,line,lline,offset+off,(-1),[],[],Ast0.NoMetaPos)) }
  | ( ("#" [' ' '\t']*  "define" [' ' '\t']+))
    ( (letter (letter | digit)*) as ident)
    '('
      { start_line true;
	let (arity,line,lline,offset,col,strbef,straft,pos) as lt =
	  get_current_line_type lexbuf in
	let off = String.length "#define " in
	TDefineParam
        (lt,
	 check_var ident
	   (* why pos here but not above? *)
	   (arity,line,lline,offset+off,(-1),strbef,straft,pos),
	 offset + off + (String.length ident)) }
  | "#" [' ' '\t']* "include" [' ' '\t']* '"' [^ '"']+ '"'
      { TIncludeL
	  (let str = tok lexbuf in
	  let start = String.index str '"' in
	  let finish = String.rindex str '"' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) }
  | "#" [' ' '\t']* "include" [' ' '\t']* '<' [^ '>']+ '>'
      { TIncludeNL
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) }
  | "#" [' ' '\t']* "if" [^'\n']*
  | "#" [' ' '\t']* "ifdef" [^'\n']*
  | "#" [' ' '\t']* "ifndef" [^'\n']*
  | "#" [' ' '\t']* "else" [^'\n']*
  | "#" [' ' '\t']* "elif" [^'\n']*
  | "#" [' ' '\t']* "endif" [^'\n']*
  | "#" [' ' '\t']* "error" [^'\n']*
      { start_line true; check_plus_linetype (tok lexbuf);
	TPragma (tok lexbuf, get_current_line_type lexbuf) }
  | "/*"
      { start_line true; check_plus_linetype (tok lexbuf);
	(* second argument to TPragma is not quite right, because
	   it represents only the first token of the comemnt, but that
	   should be good enough *)
	TPragma ("/*"^(comment lexbuf), get_current_line_type lexbuf) }
  | "---" [^'\n']*
      { (if !current_line_started
      then lexerr "--- must be at the beginning of the line" "");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) }
  | "+++" [^'\n']*
      { (if !current_line_started
      then lexerr "+++ must be at the beginning of the line" "");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) }

  | letter (letter | digit)*
      { start_line true; id_tokens lexbuf }

  | "'" { start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) }
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

  | _ { lexerr "unrecognised symbol, in token rule: " (tok lexbuf) }


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
	    | _ -> lexerr "unrecognised symbol: " (tok lexbuf)
	    );
          x
	}
  | _ { lexerr "unrecognised symbol: " (tok lexbuf) }

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
         | _ -> lexerr "unrecognised symbol:" (tok lexbuf)
	 );
          x ^ string lexbuf
       }
  | _ { lexerr "unrecognised symbol: " (tok lexbuf) }

and comment = parse
  | "*/"     { start_line true; tok lexbuf }
  | ['\n' '\r' '\011' '\012']
      { reset_line lexbuf; let s = tok lexbuf in s ^ comment lexbuf }
  | "+" { pass_zero();
	  if !current_line_started
	  then (start_line true; let s = tok lexbuf in s^(comment lexbuf))
	  else comment lexbuf }
  (* noteopti: *)
  | [^ '*'] { start_line true; let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { start_line true; let s = tok lexbuf in s ^ comment lexbuf }
  | _  
      { start_line true; let s = tok lexbuf in
        Common.pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment lexbuf
      }

