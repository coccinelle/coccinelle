# 0 "./lexer_cocci.mll"
 
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module P = Parse_aux
module FC = Flag_parsing_cocci
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
  (*line_start := -1;*)
  prev_plus := (c = D.PLUS) || (c = D.PLUSPLUS);
  (c,l,ll,ll,lex_start,preceeding_spaces,[],[],[],"")
let current_line_started = ref false
let col_zero = ref true

let contextify (c,l,ll,lle,lex_start,preceeding_spaces,bef,aft,pos,ws) =
  (D.CONTEXT,l,ll,lle,lex_start,preceeding_spaces,bef,aft,pos,ws)

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

let opt_reverse_token token =
  if !FC.interpret_inverted
  then match token with
         D.MINUS        -> D.PLUSPLUS  (* maybe too liberal *)
       | D.OPTMINUS     -> lexerr "cannot invert token ?- (an optional minus line), which is needed for reversing the patch" ""  
       | D.UNIQUEMINUS  -> D.PLUS
       | D.PLUS         -> D.MINUS
       | D.PLUSPLUS     -> D.MINUS (* may not be sufficient *)
       | _              -> token
  else token

let add_current_line_type x =
  match (opt_reverse_token x,!current_line_type) with
    (D.MINUS,(D.CONTEXT,ln,lln))  ->
      current_line_type := (D.MINUS,ln,lln)
  | (D.MINUS,(D.UNIQUE,ln,lln))   ->
      current_line_type := (D.UNIQUEMINUS,ln,lln)
  | (D.MINUS,(D.OPT,ln,lln))      ->
      current_line_type := (D.OPTMINUS,ln,lln)
  | (D.PLUS,(D.CONTEXT,ln,lln))   ->
      current_line_type := (D.PLUS,ln,lln)
  | (D.PLUSPLUS,(D.CONTEXT,ln,lln))   ->
      current_line_type := (D.PLUSPLUS,ln,lln)
  | (D.UNIQUE,(D.CONTEXT,ln,lln)) ->
      current_line_type := (D.UNIQUE,ln,lln)
  | (D.OPT,(D.CONTEXT,ln,lln))    ->
      current_line_type := (D.OPT,ln,lln)
  | _ -> lexerr "invalid control character combination" ""

let check_minus_context_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) -> lexerr "invalid in a + context: " s
  | _ -> ()

let check_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) -> ()
  | _ -> lexerr "invalid in a nonempty context: " s

let check_plus_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) -> ()
  | _ -> lexerr "invalid in a non + context: " s

let check_arity_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) | (D.PLUS,_,_) | (D.PLUSPLUS,_,_)
  | (D.UNIQUE,_,_) | (D.OPT,_,_) -> ()
  | _ -> lexerr "invalid in a nonempty context: " s

let check_comment s =
  if not !current_line_started
  then lexerr "+ expected at the beginning of the line" s

let process_include start finish str =
  (match !current_line_type with
    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
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
      if not !D.ignore_patch_or_match
      then
	(match !pm with
	  MATCH ->
	    lexerr "- or + not allowed in the first column for a match" ""
	| PATCH -> ()
	| UNKNOWN -> Flag.sgrep_mode2 := false; pm := PATCH)
  | MATCH ->
      if not !D.ignore_patch_or_match
      then
	(match !pm with
	  PATCH -> lexerr "* not allowed in the first column for a patch" ""
	| MATCH -> ()
	| UNKNOWN -> Flag.sgrep_mode2 := true; pm := MATCH)
  | _ -> failwith "unexpected argument"

(* ---------------------------------------------------------------------- *)
(* identifiers, including metavariables *)

let metavariables = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let all_metavariables =
  (Hashtbl.create(100) : (string,(string * (D.clt -> token)) list) Hashtbl.t)

let type_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let attr_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let declarer_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let iterator_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let symbol_names = (Hashtbl.create(15) : (string, D.clt -> token) Hashtbl.t)

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
	  (try
	    let x = (Hashtbl.find attr_names s) linetype in
	    check_plus_linetype s;
	    x
	  with Not_found ->
	    (try (Hashtbl.find declarer_names s) linetype
	    with Not_found ->
	      (try (Hashtbl.find iterator_names s) linetype
	      with Not_found ->
		(try (Hashtbl.find symbol_names s) linetype
		with Not_found ->
                  TIdent (s,linetype)))))) in
  if !Data.in_meta || !Data.in_rule_name
  then (try Hashtbl.find rule_names s; TRuleName s with Not_found -> fail())
  else fail()

let id_tokens lexbuf =
  let s = tok lexbuf in
  let linetype = get_current_line_type lexbuf in
  let in_rule_name = !Data.in_rule_name in
  let in_meta = !Data.in_meta && not !Data.saw_struct in
  let in_iso = !Data.in_iso in
  let in_prolog = !Data.in_prolog in
  (if s = "identifer" && in_meta
  then Common.pr2 "Warning: should identifer be identifier?");
  match s with
    "metavariable" when in_meta -> check_arity_context_linetype s; TMetavariable
  | "identifier" when in_meta -> check_arity_context_linetype s; TIdentifier
  | "type" when in_meta ->       check_arity_context_linetype s; TType
  | "parameter" when in_meta ->  check_arity_context_linetype s; TParameter
  | "constant"  when in_meta ->  check_arity_context_linetype s; TConstant
  | "generated" when in_rule_name && not (!Flag.make_hrule = None) ->
      check_arity_context_linetype s; TGenerated
  | "expression" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TExpression
  | "declaration" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TDeclaration
  | "field" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TField
  | "initialiser" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TInitialiser
  | "initializer" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TInitialiser
  | "idexpression" when in_meta ->
      check_arity_context_linetype s; TIdExpression
  | "statement" when in_meta ->  check_arity_context_linetype s; TStatement
  | "function"  when in_meta ->  check_arity_context_linetype s; TFunction
  | "local" when in_meta ->      check_arity_context_linetype s; TLocal
  | "global" when in_meta ->     check_arity_context_linetype s; TGlobal
  | "list" when in_meta ->       check_arity_context_linetype s; Tlist
  | "fresh" when in_meta ->      check_arity_context_linetype s; TFresh
  | "typedef" when in_meta ->    check_arity_context_linetype s; TTypedef
  | "attribute" when in_meta ->  check_arity_context_linetype s; TAttribute
  | "declarer" when in_meta ->   check_arity_context_linetype s; TDeclarer
  | "iterator" when in_meta ->   check_arity_context_linetype s; TIterator
  | "name" when in_meta ->       check_arity_context_linetype s; TName
  | "position" when in_meta ->   check_arity_context_linetype s; TPosition
  | "format" when in_meta ->     check_arity_context_linetype s; TFormat
  | "analysis" when in_meta ->   check_arity_context_linetype s; TAnalysis
  | "any" when in_meta ->        check_arity_context_linetype s; TPosAny
  | "pure" when in_meta && in_iso ->
      check_arity_context_linetype s; TPure
  | "context" when in_meta && in_iso ->
      check_arity_context_linetype s; TContext
  | "error" when in_meta ->      check_arity_context_linetype s; TError
  | "words" when in_meta ->      check_context_linetype s; TWords
  | "symbol" when in_meta ->     check_arity_context_linetype s; TSymbol

  | "using" when in_rule_name || in_prolog ->  check_context_linetype s; TUsing
  | "virtual" when in_prolog || in_rule_name || in_meta ->
      (* don't want to allow virtual as a rule name *)
      check_context_linetype s; TVirtual
  | "disable" when in_rule_name ->  check_context_linetype s; TDisable
  | "extends" when in_rule_name -> check_context_linetype s; TExtends
  | "depends" when in_rule_name -> check_context_linetype s; TDepends
  | "on" when in_rule_name      -> check_context_linetype s; TOn
  | "ever" when in_rule_name    -> check_context_linetype s; TEver
  | "never" when in_rule_name   -> check_context_linetype s; TNever
  (* exists and forall for when are reparsed in parse_cocci.ml *)
  | "exists" when in_rule_name  -> check_context_linetype s; TExists
  | "forall" when in_rule_name  -> check_context_linetype s; TForall
  | "script" when in_rule_name  -> check_context_linetype s; TScript
  | "initialize" when in_rule_name -> check_context_linetype s; TInitialize
  | "finalize" when in_rule_name   -> check_context_linetype s; TFinalize

  | "char" ->       Tchar     linetype
  | "short" ->      Tshort    linetype
  | "int" ->        Tint      linetype
  | "double" ->     Tdouble   linetype
  | "float" ->      Tfloat    linetype
  | "long" ->       Tlong     linetype
  | "void" ->       Tvoid     linetype
  | "size_t" ->     Tsize_t   linetype
  | "ssize_t" ->    Tssize_t  linetype
  | "ptrdiff_t" ->  Tptrdiff_t linetype
  (* in_meta is only for the first keyword; drop it now to allow any type
     name *)
  | "struct" ->     Data.saw_struct := true; Tstruct   linetype
  | "union" ->      Data.saw_struct := true; Tunion    linetype
  | "enum" ->       Data.saw_struct := true; Tenum     linetype
  | "unsigned" ->   Tunsigned linetype
  | "signed" ->     Tsigned   linetype
  | "decimal" when !Flag.ibm -> Tdecimal linetype
  | "EXEC" when !Flag.ibm -> Texec linetype

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

  | "Expression"       when !Data.in_iso -> TIsoExpression
  | "ArgExpression"    when !Data.in_iso -> TIsoArgExpression
  | "TestExpression"   when !Data.in_iso -> TIsoTestExpression
  | "ToTestExpression" when !Data.in_iso -> TIsoToTestExpression
  | "Statement"        when !Data.in_iso -> TIsoStatement
  | "Declaration"      when !Data.in_iso -> TIsoDeclaration
  | "Type"             when !Data.in_iso -> TIsoType
  | "TopLevel"         when !Data.in_iso -> TIsoTopLevel

  | "_" when !Data.in_meta -> TUnderscore

  | s -> check_var s linetype

let mkassign op lexbuf =
  TAssign (Ast.OpAssign op, (get_current_line_type lexbuf))

let init _ =
  line := 1;
  logical_line := 0;
  prev_plus := false;
  line_start := 0;
  current_line_started := false;
  current_line_type := (D.CONTEXT,0,0);
  col_zero := true;
  pm := UNKNOWN;
  Data.in_rule_name := false;
  Data.in_meta := false;
  Data.in_prolog := false;
  Data.saw_struct := false;
  Data.inheritable_positions := [];
  Hashtbl.clear all_metavariables;
  Hashtbl.clear Data.all_metadecls;
  Hashtbl.clear metavariables;
  Hashtbl.clear type_names;
  Hashtbl.clear rule_names;
  Hashtbl.clear iterator_names;
  Hashtbl.clear declarer_names;
  Hashtbl.clear symbol_names;
  let get_name (_,x) = x in
  Data.add_meta_meta :=
    (fun name pure ->
      let fn clt = TMeta(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_id_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaId(name,constraints,Ast.NoVal,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_virt_id_meta_found :=
    (fun name vl ->
      let fn clt = TIdent(vl,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_virt_id_meta_not_found :=
    (fun name pure ->
      let fn clt = TMetaId(name,Ast.IdNoConstraint,Ast.NoVal,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_fresh_id_meta :=
    (fun name seed ->
      let fn clt = TMetaId(name,Ast.IdNoConstraint,seed,Ast0.Impure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_type_meta :=
    (fun name pure ->
      let fn clt = TMetaType(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_init_meta :=
    (fun name pure ->
      let fn clt = TMetaInit(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_initlist_meta :=
    (function name -> function lenname -> function pure ->
      let fn clt = TMetaInitList(name,lenname,pure,clt) in
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
  Data.add_global_idexp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaGlobalIdExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_explist_meta :=
    (function name -> function lenname -> function pure ->
      let fn clt = TMetaExpList(name,lenname,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_decl_meta :=
    (function name -> function pure ->
      let fn clt = TMetaDecl(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_field_meta :=
    (function name -> function pure ->
      let fn clt = TMetaField(name,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_field_list_meta :=
    (function name -> function lenname -> function pure ->
      let fn clt = TMetaFieldList(name,lenname,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_fmt_meta :=
    (function name -> function iconstraints ->
      let fn clt = failwith "format metavariable only allowed in a string" in
      Data.format_metavariables :=
	(get_name name,(name,iconstraints)) :: !Data.format_metavariables;
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_fmtlist_meta :=
    (function name -> function lenname ->
      let fn clt =
	failwith "format list metavariable only allowed in a string" in
      Data.format_list_metavariables :=
	(get_name name,(name,lenname)) :: !Data.format_list_metavariables;
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
      let fn ((d,ln,_,_,_,_,_,_,_,_) as clt) =
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
  Data.add_attribute :=
    (function name ->
      let fn clt = TDirective (Ast.Space name, clt) in
      Hashtbl.replace attr_names name fn);
  Data.add_declarer_name :=
    (function name ->
      let fn clt = TDeclarerId(name,clt) in
      Hashtbl.replace declarer_names name fn);
  Data.add_iterator_name :=
    (function name ->
      let fn clt = TIteratorId(name,clt) in
      Hashtbl.replace iterator_names name fn);
  Data.add_symbol_meta :=
    (function name ->
      let fn clt = TSymId (name,clt) in
      Hashtbl.replace symbol_names name fn);
  Data.init_rule := (function _ -> Hashtbl.clear metavariables);
  Data.install_bindings :=
    (function parent ->
      List.iter (function (name,fn) -> Hashtbl.add metavariables name fn)
	(Hashtbl.find all_metavariables parent))

(* initialization for each cocci rule *)
let reinit _ =
  Data.format_metavariables := [];
  Data.format_list_metavariables := []

(* the following is needed to properly tokenize include files.  Because an
include file is included after seeing a @, so current_line_started is true.
Current_line_started is not important for parsing the name of a rule, so we
don't have to reset this value to true after parsing an included file. *)
let include_init _ =
  current_line_started := false

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

# 526 "lexer_cocci.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\165\255\166\255\081\000\119\000\172\255\173\255\192\000\
    \023\001\052\001\038\000\067\000\101\000\079\000\219\255\081\000\
    \082\000\085\000\225\255\227\255\230\255\231\255\232\255\233\255\
    \235\255\086\000\111\000\239\255\087\000\241\255\116\000\118\000\
    \140\000\122\001\144\001\231\001\089\000\062\002\150\000\255\255\
    \176\002\107\000\146\000\182\255\212\255\097\000\159\002\246\002\
    \077\003\164\003\251\003\082\004\169\004\000\005\087\005\174\005\
    \005\006\092\006\206\006\118\000\176\000\119\000\149\000\150\000\
    \037\007\103\000\124\007\211\007\013\008\100\008\158\008\106\000\
    \052\000\245\008\126\000\135\000\076\009\163\009\136\000\250\009\
    \081\010\139\010\226\010\251\255\057\011\144\011\231\011\062\012\
    \149\012\134\001\150\000\156\000\159\000\144\000\246\255\244\255\
    \163\000\226\255\148\002\220\001\051\002\150\000\152\000\153\000\
    \169\000\171\000\175\000\247\255\176\000\177\000\245\255\206\255\
    \205\255\168\255\215\255\179\000\229\255\022\001\214\255\182\000\
    \153\001\201\255\234\255\236\255\238\255\195\255\209\255\213\255\
    \200\255\165\000\168\000\207\255\204\255\211\255\167\255\202\255\
    \196\255\210\255\208\255\207\012\038\013\172\000\096\013\183\013\
    \173\000\014\014\101\014\159\014\246\014\127\000\208\000\045\001\
    \119\000\133\000\127\000\122\006\190\255\138\000\150\000\151\000\
    \054\001\104\015\179\015\152\000\150\000\173\000\183\000\149\001\
    \037\016\120\016\187\255\188\000\183\000\211\000\224\000\186\255\
    \017\015\224\000\216\000\208\000\226\000\237\000\209\006\225\002\
    \125\001\206\001\185\255\226\002\184\255\155\001\114\002\116\002\
    \203\002\156\001\206\002\208\002\027\002\227\000\014\001\046\001\
    \013\001\015\001\028\002\201\002\012\001\046\001\202\002\043\001\
    \042\001\205\002\052\001\066\001\209\002\169\255\038\015\129\001\
    \195\016\241\003\119\007\068\009\222\016\071\004\158\004\036\017\
    \130\001\170\255\115\017\220\255\193\017\231\017\224\255\225\255\
    \149\001\051\018\138\018\173\001\237\255\006\002\239\255\240\255\
    \245\004\174\001\243\255\244\255\245\255\246\255\247\255\248\255\
    \129\002\160\002\226\001\039\003\255\255\109\015\245\001\210\002\
    \233\255\252\255\236\255\251\255\235\255\250\255\242\255\077\005\
    \245\009\163\005\250\005\234\255\238\255\232\255\237\001\196\018\
    \027\019\010\002\114\019\201\019\003\020\090\020\148\020\011\002\
    \201\001\235\020\051\002\063\002\066\021\153\021\065\002\240\021\
    \071\022\129\022\216\022\047\023\134\023\096\002\192\023\023\024\
    \100\002\110\024\197\024\255\024\086\025\221\255\049\015\232\002\
    \165\025\027\007\009\014\027\016\192\025\129\007\089\008\006\026\
    \007\003\222\255\117\002\255\255\010\016\252\255\076\026\084\006\
    \203\007\254\255\099\026\253\255\228\005\254\255\251\016\255\255\
    \251\255\137\026\237\008\078\009\253\255\160\026\252\255\018\017\
    \253\255\009\017\254\255\255\255\250\255\198\026\155\009\255\009\
    \252\255\221\026\251\255\108\011\252\255\253\255\254\255\145\002\
    \255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\085\000\085\000\255\255\255\255\077\000\
    \076\000\090\000\052\000\064\000\063\000\039\000\255\255\035\000\
    \058\000\031\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\018\000\090\000\255\255\015\000\255\255\013\000\012\000\
    \057\000\027\000\076\000\076\000\005\000\076\000\032\000\255\255\
    \001\000\255\255\002\000\255\255\255\255\255\255\255\255\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\255\255\255\255\255\255\255\255\003\000\
    \255\255\079\000\255\255\080\000\255\255\078\000\255\255\255\255\
    \255\255\255\255\078\000\255\255\255\255\255\255\080\000\255\255\
    \080\000\255\255\255\255\255\255\076\000\076\000\006\000\076\000\
    \076\000\084\000\255\255\007\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\084\000\255\255\056\000\062\000\033\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\038\000\255\255\074\000\255\255\037\000\
    \075\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\061\000\034\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\081\000\255\255\255\255\255\255\
    \081\000\255\255\081\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\066\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\067\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \072\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\255\255\255\255\255\255\
    \255\255\255\255\072\000\072\000\255\255\255\255\072\000\255\255\
    \255\255\072\000\255\255\255\255\072\000\255\255\085\000\085\000\
    \255\255\084\000\255\255\255\255\085\000\084\000\255\255\085\000\
    \085\000\255\255\255\255\255\255\033\000\033\000\255\255\255\255\
    \035\000\025\000\024\000\035\000\255\255\035\000\255\255\255\255\
    \014\000\035\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\006\000\035\000\035\000\255\255\001\000\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\032\000\
    \255\255\032\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\027\000\255\255\028\000\255\255\026\000\255\255\255\255\
    \255\255\255\255\026\000\255\255\255\255\255\255\028\000\255\255\
    \028\000\255\255\255\255\255\255\029\000\255\255\255\255\255\255\
    \029\000\255\255\029\000\255\255\255\255\255\255\033\000\033\000\
    \255\255\032\000\255\255\255\255\033\000\032\000\255\255\033\000\
    \033\000\255\255\255\255\255\255\000\000\255\255\003\000\001\000\
    \001\000\255\255\002\000\255\255\255\255\255\255\001\000\255\255\
    \255\255\004\000\002\000\002\000\255\255\003\000\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\005\000\003\000\003\000\
    \255\255\004\000\255\255\255\255\255\255\255\255\255\255\004\000\
    \255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\042\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\061\000\061\000\061\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\117\000\000\000\255\255\
    \120\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \189\000\255\255\255\255\255\255\255\255\255\255\255\255\187\000\
    \185\000\185\000\000\000\187\000\000\000\189\000\189\000\189\000\
    \189\000\193\000\189\000\189\000\196\000\255\255\255\255\255\255\
    \255\255\255\255\202\000\203\000\255\255\255\255\206\000\255\255\
    \255\255\209\000\255\255\255\255\212\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\227\000\000\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\059\001\000\000\061\001\000\000\255\255\255\255\
    \255\255\000\000\255\255\000\000\069\001\000\000\072\001\000\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\000\000\080\001\
    \000\000\084\001\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\255\255\000\000\092\001\000\000\000\000\000\000\255\255\
    \000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\040\000\039\000\039\000\039\000\039\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \040\000\028\000\005\000\009\000\007\000\015\000\012\000\006\000\
    \027\000\024\000\017\000\030\000\019\000\031\000\033\000\038\000\
    \004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\010\000\018\000\032\000\013\000\016\000\029\000\
    \036\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\035\000\
    \008\000\008\000\008\000\023\000\026\000\022\000\011\000\037\000\
    \139\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\034\000\
    \008\000\008\000\008\000\021\000\025\000\020\000\014\000\217\000\
    \138\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\136\000\135\000\134\000\133\000\128\000\
    \129\000\130\000\127\000\126\000\121\000\213\000\218\000\124\000\
    \122\000\083\000\042\000\066\000\255\255\214\000\061\000\119\000\
    \062\000\045\000\137\000\115\000\072\000\217\000\215\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\220\000\219\000\
    \219\000\118\000\068\000\114\000\116\000\213\000\218\000\104\000\
    \071\000\060\000\105\000\213\000\218\000\214\000\063\000\063\000\
    \043\000\079\000\075\000\214\000\091\000\042\000\215\000\093\000\
    \102\000\101\000\092\000\103\000\215\000\096\000\095\000\216\000\
    \060\000\097\000\125\000\044\000\113\000\112\000\111\000\108\000\
    \059\000\106\000\094\000\213\000\218\000\107\000\109\000\110\000\
    \117\000\120\000\132\000\214\000\007\000\131\000\145\000\141\000\
    \210\000\171\000\163\000\123\000\215\000\157\000\158\000\216\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\045\000\159\000\046\000\160\000\164\000\165\000\
    \002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\166\000\167\000\172\000\173\000\007\000\
    \255\255\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\199\000\155\000\198\000\161\000\
    \174\000\175\000\197\000\178\000\179\000\180\000\181\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\182\000\046\000\155\000\207\000\161\000\156\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\204\000\203\000\202\000\205\000\008\000\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\255\255\176\000\206\000\255\255\255\255\200\000\
    \153\000\150\000\208\000\177\000\209\000\151\000\168\000\255\255\
    \149\000\201\000\211\000\255\255\152\000\255\255\255\255\212\000\
    \090\000\154\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\007\000\168\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\045\000\098\000\046\000\224\000\225\000\035\001\
    \013\001\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\098\000\006\001\224\000\225\000\008\000\
    \186\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \087\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\007\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\255\255\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\001\001\046\000\255\000\255\255\255\255\018\001\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\084\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\012\001\011\001\014\001\024\001\008\000\020\001\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\007\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\023\001\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\031\001\046\000\027\001\255\255\255\255\255\255\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\255\255\041\001\255\255\255\255\047\000\037\001\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\040\000\039\000\039\000\039\000\039\000\004\001\100\000\
    \096\001\100\000\000\000\064\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\255\255\000\000\
    \040\000\060\001\000\000\255\255\255\255\255\255\194\000\255\255\
    \255\255\192\000\255\255\255\255\255\255\002\001\000\000\041\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\000\000\000\000\000\000\000\000\064\000\005\001\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\007\000\255\255\255\255\064\000\003\001\255\255\
    \188\000\000\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\193\000\046\000\195\000\056\001\000\000\196\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\001\000\000\057\001\056\001\008\000\255\000\048\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\007\000\255\255\057\001\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\045\000\
    \000\000\046\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\000\000\008\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\049\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \007\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\045\000\000\000\
    \046\000\255\255\255\255\000\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000\
    \000\000\000\000\000\000\008\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \050\000\008\000\008\000\008\000\008\000\008\000\008\000\007\000\
    \000\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\045\000\098\000\046\000\
    \000\000\000\000\000\000\000\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\098\000\000\000\
    \000\000\000\000\008\000\000\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\051\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\007\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\045\000\000\000\046\000\000\000\
    \000\000\000\000\000\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\000\000\000\000\000\000\
    \000\000\008\000\000\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\052\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\007\000\221\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\045\000\000\000\046\000\000\000\000\000\
    \000\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\008\000\053\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\007\000\007\001\007\001\007\001\
    \007\001\007\001\007\001\007\001\007\001\007\001\007\001\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\045\000\000\000\046\000\000\000\000\000\000\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\000\000\000\000\000\000\000\000\008\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\054\000\008\000\008\000\
    \008\000\008\000\008\000\007\000\000\000\007\001\007\001\007\001\
    \007\001\007\001\007\001\007\001\007\001\007\001\007\001\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\008\001\046\000\000\000\000\000\000\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\001\000\000\000\000\000\000\008\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\055\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\007\000\009\001\009\001\009\001\009\001\009\001\
    \009\001\009\001\009\001\009\001\009\001\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\000\000\046\000\000\000\000\000\000\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\071\001\000\000\008\000\000\000\008\000\
    \008\000\008\000\008\000\056\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\007\000\009\001\009\001\009\001\009\001\009\001\009\001\
    \009\001\009\001\009\001\009\001\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\045\000\
    \070\001\046\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\000\000\057\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \007\000\000\000\000\000\155\000\064\001\064\001\064\001\064\001\
    \064\001\064\001\064\001\064\001\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\045\000\000\000\
    \046\000\000\000\155\000\000\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000\
    \000\000\000\000\000\000\058\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\060\000\
    \000\000\000\000\182\000\000\000\000\000\000\000\153\000\150\000\
    \000\000\000\000\000\000\151\000\255\255\000\000\149\000\000\000\
    \000\000\000\000\152\000\000\000\000\000\000\000\060\000\154\000\
    \000\000\182\000\007\000\184\000\000\000\000\000\059\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\000\000\046\000\000\000\000\000\183\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\000\000\008\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\064\000\000\000\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\000\000\
    \008\001\000\000\000\000\065\000\000\000\000\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \008\001\000\000\000\000\000\000\064\000\000\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \067\000\000\000\222\000\064\000\222\000\000\000\000\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\000\000\000\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\000\000\
    \000\000\000\000\000\000\067\000\000\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \000\000\000\000\068\000\065\001\065\001\065\001\065\001\065\001\
    \065\001\065\001\065\001\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\075\000\000\000\076\000\
    \000\000\000\000\000\000\000\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\000\000\000\000\
    \000\000\069\000\067\000\000\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \000\000\000\000\000\000\000\000\069\000\000\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\000\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\071\000\000\000\
    \070\000\000\000\000\000\000\000\000\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\000\000\
    \000\000\000\000\073\000\069\000\000\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\000\000\000\000\000\000\000\000\073\000\000\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\000\000\000\000\073\000\075\001\075\001\075\001\
    \075\001\075\001\075\001\075\001\075\001\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\000\000\
    \000\000\000\000\000\000\074\000\000\000\000\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \000\000\000\000\000\000\000\000\073\000\000\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \077\000\000\000\217\000\073\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\076\001\076\001\
    \076\001\076\001\076\001\076\001\076\001\076\001\000\000\000\000\
    \000\000\218\000\000\000\000\000\000\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\000\000\
    \000\000\218\000\000\000\077\000\000\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \000\000\000\000\077\000\087\001\087\001\087\001\087\001\087\001\
    \087\001\087\001\087\001\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\000\000\000\000\000\000\
    \000\000\078\000\000\000\000\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\000\000\000\000\
    \000\000\000\000\077\000\000\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\080\000\000\000\
    \010\001\077\000\010\001\000\000\000\000\009\001\009\001\009\001\
    \009\001\009\001\009\001\009\001\009\001\009\001\009\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\000\000\
    \000\000\000\000\000\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\000\000\000\000\000\000\
    \000\000\080\000\000\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\075\000\000\000\081\000\000\000\000\000\
    \000\000\000\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\000\000\000\000\000\000\082\000\
    \080\000\000\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\000\000\000\000\
    \000\000\000\000\082\000\000\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\000\000\
    \000\000\082\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\000\000\000\000\000\000\000\000\
    \078\000\000\000\000\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\000\000\000\000\000\000\
    \000\000\082\000\000\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\007\000\000\000\000\000\
    \082\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\045\000\000\000\046\000\094\001\094\001\
    \094\001\094\001\008\000\008\000\008\000\008\000\085\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\000\000\000\000\095\001\093\001\
    \008\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\007\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\045\000\000\000\046\000\000\000\000\000\000\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\086\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\000\000\000\000\000\000\000\000\008\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\007\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\000\000\046\000\000\000\000\000\000\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\000\000\000\000\000\000\000\000\008\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\007\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\000\000\046\000\000\000\000\000\000\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\000\000\008\000\000\000\008\000\
    \008\000\008\000\008\000\088\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\045\000\
    \000\000\046\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\140\000\008\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\086\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\000\000\000\000\000\000\000\000\140\000\000\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \141\000\000\000\142\000\000\000\000\000\000\000\000\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\000\000\000\000\000\000\143\000\140\000\000\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\000\000\000\000\000\000\000\000\143\000\
    \000\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\000\000\000\000\143\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\000\000\000\000\000\000\000\000\144\000\000\000\000\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\000\000\000\000\000\000\000\000\143\000\000\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\146\000\000\000\054\001\143\000\054\001\000\000\
    \000\000\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\000\000\000\000\000\000\000\000\146\000\000\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\141\000\
    \000\000\147\000\000\000\000\000\000\000\000\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \000\000\000\000\000\000\148\000\146\000\000\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\000\000\000\000\000\000\000\000\148\000\000\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\255\255\000\000\148\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \000\000\000\000\000\000\000\000\144\000\000\000\000\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\000\000\000\000\000\000\000\000\148\000\000\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\161\000\225\000\000\000\148\000\191\000\253\000\252\000\
    \252\000\252\000\252\000\225\000\000\000\057\001\000\000\190\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\057\001\000\000\
    \161\000\000\000\000\000\000\000\000\000\253\000\000\000\000\000\
    \000\000\000\000\225\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\225\000\254\000\057\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\057\001\000\000\
    \000\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\000\000\000\000\000\000\000\000\162\000\
    \000\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\000\000\000\000\
    \000\000\255\255\162\000\000\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\168\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\000\000\000\000\000\000\168\000\000\000\000\000\
    \000\000\049\001\000\000\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \050\001\000\000\000\000\000\000\000\000\000\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \050\001\000\000\062\001\000\000\169\000\000\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \170\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\000\000\000\000\000\000\000\000\169\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\255\255\000\000\217\000\000\000\220\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\219\000\219\000\
    \000\000\000\000\000\000\000\000\082\001\082\001\082\001\082\001\
    \000\000\000\000\000\000\218\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\214\000\074\001\074\001\074\001\074\001\074\001\
    \074\001\074\001\074\001\215\000\083\001\000\000\000\000\000\000\
    \000\000\086\001\086\001\086\001\086\001\086\001\086\001\086\001\
    \086\001\000\000\000\000\218\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\214\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\215\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\000\000\000\000\000\000\081\001\000\000\
    \214\000\000\000\000\000\073\001\000\000\000\000\000\000\000\000\
    \000\000\215\000\000\000\000\000\253\000\252\000\252\000\252\000\
    \252\000\085\001\000\000\000\000\000\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\000\000\000\000\000\000\000\000\000\000\
    \214\000\000\000\000\000\253\000\248\000\230\000\235\000\233\000\
    \000\000\215\000\231\000\247\000\246\000\236\000\000\000\239\000\
    \241\000\240\000\251\000\229\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\232\000\238\000\237\000\
    \249\000\000\000\000\000\250\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\245\000\000\000\
    \244\000\000\000\234\000\000\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\243\000\049\001\
    \242\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\045\001\050\001\000\000\
    \000\000\255\255\000\000\000\000\000\000\046\001\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\049\001\047\001\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\051\001\
    \051\001\000\000\000\000\000\000\000\000\045\001\050\001\000\000\
    \000\000\000\000\000\000\045\001\050\001\046\001\000\000\000\000\
    \000\000\000\000\000\000\046\001\000\000\000\000\047\001\000\000\
    \000\000\000\000\000\000\000\000\047\001\000\000\000\000\048\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\045\001\050\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\046\001\000\000\000\000\000\000\233\000\
    \000\000\000\000\000\000\000\000\047\001\000\000\000\000\048\001\
    \000\000\000\000\000\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\014\001\000\000\015\001\
    \000\000\000\000\000\000\255\255\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\000\000\000\000\
    \000\000\000\000\233\000\000\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\014\001\000\000\015\001\000\000\
    \000\000\000\000\000\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\000\000\000\000\000\000\
    \016\001\234\000\000\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\000\000\
    \000\000\000\000\000\000\016\001\000\000\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \000\000\000\000\016\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\000\000\000\000\000\000\
    \000\000\017\001\000\000\000\000\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\000\000\000\000\
    \000\000\000\000\016\001\000\000\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\019\001\000\000\
    \000\000\016\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\000\000\000\000\000\000\
    \000\000\019\001\000\000\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\000\000\000\000\
    \020\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\027\001\000\000\028\001\000\000\000\000\
    \000\000\000\000\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\000\000\000\000\000\000\021\001\
    \019\001\000\000\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\000\000\000\000\
    \000\000\000\000\021\001\000\000\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\023\001\000\000\022\001\000\000\
    \000\000\000\000\000\000\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\000\000\000\000\000\000\
    \025\001\021\001\000\000\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\000\000\
    \000\000\000\000\000\000\025\001\000\000\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \000\000\000\000\025\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\000\000\000\000\000\000\
    \000\000\026\001\000\000\000\000\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\000\000\000\000\
    \000\000\000\000\025\001\000\000\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\029\001\000\000\
    \000\000\025\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\000\000\000\000\000\000\
    \000\000\029\001\000\000\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\000\000\000\000\
    \029\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\000\000\000\000\000\000\000\000\030\001\
    \000\000\000\000\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\000\000\000\000\000\000\000\000\
    \029\001\000\000\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\032\001\000\000\000\000\029\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\000\000\000\000\000\000\000\000\032\001\
    \000\000\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\027\001\000\000\033\001\000\000\000\000\000\000\000\000\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\000\000\000\000\000\000\034\001\032\001\000\000\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\000\000\000\000\000\000\000\000\
    \034\001\000\000\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\000\000\000\000\034\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\000\000\000\000\000\000\000\000\030\001\000\000\
    \000\000\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\000\000\000\000\000\000\000\000\034\001\
    \000\000\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\036\001\000\000\000\000\034\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\000\000\000\000\000\000\000\000\036\001\000\000\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \037\001\000\000\038\001\000\000\000\000\000\000\000\000\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\000\000\000\000\000\000\039\001\036\001\000\000\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\000\000\000\000\000\000\000\000\039\001\
    \000\000\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\000\000\000\000\039\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\000\000\000\000\000\000\000\000\040\001\000\000\000\000\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\000\000\000\000\000\000\000\000\039\001\000\000\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\042\001\000\000\000\000\039\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\000\000\000\000\000\000\000\000\042\001\000\000\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\037\001\
    \000\000\043\001\000\000\000\000\000\000\000\000\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \000\000\000\000\000\000\044\001\042\001\000\000\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\000\000\000\000\000\000\000\000\044\001\000\000\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\000\000\000\000\044\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \000\000\000\000\000\000\000\000\040\001\000\000\000\000\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\000\000\000\000\000\000\000\000\044\001\000\000\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\000\000\000\000\000\000\044\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\055\001\055\001\
    \055\001\055\001\055\001\055\001\000\000\000\000\049\001\000\000\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \051\001\051\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\050\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\046\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\047\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\050\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\046\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\047\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\055\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\001\
    \055\001\055\001\055\001\055\001\055\001\000\000\000\000\000\000\
    \000\000\000\000\046\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\047\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\001\
    \055\001\055\001\055\001\055\001\055\001\000\000\000\000\000\000\
    \000\000\000\000\046\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\047\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\066\001\066\001\066\001\
    \066\001\066\001\066\001\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\067\001\067\001\067\001\067\001\
    \067\001\067\001\000\000\000\000\000\000\066\001\066\001\066\001\
    \066\001\066\001\066\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\000\000\067\001\067\001\067\001\067\001\
    \067\001\067\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\078\001\078\001\078\001\078\001\078\001\078\001\000\000\
    \000\000\000\000\077\001\077\001\077\001\077\001\077\001\077\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \000\000\078\001\078\001\078\001\078\001\078\001\078\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\090\001\090\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\090\001\090\001\
    \090\001\090\001\090\001\090\001\000\000\000\000\000\000\089\001\
    \089\001\089\001\089\001\089\001\089\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\090\001\090\001\
    \090\001\090\001\090\001\090\001\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \011\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\012\000\013\000\013\000\015\000\016\000\
    \016\000\016\000\017\000\025\000\028\000\003\000\003\000\026\000\
    \026\000\036\000\041\000\045\000\042\000\003\000\059\000\030\000\
    \061\000\065\000\012\000\031\000\071\000\004\000\003\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\030\000\072\000\031\000\031\000\003\000\003\000\032\000\
    \074\000\060\000\032\000\004\000\004\000\003\000\062\000\063\000\
    \038\000\075\000\078\000\004\000\090\000\038\000\003\000\091\000\
    \032\000\032\000\091\000\032\000\004\000\092\000\093\000\004\000\
    \060\000\096\000\025\000\038\000\101\000\102\000\103\000\104\000\
    \060\000\105\000\091\000\004\000\004\000\106\000\108\000\109\000\
    \115\000\119\000\129\000\004\000\007\000\130\000\141\000\144\000\
    \149\000\152\000\153\000\026\000\004\000\154\000\157\000\004\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\158\000\007\000\159\000\163\000\164\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\165\000\166\000\171\000\172\000\007\000\
    \117\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\008\000\150\000\009\000\150\000\160\000\
    \173\000\174\000\150\000\177\000\178\000\179\000\180\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\181\000\008\000\009\000\197\000\160\000\009\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\198\000\200\000\201\000\204\000\008\000\061\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\042\000\151\000\205\000\062\000\063\000\199\000\
    \009\000\009\000\207\000\151\000\208\000\009\000\167\000\184\000\
    \009\000\199\000\210\000\120\000\009\000\189\000\193\000\211\000\
    \033\000\009\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\034\000\167\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\089\000\034\000\215\000\224\000\232\000\
    \235\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\089\000\241\000\215\000\224\000\034\000\
    \185\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\117\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\250\000\035\000\254\000\196\000\202\000\014\001\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\237\000\237\000\017\001\023\001\035\000\024\001\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\037\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\026\001\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\027\001\037\000\030\001\190\000\184\000\191\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\120\000\037\001\189\000\193\000\037\000\040\001\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\040\000\040\000\040\000\040\000\040\000\248\000\098\000\
    \095\001\098\000\255\255\046\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\185\000\255\255\
    \040\000\058\001\255\255\203\000\206\000\192\000\190\000\209\000\
    \194\000\191\000\195\000\212\000\255\000\249\000\255\255\040\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\255\255\255\255\255\255\255\255\046\000\248\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\047\000\196\000\202\000\046\000\249\000\183\000\
    \187\000\255\255\255\255\255\255\255\255\255\255\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\192\000\047\000\194\000\047\001\255\255\195\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\251\000\255\255\056\001\047\001\047\000\251\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\048\000\190\000\056\001\191\000\058\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \255\255\048\000\255\255\255\255\255\255\255\255\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \255\255\255\255\255\255\255\255\048\000\255\255\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \049\000\203\000\206\000\192\000\255\255\209\000\194\000\255\255\
    \195\000\212\000\255\000\255\255\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\255\255\
    \049\000\183\000\187\000\255\255\255\255\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\255\255\
    \255\255\255\255\255\255\049\000\255\255\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\050\000\
    \255\255\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\217\000\050\000\
    \255\255\255\255\255\255\255\255\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\217\000\255\255\
    \255\255\255\255\050\000\255\255\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\051\000\221\000\
    \221\000\221\000\221\000\221\000\221\000\221\000\221\000\221\000\
    \221\000\255\255\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\255\255\051\000\255\255\
    \255\255\255\255\255\255\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\255\255\255\255\255\255\
    \255\255\051\000\255\255\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\052\000\222\000\222\000\
    \222\000\222\000\222\000\222\000\222\000\222\000\222\000\222\000\
    \255\255\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\255\255\052\000\255\255\255\255\
    \255\255\255\255\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\255\255\255\255\255\255\255\255\
    \052\000\255\255\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\053\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\255\255\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\255\255\053\000\255\255\255\255\255\255\
    \255\255\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\255\255\255\255\255\255\255\255\053\000\
    \255\255\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\054\000\255\255\007\001\007\001\007\001\
    \007\001\007\001\007\001\007\001\007\001\007\001\007\001\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\007\001\054\000\255\255\255\255\255\255\255\255\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\007\001\255\255\255\255\255\255\054\000\255\255\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\055\000\009\001\009\001\009\001\009\001\009\001\
    \009\001\009\001\009\001\009\001\009\001\255\255\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\255\255\055\000\255\255\255\255\255\255\255\255\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\255\255\255\255\068\001\255\255\055\000\255\255\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\056\000\010\001\010\001\010\001\010\001\010\001\010\001\
    \010\001\010\001\010\001\010\001\255\255\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \068\001\056\000\255\255\255\255\255\255\255\255\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \255\255\255\255\255\255\255\255\056\000\255\255\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \057\000\255\255\255\255\155\000\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\255\255\
    \057\000\255\255\155\000\255\255\255\255\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\255\255\
    \255\255\255\255\255\255\057\000\255\255\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\058\000\
    \255\255\255\255\182\000\255\255\255\255\255\255\155\000\155\000\
    \255\255\255\255\255\255\155\000\068\001\255\255\155\000\255\255\
    \255\255\255\255\155\000\255\255\255\255\255\255\058\000\155\000\
    \255\255\182\000\058\000\182\000\255\255\255\255\058\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\255\255\058\000\255\255\255\255\182\000\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\255\255\255\255\255\255\255\255\058\000\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\064\000\255\255\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\255\255\
    \049\001\255\255\255\255\064\000\255\255\255\255\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \049\001\255\255\255\255\255\255\064\000\255\255\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \066\000\255\255\218\000\064\000\218\000\255\255\255\255\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\255\255\255\255\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\255\255\
    \255\255\255\255\255\255\066\000\255\255\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\067\000\
    \255\255\255\255\066\000\064\001\064\001\064\001\064\001\064\001\
    \064\001\064\001\064\001\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\255\255\067\000\
    \255\255\255\255\255\255\255\255\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\255\255\255\255\
    \255\255\068\000\067\000\255\255\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \255\255\255\255\255\255\255\255\068\000\255\255\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \069\000\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\054\001\054\001\255\255\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\255\255\
    \069\000\255\255\255\255\255\255\255\255\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\255\255\
    \255\255\255\255\070\000\069\000\255\255\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\255\255\255\255\255\255\255\255\070\000\255\255\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\073\000\255\255\255\255\070\000\074\001\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\255\255\
    \255\255\255\255\255\255\073\000\255\255\255\255\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \255\255\255\255\255\255\255\255\073\000\255\255\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \076\000\255\255\219\000\073\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\075\001\075\001\
    \075\001\075\001\075\001\075\001\075\001\075\001\255\255\255\255\
    \255\255\219\000\255\255\255\255\255\255\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\255\255\
    \255\255\219\000\255\255\076\000\255\255\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\077\000\
    \255\255\255\255\076\000\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\255\255\255\255\255\255\
    \255\255\077\000\255\255\255\255\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\255\255\255\255\
    \255\255\255\255\077\000\255\255\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\079\000\255\255\
    \008\001\077\000\008\001\255\255\255\255\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\008\001\008\001\008\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\255\255\
    \255\255\255\255\255\255\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\255\255\255\255\255\255\
    \255\255\079\000\255\255\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\080\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\255\255\080\000\255\255\255\255\
    \255\255\255\255\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\255\255\255\255\255\255\081\000\
    \080\000\255\255\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\255\255\255\255\
    \255\255\255\255\081\000\255\255\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\082\000\255\255\
    \255\255\081\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\255\255\255\255\255\255\255\255\
    \082\000\255\255\255\255\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\255\255\255\255\255\255\
    \255\255\082\000\255\255\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\084\000\255\255\255\255\
    \082\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\255\255\084\000\091\001\091\001\
    \091\001\091\001\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\255\255\255\255\091\001\091\001\
    \084\000\255\255\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\085\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\255\255\085\000\255\255\255\255\255\255\
    \255\255\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\255\255\255\255\255\255\255\255\085\000\
    \255\255\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\086\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\255\255\086\000\255\255\255\255\255\255\255\255\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\255\255\255\255\255\255\255\255\086\000\255\255\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\087\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\091\001\255\255\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\255\255\087\000\255\255\255\255\255\255\255\255\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\255\255\255\255\255\255\255\255\087\000\255\255\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\088\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \255\255\088\000\255\255\255\255\255\255\255\255\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \255\255\255\255\255\255\139\000\088\000\255\255\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\255\255\255\255\255\255\255\255\139\000\255\255\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\140\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\255\255\140\000\255\255\255\255\255\255\255\255\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\255\255\255\255\255\255\142\000\140\000\255\255\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\255\255\255\255\255\255\255\255\142\000\
    \255\255\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\143\000\255\255\255\255\142\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\255\255\255\255\255\255\255\255\143\000\255\255\255\255\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\255\255\255\255\255\255\255\255\143\000\255\255\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\145\000\255\255\050\001\143\000\050\001\255\255\
    \255\255\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\255\255\255\255\255\255\255\255\145\000\255\255\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\146\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \255\255\146\000\255\255\255\255\255\255\255\255\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \255\255\255\255\255\255\147\000\146\000\255\255\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\255\255\255\255\255\255\255\255\147\000\255\255\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\148\000\176\000\255\255\147\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \255\255\255\255\255\255\255\255\148\000\255\255\255\255\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\255\255\255\255\255\255\255\255\148\000\255\255\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\161\000\214\000\255\255\148\000\176\000\253\000\253\000\
    \253\000\253\000\253\000\214\000\255\255\046\001\255\255\176\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\046\001\255\255\
    \161\000\255\255\255\255\255\255\255\255\253\000\255\255\255\255\
    \255\255\255\255\214\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\214\000\253\000\046\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\046\001\255\255\
    \255\255\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\255\255\255\255\255\255\255\255\161\000\
    \255\255\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\255\255\255\255\
    \255\255\176\000\162\000\255\255\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\168\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\060\001\255\255\255\255\255\255\168\000\255\255\255\255\
    \255\255\051\001\255\255\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \051\001\255\255\255\255\255\255\255\255\255\255\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \051\001\255\255\060\001\255\255\168\000\255\255\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \169\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\255\255\255\255\255\255\255\255\169\000\
    \255\255\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\216\000\216\000\216\000\216\000\
    \216\000\216\000\060\001\255\255\220\000\255\255\220\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\220\000\220\000\
    \255\255\255\255\255\255\255\255\079\001\079\001\079\001\079\001\
    \255\255\255\255\255\255\220\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\220\000\070\001\070\001\070\001\070\001\070\001\
    \070\001\070\001\070\001\220\000\079\001\255\255\255\255\255\255\
    \255\255\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\255\255\255\255\220\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\220\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\220\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\223\000\223\000\223\000\
    \223\000\223\000\223\000\255\255\255\255\255\255\079\001\255\255\
    \223\000\255\255\255\255\070\001\255\255\255\255\255\255\255\255\
    \255\255\223\000\255\255\255\255\226\000\226\000\226\000\226\000\
    \226\000\081\001\255\255\255\255\255\255\223\000\223\000\223\000\
    \223\000\223\000\223\000\255\255\255\255\255\255\255\255\255\255\
    \223\000\255\255\255\255\226\000\226\000\226\000\226\000\226\000\
    \255\255\223\000\226\000\226\000\226\000\226\000\255\255\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\255\255\255\255\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\255\255\
    \226\000\255\255\226\000\255\255\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\228\000\
    \226\000\228\000\228\000\228\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\070\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\228\000\228\000\255\255\
    \255\255\081\001\255\255\255\255\255\255\228\000\255\255\255\255\
    \255\255\255\255\079\001\255\255\255\255\229\000\228\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\255\255\255\255\255\255\255\255\228\000\228\000\255\255\
    \255\255\255\255\255\255\229\000\229\000\228\000\255\255\255\255\
    \255\255\255\255\255\255\229\000\255\255\255\255\228\000\255\255\
    \255\255\255\255\255\255\255\255\229\000\255\255\255\255\229\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\229\000\229\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\229\000\255\255\255\255\255\255\233\000\
    \255\255\255\255\255\255\255\255\229\000\255\255\255\255\229\000\
    \255\255\255\255\255\255\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\255\255\233\000\
    \255\255\255\255\255\255\226\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\255\255\255\255\
    \255\255\255\255\233\000\255\255\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\234\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\255\255\234\000\255\255\
    \255\255\255\255\255\255\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\255\255\255\255\255\255\
    \015\001\234\000\255\255\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\255\255\
    \255\255\255\255\255\255\015\001\255\255\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\016\001\
    \255\255\255\255\015\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\255\255\255\255\255\255\
    \255\255\016\001\255\255\255\255\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\255\255\255\255\
    \255\255\255\255\016\001\255\255\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\016\001\016\001\
    \016\001\016\001\016\001\016\001\016\001\016\001\018\001\255\255\
    \255\255\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\255\255\255\255\255\255\
    \255\255\018\001\255\255\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\019\001\255\255\255\255\
    \018\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\255\255\019\001\255\255\255\255\
    \255\255\255\255\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\255\255\255\255\255\255\020\001\
    \019\001\255\255\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\255\255\255\255\
    \255\255\255\255\020\001\255\255\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\021\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\255\255\021\001\255\255\
    \255\255\255\255\255\255\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\255\255\255\255\255\255\
    \022\001\021\001\255\255\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\255\255\
    \255\255\255\255\255\255\022\001\255\255\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\025\001\
    \255\255\255\255\022\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\255\255\255\255\255\255\
    \255\255\025\001\255\255\255\255\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\255\255\255\255\
    \255\255\255\255\025\001\255\255\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\025\001\025\001\
    \025\001\025\001\025\001\025\001\025\001\025\001\028\001\255\255\
    \255\255\025\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\255\255\255\255\255\255\
    \255\255\028\001\255\255\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\029\001\255\255\255\255\
    \028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\255\255\255\255\255\255\255\255\029\001\
    \255\255\255\255\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\255\255\255\255\255\255\255\255\
    \029\001\255\255\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\031\001\255\255\255\255\029\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\255\255\255\255\255\255\255\255\031\001\
    \255\255\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\032\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\255\255\032\001\255\255\255\255\255\255\255\255\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\255\255\255\255\255\255\033\001\032\001\255\255\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\255\255\255\255\255\255\255\255\
    \033\001\255\255\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\034\001\255\255\255\255\033\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\255\255\255\255\255\255\255\255\034\001\255\255\
    \255\255\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\255\255\255\255\255\255\255\255\034\001\
    \255\255\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\035\001\255\255\255\255\034\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\255\255\255\255\255\255\255\255\035\001\255\255\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\036\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\255\255\036\001\255\255\255\255\255\255\255\255\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\255\255\255\255\255\255\038\001\036\001\255\255\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\036\001\036\001\036\001\036\001\036\001\036\001\036\001\
    \036\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\255\255\255\255\255\255\255\255\038\001\
    \255\255\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\039\001\255\255\255\255\038\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\255\255\255\255\255\255\255\255\039\001\255\255\255\255\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\255\255\255\255\255\255\255\255\039\001\255\255\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \039\001\039\001\041\001\255\255\255\255\039\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\255\255\255\255\255\255\255\255\041\001\255\255\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\042\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \255\255\042\001\255\255\255\255\255\255\255\255\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \255\255\255\255\255\255\043\001\042\001\255\255\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\255\255\255\255\255\255\255\255\043\001\255\255\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\044\001\255\255\255\255\043\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \255\255\255\255\255\255\255\255\044\001\255\255\255\255\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\255\255\255\255\255\255\255\255\044\001\255\255\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\044\001\044\001\044\001\044\001\044\001\044\001\044\001\
    \044\001\255\255\255\255\255\255\044\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\048\001\048\001\
    \048\001\048\001\048\001\048\001\255\255\255\255\052\001\255\255\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\052\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\052\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\052\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\052\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\052\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\052\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\055\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\055\001\
    \055\001\055\001\055\001\055\001\055\001\255\255\255\255\255\255\
    \255\255\255\255\055\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\055\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\055\001\
    \055\001\055\001\055\001\055\001\055\001\255\255\255\255\255\255\
    \255\255\255\255\055\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\055\001\062\001\062\001\062\001\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\062\001\062\001\062\001\
    \062\001\062\001\062\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\066\001\066\001\066\001\066\001\
    \066\001\066\001\255\255\255\255\255\255\062\001\062\001\062\001\
    \062\001\062\001\062\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\073\001\073\001\073\001\073\001\073\001\073\001\073\001\
    \073\001\073\001\073\001\255\255\066\001\066\001\066\001\066\001\
    \066\001\066\001\073\001\073\001\073\001\073\001\073\001\073\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\077\001\077\001\077\001\077\001\077\001\077\001\255\255\
    \255\255\255\255\073\001\073\001\073\001\073\001\073\001\073\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\085\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \255\255\077\001\077\001\077\001\077\001\077\001\077\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\089\001\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\089\001\089\001\
    \089\001\089\001\089\001\089\001\255\255\255\255\255\255\085\001\
    \085\001\085\001\085\001\085\001\085\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\089\001\089\001\
    \089\001\089\001\089\001\089\001\255\255\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \002\000\003\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \005\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000";
  Lexing.lex_backtrk_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000";
  Lexing.lex_default_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000";
  Lexing.lex_trans_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\010\000\010\000\023\000\023\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\001\000\010\000\010\000\023\000\023\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\018\000\007\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check_code = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\040\000\160\000\161\000\167\000\168\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\040\000\160\000\161\000\167\000\168\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\166\000\159\000\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_code = 
   "\255\002\255\255\000\002\255\003\255\255\004\255\255\000\003\001\
    \004\255\006\255\005\255\255\008\255\007\255\255\000\005\001\007\
    \255\000\006\001\008\255";
}

let rec token lexbuf =
  lexbuf.Lexing.lex_mem <- Array.create 9 (-1) ; (* L=1 [2] <- p ;  *)
  lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 547 "./lexer_cocci.mll"
    ( let cls = !current_line_started in
      if not cls
      then
	begin
	  match !current_line_type with
	    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	      (* increment the logical line even though nothing seen *)
	      start_line true;
	      let info = get_current_line_type lexbuf in
	      reset_line lexbuf;
	      TDirective (Ast.Noindent "", info)
	  | _ -> reset_line lexbuf; token lexbuf
	end
      else (reset_line lexbuf; token lexbuf) )
# 2686 "lexer_cocci.ml"

  | 1 ->
let
# 562 "./lexer_cocci.mll"
                      w
# 2692 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 562 "./lexer_cocci.mll"
                         ( (* collect whitespaces only when inside a rule *)
    start_line false;
    if !Data.in_rule_name || !Data.in_prolog || !Data.in_iso
    then token lexbuf
    else TWhitespace w )
# 2700 "lexer_cocci.ml"

  | 2 ->
let
# 568 "./lexer_cocci.mll"
                                       after
# 2706 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 568 "./lexer_cocci.mll"
                                              (
    match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	let str =
	  if !current_line_started
	  then (tok lexbuf)
	  else after in
	start_line true;
	TDirective (Ast.Indent str, get_current_line_type lexbuf)
    | _ -> start_line false; token lexbuf )
# 2719 "lexer_cocci.ml"

  | 3 ->
# 580 "./lexer_cocci.mll"
   ( match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	start_line true;
	TDirective (Ast.Space (tok lexbuf), get_current_line_type lexbuf)
    | _ -> failwith "attributes only allowed in + code" )
# 2728 "lexer_cocci.ml"

  | 4 ->
# 586 "./lexer_cocci.mll"
         ( start_line true; TArobArob )
# 2733 "lexer_cocci.ml"

  | 5 ->
# 587 "./lexer_cocci.mll"
         ( pass_zero();
	   if !Data.in_rule_name || not !current_line_started
	   then (start_line true; TArob)
	   else (check_minus_context_linetype "@";
		 TPArob (get_current_line_type lexbuf)) )
# 2742 "lexer_cocci.ml"

  | 6 ->
# 594 "./lexer_cocci.mll"
      ( start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen (get_current_line_type lexbuf) )
# 2748 "lexer_cocci.ml"

  | 7 ->
# 598 "./lexer_cocci.mll"
      ( start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis (get_current_line_type lexbuf) )
# 2754 "lexer_cocci.ml"

  | 8 ->
# 609 "./lexer_cocci.mll"
           ( start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis (get_current_line_type lexbuf) )
# 2760 "lexer_cocci.ml"

  | 9 ->
# 611 "./lexer_cocci.mll"
           ( start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis (get_current_line_type lexbuf) )
# 2766 "lexer_cocci.ml"

  | 10 ->
# 613 "./lexer_cocci.mll"
            ( start_line true; check_minus_context_linetype (tok lexbuf);
	     TPOEllipsis (get_current_line_type lexbuf) )
# 2772 "lexer_cocci.ml"

  | 11 ->
# 615 "./lexer_cocci.mll"
            ( start_line true; check_minus_context_linetype (tok lexbuf);
	     TPCEllipsis (get_current_line_type lexbuf) )
# 2778 "lexer_cocci.ml"

  | 12 ->
# 628 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TMinus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.MINUS; token lexbuf) )
# 2787 "lexer_cocci.ml"

  | 13 ->
# 633 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TPlus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.PLUS; token lexbuf) )
# 2796 "lexer_cocci.ml"

  | 14 ->
# 638 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TWhy (get_current_line_type lexbuf))
          else (add_current_line_type D.OPT; token lexbuf) )
# 2804 "lexer_cocci.ml"

  | 15 ->
# 642 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TBang (get_current_line_type lexbuf))
          else (add_current_line_type D.UNIQUE; token lexbuf) )
# 2812 "lexer_cocci.ml"

  | 16 ->
# 646 "./lexer_cocci.mll"
        ( if not !col_zero
	  then (start_line true; TOPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 ("(",get_current_line_type lexbuf)))
# 2821 "lexer_cocci.ml"

  | 17 ->
# 651 "./lexer_cocci.mll"
          ( start_line true;
	    TOPar0 ("\\(",contextify(get_current_line_type lexbuf)) )
# 2827 "lexer_cocci.ml"

  | 18 ->
# 653 "./lexer_cocci.mll"
        ( if not (!col_zero)
	  then (start_line true; TOr(get_current_line_type lexbuf))
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 ("|",get_current_line_type lexbuf)))
# 2836 "lexer_cocci.ml"

  | 19 ->
# 658 "./lexer_cocci.mll"
          ( start_line true;
	    TMid0 ("\\|",contextify(get_current_line_type lexbuf)) )
# 2842 "lexer_cocci.ml"

  | 20 ->
# 660 "./lexer_cocci.mll"
        ( if not !col_zero
	  then (start_line true; TCPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 (")",get_current_line_type lexbuf)))
# 2851 "lexer_cocci.ml"

  | 21 ->
# 665 "./lexer_cocci.mll"
          ( start_line true;
	    TCPar0 ("\\)",contextify(get_current_line_type lexbuf)) )
# 2857 "lexer_cocci.ml"

  | 22 ->
# 668 "./lexer_cocci.mll"
        ( start_line true; TOCro (get_current_line_type lexbuf)   )
# 2862 "lexer_cocci.ml"

  | 23 ->
# 669 "./lexer_cocci.mll"
        ( start_line true; TCCro (get_current_line_type lexbuf)   )
# 2867 "lexer_cocci.ml"

  | 24 ->
# 670 "./lexer_cocci.mll"
        ( start_line true; TOBrace (get_current_line_type lexbuf) )
# 2872 "lexer_cocci.ml"

  | 25 ->
# 671 "./lexer_cocci.mll"
        ( start_line true; TCBrace (get_current_line_type lexbuf) )
# 2877 "lexer_cocci.ml"

  | 26 ->
# 673 "./lexer_cocci.mll"
                   ( start_line true; TPtrOp (get_current_line_type lexbuf)  )
# 2882 "lexer_cocci.ml"

  | 27 ->
# 674 "./lexer_cocci.mll"
                   ( start_line true; TDot (get_current_line_type lexbuf)    )
# 2887 "lexer_cocci.ml"

  | 28 ->
# 675 "./lexer_cocci.mll"
                   ( start_line true; TComma (get_current_line_type lexbuf)  )
# 2892 "lexer_cocci.ml"

  | 29 ->
# 676 "./lexer_cocci.mll"
                        ( start_line true; TVAEllipsis (get_current_line_type lexbuf)  )
# 2897 "lexer_cocci.ml"

  | 30 ->
# 677 "./lexer_cocci.mll"
                   ( start_line true; TPtVirg (get_current_line_type lexbuf) )
# 2902 "lexer_cocci.ml"

  | 31 ->
# 680 "./lexer_cocci.mll"
                   ( pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS; token lexbuf) )
# 2913 "lexer_cocci.ml"

  | 32 ->
# 687 "./lexer_cocci.mll"
                   ( start_line true;
		     TDmOp (Ast.Div,get_current_line_type lexbuf) )
# 2919 "lexer_cocci.ml"

  | 33 ->
# 689 "./lexer_cocci.mll"
                    ( start_line true;
		     TDmOp (Ast.Min,get_current_line_type lexbuf) )
# 2925 "lexer_cocci.ml"

  | 34 ->
# 691 "./lexer_cocci.mll"
                    ( start_line true;
		     TDmOp (Ast.Max,get_current_line_type lexbuf) )
# 2931 "lexer_cocci.ml"

  | 35 ->
# 693 "./lexer_cocci.mll"
                   ( start_line true;
		     TDmOp (Ast.Mod,get_current_line_type lexbuf) )
# 2937 "lexer_cocci.ml"

  | 36 ->
# 695 "./lexer_cocci.mll"
                   ( start_line true;  TTilde (get_current_line_type lexbuf) )
# 2942 "lexer_cocci.ml"

  | 37 ->
# 697 "./lexer_cocci.mll"
                   ( pass_zero();
 		     if !current_line_started
 		     then
 		       (start_line true; TInc (get_current_line_type lexbuf))
 		     else (patch_or_match PATCH;
 			   add_current_line_type D.PLUSPLUS; token lexbuf) )
# 2952 "lexer_cocci.ml"

  | 38 ->
# 703 "./lexer_cocci.mll"
                   ( start_line true;  TDec (get_current_line_type lexbuf) )
# 2957 "lexer_cocci.ml"

  | 39 ->
# 705 "./lexer_cocci.mll"
                   ( start_line true; TEq (get_current_line_type lexbuf) )
# 2962 "lexer_cocci.ml"

  | 40 ->
# 707 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Minus lexbuf )
# 2967 "lexer_cocci.ml"

  | 41 ->
# 708 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Plus lexbuf )
# 2972 "lexer_cocci.ml"

  | 42 ->
# 710 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Mul lexbuf )
# 2977 "lexer_cocci.ml"

  | 43 ->
# 711 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Div lexbuf )
# 2982 "lexer_cocci.ml"

  | 44 ->
# 712 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Mod lexbuf )
# 2987 "lexer_cocci.ml"

  | 45 ->
# 714 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.And lexbuf )
# 2992 "lexer_cocci.ml"

  | 46 ->
# 715 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Or lexbuf )
# 2997 "lexer_cocci.ml"

  | 47 ->
# 716 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Xor lexbuf )
# 3002 "lexer_cocci.ml"

  | 48 ->
# 717 "./lexer_cocci.mll"
                    ( start_line true; mkassign Ast.Max lexbuf )
# 3007 "lexer_cocci.ml"

  | 49 ->
# 718 "./lexer_cocci.mll"
                    ( start_line true; mkassign Ast.Min lexbuf )
# 3012 "lexer_cocci.ml"

  | 50 ->
# 720 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.DecLeft lexbuf )
# 3017 "lexer_cocci.ml"

  | 51 ->
# 721 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.DecRight lexbuf )
# 3022 "lexer_cocci.ml"

  | 52 ->
# 723 "./lexer_cocci.mll"
                   ( start_line true; TDotDot (get_current_line_type lexbuf) )
# 3027 "lexer_cocci.ml"

  | 53 ->
# 725 "./lexer_cocci.mll"
                   ( start_line true; TEqEq    (get_current_line_type lexbuf) )
# 3032 "lexer_cocci.ml"

  | 54 ->
# 726 "./lexer_cocci.mll"
                   ( start_line true; TNotEq   (get_current_line_type lexbuf) )
# 3037 "lexer_cocci.ml"

  | 55 ->
# 727 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.SupEq,get_current_line_type lexbuf) )
# 3043 "lexer_cocci.ml"

  | 56 ->
# 729 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.InfEq,get_current_line_type lexbuf) )
# 3049 "lexer_cocci.ml"

  | 57 ->
# 731 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.Inf,get_current_line_type lexbuf) )
# 3055 "lexer_cocci.ml"

  | 58 ->
# 733 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.Sup,get_current_line_type lexbuf) )
# 3061 "lexer_cocci.ml"

  | 59 ->
# 736 "./lexer_cocci.mll"
                   ( start_line true; TAndLog (get_current_line_type lexbuf) )
# 3066 "lexer_cocci.ml"

  | 60 ->
# 737 "./lexer_cocci.mll"
                   ( start_line true; TOrLog  (get_current_line_type lexbuf) )
# 3071 "lexer_cocci.ml"

  | 61 ->
# 739 "./lexer_cocci.mll"
                   ( start_line true;
		     TShROp(Ast.DecRight,get_current_line_type lexbuf) )
# 3077 "lexer_cocci.ml"

  | 62 ->
# 741 "./lexer_cocci.mll"
                   ( start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) )
# 3083 "lexer_cocci.ml"

  | 63 ->
# 744 "./lexer_cocci.mll"
                   ( start_line true; TAnd    (get_current_line_type lexbuf) )
# 3088 "lexer_cocci.ml"

  | 64 ->
# 745 "./lexer_cocci.mll"
                   ( start_line true; TXor(get_current_line_type lexbuf) )
# 3093 "lexer_cocci.ml"

  | 65 ->
# 747 "./lexer_cocci.mll"
                    ( start_line true; TCppConcatOp )
# 3098 "lexer_cocci.ml"

  | 66 ->
let
# 748 "./lexer_cocci.mll"
                                               wss
# 3104 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 748 "./lexer_cocci.mll"
                                                        def
# 3109 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(1)
and
# 749 "./lexer_cocci.mll"
                                  ident
# 3114 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 750 "./lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TUndef
	  (lt,
	   check_var ident
	     (arity,line,lline,llend,offset+off,col+off,[],[],[],wss)) )
# 3126 "lexer_cocci.ml"

  | 67 ->
let
# 759 "./lexer_cocci.mll"
                                                  wss
# 3132 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 759 "./lexer_cocci.mll"
                                                            def
# 3137 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(1)
and
# 760 "./lexer_cocci.mll"
                                   ident
# 3142 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 761 "./lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TDefine
	  (lt,
	   check_var ident
	     (arity,line,lline,llend,offset+off,col+off,[],[],[],wss)) )
# 3154 "lexer_cocci.ml"

  | 68 ->
let
# 770 "./lexer_cocci.mll"
                                                  wss
# 3160 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 770 "./lexer_cocci.mll"
                                                            def
# 3165 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(1)
and
# 771 "./lexer_cocci.mll"
                                    ident
# 3170 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 773 "./lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	TDefineParam
        (lt,
	 check_var ident
	   (* why pos here but not above? *)
	   (arity,line,lline,llend,offset+off,col+off,strbef,straft,pos,wss),
	 offset + off + (String.length ident),
	 col + off + (String.length ident)) )
# 3184 "lexer_cocci.ml"

  | 69 ->
# 785 "./lexer_cocci.mll"
      ( start_line true; TPragma(get_current_line_type lexbuf) )
# 3189 "lexer_cocci.ml"

  | 70 ->
# 790 "./lexer_cocci.mll"
      ( TIncludeL
	  (let str = tok lexbuf in
	  let start = String.index str '\"' in
	  let finish = String.rindex str '\"' in
	  start_line true;
	  (process_include start finish str, get_current_line_type lexbuf)) )
# 3199 "lexer_cocci.ml"

  | 71 ->
# 797 "./lexer_cocci.mll"
      ( TIncludeNL
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) )
# 3209 "lexer_cocci.ml"

  | 72 ->
# 811 "./lexer_cocci.mll"
      ( start_line true; check_plus_linetype (tok lexbuf);
	TDirective (Ast.Noindent(tok lexbuf), get_current_line_type lexbuf) )
# 3215 "lexer_cocci.ml"

  | 73 ->
# 814 "./lexer_cocci.mll"
      (
       match !current_line_type with
        (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
        start_line true;
	(* second argument to TDirective is not quite right, because
	   it represents only the first token of the comment, but that
	   should be good enough *)
	TDirective (Ast.Indent("/*"^(comment check_comment lexbuf)),
		 get_current_line_type lexbuf)
      |	_ -> let _ = comment (fun _ -> ()) lexbuf in token lexbuf )
# 3229 "lexer_cocci.ml"

  | 74 ->
# 825 "./lexer_cocci.mll"
      ( (if !current_line_started
      then lexerr "--- must be at the beginning of the line" "");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) )
# 3240 "lexer_cocci.ml"

  | 75 ->
# 833 "./lexer_cocci.mll"
      ( (if !current_line_started
      then lexerr "+++ must be at the beginning of the line" "");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) )
# 3251 "lexer_cocci.ml"

  | 76 ->
# 842 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3256 "lexer_cocci.ml"

  | 77 ->
# 846 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3261 "lexer_cocci.ml"

  | 78 ->
# 853 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3273 "lexer_cocci.ml"

  | 79 ->
# 864 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3285 "lexer_cocci.ml"

  | 80 ->
# 880 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3297 "lexer_cocci.ml"

  | 81 ->
# 893 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3309 "lexer_cocci.ml"

  | 82 ->
# 904 "./lexer_cocci.mll"
        ( start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) )
# 3315 "lexer_cocci.ml"

  | 83 ->
# 906 "./lexer_cocci.mll"
         ( start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) )
# 3321 "lexer_cocci.ml"

  | 84 ->
let
# 908 "./lexer_cocci.mll"
             x
# 3327 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 908 "./lexer_cocci.mll"
                   ( start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) )
# 3332 "lexer_cocci.ml"

  | 85 ->
let
# 918 "./lexer_cocci.mll"
         x
# 3338 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 918 "./lexer_cocci.mll"
            ( start_line true; TInt(x,(get_current_line_type lexbuf)) )
# 3342 "lexer_cocci.ml"

  | 86 ->
let
# 920 "./lexer_cocci.mll"
                           x
# 3348 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 921 "./lexer_cocci.mll"
      ( if !Flag.ibm
      then
	begin
	  start_line true;
	  let len = string_of_int(String.length x - 1) in
          TDecimalCst(x,len,"0",(get_current_line_type lexbuf))
	end
      else failwith "unrecognized constant modifier d/D" )
# 3359 "lexer_cocci.ml"

  | 87 ->
# 930 "./lexer_cocci.mll"
                   ( TIso )
# 3364 "lexer_cocci.ml"

  | 88 ->
# 931 "./lexer_cocci.mll"
                   ( TRightIso )
# 3369 "lexer_cocci.ml"

  | 89 ->
# 933 "./lexer_cocci.mll"
                   ( EOF )
# 3374 "lexer_cocci.ml"

  | 90 ->
# 935 "./lexer_cocci.mll"
      ( lexerr "unrecognised symbol, in token rule: " (tok lexbuf) )
# 3379 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and metavariable_decl_token lexbuf =
    __ocaml_lex_metavariable_decl_token_rec lexbuf 226
and __ocaml_lex_metavariable_decl_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 940 "./lexer_cocci.mll"
    ( reset_line lexbuf; metavariable_decl_token lexbuf )
# 3390 "lexer_cocci.ml"

  | 1 ->
# 942 "./lexer_cocci.mll"
                  (
    start_line false; metavariable_decl_token lexbuf )
# 3396 "lexer_cocci.ml"

  | 2 ->
# 945 "./lexer_cocci.mll"
                                   (
    start_line false; metavariable_decl_token lexbuf )
# 3402 "lexer_cocci.ml"

  | 3 ->
# 948 "./lexer_cocci.mll"
         ( start_line true; TArobArob )
# 3407 "lexer_cocci.ml"

  | 4 ->
# 950 "./lexer_cocci.mll"
         ( start_line true; TTildeEq (get_current_line_type lexbuf) )
# 3412 "lexer_cocci.ml"

  | 5 ->
# 951 "./lexer_cocci.mll"
         ( start_line true; TTildeExclEq (get_current_line_type lexbuf) )
# 3417 "lexer_cocci.ml"

  | 6 ->
# 952 "./lexer_cocci.mll"
         ( start_line true; TEq (get_current_line_type lexbuf) )
# 3422 "lexer_cocci.ml"

  | 7 ->
# 953 "./lexer_cocci.mll"
        ( start_line true; TOPar (get_current_line_type lexbuf) )
# 3427 "lexer_cocci.ml"

  | 8 ->
# 954 "./lexer_cocci.mll"
        ( start_line true; TCPar (get_current_line_type lexbuf) )
# 3432 "lexer_cocci.ml"

  | 9 ->
# 956 "./lexer_cocci.mll"
        ( start_line true; TOCro (get_current_line_type lexbuf)   )
# 3437 "lexer_cocci.ml"

  | 10 ->
# 957 "./lexer_cocci.mll"
        ( start_line true; TCCro (get_current_line_type lexbuf)   )
# 3442 "lexer_cocci.ml"

  | 11 ->
# 958 "./lexer_cocci.mll"
        ( start_line true; TOBrace (get_current_line_type lexbuf) )
# 3447 "lexer_cocci.ml"

  | 12 ->
# 959 "./lexer_cocci.mll"
        ( start_line true; TCBrace (get_current_line_type lexbuf) )
# 3452 "lexer_cocci.ml"

  | 13 ->
# 961 "./lexer_cocci.mll"
                   ( start_line true; TPtrOp (get_current_line_type lexbuf)  )
# 3457 "lexer_cocci.ml"

  | 14 ->
# 962 "./lexer_cocci.mll"
                   ( start_line true; TDot (get_current_line_type lexbuf)    )
# 3462 "lexer_cocci.ml"

  | 15 ->
# 963 "./lexer_cocci.mll"
                   ( start_line true; TComma (get_current_line_type lexbuf)  )
# 3467 "lexer_cocci.ml"

  | 16 ->
# 964 "./lexer_cocci.mll"
                   ( start_line true;
		     TMPtVirg (* works better with tokens_all *) )
# 3473 "lexer_cocci.ml"

  | 17 ->
# 966 "./lexer_cocci.mll"
                   ( start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) )
# 3479 "lexer_cocci.ml"

  | 18 ->
# 969 "./lexer_cocci.mll"
                   ( pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS;
			metavariable_decl_token lexbuf) )
# 3491 "lexer_cocci.ml"

  | 19 ->
# 978 "./lexer_cocci.mll"
                   ( start_line true; TEqEq    (get_current_line_type lexbuf) )
# 3496 "lexer_cocci.ml"

  | 20 ->
# 979 "./lexer_cocci.mll"
                   ( start_line true; TNotEq   (get_current_line_type lexbuf) )
# 3501 "lexer_cocci.ml"

  | 21 ->
# 980 "./lexer_cocci.mll"
                   ( start_line true; TSub     (get_current_line_type lexbuf) )
# 3506 "lexer_cocci.ml"

  | 22 ->
# 982 "./lexer_cocci.mll"
      (match !current_line_type with
        (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
        start_line true;
	(* second argument to TDirective is not quite right, because
	   it represents only the first token of the comment, but that
	   should be good enough *)
	TDirective (Ast.Indent("/*"^(comment check_comment lexbuf)),
		 get_current_line_type lexbuf)
      |	_ -> let _ = comment (fun _ -> ()) lexbuf in
	     metavariable_decl_token lexbuf )
# 3520 "lexer_cocci.ml"

  | 23 ->
# 993 "./lexer_cocci.mll"
                    ( start_line true; TCppConcatOp (* for fresh vars *) )
# 3525 "lexer_cocci.ml"

  | 24 ->
# 996 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3530 "lexer_cocci.ml"

  | 25 ->
# 1000 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3535 "lexer_cocci.ml"

  | 26 ->
# 1007 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3546 "lexer_cocci.ml"

  | 27 ->
# 1017 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3557 "lexer_cocci.ml"

  | 28 ->
# 1032 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3568 "lexer_cocci.ml"

  | 29 ->
# 1044 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3579 "lexer_cocci.ml"

  | 30 ->
# 1054 "./lexer_cocci.mll"
        ( start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) )
# 3585 "lexer_cocci.ml"

  | 31 ->
# 1056 "./lexer_cocci.mll"
         ( start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) )
# 3591 "lexer_cocci.ml"

  | 32 ->
let
# 1058 "./lexer_cocci.mll"
             x
# 3597 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1058 "./lexer_cocci.mll"
                   ( Printf.printf "36\n"; start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) )
# 3602 "lexer_cocci.ml"

  | 33 ->
let
# 1068 "./lexer_cocci.mll"
         x
# 3608 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1068 "./lexer_cocci.mll"
            ( start_line true; TInt(x,(get_current_line_type lexbuf)) )
# 3612 "lexer_cocci.ml"

  | 34 ->
let
# 1070 "./lexer_cocci.mll"
                           x
# 3618 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1071 "./lexer_cocci.mll"
      ( if !Flag.ibm
      then
	begin
	  start_line true;
	  let len = string_of_int(String.length x - 1) in
          TDecimalCst(x,len,"0",(get_current_line_type lexbuf))
	end
      else failwith "unrecognized constant modifier d/D" )
# 3629 "lexer_cocci.ml"

  | 35 ->
# 1080 "./lexer_cocci.mll"
      ( lexerr "metavariables: unrecognised symbol in metavariable_decl_token rule: "
	  (tok lexbuf) )
# 3635 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_metavariable_decl_token_rec lexbuf __ocaml_lex_state

and char lexbuf =
    __ocaml_lex_char_rec lexbuf 314
and __ocaml_lex_char_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 1085 "./lexer_cocci.mll"
          x
# 3647 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1085 "./lexer_cocci.mll"
                                       ( String.make 1 x ^ restchars lexbuf )
# 3651 "lexer_cocci.ml"

  | 1 ->
let
# 1087 "./lexer_cocci.mll"
                                             x
# 3657 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1087 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3661 "lexer_cocci.ml"

  | 2 ->
let
# 1091 "./lexer_cocci.mll"
                                  x
# 3667 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1091 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3671 "lexer_cocci.ml"

  | 3 ->
let
# 1092 "./lexer_cocci.mll"
                 v
# 3677 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1092 "./lexer_cocci.mll"
                                  x
# 3682 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1093 "./lexer_cocci.mll"
 (
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x ^ restchars lexbuf
	)
# 3696 "lexer_cocci.ml"

  | 4 ->
# 1105 "./lexer_cocci.mll"
      ( Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      )
# 3703 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_char_rec lexbuf __ocaml_lex_state

and restchars lexbuf =
    __ocaml_lex_restchars_rec lexbuf 324
and __ocaml_lex_restchars_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1110 "./lexer_cocci.mll"
                                       ( "" )
# 3714 "lexer_cocci.ml"

  | 1 ->
let
# 1111 "./lexer_cocci.mll"
          x
# 3720 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1111 "./lexer_cocci.mll"
                                       ( String.make 1 x ^ restchars lexbuf )
# 3724 "lexer_cocci.ml"

  | 2 ->
let
# 1113 "./lexer_cocci.mll"
                                             x
# 3730 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1113 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3734 "lexer_cocci.ml"

  | 3 ->
let
# 1117 "./lexer_cocci.mll"
                                  x
# 3740 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1117 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3744 "lexer_cocci.ml"

  | 4 ->
let
# 1118 "./lexer_cocci.mll"
                 v
# 3750 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1118 "./lexer_cocci.mll"
                                  x
# 3755 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1119 "./lexer_cocci.mll"
 (
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x ^ restchars lexbuf
	)
# 3769 "lexer_cocci.ml"

  | 5 ->
# 1131 "./lexer_cocci.mll"
      ( Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      )
# 3776 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_restchars_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 335
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1136 "./lexer_cocci.mll"
                                               ( "" )
# 3787 "lexer_cocci.ml"

  | 1 ->
let
# 1137 "./lexer_cocci.mll"
                                 x
# 3793 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1138 "./lexer_cocci.mll"
    ( line := !line + 1; (Printf.sprintf "%c" x) ^ string lexbuf )
# 3797 "lexer_cocci.ml"

  | 2 ->
let
# 1139 "./lexer_cocci.mll"
          x
# 3803 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1139 "./lexer_cocci.mll"
                               ( Common.string_of_char x ^ string lexbuf )
# 3807 "lexer_cocci.ml"

  | 3 ->
let
# 1140 "./lexer_cocci.mll"
                                            x
# 3813 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1140 "./lexer_cocci.mll"
                                              ( x ^ string lexbuf )
# 3817 "lexer_cocci.ml"

  | 4 ->
let
# 1141 "./lexer_cocci.mll"
                               x
# 3823 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1141 "./lexer_cocci.mll"
                                              ( x ^ string lexbuf )
# 3827 "lexer_cocci.ml"

  | 5 ->
let
# 1142 "./lexer_cocci.mll"
                v
# 3833 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1142 "./lexer_cocci.mll"
                       x
# 3838 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1143 "./lexer_cocci.mll"
       (
         (match v with
	    | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
	    | 'f' -> () | 'a' -> ()
	    | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
	    | 'e' -> ()
	    | '\n' -> ()
	    | '(' -> () | '|' -> () | ')' -> ()
	    | _ -> lexerr "unrecognised symbol:" (tok lexbuf)
	 );
          x ^ string lexbuf
       )
# 3853 "lexer_cocci.ml"

  | 6 ->
# 1155 "./lexer_cocci.mll"
      ( lexerr "unrecognised symbol: " (tok lexbuf) )
# 3858 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and comment check_comment lexbuf =
    __ocaml_lex_comment_rec check_comment lexbuf 347
and __ocaml_lex_comment_rec check_comment lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1158 "./lexer_cocci.mll"
         ( let s = tok lexbuf in check_comment s; start_line true; s )
# 3869 "lexer_cocci.ml"

  | 1 ->
# 1160 "./lexer_cocci.mll"
      ( let s = tok lexbuf in
        (* even blank line should have a + *)
        check_comment s;
        reset_line lexbuf; s ^ comment check_comment lexbuf )
# 3877 "lexer_cocci.ml"

  | 2 ->
# 1164 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true;
		let s = tok lexbuf in s^(comment check_comment lexbuf))
	  else (start_line true; comment check_comment lexbuf) )
# 3886 "lexer_cocci.ml"

  | 3 ->
# 1171 "./lexer_cocci.mll"
      ( let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf )
# 3892 "lexer_cocci.ml"

  | 4 ->
# 1174 "./lexer_cocci.mll"
      ( let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf )
# 3898 "lexer_cocci.ml"

  | 5 ->
# 1177 "./lexer_cocci.mll"
      ( start_line true; let s = tok lexbuf in
        Common.pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment check_comment lexbuf
      )
# 3906 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec check_comment lexbuf __ocaml_lex_state

;;

