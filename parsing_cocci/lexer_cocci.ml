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

# 521 "lexer_cocci.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\166\255\167\255\081\000\119\000\173\255\174\255\192\000\
    \023\001\052\001\038\000\067\000\101\000\079\000\220\255\081\000\
    \082\000\085\000\226\255\227\255\230\255\231\255\232\255\233\255\
    \235\255\086\000\111\000\239\255\087\000\241\255\116\000\118\000\
    \140\000\122\001\144\001\231\001\089\000\062\002\150\000\255\255\
    \176\002\107\000\146\000\183\255\213\255\097\000\159\002\246\002\
    \077\003\164\003\251\003\082\004\169\004\000\005\087\005\174\005\
    \005\006\092\006\206\006\118\000\176\000\119\000\149\000\150\000\
    \037\007\103\000\124\007\211\007\013\008\100\008\158\008\106\000\
    \052\000\245\008\126\000\135\000\076\009\163\009\136\000\250\009\
    \081\010\139\010\226\010\251\255\057\011\144\011\231\011\062\012\
    \149\012\134\001\150\000\156\000\140\000\246\255\244\255\148\002\
    \220\001\051\002\143\000\145\000\148\000\166\000\167\000\168\000\
    \247\255\169\000\171\000\245\255\207\255\206\255\169\255\216\255\
    \176\000\229\255\022\001\215\255\179\000\153\001\202\255\234\255\
    \236\255\238\255\196\255\210\255\214\255\201\255\162\000\163\000\
    \208\255\205\255\212\255\168\255\203\255\197\255\211\255\209\255\
    \207\012\038\013\167\000\096\013\183\013\168\000\014\014\101\014\
    \159\014\246\014\124\000\123\000\128\000\118\000\133\000\141\000\
    \122\006\191\255\153\000\153\000\153\000\053\001\104\015\179\015\
    \181\000\179\000\175\000\185\000\054\001\037\016\120\016\188\255\
    \219\000\217\000\212\000\225\000\187\255\017\015\224\000\216\000\
    \208\000\226\000\237\000\209\006\225\002\121\001\124\001\186\255\
    \226\002\185\255\155\001\115\002\116\002\203\002\156\001\206\002\
    \208\002\027\002\000\001\015\001\046\001\014\001\016\001\028\002\
    \201\002\042\001\046\001\202\002\043\001\042\001\204\002\049\001\
    \061\001\209\002\170\255\038\015\129\001\195\016\241\003\119\007\
    \068\009\222\016\071\004\158\004\036\017\130\001\171\255\115\017\
    \217\255\193\017\231\017\221\255\222\255\109\001\051\018\138\018\
    \146\001\234\255\147\001\236\255\237\255\245\004\174\001\240\255\
    \241\255\242\255\243\255\244\255\245\255\247\255\248\255\129\002\
    \160\002\176\001\248\001\255\255\109\015\245\001\210\002\230\255\
    \252\255\233\255\251\255\232\255\250\255\239\255\077\005\245\009\
    \163\005\250\005\231\255\235\255\229\255\008\002\196\018\027\019\
    \009\002\114\019\201\019\003\020\090\020\148\020\010\002\199\001\
    \235\020\013\002\051\002\066\021\153\021\065\002\240\021\071\022\
    \129\022\216\022\047\023\134\023\096\002\192\023\023\024\100\002\
    \110\024\197\024\255\024\086\025\218\255\049\015\232\002\165\025\
    \027\007\009\014\027\016\192\025\129\007\089\008\006\026\006\003\
    \219\255\117\002\255\255\010\016\252\255\076\026\084\006\203\007\
    \254\255\099\026\253\255\228\005\254\255\251\016\255\255\251\255\
    \137\026\237\008\078\009\253\255\160\026\252\255\018\017\253\255\
    \009\017\254\255\255\255\250\255\198\026\155\009\255\009\252\255\
    \221\026\251\255\108\011\252\255\253\255\254\255\145\002\255\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\084\000\084\000\255\255\255\255\076\000\
    \075\000\089\000\051\000\063\000\062\000\038\000\255\255\034\000\
    \057\000\030\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\018\000\089\000\255\255\015\000\255\255\013\000\012\000\
    \056\000\027\000\075\000\075\000\005\000\075\000\031\000\255\255\
    \001\000\255\255\002\000\255\255\255\255\255\255\255\255\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\255\255\255\255\255\255\255\255\003\000\
    \255\255\078\000\255\255\079\000\255\255\077\000\255\255\255\255\
    \255\255\255\255\077\000\255\255\255\255\255\255\079\000\255\255\
    \079\000\255\255\255\255\255\255\075\000\075\000\006\000\075\000\
    \075\000\083\000\255\255\007\000\255\255\255\255\255\255\255\255\
    \083\000\255\255\055\000\061\000\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \037\000\255\255\073\000\255\255\036\000\074\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\060\000\033\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\080\000\255\255\255\255\255\255\080\000\255\255\080\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\065\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\066\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\071\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\255\255\255\255\255\255\255\255\255\255\071\000\
    \071\000\255\255\255\255\071\000\255\255\255\255\071\000\255\255\
    \255\255\071\000\255\255\084\000\084\000\255\255\083\000\255\255\
    \255\255\084\000\083\000\255\255\084\000\084\000\255\255\255\255\
    \255\255\036\000\036\000\255\255\255\255\038\000\028\000\027\000\
    \038\000\255\255\038\000\255\255\255\255\017\000\038\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\009\000\
    \006\000\038\000\038\000\255\255\001\000\255\255\002\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\035\000\255\255\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \030\000\255\255\031\000\255\255\029\000\255\255\255\255\255\255\
    \255\255\029\000\255\255\255\255\255\255\031\000\255\255\031\000\
    \255\255\255\255\255\255\032\000\255\255\255\255\255\255\032\000\
    \255\255\032\000\255\255\255\255\255\255\036\000\036\000\255\255\
    \035\000\255\255\255\255\036\000\035\000\255\255\036\000\036\000\
    \255\255\255\255\255\255\000\000\255\255\003\000\001\000\001\000\
    \255\255\002\000\255\255\255\255\255\255\001\000\255\255\255\255\
    \004\000\002\000\002\000\255\255\003\000\255\255\255\255\255\255\
    \002\000\255\255\255\255\255\255\005\000\003\000\003\000\255\255\
    \004\000\255\255\255\255\255\255\255\255\255\255\004\000\255\255\
    ";
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
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\114\000\000\000\255\255\117\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\000\000\186\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\184\000\182\000\182\000\000\000\
    \184\000\000\000\186\000\186\000\186\000\186\000\190\000\186\000\
    \186\000\193\000\255\255\255\255\255\255\255\255\255\255\199\000\
    \200\000\255\255\255\255\203\000\255\255\255\255\206\000\255\255\
    \255\255\209\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\224\000\
    \000\000\255\255\255\255\000\000\000\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\000\000\000\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\254\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\058\001\000\000\060\001\000\000\255\255\255\255\255\255\
    \000\000\255\255\000\000\068\001\000\000\071\001\000\000\000\000\
    \255\255\255\255\255\255\000\000\255\255\000\000\079\001\000\000\
    \083\001\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \255\255\000\000\091\001\000\000\000\000\000\000\255\255\000\000\
    ";
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
    \136\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\034\000\
    \008\000\008\000\008\000\021\000\025\000\020\000\014\000\214\000\
    \135\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\133\000\132\000\131\000\130\000\125\000\
    \126\000\127\000\124\000\123\000\118\000\210\000\215\000\121\000\
    \119\000\083\000\042\000\066\000\255\255\211\000\061\000\116\000\
    \062\000\045\000\134\000\112\000\072\000\214\000\212\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\216\000\
    \216\000\115\000\068\000\111\000\113\000\210\000\215\000\101\000\
    \071\000\060\000\102\000\210\000\215\000\211\000\063\000\063\000\
    \043\000\079\000\075\000\211\000\091\000\042\000\212\000\092\000\
    \099\000\098\000\094\000\100\000\212\000\110\000\109\000\213\000\
    \060\000\108\000\122\000\044\000\105\000\103\000\104\000\106\000\
    \059\000\107\000\093\000\210\000\215\000\114\000\117\000\129\000\
    \128\000\142\000\138\000\211\000\007\000\207\000\173\000\196\000\
    \168\000\195\000\160\000\120\000\212\000\194\000\174\000\213\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\045\000\154\000\046\000\155\000\156\000\157\000\
    \002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\161\000\162\000\163\000\164\000\007\000\
    \255\255\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\169\000\152\000\158\000\165\000\
    \170\000\171\000\172\000\175\000\176\000\177\000\178\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\179\000\046\000\152\000\158\000\165\000\153\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\204\000\201\000\200\000\199\000\008\000\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\255\255\202\000\203\000\255\255\255\255\197\000\
    \150\000\147\000\205\000\255\255\206\000\148\000\183\000\208\000\
    \146\000\198\000\209\000\255\255\149\000\255\255\255\255\034\001\
    \090\000\151\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\007\000\012\001\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\045\000\095\000\046\000\221\000\222\000\011\001\
    \010\001\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\095\000\005\001\221\000\222\000\008\000\
    \000\001\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \087\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\007\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\255\255\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\255\000\046\000\254\000\255\255\255\255\254\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\084\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\017\001\013\001\023\001\019\001\008\000\022\001\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\007\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\030\001\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\255\255\046\000\026\001\255\255\255\255\255\255\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\255\255\040\001\255\255\255\255\047\000\036\001\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\040\000\039\000\039\000\039\000\039\000\003\001\097\000\
    \095\001\097\000\000\000\064\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\000\000\000\000\
    \040\000\059\001\000\000\255\255\255\255\255\255\255\255\191\000\
    \255\255\189\000\255\255\255\255\255\255\001\001\000\000\041\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\000\000\000\000\000\000\000\000\064\000\004\001\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\007\000\255\255\255\255\064\000\002\001\255\255\
    \185\000\000\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\190\000\046\000\192\000\055\001\000\000\193\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\056\001\000\000\055\001\008\000\000\000\048\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\007\000\056\001\255\255\255\255\255\255\000\000\000\000\
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
    \007\000\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
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
    \000\000\214\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \214\000\214\000\214\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\045\000\095\000\046\000\
    \000\000\000\000\000\000\000\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\095\000\000\000\
    \000\000\000\000\008\000\000\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\051\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\007\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\045\000\000\000\046\000\000\000\
    \000\000\000\000\000\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\000\000\000\000\000\000\
    \000\000\008\000\000\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\052\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\007\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\045\000\000\000\046\000\000\000\000\000\
    \000\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\008\000\053\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\007\000\006\001\006\001\006\001\
    \006\001\006\001\006\001\006\001\006\001\006\001\006\001\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\045\000\000\000\046\000\000\000\000\000\000\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\000\000\000\000\000\000\000\000\008\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\054\000\008\000\008\000\
    \008\000\008\000\008\000\007\000\000\000\006\001\006\001\006\001\
    \006\001\006\001\006\001\006\001\006\001\006\001\006\001\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\045\000\007\001\046\000\000\000\000\000\000\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\007\001\000\000\000\000\000\000\008\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\055\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\007\000\008\001\008\001\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\008\001\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\000\000\046\000\000\000\000\000\000\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\070\001\000\000\008\000\000\000\008\000\
    \008\000\008\000\008\000\056\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\007\000\008\001\008\001\008\001\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\045\000\
    \069\001\046\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\000\000\057\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \007\000\000\000\000\000\152\000\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\045\000\000\000\
    \046\000\000\000\152\000\000\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000\
    \000\000\000\000\000\000\058\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\060\000\
    \000\000\000\000\179\000\000\000\000\000\000\000\150\000\147\000\
    \000\000\000\000\000\000\148\000\255\255\000\000\146\000\000\000\
    \000\000\000\000\149\000\000\000\000\000\000\000\060\000\151\000\
    \000\000\179\000\007\000\181\000\000\000\000\000\059\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \045\000\000\000\046\000\000\000\000\000\180\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\000\000\008\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\064\000\000\000\048\001\048\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\000\000\
    \007\001\000\000\000\000\065\000\000\000\000\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \007\001\000\000\000\000\000\000\064\000\000\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \067\000\000\000\219\000\064\000\219\000\000\000\000\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\000\000\000\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\000\000\
    \000\000\000\000\000\000\067\000\000\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \000\000\000\000\068\000\064\001\064\001\064\001\064\001\064\001\
    \064\001\064\001\064\001\067\000\067\000\067\000\067\000\067\000\
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
    \069\000\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\000\000\069\000\069\000\069\000\069\000\
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
    \073\000\073\000\000\000\000\000\073\000\074\001\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\000\000\
    \000\000\000\000\000\000\074\000\000\000\000\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \000\000\000\000\000\000\000\000\073\000\000\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \077\000\000\000\214\000\073\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\075\001\075\001\
    \075\001\075\001\075\001\075\001\075\001\075\001\000\000\000\000\
    \000\000\215\000\000\000\000\000\000\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\000\000\
    \000\000\215\000\000\000\077\000\000\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \000\000\000\000\077\000\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\000\000\000\000\000\000\
    \000\000\078\000\000\000\000\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\000\000\000\000\
    \000\000\000\000\077\000\000\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\080\000\000\000\
    \009\001\077\000\009\001\000\000\000\000\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\008\001\008\001\008\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\000\000\
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
    \008\000\008\000\008\000\045\000\000\000\046\000\093\001\093\001\
    \093\001\093\001\008\000\008\000\008\000\008\000\085\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\000\000\000\000\094\001\092\001\
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
    \000\000\000\000\000\000\137\000\008\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\086\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\000\000\000\000\000\000\000\000\137\000\000\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \138\000\000\000\139\000\000\000\000\000\000\000\000\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\000\000\000\000\000\000\140\000\137\000\000\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\000\000\000\000\000\000\000\000\140\000\
    \000\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\000\000\000\000\140\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\000\000\000\000\000\000\000\000\141\000\000\000\000\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\000\000\000\000\000\000\000\000\140\000\000\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\143\000\000\000\053\001\140\000\053\001\000\000\
    \000\000\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\000\000\000\000\000\000\000\000\143\000\000\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\138\000\
    \000\000\144\000\000\000\000\000\000\000\000\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \000\000\000\000\000\000\145\000\143\000\000\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\000\000\000\000\000\000\000\000\145\000\000\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\255\255\000\000\145\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \000\000\000\000\000\000\000\000\141\000\000\000\000\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\000\000\000\000\000\000\000\000\145\000\000\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\158\000\222\000\000\000\145\000\188\000\252\000\251\000\
    \251\000\251\000\251\000\222\000\000\000\056\001\000\000\187\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\056\001\000\000\
    \158\000\000\000\000\000\000\000\000\000\252\000\000\000\000\000\
    \000\000\000\000\222\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\222\000\253\000\056\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\056\001\000\000\
    \000\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\000\000\000\000\000\000\000\000\159\000\
    \000\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\000\000\000\000\
    \000\000\255\255\159\000\000\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\165\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\062\001\000\000\000\000\000\000\165\000\000\000\000\000\
    \000\000\048\001\000\000\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \049\001\000\000\000\000\000\000\000\000\000\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \049\001\000\000\061\001\000\000\166\000\000\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \167\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\000\000\000\000\000\000\000\000\166\000\
    \000\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\220\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\255\255\000\000\214\000\000\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\216\000\216\000\
    \000\000\000\000\000\000\000\000\081\001\081\001\081\001\081\001\
    \000\000\000\000\000\000\215\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\211\000\073\001\073\001\073\001\073\001\073\001\
    \073\001\073\001\073\001\212\000\082\001\000\000\000\000\000\000\
    \000\000\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\000\000\000\000\215\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\211\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\212\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\220\000\220\000\220\000\
    \220\000\220\000\220\000\000\000\000\000\000\000\080\001\000\000\
    \211\000\000\000\000\000\072\001\000\000\000\000\000\000\000\000\
    \000\000\212\000\000\000\000\000\252\000\251\000\251\000\251\000\
    \251\000\084\001\000\000\000\000\000\000\220\000\220\000\220\000\
    \220\000\220\000\220\000\000\000\000\000\000\000\000\000\000\000\
    \211\000\000\000\000\000\252\000\247\000\227\000\232\000\230\000\
    \000\000\212\000\228\000\244\000\243\000\233\000\246\000\236\000\
    \238\000\237\000\250\000\226\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\229\000\235\000\234\000\
    \248\000\000\000\245\000\249\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\242\000\000\000\
    \241\000\000\000\231\000\000\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\240\000\048\001\
    \239\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\044\001\049\001\000\000\
    \000\000\255\255\000\000\000\000\000\000\045\001\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\048\001\046\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\050\001\
    \050\001\000\000\000\000\000\000\000\000\044\001\049\001\000\000\
    \000\000\000\000\000\000\044\001\049\001\045\001\000\000\000\000\
    \000\000\000\000\000\000\045\001\000\000\000\000\046\001\000\000\
    \000\000\000\000\000\000\000\000\046\001\000\000\000\000\047\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\044\001\049\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\045\001\000\000\000\000\000\000\230\000\
    \000\000\000\000\000\000\000\000\046\001\000\000\000\000\047\001\
    \000\000\000\000\000\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\013\001\000\000\014\001\
    \000\000\000\000\000\000\255\255\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\000\000\000\000\
    \000\000\000\000\230\000\000\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\013\001\000\000\014\001\000\000\
    \000\000\000\000\000\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\000\000\000\000\000\000\
    \015\001\231\000\000\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\000\000\
    \000\000\000\000\000\000\015\001\000\000\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \000\000\000\000\015\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\000\000\000\000\000\000\
    \000\000\016\001\000\000\000\000\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\000\000\000\000\
    \000\000\000\000\015\001\000\000\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\018\001\000\000\
    \000\000\015\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\000\000\000\000\000\000\
    \000\000\018\001\000\000\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\000\000\000\000\
    \019\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\026\001\000\000\027\001\000\000\000\000\
    \000\000\000\000\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\000\000\000\000\000\000\020\001\
    \018\001\000\000\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\000\000\000\000\
    \000\000\000\000\020\001\000\000\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\022\001\000\000\021\001\000\000\
    \000\000\000\000\000\000\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\000\000\000\000\000\000\
    \024\001\020\001\000\000\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\000\000\
    \000\000\000\000\000\000\024\001\000\000\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \000\000\000\000\024\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\000\000\000\000\000\000\
    \000\000\025\001\000\000\000\000\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\000\000\000\000\
    \000\000\000\000\024\001\000\000\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\028\001\000\000\
    \000\000\024\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \028\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\000\000\000\000\000\000\000\000\029\001\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \028\001\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\031\001\000\000\000\000\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\000\000\000\000\000\000\000\000\031\001\
    \000\000\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\026\001\000\000\032\001\000\000\000\000\000\000\000\000\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\000\000\000\000\000\000\033\001\031\001\000\000\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\000\000\000\000\000\000\000\000\
    \033\001\000\000\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\000\000\000\000\033\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\000\000\000\000\000\000\000\000\029\001\000\000\
    \000\000\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\000\000\000\000\000\000\000\000\033\001\
    \000\000\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\035\001\000\000\000\000\033\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\000\000\000\000\000\000\000\000\035\001\000\000\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \036\001\000\000\037\001\000\000\000\000\000\000\000\000\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\000\000\000\000\000\000\038\001\035\001\000\000\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\000\000\000\000\000\000\000\000\038\001\
    \000\000\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\000\000\000\000\038\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\000\000\000\000\000\000\000\000\039\001\000\000\000\000\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\000\000\000\000\000\000\000\000\038\001\000\000\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\041\001\000\000\000\000\038\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\000\000\000\000\000\000\000\000\041\001\000\000\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\036\001\
    \000\000\042\001\000\000\000\000\000\000\000\000\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \000\000\000\000\000\000\043\001\041\001\000\000\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\000\000\000\000\000\000\000\000\043\001\000\000\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\000\000\000\000\043\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \000\000\000\000\000\000\000\000\039\001\000\000\000\000\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\000\000\000\000\000\000\000\000\043\001\000\000\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\000\000\000\000\000\000\043\001\054\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\054\001\054\001\
    \054\001\054\001\054\001\054\001\000\000\000\000\048\001\000\000\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\051\001\
    \050\001\050\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\049\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\045\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\046\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\049\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\045\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\046\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\001\
    \054\001\054\001\054\001\054\001\054\001\000\000\000\000\000\000\
    \000\000\000\000\045\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\046\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\001\
    \054\001\054\001\054\001\054\001\054\001\000\000\000\000\000\000\
    \000\000\000\000\045\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\046\001\065\001\065\001\065\001\065\001\
    \065\001\065\001\065\001\065\001\065\001\065\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\065\001\065\001\065\001\
    \065\001\065\001\065\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\066\001\066\001\066\001\066\001\
    \066\001\066\001\000\000\000\000\000\000\065\001\065\001\065\001\
    \065\001\065\001\065\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\076\001\076\001\076\001\076\001\076\001\076\001\076\001\
    \076\001\076\001\076\001\000\000\066\001\066\001\066\001\066\001\
    \066\001\066\001\076\001\076\001\076\001\076\001\076\001\076\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\077\001\077\001\077\001\077\001\077\001\077\001\000\000\
    \000\000\000\000\076\001\076\001\076\001\076\001\076\001\076\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\088\001\
    \000\000\077\001\077\001\077\001\077\001\077\001\077\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\089\001\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\089\001\089\001\
    \089\001\089\001\089\001\089\001\000\000\000\000\000\000\088\001\
    \088\001\088\001\088\001\088\001\088\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\089\001\089\001\
    \089\001\089\001\089\001\089\001\000\000\000\000\000\000\000\000\
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
    \032\000\032\000\092\000\032\000\004\000\098\000\099\000\004\000\
    \060\000\100\000\025\000\038\000\101\000\102\000\103\000\105\000\
    \060\000\106\000\091\000\004\000\004\000\112\000\116\000\126\000\
    \127\000\138\000\141\000\004\000\007\000\146\000\148\000\147\000\
    \149\000\147\000\150\000\026\000\004\000\147\000\148\000\004\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\151\000\007\000\154\000\155\000\156\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\160\000\161\000\162\000\163\000\007\000\
    \114\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\008\000\168\000\009\000\157\000\164\000\
    \169\000\170\000\171\000\174\000\175\000\176\000\177\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\178\000\008\000\009\000\157\000\164\000\009\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\194\000\195\000\197\000\198\000\008\000\061\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\042\000\201\000\202\000\062\000\063\000\196\000\
    \009\000\009\000\204\000\181\000\205\000\009\000\182\000\207\000\
    \009\000\196\000\208\000\117\000\009\000\186\000\190\000\229\000\
    \033\000\009\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\034\000\232\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\089\000\034\000\212\000\221\000\234\000\
    \234\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\089\000\238\000\212\000\221\000\034\000\
    \249\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\114\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\250\000\035\000\253\000\193\000\199\000\250\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\013\001\016\001\022\001\023\001\035\000\025\001\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\037\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\026\001\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\181\000\037\000\029\001\182\000\187\000\188\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\117\000\036\001\186\000\190\000\037\000\039\001\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\040\000\040\000\040\000\040\000\040\000\247\000\095\000\
    \094\001\095\000\255\255\046\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\255\255\255\255\
    \040\000\057\001\255\255\200\000\203\000\189\000\206\000\187\000\
    \191\000\188\000\192\000\209\000\254\000\248\000\255\255\040\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\255\255\255\255\255\255\255\255\046\000\247\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\047\000\193\000\199\000\046\000\248\000\180\000\
    \184\000\255\255\255\255\255\255\255\255\255\255\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\189\000\047\000\191\000\046\001\255\255\192\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\255\255\055\001\255\255\046\001\047\000\255\255\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\048\000\055\001\187\000\188\000\057\001\255\255\255\255\
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
    \049\000\200\000\203\000\189\000\206\000\255\255\191\000\255\255\
    \192\000\209\000\254\000\255\255\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\255\255\
    \049\000\180\000\184\000\255\255\255\255\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\255\255\
    \255\255\255\255\255\255\049\000\255\255\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\050\000\
    \255\255\214\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \214\000\214\000\214\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\214\000\050\000\
    \255\255\255\255\255\255\255\255\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\214\000\255\255\
    \255\255\255\255\050\000\255\255\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\051\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\255\255\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\255\255\051\000\255\255\
    \255\255\255\255\255\255\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\255\255\255\255\255\255\
    \255\255\051\000\255\255\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\052\000\219\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \255\255\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\255\255\052\000\255\255\255\255\
    \255\255\255\255\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\255\255\255\255\255\255\255\255\
    \052\000\255\255\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\053\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\255\255\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\255\255\053\000\255\255\255\255\255\255\
    \255\255\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\255\255\255\255\255\255\255\255\053\000\
    \255\255\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\054\000\255\255\006\001\006\001\006\001\
    \006\001\006\001\006\001\006\001\006\001\006\001\006\001\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\006\001\054\000\255\255\255\255\255\255\255\255\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\006\001\255\255\255\255\255\255\054\000\255\255\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\055\000\008\001\008\001\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\008\001\255\255\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\255\255\055\000\255\255\255\255\255\255\255\255\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\255\255\255\255\067\001\255\255\055\000\255\255\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\056\000\009\001\009\001\009\001\009\001\009\001\009\001\
    \009\001\009\001\009\001\009\001\255\255\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \067\001\056\000\255\255\255\255\255\255\255\255\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \255\255\255\255\255\255\255\255\056\000\255\255\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \057\000\255\255\255\255\152\000\062\001\062\001\062\001\062\001\
    \062\001\062\001\062\001\062\001\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\255\255\
    \057\000\255\255\152\000\255\255\255\255\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\255\255\
    \255\255\255\255\255\255\057\000\255\255\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\058\000\
    \255\255\255\255\179\000\255\255\255\255\255\255\152\000\152\000\
    \255\255\255\255\255\255\152\000\067\001\255\255\152\000\255\255\
    \255\255\255\255\152\000\255\255\255\255\255\255\058\000\152\000\
    \255\255\179\000\058\000\179\000\255\255\255\255\058\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\255\255\058\000\255\255\255\255\179\000\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\255\255\255\255\255\255\255\255\058\000\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\064\000\255\255\048\001\048\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\255\255\
    \048\001\255\255\255\255\064\000\255\255\255\255\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \048\001\255\255\255\255\255\255\064\000\255\255\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \066\000\255\255\215\000\064\000\215\000\255\255\255\255\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\255\255\255\255\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\255\255\
    \255\255\255\255\255\255\066\000\255\255\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\067\000\
    \255\255\255\255\066\000\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\067\000\067\000\067\000\067\000\067\000\
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
    \069\000\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\255\255\069\000\069\000\069\000\069\000\
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
    \070\000\073\000\255\255\255\255\070\000\073\001\073\001\073\001\
    \073\001\073\001\073\001\073\001\073\001\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\255\255\
    \255\255\255\255\255\255\073\000\255\255\255\255\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \255\255\255\255\255\255\255\255\073\000\255\255\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \076\000\255\255\216\000\073\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\074\001\255\255\255\255\
    \255\255\216\000\255\255\255\255\255\255\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\255\255\
    \255\255\216\000\255\255\076\000\255\255\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\077\000\
    \255\255\255\255\076\000\085\001\085\001\085\001\085\001\085\001\
    \085\001\085\001\085\001\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\255\255\255\255\255\255\
    \255\255\077\000\255\255\255\255\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\255\255\255\255\
    \255\255\255\255\077\000\255\255\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\079\000\255\255\
    \007\001\077\000\007\001\255\255\255\255\007\001\007\001\007\001\
    \007\001\007\001\007\001\007\001\007\001\007\001\007\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\086\001\086\001\255\255\
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
    \084\000\084\000\084\000\084\000\255\255\084\000\090\001\090\001\
    \090\001\090\001\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\255\255\255\255\090\001\090\001\
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
    \255\255\255\255\255\255\255\255\090\001\255\255\087\000\087\000\
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
    \255\255\255\255\255\255\136\000\088\000\255\255\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\255\255\255\255\255\255\255\255\136\000\255\255\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\137\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\255\255\137\000\255\255\255\255\255\255\255\255\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\255\255\255\255\255\255\139\000\137\000\255\255\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\255\255\255\255\255\255\255\255\139\000\
    \255\255\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\140\000\255\255\255\255\139\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\255\255\255\255\255\255\255\255\140\000\255\255\255\255\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\255\255\255\255\255\255\255\255\140\000\255\255\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\142\000\255\255\049\001\140\000\049\001\255\255\
    \255\255\049\001\049\001\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\255\255\255\255\255\255\255\255\142\000\255\255\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\143\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \255\255\143\000\255\255\255\255\255\255\255\255\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \255\255\255\255\255\255\144\000\143\000\255\255\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\255\255\255\255\255\255\255\255\144\000\255\255\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\145\000\173\000\255\255\144\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \255\255\255\255\255\255\255\255\145\000\255\255\255\255\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\255\255\255\255\255\255\255\255\145\000\255\255\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\158\000\211\000\255\255\145\000\173\000\252\000\252\000\
    \252\000\252\000\252\000\211\000\255\255\045\001\255\255\173\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\045\001\255\255\
    \158\000\255\255\255\255\255\255\255\255\252\000\255\255\255\255\
    \255\255\255\255\211\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\211\000\252\000\045\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\045\001\255\255\
    \255\255\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\255\255\255\255\255\255\255\255\158\000\
    \255\255\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\255\255\255\255\
    \255\255\173\000\159\000\255\255\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\165\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\059\001\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\255\255\255\255\255\255\165\000\255\255\255\255\
    \255\255\050\001\255\255\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \050\001\255\255\255\255\255\255\255\255\255\255\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \050\001\255\255\059\001\255\255\165\000\255\255\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \166\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\255\255\255\255\255\255\255\255\166\000\
    \255\255\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\213\000\213\000\213\000\213\000\213\000\
    \213\000\213\000\213\000\213\000\213\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\213\000\213\000\213\000\213\000\
    \213\000\213\000\059\001\255\255\217\000\255\255\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \255\255\255\255\255\255\255\255\078\001\078\001\078\001\078\001\
    \255\255\255\255\255\255\217\000\213\000\213\000\213\000\213\000\
    \213\000\213\000\217\000\069\001\069\001\069\001\069\001\069\001\
    \069\001\069\001\069\001\217\000\078\001\255\255\255\255\255\255\
    \255\255\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\255\255\255\255\217\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\217\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\217\000\220\000\220\000\220\000\220\000\
    \220\000\220\000\220\000\220\000\220\000\220\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\220\000\220\000\220\000\
    \220\000\220\000\220\000\255\255\255\255\255\255\078\001\255\255\
    \220\000\255\255\255\255\069\001\255\255\255\255\255\255\255\255\
    \255\255\220\000\255\255\255\255\223\000\223\000\223\000\223\000\
    \223\000\080\001\255\255\255\255\255\255\220\000\220\000\220\000\
    \220\000\220\000\220\000\255\255\255\255\255\255\255\255\255\255\
    \220\000\255\255\255\255\223\000\223\000\223\000\223\000\223\000\
    \255\255\220\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\255\255\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\255\255\
    \223\000\255\255\223\000\255\255\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\223\000\
    \223\000\223\000\223\000\223\000\223\000\223\000\223\000\225\000\
    \223\000\225\000\225\000\225\000\225\000\225\000\225\000\225\000\
    \225\000\225\000\225\000\069\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\225\000\225\000\255\255\
    \255\255\080\001\255\255\255\255\255\255\225\000\255\255\255\255\
    \255\255\255\255\078\001\255\255\255\255\226\000\225\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\255\255\255\255\255\255\255\255\225\000\225\000\255\255\
    \255\255\255\255\255\255\226\000\226\000\225\000\255\255\255\255\
    \255\255\255\255\255\255\226\000\255\255\255\255\225\000\255\255\
    \255\255\255\255\255\255\255\255\226\000\255\255\255\255\226\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\226\000\226\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\226\000\255\255\255\255\255\255\230\000\
    \255\255\255\255\255\255\255\255\226\000\255\255\255\255\226\000\
    \255\255\255\255\255\255\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\255\255\230\000\
    \255\255\255\255\255\255\223\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\255\255\255\255\
    \255\255\255\255\230\000\255\255\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\231\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\255\255\231\000\255\255\
    \255\255\255\255\255\255\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\255\255\255\255\255\255\
    \014\001\231\000\255\255\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\014\001\014\001\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\255\255\
    \255\255\255\255\255\255\014\001\255\255\014\001\014\001\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\014\001\
    \014\001\014\001\014\001\014\001\014\001\014\001\014\001\015\001\
    \255\255\255\255\014\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\255\255\255\255\255\255\
    \255\255\015\001\255\255\255\255\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\255\255\255\255\
    \255\255\255\255\015\001\255\255\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\015\001\015\001\
    \015\001\015\001\015\001\015\001\015\001\015\001\017\001\255\255\
    \255\255\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\017\001\255\255\255\255\255\255\
    \255\255\017\001\255\255\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\017\001\017\001\017\001\017\001\
    \017\001\017\001\017\001\017\001\017\001\018\001\255\255\255\255\
    \017\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\255\255\018\001\255\255\255\255\
    \255\255\255\255\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\255\255\255\255\255\255\019\001\
    \018\001\255\255\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\018\001\018\001\018\001\018\001\
    \018\001\018\001\018\001\018\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\255\255\255\255\
    \255\255\255\255\019\001\255\255\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\019\001\019\001\
    \019\001\019\001\019\001\019\001\019\001\019\001\020\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\255\255\020\001\255\255\
    \255\255\255\255\255\255\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\255\255\255\255\255\255\
    \021\001\020\001\255\255\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\020\001\020\001\020\001\
    \020\001\020\001\020\001\020\001\020\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\255\255\
    \255\255\255\255\255\255\021\001\255\255\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\021\001\
    \021\001\021\001\021\001\021\001\021\001\021\001\021\001\024\001\
    \255\255\255\255\021\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\255\255\255\255\255\255\
    \255\255\024\001\255\255\255\255\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\255\255\255\255\
    \255\255\255\255\024\001\255\255\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\024\001\024\001\
    \024\001\024\001\024\001\024\001\024\001\024\001\027\001\255\255\
    \255\255\024\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\001\027\001\027\001\027\001\027\001\
    \027\001\027\001\027\001\027\001\027\001\027\001\027\001\027\001\
    \027\001\027\001\027\001\027\001\027\001\027\001\027\001\027\001\
    \027\001\027\001\027\001\027\001\027\001\255\255\255\255\255\255\
    \255\255\027\001\255\255\027\001\027\001\027\001\027\001\027\001\
    \027\001\027\001\027\001\027\001\027\001\027\001\027\001\027\001\
    \027\001\027\001\027\001\027\001\027\001\027\001\027\001\027\001\
    \027\001\027\001\027\001\027\001\027\001\028\001\255\255\255\255\
    \027\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\255\255\255\255\255\255\255\255\028\001\
    \255\255\255\255\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\255\255\255\255\255\255\255\255\
    \028\001\255\255\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\030\001\255\255\255\255\028\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
    \030\001\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
    \030\001\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
    \030\001\030\001\030\001\255\255\255\255\255\255\255\255\030\001\
    \255\255\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
    \030\001\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
    \030\001\030\001\030\001\030\001\030\001\030\001\030\001\030\001\
    \030\001\030\001\030\001\031\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\255\255\031\001\255\255\255\255\255\255\255\255\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\255\255\255\255\255\255\032\001\031\001\255\255\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\255\255\255\255\255\255\255\255\
    \032\001\255\255\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\033\001\255\255\255\255\032\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\255\255\255\255\255\255\255\255\033\001\255\255\
    \255\255\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\255\255\255\255\255\255\255\255\033\001\
    \255\255\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\034\001\255\255\255\255\033\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\255\255\255\255\255\255\255\255\034\001\255\255\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\034\001\034\001\034\001\034\001\034\001\034\001\
    \034\001\034\001\035\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\255\255\035\001\255\255\255\255\255\255\255\255\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\255\255\255\255\255\255\037\001\035\001\255\255\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\035\001\035\001\035\001\035\001\035\001\035\001\035\001\
    \035\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\255\255\255\255\255\255\255\255\037\001\
    \255\255\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\038\001\255\255\255\255\037\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\255\255\255\255\255\255\255\255\038\001\255\255\255\255\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\255\255\255\255\255\255\255\255\038\001\255\255\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\040\001\255\255\255\255\038\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\040\001\
    \040\001\040\001\040\001\040\001\040\001\040\001\040\001\040\001\
    \040\001\040\001\040\001\040\001\040\001\040\001\040\001\040\001\
    \040\001\040\001\040\001\040\001\040\001\040\001\040\001\040\001\
    \040\001\255\255\255\255\255\255\255\255\040\001\255\255\040\001\
    \040\001\040\001\040\001\040\001\040\001\040\001\040\001\040\001\
    \040\001\040\001\040\001\040\001\040\001\040\001\040\001\040\001\
    \040\001\040\001\040\001\040\001\040\001\040\001\040\001\040\001\
    \040\001\041\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \255\255\041\001\255\255\255\255\255\255\255\255\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \255\255\255\255\255\255\042\001\041\001\255\255\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \041\001\041\001\041\001\041\001\041\001\041\001\041\001\041\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\255\255\255\255\255\255\255\255\042\001\255\255\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\042\001\042\001\042\001\042\001\042\001\042\001\
    \042\001\042\001\043\001\255\255\255\255\042\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \255\255\255\255\255\255\255\255\043\001\255\255\255\255\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\255\255\255\255\255\255\255\255\043\001\255\255\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\255\255\255\255\255\255\043\001\047\001\047\001\047\001\
    \047\001\047\001\047\001\047\001\047\001\047\001\047\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\047\001\047\001\
    \047\001\047\001\047\001\047\001\255\255\255\255\051\001\255\255\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\001\047\001\047\001\
    \047\001\047\001\047\001\047\001\051\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\051\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\054\001\
    \054\001\054\001\054\001\054\001\054\001\255\255\255\255\255\255\
    \255\255\255\255\054\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\054\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\054\001\
    \054\001\054\001\054\001\054\001\054\001\255\255\255\255\255\255\
    \255\255\255\255\054\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\054\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\061\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\061\001\061\001\061\001\
    \061\001\061\001\061\001\065\001\065\001\065\001\065\001\065\001\
    \065\001\065\001\065\001\065\001\065\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\065\001\065\001\065\001\065\001\
    \065\001\065\001\255\255\255\255\255\255\061\001\061\001\061\001\
    \061\001\061\001\061\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\072\001\072\001\072\001\072\001\072\001\072\001\072\001\
    \072\001\072\001\072\001\255\255\065\001\065\001\065\001\065\001\
    \065\001\065\001\072\001\072\001\072\001\072\001\072\001\072\001\
    \076\001\076\001\076\001\076\001\076\001\076\001\076\001\076\001\
    \076\001\076\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\076\001\076\001\076\001\076\001\076\001\076\001\255\255\
    \255\255\255\255\072\001\072\001\072\001\072\001\072\001\072\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\084\001\084\001\084\001\
    \255\255\076\001\076\001\076\001\076\001\076\001\076\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\088\001\088\001\
    \088\001\088\001\088\001\088\001\255\255\255\255\255\255\084\001\
    \084\001\084\001\084\001\084\001\084\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\088\001\088\001\
    \088\001\088\001\088\001\088\001\255\255\255\255\255\255\255\255\
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
    \000\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\005\000\000\000\033\000\
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
    ";
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\000\000\000\
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
    ";
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
    ";
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
    \255\255\000\000\040\000\157\000\158\000\164\000\165\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\040\000\157\000\158\000\164\000\165\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\163\000\156\000\255\255\
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
# 542 "./lexer_cocci.mll"
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
# 2681 "lexer_cocci.ml"

  | 1 ->
let
# 557 "./lexer_cocci.mll"
                      w
# 2687 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 557 "./lexer_cocci.mll"
                         ( (* collect whitespaces only when inside a rule *)
    start_line false;
    if !Data.in_rule_name || !Data.in_prolog || !Data.in_iso
    then token lexbuf
    else TWhitespace w )
# 2695 "lexer_cocci.ml"

  | 2 ->
let
# 563 "./lexer_cocci.mll"
                                       after
# 2701 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 563 "./lexer_cocci.mll"
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
# 2714 "lexer_cocci.ml"

  | 3 ->
# 575 "./lexer_cocci.mll"
   ( match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	start_line true;
	TDirective (Ast.Space (tok lexbuf), get_current_line_type lexbuf)
    | _ -> failwith "attributes only allowed in + code" )
# 2723 "lexer_cocci.ml"

  | 4 ->
# 581 "./lexer_cocci.mll"
         ( start_line true; TArobArob )
# 2728 "lexer_cocci.ml"

  | 5 ->
# 582 "./lexer_cocci.mll"
         ( pass_zero();
	   if !Data.in_rule_name || not !current_line_started
	   then (start_line true; TArob)
	   else (check_minus_context_linetype "@";
		 TPArob (get_current_line_type lexbuf)) )
# 2737 "lexer_cocci.ml"

  | 6 ->
# 589 "./lexer_cocci.mll"
      ( start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen (get_current_line_type lexbuf) )
# 2743 "lexer_cocci.ml"

  | 7 ->
# 593 "./lexer_cocci.mll"
      ( start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis (get_current_line_type lexbuf) )
# 2749 "lexer_cocci.ml"

  | 8 ->
# 604 "./lexer_cocci.mll"
           ( start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis (get_current_line_type lexbuf) )
# 2755 "lexer_cocci.ml"

  | 9 ->
# 606 "./lexer_cocci.mll"
           ( start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis (get_current_line_type lexbuf) )
# 2761 "lexer_cocci.ml"

  | 10 ->
# 608 "./lexer_cocci.mll"
            ( start_line true; check_minus_context_linetype (tok lexbuf);
	     TPOEllipsis (get_current_line_type lexbuf) )
# 2767 "lexer_cocci.ml"

  | 11 ->
# 610 "./lexer_cocci.mll"
            ( start_line true; check_minus_context_linetype (tok lexbuf);
	     TPCEllipsis (get_current_line_type lexbuf) )
# 2773 "lexer_cocci.ml"

  | 12 ->
# 623 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TMinus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.MINUS; token lexbuf) )
# 2782 "lexer_cocci.ml"

  | 13 ->
# 628 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TPlus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.PLUS; token lexbuf) )
# 2791 "lexer_cocci.ml"

  | 14 ->
# 633 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TWhy (get_current_line_type lexbuf))
          else (add_current_line_type D.OPT; token lexbuf) )
# 2799 "lexer_cocci.ml"

  | 15 ->
# 637 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TBang (get_current_line_type lexbuf))
          else (add_current_line_type D.UNIQUE; token lexbuf) )
# 2807 "lexer_cocci.ml"

  | 16 ->
# 641 "./lexer_cocci.mll"
        ( if not !col_zero
	  then (start_line true; TOPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 ("(",get_current_line_type lexbuf)))
# 2816 "lexer_cocci.ml"

  | 17 ->
# 646 "./lexer_cocci.mll"
          ( start_line true;
	    TOPar0 ("\\(",contextify(get_current_line_type lexbuf)) )
# 2822 "lexer_cocci.ml"

  | 18 ->
# 648 "./lexer_cocci.mll"
        ( if not (!col_zero)
	  then (start_line true; TOr(get_current_line_type lexbuf))
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 ("|",get_current_line_type lexbuf)))
# 2831 "lexer_cocci.ml"

  | 19 ->
# 653 "./lexer_cocci.mll"
          ( start_line true;
	    TMid0 ("\\|",contextify(get_current_line_type lexbuf)) )
# 2837 "lexer_cocci.ml"

  | 20 ->
# 655 "./lexer_cocci.mll"
        ( if not !col_zero
	  then (start_line true; TCPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 (")",get_current_line_type lexbuf)))
# 2846 "lexer_cocci.ml"

  | 21 ->
# 660 "./lexer_cocci.mll"
          ( start_line true;
	    TCPar0 ("\\)",contextify(get_current_line_type lexbuf)) )
# 2852 "lexer_cocci.ml"

  | 22 ->
# 663 "./lexer_cocci.mll"
        ( start_line true; TOCro (get_current_line_type lexbuf)   )
# 2857 "lexer_cocci.ml"

  | 23 ->
# 664 "./lexer_cocci.mll"
        ( start_line true; TCCro (get_current_line_type lexbuf)   )
# 2862 "lexer_cocci.ml"

  | 24 ->
# 665 "./lexer_cocci.mll"
        ( start_line true; TOBrace (get_current_line_type lexbuf) )
# 2867 "lexer_cocci.ml"

  | 25 ->
# 666 "./lexer_cocci.mll"
        ( start_line true; TCBrace (get_current_line_type lexbuf) )
# 2872 "lexer_cocci.ml"

  | 26 ->
# 668 "./lexer_cocci.mll"
                   ( start_line true; TPtrOp (get_current_line_type lexbuf)  )
# 2877 "lexer_cocci.ml"

  | 27 ->
# 669 "./lexer_cocci.mll"
                   ( start_line true; TDot (get_current_line_type lexbuf)    )
# 2882 "lexer_cocci.ml"

  | 28 ->
# 670 "./lexer_cocci.mll"
                   ( start_line true; TComma (get_current_line_type lexbuf)  )
# 2887 "lexer_cocci.ml"

  | 29 ->
# 671 "./lexer_cocci.mll"
                   ( start_line true; TPtVirg (get_current_line_type lexbuf) )
# 2892 "lexer_cocci.ml"

  | 30 ->
# 674 "./lexer_cocci.mll"
                   ( pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS; token lexbuf) )
# 2903 "lexer_cocci.ml"

  | 31 ->
# 681 "./lexer_cocci.mll"
                   ( start_line true;
		     TDmOp (Ast.Div,get_current_line_type lexbuf) )
# 2909 "lexer_cocci.ml"

  | 32 ->
# 683 "./lexer_cocci.mll"
                    ( start_line true;
		     TDmOp (Ast.Min,get_current_line_type lexbuf) )
# 2915 "lexer_cocci.ml"

  | 33 ->
# 685 "./lexer_cocci.mll"
                    ( start_line true;
		     TDmOp (Ast.Max,get_current_line_type lexbuf) )
# 2921 "lexer_cocci.ml"

  | 34 ->
# 687 "./lexer_cocci.mll"
                   ( start_line true;
		     TDmOp (Ast.Mod,get_current_line_type lexbuf) )
# 2927 "lexer_cocci.ml"

  | 35 ->
# 689 "./lexer_cocci.mll"
                   ( start_line true;  TTilde (get_current_line_type lexbuf) )
# 2932 "lexer_cocci.ml"

  | 36 ->
# 691 "./lexer_cocci.mll"
                   ( pass_zero();
 		     if !current_line_started
 		     then
 		       (start_line true; TInc (get_current_line_type lexbuf))
 		     else (patch_or_match PATCH;
 			   add_current_line_type D.PLUSPLUS; token lexbuf) )
# 2942 "lexer_cocci.ml"

  | 37 ->
# 697 "./lexer_cocci.mll"
                   ( start_line true;  TDec (get_current_line_type lexbuf) )
# 2947 "lexer_cocci.ml"

  | 38 ->
# 699 "./lexer_cocci.mll"
                   ( start_line true; TEq (get_current_line_type lexbuf) )
# 2952 "lexer_cocci.ml"

  | 39 ->
# 701 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Minus lexbuf )
# 2957 "lexer_cocci.ml"

  | 40 ->
# 702 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Plus lexbuf )
# 2962 "lexer_cocci.ml"

  | 41 ->
# 704 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Mul lexbuf )
# 2967 "lexer_cocci.ml"

  | 42 ->
# 705 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Div lexbuf )
# 2972 "lexer_cocci.ml"

  | 43 ->
# 706 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Mod lexbuf )
# 2977 "lexer_cocci.ml"

  | 44 ->
# 708 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.And lexbuf )
# 2982 "lexer_cocci.ml"

  | 45 ->
# 709 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Or lexbuf )
# 2987 "lexer_cocci.ml"

  | 46 ->
# 710 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Xor lexbuf )
# 2992 "lexer_cocci.ml"

  | 47 ->
# 711 "./lexer_cocci.mll"
                    ( start_line true; mkassign Ast.Max lexbuf )
# 2997 "lexer_cocci.ml"

  | 48 ->
# 712 "./lexer_cocci.mll"
                    ( start_line true; mkassign Ast.Min lexbuf )
# 3002 "lexer_cocci.ml"

  | 49 ->
# 714 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.DecLeft lexbuf )
# 3007 "lexer_cocci.ml"

  | 50 ->
# 715 "./lexer_cocci.mll"
                   ( start_line true; mkassign Ast.DecRight lexbuf )
# 3012 "lexer_cocci.ml"

  | 51 ->
# 717 "./lexer_cocci.mll"
                   ( start_line true; TDotDot (get_current_line_type lexbuf) )
# 3017 "lexer_cocci.ml"

  | 52 ->
# 719 "./lexer_cocci.mll"
                   ( start_line true; TEqEq    (get_current_line_type lexbuf) )
# 3022 "lexer_cocci.ml"

  | 53 ->
# 720 "./lexer_cocci.mll"
                   ( start_line true; TNotEq   (get_current_line_type lexbuf) )
# 3027 "lexer_cocci.ml"

  | 54 ->
# 721 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.SupEq,get_current_line_type lexbuf) )
# 3033 "lexer_cocci.ml"

  | 55 ->
# 723 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.InfEq,get_current_line_type lexbuf) )
# 3039 "lexer_cocci.ml"

  | 56 ->
# 725 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.Inf,get_current_line_type lexbuf) )
# 3045 "lexer_cocci.ml"

  | 57 ->
# 727 "./lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.Sup,get_current_line_type lexbuf) )
# 3051 "lexer_cocci.ml"

  | 58 ->
# 730 "./lexer_cocci.mll"
                   ( start_line true; TAndLog (get_current_line_type lexbuf) )
# 3056 "lexer_cocci.ml"

  | 59 ->
# 731 "./lexer_cocci.mll"
                   ( start_line true; TOrLog  (get_current_line_type lexbuf) )
# 3061 "lexer_cocci.ml"

  | 60 ->
# 733 "./lexer_cocci.mll"
                   ( start_line true;
		     TShROp(Ast.DecRight,get_current_line_type lexbuf) )
# 3067 "lexer_cocci.ml"

  | 61 ->
# 735 "./lexer_cocci.mll"
                   ( start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) )
# 3073 "lexer_cocci.ml"

  | 62 ->
# 738 "./lexer_cocci.mll"
                   ( start_line true; TAnd    (get_current_line_type lexbuf) )
# 3078 "lexer_cocci.ml"

  | 63 ->
# 739 "./lexer_cocci.mll"
                   ( start_line true; TXor(get_current_line_type lexbuf) )
# 3083 "lexer_cocci.ml"

  | 64 ->
# 741 "./lexer_cocci.mll"
                    ( start_line true; TCppConcatOp )
# 3088 "lexer_cocci.ml"

  | 65 ->
let
# 742 "./lexer_cocci.mll"
                                               wss
# 3094 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 742 "./lexer_cocci.mll"
                                                        def
# 3099 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(1)
and
# 743 "./lexer_cocci.mll"
                                  ident
# 3104 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 744 "./lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TUndef
	  (lt,
	   check_var ident
	     (arity,line,lline,llend,offset+off,col+off,[],[],[],wss)) )
# 3116 "lexer_cocci.ml"

  | 66 ->
let
# 753 "./lexer_cocci.mll"
                                                  wss
# 3122 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 753 "./lexer_cocci.mll"
                                                            def
# 3127 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(1)
and
# 754 "./lexer_cocci.mll"
                                   ident
# 3132 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_curr_pos in
# 755 "./lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TDefine
	  (lt,
	   check_var ident
	     (arity,line,lline,llend,offset+off,col+off,[],[],[],wss)) )
# 3144 "lexer_cocci.ml"

  | 67 ->
let
# 764 "./lexer_cocci.mll"
                                                  wss
# 3150 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 764 "./lexer_cocci.mll"
                                                            def
# 3155 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(1)
and
# 765 "./lexer_cocci.mll"
                                    ident
# 3160 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 767 "./lexer_cocci.mll"
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
# 3174 "lexer_cocci.ml"

  | 68 ->
# 779 "./lexer_cocci.mll"
      ( start_line true; TPragma(get_current_line_type lexbuf) )
# 3179 "lexer_cocci.ml"

  | 69 ->
# 784 "./lexer_cocci.mll"
      ( TIncludeL
	  (let str = tok lexbuf in
	  let start = String.index str '\"' in
	  let finish = String.rindex str '\"' in
	  start_line true;
	  (process_include start finish str, get_current_line_type lexbuf)) )
# 3189 "lexer_cocci.ml"

  | 70 ->
# 791 "./lexer_cocci.mll"
      ( TIncludeNL
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) )
# 3199 "lexer_cocci.ml"

  | 71 ->
# 805 "./lexer_cocci.mll"
      ( start_line true; check_plus_linetype (tok lexbuf);
	TDirective (Ast.Noindent(tok lexbuf), get_current_line_type lexbuf) )
# 3205 "lexer_cocci.ml"

  | 72 ->
# 808 "./lexer_cocci.mll"
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
# 3219 "lexer_cocci.ml"

  | 73 ->
# 819 "./lexer_cocci.mll"
      ( (if !current_line_started
      then lexerr "--- must be at the beginning of the line" "");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) )
# 3230 "lexer_cocci.ml"

  | 74 ->
# 827 "./lexer_cocci.mll"
      ( (if !current_line_started
      then lexerr "+++ must be at the beginning of the line" "");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) )
# 3241 "lexer_cocci.ml"

  | 75 ->
# 836 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3246 "lexer_cocci.ml"

  | 76 ->
# 840 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3251 "lexer_cocci.ml"

  | 77 ->
# 847 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3263 "lexer_cocci.ml"

  | 78 ->
# 858 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3275 "lexer_cocci.ml"

  | 79 ->
# 874 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3287 "lexer_cocci.ml"

  | 80 ->
# 887 "./lexer_cocci.mll"
      ( 
	start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3299 "lexer_cocci.ml"

  | 81 ->
# 898 "./lexer_cocci.mll"
        ( start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) )
# 3305 "lexer_cocci.ml"

  | 82 ->
# 900 "./lexer_cocci.mll"
         ( start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) )
# 3311 "lexer_cocci.ml"

  | 83 ->
let
# 902 "./lexer_cocci.mll"
             x
# 3317 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 902 "./lexer_cocci.mll"
                   ( start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) )
# 3322 "lexer_cocci.ml"

  | 84 ->
let
# 912 "./lexer_cocci.mll"
         x
# 3328 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 912 "./lexer_cocci.mll"
            ( start_line true; TInt(x,(get_current_line_type lexbuf)) )
# 3332 "lexer_cocci.ml"

  | 85 ->
let
# 914 "./lexer_cocci.mll"
                           x
# 3338 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 915 "./lexer_cocci.mll"
      ( if !Flag.ibm
      then
	begin
	  start_line true;
	  let len = string_of_int(String.length x - 1) in
          TDecimalCst(x,len,"0",(get_current_line_type lexbuf))
	end
      else failwith "unrecognized constant modifier d/D" )
# 3349 "lexer_cocci.ml"

  | 86 ->
# 924 "./lexer_cocci.mll"
                   ( TIso )
# 3354 "lexer_cocci.ml"

  | 87 ->
# 925 "./lexer_cocci.mll"
                   ( TRightIso )
# 3359 "lexer_cocci.ml"

  | 88 ->
# 927 "./lexer_cocci.mll"
                   ( EOF )
# 3364 "lexer_cocci.ml"

  | 89 ->
# 929 "./lexer_cocci.mll"
      ( lexerr "unrecognised symbol, in token rule: " (tok lexbuf) )
# 3369 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and metavariable_decl_token lexbuf =
    __ocaml_lex_metavariable_decl_token_rec lexbuf 223
and __ocaml_lex_metavariable_decl_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 934 "./lexer_cocci.mll"
    ( reset_line lexbuf; metavariable_decl_token lexbuf )
# 3380 "lexer_cocci.ml"

  | 1 ->
# 936 "./lexer_cocci.mll"
                  (
    start_line false; metavariable_decl_token lexbuf )
# 3386 "lexer_cocci.ml"

  | 2 ->
# 939 "./lexer_cocci.mll"
                                   (
    start_line false; metavariable_decl_token lexbuf )
# 3392 "lexer_cocci.ml"

  | 3 ->
# 942 "./lexer_cocci.mll"
         ( start_line true; TArobArob )
# 3397 "lexer_cocci.ml"

  | 4 ->
# 944 "./lexer_cocci.mll"
         ( start_line true; TTildeEq (get_current_line_type lexbuf) )
# 3402 "lexer_cocci.ml"

  | 5 ->
# 945 "./lexer_cocci.mll"
         ( start_line true; TTildeExclEq (get_current_line_type lexbuf) )
# 3407 "lexer_cocci.ml"

  | 6 ->
# 946 "./lexer_cocci.mll"
         ( start_line true; TEq (get_current_line_type lexbuf) )
# 3412 "lexer_cocci.ml"

  | 7 ->
# 947 "./lexer_cocci.mll"
        ( pass_zero(); TPlus0 )
# 3417 "lexer_cocci.ml"

  | 8 ->
# 948 "./lexer_cocci.mll"
        ( pass_zero(); TWhy0 )
# 3422 "lexer_cocci.ml"

  | 9 ->
# 949 "./lexer_cocci.mll"
        ( pass_zero(); TBang0 )
# 3427 "lexer_cocci.ml"

  | 10 ->
# 950 "./lexer_cocci.mll"
        ( start_line true; TOPar (get_current_line_type lexbuf) )
# 3432 "lexer_cocci.ml"

  | 11 ->
# 951 "./lexer_cocci.mll"
        ( start_line true; TCPar (get_current_line_type lexbuf) )
# 3437 "lexer_cocci.ml"

  | 12 ->
# 953 "./lexer_cocci.mll"
        ( start_line true; TOCro (get_current_line_type lexbuf)   )
# 3442 "lexer_cocci.ml"

  | 13 ->
# 954 "./lexer_cocci.mll"
        ( start_line true; TCCro (get_current_line_type lexbuf)   )
# 3447 "lexer_cocci.ml"

  | 14 ->
# 955 "./lexer_cocci.mll"
        ( start_line true; TOBrace (get_current_line_type lexbuf) )
# 3452 "lexer_cocci.ml"

  | 15 ->
# 956 "./lexer_cocci.mll"
        ( start_line true; TCBrace (get_current_line_type lexbuf) )
# 3457 "lexer_cocci.ml"

  | 16 ->
# 958 "./lexer_cocci.mll"
                   ( start_line true; TPtrOp (get_current_line_type lexbuf)  )
# 3462 "lexer_cocci.ml"

  | 17 ->
# 959 "./lexer_cocci.mll"
                   ( start_line true; TDot (get_current_line_type lexbuf)    )
# 3467 "lexer_cocci.ml"

  | 18 ->
# 960 "./lexer_cocci.mll"
                   ( start_line true; TComma (get_current_line_type lexbuf)  )
# 3472 "lexer_cocci.ml"

  | 19 ->
# 961 "./lexer_cocci.mll"
                   ( start_line true;
		     TMPtVirg (* works better with tokens_all *) )
# 3478 "lexer_cocci.ml"

  | 20 ->
# 963 "./lexer_cocci.mll"
                   ( start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) )
# 3484 "lexer_cocci.ml"

  | 21 ->
# 966 "./lexer_cocci.mll"
                   ( pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS;
			metavariable_decl_token lexbuf) )
# 3496 "lexer_cocci.ml"

  | 22 ->
# 975 "./lexer_cocci.mll"
                   ( start_line true; TEqEq    (get_current_line_type lexbuf) )
# 3501 "lexer_cocci.ml"

  | 23 ->
# 976 "./lexer_cocci.mll"
                   ( start_line true; TNotEq   (get_current_line_type lexbuf) )
# 3506 "lexer_cocci.ml"

  | 24 ->
# 977 "./lexer_cocci.mll"
                   ( start_line true; TSub     (get_current_line_type lexbuf) )
# 3511 "lexer_cocci.ml"

  | 25 ->
# 979 "./lexer_cocci.mll"
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
# 3525 "lexer_cocci.ml"

  | 26 ->
# 990 "./lexer_cocci.mll"
                    ( start_line true; TCppConcatOp (* for fresh vars *) )
# 3530 "lexer_cocci.ml"

  | 27 ->
# 993 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3535 "lexer_cocci.ml"

  | 28 ->
# 997 "./lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 3540 "lexer_cocci.ml"

  | 29 ->
# 1004 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3551 "lexer_cocci.ml"

  | 30 ->
# 1014 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3562 "lexer_cocci.ml"

  | 31 ->
# 1029 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3573 "lexer_cocci.ml"

  | 32 ->
# 1041 "./lexer_cocci.mll"
      ( start_line true; 
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf 
      )
# 3584 "lexer_cocci.ml"

  | 33 ->
# 1051 "./lexer_cocci.mll"
        ( start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) )
# 3590 "lexer_cocci.ml"

  | 34 ->
# 1053 "./lexer_cocci.mll"
         ( start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) )
# 3596 "lexer_cocci.ml"

  | 35 ->
let
# 1055 "./lexer_cocci.mll"
             x
# 3602 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1055 "./lexer_cocci.mll"
                   ( Printf.printf "36\n"; start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) )
# 3607 "lexer_cocci.ml"

  | 36 ->
let
# 1065 "./lexer_cocci.mll"
         x
# 3613 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1065 "./lexer_cocci.mll"
            ( start_line true; TInt(x,(get_current_line_type lexbuf)) )
# 3617 "lexer_cocci.ml"

  | 37 ->
let
# 1067 "./lexer_cocci.mll"
                           x
# 3623 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1068 "./lexer_cocci.mll"
      ( if !Flag.ibm
      then
	begin
	  start_line true;
	  let len = string_of_int(String.length x - 1) in
          TDecimalCst(x,len,"0",(get_current_line_type lexbuf))
	end
      else failwith "unrecognized constant modifier d/D" )
# 3634 "lexer_cocci.ml"

  | 38 ->
# 1077 "./lexer_cocci.mll"
      ( lexerr "metavariables: unrecognised symbol, in token rule: "
	  (tok lexbuf) )
# 3640 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_metavariable_decl_token_rec lexbuf __ocaml_lex_state

and char lexbuf =
    __ocaml_lex_char_rec lexbuf 313
and __ocaml_lex_char_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 1082 "./lexer_cocci.mll"
          x
# 3652 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1082 "./lexer_cocci.mll"
                                       ( String.make 1 x ^ restchars lexbuf )
# 3656 "lexer_cocci.ml"

  | 1 ->
let
# 1084 "./lexer_cocci.mll"
                                             x
# 3662 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1084 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3666 "lexer_cocci.ml"

  | 2 ->
let
# 1088 "./lexer_cocci.mll"
                                  x
# 3672 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1088 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3676 "lexer_cocci.ml"

  | 3 ->
let
# 1089 "./lexer_cocci.mll"
                 v
# 3682 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1089 "./lexer_cocci.mll"
                                  x
# 3687 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1090 "./lexer_cocci.mll"
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
# 3701 "lexer_cocci.ml"

  | 4 ->
# 1102 "./lexer_cocci.mll"
      ( Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      )
# 3708 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_char_rec lexbuf __ocaml_lex_state

and restchars lexbuf =
    __ocaml_lex_restchars_rec lexbuf 323
and __ocaml_lex_restchars_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1107 "./lexer_cocci.mll"
                                       ( "" )
# 3719 "lexer_cocci.ml"

  | 1 ->
let
# 1108 "./lexer_cocci.mll"
          x
# 3725 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1108 "./lexer_cocci.mll"
                                       ( String.make 1 x ^ restchars lexbuf )
# 3729 "lexer_cocci.ml"

  | 2 ->
let
# 1110 "./lexer_cocci.mll"
                                             x
# 3735 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1110 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3739 "lexer_cocci.ml"

  | 3 ->
let
# 1114 "./lexer_cocci.mll"
                                  x
# 3745 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1114 "./lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 3749 "lexer_cocci.ml"

  | 4 ->
let
# 1115 "./lexer_cocci.mll"
                 v
# 3755 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1115 "./lexer_cocci.mll"
                                  x
# 3760 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1116 "./lexer_cocci.mll"
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
# 3774 "lexer_cocci.ml"

  | 5 ->
# 1128 "./lexer_cocci.mll"
      ( Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      )
# 3781 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_restchars_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 334
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1133 "./lexer_cocci.mll"
                                               ( "" )
# 3792 "lexer_cocci.ml"

  | 1 ->
let
# 1134 "./lexer_cocci.mll"
                                 x
# 3798 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1135 "./lexer_cocci.mll"
    ( line := !line + 1; (Printf.sprintf "%c" x) ^ string lexbuf )
# 3802 "lexer_cocci.ml"

  | 2 ->
let
# 1136 "./lexer_cocci.mll"
          x
# 3808 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 1136 "./lexer_cocci.mll"
                               ( Common.string_of_char x ^ string lexbuf )
# 3812 "lexer_cocci.ml"

  | 3 ->
let
# 1137 "./lexer_cocci.mll"
                                            x
# 3818 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1137 "./lexer_cocci.mll"
                                              ( x ^ string lexbuf )
# 3822 "lexer_cocci.ml"

  | 4 ->
let
# 1138 "./lexer_cocci.mll"
                               x
# 3828 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 1138 "./lexer_cocci.mll"
                                              ( x ^ string lexbuf )
# 3832 "lexer_cocci.ml"

  | 5 ->
let
# 1139 "./lexer_cocci.mll"
                v
# 3838 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 1139 "./lexer_cocci.mll"
                       x
# 3843 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 1140 "./lexer_cocci.mll"
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
# 3858 "lexer_cocci.ml"

  | 6 ->
# 1152 "./lexer_cocci.mll"
      ( lexerr "unrecognised symbol: " (tok lexbuf) )
# 3863 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and comment check_comment lexbuf =
    __ocaml_lex_comment_rec check_comment lexbuf 346
and __ocaml_lex_comment_rec check_comment lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1155 "./lexer_cocci.mll"
         ( let s = tok lexbuf in check_comment s; start_line true; s )
# 3874 "lexer_cocci.ml"

  | 1 ->
# 1157 "./lexer_cocci.mll"
      ( let s = tok lexbuf in
        (* even blank line should have a + *)
        check_comment s;
        reset_line lexbuf; s ^ comment check_comment lexbuf )
# 3882 "lexer_cocci.ml"

  | 2 ->
# 1161 "./lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true;
		let s = tok lexbuf in s^(comment check_comment lexbuf))
	  else (start_line true; comment check_comment lexbuf) )
# 3891 "lexer_cocci.ml"

  | 3 ->
# 1168 "./lexer_cocci.mll"
      ( let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf )
# 3897 "lexer_cocci.ml"

  | 4 ->
# 1171 "./lexer_cocci.mll"
      ( let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf )
# 3903 "lexer_cocci.ml"

  | 5 ->
# 1174 "./lexer_cocci.mll"
      ( start_line true; let s = tok lexbuf in
        Common.pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment check_comment lexbuf
      )
# 3911 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec check_comment lexbuf __ocaml_lex_state

;;

