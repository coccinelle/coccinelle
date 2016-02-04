# 0 "lexer_cocci.mll"
 
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
  prev_plus := (c = D.PLUS) or (c = D.PLUSPLUS);
  (c,l,ll,lex_start,preceeding_spaces,[],[],[])
let current_line_started = ref false
let col_zero = ref true

let contextify (c,l,ll,lex_start,preceeding_spaces,bef,aft,pos) =
  (D.CONTEXT,l,ll,lex_start,preceeding_spaces,bef,aft,pos)

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
	  (try (Hashtbl.find declarer_names s) linetype
	  with Not_found ->
	    (try (Hashtbl.find iterator_names s) linetype
	    with Not_found ->
	      (try (Hashtbl.find symbol_names s) linetype
	      with Not_found ->
                TIdent (s,linetype))))) in
  if !Data.in_meta or !Data.in_rule_name
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
  | "symbol" when in_meta ->     check_arity_context_linetype s; TSymbol

  | "using" when in_rule_name || in_prolog ->  check_context_linetype s; TUsing
  | "virtual" when in_prolog or in_rule_name or in_meta ->
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
  Data.add_symbol_meta :=
    (function name ->
      let fn clt = TSymId (name,clt) in
      Hashtbl.replace symbol_names name fn);
  Data.init_rule := (function _ -> Hashtbl.clear metavariables);
  Data.install_bindings :=
    (function parent ->
      List.iter (function (name,fn) -> Hashtbl.add metavariables name fn)
	(Hashtbl.find all_metavariables parent))

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

# 487 "lexer_cocci.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\175\255\176\255\081\000\119\000\181\255\182\255\192\000\
    \219\000\206\255\078\000\035\000\090\000\220\255\080\000\081\000\
    \224\255\225\255\228\255\229\255\230\255\231\255\233\255\082\000\
    \105\000\237\255\239\255\101\000\102\000\140\000\162\000\033\001\
    \108\001\095\000\097\000\084\000\183\001\138\000\255\255\041\002\
    \102\000\167\000\186\255\213\255\026\002\101\002\176\002\251\002\
    \070\003\145\003\220\003\039\004\114\004\189\004\008\005\122\005\
    \112\000\152\000\168\000\242\000\243\000\251\255\177\255\205\255\
    \249\255\204\255\248\255\197\005\016\006\091\006\166\006\241\006\
    \084\005\107\000\117\000\092\000\244\255\242\255\012\002\094\005\
    \060\007\093\000\117\000\135\000\141\000\144\000\245\255\145\000\
    \147\000\243\255\208\255\178\255\216\255\149\000\227\255\245\000\
    \215\255\153\000\019\001\232\255\234\255\236\255\198\255\210\255\
    \214\255\212\255\199\255\211\255\209\255\203\255\136\000\207\255\
    \097\000\089\000\116\000\123\000\104\000\112\000\109\007\193\255\
    \131\000\131\000\132\000\060\001\162\007\237\007\133\000\132\000\
    \128\000\149\000\061\001\095\008\178\008\190\255\039\007\153\000\
    \145\000\169\000\188\000\214\000\110\007\055\002\090\001\165\001\
    \189\255\056\002\188\255\056\001\155\005\110\007\154\005\084\001\
    \111\007\113\007\085\001\202\000\217\000\213\000\219\000\222\000\
    \086\001\087\001\224\000\228\000\119\001\221\000\219\000\159\001\
    \237\000\232\000\238\000\028\001\160\001\016\001\026\001\161\001\
    \046\007\096\001\253\008\124\007\147\007\085\008\024\009\066\008\
    \095\008\094\009\168\001\179\255\234\001\255\255\034\009\252\255\
    \164\009\107\007\134\007\254\255\187\009\253\255\235\001\254\255\
    \053\009\255\255\251\255\226\009\062\009\085\009\253\255\249\009\
    \252\255\042\007\254\255\025\010\255\255\251\255\049\010\132\009\
    \155\009\253\255\072\010\252\255\163\004\252\255\253\255\254\255\
    \119\001\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\076\000\076\000\255\255\255\255\072\000\
    \080\000\255\255\055\000\061\000\060\000\255\255\034\000\032\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\020\000\
    \080\000\255\255\255\255\015\000\014\000\054\000\029\000\072\000\
    \072\000\017\000\038\000\005\000\072\000\033\000\255\255\001\000\
    \255\255\002\000\255\255\255\255\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \255\255\255\255\255\255\255\255\003\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\072\000\072\000\008\000\072\000\072\000\
    \075\000\255\255\009\000\255\255\255\255\255\255\255\255\075\000\
    \255\255\053\000\059\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\037\000\255\255\070\000\
    \255\255\036\000\071\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\058\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\063\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\064\000\255\255\068\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\255\255\255\255\255\255\255\255\255\255\
    \068\000\068\000\255\255\255\255\068\000\255\255\255\255\068\000\
    \255\255\255\255\255\255\255\255\068\000\255\255\255\255\068\000\
    \076\000\076\000\255\255\075\000\255\255\255\255\076\000\075\000\
    \255\255\076\000\076\000\255\255\255\255\255\255\000\000\255\255\
    \003\000\001\000\001\000\255\255\002\000\255\255\255\255\255\255\
    \001\000\255\255\255\255\004\000\002\000\002\000\255\255\003\000\
    \255\255\255\255\255\255\001\000\255\255\255\255\004\000\002\000\
    \002\000\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \004\000\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\041\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\058\000\058\000\058\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\255\255\000\000\095\000\
    \000\000\255\255\098\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\147\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\145\000\143\000\143\000\
    \000\000\145\000\000\000\147\000\147\000\147\000\147\000\151\000\
    \147\000\147\000\154\000\255\255\255\255\255\255\255\255\255\255\
    \160\000\161\000\255\255\255\255\164\000\255\255\255\255\167\000\
    \255\255\255\255\255\255\255\255\172\000\255\255\255\255\175\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\189\000\000\000\191\000\000\000\
    \255\255\255\255\255\255\000\000\255\255\000\000\199\000\000\000\
    \202\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \000\000\210\000\000\000\213\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\255\255\000\000\221\000\000\000\000\000\000\000\
    \255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\039\000\038\000\038\000\038\000\038\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \039\000\033\000\005\000\008\000\000\000\014\000\012\000\006\000\
    \025\000\022\000\015\000\027\000\017\000\028\000\030\000\037\000\
    \004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\009\000\016\000\029\000\034\000\010\000\026\000\
    \035\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\032\000\
    \007\000\007\000\007\000\021\000\024\000\020\000\011\000\036\000\
    \108\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\031\000\
    \007\000\007\000\007\000\019\000\023\000\018\000\013\000\179\000\
    \106\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\109\000\110\000\105\000\104\000\103\000\
    \097\000\101\000\099\000\093\000\061\000\041\000\180\000\107\000\
    \058\000\074\000\077\000\091\000\065\000\176\000\063\000\062\000\
    \075\000\057\000\096\000\092\000\094\000\179\000\177\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\181\000\
    \181\000\255\255\090\000\076\000\042\000\087\000\180\000\083\000\
    \057\000\041\000\084\000\085\000\180\000\176\000\086\000\088\000\
    \056\000\089\000\095\000\176\000\098\000\111\000\177\000\043\000\
    \082\000\081\000\173\000\168\000\177\000\126\000\102\000\178\000\
    \073\000\059\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\180\000\066\000\120\000\064\000\
    \157\000\134\000\156\000\176\000\118\000\100\000\155\000\121\000\
    \122\000\135\000\123\000\127\000\177\000\128\000\129\000\178\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\130\000\118\000\136\000\137\000\119\000\255\255\
    \002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\060\000\060\000\255\255\138\000\007\000\
    \139\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\140\000\165\000\162\000\158\000\116\000\
    \114\000\161\000\255\255\160\000\115\000\124\000\131\000\112\000\
    \159\000\163\000\164\000\113\000\166\000\167\000\169\000\170\000\
    \117\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\171\000\124\000\131\000\255\255\255\255\
    \255\255\255\255\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\255\255\172\000\174\000\175\000\
    \007\000\255\255\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\070\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\225\000\255\255\
    \255\255\255\255\255\255\255\255\186\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\067\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\144\000\
    \000\000\000\000\000\000\007\000\186\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\000\000\255\255\255\255\187\000\255\255\000\000\000\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\201\000\255\255\187\000\000\000\044\000\000\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\039\000\038\000\038\000\038\000\038\000\080\000\
    \255\255\080\000\000\000\000\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\190\000\200\000\
    \000\000\039\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\255\255\255\255\255\255\255\255\
    \040\000\000\000\255\255\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\255\255\146\000\255\255\
    \000\000\007\000\000\000\045\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\255\255\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\007\000\000\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\046\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\000\000\000\000\000\000\000\000\007\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\047\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\000\000\000\000\
    \000\000\000\000\007\000\000\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\048\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\000\000\007\000\000\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\049\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\000\000\000\000\000\000\000\000\
    \007\000\000\000\007\000\050\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\000\000\
    \000\000\000\000\000\000\007\000\000\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\051\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\000\000\000\000\000\000\000\000\007\000\000\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\052\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\000\000\223\000\223\000\223\000\
    \223\000\000\000\000\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\224\000\222\000\000\000\
    \000\000\007\000\000\000\007\000\007\000\007\000\007\000\053\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\054\000\000\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\057\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \000\000\078\000\057\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\056\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\000\000\000\000\000\000\000\000\
    \000\000\078\000\000\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\000\000\000\000\000\000\
    \000\000\007\000\000\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\152\000\
    \151\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
    \007\000\007\000\068\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\007\000\000\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\069\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\000\000\000\000\000\000\000\000\007\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\000\000\000\000\
    \000\000\000\000\007\000\000\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\000\000\007\000\000\000\007\000\
    \007\000\007\000\007\000\071\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\212\000\000\000\000\000\000\000\
    \007\000\000\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\069\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\118\000\140\000\
    \255\255\255\255\187\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\187\000\000\000\000\000\211\000\000\000\
    \000\000\000\000\000\000\149\000\000\000\118\000\140\000\000\000\
    \142\000\000\000\000\000\000\000\000\000\148\000\000\000\000\000\
    \000\000\000\000\187\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\187\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\141\000\124\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\184\000\000\000\
    \184\000\078\000\124\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\000\000\000\000\000\000\
    \000\000\116\000\114\000\150\000\153\000\000\000\115\000\154\000\
    \000\000\112\000\000\000\000\000\000\000\113\000\000\000\000\000\
    \000\000\078\000\117\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\000\000\000\000\000\000\
    \000\000\125\000\000\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \000\000\000\000\000\000\000\000\125\000\000\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \131\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\000\000\000\000\000\000\131\000\
    \000\000\000\000\000\000\179\000\000\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\000\000\180\000\000\000\000\000\000\000\000\000\000\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\180\000\000\000\000\000\000\000\132\000\000\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\133\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\000\000\000\000\000\000\
    \000\000\132\000\000\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\000\000\000\000\179\000\000\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \181\000\181\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\000\000\000\000\000\000\180\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\176\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\177\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\205\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\180\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\176\000\206\000\206\000\206\000\
    \206\000\206\000\206\000\206\000\206\000\177\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \000\000\000\000\192\000\000\000\000\000\000\000\000\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\000\000\000\000\000\000\
    \000\000\000\000\176\000\000\000\000\000\203\000\000\000\000\000\
    \000\000\000\000\000\000\177\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\000\000\000\000\000\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\000\000\000\000\000\000\
    \000\000\000\000\176\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\177\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\000\000\000\000\000\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\207\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\207\000\207\000\207\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\255\255\207\000\207\000\207\000\207\000\207\000\
    \207\000\208\000\208\000\208\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\208\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\208\000\208\000\208\000\208\000\208\000\208\000\
    \000\000\000\000\000\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\208\000\208\000\208\000\208\000\208\000\208\000\
    \000\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\219\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\219\000\219\000\219\000\219\000\219\000\219\000\000\000\
    \000\000\214\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\219\000\219\000\219\000\219\000\219\000\219\000\000\000\
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
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \012\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\010\000\010\000\014\000\015\000\023\000\
    \027\000\024\000\024\000\028\000\035\000\040\000\003\000\012\000\
    \056\000\073\000\075\000\081\000\033\000\003\000\034\000\034\000\
    \074\000\057\000\027\000\028\000\028\000\004\000\003\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\041\000\082\000\074\000\037\000\083\000\003\000\029\000\
    \057\000\037\000\029\000\084\000\004\000\003\000\085\000\087\000\
    \057\000\088\000\093\000\004\000\097\000\110\000\003\000\037\000\
    \029\000\029\000\112\000\113\000\004\000\116\000\023\000\004\000\
    \030\000\058\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\004\000\033\000\117\000\034\000\
    \114\000\115\000\114\000\004\000\008\000\024\000\114\000\120\000\
    \121\000\115\000\122\000\126\000\004\000\127\000\128\000\004\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\129\000\008\000\135\000\136\000\008\000\095\000\
    \000\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\059\000\060\000\098\000\137\000\007\000\
    \138\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\139\000\155\000\156\000\157\000\008\000\
    \008\000\158\000\147\000\159\000\008\000\123\000\130\000\008\000\
    \157\000\162\000\163\000\008\000\165\000\166\000\168\000\169\000\
    \008\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\170\000\123\000\130\000\151\000\154\000\
    \160\000\161\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\142\000\171\000\173\000\174\000\
    \031\000\164\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\224\000\041\000\
    \058\000\167\000\172\000\175\000\177\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\143\000\
    \255\255\255\255\255\255\032\000\177\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\255\255\059\000\060\000\186\000\095\000\255\255\255\255\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\198\000\098\000\186\000\255\255\036\000\255\255\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\039\000\039\000\039\000\039\000\039\000\078\000\
    \147\000\078\000\255\255\255\255\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\188\000\198\000\
    \255\255\039\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\151\000\154\000\160\000\161\000\
    \039\000\255\255\142\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\141\000\145\000\164\000\
    \255\255\044\000\255\255\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\167\000\
    \172\000\175\000\255\255\255\255\255\255\143\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \255\255\255\255\255\255\255\255\045\000\255\255\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\188\000\198\000\255\255\255\255\255\255\255\255\
    \255\255\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\255\255\255\255\255\255\046\000\
    \255\255\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\255\255\255\255\141\000\
    \145\000\255\255\255\255\255\255\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\255\255\255\255\
    \255\255\255\255\047\000\255\255\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\255\255\255\255\255\255\255\255\048\000\255\255\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\255\255\255\255\255\255\255\255\
    \049\000\255\255\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\255\255\
    \255\255\255\255\255\255\050\000\255\255\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\255\255\255\255\255\255\255\255\051\000\255\255\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\255\255\220\000\220\000\220\000\
    \220\000\255\255\255\255\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\220\000\220\000\255\255\
    \255\255\052\000\255\255\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \255\255\255\255\255\255\255\255\053\000\255\255\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\255\255\255\255\255\255\255\255\054\000\
    \255\255\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\055\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \255\255\072\000\055\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\055\000\220\000\150\000\148\000\255\255\255\255\
    \255\255\255\255\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\255\255\255\255\255\255\255\255\
    \255\255\072\000\255\255\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\255\255\255\255\255\255\
    \255\255\055\000\255\255\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\148\000\
    \150\000\255\255\255\255\255\255\255\255\255\255\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \255\255\255\255\255\255\255\255\067\000\255\255\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\255\255\255\255\255\255\255\255\068\000\
    \255\255\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\255\255\255\255\255\255\
    \255\255\255\255\150\000\148\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\255\255\255\255\
    \255\255\255\255\069\000\255\255\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\255\255\255\255\255\255\255\255\070\000\255\255\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\134\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\209\000\255\255\255\255\255\255\
    \071\000\255\255\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\118\000\140\000\
    \149\000\152\000\176\000\153\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\176\000\255\255\255\255\209\000\255\255\
    \255\255\255\255\255\255\134\000\255\255\118\000\140\000\255\255\
    \140\000\255\255\255\255\255\255\255\255\134\000\255\255\255\255\
    \255\255\255\255\176\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\176\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\140\000\124\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\194\000\194\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\180\000\255\255\
    \180\000\179\000\124\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\255\255\255\255\255\255\
    \255\255\118\000\118\000\149\000\152\000\255\255\118\000\153\000\
    \255\255\118\000\255\255\255\255\255\255\118\000\255\255\255\255\
    \255\255\179\000\118\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\255\255\255\255\255\255\
    \255\255\124\000\255\255\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\134\000\
    \255\255\255\255\209\000\255\255\255\255\255\255\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \255\255\255\255\255\255\255\255\125\000\255\255\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \131\000\255\255\255\255\255\255\255\255\255\255\149\000\152\000\
    \255\255\153\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\255\255\255\255\255\255\131\000\
    \255\255\255\255\255\255\181\000\255\255\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\255\255\181\000\255\255\255\255\255\255\255\255\255\255\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\181\000\255\255\255\255\255\255\131\000\255\255\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\132\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\255\255\255\255\255\255\
    \255\255\132\000\255\255\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\178\000\178\000\
    \178\000\178\000\178\000\178\000\255\255\255\255\182\000\255\255\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\255\255\255\255\255\255\182\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\182\000\200\000\200\000\200\000\
    \200\000\200\000\200\000\200\000\200\000\182\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\182\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\182\000\205\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\182\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \255\255\255\255\190\000\255\255\255\255\255\255\255\255\185\000\
    \185\000\185\000\185\000\185\000\185\000\255\255\255\255\255\255\
    \255\255\255\255\185\000\255\255\255\255\200\000\255\255\255\255\
    \255\255\255\255\255\255\185\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\255\255\255\255\255\255\185\000\
    \185\000\185\000\185\000\185\000\185\000\255\255\255\255\255\255\
    \255\255\255\255\185\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\185\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\192\000\192\000\192\000\
    \192\000\192\000\192\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\196\000\196\000\196\000\196\000\
    \196\000\196\000\255\255\255\255\255\255\192\000\192\000\192\000\
    \192\000\192\000\192\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\203\000\203\000\203\000\203\000\203\000\203\000\
    \203\000\203\000\203\000\203\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\190\000\203\000\203\000\203\000\203\000\203\000\
    \203\000\207\000\207\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\207\000\207\000\255\255\255\255\200\000\255\255\255\255\
    \255\255\255\255\207\000\207\000\207\000\207\000\207\000\207\000\
    \255\255\255\255\255\255\203\000\203\000\203\000\203\000\203\000\
    \203\000\211\000\211\000\211\000\211\000\211\000\211\000\211\000\
    \211\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\207\000\207\000\207\000\207\000\207\000\207\000\
    \255\255\214\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \214\000\214\000\214\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\214\000\214\000\214\000\214\000\214\000\214\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\218\000\218\000\218\000\218\000\218\000\218\000\255\255\
    \255\255\211\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\218\000\218\000\218\000\218\000\218\000\218\000\255\255\
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
    \255\255\211\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
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
    \000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\004\000\005\000\000\000\021\000\000\000\000\000\
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
    \000\000\000\000";
  Lexing.lex_backtrk_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
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
    \000\000\000\000";
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
    \000\000\000\000";
  Lexing.lex_trans_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\007\000\007\000\013\000\013\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\001\000\007\000\007\000\013\000\013\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check_code = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\039\000\123\000\124\000\130\000\131\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\039\000\123\000\124\000\130\000\131\000\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_code = 
   "\255\001\255\255\000\001\255\002\255\255\000\002\255\004\255\003\
    \255\255\000\003\255\000\004\255";
}

let rec token lexbuf =
  lexbuf.Lexing.lex_mem <- Array.create 5 (-1) ; (* L=1 [1] <- p ;  *)
  lexbuf.Lexing.lex_mem.(1) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 508 "lexer_cocci.mll"
    ( let cls = !current_line_started in

      if not cls
      then
	begin
	  match !current_line_type with
	    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	      let info = get_current_line_type lexbuf in
	      reset_line lexbuf;
	      TPragma (Ast.Noindent "", info)
	  | _ -> reset_line lexbuf; token lexbuf
	end
      else (reset_line lexbuf; token lexbuf) )
# 1489 "lexer_cocci.ml"

  | 1 ->
# 522 "lexer_cocci.mll"
                   ( start_line false; token lexbuf )
# 1494 "lexer_cocci.ml"

  | 2 ->
let
# 524 "lexer_cocci.mll"
                                       after
# 1500 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 524 "lexer_cocci.mll"
                                              (
    match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	let str =
	  if !current_line_started
	  then (tok lexbuf)
	  else after in
	start_line true;
	TPragma (Ast.Indent str, get_current_line_type lexbuf)
    | _ -> start_line false; token lexbuf )
# 1513 "lexer_cocci.ml"

  | 3 ->
# 536 "lexer_cocci.mll"
   ( match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	start_line true;
	TPragma (Ast.Space (tok lexbuf), get_current_line_type lexbuf)
    | _ -> failwith "attributes only allowedin + code" )
# 1522 "lexer_cocci.ml"

  | 4 ->
# 542 "lexer_cocci.mll"
         ( start_line true; TArobArob )
# 1527 "lexer_cocci.ml"

  | 5 ->
# 543 "lexer_cocci.mll"
         ( pass_zero();
	   if !Data.in_rule_name or not !current_line_started
	   then (start_line true; TArob)
	   else (check_minus_context_linetype "@";
		 TPArob (get_current_line_type lexbuf)) )
# 1536 "lexer_cocci.ml"

  | 6 ->
# 549 "lexer_cocci.mll"
          ( start_line true; TTildeEq (get_current_line_type lexbuf) )
# 1541 "lexer_cocci.ml"

  | 7 ->
# 550 "lexer_cocci.mll"
         ( start_line true; TTildeExclEq (get_current_line_type lexbuf) )
# 1546 "lexer_cocci.ml"

  | 8 ->
# 552 "lexer_cocci.mll"
      ( start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen (get_current_line_type lexbuf) )
# 1552 "lexer_cocci.ml"

  | 9 ->
# 556 "lexer_cocci.mll"
      ( start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis (get_current_line_type lexbuf) )
# 1558 "lexer_cocci.ml"

  | 10 ->
# 567 "lexer_cocci.mll"
           ( start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis (get_current_line_type lexbuf) )
# 1564 "lexer_cocci.ml"

  | 11 ->
# 569 "lexer_cocci.mll"
           ( start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis (get_current_line_type lexbuf) )
# 1570 "lexer_cocci.ml"

  | 12 ->
# 571 "lexer_cocci.mll"
            ( start_line true; check_minus_context_linetype (tok lexbuf);
	     TPOEllipsis (get_current_line_type lexbuf) )
# 1576 "lexer_cocci.ml"

  | 13 ->
# 573 "lexer_cocci.mll"
            ( start_line true; check_minus_context_linetype (tok lexbuf);
	     TPCEllipsis (get_current_line_type lexbuf) )
# 1582 "lexer_cocci.ml"

  | 14 ->
# 586 "lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TMinus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.MINUS; token lexbuf) )
# 1591 "lexer_cocci.ml"

  | 15 ->
# 591 "lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TPlus (get_current_line_type lexbuf))
          else if !Data.in_meta
	  then TPlus0
          else (patch_or_match PATCH;
		add_current_line_type D.PLUS; token lexbuf) )
# 1602 "lexer_cocci.ml"

  | 16 ->
# 598 "lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TWhy (get_current_line_type lexbuf))
          else if !Data.in_meta
	  then TWhy0
          else (add_current_line_type D.OPT; token lexbuf) )
# 1612 "lexer_cocci.ml"

  | 17 ->
# 604 "lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true; TBang (get_current_line_type lexbuf))
          else if !Data.in_meta
	  then TBang0
          else (add_current_line_type D.UNIQUE; token lexbuf) )
# 1622 "lexer_cocci.ml"

  | 18 ->
# 610 "lexer_cocci.mll"
        ( if !Data.in_meta or not !col_zero
	  then (start_line true; TOPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 (get_current_line_type lexbuf)))
# 1631 "lexer_cocci.ml"

  | 19 ->
# 615 "lexer_cocci.mll"
          ( start_line true;
	    TOPar0 (contextify(get_current_line_type lexbuf)) )
# 1637 "lexer_cocci.ml"

  | 20 ->
# 617 "lexer_cocci.mll"
        ( if not (!col_zero)
	  then (start_line true; TOr(get_current_line_type lexbuf))
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 (get_current_line_type lexbuf)))
# 1646 "lexer_cocci.ml"

  | 21 ->
# 622 "lexer_cocci.mll"
          ( start_line true;
	    TMid0 (contextify(get_current_line_type lexbuf)) )
# 1652 "lexer_cocci.ml"

  | 22 ->
# 624 "lexer_cocci.mll"
        ( if not !col_zero
	  then (start_line true; TCPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 (get_current_line_type lexbuf)))
# 1661 "lexer_cocci.ml"

  | 23 ->
# 629 "lexer_cocci.mll"
          ( start_line true;
	    TCPar0 (contextify(get_current_line_type lexbuf)) )
# 1667 "lexer_cocci.ml"

  | 24 ->
# 632 "lexer_cocci.mll"
        ( start_line true; TOCro (get_current_line_type lexbuf)   )
# 1672 "lexer_cocci.ml"

  | 25 ->
# 633 "lexer_cocci.mll"
        ( start_line true; TCCro (get_current_line_type lexbuf)   )
# 1677 "lexer_cocci.ml"

  | 26 ->
# 634 "lexer_cocci.mll"
        ( start_line true; TOBrace (get_current_line_type lexbuf) )
# 1682 "lexer_cocci.ml"

  | 27 ->
# 635 "lexer_cocci.mll"
        ( start_line true; TCBrace (get_current_line_type lexbuf) )
# 1687 "lexer_cocci.ml"

  | 28 ->
# 637 "lexer_cocci.mll"
                   ( start_line true; TPtrOp (get_current_line_type lexbuf)  )
# 1692 "lexer_cocci.ml"

  | 29 ->
# 638 "lexer_cocci.mll"
                   ( start_line true; TDot (get_current_line_type lexbuf)    )
# 1697 "lexer_cocci.ml"

  | 30 ->
# 639 "lexer_cocci.mll"
                   ( start_line true; TComma (get_current_line_type lexbuf)  )
# 1702 "lexer_cocci.ml"

  | 31 ->
# 640 "lexer_cocci.mll"
                   ( start_line true;
		     if !Data.in_meta
		     then TMPtVirg (* works better with tokens_all *)
		     else TPtVirg (get_current_line_type lexbuf) )
# 1710 "lexer_cocci.ml"

  | 32 ->
# 646 "lexer_cocci.mll"
                   ( pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS; token lexbuf) )
# 1721 "lexer_cocci.ml"

  | 33 ->
# 653 "lexer_cocci.mll"
                   ( start_line true;
		     TDmOp (Ast.Div,get_current_line_type lexbuf) )
# 1727 "lexer_cocci.ml"

  | 34 ->
# 655 "lexer_cocci.mll"
                   ( start_line true;
		     TDmOp (Ast.Mod,get_current_line_type lexbuf) )
# 1733 "lexer_cocci.ml"

  | 35 ->
# 657 "lexer_cocci.mll"
                   ( start_line true;  TTilde (get_current_line_type lexbuf) )
# 1738 "lexer_cocci.ml"

  | 36 ->
# 659 "lexer_cocci.mll"
                   ( pass_zero();
 		     if !current_line_started
 		     then
 		       (start_line true; TInc (get_current_line_type lexbuf))
 		     else (patch_or_match PATCH;
 			   add_current_line_type D.PLUSPLUS; token lexbuf) )
# 1748 "lexer_cocci.ml"

  | 37 ->
# 665 "lexer_cocci.mll"
                   ( start_line true;  TDec (get_current_line_type lexbuf) )
# 1753 "lexer_cocci.ml"

  | 38 ->
# 667 "lexer_cocci.mll"
                   ( start_line true; TEq (get_current_line_type lexbuf) )
# 1758 "lexer_cocci.ml"

  | 39 ->
# 669 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Minus lexbuf )
# 1763 "lexer_cocci.ml"

  | 40 ->
# 670 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Plus lexbuf )
# 1768 "lexer_cocci.ml"

  | 41 ->
# 672 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Mul lexbuf )
# 1773 "lexer_cocci.ml"

  | 42 ->
# 673 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Div lexbuf )
# 1778 "lexer_cocci.ml"

  | 43 ->
# 674 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Mod lexbuf )
# 1783 "lexer_cocci.ml"

  | 44 ->
# 676 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.And lexbuf )
# 1788 "lexer_cocci.ml"

  | 45 ->
# 677 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Or lexbuf )
# 1793 "lexer_cocci.ml"

  | 46 ->
# 678 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.Xor lexbuf )
# 1798 "lexer_cocci.ml"

  | 47 ->
# 680 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.DecLeft lexbuf )
# 1803 "lexer_cocci.ml"

  | 48 ->
# 681 "lexer_cocci.mll"
                   ( start_line true; mkassign Ast.DecRight lexbuf )
# 1808 "lexer_cocci.ml"

  | 49 ->
# 683 "lexer_cocci.mll"
                   ( start_line true; TDotDot (get_current_line_type lexbuf) )
# 1813 "lexer_cocci.ml"

  | 50 ->
# 685 "lexer_cocci.mll"
                   ( start_line true; TEqEq    (get_current_line_type lexbuf) )
# 1818 "lexer_cocci.ml"

  | 51 ->
# 686 "lexer_cocci.mll"
                   ( start_line true; TNotEq   (get_current_line_type lexbuf) )
# 1823 "lexer_cocci.ml"

  | 52 ->
# 687 "lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.SupEq,get_current_line_type lexbuf) )
# 1829 "lexer_cocci.ml"

  | 53 ->
# 689 "lexer_cocci.mll"
                   ( start_line true;
		     if !Data.in_meta
		     then TSub(get_current_line_type lexbuf)
		     else TLogOp(Ast.InfEq,get_current_line_type lexbuf) )
# 1837 "lexer_cocci.ml"

  | 54 ->
# 693 "lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.Inf,get_current_line_type lexbuf) )
# 1843 "lexer_cocci.ml"

  | 55 ->
# 695 "lexer_cocci.mll"
                   ( start_line true;
		     TLogOp(Ast.Sup,get_current_line_type lexbuf) )
# 1849 "lexer_cocci.ml"

  | 56 ->
# 698 "lexer_cocci.mll"
                   ( start_line true; TAndLog (get_current_line_type lexbuf) )
# 1854 "lexer_cocci.ml"

  | 57 ->
# 699 "lexer_cocci.mll"
                   ( start_line true; TOrLog  (get_current_line_type lexbuf) )
# 1859 "lexer_cocci.ml"

  | 58 ->
# 701 "lexer_cocci.mll"
                   ( start_line true;
		     TShROp(Ast.DecRight,get_current_line_type lexbuf) )
# 1865 "lexer_cocci.ml"

  | 59 ->
# 703 "lexer_cocci.mll"
                   ( start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) )
# 1871 "lexer_cocci.ml"

  | 60 ->
# 706 "lexer_cocci.mll"
                   ( start_line true; TAnd    (get_current_line_type lexbuf) )
# 1876 "lexer_cocci.ml"

  | 61 ->
# 707 "lexer_cocci.mll"
                   ( start_line true; TXor(get_current_line_type lexbuf) )
# 1881 "lexer_cocci.ml"

  | 62 ->
# 709 "lexer_cocci.mll"
                    ( start_line true; TCppConcatOp )
# 1886 "lexer_cocci.ml"

  | 63 ->
let
# 710 "lexer_cocci.mll"
                                                  def
# 1892 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 711 "lexer_cocci.mll"
                                   ident
# 1897 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 712 "lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,offset,col,strbef,straft,pos) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TUndef
	  (lt,
	   check_var ident
	     (arity,line,lline,offset+off,col+off,[],[],[])) )
# 1909 "lexer_cocci.ml"

  | 64 ->
let
# 721 "lexer_cocci.mll"
                                                   def
# 1915 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 722 "lexer_cocci.mll"
                                   ident
# 1920 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 723 "lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,offset,col,strbef,straft,pos) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TDefine
	  (lt,
	   check_var ident
	     (arity,line,lline,offset+off,col+off,[],[],[])) )
# 1932 "lexer_cocci.ml"

  | 65 ->
let
# 732 "lexer_cocci.mll"
                                                   def
# 1938 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 733 "lexer_cocci.mll"
                                    ident
# 1943 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) (lexbuf.Lexing.lex_curr_pos + -1) in
# 735 "lexer_cocci.mll"
      ( start_line true;
	let (arity,line,lline,offset,col,strbef,straft,pos) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	TDefineParam
        (lt,
	 check_var ident
	   (* why pos here but not above? *)
	   (arity,line,lline,offset+off,col+off,strbef,straft,pos),
	 offset + off + (String.length ident),
	 col + off + (String.length ident)) )
# 1957 "lexer_cocci.ml"

  | 66 ->
# 747 "lexer_cocci.mll"
      ( TIncludeL
	  (let str = tok lexbuf in
	  let start = String.index str '"' in
	  let finish = String.rindex str '"' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) )
# 1967 "lexer_cocci.ml"

  | 67 ->
# 754 "lexer_cocci.mll"
      ( TIncludeNL
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) )
# 1977 "lexer_cocci.ml"

  | 68 ->
# 769 "lexer_cocci.mll"
      ( start_line true; check_plus_linetype (tok lexbuf);
	TPragma (Ast.Noindent(tok lexbuf), get_current_line_type lexbuf) )
# 1983 "lexer_cocci.ml"

  | 69 ->
# 772 "lexer_cocci.mll"
      (
       match !current_line_type with
        (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
        start_line true;
	(* second argument to TPragma is not quite right, because
	   it represents only the first token of the comment, but that
	   should be good enough *)
	TPragma (Ast.Indent("/*"^(comment check_comment lexbuf)),
		 get_current_line_type lexbuf)
      |	_ -> let _ = comment (fun _ -> ()) lexbuf in token lexbuf )
# 1997 "lexer_cocci.ml"

  | 70 ->
# 783 "lexer_cocci.mll"
      ( (if !current_line_started
      then lexerr "--- must be at the beginning of the line" "");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) )
# 2008 "lexer_cocci.ml"

  | 71 ->
# 791 "lexer_cocci.mll"
      ( (if !current_line_started
      then lexerr "+++ must be at the beginning of the line" "");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) )
# 2019 "lexer_cocci.ml"

  | 72 ->
# 800 "lexer_cocci.mll"
      ( start_line true; id_tokens lexbuf )
# 2024 "lexer_cocci.ml"

  | 73 ->
# 802 "lexer_cocci.mll"
        ( start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) )
# 2030 "lexer_cocci.ml"

  | 74 ->
# 804 "lexer_cocci.mll"
        ( start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) )
# 2036 "lexer_cocci.ml"

  | 75 ->
let
# 806 "lexer_cocci.mll"
             x
# 2042 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 806 "lexer_cocci.mll"
                   ( start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) )
# 2047 "lexer_cocci.ml"

  | 76 ->
let
# 816 "lexer_cocci.mll"
         x
# 2053 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 816 "lexer_cocci.mll"
            ( start_line true; TInt(x,(get_current_line_type lexbuf)) )
# 2057 "lexer_cocci.ml"

  | 77 ->
# 818 "lexer_cocci.mll"
                   ( TIso )
# 2062 "lexer_cocci.ml"

  | 78 ->
# 819 "lexer_cocci.mll"
                   ( TRightIso )
# 2067 "lexer_cocci.ml"

  | 79 ->
# 821 "lexer_cocci.mll"
                   ( EOF )
# 2072 "lexer_cocci.ml"

  | 80 ->
# 823 "lexer_cocci.mll"
      ( lexerr "unrecognised symbol, in token rule: " (tok lexbuf) )
# 2077 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and char lexbuf =
    __ocaml_lex_char_rec lexbuf 188
and __ocaml_lex_char_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 827 "lexer_cocci.mll"
          x
# 2089 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 827 "lexer_cocci.mll"
                                       ( String.make 1 x ^ restchars lexbuf )
# 2093 "lexer_cocci.ml"

  | 1 ->
let
# 829 "lexer_cocci.mll"
                                             x
# 2099 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 829 "lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 2103 "lexer_cocci.ml"

  | 2 ->
let
# 833 "lexer_cocci.mll"
                                  x
# 2109 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 833 "lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 2113 "lexer_cocci.ml"

  | 3 ->
let
# 834 "lexer_cocci.mll"
                 v
# 2119 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 834 "lexer_cocci.mll"
                                  x
# 2124 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 835 "lexer_cocci.mll"
 (
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x ^ restchars lexbuf
	)
# 2138 "lexer_cocci.ml"

  | 4 ->
# 847 "lexer_cocci.mll"
      ( Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      )
# 2145 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_char_rec lexbuf __ocaml_lex_state

and restchars lexbuf =
    __ocaml_lex_restchars_rec lexbuf 198
and __ocaml_lex_restchars_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 852 "lexer_cocci.mll"
                                       ( "" )
# 2156 "lexer_cocci.ml"

  | 1 ->
let
# 853 "lexer_cocci.mll"
          x
# 2162 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 853 "lexer_cocci.mll"
                                       ( String.make 1 x ^ restchars lexbuf )
# 2166 "lexer_cocci.ml"

  | 2 ->
let
# 855 "lexer_cocci.mll"
                                             x
# 2172 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 855 "lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 2176 "lexer_cocci.ml"

  | 3 ->
let
# 859 "lexer_cocci.mll"
                                  x
# 2182 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 859 "lexer_cocci.mll"
                                                     ( x ^ restchars lexbuf )
# 2186 "lexer_cocci.ml"

  | 4 ->
let
# 860 "lexer_cocci.mll"
                 v
# 2192 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 860 "lexer_cocci.mll"
                                  x
# 2197 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 861 "lexer_cocci.mll"
 (
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x ^ restchars lexbuf
	)
# 2211 "lexer_cocci.ml"

  | 5 ->
# 873 "lexer_cocci.mll"
      ( Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      )
# 2218 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_restchars_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 209
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 878 "lexer_cocci.mll"
                                              ( "" )
# 2229 "lexer_cocci.ml"

  | 1 ->
let
# 879 "lexer_cocci.mll"
          x
# 2235 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 879 "lexer_cocci.mll"
                               ( Common.string_of_char x ^ string lexbuf )
# 2239 "lexer_cocci.ml"

  | 2 ->
let
# 880 "lexer_cocci.mll"
                                            x
# 2245 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 880 "lexer_cocci.mll"
                                              ( x ^ string lexbuf )
# 2249 "lexer_cocci.ml"

  | 3 ->
let
# 881 "lexer_cocci.mll"
                               x
# 2255 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 881 "lexer_cocci.mll"
                                              ( x ^ string lexbuf )
# 2259 "lexer_cocci.ml"

  | 4 ->
let
# 882 "lexer_cocci.mll"
                v
# 2265 "lexer_cocci.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 882 "lexer_cocci.mll"
                       x
# 2270 "lexer_cocci.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 883 "lexer_cocci.mll"
       (
         (match v with
	    | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
	    | 'f' -> () | 'a' -> ()
	    | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
	    | 'e' -> ()
	    | '\n' -> ()
	    | '(' -> () | '|' -> () | ')' -> ()
	    | _ -> lexerr "unrecognised symbol:" (tok lexbuf)
	 );
          x ^ string lexbuf
       )
# 2285 "lexer_cocci.ml"

  | 5 ->
# 895 "lexer_cocci.mll"
      ( lexerr "unrecognised symbol: " (tok lexbuf) )
# 2290 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and comment check_comment lexbuf =
    __ocaml_lex_comment_rec check_comment lexbuf 220
and __ocaml_lex_comment_rec check_comment lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 898 "lexer_cocci.mll"
         ( let s = tok lexbuf in check_comment s; start_line true; s )
# 2301 "lexer_cocci.ml"

  | 1 ->
# 900 "lexer_cocci.mll"
      ( let s = tok lexbuf in
        (* even blank line should have a + *)
        check_comment s;
        reset_line lexbuf; s ^ comment check_comment lexbuf )
# 2309 "lexer_cocci.ml"

  | 2 ->
# 904 "lexer_cocci.mll"
        ( pass_zero();
	  if !current_line_started
	  then (start_line true;
		let s = tok lexbuf in s^(comment check_comment lexbuf))
	  else (start_line true; comment check_comment lexbuf) )
# 2318 "lexer_cocci.ml"

  | 3 ->
# 911 "lexer_cocci.mll"
      ( let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf )
# 2324 "lexer_cocci.ml"

  | 4 ->
# 914 "lexer_cocci.mll"
      ( let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf )
# 2330 "lexer_cocci.ml"

  | 5 ->
# 917 "lexer_cocci.mll"
      ( start_line true; let s = tok lexbuf in
        Common.pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment check_comment lexbuf
      )
# 2338 "lexer_cocci.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec check_comment lexbuf __ocaml_lex_state

;;

