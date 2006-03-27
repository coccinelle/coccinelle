(* splits the entire file into minus and plus fragments, and parses each
separately (thus duplicating work for the parsing of the context elements) *)

module D = Data
module PC = Parser_cocci
let pr = Printf.sprintf
(*let pr2 s = prerr_string s; prerr_string "\n"; flush stderr*)
let pr2 s = Printf.printf "%s\n" s

(* ----------------------------------------------------------------------- *)
(* Debugging... *)

let token2c (tok,_) =
  match tok with
    PC.TIdentifier -> "identifier"
  | PC.TType -> "type"
  | PC.TParameter -> "parameter"
  | PC.TConstant -> "constant"
  | PC.TExpression -> "expression"
  | PC.TStatement -> "statement"
  | PC.TFunction -> "function"
  | PC.TLocal -> "local"
  | PC.Tlist -> "list"
  | PC.TFresh -> "fresh"

  | PC.Tchar(clt) -> "char"
  | PC.Tshort(clt) -> "short"
  | PC.Tint(clt) -> "int"
  | PC.Tdouble(clt) -> "double"
  | PC.Tfloat(clt) -> "float"
  | PC.Tlong(clt) -> "long"
  | PC.Tvoid(clt) -> "void"
  | PC.Tstruct(clt) -> "struct"
  | PC.Tunion(clt) -> "union"
  | PC.Tunsigned(clt) -> "unsigned"
  | PC.Tsigned(clt) -> "signed"
  | PC.Tstatic(clt) -> "static"
  | PC.Tconst(clt) -> "const"
  | PC.Tvolatile(clt) -> "volatile"

  | PC.TInclude(s,clt) -> (pr "#include %s" s)
  | PC.TMinusFile(s,clt) -> (pr "--- %s" s)
  | PC.TPlusFile(s,clt) -> (pr "+++ %s" s)

  | PC.TInc(clt) -> "++"
  | PC.TDec(clt) -> "--"
	
  | PC.TIf(clt) -> "if"
  | PC.TElse(clt) -> "else"
  | PC.TWhile(clt) -> "while"
  | PC.TFor(clt) -> "for"
  | PC.TDo(clt) -> "do"
  | PC.TReturn(clt) -> "return"
  | PC.TIdent(s,clt) -> (pr "ident-%s" s)
  | PC.TFunName(s,clt) -> (pr "funname-%s" s)

  | PC.TString(x,clt) -> x
  | PC.TChar(x,clt) -> x
  | PC.TFloat(x,clt) -> x
  | PC.TInt(x,clt) -> x

  | PC.TOrLog(clt) -> "||"
  | PC.TAndLog(clt) -> "&&"
  | PC.TOr(clt) -> "|"
  | PC.TXor(clt) -> "^"
  | PC.TAnd (clt) -> "&"
  | PC.TEqEq(clt) -> "=="
  | PC.TNotEq(clt) -> "!="
  | PC.TInf(clt) -> "<"
  | PC.TSup(clt) -> ">"
  | PC.TInfEq(clt) -> "<="
  | PC.TSupEq (clt) -> ">="
  | PC.TShl(clt) -> "<<"
  | PC.TShr(clt) -> ">>"
  | PC.TPlus(clt) -> "+"
  | PC.TMinus(clt) -> "-"
  | PC.TMul(clt) -> "*"
  | PC.TDiv(clt) -> "/"
  | PC.TMod (clt) -> "%"

  | PC.TMetaFunName(s,clt) -> (pr "metafunname-%s" s)
  | PC.TMetaParam(_,clt) -> "parammeta"
  | PC.TMetaParamList(_,clt) -> "paramlistmeta"
  | PC.TMetaConst(_,_,clt) -> "constmeta"
  | PC.TMetaExp(_,_,clt) -> "expmeta"
  | PC.TMetaExpList(_,clt) -> "explistmeta"
  | PC.TMetaId(_,clt)    -> "idmeta"
  | PC.TMetaType(_,clt)    -> "typemeta"
  | PC.TMetaStm(_,clt)   -> "stmmeta"
  | PC.TMetaStmList(_,clt)   -> "stmlistmeta"
  | PC.TMetaFunc(_,clt)  -> "funcmeta"
  | PC.TMetaLocalFunc(_,clt) -> "funcmeta"
  | PC.TArobArob -> "@@"

  | PC.TWhen(clt) -> "WHEN"
  | PC.TEllipsis(clt) -> "..."
  | PC.TCircles(clt)  -> "ooo"
  | PC.TStars(clt)    -> "***"

  | PC.TOEllipsis(clt) -> "<..."
  | PC.TCEllipsis(clt) -> "...>"
  | PC.TOCircles(clt)  -> "<ooo"
  | PC.TCCircles(clt)  -> "ooo>"
  | PC.TOStars(clt)    -> "<***"
  | PC.TCStars(clt)    -> "***>"
  | PC.TBang0 -> "!"
  | PC.TPlus0 -> "+"
  | PC.TWhy0  -> "?"

  | PC.TWhy(clt)   -> "?"
  | PC.TDotDot(clt)   -> ":"
  | PC.TBang(clt)  -> "!"
  | PC.TOPar(clt)  -> "("
  | PC.TOPar0(clt) -> "("
  | PC.TMid(clt)   -> "|"
  | PC.TMid0(clt)  -> "|"
  | PC.TCPar(clt)  -> ")"
  | PC.TCPar0(clt) -> ")"

  | PC.TOBrace(clt) -> "{"
  | PC.TCBrace(clt) -> "}"
  | PC.TOCro(clt) -> "["
  | PC.TCCro(clt) -> "]"

  | PC.TPtrOp(clt) -> "->"

  | PC.TEq(clt) -> "="
  | PC.TAssign(_,clt) -> "=op"
  | PC.TDot(clt) -> "."
  | PC.TComma(clt) -> ","
  | PC.TPtVirg(clt) -> ";"

  | PC.EOF -> "eof"
  | PC.TLineEnd -> "line end"

(* ----------------------------------------------------------------------- *)
(* Read tokens *)

let wrap_lexbuf_info lexbuf =
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let tokens_all table file get_ats lexbuf :
    (bool * ((PC.token * (string * (int * int) * (int * int))) list)) =
  try 
    let rec aux () = 
      let result = Lexer_cocci.token lexbuf in
      let info = (Lexing.lexeme lexbuf, 
                  (table.(Lexing.lexeme_start lexbuf)),
                  (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) in
      if result = PC.EOF 
      then
	if get_ats
	then failwith "unexpected end of file in a metavariable declaration"
	else (false,[(result,info)])
      else if result = PC.TArobArob
      then (true,[(result,info)])
      else
	let (more,rest) = aux() in
	(more,(result, info)::rest)
    in aux () 
  with
    e -> pr2 (Common.error_message file (wrap_lexbuf_info lexbuf) ); raise e

(* ----------------------------------------------------------------------- *)
(* Split tokens into minus and plus fragments *)

let split t = function
    (D.MINUS,_,_) | (D.OPTMINUS,_,_) | (D.UNIQUEMINUS,_,_)
  | (D.MULTIMINUS,_,_) -> ([t],[])
  | (D.PLUS,_,_) -> ([],[t])
  | (D.CONTEXT,_,_) | (D.UNIQUE,_,_) | (D.OPT,_,_) | (D.MULTI,_,_) -> ([t],[t])

let split_token ((tok,_) as t) =
  match tok with
    PC.TIdentifier | PC.TConstant | PC.TExpression | PC.TStatement
  | PC.TFunction
  | PC.TType | PC.TParameter | PC.TLocal | PC.Tlist | PC.TFresh -> ([t],[t])

  | PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt) | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tstatic(clt) | PC.Tconst(clt) | PC.Tvolatile(clt) -> split t clt

  | PC.TPlusFile(s,clt) | PC.TMinusFile(s,clt) | PC.TInclude(s,clt) ->
      split t clt

  | PC.TIf(clt) | PC.TElse(clt)  | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TReturn(clt) | PC.TIdent(_,clt)
  | PC.TFunName(_,clt) | PC.TMetaFunName(_,clt)
  | PC.TMetaConst(_,_,clt) | PC.TMetaExp(_,_,clt) | PC.TMetaExpList(_,clt)
  | PC.TMetaParam(_,clt) | PC.TMetaParamList(_,clt)
  | PC.TMetaId(_,clt) | PC.TMetaType(_,clt)
  | PC.TMetaStm(_,clt) | PC.TMetaStmList(_,clt)
  | PC.TMetaFunc(_,clt) | PC.TMetaLocalFunc(_,clt) -> split t clt
  | PC.TArobArob -> ([t],[t])

  | PC.TWhen(clt)
  | PC.TEllipsis(clt) | PC.TCircles(clt) | PC.TStars(clt) -> split t clt

  | PC.TOEllipsis(_) | PC.TCEllipsis(_) (* clt must be context *)
  | PC.TOCircles(_) | PC.TCCircles(_)   (* clt must be context *)
  | PC.TOStars(_) | PC.TCStars(_)       (* clt must be context *)
  | PC.TBang0 | PC.TPlus0 | PC.TWhy0 ->
      ([t],[t])

  | PC.TWhy(clt)  | PC.TDotDot(clt)
  | PC.TBang(clt) | PC.TOPar(clt) | PC.TOPar0(clt) | PC.TMid(clt)
  | PC.TMid0(clt) | PC.TCPar(clt) | PC.TCPar0(clt) -> split t clt

  | PC.TInc(clt) | PC.TDec(clt) -> split t clt

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt) ->
      split t clt

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TInf(clt)
  | PC.TSup(clt) | PC.TInfEq(clt) | PC.TSupEq (clt) | PC.TShl(clt)
  | PC.TShr(clt) | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDiv(clt) | PC.TMod (clt) -> split t clt

  | PC.TOBrace(clt) | PC.TCBrace(clt) -> split t clt
  | PC.TOCro(clt) | PC.TCCro(clt) -> split t clt

  | PC.TPtrOp(clt) -> split t clt

  | PC.TEq(clt) | PC.TAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPtVirg(clt) -> split t clt

  | PC.EOF | PC.TLineEnd -> ([t],[t])

let split_token_stream tokens =
  let rec loop = function
      [] -> ([],[])
    | token::tokens ->
	let (minus,plus) = split_token token in
	let (minus_stream,plus_stream) = loop tokens in
	(minus@minus_stream,plus@plus_stream) in
  loop tokens

(* ----------------------------------------------------------------------- *)
(* Find function names *)
(* This addresses a shift-reduce problem in the parser, allowing us to
distinguish a function declaration from a function call even if the latter
has no return typed.  Undoubtedly, this is not very nice, but it doesn't
seem very convenient to refactor the grammar to get around the problem. *)

let rec find_function_names = function
    [] -> []
  | ((PC.TIdent(s,clt),info) as t1) :: ((PC.TOPar(_),_) as t2) :: rest
  | ((PC.TMetaLocalFunc(s,clt),info) as t1) :: ((PC.TOPar(_),_) as t2) :: rest
    ->
      let rec skip = function
	  [] -> ([],false,[])
	| ((PC.TCPar(_),_) as t)::rest -> ([t],true,rest)
	| ((PC.TArobArob,_) as t)::rest -> ([t],false,rest)
	| ((PC.EOF,_) as t)::rest -> ([t],false,rest)
	| t::rest ->
      	    let (pre,found,post) = skip rest in (t::pre,found,post) in
      let (pre,found,post) = skip rest in
      (match (t1,found,post) with
	((PC.TIdent(s,clt),info),true,((PC.TOBrace(_),_) as t3)::rest) ->
	  (PC.TFunName(s,clt),info) :: t2 ::
	  pre @ t3 :: (find_function_names rest)
      | ((PC.TMetaLocalFunc(s,clt),info),true,((PC.TOBrace(_),_) as t3)::rest)
	->
	  (PC.TMetaFunName(s,clt),info) :: t2 ::
	  pre @ t3 :: (find_function_names rest)
      | _ -> t1 :: t2 :: pre @ find_function_names post)
  | t :: rest -> t :: find_function_names rest

(* ----------------------------------------------------------------------- *)
(* Insert TLineEnd tokens at the end of a line that contains a WHEN.
   WHEN is restricted to a single line, to avoid ambiguity in eg:
   ... WHEN != x
   +3 *)

let token2line (tok,_) =
  match tok with
    PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt) 
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt) | PC.Tstruct(clt) 
  | PC.Tunion(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt) | PC.Tstatic(clt) 
  | PC.Tconst(clt) | PC.Tvolatile(clt) 

  | PC.TInc(clt) | PC.TDec(clt) 
	
  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt) 
  | PC.TReturn(clt) | PC.TIdent(_,clt) | PC.TFunName(_,clt) 

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt) 

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TInf(clt) 
  | PC.TSup(clt) | PC.TInfEq(clt) | PC.TSupEq (clt) | PC.TShl(clt) 
  | PC.TShr(clt) | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt) 
  | PC.TDiv(clt) | PC.TMod (clt) 

  | PC.TMetaFunName(_,clt) | PC.TMetaParam(_,clt) | PC.TMetaParamList(_,clt) 
  | PC.TMetaConst(_,_,clt) | PC.TMetaExp(_,_,clt) | PC.TMetaExpList(_,clt) 
  | PC.TMetaId(_,clt) | PC.TMetaType(_,clt)  | PC.TMetaStm(_,clt)   
  | PC.TMetaStmList(_,clt) | PC.TMetaFunc(_,clt) | PC.TMetaLocalFunc(_,clt) 

  | PC.TWhen(clt) | PC.TEllipsis(clt) | PC.TCircles(clt) | PC.TStars(clt)    

  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt) | PC.TOCircles(clt)
  | PC.TCCircles(clt) | PC.TOStars(clt) | PC.TCStars(clt)    

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt)
  | PC.TOPar0(clt) | PC.TMid(clt) | PC.TMid0(clt) | PC.TCPar(clt)  
  | PC.TCPar0(clt) 

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt) 

  | PC.TPtrOp(clt) 

  | PC.TEq(clt) | PC.TAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt) 
  | PC.TPtVirg(clt) ->
      let (_,line,_) = clt in Some line

  | _ -> None

let rec insert_line_end = function
    [] -> []
  | (((PC.TWhen(clt),q) as x)::xs) -> x::(find_line_end (token2line x) q xs)
  | x::xs -> x::(insert_line_end xs)

and find_line_end line q = function
    (* don't know what 2nd component should be so just use the info of
       the When *)
    [] -> [(PC.TLineEnd,q)]
  | x::xs when token2line x = line -> x :: (find_line_end line q xs)
  | xs -> (PC.TLineEnd,q)::(insert_line_end xs)

(* ----------------------------------------------------------------------- *)
(* Drop ... ... .  This is only allowed in + code, and arises when there is
some - code between the ... *)
(* drop whens as well - they serve no purpose in + code and they cause
problems for drop_double_dots *)

let rec drop_when = function
    [] -> []
  | (PC.TWhen(clt),info)::xs ->
      let rec loop = function
	  [] -> []
	| (PC.TLineEnd,info)::xs -> drop_when xs
	| x::xs -> loop xs in
      loop xs
  | x::xs -> x::drop_when xs

let rec drop_double_dots = function
    [] -> []
  | (PC.TEllipsis(clt),info)::(PC.TEllipsis(_),_)::rest ->
      drop_double_dots ((PC.TEllipsis(clt),info)::rest)
  | (PC.TCircles(clt),info)::(PC.TCircles(_),_)::rest ->
      drop_double_dots ((PC.TCircles(clt),info)::rest)
  | (PC.TStars(clt),info)::(PC.TStars(_),_)::rest ->
      drop_double_dots ((PC.TStars(clt),info)::rest)
  | x::xs -> x::(drop_double_dots xs)

(* ----------------------------------------------------------------------- *)
(* Read tokens *)

let get_s_starts (_, (s,_,(starts, ends))) = (s, starts)

let pop2 l = 
  let v = List.hd !l in
  l := List.tl !l;
  v

let parse_one parsefn file toks =
  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function _ =
      let (v, info) = pop2 all_tokens in
      cur_tok := (v, info);
      v in

  let lexbuf_fake =
    Lexing.from_function (function buf -> function n -> raise Common.Todo) in

  try parsefn lexer_function lexbuf_fake 
  with 
    Lexer_cocci.Lexical s ->
      pr2
	(pr "lexical error %s\n =%s\n" s
	   (Common.error_message file (get_s_starts !cur_tok) ));
      failwith ""
  | Parsing.Parse_error ->
      pr2
	(pr "parse error \n = %s\n" 
	   (Common.error_message file (get_s_starts !cur_tok) ));
      failwith ""
  | Semantic_cocci.Semantic s ->
      pr2
	(pr "semantic error %s\n =%s\n" s
	   (Common.error_message file (get_s_starts !cur_tok) ));
      failwith ""

  | e -> raise e

let parse file =
  Lexer_cocci.init ();
  let table = Common.full_charpos_to_pos file in
  let lexbuf = Lexing.from_channel (open_in file) in
  match tokens_all table file false lexbuf with
    (true,[(PC.TArobArob,_)]) -> (* read over initial @@ *)
      let rec loop _ =
	(* get metavariable declarations *)
	let (more,tokens) = tokens_all table file true lexbuf in
	let metavars = parse_one PC.meta_main file tokens in
	(* get transformation rules *)
	let (more,tokens) = tokens_all table file false lexbuf in
	let tokens = find_function_names tokens in
	let tokens = insert_line_end tokens in
	let (minus_tokens,plus_tokens) = split_token_stream tokens in 
	let plus_tokens = drop_double_dots (drop_when plus_tokens) in
	let (minus_res,plus_res) =
	  (parse_one PC.main file minus_tokens,
	   parse_one PC.main file plus_tokens) in
	Check_meta.check_meta metavars minus_res plus_res;
	if more
	then
	  let (minus_ress,plus_ress) = loop () in
	  (minus_res::minus_ress,plus_res::plus_ress)
	else ([minus_res],[plus_res]) in
      loop ()
  | (false,[(PC.TArobArob,_)]) -> ([],[])
  | _ -> failwith "unexpected code before the first rule"


(*
let parse_and_merge file  =
  if file = "" then failwith "filename required";
  try
    let (minus,plus) = parse file in
    let xs = 
    List.map2
      (function minus ->
	function plus ->
	  let minus = Arity.minus_arity minus in
	  (match plus with
	    Some plus ->
	      let plus = Arity.plus_arity plus in
	      let replus = Plus.plus plus in
	      Merge.do_merge minus replus
	  | None -> ());
	  minus)
      minus plus in
    xs
  with Failure s -> Printf.printf "%s" s; []
*)



(* parse to ast0 and then convert to ast *)
let process file verbose =
  try
    let (minus,plus) = parse file in
    List.map2
      (function minus ->
	function plus ->
	  let minus = Arity.minus_arity minus in
	  let plus = Arity.plus_arity plus in
	  let replus = Plus.plus plus in
	  Merge.do_merge minus replus;
	  if verbose then Unparse_cocci.unparse minus;
	  minus)
      minus plus
  with Failure s -> Printf.printf "%s" s; []
