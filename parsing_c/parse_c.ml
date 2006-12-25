open Common open Commonop

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.pr2 s
    
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_eof = function
  | Parser_c.EOF x -> true
  | _ -> false

let is_not_comment = function
  | Parser_c.TComment _  | Parser_c.TCommentSpace _ 
  | Parser_c.TCommentCpp _ | Parser_c.TCommentAttrOrMacro _ -> false 
  | _ -> true 

let is_comment x = not (is_not_comment x)

let not_struct_enum = function
  | (Parser_c.Tstruct _ | Parser_c.Tunion _ | Parser_c.Tenum _)::_ -> false
  | _ -> true



(* Because ocamllex force us to do it that way. Cant return a pair to
 * ocamlyacc :( 
 *)
let info_from_token = function
  | Parser_c.TComment  (i) -> i
  | Parser_c.TCommentSpace  (i) -> i
  | Parser_c.TCommentCpp  (i) -> i
  | Parser_c.TCommentAttrOrMacro  (i) -> i
  | Parser_c.TDefine  (i) -> i
  | Parser_c.TInclude  (i) -> i
  | Parser_c.TIfdef  (i) -> i
  | Parser_c.TIfdefelse  (i) -> i
  | Parser_c.TEndif  (i) -> i
  | Parser_c.THigherOrderMacro (i) -> i
  | Parser_c.TString ((string, isWchar), i) -> i
  | Parser_c.TChar  ((string, isWchar), i) -> i
  | Parser_c.TIdent  (s, i) -> i
  | Parser_c.TypedefIdent  (s, i) -> i
  | Parser_c.TInt  (s, i) -> i
  | Parser_c.TFloat ((string, floatType), i) -> i
  | Parser_c.TOPar  (i) -> i
  | Parser_c.TCPar  (i) -> i
  | Parser_c.TOBrace  (i) -> i
  | Parser_c.TCBrace  (i) -> i
  | Parser_c.TOCro  (i) -> i
  | Parser_c.TCCro  (i) -> i
  | Parser_c.TDot  (i) -> i
  | Parser_c.TComma  (i) -> i
  | Parser_c.TPtrOp  (i) -> i
  | Parser_c.TInc  (i) -> i
  | Parser_c.TDec  (i) -> i
  | Parser_c.TAssign  (assignOp, i) -> i
  | Parser_c.TEq  (i) -> i
  | Parser_c.TWhy  (i) -> i
  | Parser_c.TTilde  (i) -> i
  | Parser_c.TBang  (i) -> i
  | Parser_c.TEllipsis  (i) -> i
  | Parser_c.TDotDot  (i) -> i
  | Parser_c.TPtVirg  (i) -> i
  | Parser_c.TOrLog  (i) -> i
  | Parser_c.TAndLog  (i) -> i
  | Parser_c.TOr  (i) -> i
  | Parser_c.TXor  (i) -> i
  | Parser_c.TAnd  (i) -> i
  | Parser_c.TEqEq  (i) -> i
  | Parser_c.TNotEq  (i) -> i
  | Parser_c.TInf  (i) -> i
  | Parser_c.TSup  (i) -> i
  | Parser_c.TInfEq  (i) -> i
  | Parser_c.TSupEq  (i) -> i
  | Parser_c.TShl  (i) -> i
  | Parser_c.TShr  (i) -> i
  | Parser_c.TPlus  (i) -> i
  | Parser_c.TMinus  (i) -> i
  | Parser_c.TMul  (i) -> i
  | Parser_c.TDiv  (i) -> i
  | Parser_c.TMod  (i) -> i
  | Parser_c.Tchar  (i) -> i
  | Parser_c.Tshort  (i) -> i
  | Parser_c.Tint  (i) -> i
  | Parser_c.Tdouble  (i) -> i
  | Parser_c.Tfloat  (i) -> i
  | Parser_c.Tlong  (i) -> i
  | Parser_c.Tunsigned  (i) -> i
  | Parser_c.Tsigned  (i) -> i
  | Parser_c.Tvoid  (i) -> i
  | Parser_c.Tauto  (i) -> i
  | Parser_c.Tregister  (i) -> i
  | Parser_c.Textern  (i) -> i
  | Parser_c.Tstatic  (i) -> i
  | Parser_c.Tconst  (i) -> i
  | Parser_c.Tvolatile  (i) -> i
  | Parser_c.Tstruct  (i) -> i
  | Parser_c.Tenum  (i) -> i
  | Parser_c.Ttypedef  (i) -> i
  | Parser_c.Tunion  (i) -> i
  | Parser_c.Tbreak  (i) -> i
  | Parser_c.Telse  (i) -> i
  | Parser_c.Tswitch  (i) -> i
  | Parser_c.Tcase  (i) -> i
  | Parser_c.Tcontinue  (i) -> i
  | Parser_c.Tfor  (i) -> i
  | Parser_c.Tdo  (i) -> i
  | Parser_c.Tif  (i) -> i
  | Parser_c.Twhile  (i) -> i
  | Parser_c.Treturn  (i) -> i
  | Parser_c.Tgoto  (i) -> i
  | Parser_c.Tdefault  (i) -> i
  | Parser_c.Tsizeof  (i) -> i
  | Parser_c.Tasm  (i) -> i
  | Parser_c.Tattribute  (i) -> i
  | Parser_c.Tinline (i) -> i
  | Parser_c.EOF (i) -> i


let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let token_to_strpos tok = 
  let (parse_info,_cocci_info) = info_from_token tok in
  (parse_info.Common.str, parse_info.Common.charpos)


let linecol_of_tok tok table =
  let (parse_info,_cocci_info) = info_from_token tok in
  table.(parse_info.charpos)

let line_of_tok tok table = 
  fst (linecol_of_tok tok table)

let col_of_tok tok table = 
  snd (linecol_of_tok tok table)


let error_msg_tok file tok = 
  if not !Flag_parsing_c.opti_parsing 
  then Common.error_message file (token_to_strpos tok) 
  else "TODO remove opti_parsing flag"


let print_bad line_error (start_line, end_line) filelines  = 
  begin
    pr2 ("badcount: " ^ i_to_s (end_line - start_line));
    for i = start_line to end_line do 
      if i = line_error 
      then  pr2 ("BAD:!!!!!" ^ " " ^ filelines.(i)) 
      else  pr2 ("bad:" ^ " " ^      filelines.(i)) 
    done
  end



(* info_item is defined in ast_c.ml .
 * todo: give correct column, and charpos (for the moment not needed)
 *)
let mk_info_item2 filename line1 line2 toks = 
  let toks' = List.rev toks in
  let buf = Buffer.create 100 in
  let s = 
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks' +> List.iter (fun tok -> 
        Buffer.add_string buf (fst (token_to_strpos tok));
      );
      Buffer.contents buf
    end
  in
  (filename, (((line1, 0), 0), ((line2, 0), 0)),  s, toks') 

let mk_info_item a b c d = 
  Common.profile_code "C parsing.mk_info_item" 
    (fun () -> mk_info_item2 a b c d)



(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
type parsing_stat = {
    filename: filename;
    mutable passing_through_lines: int;
    mutable have_timeout: bool;

    mutable correct: int;  
    mutable bad: int;
  } 

let default_stat file =  { 
    filename = file;
    passing_through_lines = 0;
    have_timeout          = false;
    correct = 0; bad = 0;
  }

(* todo: stat per dir ?  give in terms of func_or_decl numbers:   
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/ 
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les 
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura 
 * aucune def  et donc aucune couverture en fait.   
 * ==> TODO evaluer les parties non parsé ? 
 *)

let print_parsing_stat_list = fun statxs -> 
  let total = (List.length statxs) in
  let perfect = 
    statxs 
      +> List.filter (function 
          {have_timeout = false; bad = 0} -> true | _ -> false)
      +> List.length 
  in
  pr2 "\n\n\n---------------------------------------------------------------";
  pr2 "pbs with files:";
  statxs 
    +> List.filter (function 
      | {have_timeout = true} -> true 
      | {bad = n} when n > 0 -> true 
      | _ -> false)
    +> List.iter (function 
        {filename = file; have_timeout = timeout; bad = n} -> 
          pr2 (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
        );
  pr2 "\n\n\n---------------------------------------------------------------";
  pr2 (
  (sprintf "NB total files = %d; " total) ^
  (sprintf "perfect = %d; " perfect) ^
  (sprintf "pbs = %d; "     (statxs +> List.filter (function 
      {have_timeout = b; bad = n} when n > 0 -> true | _ -> false) 
                               +> List.length)) ^
  (sprintf "timeout = %d; " (statxs +> List.filter (function 
      {have_timeout = true; bad = n} -> true | _ -> false) 
                               +> List.length)) ^
  (sprintf "=========> %d" ((100 * perfect) / total)) ^ "%"
                                                          
 );
  let good = (statxs +> List.fold_left (fun acc {correct = x} -> acc+x) 0) in
  let bad  = (statxs +> List.fold_left (fun acc {bad = x} -> acc+x) 0)  in
  pr2 (
  (sprintf "nb good = %d,  nb bad = %d    " good bad) ^
  (sprintf "=========> %d"  (100 * good / (good+bad))) ^ "%"
   )




(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* called by parse_print_error_heuristic *)
let tokens2 file = 
 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in
  try 
    let rec tokens_aux () = 
      let result = Lexer_c.token lexbuf in
      if is_eof result
      then [result]
      else result::(tokens_aux ())
    in
    tokens_aux ()
  with
    | Lexer_c.Lexical s -> 
        failwith ("lexical error " ^ s ^ "\n =" ^ 
                  (Common.error_message file (lexbuf_to_strpos lexbuf)))
    | e -> raise e
 )

let tokens a = 
  Common.profile_code "C parsing.tokens" (fun () -> tokens2 a)


let tokens_string string = 
  let lexbuf = Lexing.from_string string in
  try 
    let rec tokens_s_aux () = 
      let result = Lexer_c.token lexbuf in
      if is_eof result
      then [result]
      else result::(tokens_s_aux ())
    in
    tokens_s_aux ()
  with
    | Lexer_c.Lexical s -> failwith ("lexical error " ^ s ^ "\n =" )
    | e -> raise e


(*****************************************************************************)
(* Parsing, but very basic, no more used *)
(*****************************************************************************)
let parse file = 
  let lexbuf = Lexing.from_channel (open_in file) in
  let result = Parser_c.main Lexer_c.token lexbuf in
  result


let parse_print_error file = 
  let chan = (open_in file) in
  let lexbuf = Lexing.from_channel chan in

  let error_msg () = Common.error_message file (lexbuf_to_strpos lexbuf) in
  try 
    lexbuf +> Parser_c.main Lexer_c.token
  with 
  | Lexer_c.Lexical s ->   
      failwith ("lexical error " ^s^ "\n =" ^  error_msg ())
  | Parsing.Parse_error -> 
      failwith ("parse error \n = " ^ error_msg ())
  | Semantic_c.Semantic (s, i) -> 
      failwith ("semantic error " ^ s ^ "\n =" ^ error_msg ())
  | e -> raise e




(*****************************************************************************)
(* Parsing subelements, useful to debug parser *)
(*****************************************************************************)

(* old: 
 * let parse_gen parsefunc s = 
 *   let lexbuf = Lexing.from_string s in
 *   let result = parsefunc Lexer_c.token lexbuf in
 *   result
*)

let parse_gen parsefunc s = 
  let toks = tokens_string s +> List.filter is_not_comment in

  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function = 
    (fun _ -> 
      if is_eof !cur_tok
      then (pr2 "ALREADY AT END"; !cur_tok)
      else
        let v = Common.pop2 all_tokens in
        cur_tok := v;
        !cur_tok
    ) 
  in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in
  let result = parsefunc lexer_function lexbuf_fake in
  result

(* ex: parse_gen Parser_c.statement "(struct us_data*)psh->hostdata = NULL;" *)



(*****************************************************************************)
(* Lexing with lookahead *)
(*****************************************************************************)
open Parser_c
open Lexer_parser

(* LALR(k) trick. We can do stuff by adding cases in lexer_c.mll, but
 * it is more general to do it via my LALR(k) tech. Because here we can
 * transform some token give some context information. So sometimes it
 * makes sense to transform a token in one context, sometimes not, and
 * lex can not provide us this context information. Note that the order
 * in the pattern matching is important. Do not cut/paste. *)



let forLOOKAHEAD = 20

(* opti: was better to built it once and for all *)
let regexp_foreach = Str.regexp_case_fold 
  ".*for_?each\\|for_?all\\|iterate"
  
(* look if there is a '{' just after the closing ')', and handling the
 * possibility to have nested expressions inside nested parenthesis 
 *)
let rec is_really_foreach xs = 
  let rec is_foreach_aux = function
    | [] -> false, []
    | TCPar _::TOBrace _::xs -> true, xs
      (* the following attempts to handle the cases where there is a
	 single statement in the body of the loop.  undoubtedly more
	 cases are needed. *)
    | TCPar _::TIdent _::xs -> true, xs
    | TCPar _::Tif _::xs -> true, xs
    | TCPar _::Twhile _::xs -> true, xs
    | TCPar _::Tfor _::xs -> true, xs
    | TCPar _::Tswitch _::xs -> true, xs
    | TCPar _::xs -> false, xs
    | TOPar _::xs -> 
        let (_, xs') = is_foreach_aux xs in
        is_foreach_aux xs'
    | x::xs -> is_foreach_aux xs
  in
  is_foreach_aux xs +> fst




let msg_typedef s = 
  match s with
  | "u_char"   | "u_short"  | "u_int"  | "u_long"
  | "u8" | "u16" | "u32" | "u64" 
  | "s8"  | "s16" | "s32" | "s64" 
  | "__u8" | "__u16" | "__u32"  | "__u64"  -> () 
      
  | "acpi_handle" -> ()
  | "acpi_status" -> ()
        
  | s when s =~ ".*_t$" -> ()
  | _ ->  
      if !Flag_parsing_c.debug_typedef
      then pr2 ("CERTAINLY TYPEDEF, promoting: " ^ s)


let lookahead2 next before = 

  match (next, before) with

  (* special cases scsi/g_NCR5380 *)
  | (TIdent ("ANDP",i1)::TIdent (_,_)::_,   _) ->  TComma i1
  | (TIdent ("ANDP",i1)::TOPar _::_,   _)       ->  TComma i1

  (*-------------------------------------------------------------*)
  (* stringification of ident *)
  (*-------------------------------------------------------------*)
  | (TIdent (s,i1)::_, TOPar _::TIdent ("printk", _)::_) -> 
      TString ((s, Ast_c.IsChar), i1)

  | (TIdent (s,i1)::TString (_,_)::_,   _) ->  TString ((s, Ast_c.IsChar), i1)
  | (TIdent (s,i1)::_,   TString _::_) ->      TString ((s, Ast_c.IsChar), i1)


                  
  (*-------------------------------------------------------------*)
  (* typedef inference *)
  (*-------------------------------------------------------------*)
  (*  xx xx *)
  | (TIdent(s,i1)::TIdent(s2,i2)::_ , _) when not_struct_enum before && s = s2
      (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
    -> 
      (* parse_typedef_fix3:
       *    acpi_object		acpi_object;
       * etait mal parsé, car pas le temps d'appeler dt()  dans le type_spec. 
       * Le parser en interne a deja appelé le prochain token pour pouvoir
       * decider des choses.
       *  => special case in lexer_heuristic, again
       *)
      if !Flag_parsing_c.debug_typedef 
      then pr2 ("DISABLE typedef cos special case: " ^ s); 

      Lexer_parser.disable_typedef();

      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx yy *)
  | (TIdent (s, i1)::TIdent (s2, i2)::_  , _) when not_struct_enum before -> 
      (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
      
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (* [,(] xx [,)] AND param decl *)
  | (TIdent (s, i1)::(TComma _|TCPar _)::_ , (TComma _ |TOPar _)::_ )
    when not_struct_enum before && 
         !Lexer_parser._lexer_hint.parameterDeclaration
    -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx* [,)] *)
  (* specialcase:  [,(] xx* [,)] *)
  | (TIdent (s, i1)::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
        (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
    -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx** [,)] *)
  (* specialcase:  [,(] xx** [,)] *)
  | (TIdent (s, i1)::TMul _::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
        (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
    -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (*------------------------------------------------------------*)
  (* if 'x*y' maybe an expr, maybe just a classic multiplication *)
  (* but if have a '=', or ','   I think not *)
  (*------------------------------------------------------------*)

  (* static xx * yy  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::_ , 
     (Tregister _|Tstatic _  |Tvolatile _|Tconst _)::_) -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (*  TODO  xx * yy ; AND in start of compound element  *)


  (*  xx * yy,      AND  in paramdecl *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before &&
         !Lexer_parser._lexer_hint.parameterDeclaration -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy ;     AND in Toplevel  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , _)
    when not_struct_enum before && !Lexer_parser._lexer_hint.toplevel  -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx * yy (     AND in Toplevel  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOPar _::_ , _)
    when not_struct_enum before  && !Lexer_parser._lexer_hint.toplevel -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (* xx * yy [ *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOCro _::_ , _)
    when not_struct_enum before && !Lexer_parser._lexer_hint.toplevel -> 
      msg_typedef s;  Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)
        

    (*  why need TOPar condition as stated in preceding rule ? really needed ? *)
    (*   YES cos at toplevel can have some expression !! for instance when *)
    (*   enter in the dimension of an array *)
    (*
      | (TIdent s::TMul::TIdent s2::_ , _)
      when (take_safe 1 !passed_tok <> [Tstruct] &&
      (take_safe 1 !passed_tok <> [Tenum]))
      &&
      !Lexer_parser._lexer_hint = Some Lexer_parser.Toplevel -> 
      msg_typedef s; 
      Lexer_parser.add_typedef_root s;
      TypedefIdent s
     *)

  (*  xx * yy =  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TEq _::_ , _)
    when not_struct_enum before -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy)      AND in paramdecl *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TCPar _::_ , _)
      when not_struct_enum before &&
           !Lexer_parser._lexer_hint.parameterDeclaration -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)
          

  (*  xx * yy; *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , 
     (TOBrace _| TPtVirg _)::_)  when not_struct_enum before ->
      msg_typedef s;  Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy,  and ';' before xx *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , 
     (TOBrace _| TPtVirg _)::_) ->
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx_t * yy *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::_ , _)  when s =~ ".*_t$" ->
      msg_typedef s;  Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx ** yy *)  (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before -> 
        (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx ** ) *)
  | (TIdent (s, i1)::TMul _::TMul _::TCPar _::_ , _)
    when not_struct_enum before -> 
        (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  (xx) yy *)
  | (TOPar info::TIdent (s, i1)::TCPar _::(TIdent _|TInt _)::_ , x::_)  
    when (match x with Tif _ -> false | Twhile _ -> false | _ -> true) -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TOPar info

  (*  (xx * ) yy *)
  | (TOPar info::TIdent (s, i1)::TMul _::TCPar _::TIdent (s2, i2)::_ , _) -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TOPar info

  (* (xx){ ... }  constructor *)
  | (TIdent (s, i1)::TCPar _::TOBrace _::_ , TOPar _::_)  when s =~ ".*_t$"->
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


        (* can have sizeof on expression
           | (Tsizeof::TOPar::TIdent s::TCPar::_,   _) -> 
           msg_typedef s; 
           Lexer_parser.add_typedef_root s;
           Tsizeof
         *)

   (*-------------------------------------------------------------*)
   (* higher order macro, iterator macro, debug macro *)
   (*-------------------------------------------------------------*)
  | (TIdent (s, i1)::TOPar _::Tif _::_ ,     _)
      (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
    -> 
      if !Flag_parsing_c.debug_cpp 
      then pr2 ("CERTAINLY HIGHER ORDER MACRO, transforming: " ^ s);
      THigherOrderMacro i1

   (* If ident contain a for_each, then certainly a macro. But to be
    * sure should look if there is a '{' after the ')', but it requires
    * to count the '('. Because this can be expensive, we do that only
    * when the token contains "for_each". *)

  | (TIdent (s, i1)::TOPar _::rest, _) -> 
      if s ==~ regexp_foreach && 
        is_really_foreach (Common.take_safe forLOOKAHEAD rest)
      then begin
        if !Flag_parsing_c.debug_cpp 
        then pr2 ("CERTAINLY FOREACH, transforming: " ^ s);
        Twhile i1
      end
      else TIdent (s, i1)

        
        
  (*-------------------------------------------------------------*)
  (* CPP *)
  (*-------------------------------------------------------------*)
  | TDefine ii::_, _ when not !Lexer_parser._lexer_hint.toplevel -> 
      if !Flag_parsing_c.debug_cpp
      then pr2 ("DEFINE inside function, I treat it as comment");
      TCommentCpp ii

  (* do same for include often found inside structdef *)
  | TInclude ii::_, _ when not !Lexer_parser._lexer_hint.toplevel -> 
      if !Flag_parsing_c.debug_cpp
      then pr2 ("INCLUDE inside function, I treat it as comment");
      TCommentCpp ii

  | ((TIfdef ii | TIfdefelse ii | TEndif ii) as x)::_, _ -> 
      if not !Flag_parsing_c.ifdef_to_if then TCommentCpp ii 
      else 
        if not !Lexer_parser._lexer_hint.toplevel
        then x
        else begin
          if !Flag_parsing_c.debug_cpp
          then pr2 ("ifdef or related outside function. I treat it as comment");
          TCommentCpp ii
        end

                    
 (*-------------------------------------------------------------*)
 | v::xs, _ -> v
 | _ -> raise Impossible

let lookahead a b = 
  Common.profile_code "C parsing.lookahead" (fun () -> lookahead2 a b)


(*****************************************************************************)
(* Error recovery *)
(*****************************************************************************)

(* todo: do something if find Parser_c.Eof ? *)
let rec find_next_synchro next already_passed table =
  match next with
  | [] ->  
      pr2 "END OF FILE WHILE IN RECOVERY MODE"; 
      already_passed, []
  | (TCBrace i as v)::xs when col_of_tok v table = 0 -> 
      pr2 ("FOUND SYNC at line "^ i_to_s (line_of_tok v table));

      (* perhaps a }; obsolete now, because parser.mly allow empty ';' *)
      (match xs with
      | [] -> raise Impossible (* there is a EOF token normally *)
      | TPtVirg iptvirg::xs -> 
          pr2 "FOUND SYNC bis, eating } and ;";
          (TPtVirg iptvirg)::v::already_passed, xs
      | _ -> 
          v::already_passed, xs
      (* reput ? why in comment ?
       (Tstatic _, 0)  -> 
          pr2 ("FOUND SYNC 2 at line "^ i_to_s line);
         ...
      *)
      )
  | v::xs -> 
      find_next_synchro xs (v::already_passed) table
      


  
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type info_item = 
  (filename * Common.pos_file Common.pair * string * Parser_c.token list)

type program2 = programElement2 list
     and programElement2 = Ast_c.programElement * info_item


(* note: as now go in 2 pass, there is first all the error message of
 * the lexer, and then the error of the parser. It is no more
 * interwinded.
 * 
 * The use of local refs (remaining_tokens, passed_tokens, ...) makes
 * possible error recovery. Indeed, they allow to skip some tokens and
 * still be able to call again the ocamlyacc parser. It is ugly code
 * because we cant modify ocamllex and ocamlyacc. As we want some
 * extended lexing tricks, we have to use such refs.

 * Those refs are now also used for my lalr(k) technique. Indeed They
 * store the futur and previous tokens that were parsed, and so
 * provide enough context information for powerful lex trick.

 * - passed_tokens_last_ckp stores the passed tokens since last
 *   checkpoint. Used for NotParsedCorrectly and also for build the
 *   info_item attached to each program_element.
 * - passed_tokens is used for lookahead, in fact for lookback.
 * - remaining_tokens_clean is used for lookahead. Now remaining_tokens
 *   contain some comments and so would make pattern matching difficult
 *   in lookahead. Hence this variable. We would like also to get rid 
 *   of cpp instruction because sometimes a cpp instruction is between
 *   two tokens and makes a pattern matching fail. But lookahead also
 *   transform some cpp instruction (in comment) so can't remove them.

 * So remaining_tokens, passed_tokens_last_ckp contain comment-tokens.
 * passed_tokens and remaining_tokens_clean does not contain
 * comment-tokens.

 * Normally we have:
 * toks = (reverse passed_tok) ++ cur_tok ++ remaining_tokens   
 *    after the call to pop2.
 * toks = (reverse passed_tok) ++ remaining_tokens   
 *     at the and of the lexer_function call.
 * At the very beginning, cur_tok and remaining_tokens overlap, but not after.
 * At the end of lexer_function call,  cur_tok  overlap  with passed_tok.
 *)


let parse_print_error_heuristic2 file = 

  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)
  Lexer_parser.lexer_reset_typedef(); 
  let toks = tokens file in

  let table     = Common.full_charpos_to_pos file in
  let filelines = (""::Common.cat file) +> Array.of_list in

  let stat = default_stat file in

  let remaining_tokens       = ref toks in
  let remaining_tokens_clean = ref (toks +> List.filter is_not_comment) in
  let cur_tok                = ref (List.hd !remaining_tokens) in
  let passed_tokens_last_ckp = ref [] in 
  let passed_tokens          = ref [] in

  (* hacked_lex *)
  let rec lexer_function = 
    (fun lexbuf -> 
      if is_eof !cur_tok
      then begin pr2 "ALREADY AT END"; !cur_tok end
      else begin
        let v = pop2 remaining_tokens in
        cur_tok := v;

        if is_comment v
        then begin
          passed_tokens_last_ckp := v::!passed_tokens_last_ckp;
          lexer_function lexbuf
        end
        else begin
          (* with error recovery, remaining_tokens and
           * remaining_tokens_clean may not be in sync *)
          remaining_tokens_clean := !remaining_tokens_clean +>
            Common.drop_while (fun p -> p <> v);
          let x = pop2 remaining_tokens_clean  in
          assert (x = v);

          (* typedef_fix1 *)
          let v = match v with
            | TIdent (s, ii) -> 
                if Lexer_parser.is_typedef s 
                then TypedefIdent (s, ii)
                else TIdent (s, ii)
            | x -> x
          in
          let v = lookahead (v::!remaining_tokens_clean) !passed_tokens in

          passed_tokens_last_ckp := v::!passed_tokens_last_ckp;

          if !Flag_parsing_c.debug_lexer then pr2 (Dumper.dump v);  

          (* the lookahead may have change the status of the token and
           * consider it as a comment, for instance some #include are
           * turned into comments hence this code. *)
          match v with
          | TCommentCpp _ -> lexer_function lexbuf
          | v -> 
              passed_tokens := v::!passed_tokens;
              v
        end
      end
    )
  in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in



  let rec loop () =

    if !Lexer_parser._handle_typedef = false && !Flag_parsing_c.debug_typedef
    then pr2 "FALSE _handle_typedef, not normal if dont come from exn";

    (* normally have to do that only when come from an exception in which
     * case the dt() may not have been done 
     * TODO but if was in scoped scope ? have to let only the last scope
     * so need do a Lexer_parser.lexer_reset_typedef ();
     *)
    Lexer_parser._handle_typedef := true;  
    Lexer_parser._lexer_hint := {(default_hint ()) with toplevel = true; };

    (* todo?: I am not sure that it represents current_line, cos maybe
     * cur_tok partipated in the previous parsing phase, so maybe cur_tok
     * is not the first token of the next parsing phase. Same with checkpoint2.
     * It would be better to record when we have a } or ; in parser.mly,
     *  cos we know that they are the last symbols of external_declaration2.
     *)
    let checkpoint = line_of_tok !cur_tok table in

    passed_tokens_last_ckp := [];

    let elem = 
      (try 
          (* -------------------------------------------------- *)
          (* Call parser *)
          (* -------------------------------------------------- *)
          Parser_c.external_declaration2 lexer_function lexbuf_fake
        with e -> 
          begin
            (match e with
            | Lexer_c.Lexical s -> 
                pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok file !cur_tok)
            | Parsing.Parse_error -> 
                pr2 ("parse error \n = " ^ error_msg_tok file !cur_tok)
            | Semantic_c.Semantic (s, i) -> 
                pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok file !cur_tok)
            | e -> raise e
            );
            let line_error = line_of_tok !cur_tok table in

            (*  error recovery, go to next synchro point *)
            let (passed_tokens', remaining_tokens') =
              find_next_synchro !remaining_tokens !passed_tokens_last_ckp table
            in
            remaining_tokens := remaining_tokens';
            passed_tokens_last_ckp := passed_tokens';
            cur_tok := List.hd !passed_tokens_last_ckp;
            passed_tokens := [];           (* enough ? *)

            let checkpoint2 = line_of_tok !cur_tok table in
            print_bad line_error (checkpoint, checkpoint2) filelines;

            let info_of_bads = 
              Common.map_eff_rev info_from_token !passed_tokens_last_ckp 
            in Ast_c.NotParsedCorrectly info_of_bads
          end
      ) 
    in

    (* again not sure if checkpoint2 corresponds to end of bad region *)
    let checkpoint2 = line_of_tok !cur_tok table in
    let diffline = (checkpoint2 - checkpoint) in
    let info = mk_info_item file checkpoint checkpoint2 !passed_tokens_last_ckp
    in 

    (match elem with
    | Ast_c.NotParsedCorrectly _ -> stat.bad     <- stat.bad     + diffline
    | _ ->                          stat.correct <- stat.correct + diffline;
    );
    (match elem with
    | Ast_c.FinalDef x -> [(Ast_c.FinalDef x, info)]
    | xs -> (xs, info):: loop ()
    )
  in
  let v = loop() in
  (v, stat)


let parse_print_error_heuristic a  = 
  Common.profile_code "C parsing" (fun () -> parse_print_error_heuristic2 a)





