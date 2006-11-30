open Fullcommon


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

(* 
 * todo: stat per dir ?  give in terms of func_or_decl numbers:   
 *   nbfunc_or_decl pbs / nbfunc_or_decl total ?/ 
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les 
 * valeurs alors on parsera correctemet tout le fichier et pourtant y'aura 
 * aucune def  et donc aucune couverture en fait.   
 * ==> TODO evaluer les parties non pars� ? 
 *)

let print_parsing_stat_list = fun statxs -> 
  let total = (List.length statxs) in
  let perfect = 
    statxs +> List.filter (function 
      {have_timeout = false; bad = 0} -> true | _ -> false)
      +> List.length in
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
(* wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.pr2 s
  else ()
    

let wrap_lexbuf_info lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let wrap_parse_info  parse_info = 
  (parse_info.str, parse_info.charpos)

(*****************************************************************************)
(* lexing only *)
(*****************************************************************************)

let tokens file = 
 with_open_infile file (fun chan -> 

  let lexbuf = Lexing.from_channel chan in
  try 
    let rec aux () = 
      let result = Lexer_c.token lexbuf in
      (* pr2 (Dumper.dump result); *)
      if (match result with Parser_c.EOF (x) -> true | _ -> false)
      then [result]
      else result::(aux ())
    in
    aux ()
  with
    | Lexer_c.Lexical s -> 
        failwith ("lexical error " ^ s ^ "\n =" ^  
                  (error_message file (wrap_lexbuf_info lexbuf)  ))
    | e -> raise e
 )


let tokens_string string = 

  let lexbuf = Lexing.from_string string in
  try 
    let rec aux () = 
      let result = Lexer_c.token lexbuf in
      (* pr2 (Dumper.dump result); *)
      if (match result with Parser_c.EOF (x) -> true | _ -> false)
      then [result]
      else result::(aux ())
    in
    aux ()
  with
    | Lexer_c.Lexical s -> failwith ("lexical error " ^ s ^ "\n =" )
    | e -> raise e

(*---------------------------------------------------------------------------*)

(* Because ocamllex force us to do it that way :( cant return a pair to 
   ocamlyacc :( *)
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
  | Parser_c.EOF (i) -> i

  | Parser_c.THigherOrderMacro (i) -> i
  | Parser_c.THigherOrderExprStatement (i) -> i
  | Parser_c.THigherOrderExprExprStatement (i) -> i
  | Parser_c.THigherOrderExprExprExprStatement (i) -> i

  | Parser_c.Tinline (i) -> i


(*****************************************************************************)
(* parsing *)
(*****************************************************************************)
let parse file = 
  let lexbuf = Lexing.from_channel (open_in file) in
  let result = Parser_c.main Lexer_c.token lexbuf in
  result


(* old: 
 * let parse_gen parsefunc s = 
 *   let lexbuf = Lexing.from_string s in
 *   let result = parsefunc Lexer_c.token lexbuf in
 *   result
*)

let parse_gen parsefunc s = 

  let toks = tokens_string s in 
  let toks = toks +> List.filter (function 
    | Parser_c.TComment _ 
    | Parser_c.TCommentSpace _ 
    | Parser_c.TCommentCpp _ 
    | Parser_c.TCommentAttrOrMacro _ -> false 
    | _ -> true) 
  in

  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function = 
          (fun xxxxx -> 
            if (match !cur_tok with  Parser_c.EOF x -> true | _ -> false)
            then (pr2 "ALREADY AT END"; !cur_tok)
            else
              let v = pop2 all_tokens in
              cur_tok := v;
              !cur_tok
          )in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in
  let result = parsefunc lexer_function lexbuf_fake in
  result



(* parse_gen Parser_c.statement "(struct us_data*)psh->hostdata = NULL;" *)


(*---------------------------------------------------------------------------*)
let parse_print_error file = 
  let chan = (open_in file) in
  let lexbuf = Lexing.from_channel chan in
  try 
    lexbuf 
      +> Parser_c.main 
          Lexer_c.token
          (* (fun x -> Lexer_c.token x +> (fun v -> pr2 (Dumper.dump v); v)) *)
      +>  (fun x -> x) 
           (* +> (fun x -> pr2 (Dumper.dump x);x ) *)
     (*   +> Semantic.check *)
  with 
  | Lexer_c.Lexical s ->          
      failwith ("lexical error " ^ s ^ "\n =" ^  
                (error_message file (wrap_lexbuf_info lexbuf) ))
  | Parsing.Parse_error ->        
      failwith ("parse error \n = " ^            
                (error_message file (wrap_lexbuf_info lexbuf) ))
  | Semantic_c.Semantic (s, i) -> 
      failwith ("semantic error " ^ s ^ "\n =" ^ 
                (error_message file (wrap_lexbuf_info lexbuf) ))
  | e -> raise e



(*---------------------------------------------------------------------------*)
open Parser_c
open Lexer_parser

let _LookAhead = 10



(* note: as now go in 2 pass,  there is first all the error message of 
 * the lexer, and then the error of the parser. It is no more interwinded.
 *)
let parse_print_error_heuristic file = 

  let table = Common.full_charpos_to_pos file in
  let filelines = (""::Common.cat file) +> Array.of_list in

  (* pr2 ("1" ^ timenow()); *)

  (* bugfix: the lexer too do some conversion (ok, he does need anymore, 
     but still) so have to keep lexer here too !!!  *)
  Lexer_parser.lexer_reset_typedef(); 
  let toks = tokens file in 
  let toks = toks +> List.filter (function 
    | TComment _ 
    | TCommentSpace _ 
    | TCommentCpp _ 
    | TCommentAttrOrMacro _ -> false 
    | _ -> true) 
  in
  (* pr2 ("2" ^ timenow()); *)


  (* The variable that follows allow for error recovery.
   *
   * They are now also used for my lalr(k) technique. Indeed They store 
   * the futur and previous tokens that were parsed, and so provide enough
   * context information for powerful lex trick.
   *
   * normally we have:
   * toks = (reverse passed_tok) ++ cur_tok ++ all_tokens   
   *    after the call to pop2.
   * toks = (reverse passed_tok) ++ all_tokens   
   *     at the and of the lexer_function call.
   * At the very beginning, cur_tok and all_tokens overlap, but not after.
   * At the end of lexer_function call,  cur_tok  overlap  with passed_tok.
   * It is complicated because we cant modify ocamllex and ocamlyacc. 
   * As we want some extended lexing tricks, we have to use such globals. 
   *)
  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in
  let passed_tok = ref [] in

  (* normally equal to passed_tok, but this one is used only to put good stuff 
   *  in NotParsedCorrectly 
   *)
  let passed_tok2 = ref [] in 


  (* --------------------------------------------------------------------- *)
  (* hacked_lex *)
  (* --------------------------------------------------------------------- *)

  (* LALR(k) trick. We can do stuff by adding cases in lexer_c.mll, but
   * it is more general to do it via my LALR(k) tech. Because here we
   * can transform some token give some context information. So sometimes
   * it makes sense to transform a token in one context, sometimes not, 
   * and lex can not provide us this context information.
   * Note that the order in the pattern matching is important. Do not 
   * cut/paste.
   *)

  let rec lexer_function = 
          (fun lexbuf -> 
            if (match !cur_tok with  Parser_c.EOF x -> true | _ -> false)
            then (pr2 "ALREADY AT END"; !cur_tok)
            else
           
            let v = pop2 all_tokens in
            cur_tok := v;

            (*---------------------------------------------------------------*)
            (* typedef_fix1 *)
            (*---------------------------------------------------------------*)
            let v = match v with
            | TIdent (s, ii) -> 
                if Lexer_parser.is_typedef s 
                then TypedefIdent (s, ii) (* +> (fun v -> pr2 ("TYPEDEF:" ^ s); v)   *)
                else TIdent (s, ii)      (* +> (fun v -> pr2 ("IDENT:" ^ s); v) *)
            | x -> x
            in


            let msg_typedef s = 
              match s with
              | "u_char"  -> () 
              | "u_short" -> () 
              | "u_int"   -> () 
              | "u_long"  -> () 
              | "u8"  -> () 
              | "u16" -> () 
              | "u32"   -> () 
              | "u64"  -> () 
              | "s8"  -> () 
              | "s16" -> () 
              | "s32"   -> () 
              | "s64"  -> () 
              | "__u8"  -> () 
              | "__u16" -> () 
              | "__u32"   -> () 
              | "__u64"  -> () 

              | "acpi_handle" -> ()
              | "acpi_status" -> ()

              | s when s =~ ".*_t$" -> ()
              | _ ->  
                  if !Flag_parsing_c.debug_typedef
                  then pr2 ("CERTAINLY TYPEDEF, promoting: " ^ s)
            in

            let v = 
              try (
              match (v::(take_safe _LookAhead !all_tokens),  !passed_tok ) with

              (* special cases scsi/g_NCR5380 *)
              | (TIdent ("ANDP",i1)::TIdent (_,_)::_,   _) -> 
                  TComma i1

              | (TIdent ("ANDP",i1)::TOPar _::_,   _) -> 
                  TComma i1

              (*-------------------------------------------------------------*)
              (* stringification of ident *)
              (*-------------------------------------------------------------*)
              | (TIdent (s,i1)::_,   TOPar _::TIdent ("printk", _)::_) -> 
                  TString ((s, Ast_c.IsChar), i1)

              | (TIdent (s,i1)::TString (_,_)::_,   _) -> 
                  TString ((s, Ast_c.IsChar), i1)

              | (TIdent (s,i1)::_,   TString _::_) -> 
                  TString ((s, Ast_c.IsChar), i1)


                   
              (*-------------------------------------------------------------*)
              (* typedef inference *)
              (*-------------------------------------------------------------*)
                (*  xx xx *)
              | (TIdent (s,i1)::TIdent (s2,i2)::_  , _)
                when (((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true)) && 
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true))) 
                    && s = s2

                -> 
                         (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
                  msg_typedef s; 
                  if !Flag_parsing_c.debug_typedef 
                  then pr2 ("DISABLE typedef cos special case: " ^ s); 
                  (* parse_typedef_fix3:
                            acpi_object		acpi_object;
                      mal pars�, car pas le temps d'appeler  dt()  dans le type_spec. car le parser en interne
                     a deja appel� le prochain token  pour pouvoir decider des choses.
                     => special case in lexer_heuristic, again
                  *)
                  Lexer_parser.disable_typedef();
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)

                (* xx yy *)
              | (TIdent (s, i1)::TIdent (s2, i2)::_  , _)
                when (((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true)) && 
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true))) -> 
                         (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (* [,(] xx [,)] AND param decl *)
              | (TIdent (s, i1)::(TComma _|TCPar _)::_ , (TComma _ |TOPar _)::_ )
                when 
                     ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                    && !Lexer_parser._lexer_hint.parameterDeclaration
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)

                (* xx* [,)] *)
                (* specialcase:  [,(] xx* [,)] *)
              | (TIdent (s, i1)::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
                when 
                     ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                    (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (* xx** [,)] *)
                (* specialcase:  [,(] xx** [,)] *)
              | (TIdent (s, i1)::TMul _::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
                when 
                     ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                    (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)

               (*------------------------------------------------------------*)
               (* if 'x*y' maybe an expr, maybe just a classic multiplication *)
               (* but if have a '=', or ','   I think not *)
               (*------------------------------------------------------------*)

                (* static xx * yy  *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::_ , (Tregister _|Tstatic _   |Tvolatile _|Tconst _)::_)
                  -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)

                (*  TODO  xx * yy ; AND in start of compound element  *)


                (*  xx * yy,      AND  in paramdecl *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  &&
                  !Lexer_parser._lexer_hint.parameterDeclaration -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (*  xx * yy ;     AND in Toplevel  *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  &&
                  !Lexer_parser._lexer_hint.toplevel 
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (*  xx * yy (     AND in Toplevel  *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOPar _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  &&
                  !Lexer_parser._lexer_hint.toplevel 
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)

                (* xx * yy [ *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOCro _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  &&
                  !Lexer_parser._lexer_hint.toplevel 
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
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
                  Lexer_parser.add_typedef s;
                  TypedefIdent s
               *)

                (*  xx * yy =  *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TEq _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (*  xx * yy)      AND in paramdecl *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TCPar _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                    &&
                  !Lexer_parser._lexer_hint.parameterDeclaration -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (*  xx * yy; *) (* wrong ? *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , (TOBrace _| TPtVirg _)::_)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                    ->
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (*  xx * yy,  and ';' before xx *) (* wrong ? *)
              | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , (TOBrace _| TPtVirg _)::_)
                    ->
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)




                (*  xx ** yy *)  (* wrong ? *)
              | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)

                (*  xx ** ) *)
              | (TIdent (s, i1)::TMul _::TMul _::TCPar _::_ , _)
                when ((match take_safe 1 !passed_tok with [Tstruct _] -> false | _ -> true) &&
                     ((match take_safe 1 !passed_tok with [Tenum _] -> false | _ -> true)))
                  (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
                -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TypedefIdent (s, i1)


                (*  (xx) yy *)
              | (TOPar info::TIdent (s, i1)::TCPar _::(TIdent _|TInt _)::_ , x::_)  when (match x with Tif _ -> false | Twhile _ -> false | _ -> true) -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TOPar info

                (*  (xx * ) yy *)
              | (TOPar info::TIdent (s, i1)::TMul _::TCPar _::TIdent (s2, i2)::_ , _) -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  TOPar info



             (* can have sizeof on expression
              | (Tsizeof::TOPar::TIdent s::TCPar::_,   _) -> 
                  msg_typedef s; 
                  Lexer_parser.add_typedef s;
                  Tsizeof
              *)

              (*-------------------------------------------------------------*)
              (* higher order macro, iterator macro, debug macro *)
              (*-------------------------------------------------------------*)
               (* todo: if ident contain a  for_each,  then certainly a macro.
                * but to be sure should look if there is a { after the (, but
                * it requires to count the '('. Because this can be expensive,
                * maybe can do that only when the token contain "for_each".
                * less: do a pr2 when encounter a for_each ?
               *)

              | (TIdent (s, i1)::TOPar _::Tif _::_ ,     _)
                  (* && !Lexer_parser._lexer_hint = Some Lexer_parser.ParameterDeclaration *)
                -> 
                 if !Flag_parsing_c.debug_cpp 
                 then pr2 ("CERTAINLY HIGHER ORDER MACRO, transforming: " ^ s);
                 THigherOrderMacro i1
                  
              (*-------------------------------------------------------------*)
              (* CPP *)
              (*-------------------------------------------------------------*)
              | TDefine ii::_, _ -> 
                  if !Lexer_parser._lexer_hint.toplevel
                  then TDefine ii
                  else begin
                    if !Flag_parsing_c.debug_cpp
                    then pr2 ("DEFINE inside function, I treat it as comment");
                    TCommentCpp ii
                  end

               (* do same for include often found inside structdef *)
              | TInclude ii::_, _ -> 
                  if !Lexer_parser._lexer_hint.toplevel
                  then TInclude ii
                  else begin
                    if !Flag_parsing_c.debug_cpp
                    then pr2 ("INCLUDE inside function, I treat it as comment");
                    TCommentCpp ii
                  end

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
              | _ -> v

               ) with _ -> pr2 "ESN"; v
            in
         passed_tok := v::!passed_tok;
         passed_tok2 := v::!passed_tok2;
         if !Flag_parsing_c.debug_lexer then pr2 (Dumper.dump v);  
         match v with
         | TCommentCpp _ -> lexer_function lexbuf
         | v -> 
             v)
  in
  (* --------------------------------------------------------------------- *)

  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in

  (* error reporting, stat, and recovery *)

  let current_line () = 
    let (info,_) = info_from_token !cur_tok in
    fst table.(info.charpos)
   in

  let stat = default_stat file in

  (* opti ? *)
  let get_slice_file filename (line1, line2) = 
   if not !Flag_parsing_c.opti_parsing then
    cat filename 
      +> drop (line1 - 1)
      +> take (line2 - line1 + 1)
      +> unlines
    else "TODO remove opti_parsing flag"
  in

  (* opti ? *)
  let build_info_item point1 point2 = 
   (* if not !Flag_parsing_c.opti_parsing then *)
    (* todo: give correct column, and charpos (for the moment not needed) *)
   (* let info_of_toks = (List.rev (List.map info_from_token !passed_tok2)) in *)
    let info_of_toks = (Common.map_eff_rev info_from_token !passed_tok2) in
    (file, ( ((point1, 0), 0), ((point2, 0), 0)),  get_slice_file file (point1, point2), info_of_toks) 
(*   else 
    (file, ( ((point1, 0), 0), ((point2, 0), 0)),  get_slice_file file (point1, point2), [])
*)
  in




  (* --------------------------------------------------------------------- *)
  (* main loop *)
  (* --------------------------------------------------------------------- *)
  let rec loop () =

    if !Lexer_parser._handle_typedef = false && !Flag_parsing_c.debug_typedef
    then pr2 "FALSE _handle_typedef, not normal if dont come from exn";

    Lexer_parser._handle_typedef := true;  (* normally have to do that only when come from an exception in which case the dt() may not have been done *)
    (* TODO but if was in scoped scope ? have to let only the last scope *)
    (* Lexer_parser.lexer_reset_typedef (); *)
    Lexer_parser._lexer_hint := {(default_hint ()) with toplevel = true; };

    passed_tok2 := [];

    (* todo?: I am not sure that it represents current_line, cos maybe cur_tok partipated in the previous *)
    (*   parsing phase, so maybe cur_tok is not the first token of the next parsing phase *)
    (*  same with checkpoint2. better would be to record when we have a } or ; in parser.mly,  cos we know *)
    (*   that they are the last symbols of external_declaration2 *)
    let checkpoint = current_line () in

    (try 
      (match 
        lexbuf_fake 
          +> Parser_c.external_declaration2 
               lexer_function  
               (* (fun x -> lexer_function x +> (fun v -> pr2 (Dumper.dump v); v))  *)
           +>  (fun x -> x ) 
          (* +>  (fun x -> pr2 (Dumper.dump x);x )  *)
          (* +> Semantic.check  *)
      with
      | Ast_c.FinalDef x -> 
          let checkpoint2 = current_line () in
          stat.correct <- stat.correct + (checkpoint2 - checkpoint);

          [(Ast_c.FinalDef x, build_info_item checkpoint checkpoint2)]
      | xs -> 
          (* pr2 (Dumper.dump xs);  *)
          (* pr2 (Dumper.dump (Lexer_parser._handle_typedef, Lexer_parser.typedef));  *)

          let checkpoint2 = current_line () in

          (* pr2 ("correctcount: " ^ i_to_s (checkpoint2 - checkpoint)); *)
          (* for i = checkpoint to checkpoint2 do pr2 ("correct:" ^ filelines.(i)) done; *)
          stat.correct <- stat.correct + (checkpoint2 - checkpoint);
           
          let xs = xs +> (*List.map*) (fun x -> (x, build_info_item checkpoint checkpoint2)) in
          xs :: loop ()
      )
    with e -> 
      begin
        (match e with
         | Lexer_c.Lexical s ->          
            if not !Flag_parsing_c.opti_parsing then
            pr2 ("lexical error " ^ s ^ "\n =" ^  
                 (error_message file (wrap_parse_info (info_from_token !cur_tok +> fst)  ) ))
         | Parsing.Parse_error ->        
            if not !Flag_parsing_c.opti_parsing then
            pr2 ("parse error \n = " ^            
                 (error_message file (wrap_parse_info (info_from_token !cur_tok +> fst)  ) ))
         | Semantic_c.Semantic (s, i) -> 
            if not !Flag_parsing_c.opti_parsing then
            pr2 ("semantic error " ^ s ^ "\n =" ^ 
                 (error_message file (wrap_parse_info (info_from_token !cur_tok +> fst)  ) ))
         | e -> 
            raise e
        );
        (*  error recovery, go to next synchro point *)
        
          let line_error = current_line () in
       
          let rec next_sync () =
            let v = (try pop2 all_tokens with _ -> raise End_of_file) in
            cur_tok := v;
            let (line, col) = (table.((info_from_token v +> fst).charpos)) in
            passed_tok2 := v::!passed_tok2;

            (* enough ? *)
            passed_tok := [];

            let when_found () =
                  let checkpoint2 = line in
                  pr2 ("badcount: " ^ i_to_s (checkpoint2 - checkpoint));
                  for i = checkpoint to checkpoint2 do 
                      if i = line_error 
                      then  pr2 ("BAD:!!!!!" ^ " " ^ filelines.(i)) 
                      else  pr2 ("bad:" ^ " " ^      filelines.(i)) 
                  done;
                  stat.bad <- stat.bad + (checkpoint2 - checkpoint);
            in

            match  (v, col ) with
            | (TCBrace _, 0)  -> 
                  pr2 ("FOUND SYNC at line "^ i_to_s line);
                
                  when_found();

                  (* perhaps a }; ? *)
                  (* obsolete now, because parser.mly allow empty ';' *)
                  let v = (try List.hd !all_tokens with _ -> raise End_of_file)
                  in
                  (match v with
                  | TPtVirg _ -> 
                      pr2 "FOUND SYNC bis, eating } and ;";
                      let v = (try pop2 all_tokens with _ -> raise End_of_file)
                      in
                      cur_tok := v;
                      passed_tok2 := v::!passed_tok2;
                  | _ -> ()
                  )
                       
(* reput ? why in comment ?
            | (Tstatic _, 0)  -> 
                  pr2 ("FOUND SYNC 2 at line "^ i_to_s line);
                  when_found();
*)


            | _ -> (* pr2 ("passing through:" ^ filelines.(line));    *)
                   next_sync ()
          in
          try 
           next_sync();

           (*  again, not sure it corresponds to end of bad region *)
           let checkpoint2 = current_line () in
           
           (* bugfix: do it here cos if put in in the full exprt NotParsedCorrectly ( ...!passed_tok2) ...::loop()  then  bug *)
           let info_of_bads = 
             (* if not !Flag_parsing_c.opti_parsing then *)
             (* opti: (List.rev (List.map info_from_token !passed_tok2))  *)
             (Common.map_eff_rev info_from_token !passed_tok2) 
             (* else [] *)
           in
           let builded_info = build_info_item checkpoint checkpoint2 in
           (Ast_c.NotParsedCorrectly info_of_bads , builded_info)
           ::loop ()
          with End_of_file -> pr2 "END OF FILE WHILE IN RECOVERY MODE"; []
      end
   ) in


  (* still? Lexer_parser.lexer_reset_typedef (); *)
  let v = loop() in
  (v, stat)



