open Common open Commonop

module TH = Token_helpers 
module LP = Lexer_parser

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.pr2 s
    
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let token_to_strpos tok = 
  (TH.str_of_tok tok, TH.pos_of_tok tok)


let error_msg_tok file tok = 
  if !Flag_parsing_c.verbose_parsing
  then Common.error_message file (token_to_strpos tok) 
  else ("error in " ^ file ^ "set verbose_parsing for more info")


let print_bad line_error (start_line, end_line) filelines  = 
  begin
    pr2 ("badcount: " ^ i_to_s (end_line - start_line));
    for i = start_line to end_line do 
      if i = line_error 
      then  pr2 ("BAD:!!!!!" ^ " " ^ filelines.(i)) 
      else  pr2 ("bad:" ^ " " ^      filelines.(i)) 
    done
  end



let mk_info_item2 filename toks = 
  let toks' = List.rev toks in
  let buf = Buffer.create 100 in
  let s = 
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks' +> List.iter (fun tok -> 
        let s = TH.str_of_tok tok in
        match Ast_c.mark_of_info (TH.info_of_tok tok) with
        | Ast_c.OriginTok -> Buffer.add_string buf s
        | Ast_c.AbstractLineTok -> raise Impossible
        | _ -> ()
      );
      Buffer.contents buf
    end
  in
  (s, toks') 

let mk_info_item a b = 
  Common.profile_code "C parsing.mk_info_item" 
    (fun () -> mk_info_item2 a b)



(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
type parsing_stat = {
    filename: filename;
    mutable have_timeout: bool;

    mutable correct: int;  
    mutable bad: int;

    mutable passed: int; (* by our cpp commentizer *)

    (* if want to know exactly what was passed through
     * mutable passing_through_lines: int;
     * it differs from bad by starting from the error to
     * the synchro point instead of strating from start of
     * function to end of function.
     *)

  } 

let default_stat file =  { 
    filename = file;
    have_timeout = false;
    correct = 0; bad = 0;
    passed = 0;
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

  pr2 "\n\n\n";
  pr2 "files with lots of tokens passed/commentized:";
  let threshold_passed = 100 in
  statxs 
    +> List.filter (function 
      | {passed = n} when n > threshold_passed -> true
      | _ -> false)
    +> List.iter (function 
        {filename = file; passed = n} -> 
          pr2 (file ^ "  " ^ (i_to_s n));
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
  let passed = (statxs +> List.fold_left (fun acc {passed = x} -> acc+x) 0)  in
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  pr2 (
  (sprintf "nb good = %d,  nb passed = %d " good passed) ^
  (sprintf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "%")
   );
  pr2 (
  (sprintf "nb good = %d,  nb bad = %d " good bad) ^
  (sprintf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )


(*****************************************************************************)
(* Stats on what was passed/commentized  *)
(*****************************************************************************)

let commentized xs = xs +> Common.map_filter (function
  | Parser_c.TCommentCpp ii
  | Parser_c.TCommentMisc ii
  | Parser_c.TAction ii ->
      Some (ii.Ast_c.pinfo)
  | _ -> None
 )
  
let count_lines_tokens_commentized xs = 
  let line = ref (-1) in
  let count = ref 0 in
  begin
    commentized xs +> List.iter (fun pinfo -> 
      let newline = pinfo.Common.line in
      if newline <> !line
      then begin
        line := newline;
        incr count
      end
    );
    !count
  end



let print_tokens_commentized xs = 
  let line = ref (-1) in
  begin
    let ys = commentized xs in
    ys +> List.iter (fun pinfo -> 
      let newline = pinfo.Common.line in
      let s = pinfo.Common.str in
      let s = Str.global_substitute 
        (Str.regexp "\n") (fun s -> "") s 
      in
      if newline = !line
      then prerr_string (s ^ " ")
      else begin
        if !line = -1 
        then pr2_no_nl "passed:" 
        else pr2_no_nl "\npassed:";
        line := newline;
        pr2_no_nl (s ^ " ");
      end
    );
    if not (null ys) then pr2 "";
  end
      



(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* called by parse_print_error_heuristic *)
let tokens2 file = 
 let table     = Common.full_charpos_to_pos file in

 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in
  try 
    let rec tokens_aux () = 
      let tok = Lexer_c.token lexbuf in
      (* add the line x col information *)
      let tok = tok +> TH.visitor_info_of_tok (fun ii -> { ii with Ast_c.pinfo=
          Common.complete_parse_info file table ii.Ast_c.pinfo
      })
      in
      if TH.is_eof tok
      then [tok]
      else tok::(tokens_aux ())
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
      let tok = Lexer_c.token lexbuf in
      if TH.is_eof tok
      then [tok]
      else tok::(tokens_s_aux ())
    in
    tokens_s_aux ()
  with
    | Lexer_c.Lexical s -> failwith ("lexical error " ^ s ^ "\n =" )
    | e -> raise e


(*****************************************************************************)
(* Parsing, but very basic, no more used *)
(*****************************************************************************)

(*
 * !!!Those function use refs, and are not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 *)

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

(*
 * !!!Those function use refs, and are not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 *)


(* old: 
 * let parse_gen parsefunc s = 
 *   let lexbuf = Lexing.from_string s in
 *   let result = parsefunc Lexer_c.token lexbuf in
 *   result
*)

let parse_gen parsefunc s = 
  let toks = tokens_string s +> List.filter TH.is_not_comment in

  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function = 
    (fun _ -> 
      if TH.is_eof !cur_tok
      then (pr2 "LEXER: ALREADY AT END"; !cur_tok)
      else
        let v = Common.pop2 all_tokens in
        cur_tok := v;
        !cur_tok
    ) 
  in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in
  let result = parsefunc lexer_function lexbuf_fake in
  result


let type_of_string      = parse_gen Parser_c.type_name
let statement_of_string = parse_gen Parser_c.statement
let expression_of_string = parse_gen Parser_c.expr

(* ex: statement_of_string "(struct us_data* )psh->hostdata = NULL;" *)






(*****************************************************************************)
(* Error recovery *)
(*****************************************************************************)

(* todo: do something if find Parser_c.Eof ? *)
let rec find_next_synchro next already_passed =

  (* Maybe because not enough }, because for example an ifdef that
   * contains in both branch some opening {, we later eat too much,
   * "on deborde sur la fonction d'apres". So maybe we can find synchro
   * point inside already_passed instead of looking in next. But take
   * care! must progress. We must not stay in infinite loop! So look
   * at premier(external_declaration2) in parser.output and pass at
   * least those first tokens. 
   * 
   * I have chosen to start search for next synchro point after the 
   * first { I found, so quite sure we will not loop.
   *)

  let last_round = List.rev already_passed in
  let (before, after) = 
    Common.span (fun tok -> 
      match tok with
      | Parser_c.TOBrace _ -> false
      | Parser_c.TDefine _ -> false
      | _ -> true
    ) last_round
  in
  find_next_synchro_orig (after ++ next)  (List.rev before)
    
    

and find_next_synchro_orig next already_passed =
  match next with
  | [] ->  
      pr2 "ERROR-RECOV: end of file while in recovery mode"; 
      already_passed, []

  | (Parser_c.TDefEOL i as v)::xs  -> 
      pr2 ("ERROR-RECOV: found sync end of #define "^i_to_s(TH.line_of_tok v));
      v::already_passed, xs

  | (Parser_c.TCBrace i as v)::xs when TH.col_of_tok v = 0 -> 
      pr2 ("ERROR-RECOV: found sync '}' at line "^i_to_s (TH.line_of_tok v));

      (match xs with
      | [] -> raise Impossible (* there is a EOF token normally *)

      (* still useful: now parser.mly allow empty ';' so normally no pb *)
      | Parser_c.TPtVirg iptvirg::xs -> 
          pr2 "ERROR-RECOV: found sync bis, eating } and ;";
          (Parser_c.TPtVirg iptvirg)::v::already_passed, xs

      | Parser_c.TIdent x::Parser_c.TPtVirg iptvirg::xs -> 
          pr2 "ERROR-RECOV: found sync bis, eating ident, }, and ;";
          (Parser_c.TPtVirg iptvirg)::(Parser_c.TIdent x)::v::already_passed, 
          xs
            
      | Parser_c.TCommentSpace sp::Parser_c.TIdent x::Parser_c.TPtVirg iptvirg
        ::xs -> 
          pr2 "ERROR-RECOV: found sync bis, eating ident, }, and ;";
          (Parser_c.TCommentSpace sp)::
            (Parser_c.TPtVirg iptvirg)::
            (Parser_c.TIdent x)::
            v::
            already_passed, 
          xs
            
      | _ -> 
          v::already_passed, xs
      )
  | v::xs when TH.col_of_tok v = 0 && TH.is_start_of_something v  -> 
      pr2 ("ERROR-RECOV: found sync col 0 at line "^ i_to_s(TH.line_of_tok v));
      already_passed, v::xs
        
  | v::xs -> 
      find_next_synchro_orig xs (v::already_passed)

      
(*****************************************************************************)
(* Include/Define hacks *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* helpers *)
(* ------------------------------------------------------------------------- *)

(* used to generate new token from existing one *)
let new_info posadd str ii = 
  { Ast_c.pinfo = 
      { ii.Ast_c.pinfo with 
        charpos = ii.Ast_c.pinfo.charpos + posadd;
        str     = str;
        column = ii.Ast_c.pinfo.column + posadd;
      };
    (* must generate a new ref each time, otherwise share *)
    cocci_tag = ref Ast_c.emptyAnnot;
    mark = Ast_c.OriginTok;
  }


let rec comment_until_defeol xs = 
  match xs with
  | [] -> failwith "cant find end of define token TDefEOL"
  | x::xs -> 
      (match x with
      | Parser_c.TDefEOL i -> 
          Parser_c.TCommentCpp (TH.info_of_tok x)::xs
      | _ -> 
          Parser_c.TCommentCpp (TH.info_of_tok x)::comment_until_defeol xs
      )



(* ------------------------------------------------------------------------- *)
(* returns a pair (replaced token, list of next tokens) *)
(* ------------------------------------------------------------------------- *)

let tokens_include (info, includes, filename) = 
  Parser_c.TIncludeStart (Ast_c.rewrap_str includes info), 
  [Parser_c.TIncludeFilename 
      (filename, (new_info (String.length includes) filename info))
  ]

(*****************************************************************************)
(* Parsing default define, standard.h *)
(*****************************************************************************)

let parse_cpp_define_file file = 
  let toks = tokens file in
  let toks = Parsing_hacks.fix_tokens_define toks in
  Parsing_hacks.extract_cpp_define toks


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type info_item =  string * Parser_c.token list

type program2 = toplevel2 list
     and toplevel2 = Ast_c.toplevel * info_item


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
 * - passed_tokens_clean is used for lookahead, in fact for lookback.
 * - remaining_tokens_clean is used for lookahead. Now remaining_tokens
 *   contain some comments and so would make pattern matching difficult
 *   in lookahead. Hence this variable. We would like also to get rid 
 *   of cpp instruction because sometimes a cpp instruction is between
 *   two tokens and makes a pattern matching fail. But lookahead also
 *   transform some cpp instruction (in comment) so can't remove them.

 * So remaining_tokens, passed_tokens_last_ckp contain comment-tokens,
 * whereas passed_tokens_clean and remaining_tokens_clean does not contain
 * comment-tokens.

 * Normally we have:
 * toks = (reverse passed_tok) ++ cur_tok ++ remaining_tokens   
 *    after the call to pop2.
 * toks = (reverse passed_tok) ++ remaining_tokens   
 *     at the and of the lexer_function call.
 * At the very beginning, cur_tok and remaining_tokens overlap, but not after.
 * At the end of lexer_function call,  cur_tok  overlap  with passed_tok.
 * 
 * !!!This function use refs, and is not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 *)


let parse_print_error_heuristic2 file = 

  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)
  LP.lexer_reset_typedef(); 
  let toks = tokens file in
  let toks = Parsing_hacks.fix_tokens_define toks in
  let toks = Parsing_hacks.fix_tokens_cpp toks in

  let filelines = (""::Common.cat file) +> Array.of_list in

  let stat = default_stat file in

  let remaining_tokens       = ref toks in
  let remaining_tokens_clean = ref (toks +> List.filter TH.is_not_comment) 
  in
  let cur_tok                = ref (List.hd !remaining_tokens) in
  let passed_tokens_last_ckp = ref [] in 
  let passed_tokens_clean    = ref [] in

  (* hacked_lex *)
  let rec lexer_function = (fun lexbuf -> 

    if TH.is_eof !cur_tok
    then begin pr2 "ALREADY AT END"; !cur_tok end
    else begin
      let v = pop2 remaining_tokens in
      cur_tok := v;

      if !Flag_parsing_c.debug_lexer then pr2 (Dumper.dump v);

      if TH.is_comment v
      then begin
        passed_tokens_last_ckp := v::!passed_tokens_last_ckp;
        lexer_function lexbuf
      end
      else begin
        let x = pop2 remaining_tokens_clean  in
        assert (x = v);

        (match v with

        | Parser_c.TDefine (tok) -> 
            if not !LP._lexer_hint.LP.toplevel 
            then begin
              pr2 ("CPP-DEFINE: inside function, I treat it as comment");
              let v' = Parser_c.TCommentCpp (TH.info_of_tok v) in
              passed_tokens_last_ckp := v'::!passed_tokens_last_ckp;
              remaining_tokens := comment_until_defeol !remaining_tokens;
              remaining_tokens_clean := 
                List.tl 
                  (drop_until (function Parser_c.TDefEOL _ -> true | _ -> false)
                      !remaining_tokens_clean);
              lexer_function lexbuf
            end
            else begin
              passed_tokens_last_ckp := v::!passed_tokens_last_ckp;
              passed_tokens_clean := v::!passed_tokens_clean;
              v
            end

        | Parser_c.TInclude (includes, filename, info) -> 
            if not !LP._lexer_hint.LP.toplevel 
            then begin
              pr2 ("CPP-INCLUDE: inside function, I treat it as comment");
              let v = Parser_c.TCommentCpp info in
              passed_tokens_last_ckp := v::!passed_tokens_last_ckp;
              lexer_function lexbuf
            end
            else begin
              let (v,new_tokens) = tokens_include (info, includes, filename)
              in
              let new_tokens_clean = 
                new_tokens +> List.filter TH.is_not_comment
              in
              passed_tokens_last_ckp := v::!passed_tokens_last_ckp;
              passed_tokens_clean := v::!passed_tokens_clean;
              remaining_tokens := new_tokens ++ !remaining_tokens;
              remaining_tokens_clean := 
                new_tokens_clean ++ !remaining_tokens_clean;
              v
            end
            
        | _ -> 

            (* typedef_fix1 *)
            let v = match v with
              | Parser_c.TIdent (s, ii) -> 
                  if LP.is_typedef s 
                  then Parser_c.TypedefIdent (s, ii)
                  else Parser_c.TIdent (s, ii)
              | x -> x
            in
          
            let v = Parsing_hacks.lookahead 
              (v::!remaining_tokens_clean) !passed_tokens_clean 
            in

            passed_tokens_last_ckp := v::!passed_tokens_last_ckp;

            (* the lookahead may have change the status of the token and
             * consider it as a comment, for instance some #include are
             * turned into comments hence this code. *)
            match v with
            | Parser_c.TCommentCpp _ -> lexer_function lexbuf
            | v -> 
                passed_tokens_clean := v::!passed_tokens_clean;
                v
        )
      end
    end
  )
  in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in



  let rec loop () =

    if not (LP.is_enabled_typedef()) && !Flag_parsing_c.debug_typedef
    then pr2 "TYPEDEF:_handle_typedef=false. Not normal if dont come from exn";

    (* normally have to do that only when come from an exception in which
     * case the dt() may not have been done 
     * TODO but if was in scoped scope ? have to let only the last scope
     * so need do a LP.lexer_reset_typedef ();
     *)
    LP.enable_typedef();  
    LP._lexer_hint := { (LP.default_hint ()) with LP.toplevel = true; };
    LP.save_typedef_state();

    (* todo?: I am not sure that it represents current_line, cos maybe
     * cur_tok partipated in the previous parsing phase, so maybe cur_tok
     * is not the first token of the next parsing phase. Same with checkpoint2.
     * It would be better to record when we have a } or ; in parser.mly,
     *  cos we know that they are the last symbols of external_declaration2.
     *)
    let checkpoint = TH.line_of_tok !cur_tok in

    passed_tokens_last_ckp := [];

    let elem = 
      (try 
          (* -------------------------------------------------- *)
          (* Call parser *)
          (* -------------------------------------------------- *)
          Parser_c.celem lexer_function lexbuf_fake
        with e -> 
          begin
            (match e with
            (* Lexical is no more launched I think *)
            | Lexer_c.Lexical s -> 
                pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok file !cur_tok)
            | Parsing.Parse_error -> 
                pr2 ("parse error \n = " ^ error_msg_tok file !cur_tok)
            | Semantic_c.Semantic (s, i) -> 
                pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok file !cur_tok)
            | e -> raise e
            );
            LP.restore_typedef_state();
            let line_error = TH.line_of_tok !cur_tok in

            (*  error recovery, go to next synchro point *)
            let (passed_tokens', remaining_tokens') =
              find_next_synchro !remaining_tokens !passed_tokens_last_ckp
            in
            remaining_tokens := remaining_tokens';
            passed_tokens_last_ckp := passed_tokens';

            cur_tok := List.hd passed_tokens';
            passed_tokens_clean := [];           (* enough ? *)

            (* with error recovery, remaining_tokens and
             * remaining_tokens_clean may not be in sync 
             *)
            remaining_tokens_clean := 
              (!remaining_tokens +> List.filter TH.is_not_comment);

            let checkpoint2 = TH.line_of_tok !cur_tok in (* <> line_error *)
            print_bad line_error (checkpoint, checkpoint2) filelines;

            let info_of_bads = 
              Common.map_eff_rev TH.info_of_tok !passed_tokens_last_ckp in 
            Ast_c.NotParsedCorrectly info_of_bads
          end
      ) 
    in

    (* again not sure if checkpoint2 corresponds to end of bad region *)
    let checkpoint2 = TH.line_of_tok !cur_tok in
    let diffline = (checkpoint2 - checkpoint) in
    let info = mk_info_item file !passed_tokens_last_ckp
    in 
    stat.passed <- stat.passed + count_lines_tokens_commentized (snd info);

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
