open Common open Commonop

open Parser_c 


let pr2_cpp s = 
  if !Flag_parsing_c.debug_cpp
  then pr2 ("CPP-" ^ s)

(*****************************************************************************)
(* CPP handling: macros, ifdefs  *)
(*****************************************************************************)

let regexp_macro =  Str.regexp
  "^[A-Z_][A-Z_0-9]*$"

(* opti: better to built it once and for all *)
let regexp_foreach = Str.regexp_case_fold 
  ".*\\(for_?each\\|for_?all\\|iterate\\|loop\\|walk\\|each\\)"


type linecol = { line : int; col : int }

type token_with_pos = Parser_c.token * linecol


let is_same_line line = function 
  | (x, {line = line2}) when line2 = line -> true 
  | _ -> false 

let pos_of_token x = 
  Ast_c.get_pos_of_info (Token_helpers.info_from_token x)

(* ------------------------------------------------------------------------- *)
(* fuzzy parsing, different views over the same program *)
(* ------------------------------------------------------------------------- *)

(*  x list list, because x list separated by ',' *) 
type paren_grouped = 
  | Parenthised   of paren_grouped list list * token_with_pos list
  | NotParenToken of token_with_pos

(*  x list list, because x list separated by #else or #elif *) 
type ifdef_grouped = 
  | Ifdef     of ifdef_grouped list list * token_with_pos list
  | Ifdefzero of ifdef_grouped list list * token_with_pos list
  | NotIfdefLine of token_with_pos list


type 'a line_grouped = 
  Line of 'a list



type body_function_grouped = 
  | BodyFunction of token_with_pos list
  | NotBodyLine  of token_with_pos list


(* ------------------------------------------------------------------------- *)
(* fuzzy builders *)
(* ------------------------------------------------------------------------- *)

(* todo! synchro ! *)
let rec mk_parenthised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x with 
      | TOPar _, _ -> 
          let body, extras, xs = mk_parameters [x] [] xs in
          Parenthised (body,extras)::mk_parenthised xs
      | x -> 
          NotParenToken x::mk_parenthised xs
      )

(* return the body of the parenthised expression and the rest of the tokens *)
and mk_parameters extras acc_before_sep  xs = 
  match xs with
  | [] -> 
      pr2 "PB: not found closing paren in fuzzy parsing";
      [List.rev acc_before_sep], List.rev extras, []
  | x::xs -> 
      (match x with 
      | TCPar _, _ -> 
          [List.rev acc_before_sep], List.rev (x::extras), xs
      | TOPar _, _ -> 
          let body, extrasnest, xs = mk_parameters [x] [] xs in
          mk_parameters extras 
            (Parenthised (body,extrasnest)::acc_before_sep) 
            xs
      | TComma _, _ -> 
          let body, extras, xs = mk_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | x -> 
          mk_parameters extras (NotParenToken x::acc_before_sep) xs
      )
          

(* --------------------------------------- *)
let rec mk_ifdef xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x with 
      | TIfdef _, _ -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdef (body, extra)::mk_ifdef xs
      | TIfdefzero _, _ -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdefzero (body, extra)::mk_ifdef xs
          
      | tok, {line = line1} -> 
          (* todo? can have some Ifdef in the line ? *)
          let line, xs = Common.span (fun x -> is_same_line line1 x) (x::xs) in
          NotIfdefLine line::mk_ifdef xs 
      )

and mk_ifdef_parameters extras acc_before_sep xs = 
  match xs with
  | [] -> 
      pr2 "PB: not found closing ifdef in fuzzy parsing";
      [List.rev acc_before_sep], List.rev extras, []
  | x::xs -> 
      (match x with 
      | TEndif _, _ -> 
          [List.rev acc_before_sep], List.rev (x::extras), xs
      | TIfdef _, _ -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdef (body, extrasnest)::acc_before_sep) xs

      | TIfdefzero _, _ -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdefzero (body, extrasnest)::acc_before_sep) xs

      | TIfdefelse _, _ 
      | TIfdefelif _, _ -> 
          let body, extras, xs = mk_ifdef_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | tok, {line = line1} -> 
          let line, xs = Common.span (fun x -> is_same_line line1 x) (x::xs) in
          mk_ifdef_parameters extras (NotIfdefLine line::acc_before_sep) xs
      )

(* --------------------------------------- *)
let line_of_paren = function
  | NotParenToken (tok, { line = line }) -> line
  | Parenthised (xxs, info_parens) -> 
      (match info_parens with
      | [] -> raise Impossible
      | (tok, {line = line})::xs -> line
      )

let rec iter_token_paren f xs = 
  xs +> List.iter (function
  | NotParenToken tok -> f tok;
  | Parenthised (xxs, info_parens) -> 
      info_parens +> List.iter f;
      xxs +> List.iter (fun xs -> iter_token_paren f xs)
  )


let rec span_line_paren line = function
  | [] -> [],[]
  | x::xs -> 
      if line_of_paren x = line 
      then
        let (l1, l2) = span_line_paren line xs in
        (x::l1, l2)
      else ([], x::xs)
        

let rec mk_line_parenthised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      let line_no = line_of_paren x in
      let line, xs = span_line_paren line_no xs in
      Line (x::line)::mk_line_parenthised xs



(* --------------------------------------- *)
let rec mk_body_function_grouped xs = 
  match xs with 
  | [] -> []
  | x::xs -> 
      (match x with
      | TOBrace _, {col = 0} -> 
          let is_closing_brace = function 
            | TCBrace _, { col = 0 } -> true 
            | _ -> false 
          in
          let body, xs = Common.span (fun x -> not (is_closing_brace x)) xs in
          (match xs with
          | (TCBrace _, { col = 0 })::xs -> 
              BodyFunction body::mk_body_function_grouped xs
          | [] -> 
              pr2 "PB:not found closing brace in fuzzy parsing";
              [NotBodyLine body]
          | _ -> raise Impossible
          )
          
      | tok, { line = line1} -> 
          let line, xs = Common.span (fun x -> is_same_line line1 x) (x::xs) in
          NotBodyLine line::mk_body_function_grouped xs 
      )




(* ------------------------------------------------------------------------- *)
(* annotation and stringification part 1 *)
(* ------------------------------------------------------------------------- *)

let keyword_table = Common.hash_of_list [
  (* attributes. could perhaps generalize via "__.*" *)
  "__init",                     (fun ii -> TCommentCpp ii); 
  "__exit",                     (fun ii -> TCommentCpp ii); 
  "__user",                     (fun ii -> TCommentCpp ii); 
  "__iomem",                    (fun ii -> TCommentCpp ii); 
  "__initdata",                 (fun ii -> TCommentCpp ii); 
  "__exitdata",                 (fun ii -> TCommentCpp ii); 
  "__cacheline_aligned",        (fun ii -> TCommentCpp ii); 
  "____cacheline_aligned",      (fun ii -> TCommentCpp ii); 
  "__cacheline_aligned_in_smp", (fun ii -> TCommentCpp ii);
  "__devinit",                  (fun ii -> TCommentCpp ii); 
  "__devexit",                  (fun ii -> TCommentCpp ii); 
  "__devinitdata",              (fun ii -> TCommentCpp ii); 
  "__ALIGNED__",                (fun ii -> TCommentCpp ii); 
  "asmlinkage",                 (fun ii -> TCommentCpp ii);  

  "__pmac",                    (fun ii -> TCommentCpp ii);  
  "__force",                    (fun ii -> TCommentCpp ii);  
  "__nocast",                    (fun ii -> TCommentCpp ii);  

  "DIVA_EXIT_FUNCTION",                    (fun ii -> TCommentCpp ii);  
  "DIVA_INIT_FUNCTION",                    (fun ii -> TCommentCpp ii);  
  "ACPI_SYSTEM_XFACE",                    (fun ii -> TCommentCpp ii);  

  "__CS4231_INLINE__",                    (fun ii -> TCommentCpp ii);  

  "__init_or_module",                    (fun ii -> TCommentCpp ii);  

  "WPMINFO", (fun ii -> TCommentCpp ii);




  (* string macro. normally handle quite well by mu lalr(k), but
   * sometimes not enough, if have for instance the XX YY case, could
   * be considered as a declaration with XX being a typedef, so would
   * Have ambiguity. So at least by adding this special case, we can
   * catch more correct string-macro, no more a XX YY but now a good
   * "XX" YY *)

  "KERN_INFO",    (fun ii -> TString(("KERN_INFO",Ast_c.IsChar),ii));
  "KERN_ERR",     (fun ii -> TString(("KERN_ERR",Ast_c.IsChar),ii));
  "KERN_CRIT",    (fun ii -> TString(("KERN_CRIT",Ast_c.IsChar),ii));
  "KERN_DEBUG",   (fun ii -> TString(("KERN_DEBUG",Ast_c.IsChar),ii));
  "KERN_WARNING", (fun ii -> TString(("KERN_WARNING",Ast_c.IsChar),ii));
  "KERN_ALERT",   (fun ii -> TString(("KERN_ALERT",Ast_c.IsChar),ii));

]

(* For stringification I need to have at least a witness, a string, 
 * and sometimes have just printk(KERN_WARNING MYSTR) and it could
 * be transformed in a typedef later, so better to at least
 * transform in string already the string-macro we know.
 * 
 * Perhaps better to apply also as soon as possible the 
 * correct macro-annotation tagging (__init & co) to be able to
 * filter them as soon as possible so that they will not polluate
 * our pattern-matching that come later.
 *)

let fix_tokens_annotation_and_stringification_part1 tokens = 
  let tokens = tokens +> List.map (fun x -> 
    match x with
    | TIdent (s, ii) -> 
        (match Common.optionise (fun () -> Hashtbl.find keyword_table s) with
        | Some f -> f ii
        | None -> TIdent (s, ii)
        )
    | x -> x 
  ) in
  tokens




(* ------------------------------------------------------------------------- *)
(* ifdef keeping/passing *)
(* ------------------------------------------------------------------------- *)

let find_and_tag_good_ifdef xs = 
  let (keep_ifdef  : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in
  let (put_comment : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in

  let rec find_ifdef_funheaders = function
    | [] -> ()
    | NotIfdefLine _::xs -> find_ifdef_funheaders xs 

    (* ifdef-funheader if ifdef with 2 lines and a '{' in next line *)
    | Ifdef 
        ([[NotIfdefLine ((xline1, {col = 0})::line1)];
          [NotIfdefLine ((xline2, {col = 0})::line2)]
         ], info_ifdef_stmt 
        )
      ::NotIfdefLine ((TOBrace i, {col = 0})::line3)
      ::xs  
      -> 

        find_ifdef_funheaders xs;
        let all_toks = 
          xline2::(
            (List.map fst line2) @ 
            (List.map fst info_ifdef_stmt)
          )
        in
        all_toks +> List.iter (fun x -> 
          Hashtbl.add !put_comment (pos_of_token x) true;
        )

    | Ifdef 
        ([[NotIfdefLine ((xline1, {col = 0})::line1)];
          [Ifdef 
              ([[NotIfdefLine ((xline2, {col = 0})::line2)];
                [NotIfdefLine ((xline3, {col = 0})::line3)];
               ], info_ifdef_stmt2
              )
          ]
        ], info_ifdef_stmt 
        )
      ::NotIfdefLine ((TOBrace i, {col = 0})::line4)
      ::xs  
      -> 
        find_ifdef_funheaders xs;
        let all_toks = 
          xline2::xline3::(
            (List.map fst line2) @ 
            (List.map fst line3) @ 
            (List.map fst info_ifdef_stmt) @
            (List.map fst info_ifdef_stmt2)
          )
        in
        all_toks +> List.iter (fun x -> 
          Hashtbl.add !put_comment (pos_of_token x) true;
        )


  | Ifdef 
      ([[NotIfdefLine ((xline1, {col = 0})::line1)];
        [NotIfdefLine ((xline2, {col = 0})::line2)];
        [NotIfdefLine ((xline3, {col = 0})::line3)];
       ], info_ifdef_stmt 
      )
    ::NotIfdefLine ((TOBrace i, {col = 0})::line4)
    ::xs 
    -> 
      find_ifdef_funheaders xs;
      let all_toks = 
        xline2::xline3::(
          (List.map fst line2) @ 
          (List.map fst line3) @ 
          (List.map fst info_ifdef_stmt)
        )
      in
      all_toks +> List.iter (fun x -> 
        Hashtbl.add !put_comment (pos_of_token x) true;
      )
        

  | Ifdef (xxs,info_ifdef_stmt)::xs 
  | Ifdefzero (xxs,info_ifdef_stmt)::xs -> 
      List.iter find_ifdef_funheaders xxs; 
      find_ifdef_funheaders xs
        
  in
  find_ifdef_funheaders xs;

  !keep_ifdef, !put_comment
    


let adjust_tokens_based_on_mark_ifdef (keep_ifdef, put_comments) tokens = 
  tokens +> List.map (fun tok -> 
    let info = Token_helpers.info_from_token tok in
    let charpos = Ast_c.get_pos_of_info info in

    match tok with
    | TIfdef _ 
    | TIfdefelse _ 
    | TIfdefelif _
    | TEndif _ -> 
        if Hashtbl.mem keep_ifdef charpos 
        then tok
        else TCommentCpp info
    | x -> 
        if Hashtbl.mem put_comments charpos 
        then TCommentCpp info
        else x
  )


  

let fix_tokens_ifdef tokens = 

  (* une base plus pratique de travail *)
  let cleanxs = 
    tokens +> List.filter (fun x -> 
      not (Token_helpers.is_comment x) (* could filter also #define/#include *)
    ) 
  in
  let cleanxs_with_pos = List.map (fun x -> 
    let (line, col) = Token_helpers.linecol_of_tok x in
    x, { line = line; col = col}
  ) cleanxs 
  in
  (* une base vraiment plus pratique de travail *)
  let ifdef_grouped = mk_ifdef   cleanxs_with_pos in

  let keep_ifdef, put_comments    = find_and_tag_good_ifdef   ifdef_grouped in
  adjust_tokens_based_on_mark_ifdef   (keep_ifdef, put_comments) tokens
  

(* ------------------------------------------------------------------------- *)
(* macro *)
(* ------------------------------------------------------------------------- *)


(* loop, macro without ';', single macro *)
let debug_macros_list = 
  ["ASSERT"; "IRDA_ASSERT";
   "CHECK_NULL";
   "DEBUG";"DEBUG0";"DEBUG1";"DEBUG2";"DEBUG3";
   "DBG";"DEB";"PARSEDEBUG";"DEBC";"DBG_TRC";"DBG_ERR";"DBG_FTL";
   "DBGINFO"; "DFLOW";"DFLIP";"DLOG_INT_TRIG";
   "D3";"D1";"DB";"DCBDEBUG";"SCSI_LOG_MLQUEUE";
   "PLND"; "FCALND";"FCALD";
   "DEBUGRECURSION";
   "DEBUGPIO";"VDEB";
   "READ_UNLOCK_IRQRESTORE";
   "TRACE_CATCH";
   "PDBGG";

   "IF_ABR";"IF_EVENT";"IF_ERR";"IF_CBR";"IF_INIT";"IF_RX";
   "SOD";
  ]

let find_and_tag_good_macro cleanxs_with_pos = 
  let (put_comment : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in


  let rec find_macro = function
    | [] -> ()

    (* known debugging macro *)
    | (Line 
          ([NotParenToken (TIdent (s,ii),_);
            Parenthised (xxs,info_parens);
            NotParenToken (TPtVirg x, _);
          ] as line1
          ))
      ::xs when 
          List.mem s debug_macros_list
       -> 
        
        pr2_cpp ("MACRO: found debug-macro: " ^ s);
        iter_token_paren (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        ) line1;
        find_macro (xs)


    | (Line 
          ([NotParenToken (TIdent (s,ii),_);
            Parenthised (xxs,info_parens);
          ] as line1
          ))
      ::xs when 
          List.mem s debug_macros_list
      -> 
        pr2_cpp ("MACRO: found debug-macro: " ^ s);
        iter_token_paren (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        ) line1;
        find_macro (xs)

        
    (* macro fun header *)

    | (Line 
          ([NotParenToken ((TIdent (s,ii),_) as ident);
            Parenthised (xxs,info_parens);
          ]
          ))::
        xs 
          when List.mem s ["GDTH_INITFUNC";"ASC_INITFUNC"]
      -> 
        pr2_cpp "MACRO: XXX_INITFUNC detected";
        (ident::info_parens) +> List.iter (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        );
        find_macro xs



    (* macro with parameters *)
    | (Line 
          ([NotParenToken (TIdent (s,ii), {col = col1});
            Parenthised (xxs,info_parens);
          ] as line1
          ))
      ::(Line 
            (NotParenToken (tok, {col = col2 })::restline2
            ) as line2)
      ::xs 
      -> 

        let condition = 
          (col1 = col2 && 
          (match tok with
          | TOBrace _ -> false (* otherwise would match funcdecl *)
          | TPtVirg _ -> false
          | _ -> true
          )) || 
          (col2 <= col1 &&
           (match tok with
           | TCBrace _ -> true
           | _ -> false
           ))

        in

        if condition (* && s ==~ regexp_macro *)
        then begin
          pr2_cpp ("MACRO: found macro with param " ^ s);
          iter_token_paren (fun (tok, x) -> 
            Hashtbl.add !put_comment (pos_of_token tok) true
          ) line1;
        end;
        find_macro (line2::xs)

    (* single macro *)
    | (Line 
          ([NotParenToken (TIdent (s,ii), {col = col1});
          ] as line1
          ))
      ::(Line 
            (NotParenToken (tok, {col = col2 })::restline2
            ) as line2)
      ::xs -> 

        let condition = 
          (col1 = col2 && 
          col1 <> 0 && (* otherwise can match typedef of fundecl*)
          (match tok with
          | TPtVirg _ -> false 
          | TOr _ -> false 
          | _ -> true
          )) ||
          (col2 <= col1 &&
           (match tok with
           | TCBrace _ -> true
           | _ -> false
           ))

        in

        if condition
        then begin
          pr2_cpp ("MACRO: found single-macro " ^ s);
          iter_token_paren (fun (tok, x) -> 
            Hashtbl.add !put_comment (pos_of_token tok) true
          ) line1
        end;
        find_macro (line2::xs)


    | x::xs -> 
        find_macro xs

  in
(*
  let body_functions = mk_body_function_grouped cleanxs_with_pos in
  
  body_functions +> List.iter (function
  | NotBodyLine _ -> ()
  | BodyFunction cleanxs_with_pos -> 
      (* une base vraiment plus pratique de travail *)
*)
      let paren_grouped = mk_parenthised  cleanxs_with_pos in
      let line_paren_grouped = mk_line_parenthised paren_grouped in
      find_macro line_paren_grouped;
(*
  );
*)

  !put_comment



let adjust_tokens_based_on_mark_macro (put_comments) tokens = 
  tokens +> List.map (fun tok -> 
    let info = Token_helpers.info_from_token tok in
    let charpos = Ast_c.get_pos_of_info info in

    match tok with
    | x -> 
        if Hashtbl.mem put_comments charpos 
        then TCommentCpp info
        else x
  )


let fix_tokens_macro tokens = 

  (* une base plus pratique de travail *)
  let cleanxs = 
    tokens +> List.filter (fun x -> 
      not (Token_helpers.is_comment x) && 
      not (Token_helpers.is_cpp_instruction x)
    )
  in
  let cleanxs_with_pos = List.map (fun x -> 
    let (line, col) = Token_helpers.linecol_of_tok x in
    x, { line = line; col = col}
  ) cleanxs 
  in

  let put_comments    = find_and_tag_good_macro  cleanxs_with_pos in
  adjust_tokens_based_on_mark_macro   (put_comments) tokens


(* ------------------------------------------------------------------------- *)
(* main fix cpp function *)
(* ------------------------------------------------------------------------- *)
let fix_tokens_cpp tokens = 
  let tokens = fix_tokens_annotation_and_stringification_part1 tokens in

  
  (* todo? stringification detection *) 

  if !Flag_parsing_c.next_gen_parsing 
  then 
    let tokens = fix_tokens_ifdef tokens in
    let tokens = fix_tokens_macro tokens in
    tokens
  else 
    tokens


(*****************************************************************************)
(* Lexing with lookahead *)
(*****************************************************************************)
open Lexer_parser (* for the fields of lexer_hint type *)

let not_struct_enum = function
  | (Parser_c.Tstruct _ | Parser_c.Tunion _ | Parser_c.Tenum _)::_ -> false
  | _ -> true

let merge_info_str x xs = 
  let (info, annot) = x in
  { info with 
    Common.str = 
      (x::xs) +> List.map (fun (x,_annot) -> x.Common.str) +> Common.join ""
  }, annot
  



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
      then pr2 ("TYPEDEF: promoting: " ^ s)


let forLOOKAHEAD = 20

  
(* look if there is a '{' just after the closing ')', and handling the
 * possibility to have nested expressions inside nested parenthesis 
 *)
let rec is_really_foreach xs = 
  let rec is_foreach_aux = function
    | [] -> false, []
    | TCPar _::TOBrace _::xs -> true, xs
      (* the following attempts to handle the cases where there is a
	 single statement in the body of the loop.  undoubtedly more
	 cases are needed. 
         todo: premier(statement) - suivant(funcall)
      *)
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





(* LALR(k) trick. We can do stuff by adding cases in lexer_c.mll, but
 * it is more general to do it via my LALR(k) tech. Because here we can
 * transform some token give some context information. So sometimes it
 * makes sense to transform a token in one context, sometimes not, and
 * lex can not provide us this context information. Note that the order
 * in the pattern matching in lookahead is important. Do not cut/paste. 
 * 
 * Note that in next there is only "clean" tokens, there is no comment
 * or space tokens. This is done by the caller.
 * 
 * todo: but sometimes there is some __inline__ that prevent the transformation
 *  :( so perhaps could filter them from the next 10 tokens ?
 *)


let lookahead2 next before = 

  match (next, before) with

  (* special cases scsi/g_NCR5380 *)
  | (TIdent ("ANDP",i1)::TIdent (_,_)::_,   _) ->  TComma i1
  | (TIdent ("ANDP",i1)::TOPar _::_,   _)       ->  TComma i1


  (* static void mtdblock_request(RQFUNC_ARG);
   * but must put this before other rules otherwise the typedef
   * transformer will catch before 
   *)
  | TIdent (s, i1)::TCPar _::rest, TOPar _::_ 
      when !Lexer_parser._lexer_hint.parameterDeclaration -> 
      pr2_cpp ("MACRO: arg, transforming: " ^ s);
      TCommentCpp (i1)

  (*-------------------------------------------------------------*)
  (* stringification of ident *)
  (*-------------------------------------------------------------*)
  | (TIdent (s,i1)::_,       TOPar _::TIdent ("printk", _)::_) -> 
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
      then pr2 ("TYPEDEF: disable typedef cos special case: " ^ s); 

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
   (* x ( *y )(params),  function pointer *)
  | (TIdent (s, i1)::TOPar _::TMul _::TIdent _::TCPar _::TOPar _::_,  _) 
      when not_struct_enum before -> 
      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (*-------------------------------------------------------------*)
  (* CPP *)
  (*-------------------------------------------------------------*)
  | TDefine (s, body, i1, i2, i3)::_, _ when 
        not !Lexer_parser._lexer_hint.toplevel -> 
      
      pr2_cpp ("DEFINE: inside function, I treat it as comment");
      TCommentCpp (merge_info_str i1 [i2;i3])

  (* do same for include often found inside structdef *)
  | TInclude (s, i1, i2)::_, _ when not !Lexer_parser._lexer_hint.toplevel -> 
      pr2_cpp ("INCLUDE: inside function, I treat it as comment");
      TCommentCpp (merge_info_str i1 [i2])

  | ((TIfdef ii | TIfdefelse ii | TIfdefelif ii | TEndif ii) as x)::_, _ -> 
      if not !Flag_parsing_c.ifdef_to_if then TCommentCpp ii 
      else 
        if not !Lexer_parser._lexer_hint.toplevel
        then x
        else begin
          pr2_cpp("IFDEF: or related outside function. I treat it as comment");
          TCommentCpp ii
        end



  | (TIdent (s, i1)::TOPar _::rest, _) 
      when not !Lexer_parser._lexer_hint.toplevel -> 
      (* otherwise a function such as static void loopback_enable(int i) { 
       * will be considered as a loop 
       *)
 
      if s ==~ regexp_foreach && 
        is_really_foreach (Common.take_safe forLOOKAHEAD rest)
      then begin
        pr2_cpp ("MACRO: certainly foreach, transforming: " ^ s);
        Twhile i1
      end
      else TIdent (s, i1)


                    
 (*-------------------------------------------------------------*)
 | v::xs, _ -> v
 | _ -> raise Impossible

let lookahead a b = 
  Common.profile_code "C parsing.lookahead" (fun () -> lookahead2 a b)
