open Common open Commonop

open Parser_c 


let pr2 s = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.pr2 s

let pr2_cpp s = 
  if !Flag_parsing_c.debug_cpp
  then pr2 ("CPP-" ^ s)


(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)

(* Harcoded names of types or macros but they are not used by our heuristics!
 * They are just here to enable to detect false positive by printing only
 * the typedef/macros that we don't know yet. If print everything, then
 * can easily get lost with too much verbose tracing information.
 * So those functions "filter" some messages.
 *)

let msg_gen cond is_known printer s = 
  if cond
  then
    if not (!Flag_parsing_c.filter_msg)
    then printer s
    else
      if not (is_known s)
      then printer s
      

(* note: cant use partial application with let msg_typedef = 
 * because it would compute msg_typedef at compile time when 
 * the flag debug_typedef is always false
 *)
let msg_typedef s = 
  msg_gen (!Flag_parsing_c.debug_typedef)
    (fun s -> 
      (match s with
      | "u_char"   | "u_short"  | "u_int"  | "u_long"
      | "u8" | "u16" | "u32" | "u64" 
      | "s8"  | "s16" | "s32" | "s64" 
      | "__u8" | "__u16" | "__u32"  | "__u64"  
          -> true
          
      | "acpi_handle" 
      | "acpi_status" 
        -> true
          
      | s when s =~ ".*_t$" -> true
      | _ -> false 
      )
    )
    (fun s -> pr2 ("TYPEDEF: promoting: " ^ s))
    s

let msg_declare_macro s = 
  msg_gen (!Flag_parsing_c.debug_cpp)
    (fun s -> 
      (match s with 
      | "DECLARE_MUTEX" | "DECLARE_COMPLETION"  | "DECLARE_RWSEM"
      | "DECLARE_WAITQUEUE" | "DECLARE_WAIT_QUEUE_HEAD" 
      | "DEFINE_SPINLOCK" | "DEFINE_TIMER"
      | "DEVICE_ATTR" | "CLASS_DEVICE_ATTR" | "DRIVER_ATTR"
      | "SENSOR_DEVICE_ATTR"
      | "LIST_HEAD"
      | "DECLARE_WORK"  | "DECLARE_TASKLET"
      | "PORT_ATTR_RO" | "PORT_PMA_ATTR"
      | "DECLARE_BITMAP"

          -> true
(*
      | s when s =~ "^DECLARE_.*" -> true
      | s when s =~ ".*_ATTR$" -> true
      | s when s =~ "^DEFINE_.*" -> true
*)

      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found declare-macro: " ^ s))
    s
      

let msg_foreach s = 
  pr2_cpp ("MACRO: certainly foreach, transforming: " ^ s)


let msg_debug_macro s = 
  pr2_cpp ("MACRO: found debug-macro: " ^ s)


let msg_macro_noptvirg s = 
  pr2_cpp ("MACRO: found macro with param " ^ s)


let msg_macro_noptvirg_single s = 
  pr2_cpp ("MACRO: found single-macro " ^ s)


let msg_macro_higher_order s = 
  msg_gen (!Flag_parsing_c.debug_cpp)
    (fun s -> 
      (match s with 
      | "DBGINFO"
      | "DBGPX"
      | "DFLOW"
        -> true
      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found higher ordre macro : " ^ s))
    s



let msg_stringification s = 
  msg_gen (!Flag_parsing_c.debug_cpp)
    (fun s -> 
      (match s with 
      | "REVISION"
      | "UTS_RELEASE"
          -> true
      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found string-macro " ^ s))
    s

  

(*****************************************************************************)
(* CPP handling: macros, ifdefs  *)
(*****************************************************************************)

(* todo: how my fake_pos interact with the way I tag and adjust token ?
 * because I base my tagging on the position of the token ! so sometimes
 * could tag another fakeInfo that should not be tagged ? 
 *)

(* opti: better to built then once and for all, especially regexp_foreach *)

let regexp_macro =  Str.regexp
  "^[A-Z_][A-Z_0-9]*$"

let regexp_annot =  Str.regexp
  "^__.*$"

let regexp_declare =  Str.regexp
  ".*DECLARE.*"

let regexp_foreach = Str.regexp_case_fold 
  ".*\\(for_?each\\|for_?all\\|iterate\\|loop\\|walk\\|each\\|for\\)"


type linecol = { line : int; col : int }

type token_with_pos = Parser_c.token * linecol


let is_same_line line = function 
  | (x, {line = line2}) when line2 = line -> true 
  | _ -> false 


let str_of_token = Token_helpers.str_of_token
let pos_of_token = Token_helpers.pos_of_token

(* ------------------------------------------------------------------------- *)
(* fuzzy parsing, different views over the same program *)
(* ------------------------------------------------------------------------- *)

(* x list list, because x list separated by ',' *) 
type paren_grouped = 
  | Parenthised   of paren_grouped list list * token_with_pos list
  | NotParenToken of token_with_pos

(* Far better data structure than doing hacks in the lexer or parser
 * because in lexer we don't know to which ifdef a endif is related
 * and so when we want to comment a ifdef, we don't know which endif
 * we must also comment. Especially true for the #if 0 which sometimes
 * have a #else part.
 * 
 * x list list, because x list separated by #else or #elif 
 *) 
type ifdef_grouped = 
  | Ifdef     of ifdef_grouped list list * token_with_pos list
  | Ifdefbool of bool * ifdef_grouped list list * token_with_pos list
  | NotIfdefLine of token_with_pos list


type 'a line_grouped = 
  Line of 'a list



type body_function_grouped = 
  | BodyFunction of token_with_pos list
  | NotBodyLine  of token_with_pos list


(* ------------------------------------------------------------------------- *)
(* fuzzy builders *)
(* ------------------------------------------------------------------------- *)

(* todo: synchro ! can use indentation ? 
 * if paren not closed and same indentation level, certainly because
 * part of a mid-ifdef-expression.
*)
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
      (* maybe because of #ifdef which "opens" '(' in 2 branches *)
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
      | TIfdefbool (b,_), _ -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdefbool (b, body, extra)::mk_ifdef xs
          
      | tok, {line = line1} -> 
          (* todo? can have some Ifdef in the line ? *)
          let line, xs = Common.span (fun x -> is_same_line line1 x) (x::xs) in
          NotIfdefLine line::mk_ifdef xs 
      )

and mk_ifdef_parameters extras acc_before_sep xs = 
  match xs with
  | [] -> 
      (* Note that mk_ifdef is assuming that CPP instruction are alone
       * on their line. Because I do a span (fun x -> is_same_line ...)
       * I might take with me a #endif if this one is mixed on a line
       * with some "normal" tokens.
       *)
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

      | TIfdefbool (b,_), _ -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdefbool (b, body, extrasnest)::acc_before_sep) xs

      | TIfdefelse _, _ 
      | TIfdefelif _, _ -> 
          let body, extras, xs = mk_ifdef_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | tok, {line = line1} -> 
          let line, xs = Common.span (fun x -> is_same_line line1 x) (x::xs) in
          mk_ifdef_parameters extras (NotIfdefLine line::acc_before_sep) xs
      )

(* --------------------------------------- *)
let rec iter_token_paren f xs = 
  xs +> List.iter (function
  | NotParenToken tok -> f tok;
  | Parenthised (xxs, info_parens) -> 
      info_parens +> List.iter f;
      xxs +> List.iter (fun xs -> iter_token_paren f xs)
  )

let tokens_of_paren xs = 
  let g = ref [] in
  xs +> iter_token_paren (fun tok -> push2 tok g);
  !g

let rec iter_token_ifdef f xs = 
  xs +> List.iter (function
  | NotIfdefLine xs -> xs +> List.iter f;
  | Ifdefbool (_, xxs, info_ifdef) 
  | Ifdef (xxs, info_ifdef) -> 
      info_ifdef +> List.iter f;
      xxs +> List.iter (iter_token_ifdef f)
  )
      


(* --------------------------------------- *)

let line_of_paren = function
  | NotParenToken (tok, { line = line }) -> line
  | Parenthised (xxs, info_parens) -> 
      (match info_parens with
      | [] -> raise Impossible
      | (tok, {line = line})::xs -> line
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
  "____cacheline_aligned_in_smp", (fun ii -> TCommentCpp ii);
  "__devinit",                  (fun ii -> TCommentCpp ii); 
  "__devexit",                  (fun ii -> TCommentCpp ii); 
  "__devinitdata",              (fun ii -> TCommentCpp ii); 
  "__ALIGNED__",                (fun ii -> TCommentCpp ii); 

  "__pmac",                    (fun ii -> TCommentCpp ii);  
  "__force",                    (fun ii -> TCommentCpp ii);  
  "__nocast",                    (fun ii -> TCommentCpp ii);  

  "__cpuinit",                   (fun ii -> TCommentCpp ii);  
  "__cpuinitdata",                   (fun ii -> TCommentCpp ii);  
  "__must_check",                   (fun ii -> TCommentCpp ii);  
  "__unused",                   (fun ii -> TCommentCpp ii);  



  "__CS4231_INLINE__",                    (fun ii -> TCommentCpp ii);  

  "__init_or_module",                    (fun ii -> TCommentCpp ii);  



  "asmlinkage",                 (fun ii -> TCommentCpp ii);  

  "far",                    (fun ii -> TCommentCpp ii);  
  "near",                    (fun ii -> TCommentCpp ii);  

  "DIVA_EXIT_FUNCTION",                    (fun ii -> TCommentCpp ii);  
  "DIVA_INIT_FUNCTION",                    (fun ii -> TCommentCpp ii);  
  "ACPI_SYSTEM_XFACE",                    (fun ii -> TCommentCpp ii);  


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

(* the pair is the status of '()' and '{)', ex: (-1,0) 
 * if too much ')' and good '{}' 
 * could do for [] too ? 
 *)
let (count_open_close_ifdef_clause: ifdef_grouped list -> (int * int)) = 
 fun xs -> 
   let cnt_paren, cnt_brace = ref 0, ref 0 in
   xs +> iter_token_ifdef (fun (tok, pos) -> 
     (match tok with
     | TOPar _ -> incr cnt_paren
     | TOBrace _ -> incr cnt_brace
     | TCPar _ -> decr cnt_paren
     | TCBrace _ -> decr cnt_brace
     | _ -> ()
     )
   );
   !cnt_paren, !cnt_brace

let find_and_tag_good_ifdef xs = 
  let (keep_ifdef  : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in
  let (put_comment : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in

  let debug_ifdef_zero = ref false in

  (* ---------------------------------------------------------------------- *)
  let rec find_ifdef_zero xs = 
    xs +> List.iter (function 
    | NotIfdefLine _ -> ()
    | Ifdefbool (is_ifdef_positif, xxs, info_ifdef_stmt) -> 

        if is_ifdef_positif
        then pr2_cpp "commenting parts of a #if 1 or #if LINUX_VERSION"
        else pr2_cpp "commenting a #if 0 or #if LINUX_VERSION"
        ;

        (match xxs with
        | [] -> raise Impossible
        | firstclause::xxs -> 


            info_ifdef_stmt +> List.iter (fun (tok, x) ->
              if !debug_ifdef_zero 
              then pr2_cpp ("passing: " ^ str_of_token tok);
              Hashtbl.add !put_comment (pos_of_token tok) true
            );

            if is_ifdef_positif
            then begin
              xxs +> List.iter (fun xs -> 
                xs +> iter_token_ifdef (fun (tok, x) -> 
                  if !debug_ifdef_zero 
                  then pr2_cpp ("passing: " ^ str_of_token tok);
                  Hashtbl.add !put_comment (pos_of_token tok) true
                )
              );
            end
            else begin
            
              firstclause +> iter_token_ifdef (fun (tok, x) -> 
                if !debug_ifdef_zero 
                then pr2_cpp ("passing: " ^ str_of_token tok);
                Hashtbl.add !put_comment (pos_of_token tok) true
              ) ;
            
              (match List.rev xxs with
              | last::startxs -> 
                  (* keep only last *)

                  startxs +> List.iter (fun xs -> 
                    xs +> iter_token_ifdef (fun (tok, x) -> 
                      if !debug_ifdef_zero 
                      then pr2_cpp ("passing: " ^ str_of_token tok);
                      Hashtbl.add !put_comment (pos_of_token tok) true
                    )
                  );
           
              | [] -> (* not #else *) ()
              );
            end
        );

    | Ifdef (xxs, info_ifdef_stmt) -> 
        xxs +> List.iter find_ifdef_zero
    )
  in


  (* ---------------------------------------------------------------------- *)
  let rec find_ifdef_mid xs = 
    xs +> List.iter (function 
    | NotIfdefLine _ -> ()
    | Ifdef (xxs, info_ifdef_stmt) -> 
        (match xxs with 
        | [] -> raise Impossible
        | [first] -> ()
        | first::second::rest -> 
            if xxs +> List.for_all (fun xs -> List.length xs <= 3) && 
              (* don't want nested ifdef *)
               xxs +> List.for_all (fun xs -> 
                 xs +> List.for_all 
                   (function NotIfdefLine _ -> true | _ -> false)
               )
                   
            then 
              let counts = xxs +> List.map count_open_close_ifdef_clause in
              let cnt1,cnt2 = List.hd counts in 
              if cnt1 <> 0 || cnt2 <> 0 
                && counts +> List.for_all (fun x -> x = (cnt1, cnt2))
              (*
              if counts +> List.exists (fun (cnt1, cnt2) -> 
                cnt1 <> 0 || cnt2 <> 0 
                ) 
              *)
              then begin
                pr2_cpp "found ifdef-mid-something";
                info_ifdef_stmt +> List.iter (fun (tok, x) ->
                  Hashtbl.add !put_comment (pos_of_token tok) true
                );
                (second::rest) +> List.iter (fun xs -> 
                    xs +> iter_token_ifdef (fun (tok, x) -> 
                      Hashtbl.add !put_comment (pos_of_token tok) true
                    )
                );
              end
                
        );
        List.iter find_ifdef_mid xxs

    (* no need complex analysis for ifdefbool *)
    | Ifdefbool (_, xxs, info_ifdef_stmt) -> 
        List.iter find_ifdef_mid xxs

        
    )
  in
        

        


  (* ---------------------------------------------------------------------- *)
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
  | Ifdefbool (_, xxs,info_ifdef_stmt)::xs -> 
      List.iter find_ifdef_funheaders xxs; 
      find_ifdef_funheaders xs
        
  in
  (* ---------------------------------------------------------------------- *)


  find_ifdef_funheaders xs;
  find_ifdef_zero xs;
  find_ifdef_mid xs;

  !keep_ifdef, !put_comment
    




let adjust_tokens_based_on_mark_ifdef (keep_ifdef, put_comments) tokens = 
  tokens +> List.map (fun tok -> 
    let info = Token_helpers.info_from_token_fullstr tok in
    let charpos = Ast_c.get_pos_of_info info in

    match tok with
    (* don't touch this one ! *)
    | EOF x -> EOF x 

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
   "KDBG";
  ]



let find_and_tag_good_macro cleanxs_with_pos = 
  let (put_comment : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in
  let (keep_macro  : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in


  (* ---------------------------------------------------------------------- *)
  let rec find_macro xs = 
    match xs with
    | [] -> ()


    (* ex: static DEVICE_ATTR(); *)
    | (Line 
          ([NotParenToken (Tstatic _,_);
            NotParenToken (TIdent (s,_),_);
            Parenthised (xxs,info_parens);
            NotParenToken (TPtVirg _,_);
          ] as line1
          ))
        ::xs when s ==~ regexp_macro -> 
        msg_declare_macro s;
        iter_token_paren (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        ) line1;
        find_macro (xs)


    (* I do not put the final ';' because it can be on a multiline
     * and because of the way mk_line is coded, we will not have access
     * to this ';' on the next line, even if next to the ')'
     *)
    | (Line 
          ([NotParenToken (Tstatic _,_);
            NotParenToken (TIdent (s,_),_);
            Parenthised (xxs,info_parens);
          ] as line1
          ))
        ::xs when s ==~ regexp_macro -> 

        msg_declare_macro s;
        iter_token_paren (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        ) line1;
        find_macro (xs)



    (* ex: DECLARE_BITMAP(); 
     * Here I use regexp_declare and not regexp_macro because
     * Sometimes it can be a FunCallMacro such as DEBUG(foo());
     * Here we don't have the preceding 'static' so only way to
     * not have positive is to restrict to .*DECLARE.* macros.
    *)
    | (Line 
          ([NotParenToken (TIdent (s,_),_);
            Parenthised (xxs,info_parens);
            NotParenToken (TPtVirg _,_);
          ] as line1
          ))
        ::xs when s ==~ regexp_declare -> 

        msg_declare_macro s;
        iter_token_paren (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        ) line1;
        find_macro (xs)


    (* DEBUG(), because a known macro, can relax the condition
     * on the token we must have on the next line.
     *) 
    | (Line 
          ([NotParenToken (TIdent (s,ii) as macro,_);
            Parenthised (xxs,info_parens);
          ] as line1
          ))
      ::xs when 
          List.mem s debug_macros_list
      -> 
        msg_debug_macro s;
        Hashtbl.add !keep_macro (pos_of_token macro) true;

        iter_token_paren (fun (tok, x) -> 
          Hashtbl.add !put_comment (pos_of_token tok) true
        ) line1;
        find_macro (xs)

        
    (* special known macro around fun header *)
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



    (* macro with parameters 
     * ex: DEBUG()
     *     return x;
     *)
    | (Line 
          ([NotParenToken (TIdent (s,ii) as macro, {col = col1});
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
           (* TCBrace _ -> true  have false positif *)
           | Treturn _ -> true
           | _ -> false
           ))

        in

        if condition (* && s ==~ regexp_macro *)
        then begin
          msg_macro_noptvirg s;
          Hashtbl.add !keep_macro (pos_of_token macro) true;
          iter_token_paren (fun (tok, x) -> 
            Hashtbl.add !put_comment (pos_of_token tok) true
          ) line1;
        end;
        find_macro (line2::xs)

    (* single macro 
     * ex: LOCK
     *     foo();
     *     UNLOCK
     *)
    | (Line 
          ([NotParenToken (TIdent (s,ii) as macro, {col = col1});
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
           (* | TCBrace _ -> true  have some false positif *)
           | Treturn _ -> true
           | _ -> false
           ))

        in

        if condition
        then begin
          msg_macro_noptvirg_single s;
          Hashtbl.add !keep_macro (pos_of_token macro) true;

          iter_token_paren (fun (tok, x) -> 
            Hashtbl.add !put_comment (pos_of_token tok) true
          ) line1
        end;
        find_macro (line2::xs)





    | x::xs -> 
        find_macro xs

  in
  (* ---------------------------------------------------------------------- *)
  (*
    let body_functions = mk_body_function_grouped cleanxs_with_pos in
    body_functions +> List.iter (function
    | NotBodyLine _ -> ()
    | BodyFunction cleanxs_with_pos -> 
  *)

  (* une base vraiment plus pratique de travail *)
  let paren_grouped = mk_parenthised  cleanxs_with_pos in
  let line_paren_grouped = mk_line_parenthised paren_grouped in
  find_macro line_paren_grouped;

  !keep_macro, !put_comment



let adjust_tokens_based_on_mark_macro (keep_macro, put_comments) tokens = 
  tokens +> List.map (fun tok -> 
    let info = Token_helpers.info_from_token_fullstr tok in
    let charpos = Ast_c.get_pos_of_info info in

    match tok with
    | EOF x -> EOF x 

    | x -> 
        if Hashtbl.mem keep_macro charpos
        then TMacro info
        else 
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
(* action *)
(* ------------------------------------------------------------------------- *)

let is_statement_token = function
  | Tfor _ | Tdo _ | Tif _ | Twhile _ | Treturn _ 
  | Tbreak _ | Telse _ | Tswitch _ | Tcase _ | Tcontinue _
  | Tgoto _ 
  | TPtVirg _
      -> true
  | _ -> false


let find_and_tag_actions cleanxs_with_pos =
  let (put_actions : (int, bool) Hashtbl.t ref) = ref (Hashtbl.create 101) in

  (* ---------------------------------------------------------------------- *)
  let rec find_actions = function
    | [] -> ()
    | NotParenToken (TIdent (s,ii),_)
      ::Parenthised (xxs,info_parens)::xs -> 
        find_actions xs;
        xxs +> List.iter find_actions;
        let modified = find_actions_params xxs in
        if modified 
        then msg_macro_higher_order s

    | x::xs -> 
        find_actions xs

  and find_actions_params xxs = 
    xxs +> List.fold_left (fun acc xs -> 
      if List.exists (fun (tok,_)-> is_statement_token tok)(tokens_of_paren xs)
      then begin
        xs +> iter_token_paren (fun (tok, pos) -> 
          Hashtbl.add !put_actions (pos_of_token tok) true
        );
        true (* modified *)
      end
      else acc
    ) false
  in
  (* ---------------------------------------------------------------------- *)
  let paren_grouped = mk_parenthised  cleanxs_with_pos in
  find_actions paren_grouped;
  !put_actions
       


let adjust_tokens_based_on_mark_action (put_actions) tokens = 
  tokens +> List.map (fun tok -> 
    let info = Token_helpers.info_from_token_fullstr tok in
    let charpos = Ast_c.get_pos_of_info info in

    match tok with
    | EOF x -> EOF x 
    | x -> 
        if Hashtbl.mem put_actions charpos
        then TAction info
        else x
  )


let fix_tokens_action tokens =

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
  let put_actions = find_and_tag_actions cleanxs_with_pos in
  adjust_tokens_based_on_mark_action (put_actions) tokens



(* ------------------------------------------------------------------------- *)
(* main fix cpp function *)
(* ------------------------------------------------------------------------- *)
let fix_tokens_cpp tokens = 
  let tokens = fix_tokens_annotation_and_stringification_part1 tokens in

  (* todo: stringification detection *) 

  if !Flag_parsing_c.next_gen_parsing 
  then 
    let tokens = fix_tokens_action tokens in
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
      pr2_cpp ("MACRO: a la RQFUNC_ARG: " ^ s);
      TCommentCpp (i1)

  (*-------------------------------------------------------------*)
  (* stringification of ident *)
  (*-------------------------------------------------------------*)
  | (TIdent (s,i1)::_,       TOPar _::TIdent ("printk", _)::_) -> 
      msg_stringification s;
      TString ((s, Ast_c.IsChar), i1)

  | (TIdent (s,i1)::TString (_,_)::_,   _) ->  
      msg_stringification s;
      TString ((s, Ast_c.IsChar), i1)
  | (TIdent (s,i1)::_,   TString _::_) ->      
      msg_stringification s;
      TString ((s, Ast_c.IsChar), i1)


                  
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

  | (
      (TIfdef ii | TIfdefelse ii | TIfdefelif ii | TEndif ii | 
      TIfdefbool (_,ii)) as x)::_, _ -> 
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
        msg_foreach s;
        Twhile i1
      end
      else TIdent (s, i1)


                    
 (*-------------------------------------------------------------*)
 | v::xs, _ -> v
 | _ -> raise Impossible

let lookahead a b = 
  Common.profile_code "C parsing.lookahead" (fun () -> lookahead2 a b)
