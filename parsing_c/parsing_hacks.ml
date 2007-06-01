open Common open Commonop

module TH = Token_helpers 
module LP = Lexer_parser

open Parser_c 

(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)

let pr2 s = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.pr2 s

let pr2_cpp s = 
  if !Flag_parsing_c.debug_cpp
  then pr2 ("CPP-" ^ s)


(* In the following, there is some harcoded names of types or macros
 * but they are not used by our heuristics! They are just here to
 * enable to detect false positive by printing only the typedef/macros
 * that we don't know yet. If we print everything, then we can easily get
 * lost with too much verbose tracing information. So those functions
 * "filter" some messages. 
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
    (fun s -> 
      pr2 ("TYPEDEF: promoting: " ^ s)
    )
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
  pr2_cpp ("MACRO: found foreach: " ^ s)


let msg_debug_macro s = 
  pr2_cpp ("MACRO: found debug-macro: " ^ s)


let msg_macro_noptvirg s = 
  pr2_cpp ("MACRO: found macro with param noptvirg: " ^ s)


let msg_macro_noptvirg_single s = 
  pr2_cpp ("MACRO: found single-macro noptvirg: " ^ s)


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
      | "SIZE_STR"
      | "DMA_STR"
          -> true
      (* s when s =~ ".*STR.*" -> true  *) 
      | _ -> false
      )
    )
    (fun s -> pr2_cpp ("MACRO: found string-macro " ^ s))
    s

  

(*****************************************************************************)
(* CPP handling: macros, ifdefs  *)
(*****************************************************************************)

(* opti: better to built then once and for all, especially regexp_foreach *)

let regexp_macro =  Str.regexp
  "^[A-Z_][A-Z_0-9]*$"

(* linuxext: *)
let regexp_annot =  Str.regexp
  "^__.*$"

(* linuxext: *)
let regexp_declare =  Str.regexp
  ".*DECLARE.*"

(* linuxext: *)
let regexp_foreach = Str.regexp_case_fold 
  ".*\\(for_?each\\|for_?all\\|iterate\\|loop\\|walk\\|scan\\|each\\|for\\)"



(* Normally I should not use ref/mutable in the token_extended type
 * and I should have a set of functions taking a list of tokens and
 * returning a list of tokens. The problem is that to make easier some
 * functions, it is better to work on better representation, on "views"
 * over this list of tokens. But then modifying those views and get
 * back from those views to the original simple list of tokens is
 * tedious. One way is to maintain next to the view a list of "actions"
 * (I was using a hash storing the charpos of the token and associating
 * the action) but it is tedious too. Simpler to use mutable/ref. We
 * use the same idea that we use when working on the Ast_c. *)

(* old: when I using the list of "actions" next to the views, the hash
 * indexed by the charpos, there could have been some problems:
 * how my fake_pos interact with the way I tag and adjust token ?
 * because I base my tagging on the position of the token ! so sometimes
 * could tag another fakeInfo that should not be tagged ? 
 * fortunately I don't use anymore this technique.
 *)


type token_extended = { 
  mutable tok: Parser_c.token;
  (* todo: before/after ? *)

  (* line x col  cache, more easily accessible, of the info in the token *)
  line: int; 
  col : int;

  (* todo: where: InToplevel | InFunction | InEnum | InStruct | InInit *)
}

let set_as_comment x = 
  if TH.is_eof x.tok 
  then () (* otherwise parse_c will be lost if don't find a EOF token *)
  else 
    x.tok <- TCommentCpp (TH.info_from_token x.tok)
  


(* ------------------------------------------------------------------------- *)
(* fuzzy parsing, different "views" over the same program *)
(* ------------------------------------------------------------------------- *)

(* x list list, because x list separated by ',' *) 
type paren_grouped = 
  | Parenthised   of paren_grouped list list * token_extended list
  | PToken of token_extended

(* Far better data structure than doing hacks in the lexer or parser
 * because in lexer we don't know to which ifdef a endif is related
 * and so when we want to comment a ifdef, we don't know which endif
 * we must also comment. Especially true for the #if 0 which sometimes
 * have a #else part.
 * 
 * x list list, because x list separated by #else or #elif 
 *) 
type ifdef_grouped = 
  | Ifdef     of ifdef_grouped list list * token_extended list
  | Ifdefbool of bool * ifdef_grouped list list * token_extended list
  | NotIfdefLine of token_extended list


type 'a line_grouped = 
  Line of 'a list


type body_function_grouped = 
  | BodyFunction of token_extended list
  | NotBodyLine  of token_extended list


(* ------------------------------------------------------------------------- *)
(* view builders  *)
(* ------------------------------------------------------------------------- *)

(* todo: synchro ! can use indentation ? 
 * if paren not closed and same indentation level, certainly because
 * part of a mid-ifdef-expression.
*)
let rec mk_parenthised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x.tok with 
      | TOPar _ -> 
          let body, extras, xs = mk_parameters [x] [] xs in
          Parenthised (body,extras)::mk_parenthised xs
      | _ -> 
          PToken x::mk_parenthised xs
      )

(* return the body of the parenthised expression and the rest of the tokens *)
and mk_parameters extras acc_before_sep  xs = 
  match xs with
  | [] -> 
      (* maybe because of #ifdef which "opens" '(' in 2 branches *)
      pr2 "PB: not found closing paren in fuzzy parsing";
      [List.rev acc_before_sep], List.rev extras, []
  | x::xs -> 
      (match x.tok with 
      (* synchro *)
      | TOBrace _ when x.col = 0 -> 
          pr2 "PB: found synchro point } in paren";
          [List.rev acc_before_sep], List.rev (extras), (x::xs)

      | TCPar _ -> 
          [List.rev acc_before_sep], List.rev (x::extras), xs
      | TOPar _ -> 
          let body, extrasnest, xs = mk_parameters [x] [] xs in
          mk_parameters extras 
            (Parenthised (body,extrasnest)::acc_before_sep) 
            xs
      | TComma _ -> 
          let body, extras, xs = mk_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | _ -> 
          mk_parameters extras (PToken x::acc_before_sep) xs
      )
          

(* --------------------------------------- *)
let rec mk_ifdef xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x.tok with 
      | TIfdef _ -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdef (body, extra)::mk_ifdef xs
      | TIfdefbool (b,_) -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdefbool (b, body, extra)::mk_ifdef xs
          
      | _ -> 
          (* todo? can have some Ifdef in the line ? *)
          let line, xs = Common.span (fun y -> y.line = x.line) (x::xs) in
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
      (match x.tok with 
      | TEndif _ -> 
          [List.rev acc_before_sep], List.rev (x::extras), xs
      | TIfdef _ -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdef (body, extrasnest)::acc_before_sep) xs

      | TIfdefbool (b,_) -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdefbool (b, body, extrasnest)::acc_before_sep) xs

      | TIfdefelse _ 
      | TIfdefelif _ -> 
          let body, extras, xs = mk_ifdef_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | _ -> 
          let line, xs = Common.span (fun y -> y.line = x.line) (x::xs) in
          mk_ifdef_parameters extras (NotIfdefLine line::acc_before_sep) xs
      )

(* --------------------------------------- *)

let line_of_paren = function
  | PToken x -> x.line
  | Parenthised (xxs, info_parens) -> 
      (match info_parens with
      | [] -> raise Impossible
      | x::xs -> x.line
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
      | {tok = TOBrace _; col = 0} -> 
          let is_closing_brace = function 
            | {tok = TCBrace _; col = 0 } -> true 
            | _ -> false 
          in
          let body, xs = Common.span (fun x -> not (is_closing_brace x)) xs in
          (match xs with
          | ({tok = TCBrace _; col = 0 })::xs -> 
              BodyFunction body::mk_body_function_grouped xs
          | [] -> 
              pr2 "PB:not found closing brace in fuzzy parsing";
              [NotBodyLine body]
          | _ -> raise Impossible
          )
          
      | _ -> 
          let line, xs = Common.span (fun y -> y.line = x.line) (x::xs) in
          NotBodyLine line::mk_body_function_grouped xs 
      )


(* ------------------------------------------------------------------------- *)
(* view iterators  *)
(* ------------------------------------------------------------------------- *)

let rec iter_token_paren f xs = 
  xs +> List.iter (function
  | PToken tok -> f tok;
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
      

(* ------------------------------------------------------------------------- *)
(* annotation, static, if,  and stringification part 1 *)
(* ------------------------------------------------------------------------- *)

(* linuxext: *)
let keyword_table = Common.hash_of_list [
  (* attributes. could perhaps generalize via "__.*" *)
  "__init",                       (fun ii -> TCommentCpp ii); 
  "__exit",                       (fun ii -> TCommentCpp ii); 
  "__user",                       (fun ii -> TCommentCpp ii); 
  "__iomem",                      (fun ii -> TCommentCpp ii); 
  "__initdata",                   (fun ii -> TCommentCpp ii); 
  "__exitdata",                   (fun ii -> TCommentCpp ii); 
  "__devinit",                    (fun ii -> TCommentCpp ii); 
  "__devexit",                    (fun ii -> TCommentCpp ii); 
  "__devinitdata",                (fun ii -> TCommentCpp ii); 
  "__cpuinit",                    (fun ii -> TCommentCpp ii);  
  "__cpuinitdata",                (fun ii -> TCommentCpp ii);  
  "__init_or_module",             (fun ii -> TCommentCpp ii);  
  "__initdata_or_module",         (fun ii -> TCommentCpp ii);  
  "__pminit",     (fun ii -> TCommentCpp ii);  
  "__pminitdata", (fun ii -> TCommentCpp ii);  

  "__cacheline_aligned",          (fun ii -> TCommentCpp ii); 
  "____cacheline_aligned",        (fun ii -> TCommentCpp ii); 
  "__cacheline_aligned_in_smp",   (fun ii -> TCommentCpp ii);
  "____cacheline_aligned_in_smp", (fun ii -> TCommentCpp ii);
  "____cacheline_internodealigned_in_smp", (fun ii -> TCommentCpp ii);

  "__ALIGNED__",                  (fun ii -> TCommentCpp ii); 
  "__3xp_aligned",                (fun ii -> TCommentCpp ii);  

  "__pmac",                       (fun ii -> TCommentCpp ii);  
  "__force",                      (fun ii -> TCommentCpp ii);  
  "__nocast",                     (fun ii -> TCommentCpp ii);  
  "__read_mostly",                (fun ii -> TCommentCpp ii);  


  "__must_check",   (fun ii -> TCommentCpp ii);  
  "__unused",       (fun ii -> TCommentCpp ii); (* pbs *)
  "__maybe_unused", (fun ii -> TCommentCpp ii);  


  "__attribute_used__",           (fun ii -> TCommentCpp ii);  
  "__attribute_pure__",           (fun ii -> TCommentCpp ii);  
  "__attribute_const__",          (fun ii -> TCommentCpp ii);  

  "__always_inline",              (fun ii -> TCommentCpp ii);  
  "noinline",                     (fun ii -> TCommentCpp ii);  

  "__CS4231_INLINE__",            (fun ii -> TCommentCpp ii);  
  "CCIO_INLINE",                  (fun ii -> TCommentCpp ii);  
  "SBA_INLINE",                   (fun ii -> TCommentCpp ii);  

  "STATIC_INLINE",                (fun ii -> TCommentCpp ii);  

  "AGPEXTERN",                (fun ii -> TCommentCpp ii);  

  "INITSECTION",                  (fun ii -> TCommentCpp ii);  

  "NORET_TYPE",                   (fun ii -> TCommentCpp ii);  

  "__xipram",                (fun ii -> TCommentCpp ii);  
  "compat_init_data",        (fun ii -> TCommentCpp ii);  

  "fastcall",                (fun ii -> TCommentCpp ii);  
  "asmlinkage",              (fun ii -> TCommentCpp ii);  

  "far",                     (fun ii -> TCommentCpp ii);  
  "SK_FAR",                  (fun ii -> TCommentCpp ii);  

(*  "near",                    (fun ii -> TCommentCpp ii);   *)

  "DIVA_EXIT_FUNCTION",      (fun ii -> TCommentCpp ii);  
  "DIVA_INIT_FUNCTION",      (fun ii -> TCommentCpp ii);  
  "ACPI_SYSTEM_XFACE",       (fun ii -> TCommentCpp ii);  

  "ASC_INITDATA",            (fun ii -> TCommentCpp ii);  
  "in2000__INITDATA",        (fun ii -> TCommentCpp ii);  
  "PACKED",                  (fun ii -> TCommentCpp ii);  

  "WPMINFO",                 (fun ii -> TCommentCpp ii);
  "CPMINFO",                 (fun ii -> TCommentCpp ii);
  "PMINFO",                  (fun ii -> TCommentCpp ii);


  "ACPI_INTERNAL_VAR_XFACE", (fun ii -> TCommentCpp ii);

  "PNMI_STATIC",   (fun ii -> Tstatic ii); 
  "RLMT_STATIC",   (fun ii -> Tstatic ii); 
  "SISINITSTATIC", (fun ii -> Tstatic ii); 
  "SCTP_STATIC",   (fun ii -> Tstatic ii);  


  "SISIOMEMTYPE",  (fun ii -> TCommentCpp ii); 

  (* IF *)
  "BUGLVL", (fun ii -> Tif ii);
  "IFDEBUG", (fun ii -> Tif ii);



  "ACPI_STATE_COMMON" ,            (fun ii -> TCommentCpp ii); 
  "ACPI_PARSE_COMMON"  ,           (fun ii -> TCommentCpp ii); 
  "ACPI_COMMON_DEBUG_MEM_HEADER" , (fun ii -> TCommentCpp ii); 

  "TRACE_EXIT", (fun ii -> Treturn ii); 


   (* in the other part of the kernel, in arch/, mm/, etc *)
  "__sched",                      (fun ii -> TCommentCpp ii);  
  "__initmv",                     (fun ii -> TCommentCpp ii);  
  "__exception",                  (fun ii -> TCommentCpp ii);  
  "__cpuexit",                    (fun ii -> TCommentCpp ii);  
  "__kprobes",                    (fun ii -> TCommentCpp ii);  
  "__meminit",                    (fun ii -> TCommentCpp ii);  
  "__meminitdata",                (fun ii -> TCommentCpp ii);  
  "__nosavedata",                 (fun ii -> TCommentCpp ii);  
  "__kernel",                     (fun ii -> TCommentCpp ii);  
  "__nomods_init",                (fun ii -> TCommentCpp ii);  
  "__apicdebuginit",              (fun ii -> TCommentCpp ii);  
  "__ipc_init",                   (fun ii -> TCommentCpp ii);  
  "__modinit",                    (fun ii -> TCommentCpp ii);  
  "__lockfunc",                   (fun ii -> TCommentCpp ii);  
  "__weak",                       (fun ii -> TCommentCpp ii);  
  "__tlb_handler_align",          (fun ii -> TCommentCpp ii);  
  "__lock_aligned",               (fun ii -> TCommentCpp ii);  
  "__force_data",                 (fun ii -> TCommentCpp ii);  
  "__nongprelbss",                (fun ii -> TCommentCpp ii);  
  "__nongpreldata",               (fun ii -> TCommentCpp ii);  
  "nabi_no_regargs",              (fun ii -> TCommentCpp ii);  

  "__section_jiffies",            (fun ii -> TCommentCpp ii);  
  "__vsyscall_fn",                (fun ii -> TCommentCpp ii);  
  "__section_vgetcpu_mode",       (fun ii -> TCommentCpp ii);  
  "__section_vsyscall_gtod_data", (fun ii -> TCommentCpp ii);  

  "ATTRIB_NORET",     (fun ii -> TCommentCpp ii);  
  "ATTRIBUTE_UNUSED", (fun ii -> TCommentCpp ii);  
  "BTEXT",            (fun ii -> TCommentCpp ii);  
  "BTDATA",           (fun ii -> TCommentCpp ii);  
  "PAGE_ALIGNED",     (fun ii -> TCommentCpp ii);  

  "EARLY_INIT_SECTION_ATTR", (fun ii -> TCommentCpp ii);  

(*  "INIT", (fun ii -> TCommentCpp ii);   *) (* pbs *)

  "IDI_CALL_ENTITY_T", (fun ii -> TCommentCpp ii);  


  (* maybe only in old kernel *)
  "__openfirmware", (fun ii -> TCommentCpp ii);  

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
  "KERN_NOTICE",   (fun ii -> TString(("KERN_NOTICE",Ast_c.IsChar),ii));
  "KERN_EMERG",   (fun ii -> TString(("KERN_EMERG",Ast_c.IsChar),ii));
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
  tokens +> List.iter (fun x -> 
    match x.tok with
    | TIdent (s, ii) -> 
        (match Common.optionise (fun () -> Hashtbl.find keyword_table s) with
        | Some f -> x.tok <- f ii
        | None -> ()
        )
    | x -> ()
  )




(* ------------------------------------------------------------------------- *)
(* ifdef keeping/passing *)
(* ------------------------------------------------------------------------- *)

(* #if 0, #if 1,  #if LINUX_VERSION handling *)
let rec find_ifdef_zero xs = 
  xs +> List.iter (function 
  | NotIfdefLine _ -> ()
  | Ifdefbool (is_ifdef_positif, xxs, info_ifdef_stmt) -> 

      if is_ifdef_positif
      then pr2_cpp "commenting parts of a #if 1 or #if LINUX_VERSION"
      else pr2_cpp "commenting a #if 0 or #if LINUX_VERSION or __cplusplus";

      (match xxs with
      | [] -> raise Impossible
      | firstclause::xxs -> 
          info_ifdef_stmt +> List.iter set_as_comment;

          if is_ifdef_positif
          then xxs +> List.iter (iter_token_ifdef set_as_comment)
          else begin
            firstclause +> iter_token_ifdef set_as_comment;
            (match List.rev xxs with
            (* keep only last *)
            | last::startxs -> 
                startxs +> List.iter (iter_token_ifdef set_as_comment)
            | [] -> (* not #else *) ()
            );
          end
      );
      
  | Ifdef (xxs, info_ifdef_stmt) -> xxs +> List.iter find_ifdef_zero
  )



(* the pair is the status of '()' and '{}', ex: (-1,0) 
 * if too much ')' and good '{}' 
 * could do for [] too ? 
 * could do for ','   if encounter ',' at "toplevel", not inside () or {}
 * then if have ifdef, then certainly can lead to a problem.
 *)
let (count_open_close_stuff_ifdef_clause: ifdef_grouped list -> (int * int)) = 
 fun xs -> 
   let cnt_paren, cnt_brace = ref 0, ref 0 in
   xs +> iter_token_ifdef (fun x -> 
     (match x.tok with
     | TOPar _ -> incr cnt_paren
     | TOBrace _ -> incr cnt_brace
     | TCPar _ -> decr cnt_paren
     | TCBrace _ -> decr cnt_brace
     | _ -> ()
     )
   );
   !cnt_paren, !cnt_brace

let thresholdIfdefSizeMid = 6

(* infer ifdef involving not-closed expressions/statements *)
let rec find_ifdef_mid xs = 
  xs +> List.iter (function 
  | NotIfdefLine _ -> ()
  | Ifdef (xxs, info_ifdef_stmt) -> 
      (match xxs with 
      | [] -> raise Impossible
      | [first] -> ()
      | first::second::rest -> 
          (* don't analyse big ifdef *)
          if xxs +> List.for_all 
            (fun xs -> List.length xs <= thresholdIfdefSizeMid) && 
            (* don't want nested ifdef *)
            xxs +> List.for_all (fun xs -> 
              xs +> List.for_all 
                (function NotIfdefLine _ -> true | _ -> false)
            )
            
          then 
            let counts = xxs +> List.map count_open_close_stuff_ifdef_clause in
            let cnt1, cnt2 = List.hd counts in 
            if cnt1 <> 0 || cnt2 <> 0 && 
               counts +> List.for_all (fun x -> x = (cnt1, cnt2))
              (*
                if counts +> List.exists (fun (cnt1, cnt2) -> 
                cnt1 <> 0 || cnt2 <> 0 
                ) 
              *)
            then begin
              pr2_cpp "found ifdef-mid-something";
              (* keep only first, treat the rest as comment *)
              info_ifdef_stmt +> List.iter set_as_comment;
              (second::rest) +> List.iter (iter_token_ifdef set_as_comment);
            end
              
      );
      List.iter find_ifdef_mid xxs
        
  (* no need complex analysis for ifdefbool *)
  | Ifdefbool (_, xxs, info_ifdef_stmt) -> 
      List.iter find_ifdef_mid xxs
          
        
  )

(* ifdef defining alternate function header, type *)
let rec find_ifdef_funheaders = function
  | [] -> ()
  | NotIfdefLine _::xs -> find_ifdef_funheaders xs 

  (* ifdef-funheader if ifdef with 2 lines and a '{' in next line *)
  | Ifdef 
      ([[NotIfdefLine (({col = 0} as _xline1)::line1)];
        [NotIfdefLine (({col = 0} as xline2)::line2)]
      ], info_ifdef_stmt 
      )
    ::NotIfdefLine (({tok = TOBrace i; col = 0})::line3)
    ::xs  
    -> 
      find_ifdef_funheaders xs;
      let all_toks = [xline2] @ line2 @ info_ifdef_stmt in
      all_toks +> List.iter set_as_comment;

  (* ifdef with nested ifdef *)
  | Ifdef 
      ([[NotIfdefLine (({col = 0} as _xline1)::line1)];
        [Ifdef 
            ([[NotIfdefLine (({col = 0} as xline2)::line2)];
              [NotIfdefLine (({col = 0} as xline3)::line3)];
            ], info_ifdef_stmt2
            )
        ]
      ], info_ifdef_stmt 
      )
    ::NotIfdefLine (({tok = TOBrace i; col = 0})::line4)
    ::xs  
    -> 
      find_ifdef_funheaders xs;
      let all_toks = 
        [xline2;xline3] @ line2 @ line3 @ info_ifdef_stmt @ info_ifdef_stmt2
      in
      all_toks +> List.iter set_as_comment;

 (* ifdef with elseif *)
  | Ifdef 
      ([[NotIfdefLine (({col = 0} as _xline1)::line1)];
        [NotIfdefLine (({col = 0} as xline2)::line2)];
        [NotIfdefLine (({col = 0} as xline3)::line3)];
      ], info_ifdef_stmt 
      )
    ::NotIfdefLine (({tok = TOBrace i; col = 0})::line4)
    ::xs 
    -> 
      find_ifdef_funheaders xs;
      let all_toks = [xline2;xline3] @ line2 @ line3 @ info_ifdef_stmt in
      all_toks +> List.iter set_as_comment
        

  | Ifdef (xxs,info_ifdef_stmt)::xs 
  | Ifdefbool (_, xxs,info_ifdef_stmt)::xs -> 
      List.iter find_ifdef_funheaders xxs; 
      find_ifdef_funheaders xs
        



(* ------------------------------------------------------------------------- *)
(* macro *)
(* ------------------------------------------------------------------------- *)

let passed_macro_parentized_list = 
  ["__acquires";"__releases";
   "__declspec"; 
   "__page_aligned";
   "__vsyscall"
  ]

(* don't forget to recurse in each case *)
let rec find_macro_paren xs = 
  match xs with
  | [] -> ()
      
  (* attribute *)
  | PToken ({tok = Tattribute _} as id)
    ::Parenthised (xxs,info_parens)
    ::xs
     -> 
      pr2_cpp ("MACRO: __attribute detected ");
      [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;
      set_as_comment id;
      find_macro_paren xs


  (* __release, __acquire, etc *)
  | PToken ({tok = TIdent (s,ii)} as id)
    ::Parenthised (xxs,info_parens)
    ::xs
    when List.mem s passed_macro_parentized_list
     -> 
      pr2_cpp ("MACRO: __acquires/__releases like detected ");
      [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;
      set_as_comment id;
      find_macro_paren xs

   (* linuxext: special case on FASTCALL, etc *)
  | PToken ({tok = TIdent (s,_)} as id)
    ::Parenthised (xxs,info_parens)
    ::xs
    when List.mem s ["FASTCALL"] -> 
      
      pr2_cpp ("MACRO: FASTCALL detected: " ^ s);
      (* pass only the macro *)
      xxs +> List.iter find_macro_paren;
      (id::info_parens) +> List.iter set_as_comment;
      find_macro_paren xs

  (* stringification
   * 
   * the order of the matching clause is important
   *)

  (* EX_TABLE & co. 
   *
   * Replaced by a string. We can't put everything as comment
   * because it can be part of an expression where we wait for
   * something, where we wait for a string
   *
   * normally not needed if have good stringification of macro 
   * but those macros are sometimes used multiple times 
   * as in EX_TABLE(0b) EX_TABLE(1b)  and we don't detect
   * it well yet.
   *)
  | PToken ({tok = TIdent (s,ii)} as id)
    ::Parenthised (xxs,info_parens)
    ::xs
    when List.mem s 
      ["EX_TABLE";"ASM_EXCEPTIONTABLE_ENTRY";"DCACHE_CLEAR";"PPC405_ERR77"] -> 
      pr2_cpp ("MACRO: EX_TABLE like detected");
      id.tok <- TString((s,Ast_c.IsChar),ii);
      [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;

      find_macro_paren xs




  (* string macro with params, before case *)
  | PToken ({tok = TString _})::PToken ({tok = TIdent (s,_)} as id)
    ::Parenthised (xxs, info_parens)
    ::xs -> 
      pr2_cpp ("MACRO: string macro with params : " ^ s);
      id.tok <- TMacroString (TH.info_from_token id.tok);
      [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;
      find_macro_paren xs

  (* after case *)
  | PToken ({tok = TIdent (s,_)} as id)
    ::Parenthised (xxs, info_parens)
    ::PToken ({tok = TString _})
    ::xs -> 
      pr2_cpp ("MACRO: string macro with params : " ^ s);
      id.tok <- TMacroString (TH.info_from_token id.tok);
      [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;
      find_macro_paren xs

  (* string macro variable, before case *)
  | PToken ({tok = TString _})::PToken ({tok = TIdent (s,_)} as id)
      ::xs -> 
      msg_stringification s;
      id.tok <- TMacroString (TH.info_from_token id.tok);
      find_macro_paren xs

  (* after case *)
  | PToken ({tok = TIdent (s,_)} as id)::PToken ({tok = TString _})
      ::xs -> 
      msg_stringification s;
      id.tok <- TMacroString (TH.info_from_token id.tok);
      find_macro_paren xs

  (* recurse *)
  | (PToken x)::xs -> find_macro_paren xs 
  | (Parenthised (xxs, info_parens))::xs -> 
      xxs +> List.iter find_macro_paren;
      find_macro_paren xs





(* loop, macro without ';', single macro *)
(* linuxext: *)
let debug_macros_list = 
  ["ASSERT"; "IRDA_ASSERT"; "ASSERTCMP"; "ASSERTRANGE"; "ASSERTIFCMP";
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
   "IRDA_ASSERT_LABEL";
  ]

let declList =
  ["decl_subsys";
  ]



(* don't forget to recurse in each case *)
let rec find_macro xs = 
  match xs with
  | [] -> ()

  (* linuxext: ex: static [const] DEVICE_ATTR(); *)
  | (Line 
        (
          (PToken ({tok = Tstatic _})::
           PToken ({tok = TIdent (s,_)} as macro)::
           Parenthised (xxs,info_parens)::
           PToken ({tok = TPtVirg _})::
           _
          ) 
        ))
    ::xs 
    when (s ==~ regexp_macro) || List.mem s declList -> 
      msg_declare_macro s;
      let info = TH.info_from_token macro.tok in
      macro.tok <- TMacroDecl (Ast_c.get_str_of_info info, info);

      find_macro (xs)

  (* the static const case *)
  | (Line 
        (
          (PToken ({tok = Tstatic _})::
           PToken ({tok = Tconst _} as const)::
           PToken ({tok = TIdent (s,_)} as macro)::
           Parenthised (xxs,info_parens)::
           PToken ({tok = TPtVirg _})::
            _
          ) (* it could also be the same with a TEof en plus a la fin *)
            (*as line1*)

        ))
    ::xs 
    when (s ==~ regexp_macro) || List.mem s declList -> 
      msg_declare_macro s;
      let info = TH.info_from_token macro.tok in
      macro.tok <- TMacroDecl (Ast_c.get_str_of_info info, info);
      
      (* need retag this const, otherwise ambiguity in grammar 
         21: shift/reduce conflict (shift 121, reduce 137) on Tconst
  	 decl2 : Tstatic . TMacroDecl TOPar argument_list TCPar ...
	 decl2 : Tstatic . Tconst TMacroDecl TOPar argument_list TCPar ...
	 storage_class_spec : Tstatic .  (137)
      *)
      const.tok <- TMacroDeclConst (TH.info_from_token const.tok);

      find_macro (xs)


  (* same but without trailing ';'
   * 
   * I do not put the final ';' because it can be on a multiline and
   * because of the way mk_line is coded, we will not have access to
   * this ';' on the next line, even if next to the ')' *)
  | (Line 
        ([PToken ({tok = Tstatic _});
          PToken ({tok = TIdent (s,_)} as macro);
          Parenthised (xxs,info_parens);
        ] 
        ))
    ::xs 
    when s ==~ regexp_macro -> 

      msg_declare_macro s;
      let info = TH.info_from_token macro.tok in
      macro.tok <- TMacroDecl (Ast_c.get_str_of_info info, info);

      find_macro (xs)



  (* linuxext: ex: DECLARE_BITMAP(); 
   * 
   * Here I use regexp_declare and not regexp_macro because
   * Sometimes it can be a FunCallMacro such as DEBUG(foo());
   * Here we don't have the preceding 'static' so only way to
   * not have positive is to restrict to .*DECLARE.* macros.
   *
   * but there is a grammar rule for that, so don't need this case anymore
   * unless the parameter of the DECLARE_xxx are wierd and can not be mapped
   * on a argument_list
   *)
        
  | (Line 
        ([PToken ({tok = TIdent (s,_)} as macro);
          Parenthised (xxs,info_parens);
          PToken ({tok = TPtVirg _});
        ]
        ))
    ::xs 
    when (s ==~ regexp_declare) || List.mem s declList -> 

      msg_declare_macro s;
      let info = TH.info_from_token macro.tok in
      macro.tok <- TMacroDecl (Ast_c.get_str_of_info info, info);

      find_macro (xs)



  (* linuxext: ex: DEBUG()
   * 
   * because a known macro, can relax the condition on the token we
   * must have on the next line. 
   *)
  | (Line 
        ([PToken ({tok = TIdent (s,ii)} as macro);
          Parenthised (xxs,info_parens);
        ]
        ))
    ::xs 
    when List.mem s debug_macros_list -> 
      msg_debug_macro s;
      macro.tok <- TMacroStmt (TH.info_from_token macro.tok);

      [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;

      find_macro (xs)
            
            
  (* linuxext: special known macro around fun header *)
  | (Line 
        ([PToken ({tok = TIdent (s,ii)} as ident);
          Parenthised (xxs,info_parens);
        ]
        ))
    ::xs 
    when List.mem s ["GDTH_INITFUNC";"ASC_INITFUNC"]-> 
      pr2_cpp ("MACRO: XXX_INITFUNC detected: " ^ s);
      (* keep xxs only *)
      (ident::info_parens) +> List.iter set_as_comment;

      find_macro xs



  (* macro with parameters 
   * ex: DEBUG()
   *     return x;
   *)
  | (Line 
        ([PToken ({tok = TIdent (s,ii); col = col1} as macro);
          Parenthised (xxs,info_parens);
        ] as _line1
        ))
    ::(Line 
          (PToken ({col = col2 } as other)::restline2
          ) as line2)
    ::xs 
    (* when s ==~ regexp_macro *)
    -> 
      let condition = 
        (col1 = col2 && 
            (match other.tok with
            | TOBrace _ -> false (* otherwise would match funcdecl *)
            | TPtVirg _ -> false
            | _ -> true
            )
        ) 
        || 
        (col2 <= col1 &&
              (match other.tok with
              (* TCBrace _ -> true  have false positif *)
              | Treturn _ -> true
              | _ -> false
              )
          )

      in
      
      if condition
      then begin
        msg_macro_noptvirg s;
        macro.tok <- TMacroStmt (TH.info_from_token macro.tok);
        [Parenthised (xxs, info_parens)] +> iter_token_paren set_as_comment;
      end;

      find_macro (line2::xs)
        
  (* linuxext:? single macro 
   * ex: LOCK
   *     foo();
   *     UNLOCK
   *)
  | (Line 
        ([PToken ({tok = TIdent (s,ii); col = col1} as macro);
        ] as _line1
        ))
    ::(Line 
          (PToken ({col = col2 } as other)::restline2
          ) as line2)
    ::xs -> 
    (* when s ==~ regexp_macro *)
      
      let condition = 
        (col1 = col2 && 
            col1 <> 0 && (* otherwise can match typedef of fundecl*)
            (match other.tok with
            | TPtVirg _ -> false 
            | TOr _ -> false 
            | _ -> true
            )) ||
          (col2 <= col1 &&
              (match other.tok with
              (* | TCBrace _ -> true  have some false positif *)
              | Treturn _ -> true
              | _ -> false
              ))
      in
      
      if condition
      then begin
        msg_macro_noptvirg_single s;
        macro.tok <- TMacroStmt (TH.info_from_token macro.tok);
      end;
      find_macro (line2::xs)
        
  | x::xs -> 
      find_macro xs


(* ------------------------------------------------------------------------- *)
(* action *)
(* ------------------------------------------------------------------------- *)

let rec find_actions = function
  | [] -> ()
  | PToken ({tok = TIdent (s,ii)})
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
    if (tokens_of_paren xs) +> List.exists (fun x -> 
      TH.is_statement_token x.tok
    )
    then begin
      xs +> iter_token_paren (fun x -> 
        if TH.is_eof x.tok
        then 
          (* certainly because paren detection had a pb because of
           * some ifdef-exp
           *)
          pr2_cpp "PB: wierd, I try to tag an EOF token as action"
        else 
          x.tok <- TAction (TH.info_from_token x.tok);
      );
      true (* modified *)
    end
    else acc
  ) false



(* ------------------------------------------------------------------------- *)
(* main fix cpp function *)
(* ------------------------------------------------------------------------- *)
let fix_tokens_cpp tokens = 

  let tokens2 = tokens +> List.map (fun x -> 
    let (line, col) = TH.linecol_of_tok x in
    {tok = x; line = line; col = col}
  )
  in
  begin 
    fix_tokens_annotation_and_stringification_part1 tokens2;

    if !Flag_parsing_c.next_gen_parsing 
    then begin
      (* the order is important, if put action first 
       * then because of ifdef, can have not closed paren
       * and so may believe that higher order macro 
       * and it will eat too much tokens. 
       * 
       * I recompute multiple times cleaner cos the mutable
       * can have be changed and so may have more comments
       * 
       *)

      (* ifdef *)
      let cleaner = tokens2 +> List.filter (fun x -> 
        not (TH.is_comment x.tok) (* could filter also #define/#include *)
      ) in
      let ifdef_grouped = mk_ifdef cleaner in
      find_ifdef_funheaders ifdef_grouped;
      find_ifdef_zero ifdef_grouped;
      find_ifdef_mid ifdef_grouped;

      (* macro *)
      let cleaner = tokens2 +> List.filter (fun x -> 
        not (TH.is_comment x.tok) && not (TH.is_cpp_instruction x.tok)
      ) in
      let paren_grouped = mk_parenthised  cleaner in
      let line_paren_grouped = mk_line_parenthised paren_grouped in
      find_macro line_paren_grouped;
      find_macro_paren paren_grouped;

      (* actions *)
      let cleaner = tokens2 +> List.filter (fun x -> 
        not (TH.is_comment x.tok) && not (TH.is_cpp_instruction x.tok)
      ) 
      in
      let paren_grouped = mk_parenthised  cleaner in
      find_actions paren_grouped;
    end;
    tokens2 +> List.map (fun x -> x.tok)
  end




(*****************************************************************************)
(* Lexing with lookahead *)
(*****************************************************************************)

open Lexer_parser (* for the fields of lexer_hint type *)

let not_struct_enum = function
  | (Parser_c.Tstruct _ | Parser_c.Tunion _ | Parser_c.Tenum _)::_ -> false
  | _ -> true


let not_annot s = 
  not (s ==~ regexp_annot)


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
 *)


let lookahead2 next before = 

  match (next, before) with

  (*-------------------------------------------------------------*)
  (* typedef inference *)
  (*-------------------------------------------------------------*)
  (* xx xx *)
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

      LP.disable_typedef();

      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx yy *)
  | (TIdent (s, i1)::TIdent (s2, i2)::_  , _) when not_struct_enum before ->
         (* && not_annot s2 BUT lead to false positive*)

      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx inline *)
  | (TIdent (s, i1)::Tinline i2::_  , _) when not_struct_enum before -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* [,(] xx [,)] AND param decl *)
  | (TIdent (s, i1)::(TComma _|TCPar _)::_ , (TComma _ |TOPar _)::_ )
    when not_struct_enum before && !LP._lexer_hint.parameterDeclaration
    -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx* [,)] *)
  (* specialcase:  [,(] xx* [,)] *)
  | (TIdent (s, i1)::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
    -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx** [,)] *)
  (* specialcase:  [,(] xx** [,)] *)
  | (TIdent (s, i1)::TMul _::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
    -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)



  (* xx const *   USELESS because of next rule ? *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _)::TMul _::_ , _ ) 
      when not_struct_enum before ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)

      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)
  
  (* xx const *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _)::_ , _ ) 
      when not_struct_enum before ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)

      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx * const *)
  | (TIdent (s, i1)::TMul _::(Tconst _ | Tvolatile _)::_ , _ ) 
      when not_struct_enum before ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)

      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* ( const xx)  *)
  | (TIdent (s, i1)::TCPar _::_,  (Tconst _ | Tvolatile _)::TOPar _::_) -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)
      


  (* ( xx ) [sizeof, ~] *)
  | (TIdent (s, i1)::TCPar _::(Tsizeof _|TTilde _)::_ , TOPar _::_ )
    when not_struct_enum before
    -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* [(,] xx [   AND parameterdeclaration *)
  | (TIdent (s, i1)::TOCro _::_, (TComma _ |TOPar _)::_)
      when !LP._lexer_hint.parameterDeclaration
     -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (*------------------------------------------------------------*)
  (* if 'x*y' maybe an expr, maybe just a classic multiplication *)
  (* but if have a '=', or ','   I think not *)
  (*------------------------------------------------------------*)

  (* static xx * yy  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::_ , 
     (Tregister _|Tstatic _  |Tvolatile _|Tconst _)::_) -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (*  TODO  xx * yy ; AND in start of compound element  *)


  (*  xx * yy,      AND  in paramdecl *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before && !LP._lexer_hint.parameterDeclaration -> 

      msg_typedef s; Lexer_parser.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy ;     AND in Toplevel  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , _)
    when not_struct_enum before && !LP._lexer_hint.toplevel  -> 

      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx * yy (     AND in Toplevel  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOPar _::_ , _)
    when not_struct_enum before  && !LP._lexer_hint.toplevel -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (* xx * yy [ *)
  (* todo? enough ? cos in struct def we can have some expression ! *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOCro _::_ , _)
    when not_struct_enum before && 
      (!LP._lexer_hint.structDefinition > 0 || !LP._lexer_hint.toplevel)
      -> 
      msg_typedef s;  LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* u16: 10; in struct *)
  | (TIdent (s, i1)::TDotDot _::_ , (TOBrace _ | TPtVirg _)::_)
    when (!LP._lexer_hint.structDefinition > 0 || !LP._lexer_hint.toplevel)
      -> 
      msg_typedef s;  LP.add_typedef_root s;
      TypedefIdent (s, i1)
        

    (*  why need TOPar condition as stated in preceding rule ? really needed ? *)
    (*   YES cos at toplevel can have some expression !! for instance when *)
    (*   enter in the dimension of an array *)
    (*
      | (TIdent s::TMul::TIdent s2::_ , _)
      when (take_safe 1 !passed_tok <> [Tstruct] &&
      (take_safe 1 !passed_tok <> [Tenum]))
      &&
      !LP._lexer_hint = Some LP.Toplevel -> 
      msg_typedef s; 
      LP.add_typedef_root s;
      TypedefIdent s
     *)

  (*  xx * yy =  *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TEq _::_ , _)
    when not_struct_enum before -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy)      AND in paramdecl *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TCPar _::_ , _)
      when not_struct_enum before && !LP._lexer_hint.parameterDeclaration -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)
          

  (*  xx * yy; *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , 
     (TOBrace _| TPtVirg _)::_)  when not_struct_enum before ->
      msg_typedef s;  LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy,  and ';' before xx *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , 
     (TOBrace _| TPtVirg _)::_) ->
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx_t * yy *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::_ , _)  
      when s =~ ".*_t$" && not_struct_enum before 
        (* struct user_info_t sometimes *) 
        -> 
      msg_typedef s;  LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx ** yy *)  (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before -> 
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx *** yy *)
  | (TIdent (s, i1)::TMul _::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before -> 
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx ** ) *)
  | (TIdent (s, i1)::TMul _::TMul _::TCPar _::_ , _)
    when not_struct_enum before -> 
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)



  (* ----------------------------------- *)

  (*  (xx) yy *)
  | (TOPar info::TIdent (s, i1)::TCPar _::(TIdent _|TInt _)::_ , x::_)  
    when (match x with Tif _ -> false | Twhile _ -> false | _ -> true) -> 
      msg_typedef s; LP.add_typedef_root s;
      TOPar info


  (*  (xx) (    yy) *)
  | (TOPar info::TIdent (s, i1)::TCPar _::TOPar _::_ , x::_)  
    when (match x with Tif _ -> false | Twhile _ -> false | _ -> true) -> 
      msg_typedef s; LP.add_typedef_root s;
      TOPar info

  (*  (xx * ) yy *)
  | (TOPar info::TIdent (s, i1)::TMul _::TCPar _::TIdent (s2, i2)::_ , _) -> 
      msg_typedef s; LP.add_typedef_root s;
      TOPar info

  (* (xx){ ... }  constructor *)
  | (TIdent (s, i1)::TCPar _::TOBrace _::_ , TOPar _::_)  when s =~ ".*_t$"->
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


        (* can have sizeof on expression
           | (Tsizeof::TOPar::TIdent s::TCPar::_,   _) -> 
           msg_typedef s; 
           LP.add_typedef_root s;
           Tsizeof
         *)
   (* x ( *y )(params),  function pointer *)
  | (TIdent (s, i1)::TOPar _::TMul _::TIdent _::TCPar _::TOPar _::_,  _) 
      when not_struct_enum before -> 
      msg_typedef s; LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*-------------------------------------------------------------*)
  (* CPP *)
  (*-------------------------------------------------------------*)
  | ((TIfdef ii |TIfdefelse ii |TIfdefelif ii |TEndif ii |TIfdefbool (_,ii))
        as x)
    ::_, _ -> 
      if not !Flag_parsing_c.ifdef_to_if then TCommentCpp ii 
      else 
        if not !LP._lexer_hint.toplevel
        then x
        else begin
          pr2_cpp("IFDEF: or related outside function. I treat it as comment");
          TCommentCpp ii
        end


   (* If ident contain a for_each, then certainly a macro. But to be
    * sure should look if there is a '{' after the ')', but it requires
    * to count the '('. Because this can be expensive, we do that only
    * when the token contains "for_each". 
    *)
  | (TIdent (s, i1)::TOPar _::rest, _) when not !LP._lexer_hint.toplevel -> 
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


