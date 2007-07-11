open Common open Commonop

open Ast_c

module TH = Token_helpers

(*****************************************************************************)
(* todo: take care of priority. For instance A => A+A, and if had B*A,
 * we dont want to generate B*A+A, we must insert some extra () (but
 * not always, only if necessary) src: rene
 * 
 * note: if add instruction, then try keep same indentation. So need introduce
 * some spacings. Done via the semi global _current_tabbing variable. *)
(*****************************************************************************)


(*****************************************************************************)
(* Last fix *)
(*****************************************************************************)

(* Because of the ugly trick to handle initialiser, I generate fake ',' 
 * for the last initializer element, but if there is nothing around it,
 * I don't want in the end to print it.
 *)

let mcode_contain_plus = function
  | Ast_cocci.CONTEXT (_,Ast_cocci.NOTHING) -> false
  | Ast_cocci.CONTEXT _ -> true
  | Ast_cocci.MINUS (_,[]) -> false
  | Ast_cocci.MINUS (_,x::xs) -> true
  | Ast_cocci.PLUS -> raise Impossible

let contain_plus info = 
  let mck = Ast_c.mcode_of_info info in
  mcode_contain_plus mck

let lastfix program = 
  let bigf = { 
    Visitor_c.default_visitor_c_s with
    Visitor_c.kini_s = (fun (k,bigf) ini -> 
      match k ini with
      | InitList args, ii -> 
          (match ii with
          | [_i1;_i2] -> ini
          | [i1;i2;iicommaopt] -> 
              if Ast_c.mark_of_info iicommaopt = FakeTok && 
                 (not (contain_plus iicommaopt)) &&
                 (not (contain_plus i2))
              then InitList args, [i1;i2]
              else InitList args, [i1;i2;iicommaopt]
          | _ -> raise Impossible
          )
      | x -> x
    )
  } in
  Visitor_c.vk_program_s bigf program


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (is_between_two_minus : Ast_c.info -> Ast_c.info -> bool) = 
 fun infoa infob ->
   match Ast_c.mcode_of_info infoa, Ast_c.mcode_of_info infob with
   | Ast_cocci.MINUS _, Ast_cocci.MINUS _ -> true
   | _ -> false

let first_fake_token () = Ast_c.fakeInfo()


let final_str_of_info ii = 
  let s = Ast_c.str_of_info ii in
  match Ast_c.mark_of_info ii, !Flag_engine.debug_unparsing with
  | OriginTok, _ -> s

  | FakeTok         , true -> "!F!" ^ s ^ "!"
  | ExpandedTok     , true -> "!E!" ^ s ^ "!"
  | AbstractLineTok , true -> "!A!" ^ s ^ "!"

  | FakeTok, false -> s
  | ExpandedTok, false -> ""
  | AbstractLineTok, false -> 
      (* UGLY trick *)
      (match s with
      | "char" | "short" | "int" | "double" | "float" | "long" | "void"
      | "auto" | "register" | "extern" | "static"
      | "signed" | "unsigned" 
      | "struct" | "union" | "enum" | "typedef" 
      | "const" | "volatile"
      | "else" | "case" | "do"
      | "goto" | "return" 
          -> s ^ " "
      | x -> x
      )



(* When insert some new code, because of a + in a SP, we must add this
 * code at the right place, with the good indentation. So each time we
 * encounter some spacing info, with some newline, we maintain the
 * current indentation level used.
 * 
 * TODO problems: not accurate. ex: TODO
 * 
 * TODO: if in #define region, should add a \ \n
 *)
let new_tabbing2 space = 
  let xs = list_of_string space in
  if (xs +> List.exists (fun c -> c = '\n'))
  then
    Some (
      xs
      +> List.rev
      +> Common.take_until (fun c -> c = '\n')
      +> List.rev
      +> List.map string_of_char
      +> String.concat ""
    )
  else None
let new_tabbing a = 
  Common.profile_code "C unparsing.new_tabbing" (fun () -> new_tabbing2 a)



(* When we will print a token, we must print the space, comments, and
 * sometimes CPP instruction, associated with this token. So if have
 * 'xx token1 cpp yy com token2 zz' in the token list, where xx, yy, zz
 * are some space, cpp a CPP instruction and com a C commentary, and
 * that we want to print or what I call "sync" on token2, then before
 * printing token2, we must also print cpp yy and com.
 * 
 * We will also say that we "passed" on some tokens, here 'xx' and
 * 'token1'. But, we normally cant delete a token. A token is marked as
 * minus, but not deleted, so normally passed should always be empty.
 * 
 * update: now I can have some ExpandedToken tokens, so must first
 * abstract away from those tokens.
 * 
 *)
let (passed_commentsbefore_notbefore2: 
 Ast_c.info -> Parser_c.token list -> Parser_c.token list Common.triple)
 = fun info toks ->

  let (before, notbefore) = toks +> Common.span (fun tok -> 
    assert (TH.mark_of_tok tok = OriginTok);
    TH.pos_of_tok tok < pos_of_info info
  )
  in

  let (commentsbefore, passed) = 
    before +> List.rev +> 
      (* on the above example, at this stage we have 'com yy cpp token1 xx' *)
      Common.span (fun tok -> 
        (match tok with
        | Parser_c.TComment i -> true
        | Parser_c.TCommentSpace i -> true
        | Parser_c.TCommentCpp i -> true
        | Parser_c.TCommentMisc i -> true
        | _ -> false
        ))
  in
  let (commentsbefore, passed) = (List.rev commentsbefore, List.rev passed) 
  in
  (passed, commentsbefore, notbefore)

let passed_commentsbefore_notbefore a b = 
  Common.profile_code "C unparsing.passed_comments" 
    (fun () -> passed_commentsbefore_notbefore2 a b)


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* old: PPviatok was made in the beginning to allow to pretty print a
 * complete C file, including a modified C file by transformation.ml,
 * even if we don't handle yet in pretty_print_c.ml, ast_to_flow (and
 * maybe flow_to_ast) all the cases. Indeed we don't need to do some
 * fancy stuff when a function was not modified at all. Just need to
 * print the list of token as-is. But now pretty_print_c.ml handles
 * almost everything so maybe less useful. Maybe PPviatok allows to
 * optimize a little the pretty printing. 
 * 
 * update: now have PPviastr which goes even faster than PPviatok, so
 * PPviatok has disappeared.
 *)

type ppmethod = PPnormal | PPviastr


(* The pp_program function will call pretty_print_c.ml with a special
 * function to print the leaf components, the tokens. When we want to 
 * print a token, we need to print also maybe the space and comments that
 * were close to it in the original file (and that was omitted during the 
 * parsing phase), but honor what the cocci-info attached to the token says.
 * Maybe we will not print the token if it's a MINUS-token, and maybe we will
 * print it and also print some cocci-code attached in a PLUS to it. 
 * So we will also maybe call unparse_cocci. Because the cocci-code may
 * contain metavariable, unparse_cocci will in fact sometimes call back
 * pretty_print_c (which will this time don't call back again unparse_cocci)
 *)
let pp_program2 xs outfile  = 
  
  Common.with_open_outfile outfile (fun (pr,chan) -> 
    let pr s = 
      if !Flag_engine.debug_unparsing 
      then begin pr2_no_nl s; flush stderr end;
      pr s ; 
      (* flush chan; *)
      (* Common.pr2 ("UNPARSING: >" ^ s ^ "<"); *)
    in

    let _toks = ref [] in
    let _last_synced_token = ref (first_fake_token ()) in
    let _current_tabbing = ref "" in

    let update_current_tabbing s = 
      match new_tabbing s with
      | Some x -> _current_tabbing := x
      | None -> ()
    in

    (* ---------------------- *)
    (* prints space and also adjusts !toks and _current_tabbing *)
    let sync info = 
      assert (Ast_c.mark_of_info info = OriginTok);
      (* todo: if FakeTok ?  print the comments that are here ? *)

      let (passed, commentsbefore, notbefore) = 
        passed_commentsbefore_notbefore info !_toks 
      in
      passed +> List.iter (fun tok -> 
        let str = TH.str_of_tok tok in
        match tok with
        | Parser_c.TComment _ ->     pr2 ("PP_PASSING_COMMENTS: " ^ str)
        | Parser_c.TCommentCpp _ ->  pr2 ("PP_PASSING_COMMENTS: " ^ str)
        | Parser_c.TCommentMisc _ -> pr2 ("PP_PASSING_COMMENTS: " ^ str)
        | _ -> pr2 ("pp_passing_token: " ^ str);
            
      );
      let is_pure_real_comment = List.for_all TH.is_real_comment commentsbefore
      in

      commentsbefore +> List.iter (fun tok -> 
        let str = TH.str_of_tok tok in
        match tok with
        | Parser_c.TCommentSpace       _ -> 
            update_current_tabbing str;
            if not (is_between_two_minus !_last_synced_token info)
              || not (is_pure_real_comment)
            then pr str;

        | Parser_c.TComment _ |Parser_c.TCommentCpp _ |Parser_c.TCommentMisc _ 
            -> pr str
        | x -> error_cant_have x
      );
      
      _toks := notbefore;
      let tok = pop2 _toks in 

      (* pasforcement: assert_equal tokinfo.str pinfo.str;
       * 
       * Indeed we may have "reused" a token to keep its related
       * comment and just change its value (e.g. if decide to transform
       * every 0 in 1, we will reuse the info from 0) 
       *)
      assert_equal (TH.pos_of_tok tok)  (pos_of_info info);
      
    in

    (* ---------------------- *)
    (* 'ii' can be an abstract_lined token. Indeed when we capture
     * some code in a metavariable, this code may then further be
     * written back if this metavariable is put in a + line. When we
     * add some code in the environment, for the moment we
     * abstract-lined it. This allows to use '=' and makes it easy to
     * handle some SP such as f(X, X). So when we will sometimes call
     * unparse_cocci (cf below), unparse_cocci will maybe call us back
     * with some code containing abstract-lined tokens. The
     * abstract-lined code in the environment makes easier too our
     * sync technique that goes from left to right in the token list.
     * We always have the good order for all the token, except for the
     * tokens that are well identified because they are abstract-lined.
     * 
     * old: I also use those abstract-lined token for creating fake places in
     * the ast where I can add mcode, as when want add stuff at the end
     * of an if to EndStatement, or when want add something at the 
     * beginning of a function, before the storage.
     * update: now i use the mark, FakeTok
     * 
     * TODO: but that means we cant move huge chunk of code that contains
     * comments. During the move we will lose those space/comments/cpp.
     * 
     * note: normally the FakeTok only appear in ast_c and so are not
     * present in the list of tokens so no need to synchronize on them.
     * Same for AbstractLineTok that come from metavariables and so
     * were present only in '+' zone so no need to synchronize on them.
     * 
     *)
    let rec pr_elem ii = 


      if Ast_c.mark_of_info ii = OriginTok
      then 
        begin
          if not (pos_of_info ii > pos_of_info !_last_synced_token)
          then failwith "wrong order of token in unparse_c";
          sync ii; 
          _last_synced_token := ii;
        end;

      if Ast_c.mark_of_info ii = ExpandedTok
      then 
        (let x =  pop2 _toks in 
        assert(TH.info_of_tok x = ii));
      
      
      (*old: pr info.str *)
      let s = final_str_of_info ii in


      let (mcode,env) = !(ii.cocci_tag) in
      let args_pp = (env, !_current_tabbing, pr, pr_elem) in

      match mcode with
      | Ast_cocci.MINUS (_,any_xxs) -> 
          Unparse_cocci.pp_list_list_any args_pp any_xxs 
      | Ast_cocci.CONTEXT (_,any_befaft) -> 
          (match any_befaft with
          | Ast_cocci.NOTHING -> 
              pr s
          | Ast_cocci.BEFORE xxs -> 
              Unparse_cocci.pp_list_list_any args_pp xxs;
              pr s;
          | Ast_cocci.AFTER xxs -> 
              pr s;
              Unparse_cocci.pp_list_list_any args_pp xxs;
          | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
              Unparse_cocci.pp_list_list_any args_pp xxs;
              pr s;
              Unparse_cocci.pp_list_list_any args_pp yys;
          )
      | Ast_cocci.PLUS -> raise Impossible

    in
    (* ---------------------- *)
    (* start point *)
    (* ---------------------- *)

    xs +> List.iter (fun ((e,(str, toks_e)), ppmethod) -> 

      let toks_e = toks_e +> Common.exclude (fun tok -> 
        TH.is_comment tok && (TH.mark_of_tok tok = ExpandedTok)
      ) in
        
      toks_e +> List.iter (fun tok -> 
        assert(List.mem (TH.mark_of_tok tok) [OriginTok; ExpandedTok]);
      );


      _toks := toks_e;
      _last_synced_token := first_fake_token ();

      let e = lastfix e in

      match ppmethod with
      | PPnormal -> 
          Pretty_print_c.pp_program_gen pr_elem e;
          (* assert(null !_toks); *)
      | PPviastr -> pr str
    );
  )


let pp_program a b = 
  Common.profile_code "C unparsing" (fun () -> pp_program2 a b)
