open Common open Commonop

open Ast_c

(*****************************************************************************)
(* todo: take care of priority. For instance A => A+A, and if had B*A,
 * we dont want to generate B*A+A, we must insert some extra () (but
 * not always, only if necessary) src: rene

 * note: if add instruction, then try keep same indentation. So need introduce
 * some spacings. Done via the semi global _current_tabbing variable. *)
(*****************************************************************************)



(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_between_two_minus (infoa,(mcoda,enva)) (infob,(mcodb,envb)) =
  match mcoda, mcodb with
  | Ast_cocci.MINUS _, Ast_cocci.MINUS _ -> true
  | _ -> false

let pinfo_from_tok tok = fst (Parse_c.info_from_token tok)

(* When insert some new code, because of a + in a SP, we must add this
 * code at the right place, with the good indentation. So each time we
 * encounter some spacing info, with some newline, we maintain the
 * current indentation level used 
 * problems: not accurate. ex: TODO
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
 *)
let (passed_commentsbefore_notbefore2: 
 Common.parse_info -> Parser_c.token list -> Parser_c.token list Common.triple)
 = fun pinfo toks ->

  let (before, notbefore) = toks +> Common.span (fun tok -> 
    (pinfo_from_tok tok).charpos < pinfo.charpos)   
  in
  let (commentsbefore, passed) = 
    before +> List.rev +> 
      (* on the above example, at this stage we have 'com yy cpp token1 xx' *)
      Common.span (fun tok -> 
        (match tok with
        | Parser_c.TComment i -> true
        | Parser_c.TCommentSpace i -> true
        | Parser_c.TCommentCpp i -> true
        | Parser_c.TCommentAttrOrMacro i -> true
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

(* PPviatok was made in the beginning to allow to pretty print a
 * complete C file, including a modified C file by transformation.ml,
 * even if we don't handle yet in pretty_print_c.ml, ast_to_flow (and
 * maybe flow_to_ast) all the cases. Indeed we don't need to do some
 * fancy stuff when a function was not modified at all. Just need to
 * print the list of token as-is. But now pretty_print_c.ml handles
 * almost everything so maybe less useful. Maybe PPviatok allows to
 * optimize a little the pretty printing. *)

type ppmethod = PPviatok | PPnormal


(* The pp_program function will call pretty_print_c.ml with a special
 * function to print the leaf components, the tokens. When we want to 
 * print a token, we need to print also maybe the space and comments that
 * were close to it in the original file (and that was omitted during the 
 * parsing phase), but follows what the cocci-info attached to the token says.
 * Maybe we will not print the token if it's a MINUS-token, and maybe we will
 * print it and also print some cocci-code attached in a PLUS to it. 
 * So we will also maybe call unparse_cocci. Because the cocci-code may
 * contain metavariable, unparse_cocci will in fact sometimes call back
 * pretty_print_c (which will this time don't call back again unparse_cocci)
 *)
let pp_program2 xs outfile  = 
  
  Common.with_open_outfile outfile (fun (pr,chan) -> 
    let pr s = pr s (*; flush chan*) in

    let (toks: Parser_c.token list ref) = ref [] in
    let _last_synced_token = ref (Common.fake_parse_info, Ast_c.emptyAnnot) in

    let _current_tabbing = ref "" in
    let update_current_tabbing s = 
      match new_tabbing s with
      | Some x -> _current_tabbing := x
      | None -> ()
    in

    (* ---------------------- *)
    (* prints space and also adjusts !toks and _current_tabbing *)
    let sync (pinfo,annot) = 
      assert (pinfo <> Common.fake_parse_info);
      (* todo: if fake_parse_info ?  print the comments that are here ? *)

      let (passed, commentsbefore, notbefore) = 
        passed_commentsbefore_notbefore pinfo !toks 
      in
      passed +> List.iter (fun tok -> 
        match tok with
        | Parser_c.TComment (i,_) ->     pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
        | Parser_c.TCommentCpp (i,_) ->  pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
        | Parser_c.TCommentAttrOrMacro (i,_) -> 
            pr2 ("PP_PASSING_COMMENTS: " ^ i.str)
        | _ -> pr2 ("pp_passing_token: " ^ (pinfo_from_tok tok).str);
            
      );
      commentsbefore +> List.iter (fun tok -> 
        match tok with
        | Parser_c.TCommentSpace       (i,_) -> 
            update_current_tabbing i.str;
            if not (is_between_two_minus !_last_synced_token (pinfo,annot))
            then pr i.str;

        | Parser_c.TComment            (i,_) -> pr i.str
        | Parser_c.TCommentCpp         (i,_) -> pr i.str
        | Parser_c.TCommentAttrOrMacro (i,_) -> pr i.str
        | x -> error_cant_have x
      );
      
      toks := notbefore;
      let tok = pop2 toks in 

      (* pasforcement: assert_equal tokinfo.str pinfo.str; Indeed we may
       * have "reused" a token to keep its related comment and just
       * change its value (e.g. if decide to transform every 0 in 1, we
       * will reuse the info from 0) *)
      assert_equal (pinfo_from_tok tok).charpos pinfo.charpos;
      
    in

    (* ---------------------- *)
    let rec pr_elem ((pinfo,(mcode,env)) as e) = 

      (* 'e' can be an abstract_lined token. Indeed when we capture
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
       * todo: but that means we cant move huge chunk of code that contains
       * comments. During the move we will lose those space/comments/cpp.
       *)
      if not (Ast_c.is_al_info pinfo)
      then 
        begin
          if not ((fst e).charpos > (fst !_last_synced_token).charpos)
          then 
            failwith 
              (sprintf 
                  "pp_c: wrong order, you ask for %s but have already pass %s"
                  (Dumper.dump e) (Dumper.dump !_last_synced_token)); 

          sync e; 
          _last_synced_token := e;
        end;
      
      (*old: pr info.str *)
      let s = pinfo.str in

      (* UGLY trick *)
      let s = 
        if Ast_c.is_al_info pinfo 
        then
          (match s with
          | "char" | "short" | "int" | "double" | "float" | "long" 
          | "struct" | "union" | "signed" | "unsigned" 
          | "const" | "volatile"
              -> s ^ " "
          | x -> x
          )
        else s
      in

      match mcode with
      | Ast_cocci.MINUS (any_xxs) -> 
          Unparse_cocci.pp_list_list_any 
            (env,!_current_tabbing,pr, pr_elem) any_xxs 
      | Ast_cocci.CONTEXT (any_befaft) -> 
          (match any_befaft with
          | Ast_cocci.NOTHING -> pr s
              
          | Ast_cocci.BEFORE xxs -> 
              Unparse_cocci.pp_list_list_any 
                (env,!_current_tabbing,pr, pr_elem) xxs;
              pr s;
          | Ast_cocci.AFTER xxs -> 
              pr s;
              Unparse_cocci.pp_list_list_any 
                (env,!_current_tabbing,pr, pr_elem) xxs;
          | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
              Unparse_cocci.pp_list_list_any 
                (env,!_current_tabbing,pr, pr_elem) xxs;
              pr s;
              Unparse_cocci.pp_list_list_any 
                (env,!_current_tabbing,pr, pr_elem) yys;
              
          )
      | Ast_cocci.PLUS -> raise Impossible

    in
    (* ---------------------- *)
    (* start point *)
    (* ---------------------- *)

    xs +> List.iter (fun ((e,(_filename, _pos, _s, toks_e)), ppmethod) -> 
      toks := toks_e;
      match ppmethod with
      | PPviatok -> 
          (match e with
          | FinalDef (ii,_ANNOT) -> 
              (* less: assert that FinalDef is the last one in the list *)
              pr_elem ({ii with str = ""},Ast_c.emptyAnnot) 
          | e -> !toks +> List.map Parse_c.info_from_token +> List.iter pr_elem
          )
      | PPnormal -> Pretty_print_c.pp_program_gen pr_elem e
    );
  )

let pp_program a b = 
  Common.profile_code "C unparsing" (fun () -> pp_program2 a b)
