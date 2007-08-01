open Common open Commonop 


(*****************************************************************************)
(* Test framework *)
(*****************************************************************************)

(* There can have multiple .c for the same cocci file. The convention
 * is to have one base.cocci and a base.c and some optional
 * base_vernn.[c,res].
 * 
 * Note that I use standard.iso when, iso_file parameter is empty.
 * If want to test without iso, use -iso_file empty.iso option.
 *)
let testone x compare_with_expected_flag = 
  let x    = if x =~ "\\(.*\\)_ver0$" then matched1 x else x in
  let base = if x =~ "\\(.*\\)_ver[0-9]+$" then matched1 x else x in

  let cfile      = "tests/" ^ x ^ ".c" in 
  let cocci_file = "tests/" ^ base ^ ".cocci" in

  let expected_res   = "tests/" ^ x ^ ".res" in
  begin
    let res = Cocci.full_engine (cocci_file, !Config.std_iso) [cfile] in
    match res with
    | [s, Some outfile] when s = cfile -> 
        if compare_with_expected_flag
        then 
          Compare_c.compare_default outfile expected_res 
          +> Compare_c.compare_result_to_string 
          +> pr2;
        let tmpfile = "/tmp/"^Common.basename cfile in
        pr2 (sprintf "One file modified. Result is here: %s" tmpfile);
        Common.command2 ("cp "^outfile^" "^tmpfile);
    | _ -> pr2 "no modification on the input file or wrong file"
  end
          

(* ------------------------------------------------------------------------ *)
let best_score_file = "/tmp/score_cocci_best.marshalled"
let timeout_testall = 30

let testall () =

  let newscore  = empty_score () in

  let expected_result_files = 
    Common.readdir_to_file_list "tests/" +> List.filter (fun s -> 
      s =~ ".*\\.res$" && Common.filesize ("tests/" ^ s) > 0
    ) +> List.sort compare
  in

  begin
    expected_result_files +> List.iter (fun res -> 
      let x = if res =~ "\\(.*\\).res" then matched1 res else raise Impossible 
      in
      let base = if x =~ "\\(.*\\)_ver[0-9]+" then matched1 x else x in 
      let cfile      = "tests/" ^ x ^ ".c" in
      let cocci_file = "tests/" ^ base ^ ".cocci" in
      let expected = "tests/" ^ res in

      try (
        Common.timeout_function timeout_testall  (fun () -> 
          
          let xs = Cocci.full_engine (cocci_file, !Config.std_iso) [cfile] in
          let generated = 
            match List.assoc cfile xs with
            | Some generated -> generated
            | None -> cfile
          in

          let (correct, diffxs) = Compare_c.compare_default generated expected
          in

	  pr2 res;
          (* I don't use Compare_c.compare_result_to_string because
           * I want to indent a little more the messages.
           *)
          (match correct with
          | Compare_c.Correct -> Hashtbl.add newscore res Common.Ok;
          | Compare_c.Pb s -> 
              let s = Str.global_replace 
                (Str.regexp "\"/tmp/cocci-output.*\"") "<COCCIOUTPUTFILE>" s
              in
              let s = 
                "INCORRECT:" ^ s ^ "\n" ^ 
                "    diff (result(<) vs expected_result(>)) = \n" ^
                (diffxs +> List.map(fun s -> "    "^s^"\n") +> Common.join "")
              in
              Hashtbl.add newscore res (Common.Pb s)
          | Compare_c.PbOnlyInNotParsedCorrectly s -> 
              let s = 
                "seems incorrect, but only because of code that " ^
                "was not parsable" ^ s
              in
              Hashtbl.add newscore res (Common.Pb s)
          )
        )
      )
      with exn -> 
        Common.reset_pr_indent();
        let s = "PROBLEM\n" ^ ("   exn = " ^ Printexc.to_string exn ^ "\n") in
        Hashtbl.add newscore res (Common.Pb s)
    );


    pr2 "--------------------------------";
    pr2 "statistics";
    pr2 "--------------------------------";

    Common.hash_to_list newscore +> List.iter (fun (s, v) -> 
      pr_no_nl (Printf.sprintf "%-30s: " s);
      pr_no_nl (
        match v with
        | Common.Ok ->  "CORRECT\n" 
        | Common.Pb s -> s
      )
    );
    flush stdout; flush stderr;

    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    Common.regression_testing newscore best_score_file;


    pr2 "--------------------------------";
    pr2 "total score";
    pr2 "--------------------------------";
    let total = Common.hash_to_list newscore +> List.length in
    let good  = Common.hash_to_list newscore +> List.filter 
      (fun (s, v) -> v = Ok) +> List.length 
    in
    
    pr2 (sprintf "good = %d/%d" good total);

  end


(* ------------------------------------------------------------------------ *)

type okfailed = Ok | SpatchOK | Failed

(* test_to_string *)
let t_to_s = function
  | Ok -> ".ok"
  | SpatchOK -> ".spatch_ok"
  | Failed -> ".failed"

let delete_previous_result_files infile = 
  [Ok;SpatchOK;Failed] +> List.iter (fun kind -> 
    Common.command2 ("rm -f " ^ infile ^ t_to_s kind)
  )


let test_okfailed cocci_file cfiles = 


  cfiles +> List.iter delete_previous_result_files;

  (* final_files contain the name of an output file (a .ok or .failed
   * or .spatch_ok), and also some additionnal strings to be printed in
   * this output file in addition to the general error message of
   * full_engine. *)
  let final_files = ref [] in 


  let newout = Common.new_temp_file "cocci" ".stdout" in

  let redirect_in = 
    match cfiles with
    | x::xs -> 
        let (dir, base, ext) = Common.dbe_of_filename x in
        let varfile = Common.filename_of_dbe (dir, base, "var") in
        if ext = "c" && Common.lfile_exists varfile
        then Common.redirect_stdin varfile
        else (fun f -> f ())
    | [] -> failwith "wierd: no files for test_okfailed given"
  in
  
  redirect_in (fun () -> 
    Common.redirect_stdout_stderr newout (fun () -> 
      try (
        Common.timeout_function_opt !Flag_cocci.timeout (fun () ->
          
          let outfiles = Cocci.full_engine (cocci_file, !Config.std_iso) cfiles
          in
          
          outfiles +> List.iter (fun (infile, outopt) -> 
            let (dir, base, ext) = Common.dbe_of_filename infile in
            let expected_suffix   = 
              match ext with
              | "c" -> "res"
              | "h" -> "h.res"
              | s -> failwith ("wierd C file, not a .c or .h :" ^ s)
            in
            let expected_res =  
              Common.filename_of_dbe  (dir, base, expected_suffix) in
            let expected_res2 = 
              Common.filename_of_dbe (dir,"corrected_"^ base,expected_suffix) 
            in

            (* can detele more than the first delete_previous_result_files
             * because here we can have more files than in cfiles, for instance
             * the header files
             *)
          delete_previous_result_files infile;
          
          match outopt, Common.lfile_exists expected_res with
          | None, false -> 
              ()
          | Some outfile, false -> 
              let s =("PB: input file " ^ infile ^ " modified but no .res") in
              push2 (infile^t_to_s Failed, [s]) final_files

          | x, true -> 
              let outfile = 
                match x with 
                | Some outfile -> outfile 
                | None -> infile 
              in
              
              let diff = Compare_c.compare_default outfile expected_res in
              let s1 = (Compare_c.compare_result_to_string diff) in
              if fst diff = Compare_c.Correct
              then push2 (infile ^ (t_to_s Ok), [s1]) final_files
              else 
                if Common.lfile_exists expected_res2
                then begin
                  let diff = Compare_c.compare_default outfile expected_res2 in
                  let s2 = Compare_c.compare_result_to_string diff in
                  if fst diff = Compare_c.Correct
                  then push2 (infile ^ (t_to_s SpatchOK),[s2;s1]) final_files
                  else push2 (infile ^ (t_to_s Failed), [s2;s1]) final_files
                end
              else push2 (infile ^ (t_to_s Failed), [s1]) final_files
        )
      );
    )
    with exn -> 
      let clean s =
	Str.global_replace (Str.regexp "\\\\n") "\n"
	  (Str.global_replace (Str.regexp ("\\\\\"")) "\""
	     (Str.global_replace (Str.regexp "\\\\t") "\t" s)) in
      let s = "PROBLEM\n"^("   exn = " ^ clean(Printexc.to_string exn) ^ "\n")
      in
      (* we may miss some file because cfiles is shorter than outfiles.
       * For instance the detected local headers are not in cfiles, so
       * may have less failed. But at least have some failed.
       *)
      cfiles +> List.iter (fun infile -> 
        push2 (infile ^ (t_to_s Failed), [s]) final_files;
      );
  ));
  !final_files +> List.iter (fun (file, additional_strs) -> 
    Common.command2 ("cp " ^ newout ^ " " ^ file);
    with_open_outfile file (fun (pr, chan) -> 
      additional_strs +> List.iter (fun s -> pr (s ^ "\n"))
    );
    
  )

  


let test_regression_okfailed () = 

  let newscore  = Common.empty_score () in
  let oks = 
    Common.cmd_to_list ("find -name \"*.ok\"") 
    ++
    Common.cmd_to_list ("find -name \"*.spatch_ok\"")
  in
  let failed = Common.cmd_to_list ("find -name \"*.failed\"") in

  if null (oks ++ failed) 
  then failwith "no ok/failed file, you certainly did a make clean"
  else begin
    oks +> List.iter (fun s -> 
      Hashtbl.add newscore (Filename.chop_extension s)  Common.Ok
    );
    failed +> List.iter (fun s -> 
      Hashtbl.add newscore (Filename.chop_extension s) (Common.Pb "fail")
    );
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    Common.regression_testing newscore ("score_failed.marshalled")
  end
    





let compare_with_expected outfiles =
  raise Todo
(*
  outfiles +> List.iter (fun (infile, outopt) -> 
    let base = Common.fileprefix infile in
    let expected_res = base ^ ".res" in
            let (correct, diffxs) = 
              Compare_c.compare_default generated_file expected_res 
            in
            pr2 (Compare_c.compare_result_to_string (correct, diffxs));
            if (List.length fullxs = 1)
            then 
*)
  



(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let tmpfile = "/tmp/output.c" 



let test_tokens_c file = 
  if not (file =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.debug_lexer := true; 
  Flag_parsing_c.verbose_parsing := true;

  Parse_c.tokens file +> Common.pr2_gen


        

let test_parse_gen xs dirmode ext = 
        
  Flag_parsing_c.debug_typedef := true;
  Flag_parsing_c.debug_cpp := true;

  let fullxs = 
    if dirmode
    then Common.cmd_to_list ("find " ^(join " " xs) ^" -name \"*." ^ext^"\"")
    else xs 
  in
      
  let stat_list = ref [] in
  let newscore  = Common.empty_score () in

  fullxs +> List.iter (fun file -> 
    if not (file =~ (".*\\."^ext))
    then pr2 ("warning: seems not a ."^ext^" file");

    pr2 "";
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_c.parse_print_error_heuristic file in
    xs +> List.iter (fun (ast, (s, toks)) -> Parse_c.print_commentized toks);

    Common.push2 stat stat_list;
    let s = 
      sprintf "bad = %d, timeout = %B" 
        stat.Parse_c.bad stat.Parse_c.have_timeout
    in
    if stat.Parse_c.bad = 0 && not stat.Parse_c.have_timeout
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );
  
  if not (null !stat_list) 
  then Parse_c.print_parsing_stat_list !stat_list;
  
  if dirmode
  then begin 
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" (List.hd xs) in
    let def = if !Flag_parsing_c.filter_define_error then "_def_" else "" in
    let ext = if ext = "c" then "" else ext in
    Common.regression_testing newscore 
      ("/tmp/score_parsing__" ^str ^ def ^ ext ^ ".marshalled")
  end
        

let test_parse_cocci file = 
  if not (file =~ ".*\\.cocci") 
  then pr2 "warning: seems not a .cocci file";

  let (xs,_,_,_) = Parse_cocci.process file (Some !Config.std_iso) false in
  xs +> List.iter Pretty_print_cocci.unparse



(* file can be   "foo.c"  or "foo.c:main" *)
let test_cfg file = 

  let (file, specific_func) = 
    if file =~ "\\(.*\\.c\\):\\(.*\\)"
    then 
      let (a,b) = matched2 file in 
      a, Some b
    else 
      file, None
  in

  if not (file =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  let (program, _stat) = Parse_c.parse_print_error_heuristic file in

  program +> List.iter (fun (e,_) -> 
    let toprocess = 
      match specific_func, e with
      | None, _ -> true
      | Some s, Ast_c.Definition (((funcs, _, _, c),_))  -> 
          s = funcs
      | _, _ -> false 
    in
          
    if toprocess
    then 
      (* old: Flow_to_ast.test !Flag.show_flow def *)
      (try 
          let flow = Ast_to_flow.ast_to_control_flow e in
          flow +> do_option (fun flow -> 
            Ast_to_flow.deadcode_detection flow;
            let flow' = 
              if !Flag_cocci.show_before_fixed_flow 
              then flow
              else Ctlcocci_integration.fix_flow_ctl flow
            in
            Ograph_extended.print_ograph_mutable flow'  ("/tmp/output.dot")
          )
        with Ast_to_flow.Error (x) -> Ast_to_flow.report_error x
      )
  )

let test_parse_c xs dirmode = test_parse_gen xs dirmode "c"
let test_parse_h xs dirmode = test_parse_gen xs dirmode "h"
let test_parse_ch xs dirmode = test_parse_gen xs dirmode "[ch]"



let test_parse_unparse infile = 
  if not (infile =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  let (program2, _stat) = Parse_c.parse_print_error_heuristic infile in
  let program2_with_ppmethod = 
    program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
  in
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile)




let test_typeur infile = 
  if not (infile =~ ".*\\.c") 
  then pr2 "warning: seems not a .c file";

  Flag_parsing_c.pretty_print_type_info := true;

  let (program2, _stat) =  Parse_c.parse_print_error_heuristic infile in
  let program2 =
    program2 
    +> Common.unzip 
    +> (fun (program, infos) -> 
      Type_annoter_c.annotate_program Type_annoter_c.initial_env
        program +> List.map fst,
      infos
    )
    +> Common.uncurry Common.zip
  in
  let program2_with_ppmethod = 
    program2 +> List.map (fun x -> x, Unparse_c.PPnormal)
  in
  Unparse_c.pp_program program2_with_ppmethod tmpfile;
  Common.command2 ("cat " ^ tmpfile)


(* used by generic_makefile now *)
let test_compare_c file1 file2 = 
  let (correct, diffxs) = Compare_c.compare_default file1 file2 in
  let res = Compare_c.compare_result_to_bool correct in
  if res 
  then raise (Common.UnixExit 0)
  else raise (Common.UnixExit (-1))


let test_compare_c_hardcoded () =
  Compare_c.compare_default 
    "parsing_c/tests/compare1.c" 
    "parsing_c/tests/compare2.c" 
    (*
      "parsing_c/tests/equal_modulo1.c" 
      "parsing_c/tests/equal_modulo2.c" 
    *)
  +> Compare_c.compare_result_to_string 
  +> pr2




(*****************************************************************************)
(* to be called by ocaml toplevel, to test. *)
(*****************************************************************************)

let cprogram_of_file file = 
  let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
  program2 

let (cstatement_of_string: string -> Ast_c.statement) = fun s ->
  begin
    Common.write_file ("/tmp/__cocci.c") ("void main() { \n" ^ s ^ "\n}");
    let program = cprogram_of_file ("/tmp/__cocci.c") in
    program +> Common.find_some (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, [st]),_) -> Some st
      | _ -> None
      )
  end

let (cexpression_of_string: string -> Ast_c.expression) = fun s ->
  begin
    Common.write_file ("/tmp/__cocci.c") ("void main() { \n" ^ s ^ ";\n}");
    let program = cprogram_of_file ("/tmp/__cocci.c") in
    program +> Common.find_some (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, compound),_) -> 
          (match compound with
          | [(Ast_c.ExprStatement (Some e),ii)] -> Some e
          | _ -> None
          )
      | _ -> None
      )
  end
  


let sp_of_file file iso    = Parse_cocci.process file iso false

let (rule_elem_of_string: string -> filename option -> Ast_cocci.rule_elem) =
 fun s iso -> 
  begin
    Common.write_file ("/tmp/__cocci.cocci") (s);
    let (astcocci, _,_,_) = sp_of_file ("/tmp/__cocci.cocci") iso in
    let stmt =
      astcocci +>
      List.hd +> (function (_,_,_,x) -> List.hd x) +> (function x ->
	match Ast_cocci.unwrap x with
	| Ast_cocci.CODE stmt_dots -> Ast_cocci.undots stmt_dots +> List.hd
	| _ -> raise Not_found)
    in
    match Ast_cocci.unwrap stmt with
    | Ast_cocci.Atomic(re) -> re
    | _ -> failwith "only atomic patterns allowed"
  end




(*
let flows_of_ast astc = 
  astc +> Common.map_filter (fun e -> ast_to_flow_with_error_messages e)

let one_flow flows = 
  List.hd flows

let one_ctl ctls = List.hd (List.hd ctls)
*)



(*****************************************************************************)
(* xxx *)
(*****************************************************************************)

open Ast_c 

type entities = {
  macros: string hashset; (* object-like or function-like *)
  variables: string hashset;
  static_variables: string hashset;
  functions: string hashset;
  static_functions: string hashset;
  structs: string hashset; (* union defs, enum defs, enum values *)
  typedefs: string hashset;
}

(* inverted index *)
type idx_entities = { 
  idx_macros: (string, (filename hashset)) Hashtbl.t; 
  idx_variables: (string, (filename hashset)) Hashtbl.t; 
  idx_functions: (string, (filename hashset)) Hashtbl.t; 
  idx_structs: (string, (filename hashset)) Hashtbl.t; 
  idx_typedefs: (string, (filename hashset)) Hashtbl.t; 
}

let empty_entities () = {
  macros = Hashtbl.create 101;
  variables = Hashtbl.create 101;
  static_variables = Hashtbl.create 101;
  functions = Hashtbl.create 101;
  static_functions = Hashtbl.create 101;
  structs = Hashtbl.create 101;
  typedefs = Hashtbl.create 101;
}

let empty_idx_entities () = {
  idx_macros = Hashtbl.create 101;
  idx_variables = Hashtbl.create 101;
  idx_functions = Hashtbl.create 101;
  idx_structs = Hashtbl.create 101;
  idx_typedefs = Hashtbl.create 101;
}


let h_to_l h = Common.hash_to_list h +> List.map fst

let print_entities e =
 begin
(*  e.macros +> h_to_l +> List.iter (fun s -> pr("MACRO: " ^ s)); *)
  e.variables +> h_to_l +> List.iter (fun s -> pr("VAR: " ^ s));
  e.static_variables +> h_to_l +> List.iter (fun s -> pr("STATICVAR: " ^ s));
  e.functions +> h_to_l +> List.iter (fun s -> pr("FUNC: " ^ s));
  e.static_functions +> h_to_l +> List.iter (fun s -> pr("STATICFUNC: " ^ s));
  e.structs +> h_to_l +> List.iter (fun s -> pr("STRUCT: "^s));
  e.typedefs +> h_to_l +> List.iter (fun s -> pr("TYPEDEF: "^s));
 end
  
(* look only for toplevel definition *)
let defined_stuff xs = 
  let e = empty_entities() in
  let add h s = Hashtbl.add h  s true in

  (* look only for toplevel definition: don't recurse, don't call k *)
  let bigf = { Visitor_c.default_visitor_c with
    Visitor_c.kprogram = (fun (k,bigf) t -> 
      match t with
      | Declaration decl -> 
          (match decl with
          | DeclList (xs, ii) -> 
              xs +> List.iter (fun ((var, t, sto), iicomma) -> 
                Visitor_c.vk_type bigf t;
                match var, sto with 
                | None, _ -> ()
                | Some ((s, ini), ii_s_ini),  (StoTypedef,inline) -> 
                    add e.typedefs s;
                | Some ((s, ini), ii_s_ini),  (Sto Static,inline) -> 
                    (* need add them to do the adjust_need *)
                    add e.static_variables s;

                | Some ((s, ini), ii_s_ini),  (Sto Extern,inline) -> 
                    ()
                | Some ((s, ini), ii_s_ini),  (_,inline) -> 
                    add e.variables s;
              );
          | MacroDecl ((s, args),ii) -> ()
          )

      | Definition def -> 
          let ((s, typ, sto, cp), ii) = def in
          (match sto with
          | Sto Static, inline ->
              (* need add them to do the adjust_need *)
              add e.static_functions s
          | _ -> add e.functions s
          )

      | Include includ -> ()
      | Define ((s,ii), body) -> add e.macros s
      | MacroTop (s, args, ii) -> ()

      | EmptyDef _ | NotParsedCorrectly _ | FinalDef _ -> ()
    );

    Visitor_c.ktype = (fun (k, bigf) t -> 
      match Ast_c.unwrap_typeC t with
      | StructUnion (su, sopt, fields) -> 
          sopt +> do_option (fun s -> 
            add e.structs s;
          );
        
      | _ -> ()
    );

  } in
  xs +> List.iter (fun (p, info_item) -> Visitor_c.vk_program bigf p);
  e






(* look only for use of external stuff. Don't consider local vars, 
 * typedefs, structures *)
let used_stuff xs = 

  let e = empty_entities() in
  let add h s = Hashtbl.replace h s true in

  let initial_env = [
    ["NULL";
     "do_gettimeofday";
     "le32_to_cpu";
     "udelay";
     "printk";
     (* !!! sometimes considered as VAR :( *)
     "u8"; "u16"; "u32"; 
     "s32";
    ] +> List.map (fun s -> s, true);
  ]
  in
  let regexp_macro =  Str.regexp
    "^[A-Z_][A-Z_0-9]*$" 
  in

  let (_env: (string, bool) Common.scoped_env ref) = ref initial_env in


  let bigf = { Visitor_c.default_visitor_c with

    (* --------- handling scope of variables --------- *)
    Visitor_c.kstatement = (fun (k, bigf) st -> 
      match st with 
      | Compound statxs, ii -> Common.do_in_new_scope _env (fun () -> k st);
      | _ -> k st
    );
    Visitor_c.kdecl = (fun (k, bigf) d -> 
      k d; (* to add possible definition in type found in Decl *)
      (match d with
      | (DeclList (xs, ii)) -> 
          xs +> List.iter (fun ((var, t, sto), iicomma) -> 
            var +> do_option (fun ((s, ini), ii_s_ini) -> 
              match sto with 
              | StoTypedef, _inline -> 
                  (* add_binding (TypeDef (s)) true; *)
                  ()
              | _ ->
                  Common.add_in_scope _env (s, true);
            );
          );
      | _ -> ()
      );
    );
    Visitor_c.kprogram = (fun (k, bigf) elem -> 
      match elem with
      | Definition def -> 
          let (funcs, ((returnt, (paramst, b))), sto, statxs),ii = def in
          Common.do_in_new_scope _env (fun () -> 
            paramst +> List.iter (fun (((b, s, t), _),_) -> 
              match s with 
              | Some s -> Common.add_in_scope _env (s, true)
              | None -> pr2 "no type, certainly because Void type ?"
            );
            k elem
          );
      | Define (s, (defkind, defval)) -> 
          Common.do_in_new_scope _env (fun () -> 
            (match defkind with
            | DefineFunc (params, ii) -> 
                params +> List.iter (fun ((s,iis), iicomma) -> 
                  Common.add_in_scope _env (s, true)
                );
            | _ -> ()
            );
            k elem
          );
      | _ -> k elem
    );


    (* --------- and now looking for use --------- *)
    Visitor_c.kexpr = (fun (k,bigf) x -> 
      match Ast_c.unwrap_expr x with

      | FunCall (((Ident f, typ), ii), args) -> 
          if not (Common.member_env_key f !_env)
          then 
            if f ==~ regexp_macro
            then add e.macros f
            else add e.functions f
          ;
          args +> List.iter (fun (x,ii) -> 
            Visitor_c.vk_argument bigf x
          );
      | Ident s -> 
          if not (Common.member_env_key s !_env)
          then 
            if s ==~ regexp_macro
            then add e.macros s
            else add e.variables s

      | _ -> k x
    );

    Visitor_c.ktype = (fun (k,bigf) t -> 
      match Ast_c.unwrap_typeC t with
      | StructUnionName (su, s) -> 
          if not (Common.member_env_key s !_env)
          then 
            add e.structs s;
      | TypeName s -> 
          if not (Common.member_env_key s !_env)
          then 
            add e.typedefs s;
      | _ -> k t
    );

  } in
  xs +> List.iter (fun (p, info_item) -> Visitor_c.vk_program bigf p);
  e

let adjust_used_only_external used defined = 
 begin
  used.variables +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.variables s || 
       Hashtbl.mem defined.static_variables s || 
       Hashtbl.mem defined.functions s ||
       Hashtbl.mem defined.static_functions s
    then Hashtbl.remove used.variables s);
  used.functions +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.functions s || 
       Hashtbl.mem defined.static_functions s
    then Hashtbl.remove used.functions s);
  used.structs +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.structs s
    then Hashtbl.remove used.structs s);
  used.typedefs +> h_to_l +> List.iter (fun s -> 
    if Hashtbl.mem defined.typedefs s
    then Hashtbl.remove used.typedefs s);
 end


let cprogram_of_file_cached file = 
  Common.cache_file file ".ast_raw" (fun () -> cprogram_of_file file)
  

type used_defined = { 
  used: entities;
  defined: entities;
}
type global_definitions = idx_entities

let mk_global_definitions_index xs = 
  let idx = empty_idx_entities () in
  xs +> List.iter (fun (file, {defined = defined}) -> 
    defined.variables +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_variables;
    );
    defined.functions +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_functions;
    );
    defined.structs +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_structs;
    );
    defined.typedefs +> h_to_l +> List.iter (fun s -> 
      Common.hash_hashset_add s file idx.idx_typedefs;
    );
  );
  idx

let known_duplicate = ["init_module"; "cleanup_module"] 

let check_no_duplicate_global_definitions idx = 
 begin
  pr2 "DUPLICATE processing:";
  idx.idx_functions +> hash_to_list +> List.iter (fun (f, set) -> 
    let xs = hash_to_list set in
    if List.length xs <> 1 && not (List.mem f known_duplicate)
    then 
      pr2 ("mutliple def for : " ^ f ^ " in " ^ (join " " (List.map fst xs)));
  );
 end
  
  

let build_graph xs dep graphfile = 
  let g = ref (new Ograph_extended.ograph_mutable)  in
  let h = Hashtbl.create 101 in
  let s_to_nodei s = Hashtbl.find h s in

  pr2 "BUILDING graph:";
  (* build nodes *)
  xs +> List.iter (fun (file, {used = used}) -> 
    let xi = !g#add_node (file, Filename.basename file) in
    Hashtbl.add h file xi;
  );

  xs +> List.iter (fun (file, {used = used}) -> 

    used.functions +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_functions s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
            let (file, file_defined) = basename file, basename file_defined in
            pr2 (sprintf "%-20s -- f:%25s --> %s" file s file_defined);
          );
    );
    (* sometime use functions as variable *)
    used.variables +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_functions s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
            let (file, file_defined) = basename file, basename file_defined in
            pr2 (sprintf "%-20s -- f:%25s --> %s" file s file_defined);
          );
    );

    used.variables +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_variables s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
            let (file, file_defined) = basename file, basename file_defined in
            pr2 (sprintf "%-20s -- v:%10s --> %s" file s file_defined);
          );
    );
(*
    used.structs +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_structs s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
          );
    );

    used.typedefs +> h_to_l +> List.iter (fun s -> 
      match Common.optionise (fun () -> Hashtbl.find dep.idx_typedefs s) with
      | None -> ()
      | Some hset -> 
          hset +> h_to_l +> List.iter (fun file_defined -> 
            !g#add_arc ((s_to_nodei file, s_to_nodei file_defined), true);
          );
    );
*)

  );
  Ograph_extended.print_ograph_mutable !g graphfile
  
    
  

let test_xxx () = 
  Sys.chdir "/home/pad/kernels/git/linux-2.6";
  let path="drivers/net" in

  let dirs = Common.cmd_to_list ("find " ^ path ^ " -type d") in
  dirs +> List.iter (fun dir -> 
    
    let c_info = 
      Common.glob (Filename.concat dir "*.[c]")
      +> List.map (fun file -> 
        let x = cprogram_of_file_cached file in
        let defined = defined_stuff x in
        let used = used_stuff x in
        adjust_used_only_external used defined;
        file, { used = used; defined = defined}
      ) in
    let global = mk_global_definitions_index c_info in
    c_info +> List.iter (fun (file, used_defined) -> 
      pr2 ("HANDLING : " ^ file);
      print_entities used_defined.used;
    );
    check_no_duplicate_global_definitions global;
    (*build_graph c_info global (Filename.concat dir "depgraph.dot");*)
  );





(*
  ignore(Parse_c.parse_cpp_define_file "standard.h")
  pr2 "pr2";
  pr  "pr"

  Format.print_newline();
  Format.printf "@[<v 5>--@,--@,@[<v 5>--@,--@,@]--@,--@,@]";
  Format.print_newline();
  Format.printf "@[<v>(---@[<v>(---@[<v>(---@,)@]@,)@]@,)@]"
*)

