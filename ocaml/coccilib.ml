open Common

(** A library of functions for use with Coccinelle OCaml script code.
*)


(** Coccinelle modules accessible from an ocaml script.
*)
include Exposed_modules
(**/**)

(** A value of type {b pos} describes a position in a source file.
*)
type pos = {
  current_element : string;
  (** {b current_element} is the name of the function containing the
      matched position *)
  current_element_line : int;
  (** {b line} is the number of the line containing the first
      character of the matched position *)
  current_element_col : int;
  (** {b col} is the column containing the first character of the
      matched position *)
  current_element_line_end : int;
  (** {b line_end} is the number of the line containing the last
      character of the matched position *)
  current_element_col_end : int;
  (** {b col_end} is the column containing the last character of the
       matched position. *)
  file :string ;
  (** {b file} is the name of the file containing the matched
      position *)
  line : int;
  (** {b line} is the number of the line containing the first
      character of the matched position *)
  col : int;
  (** {b col} is the column containing the first character of the
      matched position *)
  line_end : int;
  (** {b line_end} is the number of the line containing the last
      character of the matched position *)
  col_end : int;
  (** {b col_end} is the column containing the last character of the
       matched position. *)
}

(**
   Types describing the metavariables.
*)
type param_type =
    Pos of pos list
  | Com of (string list * string list * string list) list
  | AstCom of (Token_c.comment_like_token list *
		 Token_c.comment_like_token list *
		 Token_c.comment_like_token list) list
  | AssignOp of Ast_c.assignOp
  | BinaryOp of Ast_c.binaryOp
  | Str of string
  | Type of Ast_c.fullType
  | Init of Ast_c.initialiser
  | InitList of Ast_c.initialiser Ast_c.wrap2 list
  | Int of int
  | Param of Ast_c.parameterType
  | ParamList of Ast_c.parameterType Ast_c.wrap2 list
  | DParamList of (string Ast_c.wrap) Ast_c.wrap2 list
  | Expr of Ast_c.expression
  | ExprList of Ast_c.argument Ast_c.wrap2 list
  | Decl of Ast_c.declaration
  | Field of Ast_c.field
  | FieldList of Ast_c.field list
  | FragList of Ast_c.string_fragment list
  | Fmt of Ast_c.string_format
  | Attribute of Ast_c.attribute
  | AttrArg of Ast_c.attr_arg
  | Stmt of Ast_c.statement
  | StmtList of Ast_c.statement_sequencable list


(* Function table management *)

(**
   For internal use only.
*)
let fcts :
 (string, param_type list -> Ast_c.metavar_binding_kind ref list -> unit)
    Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

let bool_fcts :
 (string, param_type list -> bool) Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

let string_fcts :
 (string, param_type list -> string) Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

let variables_to_merge: (unit -> string array) ref = ref (fun () -> [| |])

let merged_variables: string list array option ref = ref None
(**/**)

(* This code needs to be here because we need to call the type annotater *)
let no_format s =
  try let _ = Str.search_forward (Str.regexp_string "%") s 0 in false
  with Not_found -> true

(* no point to parse strings in these cases. never applied to a format
   string *)
(* The env argument makes it possible to add some variable declarations to
   create a type environment.  This could be exploited for other things, but
   not clear why that would be useful. *)
let (cstatement_of_string: string -> string -> Ast_c.statement) =
  fun env s ->
(*  assert (no_format s);*) (* format strings are not getting parsed *)
  let tmpfile = Common.new_temp_file "cocci_stmt_of_s" "c" in
  Common.write_file tmpfile (Printf.sprintf "void main() {\n%s\n%s\n}" env s);
  let program = Parse_c.parse_c_and_cpp false false tmpfile +> fst in
  let _ =
    Type_annoter_c.annotate_program !Type_annoter_c.initial_env
      (List.map fst program) in
  program +> Common.find_some (fun (e,_) ->
    match e with
    | Ast_c.Definition ({Ast_c.f_body = l},_) ->
	(match List.rev l with
	  (Ast_c.StmtElem st) :: _ -> Some st
	| _ -> None)
    | _ -> None
  )

let (cexpression_of_string: string -> string -> Ast_c.expression) =
  fun env s ->
(*  assert (no_format s);*) (* format strings are not getting parsed *)
  let tmpfile = Common.new_temp_file "cocci_expr_of_s" "c" in
  Common.write_file tmpfile (Printf.sprintf "void main() {\n%s\n%s;\n}" env s);
  let program = Parse_c.parse_c_and_cpp false false tmpfile +> fst in
  let _ =
    Type_annoter_c.annotate_program !Type_annoter_c.initial_env
      (List.map fst program) in
  let rec find_some p = function
    | [] -> failwith (Printf.sprintf "make_expr: no expression found in: %s" s)
    | x :: l ->
	match p x with
	| Some v -> v
	| None -> find_some p l in
  program +> find_some (fun (e,_) ->
    match e with
    | Ast_c.Definition ({Ast_c.f_body = compound},_) ->
	(match List.rev compound with
	| Ast_c.StmtElem st :: _ ->
	    (match Ast_c.unwrap_st st with
	    | Ast_c.ExprStatement (Some e) -> Some e
	    | _ -> None)
	| _ -> None)
    | _ -> None)

let parse_failure str s f =
  try f s
  with e -> Printf.eprintf "failed to parse %s %s\n" str s; raise e

let make_ident s = Ast_c.MetaIdVal(s)
let make_expr s =
  parse_failure "expression" s
    (function s ->
      Ast_c.MetaExprVal(Lib_parsing_c.al_expr(cexpression_of_string "" s), [],
			Ast_c.WITHOUT_TYPES))
let make_expr_with_env env s =
  parse_failure "expression" s
    (function s ->
      Ast_c.MetaExprVal(Lib_parsing_c.al_expr(cexpression_of_string env s), [],
			Ast_c.WITH_TYPES))
let make_stmt s =
  parse_failure "statement" s
    (function s ->
      let s = Lib_parsing_c.al_statement(cstatement_of_string "" s) in
      Ast_c.MetaStmtVal(s,s,Ast_c.WITHOUT_TYPES))
let make_stmt_with_env env s =
  parse_failure "statement" s
    (function s ->
      let s = Lib_parsing_c.al_statement(cstatement_of_string env s) in
      Ast_c.MetaStmtVal(s,s,Ast_c.WITH_TYPES))
let make_type s =
  parse_failure "type" s
    (function s ->
      Ast_c.MetaTypeVal(Lib_parsing_c.al_type(Parse_c.type_of_string s)))
let make_listlen i = Ast_c.MetaListlenVal i
let make_full_position fl fn ce_startl ce_startc ce_endl ce_endc
    startl startc endl endc =
  Ast_c.MetaPosValList
    [(fl, fn, Some ((ce_startl,ce_startc),(ce_endl,ce_endc)),
      (startl, startc), (endl,endc))]
let make_position fl fn startl startc endl endc =
  Ast_c.MetaPosValList [(fl, fn, None, (startl, startc), (endl,endc))]

(* ---------------------------------------------------------------------- *)
(* Match management *)

(**
   See include_match.
*)
let inc_match = ref true

(** If the argument is true, retain the environment with respect to
    which the ocaml script code is being executed for use in subsequent
    rules.  If the argument is false, discard this environment.  By
    default, the environment is retained.
*)
let include_match x = inc_match := x

(**
   See exit
*)
let exited = ref false

(** If called, aborts the treatment of the current file.  All previous
    changes take effect.
*)
let exit () = exited := true

let dir () = !Flag.dir

let files () = !Flag.currentfiles

let cocci_version () = Config.version

(* ---------------------------------------------------------------------- *)
(* org mode *)

let build_link p msg color =
  Printf.sprintf
    "[[view:%s::face=%s::linb=%d::colb=%d::cole=%d][%s]]"
    p.file color p.line p.col p.col_end msg

let print_todo ?color:(color="ovl-face1") ?msg:(msg="") p =
  let msg =
    if msg = ""
    then Printf.sprintf "%s::%d" p.file p.line
    else msg in
  Printf.printf "* TODO %s\n" (build_link p msg color)

let print_link ?color:(color="ovl-face2") ?msg:(msg="") p =
  let msg =
    if msg = ""
    then Printf.sprintf "%s::%d" p.file p.line
    else msg in
  Printf.printf "%s\n" (build_link p msg color)

let print_safe_todo ?color:(color="ovl-face1") ?msg:(msg="") p =
  let msg = String.concat "@(" (Str.split_delim (Str.regexp_string "[") msg) in
  let msg = String.concat ")" (Str.split_delim (Str.regexp_string "]") msg) in
  print_todo ~color:color ~msg:msg p

let print_safe_link ?color:(color="ovl-face2") ?msg:(msg="") p =
  let msg = String.concat "@(" (Str.split_delim (Str.regexp_string "[") msg) in
  let msg = String.concat ")" (Str.split_delim (Str.regexp_string "]") msg) in
  print_link ~color:color ~msg:msg p

(*
print_main, print_sec and print_secs
*)
let print_main ?color:(color="ovl-face1") msg ps =
  let p = List.hd ps in
  let oldmsgfmt =
    if msg == ""
    then Printf.sprintf "%s::%d" p.file p.line
    else Printf.sprintf "%s %s::%d" msg p.file p.line in
  print_todo ~color:color ~msg:oldmsgfmt p

let print_sec ?color:(color="ovl-face2") msg ps =
  print_link ~color:color ~msg:msg (List.hd ps)

let print_secs ?color:(color="ovl-face2") msg ps =
  List.iter (function i -> print_link ~color:color ~msg:msg i) ps

(*
pos transformations
*)

(** convert the filename of a pos to its basename *)
let basename_pos pos = { pos with file = Filename.basename (pos.file) }


(*
external analysis results interface
(in a separate module to not pollute the namespace)
*)

(** external analysis integration. Note: do not use after transformations. *)
module Ana = struct
  (** the type of analysis results: currently only integer ranges. *)
  type result = Externalanalysis.result
  type bound  = Externalanalysis.bound

  (** convert a bound to a string for showing. *)
  let show_bound = Externalanalysis.show_bound

  (** convert a result value to a string for showing. *)
  let show_result = Externalanalysis.show_result

  (** loads some analysis results from the given file. *)
  let load_results =
    Externalanalysis.load_external_results

  (** finds the analysis results for a given position. *)
  let find pos =
    Externalanalysis.find_results pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** computes the intersection of analysis results, if possible. *)
  let inter = Externalanalysis.intersect_results

  (** predicate over a list of analysis results of a given position. *)
  let satisfy f pos =
    Externalanalysis.satisfy f pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** predicate over the intersection of analysis results. *)
  let satisfy1 f pos =
    Externalanalysis.satisfy1 f pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** true if an analysis result exists for the given position. *)
  let has_any pos =
    Externalanalysis.has_any_result pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** predicate over all analysis results of a given position. *)
  let for_all p pos =
    Externalanalysis.for_all p pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** predicate over all analysis results (at least one) of a given position. *)
  let for_all1 p pos =
    Externalanalysis.for_all1 p pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** true if the predicate is satisfied for at least one result
        of a given position. *)
  let exists p pos =
    Externalanalysis.exists p pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** true if the result contains only a single integer as range *)
  let single_int = Externalanalysis.single_int

  (** true if the result range contains the given integer. *)
  let contains_int = Externalanalysis.contains_int

  (** analysis result of the position has only the zero value. *)
  let has_only_nul pos =
    Externalanalysis.has_only_nul pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** analysis result of the position contains also the zero value. *)
  let has_also_nul pos =
    Externalanalysis.has_also_nul pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  (** analysis result of the position contains also the given integer. *)
  let has_also_int c pos =
    Externalanalysis.has_also_int c pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)
end
