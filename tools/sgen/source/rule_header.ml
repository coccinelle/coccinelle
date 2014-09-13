module Ast = Ast_cocci
module M = Meta_variable

(* ------------------------------------------------------------------------- *)

(* Generates rule headers with dependencies, isos, etc. in correct order.
 * Also possible to generate rule header with hardcoded standard dependencies.
 *)

(* ------------------------------------------------------------------------- *)
(* TYPES AND HELPERS *)

type t = {
  first_line : string; (* @rulename ...@ *)
  meta_vars : M.t list;
  meta_pos : M.t list;
  last_line : string; (* @@ *)
}

let comma_sep = String.concat ", "


(* ------------------------------------------------------------------------- *)
(* DEPENDENCIES *)

(* adds the hardcoded 'default' context rule dependency.
 * context mode: context || org || report
 * patch mode: !patch && (context || org || report) *)
let add_context_dependency dep context_mode =
  let context_dep = (* context || org || report *)
    Ast.OrDep(Ast.Dep "context", Ast.OrDep(Ast.Dep "org", Ast.Dep "report")) in
  let context_dep =
    if context_mode then context_dep
    else Ast.AndDep(Ast.AntiDep "patch", context_dep) in
  if dep = Ast.NoDep then context_dep else Ast.AndDep(dep, context_dep)

(* adds the hardcoded 'default' patch rule dependency.
 * patch && !context && !org && !report *)
let add_patch_dependency dep =
  let patch_dep =
    Ast.AndDep(
      Ast.Dep "patch",
      Ast.AndDep(
        Ast.AntiDep "context",
        Ast.AndDep(Ast.AntiDep "org", Ast.AntiDep "report"))) in
  if dep = Ast.NoDep then patch_dep else Ast.AndDep(dep, patch_dep)

(* Stringify dependencies. Adds parentheses where necessary for precedence. *)
let tostring_dep = function
  | Ast.NoDep -> ""
  | deps ->
      let rec dep in_and n k = match n with
        | Ast.Dep s -> k s
        | Ast.AntiDep s -> k ("!" ^ s)
        | Ast.EverDep s -> k ("ever " ^ s)
        | Ast.NeverDep s -> k ("never " ^ s)
        | Ast.AndDep(s1, s2) ->
            dep true s1 (fun l ->
              dep true s2 (fun r -> k (l ^ " && " ^ r)))
        | Ast.OrDep(s1, s2) ->
            dep false s1 (fun l ->
              dep false s2 (fun r ->
                let s = l ^ " || " ^ r in
                if in_and then k ("(" ^ s ^ ")") else k s))
        | Ast.NoDep -> k "no_dep"
        | Ast.FailDep -> k "fail_dep" in
      " depends on " ^ (dep false deps (fun x -> x))


(* ------------------------------------------------------------------------- *)
(* RULE HEADERS *)

(* gather data into a string rule header... may or may not be slightly buggy.
 * TODO: add newlines at 80-character limit
 *)
let rule_declaration ~rulename ~isos ~dropisos ~deps ~exists =
  let _ = assert (not (String.contains rulename ' ')) in
  let extends = "" in (* where is this information ?? *)
  let expression = "" in (* where is this information ?? *)
  let deps = tostring_dep deps in
  let isos = match isos with
    | [] -> ""
    | x -> " using " ^ (comma_sep (List.map (fun x -> "\""^x^"\"") x)) in
  let dropisos = match dropisos with
    | [] -> ""
    | x -> " disable " ^ (comma_sep x) in
  let exists = match exists with
    | Ast.Exists -> " exists" | Ast.Forall -> " forall"
    | Ast.Undetermined -> "" in
  String.concat ""
    ["@"; rulename; extends; deps; isos; dropisos; exists; expression; "@"]

(* generate 'vanilla' rule header (ie. no new dependencies) *)
let generate ~rulename ~isos ~dropisos ~deps ~exists ~meta_vars ~meta_pos =
  let first_line = rule_declaration ~rulename ~isos ~dropisos ~deps ~exists in
  let last_line = "@@\n\n" in
  {first_line = first_line ^ "\n"; meta_vars; meta_pos; last_line;}

(* generate rule header with patch dependency *)
let generate_patch
  ~rulename ~isos ~dropisos ~deps ~exists ~meta_vars ~meta_pos =
  let deps = add_patch_dependency deps in
  generate ~rulename ~isos ~dropisos ~deps ~exists ~meta_vars ~meta_pos

(* generate rule header with context/org/report dependency.
 * if in context_mode, don't include !patch. *)
let generate_context
  ~rulename ~isos ~dropisos ~deps ~exists ~meta_vars ~meta_pos ~context_mode =
  let deps = add_context_dependency deps context_mode in
  generate ~rulename ~isos ~dropisos ~deps ~exists ~meta_vars ~meta_pos

(* print a rule header *)
let print chan {first_line = f; meta_vars = mv; meta_pos = mp; last_line = l;} =
  output_string chan f;
  M.print chan ~do_group:true mv;
  M.print chan ~do_group:true mp;
  output_string chan l

(* prints only the first line of the rule header, ie. the declaration *)
let print_declaration chan {first_line = f; _} = output_string chan f
