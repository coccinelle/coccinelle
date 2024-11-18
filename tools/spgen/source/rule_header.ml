(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

module Ast = Ast_cocci
module MV = Meta_variable

(* ------------------------------------------------------------------------- *)

(* Generates rule headers with dependencies, isos, etc. in correct order. *)

(* ------------------------------------------------------------------------- *)
(* TYPES AND HELPERS *)

type t =
{
  first_line : string; (* @rule_name ...@ *)
  meta_vars : MV.t list;
  meta_pos : MV.t list;
  last_line : string; (* @@ *)
}

let comma_sep = String.concat ", "

(* Stringify dependencies. Adds parentheses where necessary for precedence. *)
let tostring_dep = function
  | Ast.NoDep -> ""
  | deps ->
      let rec dep in_and n k =
        match n with
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
        | Ast.FileIn s -> k ("file in \"" ^ s ^ "\"")
        | Ast.NotFileIn s -> k ("!(file in \"" ^ s ^ "\")") in
      let topdep = function
        | Ast.NoDep -> "no_dep"
        | Ast.FailDep -> "fail_dep"
	| Ast.ExistsDep d -> dep false d (fun x -> x)
	| Ast.ForallDep d -> "forall " ^ (dep false d (fun x -> x)) in
      " depends on " ^ (topdep deps)

(* gather data into a string rule header... may or may not be slightly buggy.*)
let rule_declaration ~rule_name ~isos ~drop_isos ~deps ~exists =
  let _ = assert (not (String.contains rule_name ' ')) in
  let extends = "" in (* where is this information ?? *)
  let expression = "" in (* where is this information ?? *)
  let deps = tostring_dep deps in
  let isos = match isos with
    | [] -> ""
    | x -> " using " ^ (comma_sep (List.map (fun x -> "\""^x^"\"") x)) in
  let drop_isos = match drop_isos with
    | [] -> ""
    | x -> " disable " ^ (comma_sep x) in
  let exists = match exists with
    | Ast.Exists -> " exists" | Ast.Forall -> " forall"
    | Ast.Undetermined -> "" in
  Globals.concat_limit_width
    ["@"; rule_name; extends; deps; isos; drop_isos; exists; expression; "@"]

(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

(* generate rule header *)
let generate
  ~rule_name ~isos ~drop_isos ~deps ~exists ~meta_vars ~meta_pos =
  let first_line =
    rule_declaration ~rule_name ~isos ~drop_isos ~deps ~exists in
  let last_line = "@@\n\n" in
  {
    first_line = first_line ^ "\n";
    meta_vars; meta_pos; last_line;
  }

(* print a rule header *)
let print out {first_line = f; meta_vars = mv; meta_pos = mp; last_line = l;} =
  output_string out f;
  MV.print_list out ~do_group:true mv;
  MV.print_list out ~do_group:true mp;
  output_string out l

(* prints only the first line of the rule header, ie. the declaration *)
let print_declaration out {first_line = f; _} = output_string out f
