(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(*****************************************************************************)
(* Includes dependency graph *)
(*****************************************************************************)

val add_to_dependency_graph : Common.filename -> Common.filename -> unit

val print_dependency_graph : unit -> unit

(*****************************************************************************)
(* Name cache *)
(*****************************************************************************)

type cache_exp =
  | CacheField of string (* Name of the struct/union it is defined in *)
  | CacheEnumConst
  | CacheVarFunc
  | CacheTypedef

type cache_return =
  | RetVarOrFunc of string * Ast_c.exp_type
  | RetEnumConstant of string * string option
  | RetTypeDef   of string * Ast_c.fullType
  | RetStructUnionNameDef of string * (Ast_c.structUnion * Ast_c.structType)
                          Ast_c.wrap

val extract_names : Common.filename -> Ast_c.program -> unit

val get_types_from_name_cache :
  Common.filename ->
    string ->
    cache_exp list ->
    (Common.filename -> Ast_c.program) ->
    cache_return list

(*****************************************************************************)
(* Set of parsed files *)
(*****************************************************************************)

val has_been_parsed : Common.filename -> bool

val add_to_parsed_files : Common.filename -> unit
