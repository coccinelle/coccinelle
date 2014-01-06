type nameenv =
    {var_or_func : (string * Ast_c.exp_type) list;
      enum_constant : (string * string option) list;
      typedef : (string * Ast_c.fullType) list;
      struct_union_name_def :
	(string * (Ast_c.structUnion * Ast_c.structType) Ast_c.wrap) list;
      macro : (string * (Ast_c.define_kind * Ast_c.define_val)) list}

(* have nested scope, so nested list*)
type environment = nameenv list

(* can be set with init_env *)
val initial_env : environment ref
(* ex: config/envos/environment_unix.h, seems to be unused *)
val init_env_unused : Common.filename -> unit



val annotate_type_and_localvar :
  environment -> Ast_c.toplevel list ->
  (Ast_c.toplevel * environment Common.pair) list

(* julia: cocci *)
val annotate_test_expressions :
  Ast_c.toplevel list -> unit



(* !!Annotate via side effects!!. Fill in the type
 * information that was put to None during parsing.
 *)
val annotate_program :
  environment -> Ast_c.toplevel list ->
  (Ast_c.toplevel * environment Common.pair) list

