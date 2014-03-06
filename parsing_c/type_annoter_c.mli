(* have nested scope, so nested list*)
type environment

(* can be set with init_env *)
val initial_env : environment ref
(* ex: config/envos/environment_unix.h, seems to be unused *)

(* !!Annotate via side effects!!. Fill in the type
 * information that was put to None during parsing.
 *)
val annotate_program :
  environment -> Ast_c.toplevel list ->
  (Ast_c.toplevel * environment Common.pair) list

