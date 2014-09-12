(* Generates rule headers like
 *
 * @rulename depends on ...@
 * metavariable mv;
 * position p;
 * @@
 *
 * Can insert default (aka hardcoded) dependencies.
 * Context: !patch && (context || org || report)
 * Patch: patch && !context && !org && !report
 *)

(* ------------------------------------------------------------------------- *)
(* RULE HEADER FUNCTIONS *)

type t

(* Generates rule header. *)
val generate :
 rulename:string ->
 isos:string list ->
 dropisos:string list ->
 deps: Ast_cocci.dependency ->
 exists:Ast_cocci.exists ->
 meta_vars: Meta_variable.t list ->
 meta_pos: Meta_variable.t list ->
 t

(* Generates header with context/org/report dependency.
 * If context_mode, don't include !patch in the dependencies. *)
val generate_context :
 rulename:string ->
 isos:string list ->
 dropisos:string list ->
 deps: Ast_cocci.dependency ->
 exists:Ast_cocci.exists ->
 meta_vars: Meta_variable.t list ->
 meta_pos: Meta_variable.t list ->
 context_mode:bool ->
 t

(* Generates header with patch dependency. *)
val generate_patch :
 rulename:string ->
 isos:string list ->
 dropisos:string list ->
 deps: Ast_cocci.dependency ->
 exists:Ast_cocci.exists ->
 meta_vars: Meta_variable.t list ->
 meta_pos: Meta_variable.t list ->
 t

(* print the full header, metavariables and all *)
val print : out_channel -> t -> unit

(* print only the first line @rulename depends on ...@ *)
val print_declaration : out_channel -> t -> unit
