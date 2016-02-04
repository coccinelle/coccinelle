(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Generates rule headers like
 *
 *      @rulename depends on ...@
 *      metavariable mv;
 *      position p;
 *      @@
 *
 *)

(* ------------------------------------------------------------------------- *)
(* RULE HEADER FUNCTIONS *)

type t

(* Generates rule header. *)
val generate :
 rule_name:string ->
 isos:string list ->
 drop_isos:string list ->
 deps: Ast_cocci.dependency ->
 exists:Ast_cocci.exists ->
 meta_vars: Meta_variable.t list ->
 meta_pos: Meta_variable.t list ->
 t

(* print the full header, metavariables and all *)
val print : out_channel -> t -> unit

(* print only the first line @rulename depends on ...@ *)
val print_declaration : out_channel -> t -> unit
