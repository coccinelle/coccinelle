(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

val totex :
  string ->
    Ast_cocci.rule list ->
      (Lib_engine.ctlcocci * 'a) list list ->
	unit
