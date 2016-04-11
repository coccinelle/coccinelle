(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val process :
    Ast_cocci.meta_name list (* used after *) ->
      (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2) list
      (*inherited env*)->
    (Control_flow_c.G.key *
       (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2) list *
       Lib_engine.predicate) list list ->
	 (int list (*index*) *
	    (Control_flow_c.G.key *
	       (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2)
	       list *
	       Lib_engine.predicate)) list *
	   (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2) list list
