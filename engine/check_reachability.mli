(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type witness =
    (Control_flow_c.G.key, unit,
     (Control_flow_c.G.key, unit, unit) Ast_ctl.generic_ctl list)
      Ast_ctl.generic_witnesstree

type ('a,'b,'c,'d,'e) triples =
    (Control_flow_c.G.key * 'a *
     (Control_flow_c.G.key,
      ('b, ('c,'d) Wrapper_ctl.wrapped_binding) Ast_ctl.generic_subst list, 'e)
     Ast_ctl.generic_witnesstree list) list

val check_reachability :
    string (*rulename*) ->
      ('a,'b,'c,'d,'e) triples -> Control_flow_c.cflow -> unit
