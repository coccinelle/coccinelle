type witness =
    (Ograph_extended.nodei, unit,
     (Ograph_extended.nodei, unit, unit) Ast_ctl.generic_ctl list)
      Ast_ctl.generic_witnesstree

type ('a,'b,'c,'d,'e) triples =
    (Ograph_extended.nodei * 'a *
     (Ograph_extended.nodei,
      ('b, ('c,'d) Wrapper_ctl.wrapped_binding) Ast_ctl.generic_subst list, 'e)
     Ast_ctl.generic_witnesstree list) list

val check_reachability :
    ('a,'b,'c,'d,'e) triples -> Control_flow_c.cflow -> unit
