
val pp_binding_kind2 : Lib_engine.metavar_binding_kind2 -> unit
val pp_binding2_ctlsubst : 
    (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) 
    Ast_ctl.generic_substitution -> 
    unit
val pp_predicate : Lib_engine.predicate -> unit
val predicate_to_string : Lib_engine.predicate -> string


val pp_ctlcocci : 
  bool (* show_plus *) -> bool (* inline_let *) -> Lib_engine.ctlcocci -> unit

