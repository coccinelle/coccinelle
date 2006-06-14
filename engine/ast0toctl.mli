val pred2c : Lib_engine.predicate -> string

val ast0toctl :
    Ast0_cocci.top_level list -> (Lib_engine.full_predicate, string) Ast_ctl.generic_ctl list
