val pred2c : Lib_engine.predicate -> string * int

val ast0toctl :
    Ast_cocci.top_level list ->
      (Lib_engine.predicate * string Ast_ctl.modif,string)
	Ast_ctl.generic_ctl list
