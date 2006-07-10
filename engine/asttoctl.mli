type cocci_predicate = Lib_engine.predicate * string Ast_ctl.modif

val asttoctl :
    Ast_cocci.top_level list ->
      (cocci_predicate,string,Wrapper_ctl.info) Ast_ctl.generic_ctl list

val pp_cocci_predicate : cocci_predicate -> unit

val cocci_predicate_to_string : cocci_predicate -> string
