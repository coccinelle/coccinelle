type cocci_predicate = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif
type top_formula = NONDECL of Lib_engine.ctlcocci | CODE of Lib_engine.ctlcocci

val asttoctl :
    Ast_cocci.rule ->
      (Ast_cocci.meta_name list list (* used after *) *
	 Ast_cocci.meta_name list list (* fresh used after *) *
	 Ast_cocci.meta_name list list (* fresh used after seeds *)) ->
      Ast_cocci.meta_name list list (* positions *) ->
      top_formula list

val pp_cocci_predicate : cocci_predicate -> unit

val cocci_predicate_to_string : cocci_predicate -> string
