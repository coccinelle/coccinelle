type cocci_predicate = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif
type formula =
    (cocci_predicate,Ast_cocci.meta_name, Wrapper_ctl.info) Ast_ctl.generic_ctl

val asttoctl :
    Ast_cocci.rule -> Ast_cocci.meta_name list list (* used after *) ->
      Ast_cocci.meta_name list list (* positions *) ->
      formula list

val pp_cocci_predicate : cocci_predicate -> unit

val cocci_predicate_to_string : cocci_predicate -> string
