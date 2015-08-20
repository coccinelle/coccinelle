val run :
    (Ast_cocci.script_meta_name * Ast_cocci.meta_name *
       Ast_cocci.metavar) list ->
    Ast_c.metavars_binding (*virts*) ->
    Ast_cocci.meta_name list (*fresh vars*) ->
    string (*rule name*) ->
    string (*code*) ->
    Ast_c.metavar_binding_kind list (* final values of script vars *)
