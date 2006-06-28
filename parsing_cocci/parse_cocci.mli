val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
      Ast_cocci.rule_with_metavars list

val process_for_ctl :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
      Ast_cocci.rule list

(* val parse_and_merge : string -> Ast_cocci.rule list *)

