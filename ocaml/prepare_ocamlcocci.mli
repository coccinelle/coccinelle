
exception CompileFailure of string
exception LinkFailure of string

val prepare : string -> Ast_cocci.rule list -> string option (* filename *)
val load_file : string -> unit
val clean_file : string -> unit
val test : unit -> unit
