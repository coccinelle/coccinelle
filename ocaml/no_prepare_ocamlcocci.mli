val ocaml_support : bool

exception CompileFailure of string
exception LinkFailure of string
val prepare : 'a -> Ast_cocci.rule list -> 'b option
val prepare_simple : 'a -> 'b
val load_file : 'a -> unit
val clean_file : 'a -> unit
val test : unit -> unit
