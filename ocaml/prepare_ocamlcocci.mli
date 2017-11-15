val ocaml_support : bool

exception CompileFailure of string
exception LinkFailure of string * string

val prepare : string -> Ast_cocci.rule list -> string option
val prepare_simple : string -> string
val load_file : string -> unit
val clean_file : string -> unit
val test : unit -> unit
