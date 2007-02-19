
val enable_typedef  : unit -> unit
val disable_typedef : unit -> unit

val is_enable_state : unit -> bool

val add_ident   : string -> unit
val add_typedef : string -> unit
val add_typedef_root : string -> unit

val new_scope : unit -> unit
val del_scope : unit -> unit

val is_typedef : string -> bool

val lexer_reset_typedef : unit -> unit

type lexer_hint = { 
    mutable parameterDeclaration: bool;
    mutable structDefinition: bool;
    mutable statements: bool;
    mutable toplevel: bool;
  }

val _lexer_hint : lexer_hint ref

val default_hint : unit -> lexer_hint
