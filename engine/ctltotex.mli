val ctltotex :
    Ast_cocci.rule -> ('pred -> string) -> ('vr -> string) ->
      ('pred,'vr) Ast_ctl.generic_ctl list -> out_channel -> unit

val make_prelude : out_channel -> unit
val make_postlude : out_channel -> unit
