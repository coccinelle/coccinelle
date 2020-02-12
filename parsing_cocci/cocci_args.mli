(**
 * [prepend arglist] find command line arguments in the cocci file specified
 * in the [arglist] and place them after the first elemt of [arglist] which
 * is assumed to be the binary name
 *)
val read_args : string list -> string list
