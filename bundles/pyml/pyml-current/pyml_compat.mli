val lowercase: string -> string

val mapi: (int -> 'a -> 'b) -> 'a list -> 'b list

val lazy_from_fun: (unit -> 'a) -> 'a Lazy.t
