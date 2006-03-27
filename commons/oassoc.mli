class virtual ['a, 'b] oassoc :
  object ('c)
    method virtual add : 'a * 'b -> 'c
    method apply : 'a -> ('b -> 'b) -> 'c
    method virtual assoc : 'a -> 'b
    method debug : ('a * 'b) list
    method virtual del : 'a * 'b -> 'c
    method virtual delkey : 'a -> 'c
    method virtual empty : 'c
    method exists : ('a * 'b -> bool) -> bool
    method filter : ('a * 'b -> bool) -> 'c
    method find : 'a -> 'b
    method fold : ('d -> 'a * 'b -> 'd) -> 'd -> 'd
    method fromlist : ('a * 'b) list -> 'c
    method generic : unit
    method getone : 'a * 'b
    method haskey : 'a -> bool
    method invariant : unit -> unit
    method virtual iter : ('a * 'b -> unit) -> unit
    method length : int
    method virtual mem : 'a * 'b -> bool
    method misc_op_hook : unit -> 'c
    method virtual null : bool
    method others : 'c
    method replkey : 'a * 'b -> 'c
    method string_of : unit -> string
    method tolist : ('a * 'b) list
    method virtual view : ('a * 'b, 'c) Ocollection.view
  end
