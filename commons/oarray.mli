class ['a] oarray :
  int ->
  'a ->
  object ('b)
    val data : 'a array
    method add : int * 'a -> 'b
    method apply : int -> ('a -> 'a) -> 'b
    method assoc : int -> 'a
    method create : int -> 'a -> 'b
    method debug : (int * 'a) list
    method del : int * 'a -> 'b
    method delkey : int -> 'b
    method empty : 'b
    method exists : (int * 'a -> bool) -> bool
    method filter : (int * 'a -> bool) -> 'b
    method find : int -> 'a
    method first : 'a
    method fold : ('c -> int * 'a -> 'c) -> 'c -> 'c
    method fromlist : (int * 'a) list -> 'b
    method generic : unit
    method getone : int * 'a
    method haskey : int -> bool
    method invariant : unit -> unit
    method iter : (int * 'a -> unit) -> unit
    method last : 'a
    method length : int
    method mem : int * 'a -> bool
    method misc_op_hook : unit -> 'b
    method nth : int -> 'a
    method null : bool
    method others : 'b
    method replkey : int * 'a -> 'b
    method string_of : unit -> string
    method tolist : (int * 'a) list
    method view : (int * 'a, 'b) Ocollection.view
  end
