class virtual ['a] osequence :
  object ('b)
    method virtual add : int * 'a -> 'b
    method apply : int -> ('a -> 'a) -> 'b
    method virtual assoc : int -> 'a
    method debug : (int * 'a) list
    method virtual del : int * 'a -> 'b
    method virtual delkey : int -> 'b
    method virtual empty : 'b
    method exists : (int * 'a -> bool) -> bool
    method filter : (int * 'a -> bool) -> 'b
    method find : int -> 'a
    method virtual first : 'a
    method fold : ('c -> int * 'a -> 'c) -> 'c -> 'c
    method fromlist : (int * 'a) list -> 'b
    method generic : unit
    method getone : int * 'a
    method haskey : int -> bool
    method invariant : unit -> unit
    method virtual iter : (int * 'a -> unit) -> unit
    method virtual last : 'a
    method length : int
    method virtual mem : int * 'a -> bool
    method misc_op_hook : unit -> 'b
    method virtual nth : int -> 'a
    method virtual null : bool
    method others : 'b
    method replkey : int * 'a -> 'b
    method string_of : unit -> string
    method tolist : (int * 'a) list
    method virtual view : (int * 'a, 'b) Ocollection.view
  end
