type ('a, 'b) view = Empty | Cons of 'a * 'b
class virtual ['a] ocollection :
  object ('b)
    method virtual add : 'a -> 'b
    method debug : 'a list
    method virtual del : 'a -> 'b
    method virtual empty : 'b
    method exists : ('a -> bool) -> bool
    method filter : ('a -> bool) -> 'b
    method fold : ('c -> 'a -> 'c) -> 'c -> 'c
    method fromlist : 'a list -> 'b
    method getone : 'a
    method invariant : unit -> unit
    method virtual iter : ('a -> unit) -> unit
    method length : int
    method virtual mem : 'a -> bool
    method misc_op_hook : unit -> 'b
    method virtual null : bool
    method others : 'b
    method string_of : unit -> string
    method tolist : 'a list
    method virtual view : ('a, 'b) view
  end
