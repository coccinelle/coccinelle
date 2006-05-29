class virtual ['a] oset :
  object ('b)
    method virtual add : 'a -> 'b
    method cardinal : int
    method debug : 'a list
    method virtual del : 'a -> 'b
    method virtual empty : 'b
    method exists : ('a -> bool) -> bool
    method filter : ('a -> bool) -> 'b
    method fold : ('c -> 'a -> 'c) -> 'c -> 'c
    method fromlist : 'a list -> 'b
    method getone : 'a
    method virtual inter : 'b -> 'b
    method invariant : unit -> unit
    method is_singleton : bool
    method is_subset_of : 'b -> bool
    method is_equal : 'b -> bool
    method virtual iter : ('a -> unit) -> unit
    method length : int
    method virtual mem : 'a -> bool
    method virtual minus : 'b -> 'b
    method misc_op_hook : unit -> 'b
    method virtual null : bool
    method others : 'b
    method string_of : unit -> string
    method tolist : 'a list
    method virtual toset : 'd
    method tosetb : 'a Setb.t
    method toseti : Seti.seti
    method tosetpt : SetPt.t
    method virtual union : 'b -> 'b
    method virtual view : ('a, 'b) Ocollection.view
  end
val ( $??$ ) : 'a -> < mem : 'a -> 'b; .. > -> 'b
val ( $++$ ) : < union : 'a -> 'b; .. > -> 'a -> 'b
val ( $**$ ) : < inter : 'a -> 'b; .. > -> 'a -> 'b
val ( $--$ ) : < minus : 'a -> 'b; .. > -> 'a -> 'b
val ( $<<=$ ) : < is_subset_of : 'a -> 'b; .. > -> 'a -> 'b
val ( $==$ ) : < is_equal : 'a -> 'b; .. > -> 'a -> 'b
val mapo : ('a -> 'b) -> 'b oset -> 'a oset -> 'b oset
