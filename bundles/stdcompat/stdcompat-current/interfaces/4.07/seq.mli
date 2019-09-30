type 'a t = unit -> 'a node
and 'a node =
  | Nil 
  | Cons of 'a * 'a t 
val empty : 'a t
val return : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val flat_map : ('a -> 'b t) -> 'a t -> 'b t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val iter : ('a -> unit) -> 'a t -> unit
