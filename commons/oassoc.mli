
class virtual ['a, 'b] oassoc :
object ('o)
  inherit ['a * 'b] Ocollection.ocollection

  method virtual assoc : 'a -> 'b
  method virtual delkey : 'a -> 'o

  method find : 'a -> 'b

  method haskey : 'a -> bool
  method replkey : 'a * 'b -> 'o

  (* better to implement it yourself *)
  method virtual keys: 'a list

  method apply : 'a -> ('b -> 'b) -> 'o
  method apply_with_default : 'a -> ('b -> 'b) -> (unit -> 'b) -> 'o

  (* effect version *)
  method apply_with_default2 : 'a -> ('b -> 'b) -> (unit -> 'b) -> unit

end
