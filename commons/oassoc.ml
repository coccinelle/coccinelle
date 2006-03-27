open Ocollection

class virtual ['a,'b] oassoc = (* or map or dictionnary *)
  object(o: 'o)
    inherit ['a * 'b] ocollection
    method virtual assoc: 'a -> 'b
    method virtual delkey: 'a -> 'o
    method replkey: ('a * 'b) -> 'o = 
      fun (k,v) -> o#add (k,v) (*  TODO could have a pre that must be in, and for add a pre must not be in *)
    (*    method virtual keys: 'a oset *)
    method find: 'a -> 'b = fun k -> o#assoc k

    method haskey: 'a -> bool = 
      fun k -> (try (ignore(o#assoc k); true) with Not_found -> false)
    method apply: 'a -> ('b -> 'b) -> 'o =
      fun k f -> let old = o#assoc k in o#replkey (k, f old)
    (* apply default, assoc_default, take in class parameters a default value *)

    method generic: unit = ()(*  kind of ugly, but sux to do explicit casting *)
  end
