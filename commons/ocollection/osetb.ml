open Ocollection
open Oset

let empty = Setb.empty

class ['a] osetb xs   =
  object(o)
    inherit ['a] oset

    val data = xs (*  Setb.empty *)
    method tosetb = data

    (* if put [] then no segfault, if [11] then segfault *)
    method toset = Obj.magic data

    method empty = {< data = Setb.empty >}
    method add e = {< data = Setb.add e data >}
    method iter f = Setb.iter f   data
    method view =
      if Setb.is_empty data
      then Empty
      else let el = Setb.choose data in Cons (el, o#del el)

    method del e = {< data = Setb.remove e data >}
    method mem e  = Setb.mem e    data
    method null   = Setb.is_empty data

    method tolist = Setb.elements data
    method length = Setb.cardinal data

    method union s = {< data = Setb.union data s#tosetb >}
    method inter s = {< data = Setb.inter data s#tosetb >}
    method minus s = {< data = Setb.diff  data s#tosetb >}
    (* todo: include, ... *)

  end
