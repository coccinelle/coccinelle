open Common

open Oassoc

class ['a,'b] oassocb xs =
  object(o)
    inherit ['a,'b] oassoc

    val data = Mapb.empty

    method empty = {< data = Mapb.empty >}
    method add (k,v) = {< data = Mapb.add k v data >}
    method replkey (k,v) = {< data = Mapb.add k v (Mapb.remove k data) >}
    method iter f = Mapb.iter (curry f) data
    method view = raise Todo

    method del (k,v) = {< data = Mapb.remove k data >}
    method mem e = raise Todo
    method null = (Mapb.height data =|= 0)

    method assoc k = Mapb.find k data
    method delkey k = {< data = Mapb.remove k data >}

    method keys =
      List.map fst (o#tolist)
end

