(* !!take care!!: this class does side effect, not a pure oassoc *)
class ['a,'b] oassoc_btree : 
  Bdb.db -> 
  string                     (* db name, for profiling *) -> 
  (unit -> Bdb.dbtxn option) (* transaction handler *) ->
  ('b -> 'e) -> ('e -> 'b)   (* marshaller/unmarshaller wrappers *) ->
object('o)
  inherit ['a,'b] Oassoc.oassoc

  (* ocollection concrete instantiation of virtual methods *)
  method empty : 'o
  method add : 'a * 'b -> 'o

  method iter : ('a * 'b -> unit) -> unit
  method view : ('a * 'b, 'o) Ocollection.view

  method del : 'a * 'b -> 'o
  method mem : 'a * 'b -> bool
  method null : bool

  (* oassoc concrete instantiation of virtual methods *)
  method assoc : 'a -> 'b
  method delkey : 'a -> 'o

end
