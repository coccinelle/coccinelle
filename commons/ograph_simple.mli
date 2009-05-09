open Common


(* essentially a convenient way to access a hash and its reverse hash *)

class ['key, 'node, 'edge] ograph_mutable :
object ('o)
  method add_node : 'key -> 'node -> unit
  method del_node : 'key -> unit
  method replace_node: 'key -> 'node -> unit

  method add_arc : ('key * 'key) * 'edge -> unit
  method del_arc : ('key * 'key) * 'edge -> unit

  method nodes : ('key, 'node) Oassoc.oassoc

  method successors : 'key -> ('key * 'edge) Oset.oset
  method predecessors : 'key -> ('key * 'edge) Oset.oset
  method allsuccessors : ('key, ('key * 'edge) Oset.oset) Oassoc.oassoc
end
