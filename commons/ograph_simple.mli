(* essentially a convenient way to access a hash and its reverse hash *)

module type S =
  sig
    type key
    type node
    type edge
    type keys (* set of keys *)
    type edges (* sets of (key,edge) pairs *)
    type keynodemap
    type keyedgesmap
    class ograph_mutable :
    object ('o)
      method add_node : key -> node -> unit
      method del_node : key -> unit
      method replace_node : key -> node -> unit


      method add_arc : (key * key) -> edge -> unit
      method del_arc : (key * key) -> edge -> unit

      method nodes : keynodemap

      method successors : key -> edges
      method predecessors : key -> edges
      method allsuccessors : keyedgesmap


      method del_leaf_node_and_its_edges : key -> unit

      method ancestors : key -> keys
      method leaf_nodes : unit -> keys

    end

    val print_ograph_generic:
      str_of_key:(key -> string) ->
      str_of_node:(key -> node -> string) ->
      Common.filename ->
      ograph_mutable ->
      unit
  end

module Make
  (Key : Set.OrderedType)
  (KeySet : Set.S with type elt = Key.t)
  (KeyMap : Map.S with type key = Key.t)
  (Node : Set.OrderedType)
  (Edge : Set.OrderedType)
  (KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t)
  (KeyEdgeSet : Set.S with type elt = KeyEdgePair.t) : S with
  type key = Key.t and
  type node = Node.t and
  type edge = Edge.t and
  type keys = KeySet.t and
  type edges = KeyEdgeSet.t and
  type keynodemap = Node.t KeyMap.t and
  type keyedgesmap = KeyEdgeSet.t KeyMap.t
