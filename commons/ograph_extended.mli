(* graph structure:
 *  - node: index -> nodevalue
 *  - arc: (index * index) * edgevalue
 *
 * How ? matrix ? but no growing array :(
 *
 * When need index ? Must have an index when can't just use the nodevalue
 * as a key, cos sometimes may have 2 times the same key, but it must
 * be 2 different nodes. For instance in a C program 'f(); f();' we want 2
 * nodes, one per 'f();' hence the index. If each node is different, then
 * no problem, can omit index.
 *)

module type S =
  sig
    type key
    type edge
    type keys (* set of keys *)
    type edges (* sets of (key,edge) pairs *)
    type 'node keymap
    type keyedgesmap

    class ['node] ograph_extended :
    object ('o)
      method add_node : 'node -> 'o * key
      method add_nodei : key -> 'node -> 'o * key
      method replace_node : key * 'node -> 'o
      method del_node : key -> 'o

      method add_arc : (key * key) * edge -> 'o
      method del_arc : (key * key) * edge -> 'o

      method nodes : 'node keymap

      method successors : key -> edges
      method predecessors : key -> edges
      method allsuccessors : keyedgesmap
    end

    class ['node] ograph_mutable :
    object ('o)
      method add_node : 'node -> key
      method add_nodei : key -> 'node -> unit
      method replace_node : key * 'node -> unit
      method del_node : key -> unit

      method add_arc : (key * key) * edge -> unit
      method del_arc : (key * key) * edge -> unit

      method nodes : 'node keymap

      method successors : key -> edges
      method predecessors : key -> edges
      method allsuccessors : keyedgesmap
    end

    val dfs_iter : key -> (key -> unit) -> 'node ograph_mutable -> unit

    val dfs_iter_with_path :
      key -> (key -> key list -> unit) -> 'node ograph_mutable -> unit

    val print_ograph_mutable_generic :
      'node ograph_mutable ->
      string option -> (* label for the entire graph *)
      (* what string to print for a node and how to color it *)
      ((key * 'node) -> (string * string option * string option)) ->
      output_file : Common.filename ->
      launch_gv:bool ->
      unit

    val print_ograph_extended :
      ('node * string) ograph_extended ->
      Common.filename (* output file *) ->
      bool (* launch gv ? *) ->
      unit

    val print_ograph_mutable :
      ('node * string) ograph_mutable ->
      Common.filename (* output file *) ->
      bool (* launch gv ? *) ->
      unit
end

module Make
  (Key : Set.OrderedType with type t = int)
  (KeySet : Set.S with type elt = Key.t)
  (KeyMap : Map.S with type key = Key.t)
  (Edge : Set.OrderedType)
  (KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t)
  (KeyEdgeSet : Set.S with type elt = KeyEdgePair.t) : S with
  type key = Key.t and
  type edge = Edge.t and
  type keys = KeySet.t and
  type edges = KeyEdgeSet.t and
  type 'node keymap = 'node KeyMap.t and
  type keyedgesmap = KeyEdgeSet.t KeyMap.t
