(* Difference with ograph_extended ? why not share code ? could, but
 * in ograph_extended we don't force the user to have a key and we
 * generate those keys as he add nodes. Here we assume the user already
 * have an idea of what kind of key he wants to use (a string, a
 * filename, a, int, whatever)
*)

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
  type keyedgesmap = KeyEdgeSet.t KeyMap.t =
struct
  type key = Key.t
  type node = Node.t
  type edge = Edge.t
  type keys = KeySet.t
  type edges = KeyEdgeSet.t
  type keynodemap = node KeyMap.t
  type keyedgesmap = KeyEdgeSet.t KeyMap.t
  class ograph_mutable =
  object(o)
    val mutable succ = KeyMap.empty
    val mutable pred = KeyMap.empty
    val mutable nods = KeyMap.empty

    method add_node key (value : node) =
      nods <- KeyMap.add key value nods;
      pred <- KeyMap.add key KeyEdgeSet.empty pred;
      succ <- KeyMap.add key KeyEdgeSet.empty succ;

    method del_node key =
      (* check: e is effectively the index associated with e,
        and check that already in *)
      (* todo: assert that have no pred and succ, otherwise
       * will have some dangling pointers
       *)
      nods <- KeyMap.remove key nods;
      pred <- KeyMap.remove key pred;
      succ <- KeyMap.remove key succ;

    method private is_leaf_node key = KeyEdgeSet.is_empty (o#successors key)

    method del_leaf_node_and_its_edges key =
      if not (o#is_leaf_node key)
      then failwith "del_leaf_node_and_its_edges has been called on a non-leaf node"
      else begin
        let pred = o#predecessors key in
        let f (k, edge) = o#del_arc (k, key) edge in
        KeyEdgeSet.iter f pred;
        o#del_node key
      end

    method leaf_nodes () =
      let f key _ keyset =
        if o#is_leaf_node key
        then KeySet.add key keyset
        else keyset in
      KeyMap.fold f o#nodes KeySet.empty

    method replace_node key e =
      assert (KeyMap.mem key nods);
      nods <- KeyMap.add key e nods;

    method add_arc (a,b) v =
      let a_successors = KeyMap.find a succ in
      let new_a_successors = KeyEdgeSet.add (b, v) a_successors in
      succ <- KeyMap.add a new_a_successors succ;
      let b_predecessors = KeyMap.find b pred in
      let new_b_predecessors = KeyEdgeSet.add (a, v) b_predecessors in
      pred <- KeyMap.add b new_b_predecessors pred;

    method del_arc (a,b) v =
      let a_successors = KeyMap.find a succ in
      let new_a_successors = KeyEdgeSet.remove (b, v) a_successors in
      succ <- KeyMap.add a new_a_successors succ;
      let b_predecessors = KeyMap.find b pred in
      let new_b_predecessors = KeyEdgeSet.remove (a, v) b_predecessors in
      pred <- KeyMap.add b new_b_predecessors pred;

    method successors key = KeyMap.find key succ
    method predecessors key = KeyMap.find key pred

    method nodes = nods
    method allsuccessors = succ

    method ancestors key =
      let predecessors_keys key0 =
        let f (key, _) keys = KeySet.add key keys in
        KeyEdgeSet.fold f (o#predecessors key0) KeySet.empty in
      let rec aux current_ancestors =
        let f key keyset = KeySet.union keyset (predecessors_keys key) in
        let new_ancestors =
          KeySet.fold f current_ancestors current_ancestors in
        if KeySet.equal new_ancestors current_ancestors
        then current_ancestors
        else aux new_ancestors in
      aux (predecessors_keys key)
  end

  let print_ograph_generic ~str_of_key ~str_of_node filename g =
    let spf = Common.spf in
    Common.with_open_outfile filename (fun (pr,_) ->
      pr "digraph misc {\n" ;
      pr "size = \"10,10\";\n" ;

      let nodes = g#nodes in
      let f (k:key) (node:node) =
        pr (spf "%s [label=\"%s\"];\n" (str_of_key k) (str_of_node k node)) in
      KeyMap.iter f nodes;
      let f2 k node =
        let succ = g#successors k in
        let f3 (j, _) =
          pr (spf "%s -> %s;\n" (str_of_key k) (str_of_key j)) in
        KeyEdgeSet.iter f3 succ in
      KeyMap.iter f2 nodes;
      pr "}\n" ;
      );
    ()
end
