(*
 * graph structure:
 *  -  node: index -> nodevalue
 *  -  arc: (index * index) * edgevalue
 *
 * invariant: key in pred is also in succ (completness) and value in
 * either assoc is a key also.
 *
 * How ? matrix ? but no growing array :(
 *
 * When need index ? Must have an index when can't just use nodevalue
 * as a key, cos sometimes may have 2 times the same key, but it must
 * be 2 different nodes. For instance in program f(); f(); we want 2
 * nodes, one per f(); hence the index. If each node is different,
 * then no problem, can omit index.
 *
 * todo?: prend en parametre le type de finitemap et set a prendre
 * todo?: add_arc doit ramer, car del la key, puis add => better to
 * have a ref to a set.
 *
 * opti: graph with pointers and a tag visited => need keep global value
 * visited_counter.  check(that node is in, ...), display.
 * opti: when the graph structure is stable, have a method compact,  that
 * transforms that in a matrix (assert that all number between 0 and
 * free_index are used,  or do some defrag-like-move/renaming).
 *
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
  type keyedgesmap = KeyEdgeSet.t KeyMap.t =
struct
  type key = Key.t
  type edge = Edge.t
  type keys = KeySet.t
  type edges = KeyEdgeSet.t
  type 'a keymap = 'a KeyMap.t
  type keyedgesmap = KeyEdgeSet.t KeyMap.t

  class ['node] ograph_extended =
  object(o)

    val free_index = 0

    val succ = KeyMap.empty
    val pred = KeyMap.empty
    val nods = (KeyMap.empty : 'node KeyMap.t)

    method add_node n =
      let i = free_index in
      ({<
        nods = KeyMap.add i n nods;
        pred = KeyMap.add i KeyEdgeSet.empty pred;
        succ = KeyMap.add i KeyEdgeSet.empty succ;
        free_index = i + 1;
       >}, i)

    method add_nodei i n =
      ({<
        nods = KeyMap.add i n nods;
        pred = KeyMap.add i KeyEdgeSet.empty pred;
        succ = KeyMap.add i KeyEdgeSet.empty succ;
        free_index = (max free_index i) + 1;
       >}, i)


    method del_node i =
      {<
        (* check: e is effectively the index associated with e,
           and check that already in *)

        (* todo: assert that have no pred and succ, otherwise
         * will have some dangling pointers
         *)
        nods = KeyMap.remove i nods;
        pred = KeyMap.remove i pred;
        succ = KeyMap.remove i succ;
        >}


    method replace_node (i, n) =
      assert (KeyMap.mem i nods);
      {<
        nods = KeyMap.add i n nods;
       >}

    method add_arc ((a,b),(v: Edge.t)) =
      let a_successors = KeyMap.find a succ in
      let new_a_successors = KeyEdgeSet.add (b, v) a_successors in
      let b_predecessors = KeyMap.find b pred in
      let new_b_predecessors = KeyEdgeSet.add (a, v) b_predecessors in
      {<
        succ = KeyMap.add a new_a_successors succ;
        pred = KeyMap.add b new_b_predecessors pred;
        >}
    method del_arc ((a,b),v) =
      let a_successors = KeyMap.find a succ in
      let new_a_successors = KeyEdgeSet.remove (b, v) a_successors in
      let b_predecessors = KeyMap.find b pred in
      let new_b_predecessors = KeyEdgeSet.remove (a, v) b_predecessors in
      {<
        succ = KeyMap.add a new_a_successors succ;
        pred = KeyMap.add b new_b_predecessors pred;
        >}

    method successors e = KeyMap.find e succ
    method predecessors e = KeyMap.find e pred

    method nodes = nods
    method allsuccessors = succ
  end

  class ['node] ograph_mutable =
  object(o)
    val mutable free_index = 0
    val mutable succ = KeyMap.empty
    val mutable pred = KeyMap.empty
    val mutable nods = (KeyMap.empty : 'node KeyMap.t)

    method add_node (n : 'node) =
      let i = free_index in
      nods <- KeyMap.add i n nods;
      pred <- KeyMap.add i KeyEdgeSet.empty pred;
      succ <- KeyMap.add i KeyEdgeSet.empty succ;
      free_index <- i + 1;
      i

    method add_nodei i (n: 'node) =
      nods <- KeyMap.add i n nods;
      pred <- KeyMap.add i KeyEdgeSet.empty pred;
      succ <- KeyMap.add i KeyEdgeSet.empty succ;
      free_index <- (max free_index i) + 1;

    method del_node i =
        (* check: e is effectively the index associated with e,
           and check that already in *)

        (* todo: assert that have no pred and succ, otherwise
         * will have some dangling pointers
         *)
        nods <- KeyMap.remove i nods;
        pred <- KeyMap.remove i pred;
        succ <- KeyMap.remove i succ;

    method replace_node (i, (n: 'node)) =
      assert (KeyMap.mem i nods);
      nods <- KeyMap.add i n nods;

    method add_arc ((a,b),(v: Edge.t)) =
      let a_successors = KeyMap.find a succ in
      let new_a_successors = KeyEdgeSet.add (b, v) a_successors in
      succ <- KeyMap.add a new_a_successors succ;
      let b_predecessors = KeyMap.find b pred in
      let new_b_predecessors = KeyEdgeSet.add (a, v) b_predecessors in
      pred <- KeyMap.add b new_b_predecessors pred;

    method del_arc ((a,b),v) =
      let a_successors = KeyMap.find a succ in
      let new_a_successors = KeyEdgeSet.remove (b, v) a_successors in
      succ <- KeyMap.add a new_a_successors succ;
      let b_predecessors = KeyMap.find b pred in
      let new_b_predecessors = KeyEdgeSet.remove (a, v) b_predecessors in
      pred <- KeyMap.add b new_b_predecessors pred;

    method successors e = KeyMap.find e succ
    method predecessors e = KeyMap.find e pred

    method nodes = nods
    method allsuccessors = succ
  end

  (* depth first search *)
  let dfs_iter xi f g =
    let already = Hashtbl.create 101 in
    let rec aux_dfs xs =
      let g xi =
        if not (Hashtbl.mem already xi)
        then begin
          Hashtbl.add already xi true;
          f xi;
          let f' (key, _) keyset = KeySet.add key keyset in
          let newset = KeyEdgeSet.fold f' (g#successors xi) KeySet.empty in
          aux_dfs newset
        end in
      KeySet.iter g xs in
    aux_dfs (KeySet.singleton xi)

  let dfs_iter_with_path xi f g =
    let already = Hashtbl.create 101 in
    let rec aux_dfs path xi =
      if Hashtbl.mem already xi then ()
      else begin
        Hashtbl.add already xi true;
        f xi path;
        let f' (key, _) keyset = KeySet.add key keyset in
        let newset = KeyEdgeSet.fold f' (g#successors xi) KeySet.empty in
        let g yi = aux_dfs (xi::path) yi in
        KeySet.iter g newset
      end
    in
    aux_dfs [] xi

  let generate_ograph_generic g label fnode filename =
    Common.with_open_outfile filename (fun (pr,_) ->
      pr "digraph misc {\n" ;
      pr "size = \"10,10\";\n" ;
      (match label with
        None -> ()
      | Some x -> pr (Printf.sprintf "label = \"%s\";\n" x));

      let print_node k node =
        let (str,border_color,inner_color) = fnode (k, node) in
        let color =
	  match inner_color with
	    None ->
	      (match border_color with
	        None -> ""
	      | Some x -> Printf.sprintf ", style=\"setlinewidth(3)\", color = %s" x)
	  | Some x ->
	      (match border_color with
	        None -> Printf.sprintf ", style=\"setlinewidth(3),filled\", fillcolor = %s" x
	      | Some x' -> Printf.sprintf ", style=\"setlinewidth(3),filled\", fillcolor = %s, color = %s" x x') in
       (* so can see if nodes without arcs were created *)
        pr (Printf.sprintf "%d [label=\"%s   [%d]\"%s];\n" k str k color) in
      let nodes = g#nodes in
      KeyMap.iter print_node nodes;

      let print_edges k node =
        let print_edge (j, _) = pr (Printf.sprintf "%d -> %d;\n" k j) in
        KeyEdgeSet.iter print_edge (g#successors k) in

      KeyMap.iter print_edges nodes;
      pr "}\n" ;
      );
    ()

  let generate_ograph_xxx g filename =
    Common.with_open_outfile filename (fun (pr,_) ->
      pr "digraph misc {\n" ;
      pr "size = \"10,10\";\n" ;

      let print_node k (node, s) =
        (* so can see if nodes without arcs were created *)
        pr (Printf.sprintf "%d [label=\"%s   [%d]\"];\n" k s k) in
      let nodes = g#nodes in
      KeyMap.iter print_node nodes;

      let print_edges k _ =
        let print_edge (j, _) = pr (Printf.sprintf "%d -> %d;\n" k j) in
        KeyEdgeSet.iter print_edge (g#successors k) in
      KeyMap.iter print_edges nodes;
      pr "}\n" ;
      );
    ()

  let launch_gv_cmd filename =
    let _status =
      Unix.system ("dot " ^ filename ^ " -Tps  -o " ^ filename ^ ".ps;") in
    let _status = Unix.system ("gv " ^ filename ^ ".ps &")
    in
    (* zarb: I need this when I launch the program via eshell, otherwise gv
       do not get the chance to be launched *)
    Unix.sleep 1;
    ()

  let print_ograph_extended g filename launchgv =
    generate_ograph_xxx g filename;
    if launchgv then launch_gv_cmd filename

  let print_ograph_mutable g filename launchgv =
     generate_ograph_xxx g filename;
     if  launchgv then launch_gv_cmd filename

  let print_ograph_mutable_generic g label fnode ~output_file ~launch_gv =
    generate_ograph_generic g label fnode output_file;
    if launch_gv then launch_gv_cmd output_file
end
