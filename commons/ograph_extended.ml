open Common

open Ocollection
open Oset
open Oassoc
(* open Ograph *)

open Oassocb
open Osetb

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

type nodei = int

class ['a,'b] ograph_extended =
  let build_assoc () = new oassocb [] in (* opti?: = oassoch *)
  let build_set ()   = new osetb Setb.empty in

  object(o)
    (* inherit ['a] ograph *)

    val free_index = 0

    val succ = build_assoc()
    val pred = build_assoc()
    val nods = build_assoc()

    method add_node (e: 'a) =
      let i = free_index in
      ({<
        nods = nods#add (i, e);
        pred = pred#add (i, build_set() );
        succ = succ#add (i, build_set() );
        free_index = i + 1;
       >}, i)

    method add_nodei i (e: 'a) =
      ({<
        nods = nods#add (i, e);
        pred = pred#add (i, build_set() );
        succ = succ#add (i, build_set() );
        free_index = (max free_index i) + 1;
       >}, i)


    method del_node (i) =
      {<
        (* check: e is effectively the index associated with e,
           and check that already in *)

        (* todo: assert that have no pred and succ, otherwise
         * will have some dangling pointers
         *)
        nods = nods#delkey i;
        pred = pred#delkey i;
        succ = succ#delkey i;
        >}

    method replace_node (i, (e: 'a)) =
      assert (nods#haskey i);
      {<
        nods = nods#replkey (i, e);
       >}

    method add_arc ((a,b),(v: 'b)) =
      {<
        succ = succ#replkey (a, (succ#find a)#add (b, v));
        pred = pred#replkey (b, (pred#find b)#add (a, v));
        >}
    method del_arc ((a,b),v) =
      {<
        succ = succ#replkey (a, (succ#find a)#del (b,v));
        pred = pred#replkey (b, (pred#find b)#del (a,v));
        >}

    method successors   e = succ#find e
    method predecessors e = pred#find e

    method nodes = nods
    method allsuccessors = succ

(*
    method ancestors xs =
      let rec aux xs acc =
        match xs#view with (* could be done with an iter *)
        | Empty -> acc
        | Cons(x, xs) -> (acc#add x)
              +> (fun newacc -> aux (o#predecessors x) newacc)
              +> (fun newacc -> aux xs newacc)
      in aux xs (f2()) (* (new osetb []) *)

    method children  xs =
      let rec aux xs acc =
        match xs#view with (* could be done with an iter *)
        | Empty -> acc
        | Cons(x, xs) -> (acc#add x)
              +> (fun newacc -> aux (o#successors x) newacc)
              +> (fun newacc -> aux xs newacc)
      in aux xs (f2()) (* (new osetb []) *)

    method brothers  x =
      let parents = o#predecessors x in
      (parents#fold (fun acc e -> acc $++$ o#successors e) (f2()))#del x

*)

  end




class ['a,'b] ograph_mutable =
  let build_assoc () = new oassocb [] in
  let build_set ()   = new osetb Setb.empty in

  object(o)

    val mutable free_index = 0

    val mutable succ = build_assoc()
    val mutable pred = build_assoc()
    val mutable nods = build_assoc()

    method add_node (e: 'a) =
      let i = free_index in
      nods <- nods#add (i, e);
      pred <- pred#add (i, build_set() );
      succ <- succ#add (i, build_set() );
      free_index <- i + 1;
      i

    method add_nodei i (e: 'a) =
      nods <- nods#add (i, e);
      pred <- pred#add (i, build_set() );
      succ <- succ#add (i, build_set() );
      free_index <- (max free_index i) + 1;


    method del_node (i) =
        (* check: e is effectively the index associated with e,
           and check that already in *)

        (* todo: assert that have no pred and succ, otherwise
         * will have some dangling pointers
         *)
        nods <- nods#delkey i;
        pred <- pred#delkey i;
        succ <- succ#delkey i;

    method replace_node (i, (e: 'a)) =
      assert (nods#haskey i);
      nods <- nods#replkey (i, e);

    method add_arc ((a,b),(v: 'b)) =
      succ <- succ#replkey (a, (succ#find a)#add (b, v));
      pred <- pred#replkey (b, (pred#find b)#add (a, v));
    method del_arc ((a,b),v) =
      succ <- succ#replkey (a, (succ#find a)#del (b,v));
      pred <- pred#replkey (b, (pred#find b)#del (a,v));

    method successors   e = succ#find e
    method predecessors e = pred#find e

    method nodes = nods
    method allsuccessors = succ

  end


(* depth first search *)
let dfs_iter xi f g =
  let already = Hashtbl.create 101 in
  let rec aux_dfs xs =
    xs +> List.iter (fun xi ->
      if Hashtbl.mem already xi then ()
      else begin
        Hashtbl.add already xi true;
        f xi;
        let succ = g#successors xi in
        aux_dfs (succ#tolist +> List.map fst);
      end
    ) in
  aux_dfs [xi]


let dfs_iter_with_path xi f g =
  let already = Hashtbl.create 101 in
  let rec aux_dfs path xi =
    if Hashtbl.mem already xi then ()
    else begin
      Hashtbl.add already xi true;
      f xi path;
      let succ = g#successors xi in
      let succ' = succ#tolist +> List.map fst in
      succ' +> List.iter (fun yi ->
          aux_dfs (xi::path) yi
      );
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

    let nodes = g#nodes in
    nodes#iter (fun (k,node) ->
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
      pr (sprintf "%d [label=\"%s   [%d]\"%s];\n" k str k color)
    );

    nodes#iter (fun (k,node) ->
      let succ = g#successors k in
      succ#iter (fun (j,edge) ->
        pr (sprintf "%d -> %d;\n" k j);
      );
    );
    pr "}\n" ;
    );
  ()


let generate_ograph_xxx g filename =
  with_open_outfile filename (fun (pr,_) ->
    pr "digraph misc {\n" ;
    pr "size = \"10,10\";\n" ;

    let nodes = g#nodes in
    nodes#iter (fun (k,(node, s)) ->
     (* so can see if nodes without arcs were created *)
      pr (sprintf "%d [label=\"%s   [%d]\"];\n" k s k)
    );

    nodes#iter (fun (k,node) ->
      let succ = g#successors k in
      succ#iter (fun (j,edge) ->
        pr (sprintf "%d -> %d;\n" k j);
      );
    );
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
  if launchgv then launch_gv_cmd filename

let print_ograph_mutable_generic g label fnode ~output_file ~launch_gv =
  generate_ograph_generic g label fnode output_file;
  if launch_gv then launch_gv_cmd output_file
