open Common
open Commonop

open Ocollection
open Oset
open Oassoc
(* open Ograph *)

open Oassocb
open Osetb

(* 
 graph: 
  node: index -> nodevalue
  arc: (index * index) * edgevalue
 how ? matrix ? but no growing array, so lourd.
 when need index ? 
  must have an index, when cant just use nodevalue as key,  cos sometimes may have 2 times same key, but
  it must be 2 different nodes (for instance in program   f(); f();   we want 2 nodes, one per  f(); 
  hence the index. if each node is different, then no problem, can omit index

 todo?: prend en parametre le type de finitemap et set a prendre
 todo?: add_arc doit ramer, car del la key, puis add => better to have a ref to a set 
 opti: graph with pointers
    and a tag visited => need keep global value visited_counter
    check(that node is in, ...), display
 opti: when the graph structure is stable, have a method compact,  that transform
   that in a matrix (assert that all number between 0 and free_index are used,  or do some defrag-like-move/renaming

 invariant: key in pred is also in succ (completness) and value in either assoc is a key also
 
*)

class ['a,'b] ograph_extended =
  let build_assoc () = new oassocb [] in (* opti?: = oassoch *)
  let build_set ()   = new osetb Setb.Empty in
  object(o)
    (* inherit ['a] ograph *)
      
    val free_index = 0

    val succ = build_assoc()
    val pred = build_assoc()
    val nods = build_assoc()

(*    method empty = raise Todo  *)
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


    method del_node (i, (e: 'a)) = 
      {<
        nods = nods#delkey i; (* check: e is effectively the index associated with e, and check that already in *)
        pred = pred#delkey i;
        succ = succ#delkey i;
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
