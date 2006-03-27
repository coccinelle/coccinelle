(* TODO invariant succesors/predecessors *)
(*  TODO see c++ library, GTL ... (cf paper from ASTL, cf paper from jfla05 on ocamlgraph) *)
open Common
open Commonop
open Oset

class virtual ['a] ograph =
  object(o: 'o)
    method virtual empty: 'o
    method virtual add_node: 'a -> 'o
    method virtual del_node: 'a -> 'o
    method virtual add_arc: ('a * 'a) -> 'o
    method virtual del_arc: ('a * 'a) -> 'o
    method virtual successors: 'a -> 'a oset
    method virtual predecessors: 'a -> 'a oset
    method virtual nodes: 'a oset
    method virtual ancestors: 'a oset -> 'a oset
    method virtual children: 'a oset -> 'a oset
    method virtual brothers: 'a -> 'a oset

    method debug: ('a * 'a list) list = 
      (o#nodes)#tolist +> map (fun a -> (a, (o#successors a)#tolist))
  end
