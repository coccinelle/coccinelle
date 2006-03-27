open Common
open Commonop
(************************************************************************************)
(* collection: *)
(************************************************************************************)
(*
  sequence(nth): array, list, stack, queue, mixed (fast cons, snoc, append, cf okasaki)
  set(union): setl, setb, seti, seth 
  assoc(find): assocl, mapb, hash, btree, multimap(bof can do with map to set) 
  graph: graph1way, graph2way, graphref

 views a la wadler to make it cool (i hate get/set))
 pb binary methods => tosetb tricks, or via (not safe) Obj.magic
 as in Set.pmi, the core method and default method (via virtual class)
  better to use virtual than typeclass, virtual play both roles (an interface and
  default code)

 todo: cf book on algo, a la rivest/sedgewick 
 todo: recreate collection hierarchy, inspire smalltalk ? haskell ? merd ? 

 take list in parameters (be able to construct value as is)
 take the comparaison function in parameters (=> functorial set made cool)
 make l [], h [], ... as in perl, and pass the func from pervasive in oo form (list, ...)
 mixins: comparable, iterator, via virtual class in ocaml 
  kind of haskell class + default value 

 pure/impure 
  could put 2 interface, with one that show that inpure by making the operation
  return unit, but simpler to have one interface
 persistence, 
 cached (peut prendre en param le type de map qu'il cache, comme en perl
   evite du marshalling kan wrapped = bdb)
  lazy wrapper,  how avoid complexity of having to define each time 
  a hashP, hashC, hashL, hashPCL, ... ?

 array/list is a sequence and a dictionnary
   pb cos a collection(a) and a collection(i,a) at the same time 
   in fact for array, we see it principally as an assoc => favor assoc
   for list, see it principally as collection => favor collection
 => TODO better put a clean sequence (inherit collection) and make array a special class
*)

(* i define those class cos their name are cool, say what is intended to do with *)
(* TODO: make ostack (FIFO), oqueue (LIFO)  *)

(*----------------------------------------------------------------------------------*)
type ('a, 'b) view = Empty | Cons of 'a * 'b

class virtual ['a] ocollection = 
  object(o: 'o)
    method virtual empty: 'o
    method virtual add: 'a -> 'o
    method virtual iter: ('a -> unit) -> unit
    method virtual view: ('a, 'o) view

    (* no need virtual, but better to redefine (efficiency) *)
    method virtual del: 'a -> 'o   (*  with view+iter *)
    method virtual mem: 'a -> bool (*  mem (tolist) *)
    method virtual null: bool      (*  lenght (tolist) = 0 *)

    method fold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b = 
      fun f a -> let a = ref a in (o#iter (fun e -> a := f !a e);!a)

    method tolist: 'a list = 
      List.rev (o#fold (fun acc e -> e::acc) [])
    method fromlist: 'a list -> 'o = 
      fun xs -> xs +> List.fold_left (fun o e -> o#add e) o#empty


    method length: int = 
      o#tolist +> List.length
    method exists: ('a -> bool) -> bool = 
      fun f -> o#tolist +> List.exists f
    method filter: ('a -> bool) -> 'o = 
      (* iter and call add from empty, or del *)
      fun f -> o#tolist +> List.filter f +> o#fromlist

        (* forall, fold, map *)
    method getone: 'a = 
      match o#view with Cons (e,tl) -> e  | _ -> failwith "no head"
    method others: 'o = 
      match o#view with Cons (e,tl) -> tl | _ -> failwith "no tail"

    method debug: 'a list = 
      o#tolist
    method invariant: unit -> unit = 
      raise Todo
    method string_of: unit -> string = 
      raise Todo
    method misc_op_hook: unit -> 'o = 
      raise Todo
      

  end

