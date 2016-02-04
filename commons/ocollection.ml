open Common

(*****************************************************************************)
(* Collection *)
(*****************************************************************************)
(*
 * The derived classes of collections:
 * - sequence(next, nth): array, list, stack, queue, and mixed
 *   (fast cons, fast snoc, fast append, cf okasaki)
 * - set(union): setl, setb, seti, seth
 * - assoc(find): assocl, mapb, hash, btree, multimap (mais bof, can do
 *   with map of set)
 * - graph: graph1way, graph2way, graphref, graphmatrix?
 *
 * Some features/notes:
 * - views a la wadler to make it cool (I hate get/set).
 * - take list in parameters to be able to construct value as is easily
 * - take the comparaison function in parameters (=> functorial set made cool)
 *   make l [], h [], ... as in perl, and pass the func from pervasive
 *   in oo form (list, ...)
 * - pure/impure: could put 2 interface, with one that show that inpure
 *   by making the operation return unit, but simpler to have one interface.
 * - the core method and default method (via virtual classes)
 *   better to use virtual than typeclass, virtual play both roles:
 *   an interface and default code
 *
 * - pb binary methods: use tosetb tricks, or via (not safe) Obj.magic.
 * - array/list are both a sequence _and_ a dictionnary, so are both
 *   a collection(a) and a collection(i,a) at the same time. But cant do that.
 *   So for array, I see it mainly as an assoc => favor assoc, and
 *   for list, I see it mainly as a collection => favor collection
 *
 * ??mixins: comparable, iterator, via virtual class in ocaml
 * ?? kind of haskell class + default value
 *
 * ?? persistence, caching, peut prendre en param le type de map qu'il cache,
 * comme en perl, evite du marshalling kan wrapped = bdb.
 *
 * ?? lazy wrapper,  how avoid complexity of having to define each time
 * a hashP, hashC, hashL, hashPCL, ... ?
 *
 * ?? I define those classes cos their name are cool, say what is intended to
 * do with
 *
 * todo: cf book on algo, a la rivest/sedgewick
 * todo: recreate collection hierarchy, inspire smalltalk ? haskell ? merd ?
 * todo: put a clean sequence (inherit collection) and make array a special
 * class
 * todo: make ostack (FIFO), oqueue (LIFO)
 *
 *
 * influences: okasaki, merd (pixel), java classes, smalltalk classes
 *)

(*---------------------------------------------------------------------------*)
type ('a, 'b) view = Empty | Cons of 'a * 'b

class virtual ['a] ocollection =
object(o: 'o)
  inherit Objet.objet

  method virtual empty: 'o
  method virtual add: 'a -> 'o

  method virtual iter: ('a -> unit) -> unit
  method virtual view: ('a, 'o) view

  (* no need virtual, but better to redefine for efficiency *)
  method virtual del: 'a -> 'o   (* can do default with: view+iter *)
  method virtual mem: 'a -> bool (* can do default with: mem(tolist) *)
  method virtual null: bool      (* can do default with: lenght(tolist)= 0 *)


  method add2: 'a -> unit = fun a ->
    o#add a +> ignore;
    ()
  method del2: 'a -> unit = fun a ->
    o#del a +> ignore;
    ()
  method clear: unit =
    o#iter (fun e -> o#del2 e);




  method fold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b = fun f a ->
    let a = ref a in
    o#iter (fun e -> a := f !a e);
    !a

  method tolist: 'a list =
    List.rev (o#fold (fun acc e -> e::acc) [])
  method fromlist: 'a list -> 'o =
    fun xs -> xs +> List.fold_left (fun o e -> o#add e) o#empty

  method length: int =
    (* oldsimple: o#tolist +> List.length *)
    (* opti: *)
    let count = ref 0 in
    o#iter (fun e -> incr count);
    !count

  method exists: ('a -> bool) -> bool = fun f ->
    o#tolist +> List.exists f

  method filter: ('a -> bool) -> 'o = fun f ->
    (* iter and call add from empty, or del *)
    o#tolist +> List.filter f +> o#fromlist

  (* forall, fold, map *)

  method getone: 'a =
    match o#view with Cons (e,tl) -> e  | Empty -> failwith "no head"
  method others: 'o =
    match o#view with Cons (e,tl) -> tl | Empty -> failwith "no tail"

end

