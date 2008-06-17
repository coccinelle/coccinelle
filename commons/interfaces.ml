(*****************************************************************************)
(* TypeClass via module signature.
 *
 * Use this not so much for functors, I hate functors, but
 * more to force me to have consistent naming of stuff. 
 * 
 * It's related to objet.ml in some way, but use a different scheme.
 * 
 * src: (strongly) inspired by Jane Street core lib, which in turn 
 * may have been strongly inspired by Java Interfaces or Haskell
 * TypeClass.
 * 
 * Example of use in .mli:
 * 
 *   open Interfaces
 *   include Stringable with type stringable = t
 *   include Comparable with type comparable = t
 * 
 * Example of use in .ml:
 * 
 *   type xxx
 *   type stringable = xxx
 *   let of_string = bool_of_string
 *   let to_string = string_of_bool
 * 
 *)
open Common.BasicType

(*****************************************************************************)
(* Basic *)
(*****************************************************************************)
(* Normally should not use the '=' of ocaml. Cf common.mli on this *)
module type Eq_able = sig
  type eqable
  val equal : eqable -> eqable -> bool
  (* Jane Street have far more (complex) stuff for this typeclass *)

  val (=*=): eqable -> eqable -> bool
end



(* Same, should not use compare normally, dangerous when evolve code *)
module type Compare_able = sig
  type compareable
  val equal: compareable -> compareable -> bool
end
(* Jane street have also some binable, sexpable *)


module type Num_able = sig
  type numable
  (* +, -, etc *)
end

module type Check_able = sig
  type checkable
  val invariant: checkable -> unit (* raise exception *)
end


(*****************************************************************************)
(* Show/read related *)
(*****************************************************************************)
(* also called show/read in haskell *)
module type String_able = sig
  type stringable
  val of_string : string -> stringable
  val to_string : stringable -> string
end

module type Debug_able = sig
    type debugable
    val debug: debugable -> string
end


module type XML_able = sig
    type xmlable
    val of_xml: string -> xmlable
    val to_xml: xmlable -> string
end
(* Jane street have also some binable, sexpable *)

module type File_able = sig
    type fileable
    val load: filename -> fileable
    val save: fileable -> filename -> unit
end

(*****************************************************************************)
(* Other *)
(*****************************************************************************)

(* This is related to ocollection.ml in some way, but use a different scheme *)

(* Require Constructor class, can not do it *)
module type Map_able = sig
    type 'a mapable
    val map: ('a -> 'b) -> 'a mapable -> 'b mapable
end

module type Iter_able = sig
    type 'a iterable
    val iter: ('a -> unit) -> 'a iterable -> unit
end


(* testable *)

(* *)

(* monad ?*)



(*****************************************************************************)
(* Idea taken from Jane Street Core library, slightly changed.

 * It's another way to organize data structures, module instead of objects. 
 * It's also the Java way. 
 * 
 * It makes some code looks a little bit like Haskell* typeclass.
 * 
 *)

(* In Jane Street they put each interface in its own file but then have to
 * do that:
 * 
 * module type Stringable = Stringable.S
 * module type Comparable = Comparable.S
 * module type Floatable = Floatable.S
 * module type Hashable = Hashable.S
 * module type Infix_comparators = Comparable.Infix
 * module type Monad = Monad.S
 * module type Robustly_comparable = Robustly_comparable.S
 * module type Setable = Setable.S
 * module type Sexpable = Sexpable.S
 * module type Binable = Binable.S
 * 
 * And I dont like having too much files, especially as all those xxable
 * end with able, not start, so don't see them together in the directory.
 *)
