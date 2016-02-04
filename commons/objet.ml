open Common

(* TypeClass via objects. Cf also now interfaces.ml
 *
 * todo? get more inspiration from Java to put fundamental interfaces
 * here ? such as clonable, equaable, showable, debugable, etc
 *)

class virtual objet =
object(o:'o)
  method invariant: unit -> unit = fun () ->
    raise Todo
  (* method check: unit -> unit = fun () ->
    assert(o#invariant());
  *)

  method of_string: string -> unit =
    raise Todo
  method to_string: unit -> string =
    raise Todo
  method debug: unit -> unit =
    raise Todo

  method misc_op_hook: unit -> 'o =
    raise Todo

  method misc_op_hook2: unit =
    ()
end
