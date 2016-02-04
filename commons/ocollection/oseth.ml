open Common

open Oset

(* !!take care!!: this class does side effect, not a pure oassoc *)
class ['a] oseth xs   =
object(o)
  inherit ['a] oset

  val data = Hashtbl.create 100

  (* if put [] then no segfault, if [11] then segfault *)
  method toset = Obj.magic data

  method empty = {< data = Hashtbl.create 100 >}
  method add k =
    Hashtbl.add data k true;
    o

  method iter f = Hashtbl.iter (fun k v -> f k) data
  method view = raise Todo

  method del k =
    Hashtbl.remove data k;
    o
  method mem k =
    try (ignore(Hashtbl.find data k); true)
    with Not_found -> false

  method null =
    try (Hashtbl.iter (fun k v -> raise ReturnExn) data; false)
    with ReturnExn -> true

(* TODO    method length *)

  method union s =
    let v = Hashtbl.create 100 in
    o#iter (fun k -> Hashtbl.add v k true);
    s#iter (fun k -> Hashtbl.add v k true);
    {< data = v >}
  method inter s =
    let v = Hashtbl.create 100 in
    o#iter (fun k -> if s#mem k then Hashtbl.add v k true);
    {< data = v >}
  method minus s =
    let v = Hashtbl.create 100 in
    o#iter (fun k -> if not(s#mem k) then Hashtbl.add v k true);
    {< data = v >}

  (* override default  *)
  method getone =
    let x = ref None in
    try (
      Hashtbl.iter (fun k _ -> x := Some k; raise ReturnExn) data;
      raise Not_found
    )
    with ReturnExn -> some !x


end


