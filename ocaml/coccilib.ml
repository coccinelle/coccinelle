(* Function table management *)

type pos = { current_element : string;
	     file :string ;
	     line : int;
	     col : int;
	     line_end : int;
	     col_end : int; }

type param_type = Pos of pos list | Str of string

let fcts : (string, param_type list -> unit) Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

(* ---------------------------------------------------------------------- *)
(* Match management *)

let inc_match = ref true
let include_match x = inc_match := x
let dir () = !Flag.dir
