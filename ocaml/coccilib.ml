(* Function table management *)

type pos = { current_element : string; file :string ; line : string; col : string;
	     line_end : string; col_end : string; }

type param_type = Pos of pos list | Str of string

let fcts : (string, param_type list -> unit) Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

(* ---------------------------------------------------------------------- *)
(* Match management *)

let inc_match = ref true
let include_match x = inc_match := x
