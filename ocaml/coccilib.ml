(* Function table management *)

type pos = { current_element : string; line : string; col : string;
	     line_end : string; col_end : string; }

type param_type = Pos of pos | Str of string

let fcts : (string, param_type list -> unit) Hashtbl.t =
  Hashtbl.create 10

(* ---------------------------------------------------------------------- *)
(* Match management *)

let inc_match = ref true
let include_match x = inc_match := x
