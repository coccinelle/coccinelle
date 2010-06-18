type pos = { current_element : string; file: string ; line : string; col : string;
	     line_end : string; col_end : string; }

type param_type = Pos of pos list | Str of string

val fcts : (string, param_type list -> unit) Hashtbl.t

(* ---------------------------------------------------------------------- *)
(* Match management *)

val inc_match : bool ref
val include_match : bool -> unit
