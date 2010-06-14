type pos = { current_element : string; line : string; col : string;
	     line_end : string; col_end : string; }

type param_type = Pos of pos | Str of string

val fcts : (string, param_type list -> unit) Hashtbl.t

(* ---------------------------------------------------------------------- *)
(* Match management *)

val include_match : bool -> unit
