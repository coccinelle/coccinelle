
type pos = { current_element : string;
	     file :string ;
	     line : int;
	     col : int;
	     line_end : int;
	     col_end : int; }

type param_type = Pos of pos list | Str of string

val fcts : (string, param_type list -> unit) Hashtbl.t

(* ---------------------------------------------------------------------- *)
(* Match management *)

val inc_match : bool ref
val include_match : bool -> unit
val dir : unit -> string
