(* Types and functionality for user input
 * Turns the raw input from the config/interactive into printable strings and
 * information ready to use by the generators.
 *)

(* ------------------------------------------------------------------------- *)
(* CONFIDENCE *)

(* The confidence level of the script. Low, Moderate, or High. *)
type confidence
val conf_tostring : confidence -> string

(* fails if the string is not l, m, h, low, moderate, or high.
 * case insensitive. *)
val conf_fromstring : string -> confidence


(* ------------------------------------------------------------------------- *)
(* USER INPUT *)

(* The user input type. encapsulates the stuff that is specified by the user*)
type t

(* constructor; description and confidence level are required *)
val make : description:string -> confidence:confidence -> t

(* setters *)
val set_keys : string -> t -> t
val set_conf : confidence -> t -> t
val set_comments : string -> t -> t
val set_options : string -> t -> t
val set_url : string -> t -> t
val set_limits : string list -> t -> t
val add_limit : string -> t -> t
val set_authors : string list -> t -> t
val add_author : string -> t -> t
val add_rule :
  ((string * string option) * (* original rule name, new rule name *)
   (string * string list) * (* org message, org metavars *)
   (string * string list)) -> (* rep message, rep metavars *)
  t -> t

(* getters *)

(* check if there's already a rule with that name *)
val check_name : string -> t -> unit
val get_preface : t -> string

(* ordered_rules are all the original */+/- rules from input script, ordered
 * by when they occur in the script. Returns the same rules mapped to the
 * specified user input messages - or generated messages if no user input. *)
val get_rules :
  ordered_rules:string list -> t ->
  ((string * string option) *
   (string * Meta_variable.t list) *
   (string * Meta_variable.t list)) list

(* turns a user input into the config that generates it *)
val unparse : t -> string

(* format string checking *)
val check_format_string : string * string list -> unit
val count_format_vars : string -> int
