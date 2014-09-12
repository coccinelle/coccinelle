(* Global variables, common string functions, and helpers. *)

(* ------------------------------------------------------------------------- *)
(* GLOBAL VARIABLES *)

(* global variables are readonly outside initialisation *)
val init :
  rule_name:string -> (* default rule name for nameless rules *)
  pos_name:string -> (* default position name for generated positions *)
  error_msg:string -> (* default error message for org and report mode *)
  char_limit:int -> (* page width limit for generated script *)
  unit

(* default position name *)
val get_pos_name : unit -> string

(* default error message *)
val get_default_message : unit -> string

(* use input rulename to make a name for its context counterpart. *)
val get_context_name : context_mode:bool -> string -> string

(* use input rulename to make a name for its disjunction counterpart. *)
val get_disj_name : string -> string


(* ------------------------------------------------------------------------- *)
(* STRING FUNCTIONS *)

val starts_with_digit : string -> bool

(* splits string into string list of length at most char_limit, delimitering
 * by space, and prefixing each string with prefix. *)
val pre_split : ?prefix:string -> string -> string

(* same as above, except for string options. If None, return "". *)
val pre_split_opt : ?prefix:string -> string option -> string

(* takes in a filename and gives it a new extension. *)
val new_extension : new_ext:string -> string -> string


(* ------------------------------------------------------------------------- *)
(* SANITY CHECKS AND RULE HELPERS *)

(* specific for nameless rule that are automatically given the name
 * "rule starting on line <number>". extracts the number or fails if none. *)
val extract_line : string -> int

(* check rulename for validity. fails if invalid. if strict, fail on rulenames
 * containing spaces. *)
val check_rule : strict:bool -> string -> unit

(* takes a rulename as input and generates a new one if it is invalid. *)
val generate_rule : string -> string option

(* takes existing virtual rulenames, checks them, and returns the standard
 * ones: (patch), context, org, and report. *)
val key_virtuals : string list -> bool -> string list


(* ------------------------------------------------------------------------- *)
(* MISC *)

val get_current_year : unit -> int
