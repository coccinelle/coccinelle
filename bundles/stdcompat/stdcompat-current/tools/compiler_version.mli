(* Dealing with compiler versions *)

type extra_prefix = Plus | Tilde

type extra_info = extra_prefix * string

type t

(* Known compiler versions *)

val v3_08_0 : t
val v3_09_0 : t
val v3_10_0 : t
val v3_11_0 : t
val v3_12_0 : t
val v3_12_1 : t
val v4_00_0 : t
val v4_00_1 : t
val v4_01_0 : t
val v4_02_0 : t
val v4_02_1 : t
val v4_02_2 : t
val v4_02_3 : t
val v4_03_0 : t
val v4_04_0 : t
val v4_05_0 : t
val v4_06_0 : t
val v4_06_1 : t
val v4_07_0 : t
val v4_07_1 : t
val v4_08_0 : t
val v4_08_1 : t
val v4_09_0 : t
val v4_09_1 : t
val v4_10_0 : t
val v4_10_1 : t
val v4_10_2 : t
val v4_11_0 : t
val v4_11_1 : t
val v4_11_2 : t
val v4_12_0 : t
val v4_12_1 : t
val v4_13_0 : t
val v4_13_1 : t
val v4_14_0 : t
val v4_14_1 : t
val v4_14_2 : t
val v5_0_0 : t
val v5_1_0 : t
val v5_1_1 : t
val v5_2_0 : t
val v5_2_1 : t
val v5_3_0 : t

val known_versions : t list

val is_known : t -> bool

val is_development : t -> bool

val major : t -> int
val minor : t -> int
val patch_level : t -> int
val extra_info : t -> extra_info option
val string_of_extra_info_opt : extra_info option -> string
val ocaml_of_extra_info_opt : extra_info option -> string

val to_string : t -> string
val of_string : string -> t

val compare : t -> t -> int

val test : unit -> unit
