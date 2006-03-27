val member : 'a -> 'a list -> bool

val is_set : 'a list -> bool

val make_set : 'a list -> 'a list

val subset : 'a list -> 'a list -> bool

val subset_fail : 'a list -> 'a list -> 'a option

val set_equal : 'a list -> 'a list -> bool

val intersect : 'a list -> 'a list -> 'a list

val intersect_all : 'a list list -> 'a list

val union : 'a list -> 'a list -> 'a list

val union_list : 'a list -> 'a list

val sorted_union : 'a list -> 'a list -> 'a list

val set2c : string list -> string

val remove : 'a -> 'a list -> 'a list

val subtract : 'a list -> 'a list -> 'a list

val option_filter : ('a -> 'b option) -> 'a list -> 'b list

val option_for_all2 :
    ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list option

val split3 : (('a * 'b * 'c) list) -> ('a list * 'b list * 'c list)

val combine3 : 'a list -> 'b list -> 'c list -> (('a * 'b * 'c) list)

val flat_map : ('a -> 'b list) -> 'a list -> 'b list

val map_union : ('a -> 'b list) -> 'a list -> 'b list

val app_option : ('a -> 'b) -> 'a option -> 'b option

val non_empty_prefixes : 'a list -> 'a list list

val non_empty_suffixes : 'a list -> 'a list list

val split_last : 'a list -> ('a list * 'a)

val partition : 'a list -> int -> 'a list list list

val list_find : string -> ('a -> bool) -> 'a list -> 'a

val partition_one : ('a -> bool) -> 'a list -> ('a * 'a list) option

(* ----------------------------- Stack ----------------------------- *)

val push : 'a -> 'a list ref -> unit
val pop : 'a list ref -> unit
val top : 'a list ref -> 'a
val union_top : 'a list -> 'a list list ref -> unit
