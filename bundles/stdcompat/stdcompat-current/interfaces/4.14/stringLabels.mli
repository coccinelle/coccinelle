type t = string
val make : int -> char -> string
val init : int -> f:(int -> char) -> string
val empty : string
val of_bytes : bytes -> string
val to_bytes : string -> bytes
external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
val concat : sep:string -> string list -> string
val cat : string -> string -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val starts_with : prefix:string -> string -> bool
val ends_with : suffix:string -> string -> bool
val contains_from : string -> int -> char -> bool
val rcontains_from : string -> int -> char -> bool
val contains : string -> char -> bool
val sub : string -> pos:int -> len:int -> string
val split_on_char : sep:char -> string -> string list
val map : f:(char -> char) -> string -> string
val mapi : f:(int -> char -> char) -> string -> string
val fold_left : f:('a -> char -> 'a) -> init:'a -> string -> 'a
val fold_right : f:(char -> 'a -> 'a) -> string -> init:'a -> 'a
val for_all : f:(char -> bool) -> string -> bool
val exists : f:(char -> bool) -> string -> bool
val trim : string -> string
val escaped : string -> string
val uppercase_ascii : string -> string
val lowercase_ascii : string -> string
val capitalize_ascii : string -> string
val uncapitalize_ascii : string -> string
val iter : f:(char -> unit) -> string -> unit
val iteri : f:(int -> char -> unit) -> string -> unit
val index_from : string -> int -> char -> int
val index_from_opt : string -> int -> char -> int option
val rindex_from : string -> int -> char -> int
val rindex_from_opt : string -> int -> char -> int option
val index : string -> char -> int
val index_opt : string -> char -> int option
val rindex : string -> char -> int
val rindex_opt : string -> char -> int option
val to_seq : t -> char Seq.t
val to_seqi : t -> (int * char) Seq.t
val of_seq : char Seq.t -> t
val get_utf_8_uchar : t -> int -> Uchar.utf_decode
val is_valid_utf_8 : t -> bool
val get_utf_16be_uchar : t -> int -> Uchar.utf_decode
val is_valid_utf_16be : t -> bool
val get_utf_16le_uchar : t -> int -> Uchar.utf_decode
val is_valid_utf_16le : t -> bool
external create : int -> bytes = "caml_create_string"
external set : bytes -> int -> char -> unit = "%string_safe_set"
val blit :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
val copy : string -> string
val fill : bytes -> pos:int -> len:int -> char -> unit
val uppercase : string -> string
val lowercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string
val get_uint8 : string -> int -> int
val get_int8 : string -> int -> int
val get_uint16_ne : string -> int -> int
val get_uint16_be : string -> int -> int
val get_uint16_le : string -> int -> int
val get_int16_ne : string -> int -> int
val get_int16_be : string -> int -> int
val get_int16_le : string -> int -> int
val get_int32_ne : string -> int -> int32
val get_int32_be : string -> int -> int32
val get_int32_le : string -> int -> int32
val get_int64_ne : string -> int -> int64
val get_int64_be : string -> int -> int64
val get_int64_le : string -> int -> int64
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit =
    "caml_blit_string"[@@noalloc ]
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_string"[@@noalloc
                                                                    ]
