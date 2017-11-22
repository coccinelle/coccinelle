# 1 "stdcompat.mlip"
val hypot : float -> float -> float

val copysign : float -> float -> float

val ( |> ) : 'a -> ('a -> 'b) -> 'b

val ( @@ ) : ('a -> 'b) -> 'a -> 'b

val raise_notrace : exn -> 'a
(** @before 4.02.0 equivalent to [raise]. *)

# 13
type bytes = string
# 18
(** @since 4.02.0 alias to [bytes].
    @before 4.02.0 alias to [string]. *)

val print_bytes : bytes -> unit

val prerr_bytes : bytes -> unit

val output_subbytes : out_channel -> bytes -> int -> int -> unit

val output_substring : out_channel -> string -> int -> int -> unit

val really_input_string : in_channel -> int -> string

type ('a, 'b) result
   
# 35
   = Ok of 'a | Error of 'b
(** @since 4.03.0 alias to [Pervasives.result]. *)

val bool_of_string_opt : string -> bool option

val int_of_string_opt : string -> int option

val float_of_string_opt : string -> float option

val read_int_opt : unit -> int option

val read_float_opt : unit -> float option

# 49
type floatarray = float array
# 54
(** @since 4.06.0 alias to [floatarray].
    @before 4.06.0 alias to [float array]. *)

module Arg : sig
  include module type of Arg

  val read_arg : string -> string array

  val read_arg0 : string -> string array

  val write_arg : string -> string array -> unit

  val write_arg0 : string -> string array -> unit
end

module Lazy : sig
  include module type of Lazy

  val from_fun : (unit -> 'a) -> 'a t

  val from_val : 'a -> 'a t
end

module Char : sig
  include module type of Char

  val lowercase_ascii : t -> t

  val uppercase_ascii : t -> t

  val equal : t -> t -> bool
end

module String : sig
  include module type of String

  val init : int -> (int -> char) -> string

  val mapi : (int -> char -> char) -> string -> string

  val iteri : (int -> char -> unit) -> string -> unit

  val map : (char -> char) -> string -> string

  val trim : string -> string

  val lowercase_ascii : string -> string

  val uppercase_ascii : string -> string

  val capitalize_ascii : string -> string

  val uncapitalize_ascii : string -> string

  val equal : t -> t -> bool

  val split_on_char : char -> string -> string list

  val index_opt : string -> char -> int option

  val rindex_opt : string -> char -> int option

  val index_from_opt : string -> int -> char -> int option

  val rindex_from_opt : string -> int -> char -> int option
end

module StringLabels : sig
  include module type of StringLabels

  val init : int -> f:(int -> char) -> string

  val mapi : f:(int -> char -> char) -> string -> string

  val iteri : f:(int -> char -> unit) -> string -> unit

  val map : f:(char -> char) -> string -> string

  val trim : string -> string

  val lowercase_ascii : string -> string

  val uppercase_ascii : string -> string

  val capitalize_ascii : string -> string

  val uncapitalize_ascii : string -> string

  val equal : t -> t -> bool

  val split_on_char : sep:char -> string -> string list

  val index_opt : string -> char -> int option

  val rindex_opt : string -> char -> int option

  val index_from_opt : string -> int -> char -> int option

  val rindex_from_opt : string -> int -> char -> int option
end

module Stack : sig
  include module type of struct
    include Stack
  end

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module Hashtbl : sig
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  val clear : ('a, 'b) t -> unit

  val copy : ('a, 'b) t -> ('a, 'b) t

  val add : ('a, 'b) t -> 'a -> 'b -> unit

  val find : ('a, 'b) t -> 'a -> 'b

  val find_all : ('a, 'b) t -> 'a -> 'b list

  val mem : ('a, 'b) t -> 'a -> bool

  val remove : ('a, 'b) t -> 'a -> unit

  val replace : ('a, 'b) t -> 'a -> 'b -> unit

  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

  val length : ('a, 'b) t -> int

  module type HashedType = Hashtbl.HashedType

  val hash : 'a -> int

  val hash_param : int -> int -> 'a -> int

  val create : ?random:bool -> int -> ('a, 'b) t

  val reset : ('a, 'b) t -> unit
  (** @before 4.00.0 alias to [Hashtbl.clear]. *)

  val randomize : unit -> unit
  (** @before 4.00.0 ignored: does nothing. *)

  type statistics
     
# 206
     = {
      num_bindings : int;
      num_buckets : int;
      max_bucket_length : int;
      bucket_histogram : int array;
    }

  val stats : ('a, 'b) t -> statistics
  (** @before 4.00.0 returns void statistics (only the field [num_bindings] is
      initialized correctly. *)

  val is_randomized : unit -> bool
  (** @before 4.00.0 always false *)

  val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
  (** @before 4.03.0 clear and rebuild the hash table when several values
      have been associated to the same key and the updated values are not the
      ones that are currently associated with the key. *)

  val find_opt : ('a, 'b) t -> 'a -> 'b option

  val seeded_hash : int -> 'a -> int

  val seeded_hash_param : int -> int -> int -> 'a -> int

  module type S = sig
    type 'a t

    type key

    val clear : 'a t -> unit

    val copy : 'a t -> 'a t

    val add : 'a t -> key -> 'a -> unit

    val find : 'a t -> key -> 'a

    val find_all : 'a t -> key -> 'a list

    val mem : 'a t -> key -> bool

    val remove : 'a t -> key -> unit

    val replace : 'a t -> key -> 'a -> unit

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val fold : (key -> 'a -> 'c -> 'c) -> 'a t -> 'c -> 'c

    val length : 'a t -> int

    val create : int -> 'a t

    val reset : 'a t -> unit

    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

    val find_opt : 'a t -> key -> 'a option

    val stats : 'a t -> statistics
  end

  module Make (H : HashedType) : sig
    include module type of struct
      include (Hashtbl.Make (H))
    end

    include S with type 'a t := 'a t and type key := key
  end

  module type SeededHashedType = sig
    type t

    val equal : t -> t -> bool

    val hash : int -> t -> int
  end

  module type SeededS = sig
    include S

    val create : ?random:bool -> int -> 'a t
  end

  module MakeSeeded (H : SeededHashedType) : sig
    
# 293
    include SeededS
  
# 301
  end
  (** before 4.00.0 a non-seeded hash table is created instead: the seed is
      always 0. *)
end

module Set : sig
  module type OrderedType = Set.OrderedType

  module type S = sig
    include Set.S

    val find : elt -> t -> elt

    val of_list : elt list -> t

    val map : (elt -> elt) -> t -> t

    val min_elt_opt : t -> elt option

    val max_elt_opt : t -> elt option

    val choose_opt : t -> elt option

    val find_opt : elt -> t -> elt option

    val find_first : (elt -> bool) -> t -> elt
    (** @before 4.05.0 linear time complexity. *)

    val find_first_opt : (elt -> bool) -> t -> elt option
    (** @before 4.05.0 linear time complexity. *)

    val find_last : (elt -> bool) -> t -> elt
    (** @before 4.05.0 linear time complexity. *)

    val find_last_opt : (elt -> bool) -> t -> elt option
    (** @before 4.05.0 linear time complexity. *)
  end

  module Make (Ord : OrderedType) : sig
    include module type of struct
      include Set.Make (Ord)
    end

    include S with type elt := elt and type t := t
  end
end

module Map : sig
  module type OrderedType = Map.OrderedType

  module type S = sig
    include Map.S

    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

    val find_opt : key -> 'a t -> 'a option

    val min_binding_opt : 'a t -> (key * 'a) option

    val max_binding_opt : 'a t -> (key * 'a) option

    val choose_opt : 'a t -> (key * 'a) option

    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  end

  module Make (Ord : OrderedType) : sig
    include module type of struct
      include Map.Make (Ord)
    end

    include S with type 'a t := 'a t and type key := key
  end
end

module Weak : sig
  type 'a t = 'a Weak.t

  val create : int -> 'a t

  val length : 'a t -> int

  val set : 'a t -> int -> 'a option -> unit

  val get : 'a t -> int -> 'a option

  val get_copy : 'a t -> int -> 'a option

  val check : 'a t -> int -> bool

  val fill : 'a t -> int -> int -> 'a option -> unit

  val blit : 'a t -> int -> 'a t -> int -> int -> unit

  module type S = sig
    include Weak.S

    val find_opt: t -> data -> data option
  end

  module Make (H : Hashtbl.HashedType) : sig
    include module type of struct
      include Weak.Make (H)
    end

    include S with type t := t and type data := data
  end
end

module Sys : sig
  include module type of struct
    include Sys
  end

  val unix : bool

  val win32 : bool

  val cygwin : bool

  val sigbus : int

  val sigpoll : int

  val sigsys : int

  val sigtrap : int

  val sigurg : int

  val sigxcpu : int

  val sigxfsz : int

  
# 436
  type backend_type =
    | Native
    | Bytecode
    | Other of string

  
# 442
  val backend_type : backend_type

  val int_size : int

  val big_endian : bool

  val runtime_variant : unit -> string
  (** @before 4.03.0 returns always the empty string *)

  val runtime_parameters : unit -> string
  (** @before 4.03.0 returns always the empty string *)

  val enable_runtime_warnings : bool -> unit
  (** @before 4.03.0 does nothing *)

  val runtime_warnings_enabled : unit -> bool
  (** @before 4.03.0 returns always false *)

  val getenv_opt : string -> string option
end

module Uchar : sig
  
# 465
  type t
  
# 471
  (** @before 4.03.0 the module [Uchar] is redefined from scratch. *)

  val min : t

  val max : t

  val succ : t -> t

  val pred : t -> t

  val is_valid : int -> bool

  val of_int : int -> t

  val unsafe_of_int : int -> t

  val to_int : t -> int

  val is_char : t -> bool

  val of_char : char -> t

  val to_char : t -> char

  val unsafe_to_char : t -> char

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int

  val bom : t

  val rep : t
end

module Bytes : sig
  
# 510
  include module type of struct
    include String
  end
  
# 518
  (** @before 4.02.0 the module [Bytes] is defined on top of the module
      [String]. *)

  val empty : t

  val of_string : string -> t

  val to_string : t -> string

  val sub_string : t -> int -> int -> string

  val extend : t -> int -> int -> t

  val blit_string : string -> int -> t -> int -> int -> unit

  val cat : t -> t -> t

  val unsafe_of_string : string -> t

  val unsafe_to_string : t -> string

  val uppercase_ascii : t -> t

  val lowercase_ascii : t -> t

  val capitalize_ascii : t -> t

  val uncapitalize_ascii : t -> t

  val equal : t -> t -> bool
end

module BytesLabels : sig
  
# 552
  include module type of struct
    include StringLabels
  end
  
# 560
  (** @before 4.02.0 the module [BytesLabels] is defined on top of the module
      [StringLabels]. *)

  val empty : t

  val of_string : string -> t

  val to_string : t -> string

  val sub_string : t -> pos:int -> len:int -> string

  val extend : t -> left:int -> right:int -> t

  val blit_string :
    src:string -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

  val cat : t -> t -> t

  val unsafe_of_string : string -> t

  val unsafe_to_string : t -> string

  val uppercase_ascii : t -> t

  val lowercase_ascii : t -> t

  val capitalize_ascii : t -> t

  val uncapitalize_ascii : t -> t

  val equal : t -> t -> bool
end

module Buffer : sig
  include module type of struct
    include Buffer
  end

  val to_bytes : t -> bytes

  val truncate : t -> int -> unit
  (** @before 4.05.0 duplicates the prefix kept. *)

  val add_bytes : t -> bytes -> unit

  val add_subbytes : t -> bytes -> int -> int -> unit

  val add_utf_8_uchar : t -> Uchar.t -> unit

  val add_utf_16be_uchar : t -> Uchar.t -> unit

  val add_utf_16le_uchar : t -> Uchar.t -> unit
end

module Stream : sig
  include module type of struct
    include Stream
  end

  val of_bytes : bytes -> char t
end

module Digest : sig
  include module type of struct
    include Digest
  end

  val equal : t -> t -> bool
end

module Nativeint : sig
  include module type of struct
    include Nativeint
  end

  val equal : t -> t -> bool

  val of_string_opt : string -> t option
end

module Int32 : sig
  include module type of struct
    include Int32
  end

  val equal : t -> t -> bool

  val of_string_opt : string -> t option
end

module Int64 : sig
  include module type of struct
    include Int64
  end

  val equal : t -> t -> bool

  val of_string_opt : string -> t option
end

module List : sig
  include module type of struct
    include List
  end

  val iteri : (int -> 'a -> unit) -> 'a list -> unit

  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

  val cons : 'a -> 'a list -> 'a list

  val compare_lengths : 'a list -> 'a list -> int

  val compare_length_with : 'a list -> int -> int

  val nth_opt : 'a list -> int -> 'a option

  val find_opt : ('a -> bool) -> 'a list -> 'a option

  val assoc_opt : 'a -> ('a * 'b) list -> 'b option

  val assq_opt : 'a -> ('a * 'b) list -> 'b option
end

module ListLabels : sig
  include module type of struct
    include ListLabels
  end

  val iteri : f:(int -> 'a -> unit) -> 'a list -> unit

  val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list

  val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list

  val cons : 'a -> 'a list -> 'a list

  val compare_lengths : 'a list -> 'a list -> int

  val compare_length_with : 'a list -> len:int -> int

  val nth_opt : 'a list -> int -> 'a option

  val find_opt : f:('a -> bool) -> 'a list -> 'a option

  val assoc_opt : 'a -> ('a * 'b) list -> 'b option

  val assq_opt : 'a -> ('a * 'b) list -> 'b option
end

module Filename : sig
  include module type of struct
    include Filename
  end

  val extension : string -> string

  val remove_extension : string -> string

  val get_temp_dir_name : unit -> string

  val set_temp_dir_name : string -> unit

  val open_temp_file :
      ?mode : open_flag list -> ?perms : int -> ?temp_dir : string -> string
          -> string -> string * out_channel
end

module Array : sig
  include module type of struct
    include Array
  end

  
# 736
  module Floatarray : sig
    val create : int -> floatarray

    val length : floatarray -> int

    val get : floatarray -> int -> float

    val set : floatarray -> int -> float -> unit

    val unsafe_get : floatarray -> int -> float

    val unsafe_set : floatarray -> int -> float -> unit
  end

  
# 751
  val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit

  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

  val for_all : ('a -> bool) -> 'a array -> bool

  val exists : ('a -> bool) -> 'a array -> bool

  val mem : 'a -> 'a array -> bool

  val memq : 'a -> 'a array -> bool

  val create_float : int -> float array
  (** @before 4.03.0 alias for [Array.make_float]
      @before 4.02.0 implemented as [Array.make len 0]. *)
end

module ArrayLabels : sig
  include module type of struct
    include ArrayLabels
  end

  
# 774
  module Floatarray : module type of Array.Floatarray

  
# 777
  val iter2 : f:('a -> 'b -> unit) -> 'a array -> 'b array -> unit

  val map2 : f:('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

  val for_all : f:('a -> bool) -> 'a array -> bool

  val exists : f:('a -> bool) -> 'a array -> bool

  val mem : 'a -> set:'a array -> bool

  val memq : 'a -> set:'a array -> bool

  val create_float : int -> float array
  (** @before 4.03.0 alias for [Array.make_float]
      @before 4.02.0 implemented as [Array.make len 0]. *)
end
