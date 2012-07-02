type sexp_bool = bool
type 'a sexp_option = 'a option
type 'a sexp_list = 'a list
type 'a sexp_array = 'a array
type 'a sexp_opaque = 'a
type bigstring = Sexp.bigstring
type float32_vec =
    (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array1.t
type float64_vec =
    (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t
type vec = float64_vec
type float32_mat =
    (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array2.t
type float64_mat =
    (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t
type mat = float64_mat
val default_string_of_float : (float -> string) ref
val read_old_option_format : bool ref
val write_old_option_format : bool ref
val list_map : ('a -> 'b) -> 'a list -> 'b list
val sexp_of_unit : unit -> Sexp.t
val sexp_of_bool : bool -> Sexp.t
val sexp_of_string : string -> Sexp.t
val sexp_of_char : char -> Sexp.t
val sexp_of_int : int -> Sexp.t
val sexp_of_float : float -> Sexp.t
val sexp_of_int32 : int32 -> Sexp.t
val sexp_of_int64 : int64 -> Sexp.t
val sexp_of_nativeint : nativeint -> Sexp.t
val sexp_of_big_int : Big_int.big_int -> Sexp.t
val sexp_of_nat : Nat.nat -> Sexp.t
val sexp_of_num : Num.num -> Sexp.t
val sexp_of_ratio : Ratio.ratio -> Sexp.t
val sexp_of_ref : ('a -> 'b) -> 'a ref -> 'b
val sexp_of_lazy_t : ('a -> 'b) -> 'a Lazy.t -> 'b
val sexp_of_option : ('a -> Sexp.t) -> 'a option -> Sexp.t
val sexp_of_pair : ('a -> Sexp.t) -> ('b -> Sexp.t) -> 'a * 'b -> Sexp.t
val sexp_of_triple :
  ('a -> Sexp.t) ->
  ('b -> Sexp.t) -> ('c -> Sexp.t) -> 'a * 'b * 'c -> Sexp.t
val sexp_of_list : ('a -> Sexp.t) -> 'a list -> Sexp.t
val sexp_of_array : ('a -> Sexp.t) -> 'a array -> Sexp.t
val sexp_of_hashtbl :
  ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) Hashtbl.t -> Sexp.t
val sexp_of_float_vec : (float, 'a, 'b) Bigarray.Array1.t -> Sexp.t
val sexp_of_bigstring : bigstring -> Sexp.t
val sexp_of_float32_vec : float32_vec -> Sexp.t
val sexp_of_float64_vec : float64_vec -> Sexp.t
val sexp_of_vec : vec -> Sexp.t
val sexp_of_float_mat :
  (float, 'a, Bigarray.fortran_layout) Bigarray.Array2.t -> Sexp.t
val sexp_of_float32_mat : float32_mat -> Sexp.t
val sexp_of_float64_mat : float64_mat -> Sexp.t
val sexp_of_mat : mat -> Sexp.t
val sexp_of_opaque : 'a -> Sexp.t
val sexp_of_fun : 'a -> Sexp.t
val string_of__of__sexp_of : ('a -> Sexp.t) -> 'a -> string
module Exn_converter :
  sig
    type t = int64
    module Ids :
      sig
        type key = Int64.t
        type 'a t = 'a Map.Make(Int64).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val max_binding : 'a t -> key * 'a
        val choose : 'a t -> key * 'a
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      end
    val exn_id_cnt : Ids.key ref
    val exn_handlers : (exn -> Sexp.t option) Ids.t ref
    val add_slow : (exn -> Sexp.t option) -> Ids.key
    val del_slow : Ids.key -> unit
    exception Found_sexp_opt of Sexp.t option
    val find_slow : exn -> Sexp.t option
    module Int : sig type t = int val compare : int -> int -> int end
    module Addrs :
      sig
        type key = Int.t
        type 'a t = 'a Map.Make(Int).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val max_binding : 'a t -> key * 'a
        val choose : 'a t -> key * 'a
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      end
    type weak_repr = (Obj.t Weak.t * (exn -> Sexp.t)) Ids.t
    val exn_addr_map :
      (int * (Obj.t Weak.t * (exn -> Sexp.t)) Ids.t) Addrs.t ref
    val get_exn_tag : exn -> Obj.t
    val get_exn_tag_str_addr : Obj.t -> int
    val get_exn_str_addr : exn -> int
    val clean_up_handler : Ids.key -> Obj.t -> unit
    val fast_id_cnt : Ids.key ref
    exception Found_sexp of Sexp.t
    val max_exn_tags : int ref
    val set_max_exn_tags : int -> unit
    val get_max_exn_tags : unit -> int
    val add_auto : ?finalise:bool -> exn -> (exn -> Sexp.t) -> unit
    val find_auto : exn -> Sexp.t option
  end
val sexp_of_exn_opt : exn -> Sexp.t option
val sexp_of_exn : exn -> Sexp.t
val exn_to_string : exn -> string
exception Of_sexp_error of exn * Pre_sexp.t
val record_check_extra_fields : bool ref
val of_sexp_error_exn : exn -> Pre_sexp.t -> 'a
val of_sexp_error : string -> Pre_sexp.t -> 'a
val unit_of_sexp : Sexp.t -> unit
val bool_of_sexp : Sexp.t -> bool
val string_of_sexp : Sexp.t -> string
val char_of_sexp : Sexp.t -> char
val int_of_sexp : Sexp.t -> int
val float_of_sexp : Sexp.t -> float
val int32_of_sexp : Sexp.t -> int32
val int64_of_sexp : Sexp.t -> int64
val nativeint_of_sexp : Sexp.t -> nativeint
val big_int_of_sexp : Sexp.t -> Big_int.big_int
val nat_of_sexp : Sexp.t -> Nat.nat
val num_of_sexp : Sexp.t -> Num.num
val ratio_of_sexp : Sexp.t -> Ratio.ratio
val ref_of_sexp : ('a -> 'b) -> 'a -> 'b ref
val lazy_t_of_sexp : ('a -> 'b) -> 'a -> 'b Lazy.t
val option_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a option
val pair_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> 'a * 'b
val triple_of_sexp :
  (Sexp.t -> 'a) ->
  (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> 'a * 'b * 'c
val list_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a list
val array_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a array
val hashtbl_of_sexp :
  (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) Hashtbl.t
val bigstring_of_sexp :
  Sexp.t ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val float_vec_of_sexp :
  (float, 'a, 'b) Bigarray.Array1.t ->
  (int -> (float, 'a, 'b) Bigarray.Array1.t) ->
  Sexp.t -> (float, 'a, 'b) Bigarray.Array1.t
val create_float32_vec :
  int ->
  (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val create_float64_vec :
  int ->
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val empty_float32_vec :
  (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val empty_float64_vec :
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val float32_vec_of_sexp :
  Sexp.t ->
  (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val float64_vec_of_sexp :
  Sexp.t ->
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val vec_of_sexp :
  Sexp.t ->
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t
val check_too_much_data : Pre_sexp.t -> 'a list -> 'b -> 'b
val float_mat_of_sexp :
  (int -> int -> (float, 'a, Bigarray.fortran_layout) Bigarray.Array2.t) ->
  Sexp.t -> (float, 'a, Bigarray.fortran_layout) Bigarray.Array2.t
val create_float32_mat :
  int ->
  int ->
  (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array2.t
val create_float64_mat :
  int ->
  int ->
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t
val float32_mat_of_sexp :
  Sexp.t ->
  (float, Bigarray.float32_elt, Bigarray.fortran_layout) Bigarray.Array2.t
val float64_mat_of_sexp :
  Sexp.t ->
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t
val mat_of_sexp :
  Sexp.t ->
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t
val opaque_of_sexp : Pre_sexp.t -> 'a
val fun_of_sexp : Pre_sexp.t -> 'a
val of_string__of__of_sexp : (Sexp.t -> 'a) -> string -> 'a
val get_flc_error : string -> string * int * int -> Sexp.t
