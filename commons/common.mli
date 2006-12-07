(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

val pr : string -> unit
val pr2 : string -> unit
val pr2gen: 'a -> unit

val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b

val _chan : out_channel ref
val start_log_file : unit -> unit

val verbose_level : int ref

val log : string -> unit
val log2 : string -> unit
val log3 : string -> unit

val pause : unit -> unit

val _trace_var : int ref
val add_var : unit -> unit
val dec_var : unit -> unit
val get_var : unit -> int

val print_n : int -> string -> unit
val printerr_n : int -> string -> unit

val showCodeHex : int list -> unit

val _debug : bool ref
val debugon : unit -> unit
val debugoff : unit -> unit
val debug : (unit -> unit) -> unit

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

val get_mem : unit -> unit
val memory_stat : unit -> string

val timenow : unit -> string

val _count1 : int ref
val _count2 : int ref
val _count3 : int ref
val _count4 : int ref
val _count5 : int ref

val count1 : unit -> unit
val count2 : unit -> unit
val count3 : unit -> unit
val count4 : unit -> unit
val count5 : unit -> unit

val profiling_diagnostic : unit -> string

val time_func : (unit -> 'a) -> 'a

val profile : bool ref
val profile_code : string -> (unit -> 'a) -> 'a

val profile_diagnostic : unit -> unit

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

val example : bool -> unit

val assert_equal : 'a -> 'a -> unit

val _list_bool : (string * bool) list ref

val example2 : string -> bool -> unit

val test_all : unit -> unit

type 'a gen = unit -> 'a

val ig : int gen
val lg : 'a gen -> 'a list gen
val pg : 'a gen -> 'b gen -> ('a * 'b) gen
val polyg : int gen
val ng : string gen

val oneofl : 'a list -> 'a gen
val oneof : 'a gen list -> 'a gen
val always : 'a -> 'a gen
val frequency : (int * 'a gen) list -> 'a gen
val frequencyl : (int * 'a) list -> 'a gen

val laws : string -> ('a -> bool) -> 'a gen -> 'a option

val statistic_number : 'a list -> (int * 'a) list
val statistic : 'a list -> (int * 'a) list
val laws2 :
  string -> ('a -> bool * 'b) -> 'a gen -> 'a option * (int * 'b) list

(*****************************************************************************)
(* Persistence *)
(*****************************************************************************)

val get_value : string -> 'a
val write_value : 'a -> string -> unit
val write_back : ('a -> 'b) -> string -> unit

(*****************************************************************************)
(* Counter *)
(*****************************************************************************)
val _counter : int ref
val counter : unit -> int
val _counter2 : int ref
val counter2 : unit -> int
val _counter3 : int ref
val counter3 : unit -> int

type timestamp = int

(*****************************************************************************)
(* String_of and (pretty) printing *)
(*****************************************************************************)

val string_of_string : (string -> string) -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_unit : unit -> string
val string_of_array : ('a -> string) -> 'a array -> string
val string_of_option : ('a -> string) -> 'a option -> string

val print_bool : bool -> unit
val print_option : ('a -> 'b) -> 'a option -> unit
val print_list : ('a -> 'b) -> 'a list -> unit
val print_between : (unit -> unit) -> ('a -> unit) -> 'a list -> unit

val pp_do_in_box : (unit -> unit) -> unit
val pp_do_in_zero_box : (unit -> unit) -> unit
val pp_init : (unit -> unit) -> unit
val pp : string -> unit

(* convert something printed using format to print into a string *)
val format_to_string : (unit -> unit) (* printer *) -> string

val print_xxxxxxxxxxxxxxxxx : unit -> unit


(*****************************************************************************)
(* Macro *)
(*****************************************************************************)

val macro_expand : string -> unit

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(*-val ( +> ) : 'a -> ('a -> 'b) -> 'b*)
val ( +!> ) : 'a ref -> ('a -> 'a) -> unit
val ( $ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val id : 'a -> 'a

val applyn : int -> ('a -> 'a) -> 'a -> 'a

class ['a] shared_variable_hook :
  'a ->
  object
    val mutable data : 'a
    val mutable registered : (unit -> unit) list
    method get : 'a
    method modify : ('a -> 'a) -> unit
    method register : (unit -> unit) -> unit
    method set : 'a -> unit
  end

val ptFix : ('a -> 'a) -> 'a -> 'a
val ptFixForObjetct : ((< equal : 'a -> bool; .. > as 'a) -> 'a) -> 'a -> 'a

val add_hook : ('a -> ('a -> 'b) -> 'b) ref -> ('a -> ('a -> 'b) -> 'b) -> unit
val add_hook_action : ('a -> unit) ->   ('a -> unit) list ref -> unit
val run_hooks_action : 'a -> ('a -> unit) list ref -> unit

type 'a mylazy = (unit -> 'a)

val save_excursion : 'a ref -> (unit -> 'b) -> 'b

val unwind_protect : (unit -> 'a) -> (exn -> 'b) -> 'a

val memoized : ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)
exception Todo
exception Impossible
exception Here
exception ReturnExn

val internal_error : string -> 'a
val myassert : bool -> unit
val warning : string -> 'a -> 'a
val error_cant_have : 'a -> 'b


(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

val (=|=) : int    -> int    -> bool
val (=<=) : char   -> char   -> bool
val (=$=) : string -> string -> bool
val (=:=) : bool   -> bool   -> bool

val (=*=): 'a -> 'a -> bool

(*
val (=): int -> int -> bool
*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)

val ( ||| ) : 'a -> 'a -> 'a
val ( ==> ) : bool -> bool -> bool
val xor : 'a -> 'a -> bool


(*****************************************************************************)
(* Char *)
(*****************************************************************************)

val string_of_char : char -> string
val is_single : char -> bool
val is_symbol : char -> bool
val is_space : char -> bool
val cbetween : char -> char -> char -> bool
val is_upper : char -> bool
val is_lower : char -> bool
val is_alpha : char -> bool
val is_digit : char -> bool

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

val ( /! ) : int -> int -> int

val do_n : int -> (unit -> unit) -> unit
val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a

val sum_float : float list -> float
val sum_int : int list -> int

val pi : float
val pi2 : float
val pi4 : float

val deg_to_rad : float -> float

val clampf : float -> float

val square : float -> float
val power : int -> int -> int

val between : 'a -> 'a -> 'a -> bool
val between_strict : int -> int -> int -> bool
val bitrange : int -> int -> bool

val prime1 : int -> int option
val prime : int -> int option

val sum : int list -> int
val product : int list -> int

val decompose : int -> int list

val mysquare : int -> int
val sqr : float -> float

type compare = Equal | Inf | Sup
val ( <=> ) : 'a -> 'a -> compare
val ( <==> ) : 'a -> 'a -> int

type uint = int

val int_of_stringchar : string -> int
val int_of_base : string -> int -> int
val int_of_stringbits : string -> int
val int_of_octal : string -> int
val int_of_all : string -> int

val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit

(*****************************************************************************)
(* Numeric/overloading *)
(*****************************************************************************)

type 'a numdict =
    NumDict of
      (('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a))
val add : 'a numdict -> 'a -> 'a -> 'a
val mul : 'a numdict -> 'a -> 'a -> 'a
val div : 'a numdict -> 'a -> 'a -> 'a
val neg : 'a numdict -> 'a -> 'a

val numd_int : int numdict
val numd_float : float numdict

val testd : 'a numdict -> 'a -> 'a

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

val map_fst : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b

val pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b

val snd : 'a * 'b -> 'b
val fst : 'a * 'b -> 'a

val double : 'a -> 'a * 'a
val swap : 'a * 'b -> 'b * 'a

(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c

val just : 'a option -> 'a
val some : 'a option -> 'a

val fmap : ('a -> 'b) -> 'a option -> 'b option
val map_option : ('a -> 'b) -> 'a option -> 'b option
val do_option : ('a -> unit) -> 'a option -> unit

val optionise : (unit -> 'a) -> 'a option

val some_or : 'a option -> 'a -> 'a

val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list

val filter_some : 'a option list -> 'a list
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
val find_some : ('a -> 'b option) -> 'a list -> 'b

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

val slength : string -> int
val concat : string -> string list -> string

val i_to_s : int -> string
val s_to_i : string -> int

val _shareds : (string, string) Hashtbl.t
val shared_string : string -> string

val chop : string -> string

val ( <!!> ) : string -> int * int -> string
val ( <!> ) : string -> int -> char

val split_on_char : char -> string -> string list

val lowercase : string -> string


(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)
(*
-val ( =~ ) : string -> string -> bool
-val ( ==~ ) : string -> Str.regexp -> bool
*)


val regexp_match : string -> string -> string

val matched : int -> string -> string

val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string

val split : string -> string -> string list

val join : string -> string list -> string

val split_list_regexp : string -> string list -> (string * string list) list

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

val dirname : string -> string
val basename : string -> string

type filename = string

val filesuffix : filename -> string
val fileprefix : filename -> filename

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

val int_to_month : int -> string


(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

val list_of_string : string -> char list

val lines : string -> string list
val unlines : string list -> string

val words : string -> string list
val unwords : string list -> string


(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
val cat_orig : string -> string list

val cat : string -> string list

val interpolate : string -> string list

val echo : string -> string

val usleep : int -> unit

val process_output_to_list : string -> string list

val command2 : string -> unit

val do_in_fork : (unit -> unit) -> int

val read_file : string -> string
val write_file : string -> string -> unit

val filesize : string -> int
val lfile_exists : string -> bool

val capsule_unix : ('a -> unit) -> 'a -> unit

val readdir_to_kind_list : string -> Unix.file_kind -> string list
val readdir_to_dir_list : string -> string list
val readdir_to_file_list : string -> string list
val readdir_to_link_list : string -> string list
val readdir_to_dir_size_list : string -> (string * int) list

type rwx = [ `R | `W | `X ] list
val file_perm_of : u:rwx -> g:rwx -> o:rwx -> Unix.file_perm

val has_env : string -> bool

val with_open_outfile : 
     filename -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile : 
      filename -> (in_channel -> 'a) -> 'a

exception Timeout

val timeout_function : int -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* List *)
(*****************************************************************************)

val uncons : 'a list -> 'a * 'a list

val safe_tl : 'a list -> 'a list

val zip : 'a list -> 'b list -> ('a * 'b) list
val zip_safe : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list

val take : int -> 'a list -> 'a list
val take_safe : int -> 'a list -> 'a list
val take_until : ('a -> bool) -> 'a list -> 'a list
val take_while : ('a -> bool) -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list
val drop_while : ('a -> bool) -> 'a list -> 'a list

val span : ('a -> bool) -> 'a list -> 'a list * 'a list

val skip_until : ('a list -> bool) -> 'a list -> 'a list
val skipfirst : 'a -> 'a list -> 'a list

val enum : int -> int -> int list

val index_list : 'a list -> ('a * int) list

val snoc : 'a -> 'a list -> 'a list
val cons : 'a -> 'a list -> 'a list

val head_middle_tail : 'a list -> 'a * 'a list * 'a


val ( ++ ) : 'a list -> 'a list -> 'a list

val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a

val fold_k : ('a -> 'b -> ('a -> 'a) -> 'a) -> ('a -> 'a) -> 'a -> 'b list -> 'a

val list_init : 'a list -> 'a list
val list_last : 'a list -> 'a

val last_n : int -> 'a list -> 'a list
val last : 'a list -> 'a

val join_gen : 'a -> 'a list -> 'a list

val iter_index : ('a -> int -> 'b) -> 'a list -> unit
val map_index : ('a -> int -> 'b) -> 'a list -> 'b list
val filter_index : (int -> 'a -> bool) -> 'a list -> 'a list

val do_withenv :
  (('a -> 'b) -> 'c -> 'd) -> ('e -> 'a -> 'b * 'e) -> 'e -> 'c -> 'd * 'e

val fold_left_with_index : ('a -> 'b -> int -> 'a) -> 'a -> 'b list -> 'a

val map_withenv : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a

val collect_accu : ('a -> 'b list) -> 'b list -> 'a list -> 'b list
val collect : ('a -> 'b list) -> 'a list -> 'b list

val fpartition : ('a -> 'b option) -> 'a list -> 'b list * 'a list

val removelast : 'a list -> 'a list
val remove : 'a -> 'a list -> 'a list

val inits : 'a list -> 'a list list
val tails : 'a list -> 'a list list

val reverse : 'a list -> 'a list
val rev : 'a list -> 'a list

val nth : 'a list -> int -> 'a

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val rev_map : ('a -> 'b) -> 'a list -> 'b list

val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a

val maximum : 'a list -> 'a
val minimum : 'a list -> 'a

val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list

val generate : int -> 'a -> 'a list

val uniq : 'a list -> 'a list

val all_assoc : 'a -> ('a * 'b) list -> 'b list
val prepare_want_all_assoc : ('a * 'b) list -> ('a * 'b list) list

val rotate : 'a list -> 'a list

val or_list : bool list -> bool
val and_list : bool list -> bool

val return_when : ('a -> 'b option) -> 'a list -> 'b

val splitAt : int -> 'a list -> 'a list * 'a list
val pack : int -> 'a list -> 'a list list

val min_with : ('a -> 'b) -> 'a list -> 'a
val two_mins_with : ('a -> 'b) -> 'a list -> 'a * 'a

val grep_with_previous : ('a -> 'a -> bool) -> 'a list -> 'a list
val iter_with_previous : ('a -> 'a -> 'b) -> 'a list -> unit

val get_pair : 'a list -> ('a * 'a) list
val rang : 'a -> 'a list -> int
val doublon : 'a list -> bool

val inseredans : 'a -> 'a list -> 'a list list
val permutation : 'a list -> 'a list list

val map_flatten : ('a -> 'b list) -> 'a list -> 'b list

val repeat : 'a -> int -> 'a list

val map2 : ('a -> 'b) -> 'a list -> 'b list
val map3 : ('a -> 'b) -> 'a list -> 'b list

val pack_sorted : ('a -> 'a -> bool) -> 'a list -> 'a list list

val test : int list list

val uniq2 : 'a list -> 'a list

val keep_best : ('a * 'a -> 'a option) -> 'a list -> 'a list
val sorted_keep_best : ('a -> 'a -> 'a option) -> 'a list -> 'a list

val cartesian_product : 'a list -> 'b list -> ('a * 'b) list

val surEnsemble : 'a list -> 'a list list -> 'a list list
val realCombinaison : 'a list -> 'a list list
val combinaison : 'a list -> ('a * 'a) list
val insere : 'a -> 'a list list -> 'a list list
val insereListeContenant : 'a list -> 'a -> 'a list list -> 'a list list
val fusionneListeContenant : 'a * 'a -> 'a list list -> 'a list list

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)


val array_find_index : ('a -> bool) -> 'a array -> int

(*****************************************************************************)
(* Fast array *)
(*****************************************************************************)

val b_array_string_of_t : 'a -> 'b -> string
val bigarray_string_of_int16_unsigned_elt : 'a -> string
val bigarray_string_of_c_layout : 'a -> string


(*****************************************************************************)
(* Set. Have a look too at set*.mli *)
(*****************************************************************************)

type 'a set = 'a list

val empty_set : 'a set

val insert_set : 'a -> 'a set -> 'a set
val single_set : 'a -> 'a set
val set : 'a list -> 'a set

val exists_set : ('a -> bool) -> 'a set -> bool
val forall_set : ('a -> bool) -> 'a set -> bool

val filter_set : ('a -> bool) -> 'a set -> 'a set
val fold_set : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a
val map_set : ('a -> 'b) -> 'a set -> 'b set

val member_set : 'a -> 'a set -> bool
val find_set : ('a -> bool) -> 'a list -> 'a

val sort_set : ('a -> 'a -> int) -> 'a list -> 'a list

val iter_set : ('a -> unit) -> 'a list -> unit

val top_set : 'a set -> 'a

val inter_set : 'a set -> 'a set -> 'a set
val union_set : 'a set -> 'a set -> 'a set
val minus_set : 'a set -> 'a set -> 'a set

val big_union_set : ('a -> 'b set) -> 'a set -> 'b set
val card_set : 'a set -> int

val include_set : 'a set -> 'a set -> bool
val equal_set : 'a set -> 'a set -> bool
val include_set_strict : 'a set -> 'a set -> bool

val ( $*$ ) : 'a set -> 'a set -> 'a set
val ( $+$ ) : 'a set -> 'a set -> 'a set
val ( $-$ ) : 'a set -> 'a set -> 'a set

val ( $?$ ) : 'a -> 'a set -> bool
val ( $<$ ) : 'a set -> 'a set -> bool
val ( $<=$ ) : 'a set -> 'a set -> bool
val ( $=$ ) : 'a set -> 'a set -> bool

val ( $@$ ) : 'a list -> 'a list -> 'a list

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)

(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)


(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)

type ('a, 'b) assoc = ('a * 'b) list

val assoc_to_function : ('a, 'b) assoc -> ('a -> 'b)

val empty_assoc : ('a, 'b) assoc
val fold_assoc : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val insert_assoc : 'a -> 'a list -> 'a list
val map_assoc : ('a -> 'b) -> 'a list -> 'b list
val filter_assoc : ('a -> bool) -> 'a list -> 'a list

val assoc : 'a -> ('a * 'b) list -> 'b

val keys : ('a * 'b) list -> 'a list
val lookup : 'a -> ('a * 'b) list -> 'b

val del_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val replace_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
val apply_assoc : 'a -> ('b -> 'b) -> ('a * 'b) list -> ('a * 'b) list

val big_union_assoc : ('a -> 'b set) -> 'a list -> 'b set

val assoc_reverse : ('a * 'b) list -> ('b * 'a) list
val assoc_map : ('a * 'b) list -> ('a * 'b) list -> ('a * 'a) list

val lookup_list : 'a -> ('a, 'b) assoc list -> 'b
val lookup_list2 : 'a -> ('a, 'b) assoc list -> 'b * int

(*****************************************************************************)
(* Assoc. Have a look too at Mapb.mli *)
(*****************************************************************************)

module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val intmap_to_list : 'a IntMap.t -> (IntMap.key * 'a) list
val intmap_string_of_t : 'a -> 'b -> string

module IntIntMap :
  sig
    type key = int * int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val intintmap_to_list : 'a IntIntMap.t -> (IntIntMap.key * 'a) list
val intintmap_string_of_t : 'a -> 'b -> string


(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

val hcreate : unit -> ('a, 'b) Hashtbl.t
val hadd : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hmem : 'a -> ('a, 'b) Hashtbl.t -> bool
val hfind : 'a -> ('a, 'b) Hashtbl.t -> 'b
val hreplace : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hiter : ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
val hfold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) Hashtbl.t -> 'c -> 'c
val hremove : 'a -> ('a, 'b) Hashtbl.t -> unit

val find_hash_set : 'a -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> 'b
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t 

val hash_hashset_add : 'a -> 'b -> ('a, 'b hashset) Hashtbl.t -> unit
val hashset_to_set : 
 < fromlist : ('a ) list -> 'c; .. > -> ('a, 'b) Hashtbl.t -> 'c

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

type 'a stack = 'a list
val empty_stack : 'a stack
val push : 'a -> 'a stack -> 'a stack
val top : 'a stack -> 'a
val pop : 'a stack -> 'a stack

val push2 : 'a -> 'a stack ref -> unit
val pop2 : 'a stack ref -> 'a


(*****************************************************************************)
(* Binary tree *)
(*****************************************************************************)
type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree)

(*****************************************************************************)
(* Graph. Have a look too at Ograph_*.mli *)
(*****************************************************************************)

type 'a graph = 'a set * ('a * 'a) set

val add_node : 'a -> 'a graph -> 'a graph
val del_node : 'a -> 'a graph -> 'a graph

val add_arc : 'a * 'a -> 'a graph -> 'a graph
val del_arc : 'a * 'a -> 'a graph -> 'a graph

val successors : 'a -> 'a graph -> 'a set
val predecessors : 'a -> 'a graph -> 'a set

val nodes : 'a graph -> 'a set

val fold_upward : ('a -> 'b -> 'a) -> 'b set -> 'a -> 'b graph -> 'a

val empty_graph : 'a list * 'b list


(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)

val map : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list
val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val member : 'a -> 'a list -> bool

val iter : ('a -> unit) -> 'a list -> unit

val find : ('a -> bool) -> 'a list -> 'a

val exists : ('a -> bool) -> 'a list -> bool
val forall : ('a -> bool) -> 'a list -> bool

val big_union : ('a -> 'b set) -> 'a list -> 'b set

val empty : 'a list

val sort : ('a -> 'a -> int) -> 'a list -> 'a list

val length : 'a list -> int

val null : 'a list -> bool

val head : 'a list -> 'a
val tail : 'a list -> 'a list

val is_singleton : 'a list -> bool


(*****************************************************************************)
(* Geometry (raytracer) *)
(*****************************************************************************)

type vector = float * float * float

type point = vector
type color = vector

val dotproduct : vector * vector -> float

val vector_length : vector -> float

val minus_point : point * point -> vector

val distance : point * point -> float

val normalise : vector -> vector

val mult_coeff : vector -> float -> vector

val add_vector : vector -> vector -> vector
val mult_vector : vector -> vector -> vector
val sum_vector : vector list -> vector


(*****************************************************************************)
(* Pics (raytracer) *)
(*****************************************************************************)
type pixel = int * int * int
val write_ppm : int -> int -> pixel list -> string -> unit
val test1 : unit -> unit

(*****************************************************************************)
(* Diff (lfs) *)
(*****************************************************************************)

type diff = Match | BnotinA | AnotinB
val diff : (int -> int -> diff -> unit) -> string list * string list -> unit
val diff2 : (int -> int -> diff -> unit) -> string * string -> unit

(*****************************************************************************)
(* Parsers (aop-colcombet)                                                 *)
(*****************************************************************************)

val parserCommon : Lexing.lexbuf -> ('a -> Lexing.lexbuf -> 'b) -> 'a -> 'b
val getDoubleParser :
  ('a -> Lexing.lexbuf -> 'b) -> 'a -> (string -> 'b) * (string -> 'b)

(*****************************************************************************)
(* parser related (cocci) *)
(*****************************************************************************)

type pos_file = ((int * int) * int) (* (line * column), charpos) *)

type parse_info = {
    str: string;
    charpos: int;
  } 

val fake_parse_info : parse_info

val full_charpos_to_pos : filename -> (int * int) array

val charpos_to_pos : int -> filename -> (filename * int * int * string)

val error_messagebis : filename -> (string * int (* * int*)) -> int -> string
val error_message : filename -> (string * int (* * int*)) -> string


(*****************************************************************************)
(* Misc/test *)
(*****************************************************************************)

val size_mo_ko : int -> string
val plural : int -> string -> string
val sec_to_days : int -> string
val random_list : 'a list -> 'a
val randomize_list : 'a list -> 'a list
val generic_print : 'a -> string -> string
class ['a] olist :
  'a list ->
  object
    val xs : 'a list
    method fold : ('b -> 'a -> 'b) -> 'b -> 'b
    method view : 'a list
  end
val typing_sux_test : unit -> unit

