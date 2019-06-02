(*###########################################################################*)
(* Globals *)
(*###########################################################################*)

(* Some conventions:
 *
 * When I have some _xxx variables before some functions, it's
 * because I want to show that those functions internally use a global
 * variable. That does not mean I want people to modify this global.
 * In fact they are kind of private, but I still want to show them.
 * Maybe one day OCaml will have an effect type system so I don't need this.
 *
 * The variables that are called _init_xxx show the internal init
 * side effect of the module (like static var trick used in C/C++)
 *
 * Why not split the functionalities of this file in different files ?
 * Because when I write ocaml script I want simply to load one
 * file, common.ml, and that's it. Cf common_extra.ml for more on this.
 *)


(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
(* see the corresponding section for the use of those flags. See also
 * the "Flags and actions" section at the end of this file.
 *)

(* if set then will not do certain finalize so faster to go back in replay *)
val debugger : bool ref

type prof = PALL | PNONE | PSOME of string list
val profile : prof ref
val show_trace_profile : bool ref


val verbose_level : int ref

(* forbid pr2_once to do the once "optimisation" *)
val disable_pr2_once : bool ref



(* works with new_temp_file *)
val save_tmp_files : bool ref



(*****************************************************************************)
(* Module side effect *)
(*****************************************************************************)
(*
 * I define a few unit tests via some let _ = example (... = ...).
 * I also initialize the random seed, cf _init_random .
 * I also set Gc.stack_size, cf _init_gc_stack .
*)

(*****************************************************************************)
(* Semi globals *)
(*****************************************************************************)
(* cf the _xxx variables in this file *)

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

type filename = string
type dirname = string

(* Trick in case you don't want to do an 'open Common' while still wanting
 * more pervasive types than the one in Pervasives. Just do the selective
 * open Common.BasicType.
 *)
module BasicType : sig
  type filename = string
end

(* Same spirit. Trick found in Jane Street core lib, but originated somewhere
 * else I think: the ability to open nested modules. *)
module Infix : sig
  val ( +> ) : 'a -> ('a -> 'b) -> 'b
  val ( =~ ) : string -> string -> bool
  val ( ==~ ) : string -> Str.regexp -> bool
end


(*
 * Another related trick, found via Jon Harrop to have an extended standard
 * lib is to do something like
 *
 * module List = struct
 *  include List
 *  val map2 : ...
 * end
 *
 * And then can put this "module extension" somewhere to open it.
 *)



(* This module defines the Timeout and UnixExit exceptions.
 * You  have to make sure that those exn are not intercepted. So
 * avoid exn handler such as try (...) with _ -> cos Timeout will not bubble up
 * enough. In such case, add a case before such as
 * with Timeout -> raise Timeout | _ -> ...
 * The same is true for UnixExit (see below).
 *)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

val _tab_level_print: int ref
val indent_do : (unit -> 'a) -> 'a
val reset_pr_indent : unit -> unit

(* The following functions first indent _tab_level_print spaces.
 * They also add the _prefix_pr, for instance used in MPI to show which
 * worker is talking.
 * update: for pr2, it can also print into a log file.
 *
 * The use of 2 in pr2 is because 2 is under UNIX the second descriptor
 * which corresponds to stderr.
 *)
val _prefix_pr : string ref

val pr : string -> unit
val pr_no_nl : string -> unit
val pr_xxxxxxxxxxxxxxxxx : unit -> unit

(* pr2 print on stderr, but can also in addition print into a file *)
val _chan_pr2: out_channel option ref
val print_to_stderr : bool ref
val pr2 : string -> unit
val pr2_no_nl : string -> unit
val pr2_xxxxxxxxxxxxxxxxx : unit -> unit

(* use Dumper.dump *)
val pr2_gen: 'a -> unit
(* val dump: 'a -> string *)

(* see flag: val disable_pr2_once : bool ref *)
val _already_printed : (string, bool) Hashtbl.t
val pr2_once : string -> unit
val clear_pr2_once : unit -> unit

val mk_pr2_wrappers: bool ref -> (string -> unit) * (string -> unit)


val redirect_stdout_opt : filename option -> (unit -> 'a) -> 'a
val redirect_stdout_stderr : filename -> (unit -> unit) -> unit
val redirect_stdin : filename -> (unit -> 'a) -> 'a
val redirect_stdin_opt : filename option -> (unit -> 'a) -> 'a

val with_pr2_to_string: (unit -> unit) -> string list

(* alias *)
val spf : ('a, unit, string) format -> 'a

(* default = stderr *)
val _chan : out_channel ref
(* generate & use a /tmp/debugml-xxx file *)
val start_log_file : unit -> unit

(* see flag: val verbose_level : int ref *)
val log : string -> unit
val log2 : string -> unit
val log3 : string -> unit
val log4 : string -> unit

val if_log  : (unit -> unit) -> unit
val if_log2 : (unit -> unit) -> unit
val if_log3 : (unit -> unit) -> unit
val if_log4 : (unit -> unit) -> unit

val pause : unit -> unit

(* was used by fix_caml *)
val _trace_var : int ref
val add_var : unit -> unit
val dec_var : unit -> unit
val get_var : unit -> int

val print_n : int -> string -> unit
val printerr_n : int -> string -> unit

val _debug : bool ref
val debugon : unit -> unit
val debugoff : unit -> unit
val debug : (unit -> unit) -> unit

(* see flag: val debugger : bool ref *)


(*****************************************************************************)
(* Profiling (cpu/mem) *)
(*****************************************************************************)

(* see flag: type prof = PALL | PNONE | PSOME of string list *)
(* see flag: val profile : prof ref *)

val _profile_table : (string, (float ref * int ref)) Hashtbl.t ref
val profile_code : string -> (unit -> 'a) -> 'a
val profile_diagnostic : unit -> string
val reset_profile : unit -> unit

val profile_code_exclusif : string -> (unit -> 'a) -> 'a
val profile_code_inside_exclusif_ok : string -> (unit -> 'a) -> 'a

val report_if_take_time : int -> string -> (unit -> 'a) -> 'a

(* similar to profile_code but print some information during execution too *)
val profile_code2 : string -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

val example : bool -> unit
(* generate failwith <string> when pb *)
val example2 : string -> bool -> unit
(* use Dumper to report when pb *)
val assert_equal : 'a -> 'a -> unit

val _list_bool : (string * bool) list ref
val example3 : string -> bool -> unit
val test_all : unit -> unit


(* regression testing *)
type score_result = Ok | Pb of string
type score =      (string (* usually a filename *), score_result) Hashtbl.t
type score_list = (string (* usually a filename *) * score_result) list
val empty_score : unit -> score
val load_score : string -> unit -> score
val save_score : score -> string -> unit
val regression_testing :
  score -> filename (* old score file on disk (usually in /tmp) *) -> unit
val regression_testing_vs: score -> score -> score
val total_scores : score -> int (* good *) * int (* total *)
val print_score : score -> unit
val print_total_score: score -> unit


(* quickcheck spirit *)
type 'a gen = unit -> 'a

(* quickcheck random generators *)
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

(* example of use:
 * let b = laws "unit" (fun x -> reverse [x] = [x])    ig
 *)

val statistic_number : 'a list -> (int * 'a) list
val statistic : 'a list -> (int * 'a) list

val laws2 :
  string -> ('a -> bool * 'b) -> 'a gen -> 'a option * (int * 'b) list

(*****************************************************************************)
(* Persistence *)
(*****************************************************************************)

(* just wrappers around Marshal *)
val get_value : filename -> 'a
val read_value : filename -> 'a (* alias *)
val write_value : 'a -> filename -> unit
val write_back : ('a -> 'b) -> filename -> unit

(* wrappers that also use profile_code *)
val marshal__to_string:   'a -> Marshal.extern_flags list -> string
val marshal__from_string:   string -> int -> 'a

(*****************************************************************************)
(* Counter *)
(*****************************************************************************)
val _counter : int ref
val _counter2 : int ref
val _counter3 : int ref

val counter : unit -> int
val counter2 : unit -> int
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

(* use Format internally *)
val pp_do_in_box : (unit -> unit) -> unit
val pp_f_in_box : (unit -> 'a) -> 'a
val pp_do_in_zero_box : (unit -> unit) -> unit
val pp : string -> unit

(* convert something printed using Format to print into a string *)
val format_to_string : (unit -> unit) (* printer *) -> string

(* works with _tab_level_print enabling to mix some calls to pp, pr2
 * and indent_do to sometimes use advanced indentation pretty printing
 * (with the pp* functions) and sometimes explicit and simple indentation
 * printing (with pr* and indent_do) *)
val adjust_pp_with_indent : (unit -> unit) -> unit
val adjust_pp_with_indent_and_header : string -> (unit -> unit) -> unit


val mk_str_func_of_assoc_conv:
  ('a * string) list -> (string -> 'a) * ('a -> string)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

val ( +> ) : 'a -> ('a -> 'b) -> 'b
val ( +!> ) : 'a ref -> ('a -> 'a) -> unit
val ( $ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val id : 'a -> 'a
val do_nothing : unit -> unit

val forever : (unit -> unit) -> unit

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

val fixpoint : ('a -> 'a) -> 'a -> 'a
val fixpoint_for_object : ((< equal : 'a -> bool; .. > as 'a) -> 'a) -> 'a -> 'a

val add_hook : ('a -> ('a -> 'b) -> 'b) ref -> ('a -> ('a -> 'b) -> 'b) -> unit
val add_hook_action : ('a -> unit) ->   ('a -> unit) list ref -> unit
val run_hooks_action : 'a -> ('a -> unit) list ref -> unit

type 'a mylazy = (unit -> 'a)

(* emacs spirit *)
val save_excursion : 'a ref -> (unit -> 'b) -> 'b
val save_excursion_and_disable : bool ref -> (unit -> 'b) -> 'b
val save_excursion_and_enable :  bool ref -> (unit -> 'b) -> 'b

(* emacs spirit *)
val unwind_protect : (unit -> 'a) -> (exn -> 'b) -> 'a

(* java spirit *)
val finalize :       (unit -> 'a) -> (unit -> 'b) -> 'a

val memoized : ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

val cache_in_ref : 'a option ref -> (unit -> 'a) -> 'a


(* take file from which computation is done, an extension, and the function
 * and will compute the function only once and then save result in
 * file ^ extension
 *)
val cache_computation :
  ?verbose:bool -> ?use_cache:bool -> filename  -> string (* extension *) ->
  (unit -> 'a) -> 'a

(* a more robust version where the client describes the dependencies of the
 * computation so it will relaunch the computation in 'f' if needed.
 *)
val cache_computation_robust :
  filename ->
  string (* extension for marshaled object *) ->
  (filename list * 'x) ->
  string (* extension for marshalled dependencies *) ->
  (unit -> 'a) ->
  'a

val cache_computation_robust_in_dir :
  string option (* destination directory *) -> filename ->
  string (* extension for marshalled object *) ->
  (filename list * 'x) ->
  string (* extension for marshaled dependencies *) ->
  (unit -> 'a) ->
  'a



val once : ('a -> unit) -> ('a -> unit)

val before_leaving : ('a -> unit) -> 'a -> 'a

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit


(* cf also the timeout function below that are control related too *)


(*****************************************************************************)
(* Concurrency *)
(*****************************************************************************)

(* how ensure really atomic file creation ? hehe :) *)
exception FileAlreadyLocked
val acquire_file_lock : filename -> unit
val release_file_lock : filename -> unit

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
exception Todo
exception Impossible of int
exception Here
exception ReturnExn

exception Multi_found

exception WrongFormat of string


val internal_error : string -> 'a
val myassert : bool -> unit
val warning : string -> 'a -> 'a
val error_cant_have : 'a -> 'b

val exn_to_s : exn -> string
(* alias *)
val string_of_exn : exn -> string


type evotype = unit
val evoval : evotype

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

val check_stack_size: int -> unit
val check_stack_nbfiles: int -> unit

(* internally common.ml set Gc. parameters *)
val _init_gc_stack : unit

(*****************************************************************************)
(* Arguments and command line *)
(*****************************************************************************)

type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list


type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list


(* A wrapper around Arg modules that have more logical argument order,
 * and returns the remaining args.
 *)
val parse_options :
  cmdline_options -> Arg.usage_msg -> string array -> string list

(* Another wrapper that does Arg.align automatically *)
val usage : Arg.usage_msg -> cmdline_options -> unit



(* Work with the options_with_title type way to organize a long
 * list of command line switches.
 *)
val short_usage :
  Arg.usage_msg -> short_opt:cmdline_options -> unit
val long_usage :
  Arg.usage_msg -> short_opt:cmdline_options -> long_opt:cmdline_sections ->
  unit

(* With the options_with_title way, we don't want the default -help and --help
 * so need adapter of Arg module, not just wrapper.
 *)
val arg_align2 : cmdline_options -> cmdline_options
val arg_parse2 :
  cmdline_options -> Arg.usage_msg -> (unit -> unit) (* short_usage func *) ->
  string list





(* The action lib. Useful to debug supart of your system. cf some of
 * my main.ml for example of use. *)
type flag_spec   = Arg.key * Arg.spec * Arg.doc
type action_spec = Arg.key * Arg.doc * action_func
   and action_func = (string list -> unit)

type cmdline_actions = action_spec list
exception WrongNumberOfArguments

val mk_action_0_arg : (unit -> unit)                       -> action_func
val mk_action_1_arg : (string -> unit)                     -> action_func
val mk_action_2_arg : (string -> string -> unit)           -> action_func
val mk_action_3_arg : (string -> string -> string -> unit) -> action_func

val mk_action_n_arg : (string list -> unit) -> action_func

val options_of_actions:
  string ref (* the action ref *) -> cmdline_actions -> cmdline_options
val action_list:
  cmdline_actions -> Arg.key list
val do_action:
  Arg.key -> string list (* args *) -> cmdline_actions -> unit

(*###########################################################################*)
(* And now basic types *)
(*###########################################################################*)

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
val string_of_chars : char list -> string

val is_single : char -> bool
val is_symbol : char -> bool
val is_space : char -> bool
val is_upper : char -> bool
val is_lower : char -> bool
val is_alpha : char -> bool
val is_digit : char -> bool

val cbetween : char -> char -> char -> bool

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

val ( /! ) : int -> int -> int

val do_n : int -> (unit -> unit) -> unit
val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a

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

(* useful but sometimes when want grep for all places where do modif,
 * easier to have just code using ':=' and '<-' to do some modifications.
 * In the same way avoid using {contents = xxx} to build some ref.
 *)
val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit

val pourcent: int -> int -> int
val pourcent_float: int -> int -> float
val pourcent_float_of_floats: float -> float -> float

val pourcent_good_bad: int -> int -> int
val pourcent_good_bad_float: int -> int -> float

type 'a max_with_elem = int ref * 'a ref
val update_max_with_elem:
  'a max_with_elem -> is_better:(int -> int ref -> bool) -> int * 'a -> unit
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

val numd_int   : int   numdict
val numd_float : float numdict

val testd : 'a numdict -> 'a -> 'a


module ArithFloatInfix : sig
    val (+) : float -> float -> float
    val (-) : float -> float -> float
    val (/) : float -> float -> float
    val ( * ) : float -> float -> float


    val (+..) : int -> int -> int
    val (-..) : int -> int -> int
    val (/..) : int -> int -> int
    val ( *..) : int -> int -> int

    val (+=) : float ref -> float -> unit
end

(*****************************************************************************)
(* Random *)
(*****************************************************************************)

val _init_random : unit
val random_list : 'a list -> 'a
val randomize_list : 'a list -> 'a list
val random_subset_of_list : int -> 'a list -> 'a list


(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

type 'a pair = 'a * 'a
type 'a triple = 'a * 'a * 'a

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

val sndthd : 'a * 'b * 'c -> 'b * 'c

val map_fst : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b

val pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b

val snd : 'a * 'b -> 'b (* alias *)
val fst : 'a * 'b -> 'a (* alias *)

val double : 'a -> 'a * 'a
val swap : 'a * 'b -> 'b * 'a

(* maybe a sign of bad programming if use those functions :) *)
val tuple_of_list1 : 'a list -> 'a
val tuple_of_list2 : 'a list -> 'a * 'a
val tuple_of_list3 : 'a list -> 'a * 'a * 'a
val tuple_of_list4 : 'a list -> 'a * 'a * 'a * 'a
val tuple_of_list5 : 'a list -> 'a * 'a * 'a * 'a * 'a
val tuple_of_list6 : 'a list -> 'a * 'a * 'a * 'a * 'a * 'a

(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c

val just : 'a option -> 'a
val some : 'a option -> 'a (* alias *)

val fmap :       ('a -> 'b) -> 'a option -> 'b option
val map_option : ('a -> 'b) -> 'a option -> 'b option (* alias *)

val equal_option : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool
(**
 * [equal_option eq x y] returns [true] iff
 * either [x = None] and [y = None],
 * or [x = Some x'] and [y = Some y'] and [eq x' y'].
 *)

val default : 'a -> ('b -> 'a) -> 'b option -> 'a
(**
 * [default d f None] returns [d]
 * and [default d f (Some x)] returns [f x]
 *)

val do_option : ('a -> unit) -> 'a option -> unit

val optionise : (unit -> 'a) -> 'a option

val some_or : 'a option -> 'a -> 'a

val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
val partition_either3 :
    ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list

val filter_some : 'a option list -> 'a list
(**
 * [filter_some [None;Some x1; None; Some X2; ... Some xn;None]]
 * Returns [[x1;x2;...;xn]]
 *)

val map_filter : ('a -> 'b option) -> 'a list -> 'b list
(**
 * [map_filter f [x1;...;xn]] returns the list of results obtained
 * by applying f to x1 ... xn, given that not all applications of f
 * may lead to a result. So if [f xi] is None it does not appear in the
 * resulting list, but if it's [Some yi] then [yi] is in the returned list.
 *)

val tail_map_filter : ('a -> 'b option) -> 'a list -> 'b list
val find_some : ('a -> 'b option) -> 'a list -> 'b

val list_to_single_or_exn: 'a list -> 'a

(*****************************************************************************)
(* TriBool *)
(*****************************************************************************)
type bool3 = True3 | False3 | TrueFalsePb3 of string


(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* strings take space in memory. Better when can share the space used by
 * similar strings.
 *)
val _shareds : (string, string) Hashtbl.t
val shared_string : string -> string

val chop : string -> string
val chop_dirsymbol : string -> string

val ( <!!> ) : string -> int * int -> string
val ( <!> ) : string -> int -> char

val take_string: int -> string -> string
val take_string_safe: int -> string -> string

val quote : string -> string

val is_blank_string : string -> bool
val is_string_prefix : string -> string -> bool


val plural : int -> string -> string

val showCodeHex : int list -> unit

val size_mo_ko : int -> string
val size_ko : int -> string

val edit_distance: string -> string -> int

val md5sum_of_string : string -> string

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

val regexp_alpha : Str.regexp
val regexp_int : Str.regexp
val regexp_word : Str.regexp

val _memo_compiled_regexp : (string, Str.regexp) Hashtbl.t
val ( =~ ) : string -> string -> bool
val ( ==~ ) : string -> Str.regexp -> bool



val regexp_match : string -> string -> string

val matched : int -> string -> string

(* not yet politypic functions in ocaml *)
val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string
val matched7 : string -> string * string * string * string * string * string * string

val string_match_substring : Str.regexp -> string -> bool

val split : string (* sep regexp *) -> string -> string list

val split_list_regexp : string -> string list -> (string * string list) list

val all_match : string (* regexp *) -> string -> string list
val global_replace_regexp :
  string (* regexp *) -> (string -> string) -> string -> string

val regular_words: string -> string list
val contain_regular_word: string -> bool

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

(* now at beginning of this file: type filename = string *)

val filesuffix : filename -> string
val fileprefix : filename -> string

val adjust_ext_if_needed : filename -> string -> filename

(* db for dir, base *)
val db_of_filename : filename -> (string * filename)
val filename_of_db : (string * filename) -> filename

(* dbe for dir, base, ext *)
val dbe_of_filename : filename -> string * string * string
val dbe_of_filename_nodot : filename -> string * string * string
(* Left (d,b,e) | Right (d,b)  if file has no extension *)
val dbe_of_filename_safe :
  filename -> (string * string * string,  string * string) either

val filename_of_dbe : string * string * string -> filename

(* ex: replace_ext "toto.c" "c" "var" *)
val replace_ext: filename -> string -> string -> filename

(* remove the ., .. *)
val normalize_path : filename -> filename

val relative_to_absolute : filename -> filename

val is_relative: filename -> bool
val is_absolute: filename -> bool

val filename_without_leading_path : string -> filename -> filename

val join_filename : filename -> filename -> filename
(** like Filename.concat, but simplify . and .. that occur in the second
    argument *)

val resolve_symlink : filename -> filename
(** if the argument is a symbolic link, follow it recursively and
    return the target. Otherwise, return the argument unchanged.
    Symbolic links on the parent directories are not resolved. *)

(*****************************************************************************)
(* i18n *)
(*****************************************************************************)
type langage =
  | English
  | Francais
  | Deutsch

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(* can also use ocamlcalendar, but heavier, use many modules ... *)

type month =
  | Jan  | Feb  | Mar  | Apr  | May  | Jun
  | Jul  | Aug  | Sep  | Oct  | Nov  | Dec
type year = Year of int
type day = Day of int

type date_dmy = DMY of day * month * year

type hour = Hour of int
type minute = Min of int
type second = Sec of int

type time_hms = HMS of hour * minute * second

type full_date = date_dmy * time_hms


(* intervalle *)
type days = Days of int

type time_dmy = TimeDMY of day * month * year


(* from Unix *)
type float_time = float


val mk_date_dmy : int -> int -> int -> date_dmy


val check_date_dmy : date_dmy -> unit
val check_time_dmy : time_dmy -> unit
val check_time_hms : time_hms -> unit

val int_to_month : int -> string
val int_of_month : month -> int
val month_of_string : string -> month
val month_of_string_long : string -> month
val string_of_month : month -> string

val string_of_date_dmy : date_dmy -> string
val string_of_unix_time : ?langage:langage -> Unix.tm -> string
val short_string_of_unix_time : ?langage:langage -> Unix.tm -> string
val string_of_floattime: ?langage:langage -> float_time -> string
val short_string_of_floattime: ?langage:langage -> float_time -> string

val floattime_of_string: string -> float_time

val dmy_to_unixtime: date_dmy -> float_time * Unix.tm
val unixtime_to_dmy: Unix.tm -> date_dmy
val unixtime_to_floattime: Unix.tm -> float_time
val floattime_to_unixtime: float_time -> Unix.tm

val sec_to_days : int -> string
val sec_to_hours : int -> string

val today : unit -> float_time
val yesterday : unit -> float_time
val tomorrow : unit -> float_time

val lastweek : unit -> float_time
val lastmonth : unit -> float_time

val week_before: float_time -> float_time
val month_before: float_time -> float_time
val week_after: float_time -> float_time

val days_in_week_of_day : float_time -> float_time list

val first_day_in_week_of_day : float_time -> float_time
val last_day_in_week_of_day : float_time -> float_time

val day_secs: float_time

val rough_days_since_jesus : date_dmy -> days
val rough_days_between_dates : date_dmy -> date_dmy -> days

val string_of_unix_time_lfs : Unix.tm -> string

val is_more_recent : date_dmy -> date_dmy -> bool
val max_dmy : date_dmy -> date_dmy -> date_dmy
val min_dmy : date_dmy -> date_dmy -> date_dmy
val maximum_dmy : date_dmy list -> date_dmy
val minimum_dmy : date_dmy list -> date_dmy

val this_year : unit -> int

(*****************************************************************************)
(* Lines/Words/Strings *)
(*****************************************************************************)

val list_of_string : string -> char list

val lines : string -> string list
val unlines : string list -> string

val words : string -> string list
val unwords : string list -> string

val split_space : string -> string list

val lines_with_nl : string -> string list

val nblines : string -> int

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
val cat :      filename -> string list
val cat_orig : filename -> string list
val cat_array: filename -> string array

val uncat: string list -> filename -> unit

val interpolate : string -> string list

val echo : string -> string

val process_output_to_list : string -> string list
val cmd_to_list :            string -> string list (* alias *)
val cmd_to_list_and_status : string -> string list * Unix.process_status

val file_to_stdout : string -> unit
val file_to_stderr : string -> unit

val command2 : string -> unit
val _batch_mode: bool ref
val command2_y_or_no : string -> bool
val command2_y_or_no_exit_if_no : string -> unit

val do_in_fork : (unit -> unit) -> int

val mkdir: ?mode:Unix.file_perm -> string -> unit

val write_file : file:filename -> string -> unit

val filesize : filename -> int
val filemtime : filename -> float

val nblines_file : filename -> int

val lfile_exists : filename -> bool
val is_directory : filename -> bool

val capsule_unix : ('a -> unit) -> 'a -> unit

val readdir_to_kind_list : string -> Unix.file_kind -> string list
val readdir_to_dir_list : string -> string list
val readdir_to_file_list : string -> string list
val readdir_to_link_list : string -> string list
val readdir_to_dir_size_list : string -> (string * int) list

val glob : string -> filename list
val files_of_dir_or_files :
  string (* ext *) -> string list -> filename list
val files_of_dir_or_files_no_vcs :
  string (* ext *) -> string list -> filename list
(* use a post filter =~ for the ext filtering *)
val files_of_dir_or_files_no_vcs_post_filter :
  string (* regexp *) -> string list -> filename list


val sanity_check_files_and_adjust :
  string (* ext *) -> string list -> filename list


type rwx = [ `R | `W | `X ] list
val file_perm_of : u:rwx -> g:rwx -> o:rwx -> Unix.file_perm

val has_env : string -> bool

(* scheme spirit. do a finalize so no leak. *)
val with_open_outfile :
  filename -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile :
  filename -> (in_channel -> 'a) -> 'a
val with_open_outfile_append :
  filename -> ((string -> unit) * out_channel -> 'a) -> 'a

val with_open_stringbuf :
  (((string -> unit) * Buffer.t) -> unit) -> string

exception Timeout

(* subtil: have to make sure that Timeout is not intercepted before here. So
 * avoid exn handler such as try (...) with _ -> cos Timeout will not bubble up
 * enough. In such case, add a case before such as
 * with Timeout -> raise Timeout | _ -> ...
 *
 * The same is true for UnixExit (see below).
 *)
val timeout_function : string -> int -> (unit -> 'a) -> 'a

val timeout_function_opt : string -> int option -> (unit -> 'a) -> 'a

val remove_file : string -> unit

(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
 *)
val _temp_files_created : string list ref
(* see flag: val save_tmp_files : bool ref *)
val temp_files : string ref
val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename
val erase_temp_files : unit -> unit
val erase_this_temp_file : filename -> unit

(* If the user use some exit 0 in his code, then no one can intercept this
 * exit and do something before exiting. There is exn handler for exit 0
 * so better never use exit 0 but instead use an exception and just at
 * the very toplevel transform this exn in a unix exit code.
 *
 * subtil: same problem than with Timeout. Do not intercept such exception
 * with some blind try (...) with _ -> ...
 *)
exception UnixExit of int
val exn_to_real_unixexit : (unit -> 'a) -> 'a








(*###########################################################################*)
(* And now collection-like types. See also ocollection.mli *)
(*###########################################################################*)

(*****************************************************************************)
(* List *)
(*****************************************************************************)


(* tail recursive efficient map (but that also reverse the element!) *)
val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list
(* tail recursive efficient map, use accumulator  *)
val acc_map : ('a -> 'b) -> 'a list -> 'b list


val zip : 'a list -> 'b list -> ('a * 'b) list
val combine4 : 'a list -> 'b list -> 'c list -> 'd list ->
                  ('a * 'b * 'c * 'd) list
val zip_safe : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list

val take : int -> 'a list -> 'a list
val take_safe : int -> 'a list -> 'a list
val take_until : ('a -> bool) -> 'a list -> 'a list
val take_while : ('a -> bool) -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list
val drop_while : ('a -> bool) -> 'a list -> 'a list
val drop_until : ('a -> bool) -> 'a list -> 'a list

val span : ('a -> bool) -> 'a list -> 'a list * 'a list

val skip_until : ('a list -> bool) -> 'a list -> 'a list
val skipfirst : (* Eq a *) 'a -> 'a list -> 'a list

(* cf also List.partition *)
val fpartition : ('a -> 'b option) -> 'a list -> 'b list * 'a list

val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list
val exclude_but_keep_attached: ('a -> bool) -> 'a list -> ('a * 'a list) list
val group_by_post: ('a -> bool) -> 'a list -> ('a list * 'a) list *    'a list
val group_by_pre:  ('a -> bool) -> 'a list -> 'a list *    ('a * 'a list) list
val group_by_mapped_key: ('a -> 'b) -> 'a list -> ('b * 'a list) list

(* Use hash internally to not be in O(n2). If you want to use it on a
 * simple list, then first do a List.map to generate a key, for instance the
 * first char of the element, and then use this function.
 *)
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list

val splitAt : int -> 'a list -> 'a list * 'a list

val split_when: ('a -> bool) -> 'a list -> 'a list * 'a * 'a list
val split_gen_when: ('a list -> 'a list option) -> 'a list -> 'a list list

val pack : int -> 'a list -> 'a list list


val enum : int -> int -> int list
val repeat : 'a -> int -> 'a list
val generate : int -> 'a -> 'a list



val index_list   : 'a list -> ('a * int) list
val index_list_1 : 'a list -> ('a * int) list
val index_list_and_total   : 'a list -> ('a * int * int) list

val iter_index : ('a -> int -> 'b) -> 'a list -> unit
val map_index : ('a -> int -> 'b) -> 'a list -> 'b list
val filter_index : (int -> 'a -> bool) -> 'a list -> 'a list
val fold_left_with_index : ('a -> 'b -> int -> 'a) -> 'a -> 'b list -> 'a

val nth : 'a list -> int -> 'a
val rang : (* Eq a *) 'a -> 'a list -> int

val last_n : int -> 'a list -> 'a list



val snoc : 'a -> 'a list -> 'a list
val cons : 'a -> 'a list -> 'a list
val uncons : 'a list -> 'a * 'a list
val safe_tl : 'a list -> 'a list
val head_middle_tail : 'a list -> 'a * 'a list * 'a
val last : 'a list -> 'a
val list_init : 'a list -> 'a list
val list_last : 'a list -> 'a
val removelast : 'a list -> 'a list

val inits : 'a list -> 'a list list
val tails : 'a list -> 'a list list

val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val fold_k : ('a -> 'b -> ('a -> 'a) -> 'a) -> ('a -> 'a) -> 'a -> 'b list -> 'a
val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val rev_map : ('a -> 'b) -> 'a list -> 'b list

val join_gen : 'a -> 'a list -> 'a list

val do_withenv :
  (('a -> 'b) -> 'c -> 'd) -> ('e -> 'a -> 'b * 'e) -> 'e -> 'c -> 'd * 'e
val map_withenv : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a
val map_withkeep: ('a -> 'b) -> 'a list -> ('b * 'a) list

val collect_accu : ('a -> 'b list) -> 'b list -> 'a list -> 'b list
val collect : ('a -> 'b list) -> 'a list -> 'b list

val remove : 'a -> 'a list -> 'a list

val exclude : ('a -> bool) -> 'a list -> 'a list

(* Not like unix uniq command line tool that only delete contiguous repeated
 * line. Here we delete any repeated line (here list element).
 *)
val uniq : 'a list -> 'a list
val uniq_eff: 'a list -> 'a list

val has_no_duplicate: 'a list -> bool
val is_set_as_list: 'a list -> bool
val get_duplicates: 'a list -> 'a list

val doublon : 'a list -> bool

val reverse : 'a list -> 'a list (* alias *)
val rev : 'a list -> 'a list (* alias *)
val rotate : 'a list -> 'a list

val map_flatten : ('a -> 'b list) -> 'a list -> 'b list

val map2 : ('a -> 'b) -> 'a list -> 'b list
val map3 : ('a -> 'b) -> 'a list -> 'b list


val maximum : 'a list -> 'a
val minimum : 'a list -> 'a

val min_with : ('a -> 'b) -> 'a list -> 'a
val two_mins_with : ('a -> 'b) -> 'a list -> 'a * 'a

val all_assoc : (* Eq a *) 'a -> ('a * 'b) list -> 'b list
val prepare_want_all_assoc : ('a * 'b) list -> ('a * 'b list) list

val or_list : bool list -> bool
val and_list : bool list -> bool

val sum_float : float list -> float
val sum_int : int list -> int
val avg_list: int list -> float

val return_when : ('a -> 'b option) -> 'a list -> 'b


val grep_with_previous : ('a -> 'a -> bool) -> 'a list -> 'a list
val iter_with_previous : ('a -> 'a -> 'b) -> 'a list -> unit

val iter_with_before_after :
 ('a list -> 'a -> 'a list -> unit) -> 'a list -> unit

val get_pair : 'a list -> ('a * 'a) list

val permutation : 'a list -> 'a list list

val remove_elem_pos : int -> 'a list -> 'a list
val insert_elem_pos : ('a * int) -> 'a list -> 'a list
val uncons_permut :   'a list -> (('a * int) * 'a list) list
val uncons_permut_lazy :   'a list -> (('a * int) * 'a list Lazy.t) list


val pack_sorted : ('a -> 'a -> bool) -> 'a list -> 'a list list

val keep_best : ('a * 'a -> 'a option) -> 'a list -> 'a list
val sorted_keep_best : ('a -> 'a -> 'a option) -> 'a list -> 'a list


val cartesian_product : 'a list -> 'b list -> ('a * 'b) list

val equal_list : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(**
 * [equal_list eq [x0; x1; ...; xm] [y0; y1; ...; yn]] returns [true] iff
 * [m = n] and [eq x0 y0], [eq x1 y1], ..., and [eq xn yn].
 *)

(* old stuff *)
val surEnsemble : 'a list -> 'a list list -> 'a list list
val realCombinaison : 'a list -> 'a list list
val combinaison : 'a list -> ('a * 'a) list
val insere : 'a -> 'a list list -> 'a list list
val insereListeContenant : 'a list -> 'a -> 'a list list -> 'a list list
val fusionneListeContenant : 'a * 'a -> 'a list list -> 'a list list

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

val array_find_index : (int -> bool) -> 'a array -> int
val array_find_index_via_elem : ('a -> bool) -> 'a array -> int

(* for better type checking, as sometimes when have an 'int array', can
 * easily mess up the index from the value.
 *)
type idx = Idx of int
val next_idx: idx -> idx
val int_of_idx: idx -> int

val array_find_index_typed : (idx -> bool) -> 'a array -> idx

(*****************************************************************************)
(* Matrix *)
(*****************************************************************************)

type 'a matrix = 'a array array

val map_matrix : ('a -> 'b) -> 'a matrix -> 'b matrix

val make_matrix_init:
  nrow:int -> ncolumn:int -> (int -> int -> 'a) -> 'a matrix

val iter_matrix:
  (int -> int -> 'a -> unit) -> 'a matrix -> unit

val nb_rows_matrix: 'a matrix -> int
val nb_columns_matrix: 'a matrix -> int

val rows_of_matrix: 'a matrix -> 'a list list
val columns_of_matrix: 'a matrix -> 'a list list

val all_elems_matrix_by_row: 'a matrix -> 'a list

(*****************************************************************************)
(* Fast array *)
(*****************************************************************************)

(* ?? *)

(*****************************************************************************)
(* Set. But have a look too at set*.mli; it's better. Or use Hashtbl. *)
(*****************************************************************************)

type 'a set = 'a list

val empty_set : 'a set

val insert_set : 'a -> 'a set -> 'a set
val single_set : 'a -> 'a set
val set : 'a list -> 'a set

val is_set: 'a list -> bool

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

val union_all : ('a set) list -> 'a set
val inter_all : ('a set) list -> 'a set

val big_union_set : ('a -> 'b set) -> 'a set -> 'b set
val card_set : 'a set -> int

val include_set : 'a set -> 'a set -> bool
val equal_set : 'a set -> 'a set -> bool
val include_set_strict : 'a set -> 'a set -> bool

(* could put them in Common.Infix *)
val ( $*$ ) : 'a set -> 'a set -> 'a set
val ( $+$ ) : 'a set -> 'a set -> 'a set
val ( $-$ ) : 'a set -> 'a set -> 'a set

val ( $?$ ) : 'a -> 'a set -> bool
val ( $<$ ) : 'a set -> 'a set -> bool
val ( $<=$ ) : 'a set -> 'a set -> bool
val ( $=$ ) : 'a set -> 'a set -> bool

val ( $@$ ) : 'a list -> 'a list -> 'a list

val nub : 'a list -> 'a list

(* use internally a hash and return
 * - the common part,
 * - part only in a,
 * - part only in b
 *)
val diff_two_say_set_eff : 'a list -> 'a list ->
  'a list * 'a list * 'a list

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)

(* cf above *)

(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)


(*****************************************************************************)
(* Sets specialized *)
(*****************************************************************************)

(*
module StringSet = Set.Make(struct type t = string let compare = compare end)
*)


(*****************************************************************************)
(* Assoc. But have a look too at Mapb.mli; it's better. Or use Hashtbl. *)
(*****************************************************************************)

type ('a, 'b) assoc = ('a * 'b) list

val assoc_to_function : (* Eq a *) ('a, 'b) assoc -> ('a -> 'b)

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

val assoc_option : 'a -> ('a, 'b) assoc -> 'b option
val assoc_with_err_msg : 'a -> ('a, 'b) assoc -> 'b

val sort_by_val_lowfirst: ('a,'b) assoc -> ('a * 'b) list
val sort_by_val_highfirst: ('a,'b) assoc -> ('a * 'b) list

val sort_by_key_lowfirst: (int,'b) assoc -> (int * 'b) list
val sort_by_key_highfirst: (int,'b) assoc -> (int * 'b) list

val sortgen_by_key_lowfirst: ('a,'b) assoc -> ('a * 'b) list
val sortgen_by_key_highfirst: ('a,'b) assoc -> ('a * 'b) list

(*****************************************************************************)
(* Assoc, specialized. *)
(*****************************************************************************)

module IntMap : Map.S with type key = int
val intmap_to_list : 'a IntMap.t -> (IntMap.key * 'a) list
val intmap_string_of_t : 'a -> 'b -> string

module IntIntMap : Map.S with type key = int * int
val intintmap_to_list : 'a IntIntMap.t -> (IntIntMap.key * 'a) list
val intintmap_string_of_t : 'a -> 'b -> string


(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

(* Note that Hashtbl keep old binding to a key so if want a hash
 * of a list, then can use the Hashtbl as is. Use Hashtbl.find_all then
 * to get the list of bindings
 *
 * Note that Hashtbl module use different convention :( the object is
 * the first argument, not last as for List or Map.
 *)

(* obsolete: can use directly the Hashtbl module *)
val hcreate : unit -> ('a, 'b) Hashtbl.t
val hadd : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hmem : 'a -> ('a, 'b) Hashtbl.t -> bool
val hfind : 'a -> ('a, 'b) Hashtbl.t -> 'b
val hreplace : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hiter : ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
val hfold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) Hashtbl.t -> 'c -> 'c
val hremove : 'a -> ('a, 'b) Hashtbl.t -> unit


val hfind_default : 'a -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> 'b
val hfind_option : 'a -> ('a, 'b) Hashtbl.t -> 'b option
val hupdate_default :
  'a -> ('b -> 'b) -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit

val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hash_to_list_unsorted : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t

val hashadd : ('a, 'b list ref) Hashtbl.t -> 'a -> 'b -> unit
val hashinc : ('a, int ref) Hashtbl.t -> 'a -> int -> unit


val hkeys : ('a, 'b) Hashtbl.t -> 'a list

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t


(* common use of hashset, in a hash of hash *)
val hash_hashset_add : 'a -> 'b -> ('a, 'b hashset) Hashtbl.t -> unit

val hashset_to_set :
 < fromlist : ('a ) list -> 'c; .. > -> ('a, 'b) Hashtbl.t -> 'c

val hashset_to_list : 'a hashset -> 'a list
val hashset_of_list : 'a list -> 'a hashset


(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

type 'a stack = 'a list
val empty_stack : 'a stack
val push : 'a -> 'a stack -> 'a stack
val top : 'a stack -> 'a
val pop : 'a stack -> 'a stack

val top_option: 'a stack -> 'a option

val push2 : 'a -> 'a stack ref -> unit
val pop2: 'a stack ref -> 'a

(*****************************************************************************)
(* Stack with undo/redo support *)
(*****************************************************************************)

type 'a undo_stack = 'a list * 'a list
val empty_undo_stack : 'a undo_stack
val push_undo : 'a -> 'a undo_stack -> 'a undo_stack
val top_undo : 'a undo_stack -> 'a
val pop_undo : 'a undo_stack -> 'a undo_stack
val redo_undo: 'a undo_stack -> 'a undo_stack
val undo_pop: 'a undo_stack -> 'a undo_stack

val top_undo_option: 'a undo_stack -> 'a option


(*****************************************************************************)
(* Binary tree *)
(*****************************************************************************)
type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree)

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

(* no empty tree, must have one root at least *)
type 'a tree = Tree of 'a * ('a tree) list

val tree_iter : ('a -> unit) -> 'a tree -> unit


(*****************************************************************************)
(* N-ary tree with updatable children *)
(*****************************************************************************)

(* no empty tree, must have one root at least *)
type 'a treeref =
  | NodeRef of 'a *   'a treeref list ref

val treeref_node_iter:
  (('a * 'a treeref list ref) -> unit) -> 'a treeref -> unit
val treeref_node_iter_with_parents:
  (('a * 'a treeref list ref) -> ('a list) -> unit) ->
  'a treeref -> unit

val find_treeref:
  (('a * 'a treeref list ref) -> bool) ->
  'a treeref -> 'a treeref

val treeref_children_ref:
  'a treeref -> 'a treeref list ref

val find_treeref_with_parents_some:
 ('a * 'a treeref list ref -> 'a list -> 'c option) ->
 'a treeref -> 'c

val find_multi_treeref_with_parents_some:
 ('a * 'a treeref list ref -> 'a list -> 'c option) ->
 'a treeref -> 'c list


(* Leaf can seem redundant, but sometimes want to directly see if
 * a children is a leaf without looking if the list is empty.
 *)
type ('a, 'b) treeref2 =
  | NodeRef2 of 'a * ('a, 'b) treeref2 list ref
  | LeafRef2 of 'b


val find_treeref2:
  (('a * ('a, 'b) treeref2 list ref) -> bool) ->
  ('a, 'b) treeref2 -> ('a, 'b) treeref2

val treeref_node_iter_with_parents2:
  (('a * ('a, 'b) treeref2 list ref) -> ('a list) -> unit) ->
  ('a, 'b) treeref2 -> unit

val treeref_node_iter2:
  (('a * ('a, 'b) treeref2 list ref) -> unit) -> ('a, 'b) treeref2 -> unit

(*


val treeref_children_ref: ('a, 'b) treeref -> ('a, 'b) treeref list ref

val find_treeref_with_parents_some:
 ('a * ('a, 'b) treeref list ref -> 'a list -> 'c option) ->
 ('a, 'b) treeref -> 'c

val find_multi_treeref_with_parents_some:
 ('a * ('a, 'b) treeref list ref -> 'a list -> 'c option) ->
 ('a, 'b) treeref -> 'c list
*)

(*****************************************************************************)
(* Graph. But have a look too at Ograph_*.mli; it's better *)
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

(* mostly alias to functions in List *)

val map : ('a -> 'b) -> 'a list -> 'b list
val tail_map : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list
val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val member : 'a -> 'a list -> bool

val iter : ('a -> unit) -> 'a list -> unit

val find : ('a -> bool) -> 'a list -> 'a

val exists : ('a -> bool) -> 'a list -> bool
val forall : ('a -> bool) -> 'a list -> bool

val big_union : ('a -> 'b set) -> 'a list -> 'b set

val sort : ('a -> 'a -> int) -> 'a list -> 'a list

val length : 'a list -> int

val head : 'a list -> 'a
val tail : 'a list -> 'a list

val is_singleton : 'a list -> bool









(*###########################################################################*)
(* And now misc functions *)
(*###########################################################################*)

(*****************************************************************************)
(* GUI (LFS, CComment, otimetracker) *)
(*****************************************************************************)

(* cf ocamlgtk and my gui.ml *)


(*****************************************************************************)
(* Graphics (otimetracker) *)
(*****************************************************************************)

(* cf ocamlgl and my opengl.ml *)



(*****************************************************************************)
(* Geometry (ICFP raytracer) *)
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
(* Pics (ICFP raytracer) *)
(*****************************************************************************)
type pixel = int * int * int
val write_ppm : int -> int -> pixel list -> filename -> unit
val test_ppm1 : unit -> unit

(*****************************************************************************)
(* Diff (LFS) *)
(*****************************************************************************)

type diff = Match | BnotinA | AnotinB
val diff : (int -> int -> diff -> unit) -> string list * string list -> unit
val diff2 : (int -> int -> diff -> unit) -> string * string -> unit

(*****************************************************************************)
(* Parsers (cocci) *)
(*****************************************************************************)


(* Currently lexing.ml does not handle the line number position.
 * Even if there is some fields in the lexing structure, they are not
 * maintained by the lexing engine :( So the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence those functions to overcome the previous limitation.
 *)

type parse_info = {
    str: string;
    charpos: int;

    line: int;
    column: int;
    file: filename;
  }
val fake_parse_info : parse_info
val string_of_parse_info : parse_info -> string
val string_of_parse_info_bis : parse_info -> string

(* array[i] will contain the (line x col) of the i char position *)
val full_charpos_to_pos : filename -> (int * int) array

(* fill in the line and column field of parse_info that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_parse_info :
  filename -> (int * int) array -> parse_info -> parse_info

(* return line x col x str_line  from a charpos. This function is quite
 * expensive so don't use it to get the line x col from every token in
 * a file. Instead use full_charpos_to_pos.
 *)
val info_from_charpos : int -> filename -> (int * int * string)

val error_message :       filename -> (string * int) -> string
val error_message_short : filename -> (string * int) -> string

(* add a 'decalage/shift' argument to handle stuff such as cpp which includes
 * files and who can make shift.
 *)
val error_messagebis : filename -> (string * int) -> int -> string

(*****************************************************************************)
(* Scope management (cocci) *)
(*****************************************************************************)

(* for example of use, see the code used in coccinelle *)
type ('a, 'b) scoped_env = ('a, 'b) assoc list

val lookup_env : (* Eq a *) 'a -> ('a, 'b) scoped_env -> 'b
val member_env_key : 'a -> ('a, 'b) scoped_env -> bool

val new_scope : ('a, 'b) scoped_env ref -> unit
val del_scope : ('a, 'b) scoped_env ref -> unit

val do_in_new_scope : ('a, 'b) scoped_env ref -> (unit -> unit) -> unit

val add_in_scope : ('a, 'b) scoped_env ref -> 'a * 'b -> unit




(* for example of use, see the code used in coccinelle *)
type ('a, 'b) scoped_h_env = {
  scoped_h : ('a, 'b) Hashtbl.t;
  scoped_list : ('a, 'b) assoc list;
}
val empty_scoped_h_env : unit -> ('a, 'b) scoped_h_env
val clone_scoped_h_env : ('a, 'b) scoped_h_env -> ('a, 'b) scoped_h_env

val lookup_h_env : 'a -> ('a, 'b) scoped_h_env -> 'b
val member_h_env_key : 'a -> ('a, 'b) scoped_h_env -> bool

val new_scope_h : ('a, 'b) scoped_h_env ref -> unit
val del_scope_h : ('a, 'b) scoped_h_env ref -> unit
val clean_scope_h : ('a, 'b) scoped_h_env ref -> unit

val do_in_new_scope_h : ('a, 'b) scoped_h_env ref -> (unit -> unit) -> unit

val add_in_scope_h : ('a, 'b) scoped_h_env ref -> 'a * 'b -> unit

(*****************************************************************************)
(* Terminal (LFS) *)
(*****************************************************************************)

(* don't forget to call Common_extra.set_link () *)

val _execute_and_show_progress_func :
  (int (* length *) -> ((unit -> unit) -> unit) -> unit) ref
val execute_and_show_progress :
 int (* length *) -> ((unit -> unit) -> unit) -> unit

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)

val cmdline_flags_devel : unit -> cmdline_options
val cmdline_flags_verbose : unit -> cmdline_options
val cmdline_flags_other : unit -> cmdline_options

(*****************************************************************************)
(* Misc/test *)
(*****************************************************************************)

val generic_print : 'a -> string -> string

class ['a] olist :
  'a list ->
  object
    val xs : 'a list
    method fold : ('b -> 'a -> 'b) -> 'b -> 'b
    method view : 'a list
  end

val typing_sux_test : unit -> unit

module StringSet: Set.S with type elt = string

(*****************************************************************************)
(* cache *)
(*****************************************************************************)

type 'a dll = DElem of 'a dll option ref * 'a * 'a dll option ref

val create_bounded_cache :
    int -> ('a * 'b) ->
      (int * int ref * ('a,('a * 'b) dll) Hashtbl.t * ('a * 'b) dll)

val find_bounded_cache :
    (int * int ref * ('a,('a * 'b) dll) Hashtbl.t * ('a * 'b) dll) ->
      'a -> 'b

val extend_bounded_cache :
    (int * int ref * ('a,('a * 'b) dll) Hashtbl.t * ('a * 'b) dll) ->
      'a -> 'b -> unit
