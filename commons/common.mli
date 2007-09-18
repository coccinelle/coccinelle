type filename = string

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

val _tab_level_print: int ref
val indent_do : (unit -> 'a) -> 'a
val reset_pr_indent : unit -> unit

(* The following functions first indent _tab_level_print spaces.
 * 
 * The use of 2 in pr2 is because 2 is under UNIX the second descriptor
 * which corresponds to stderr. 
 *)
val pr : string -> unit
val pr2 : string -> unit
val pr_no_nl : string -> unit
val pr2_no_nl : string -> unit
val pr_xxxxxxxxxxxxxxxxx : unit -> unit
val pr2_xxxxxxxxxxxxxxxxx : unit -> unit

(* use Dumper.dump *)
val pr2_gen: 'a -> unit

val _already_printed : (string, bool) Hashtbl.t
val _disable_once : bool ref
val pr2_once : string -> unit

val redirect_stdout_stderr : filename -> (unit -> unit) -> unit
val redirect_stdin : filename -> (unit -> unit) -> unit
val redirect_stdin_opt : filename option -> (unit -> unit) -> unit

val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b

val spf : ('a, unit, string) format -> 'a

val _chan : out_channel ref (* default = stderr *)
val start_log_file : unit -> unit (* generate & use a /tmp/debugml-xxx file *)
val verbose_level : int ref
val log : string -> unit
val log2 : string -> unit
val log3 : string -> unit

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

(* if set then will not do certain finalize so faster to go back in replay *)
val debugger : bool ref

(*****************************************************************************)
(* Profiling (cpu/mem) *)
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
val profile_diagnostic_basic : unit -> string

val time_func : (unit -> 'a) -> 'a

val _profile_table : (string, (float ref * int ref)) Hashtbl.t ref
val profile : bool ref
val profile_code : string -> (unit -> 'a) -> 'a
val profile_diagnostic : unit -> unit

val report_if_take_time : int -> string -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

val example : bool -> unit
val example2 : string -> bool -> unit (* generate failwith <string> when pb *)
val assert_equal : 'a -> 'a -> unit (* use Dumper to report when pb *)

val _list_bool : (string * bool) list ref
val example3 : string -> bool -> unit
val test_all : unit -> unit

type score_result = Ok | Pb of string 
type score = (string (* usually a filename *), score_result) Hashtbl.t
val empty_score : unit -> score
val regression_testing : 
  score -> filename (* old score file on disk (usually in /tmp) *) -> unit
val print_score : score -> unit


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

val get_value : filename -> 'a
val write_value : 'a -> filename -> unit
val write_back : ('a -> 'b) -> filename -> unit

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
 * (with the pp* functions) and sometimes explicit and simple indendation
 * printing (with pr* and indent_do) *)
val adjust_pp_with_indent : (unit -> unit) -> unit
val adjust_pp_with_indent_and_header : string -> (unit -> unit) -> unit

(*****************************************************************************)
(* Macro *)
(*****************************************************************************)

val macro_expand : string -> unit

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(* now in Commonop: val ( +> ) : 'a -> ('a -> 'b) -> 'b *)
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

(* emacs spirit *)
val save_excursion : 'a ref -> (unit -> 'b) -> 'b

(* emacs spirit *)
val unwind_protect : (unit -> 'a) -> (exn -> 'b) -> 'a

(* java spirit *)
val finalize :       (unit -> 'a) -> (unit -> 'b) -> 'a

val memoized : ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b


(* take file from which computation is done, extension, and function
 * and will compute the function only once and then save result in 
 * file ^ extension
 *)
val cache_computation : 
  filename  -> string (* extension *) -> (unit -> 'a) -> 'a

(* a more robust version where describes the dependencies of the 
 * computation so it will relaunch the computation in 'f' if needed. 
 *)
val cache_computation_robust :
  filename -> 
  string (* extension for marshalled object *) -> 
  (filename list * 'x) -> 
  string (* extension for marshalled dependencies *) -> 
  (unit -> 'a) -> 
  'a
  


val once : ('a -> unit) -> ('a -> unit)

(* cf also the timeout function below that are control related too *)

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

(* Using the generic (=) is tempting, but it backfires, so better avoid it *)

(* To infer all the code that use an equal, and that should be
 * transformed, is not that easy, because (=) is used by many
 * functions, such as List.find, List.mem, and so on, so the strategy
 * is to turn what you were previously using into a function, because
 * (=) return an exception when applied to a function, then you simply
 * use ocamldebug to infer where the code has to be transformed by
 * finding where the exception was launched from.
 *)

val (=|=) : int    -> int    -> bool
val (=<=) : char   -> char   -> bool
val (=$=) : string -> string -> bool
val (=:=) : bool   -> bool   -> bool

val (=*=): 'a -> 'a -> bool

(* if want to restrict the use of '=', uncomment this:
val (=): int -> int -> bool
*)









(* And now basic types *)

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

(* useful but sometimes when want grep for all places where do modif,
 * easier to have just code using ':=' and '<-' to do some modifications.
 * In the same way avoid using {contents = xxx} to build some ref.
 *)
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

val numd_int   : int   numdict
val numd_float : float numdict

val testd : 'a numdict -> 'a -> 'a

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

type 'a pair = 'a * 'a
type 'a triple = 'a * 'a * 'a

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

val map_fst : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b

val pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b

val snd : 'a * 'b -> 'b (* alias *)
val fst : 'a * 'b -> 'a (* alias *)

val double : 'a -> 'a * 'a
val swap : 'a * 'b -> 'b * 'a

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

val slength : string -> int (* alias *)
val concat : string -> string list -> string (* alias *)

val i_to_s : int -> string
val s_to_i : string -> int

(* strings take space in memory. Better when can share the space used by
 * similar strings.
 *)
val _shareds : (string, string) Hashtbl.t
val shared_string : string -> string

val chop : string -> string
val chop_dirsymbol : string -> string

val ( <!!> ) : string -> int * int -> string
val ( <!> ) : string -> int -> char

val split_on_char : char -> string -> string list

val lowercase : string -> string

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

val regexp_alpha : Str.regexp


(* now in Commonop:
 val ( =~ ) : string -> string -> bool
 val ( ==~ ) : string -> Str.regexp -> bool
*)
val _memo_compiled_regexp : (string, Str.regexp) Hashtbl.t

val regexp_match : string -> string -> string

val matched : int -> string -> string

val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string

val string_match_substring : Str.regexp -> string -> bool

val split : string -> string -> string list
val join : string -> string list -> string

val split_list_regexp : string -> string list -> (string * string list) list

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

(* now at beginning of this file: type filename = string *)
val dirname : string -> string
val basename : string -> string

val filesuffix : filename -> string
val fileprefix : filename -> string

val adjust_ext_if_needed : filename -> string -> filename

(* dbe for dir, base, ext *)
val dbe_of_filename : filename -> string * string * string
val filename_of_dbe : string * string * string -> filename
(* Left (d,b,e) | Right (d,b)  if file has no extension *)
val dbe_of_filename_safe : 
  filename -> (string * string * string,  string * string) either

(* ex: replace_ext "toto.c" "c" "var" *)
val replace_ext: filename -> string -> string -> filename

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

val int_to_month : int -> string


(*****************************************************************************)
(* Lines/Words/Strings *)
(*****************************************************************************)

val list_of_string : string -> char list

val lines : string -> string list
val unlines : string list -> string

val words : string -> string list
val unwords : string list -> string

val lines_with_nl : string -> string list


(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
val cat :      filename -> string list
val cat_orig : filename -> string list

val interpolate : string -> string list

val echo : string -> string

val usleep : int -> unit

val process_output_to_list : string -> string list
val cmd_to_list :            string -> string list (* alias *)

val command2 : string -> unit
val command2_y_or_no : string -> unit

val do_in_fork : (unit -> unit) -> int

val read_file : filename -> string
val write_file : filename -> string -> unit

val filesize : filename -> int
val filemtime : filename -> float

val lfile_exists : filename -> bool

val capsule_unix : ('a -> unit) -> 'a -> unit

val readdir_to_kind_list : string -> Unix.file_kind -> string list
val readdir_to_dir_list : string -> string list
val readdir_to_file_list : string -> string list
val readdir_to_link_list : string -> string list
val readdir_to_dir_size_list : string -> (string * int) list

val glob : string -> filename list

type rwx = [ `R | `W | `X ] list
val file_perm_of : u:rwx -> g:rwx -> o:rwx -> Unix.file_perm

val has_env : string -> bool

(* scheme spirit. do a finalize so no leak. *)
val with_open_outfile : 
  filename -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile : 
  filename -> (in_channel -> 'a) -> 'a

exception Timeout

(* subtil: have to make sure that Timeout is not intercepted before here. So 
 * avoid exn handler such as try (...) with _ -> cos Timeout will not bubble up
 * enough. In such case, add a case before such as  
 * with Timeout -> raise Timeout | _ -> ... 
 * 
 * The same is true for UnixExit (see below).
 *)
val timeout_function : int -> (unit -> 'a) -> 'a

val timeout_function_opt : int option -> (unit -> 'a) -> 'a


(* creation of /tmp files, a la gcc 
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" 
 *)
val _temp_files_created : string list ref
val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename
val erase_temp_files : unit -> unit

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








(* And now collection like types *)

(*****************************************************************************)
(* List *)
(*****************************************************************************)

(* tail recursive efficient map (but that also reverse the element!) *)
val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list


val zip : 'a list -> 'b list -> ('a * 'b) list
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
val skipfirst : 'a -> 'a list -> 'a list

(* cf also List.partition *)
val fpartition : ('a -> 'b option) -> 'a list -> 'b list * 'a list

val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list

(* use hash internally to not be in O(n2) *)
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list

val splitAt : int -> 'a list -> 'a list * 'a list

val split_when: ('a -> bool) -> 'a list -> 'a list * 'a * 'a list

val pack : int -> 'a list -> 'a list list


val enum : int -> int -> int list
val repeat : 'a -> int -> 'a list
val generate : int -> 'a -> 'a list



val index_list   : 'a list -> ('a * int) list
val index_list_1 : 'a list -> ('a * int) list

val iter_index : ('a -> int -> 'b) -> 'a list -> unit
val map_index : ('a -> int -> 'b) -> 'a list -> 'b list
val filter_index : (int -> 'a -> bool) -> 'a list -> 'a list
val fold_left_with_index : ('a -> 'b -> int -> 'a) -> 'a -> 'b list -> 'a

val nth : 'a list -> int -> 'a
val rang : 'a -> 'a list -> int

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


val ( ++ ) : 'a list -> 'a list -> 'a list

val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val fold_k : ('a -> 'b -> ('a -> 'a) -> 'a) -> ('a -> 'a) -> 'a -> 'b list -> 'a
val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val rev_map : ('a -> 'b) -> 'a list -> 'b list

val join_gen : 'a -> 'a list -> 'a list

val do_withenv :
  (('a -> 'b) -> 'c -> 'd) -> ('e -> 'a -> 'b * 'e) -> 'e -> 'c -> 'd * 'e
val map_withenv : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a

val collect_accu : ('a -> 'b list) -> 'b list -> 'a list -> 'b list
val collect : ('a -> 'b list) -> 'a list -> 'b list

val remove : 'a -> 'a list -> 'a list

val exclude : ('a -> bool) -> 'a list -> 'a list

(* Not like unix uniq command line tool that only delete contiguous repeated
 * line. Here we delete any repeated line (here list element).
 *)
val uniq : 'a list -> 'a list
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

val all_assoc : 'a -> ('a * 'b) list -> 'b list
val prepare_want_all_assoc : ('a * 'b) list -> ('a * 'b list) list

val or_list : bool list -> bool
val and_list : bool list -> bool

val return_when : ('a -> 'b option) -> 'a list -> 'b


val grep_with_previous : ('a -> 'a -> bool) -> 'a list -> 'a list
val iter_with_previous : ('a -> 'a -> 'b) -> 'a list -> unit

val get_pair : 'a list -> ('a * 'a) list

val inseredans : 'a -> 'a list -> 'a list list
val permutation : 'a list -> 'a list list

val remove_elem_pos : int -> 'a list -> 'a list
val insert_elem_pos : ('a * int) -> 'a list -> 'a list
val uncons_permut :   'a list -> (('a * int) * 'a list) list


val pack_sorted : ('a -> 'a -> bool) -> 'a list -> 'a list list

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


(*****************************************************************************)
(* Set. But have a look too at set*.mli. It's better. Or use Hashtbl. *)
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

val union_all : ('a set) list -> 'a set

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
(* Assoc. But have a look too at Mapb.mli. It's better. Or use Hashtbl. *)
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
(* Assocs specialized. *)
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

(* note that Hashtbl keep old binding to a key so if want a hash
 * of a list, then can use the Hashtbl as is. use Hashtbl.find_all then
 * to get the list of bindings
 *)

val hcreate : unit -> ('a, 'b) Hashtbl.t
val hadd : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hmem : 'a -> ('a, 'b) Hashtbl.t -> bool
val hfind : 'a -> ('a, 'b) Hashtbl.t -> 'b
val hreplace : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hiter : ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
val hfold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) Hashtbl.t -> 'c -> 'c
val hremove : 'a -> ('a, 'b) Hashtbl.t -> unit


val hfind_default : 'a -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> 'b

val find_hash_set : 'a -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> 'b
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t


val hkeys : ('a, 'b) Hashtbl.t -> 'a list 

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t 

val hash_hashset_add : 'a -> 'b -> ('a, 'b hashset) Hashtbl.t -> unit
val hashset_to_set : 
 < fromlist : ('a ) list -> 'c; .. > -> ('a, 'b) Hashtbl.t -> 'c

val hashset_to_list : 'a hashset -> 'a list

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

type 'a stack = 'a list
val empty_stack : 'a stack
val push : 'a -> 'a stack -> 'a stack
val top : 'a stack -> 'a
val pop : 'a stack -> 'a stack

val push2 : 'a -> 'a stack ref -> unit
val pop2: 'a stack ref -> 'a


(*****************************************************************************)
(* Binary tree *)
(*****************************************************************************)
type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree)

(*****************************************************************************)
(* Graph. But have a look too at Ograph_*.mli. It's better *)
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









(* And now misc functions *)

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
val write_ppm : int -> int -> pixel list -> filename -> unit
val test_ppm1 : unit -> unit

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
(* Parsers (cocci) *)
(*****************************************************************************)


(* Currently lexing.ml does not handle the line number position.
 * Even if there is some fields in the lexing structure, they are not 
 * maintained by the lexing engine :( So the following code does not work:
 *   let pos = Lexing.lexeme_end_p lexbuf in 
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum  
 *      (pos.pos_cnum - pos.pos_bol) in 
 * Hence those functions to overcome the previous limitation.
 *)

type parse_info = {
    str: string;
    charpos: int;

    line: int; column: int;
    file: filename;
  } 

val fake_parse_info : parse_info


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
(* Scope managment *)
(*****************************************************************************)

(* for example of use, see the code used in coccinelle *)
type ('a, 'b) scoped_env = ('a, 'b) assoc list

val lookup_env : 'a -> ('a, 'b) scoped_env -> 'b
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

val do_in_new_scope_h : ('a, 'b) scoped_h_env ref -> (unit -> unit) -> unit

val add_in_scope_h : ('a, 'b) scoped_h_env ref -> 'a * 'b -> unit


(*****************************************************************************)
(* Misc/test *)
(*****************************************************************************)

val showCodeHex : int list -> unit
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

