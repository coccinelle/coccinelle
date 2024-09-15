(*****************************************************************************)
(* convenient globals. *)
(*****************************************************************************)
let path = ref
  (try (Sys.getenv "YACFE_HOME")
    with Not_found-> "/home/pad/c-yacfe"
  )

(*****************************************************************************)
(* macros *)
(*****************************************************************************)

let macro_dir = "config/macros/"
let mk_macro_path ~cocci_path file =
  Filename.concat cocci_path (macro_dir ^ file)


(* to pass to parse_c.init_defs *)
let std_h   = ref (mk_macro_path ~cocci_path:!path "standard.h")
let common_h   = ref (mk_macro_path ~cocci_path:!path "common_macros.h")


(*****************************************************************************)
(* used only by cpp_ast_c, not by the parser *)
(*****************************************************************************)
let cpp_i_opts = ref []
let cpp_d_opts = ref []

(*****************************************************************************)
(* show *)
(*****************************************************************************)

let show_parsing_error = ref true

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_lexing = ref true
let verbose_parsing = ref true
let verbose_type    = ref true
let verbose_cfg    = ref true
let verbose_annotater = ref true
let verbose_unparsing = ref true
let verbose_visit = ref true
let verbose_cpp_ast = ref true
let verbose_includes = ref true

let filter_msg = ref false
let filter_msg_define_error = ref false

let filter_define_error = ref false

let filter_passed_level = ref 0

let pretty_print_type_info = ref false
let pretty_print_comment_info = ref false
let pretty_print_typedef_value = ref false

(* cocci specific *)
let show_flow_labels = ref true


(*****************************************************************************)
(* debugging *)
(*****************************************************************************)

let debug_lexer   = ref false
let debug_etdt    = ref false
let debug_typedef = ref false
let debug_cpp     = ref false

let debug_cpp_ast  = ref false

let debug_unparsing = ref false

let debug_cfg = ref false

(*****************************************************************************)
(* checks *)
(*****************************************************************************)

let check_annotater = ref true

(*****************************************************************************)
(* change algorithm *)
(*****************************************************************************)

(* cocci specific *)
let label_strategy_2 = ref false

(*****************************************************************************)
(* Disable parsing feature (for CC09 and also to see if useful) *)
(*****************************************************************************)

let cpp_directive_passing = ref false
let ifdef_directive_passing = ref false
let ifdef_to_if = ref true(*false*)

let disable_multi_pass = ref false
let disable_add_typedef = ref false

let if0_passing = ref true
let add_typedef_root = ref true
let exts_ITU = ref false (* ITU.dk extensions *)

(* defined and undefined constants *)
let add c s = c := (Str.split (Str.regexp ",") s) @ !c
let defined = ref ([] : string list)
let undefined = ref ([] : string list)

(*****************************************************************************)
(* other *)
(*****************************************************************************)

(* for compare_c *)
let diff_lines = ref (None : string option) (* number of lines of context *)

(* for parse_c *)
let use_cache = ref false
let cache_prefix = ref (None : string option)
let cache_limit = ref (None : int option)

(*****************************************************************************)
(* for lexing of integer constants *)
(*****************************************************************************)

(* "by hand" calculation of power of two to get rid of Big_int dependency *)

let int_of_digit digit =
  int_of_char digit - int_of_char '0'

let digit_of_int i =
  char_of_int (i + int_of_char '0')

let multiply_digit_by_two (carry, accu) digit =
  let sum = carry + int_of_digit digit * 2 in
  let carry = sum / 10 in
  let accu = digit_of_int (sum mod 10) :: accu in
  (carry, accu)

let multiply_digit_list_by_two digit_list =
  let carry, accu =
    List.fold_left multiply_digit_by_two (0, []) (List.rev digit_list) in
  if carry = 0 then
    accu
  else
    digit_of_int carry :: accu

let rec multiply_digit_list_by_two_n n digit_list =
  if n = 0 then digit_list
  else
    let digit_list = multiply_digit_list_by_two digit_list in
    multiply_digit_list_by_two_n (pred n) digit_list

let string_of_digit_list digit_list =
  let a = Array.of_list digit_list in
  Stdcompat.String.init (Array.length a) (fun i -> a.(i))

let string_of_power_of_two n =
  string_of_digit_list (multiply_digit_list_by_two_n n ['1'])

let int_thresholds =
  ref (None :
	 (int (*int_sz*) * int (*long_sz*) *
	    string (*int threshold*) *
	    string (*uint threshold*) *
	    string (*long threshold*) *
	    string (*ulong threshold*)) option)

let set_int_bits n =
  match !int_thresholds with
    None ->
      (*assume long is 2*int; this can be corrected by a subsequent long_bits*)
      let int_threshold  = string_of_power_of_two (n - 1) in
      let uint_threshold  = string_of_power_of_two n in
      let long_threshold  = string_of_power_of_two (2 * n - 1) in
      let ulong_threshold = string_of_power_of_two (2 * n) in
      int_thresholds :=
	Some
	  (n, 2 * n, int_threshold, uint_threshold, long_threshold,
	   ulong_threshold)
  | Some
      (int_sz, long_sz, int_threshold, uint_threshold, long_threshold,
       ulong_threshold) ->
      let int_threshold = string_of_power_of_two (n - 1) in
      let uint_threshold = string_of_power_of_two n in
      int_thresholds :=
	Some
	  (n, long_sz, int_threshold, uint_threshold, long_threshold,
	   ulong_threshold)

let set_long_bits n =
  match !int_thresholds with
    None ->
      (*assume int is 1/2*int; this can be corrected by a subsequent int_bits*)
      set_int_bits (n/2)
  | Some
      (int_sz, long_sz, int_threshold, uint_threshold, long_threshold,
       ulong_threshold) ->
      let long_threshold  = string_of_power_of_two (n - 1) in
      let ulong_threshold = string_of_power_of_two n in
      int_thresholds :=
	Some
	  (n, long_sz, int_threshold, uint_threshold, long_threshold,
	   ulong_threshold)

(*****************************************************************************)
(* unparsing strategy *)
(*****************************************************************************)

type spacing = LINUX | SMPL
let spacing = ref LINUX
let indent = ref 0

let set_linux_spacing _ = spacing := LINUX (*follow the conventions of Linux*)
let set_smpl_spacing _ = spacing := SMPL   (*use spacing from the SP*)

let max_width = ref 78

(*****************************************************************************)

(* drop back edges made by proper loop constructs -
   unsafe but more efficient *)
let no_loops = ref false
let no_gotos = ref false

let keep_comments = ref false (* unparsing *)

let parsing_header_for_types = ref false

let force_kr = ref false
let prevent_kr = ref false
