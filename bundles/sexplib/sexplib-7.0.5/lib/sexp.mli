type t = Type.t = Atom of string | List of t list
exception Of_sexp_error of exn * t
type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val default_indent : int ref
val is_special_char : char -> bool
val must_escape : string -> bool
val maybe_esc_str : string -> string
val pp_maybe_esc_str : Format.formatter -> string -> unit
val pp_hum_indent : int -> Format.formatter -> t -> unit
val pp_hum_rest : int -> Format.formatter -> t list -> unit
val pp_mach_internal : bool -> Format.formatter -> t -> bool
val pp_mach_rest : bool -> Format.formatter -> t list -> unit
val pp_hum : Format.formatter -> t -> unit
val pp_mach : Format.formatter -> t -> unit
val pp : Format.formatter -> t -> unit
val size_loop : int * int -> t -> int * int
val size : t -> int * int
val to_buffer_hum : buf:Buffer.t -> ?indent:int -> t -> unit
val to_buffer_mach : buf:Buffer.t -> t -> unit
val to_buffer : buf:Buffer.t -> t -> unit
val buffer : unit -> Buffer.t
val with_new_buffer : out_channel -> (Buffer.t -> 'a) -> unit
val output_hum : out_channel -> t -> unit
val output_hum_indent : int -> out_channel -> t -> unit
val output_mach : out_channel -> t -> unit
val output : out_channel -> t -> unit
val save_of_output :
  ?perm:Unix.file_perm -> (out_channel -> 'a -> 'b) -> string -> 'a -> unit
val output_sexp_nl : (out_channel -> 'a -> 'b) -> out_channel -> 'a -> unit
val save_hum : ?perm:Unix.file_perm -> string -> t -> unit
val save_mach : ?perm:Unix.file_perm -> string -> t -> unit
val save : ?perm:Unix.file_perm -> string -> t -> unit
val output_sexps_nl :
  (out_channel -> 'a -> 'b) -> out_channel -> 'a list -> unit
val save_sexps_hum : ?perm:Unix.file_perm -> string -> t list -> unit
val save_sexps_mach : ?perm:Unix.file_perm -> string -> t list -> unit
val save_sexps : ?perm:Unix.file_perm -> string -> t list -> unit
val to_string_hum : ?indent:int -> t -> string
val to_string_mach : t -> string
val to_string : t -> string
val scan_sexp : ?buf:Buffer.t -> Lexing.lexbuf -> Type.t
val scan_sexps : ?buf:Buffer.t -> Lexing.lexbuf -> Type.t list
val get_main_buf : Buffer.t option -> Lexing.lexbuf -> Parser.token
val scan_fold_sexps :
  ?buf:Buffer.t -> f:('a -> Type.t -> 'a) -> init:'a -> Lexing.lexbuf -> 'a
val scan_iter_sexps :
  ?buf:Buffer.t -> f:(Type.t -> unit) -> Lexing.lexbuf -> unit
val scan_sexps_conv :
  ?buf:Buffer.t -> f:(Type.t -> 'a) -> Lexing.lexbuf -> 'a list
module Annot :
  sig
    type pos = Pre_sexp.Annot.pos = { line : int; col : int; offset : int; }
    type range = Pre_sexp.Annot.range = { start_pos : pos; end_pos : pos; }
    type t =
      Pre_sexp.Annot.t =
        Atom of range * Type.t
      | List of range * t list * Type.t
    type 'a conv = [ `Error of exn * t | `Result of 'a ]
    exception Conv_exn of string * exn
    type stack =
      Pre_sexp.Annot.stack = {
      mutable positions : pos list;
      mutable stack : t list list;
    }
    val get_sexp : t -> Type.t
    val get_range : t -> range
    exception Annot_sexp of t
    val find_sexp : t -> Type.t -> t option
  end
module Parse_pos :
  sig
    type t =
      Pre_sexp.Parse_pos.t = {
      mutable text_line : int;
      mutable text_char : int;
      mutable global_offset : int;
      mutable buf_pos : int;
    }
    val create :
      ?text_line:int ->
      ?text_char:int -> ?buf_pos:int -> ?global_offset:int -> unit -> t
    val with_buf_pos : t -> int -> t
  end
type ('a, 'b) parse_result =
  ('a, 'b) Pre_sexp.parse_result =
    Done of 'b * Parse_pos.t
  | Cont of bool * ('a, 'b) parse_fun
and ('a, 'b) parse_fun = pos:int -> len:int -> 'a -> ('a, 'b) parse_result
type 'a parse_state =
  'a Pre_sexp.parse_state = {
  parse_pos : Parse_pos.t;
  mutable pstack : 'a;
  pbuf : Buffer.t;
}
type parse_error =
  Pre_sexp.parse_error = {
  location : string;
  err_msg : string;
  parse_state :
    [ `Annot of Annot.stack parse_state | `Sexp of t list list parse_state ];
}
exception Parse_error of parse_error
val bump_text_line : 'a parse_state -> unit
val bump_text_pos : 'a parse_state -> unit
val bump_pos_cont :
  'a parse_state ->
  'b ->
  max_pos:'c ->
  pos:int -> ('a parse_state -> 'b -> max_pos:'c -> pos:int -> 'd) -> 'd
val bump_line_cont :
  'a parse_state ->
  'b ->
  max_pos:'c ->
  pos:int -> ('a parse_state -> 'b -> max_pos:'c -> pos:int -> 'd) -> 'd
val add_bump :
  ('a parse_state -> 'b) ->
  'a parse_state ->
  'c ->
  max_pos:'d ->
  pos:int ->
  char -> ('a parse_state -> 'c -> max_pos:'d -> pos:int -> 'e) -> 'e
val add_bump_pos :
  'a parse_state ->
  'b ->
  max_pos:'c ->
  pos:int ->
  char -> ('a parse_state -> 'b -> max_pos:'c -> pos:int -> 'd) -> 'd
val add_bump_line :
  'a parse_state ->
  'b ->
  max_pos:'c ->
  pos:int ->
  char -> ('a parse_state -> 'b -> max_pos:'c -> pos:int -> 'd) -> 'd
val set_parse_pos : Parse_pos.t -> int -> unit
val mk_parse_pos : 'a parse_state -> int -> Parse_pos.t
val raise_parse_error :
  [ `Annot of Annot.stack parse_state | `Sexp of t list list parse_state ] ->
  string -> int -> string -> 'a
val raise_unexpected_char :
  [ `Annot of Annot.stack parse_state | `Sexp of t list list parse_state ] ->
  string -> int -> char -> 'a
val parse_str :
  ?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, t) parse_result
val parse :
  ?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, t) parse_result
val get_glob_ofs : Parse_pos.t -> int -> int
val mk_annot_pos : Parse_pos.t -> int -> Annot.pos
val mk_annot_pos1 : Parse_pos.t -> int -> Annot.pos
val add_annot_pos : Annot.stack parse_state -> int -> unit
val add_annot_pos1 : Annot.stack parse_state -> int -> unit
val get_annot_range : Annot.stack parse_state -> int -> Annot.range
val mk_annot_atom : Annot.stack parse_state -> string -> int -> Annot.t
val mk_annot_list : Annot.stack parse_state -> Annot.t list -> int -> Annot.t
val init_annot_pstate : unit -> Annot.stack
val parse_str_annot :
  ?parse_pos:Parse_pos.t ->
  ?len:int -> string -> (string, Annot.t) parse_result
val parse_bigstring :
  ?parse_pos:Parse_pos.t ->
  ?len:int -> bigstring -> (bigstring, t) parse_result
val bump_found_atom :
  (Annot.stack parse_state -> 'a) ->
  Annot.stack parse_state ->
  'b ->
  max_pos:'c ->
  pos:int ->
  (Annot.stack parse_state ->
   'b -> max_pos:'c -> pos:int -> ('d, Annot.t) parse_result) ->
  ('d, Annot.t) parse_result
val check_str_bounds : string -> pos:int -> len:int -> bigstring -> int
val mk_cont :
  string ->
  (Annot.stack parse_state ->
   bigstring -> max_pos:int -> pos:int -> (bigstring, 'a) parse_result) ->
  Annot.stack parse_state -> (bigstring, 'a) parse_result
val parse_nl :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_comment :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_atom :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val reg_parse_quoted :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_quoted :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_quoted_nl :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_escaped :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_skip_ws :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_skip_ws_nl :
  Annot.stack parse_state ->
  bigstring -> max_pos:int -> pos:int -> (bigstring, Annot.t) parse_result
val parse_dec :
  Annot.stack parse_state ->
  bigstring ->
  max_pos:int ->
  pos:int -> count:int -> d:int -> (bigstring, Annot.t) parse_result
val parse_hex :
  Annot.stack parse_state ->
  bigstring ->
  max_pos:int ->
  pos:int -> count:int -> d:int -> (bigstring, Annot.t) parse_result
val parse_bigstring_annot :
  ?parse_pos:Parse_pos.t ->
  ?len:int -> bigstring -> (bigstring, Annot.t) parse_result
val mk_this_parse :
  ?parse_pos:Parse_pos.t ->
  (?parse_pos:Parse_pos.t -> ?len:'a -> 'b -> 'c) ->
  pos:int -> len:'a -> 'b -> 'c
val gen_input_sexp :
  (?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, 'a) parse_result) ->
  ?parse_pos:Parse_pos.t -> in_channel -> 'a
val input_sexp : ?parse_pos:Parse_pos.t -> in_channel -> t
val gen_input_rev_sexps :
  (?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, 'a) parse_result) ->
  ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> 'a list
val input_rev_sexps :
  ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> t list
val input_sexps :
  ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> t list
val of_string_bigstring :
  string ->
  ('a -> ('b, 'c) parse_result) ->
  'b -> ('a -> int) -> ('a -> int -> int -> string) -> 'a -> 'c
val of_string : string -> t
val get_bstr_sub_str :
  (char, 'a, 'b) Bigarray.Array1.t -> int -> int -> string
val bstr_ws_buf :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val of_bigstring : bigstring -> t
val gen_load_rev_sexps :
  (?parse_pos:'a -> ?buf:'b -> in_channel -> 'c) -> ?buf:'b -> string -> 'c
val load_rev_sexps : ?buf:string -> string -> t list
val load_sexps : ?buf:string -> string -> t list
val gen_load_sexp :
  (?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, 'a) parse_result) ->
  ?strict:bool -> ?buf:string -> string -> 'a
val load_sexp : ?strict:bool -> ?buf:string -> string -> t
module Annotated :
  sig
    type pos = Annot.pos = { line : int; col : int; offset : int; }
    type range = Annot.range = { start_pos : pos; end_pos : pos; }
    type t =
      Annot.t =
        Atom of range * Type.t
      | List of range * t list * Type.t
    type 'a conv = [ `Error of exn * t | `Result of 'a ]
    exception Conv_exn of string * exn
    type stack =
      Annot.stack = {
      mutable positions : pos list;
      mutable stack : t list list;
    }
    val get_sexp : t -> Type.t
    val get_range : t -> range
    exception Annot_sexp of t
    val find_sexp : t -> Type.t -> t option
    val parse :
      ?parse_pos:Parse_pos.t ->
      ?len:int -> string -> (string, Annot.t) parse_result
    val parse_bigstring :
      ?parse_pos:Parse_pos.t ->
      ?len:int -> bigstring -> (bigstring, Annot.t) parse_result
    val input_rev_sexps :
      ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> Annot.t list
    val input_sexp : ?parse_pos:Parse_pos.t -> in_channel -> Annot.t
    val input_sexps :
      ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> Annot.t list
    val of_string : string -> Annot.t
    val of_bigstring : bigstring -> Annot.t
    val load_rev_sexps : ?buf:string -> string -> Annot.t list
    val load_sexps : ?buf:string -> string -> Annot.t list
    val load_sexp : ?strict:bool -> ?buf:string -> string -> Annot.t
    val conv : (Type.t -> 'a) -> t -> [> `Error of exn * t | `Result of 'a ]
    val get_conv_exn : file:string -> exc:exn -> t -> exn
  end
val load_sexp_conv :
  ?strict:bool ->
  ?buf:string ->
  string -> (t -> 'a) -> [> `Error of exn * Annotated.t | `Result of 'a ]
val raise_conv_exn :
  file:string -> [< `Error of exn * Annotated.t | `Result of 'a ] -> 'a
val load_sexp_conv_exn :
  ?strict:bool -> ?buf:string -> string -> (t -> 'a) -> 'a
val load_sexps_conv :
  ?buf:string ->
  string ->
  (t -> 'a) -> [> `Error of exn * Annotated.t | `Result of 'a ] list
val load_sexps_conv_exn : ?buf:string -> string -> (t -> 'a) -> 'a list
val gen_of_string_conv :
  ('a -> Type.t) ->
  ('a -> Annotated.t) ->
  'a -> (Type.t -> 'b) -> [> `Error of exn * Annotated.t | `Result of 'b ]
val of_string_conv :
  string -> (t -> 'a) -> [> `Error of exn * Annotated.t | `Result of 'a ]
val of_bigstring_conv :
  bigstring -> (t -> 'a) -> [> `Error of exn * Annotated.t | `Result of 'a ]
module Of_string_conv_exn :
  sig
    type t =
      Pre_sexp.Of_string_conv_exn.t = {
      exc : exn;
      sexp : Type.t;
      sub_sexp : Type.t;
    }
    exception E of t
  end
val gen_of_string_conv_exn : ('a -> Type.t) -> 'a -> (Type.t -> 'b) -> 'b
val of_string_conv_exn : string -> (t -> 'a) -> 'a
val of_bigstring_conv_exn : bigstring -> (t -> 'a) -> 'a
val unit : t
external sexp_of_t : t -> t = "%identity"
external t_of_sexp : t -> t = "%identity"
type found = [ `Found | `Pos of int * found ]
type search_result = [ `Found | `Not_found | `Pos of int * found ]
val search_physical :
  t -> contained:t -> [ `Found | `Not_found | `Pos of int * found ]
val subst_found : t -> subst:t -> ([< `Found | `Pos of int * 'a ] as 'a) -> t
