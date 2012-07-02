module type S =
  sig
    type t = Type.t = Atom of string | List of t list
    type bigstring =
        (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
        Bigarray.Array1.t
    val default_indent : int ref
    val size : t -> int * int
    val scan_sexp : ?buf:Buffer.t -> Lexing.lexbuf -> t
    val scan_sexps : ?buf:Buffer.t -> Lexing.lexbuf -> t list
    val scan_iter_sexps :
      ?buf:Buffer.t -> f:(t -> unit) -> Lexing.lexbuf -> unit
    val scan_fold_sexps :
      ?buf:Buffer.t -> f:('a -> t -> 'a) -> init:'a -> Lexing.lexbuf -> 'a
    val scan_sexps_conv :
      ?buf:Buffer.t -> f:(t -> 'a) -> Lexing.lexbuf -> 'a list
    module Parse_pos :
      sig
        type t =
          Pre_sexp.Parse_pos.t = private {
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
    and ('a, 'b) parse_fun =
        pos:int -> len:int -> 'a -> ('a, 'b) parse_result
    module Annotated :
      sig
        type pos =
          Pre_sexp.Annotated.pos = {
          line : int;
          col : int;
          offset : int;
        }
        type range =
          Pre_sexp.Annotated.range = {
          start_pos : pos;
          end_pos : pos;
        }
        type t =
          Pre_sexp.Annotated.t =
            Atom of range * Type.t
          | List of range * t list * Type.t
        type 'a conv = [ `Error of exn * t | `Result of 'a ]
        exception Conv_exn of string * exn
        type stack =
          Pre_sexp.Annotated.stack = {
          mutable positions : pos list;
          mutable stack : t list list;
        }
        val get_sexp : t -> Type.t
        val get_range : t -> range
        val find_sexp : t -> Type.t -> t option
        val parse :
          ?parse_pos:Parse_pos.t ->
          ?len:int -> string -> (string, t) parse_result
        val parse_bigstring :
          ?parse_pos:Parse_pos.t ->
          ?len:int -> bigstring -> (bigstring, t) parse_result
        val input_sexp : ?parse_pos:Parse_pos.t -> in_channel -> t
        val input_sexps :
          ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> t list
        val input_rev_sexps :
          ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> t list
        val load_sexp : ?strict:bool -> ?buf:string -> string -> t
        val load_sexps : ?buf:string -> string -> t list
        val load_rev_sexps : ?buf:string -> string -> t list
        val of_string : string -> t
        val of_bigstring : bigstring -> t
        val conv : (Type.t -> 'a) -> t -> 'a conv
        val get_conv_exn : file:string -> exc:exn -> t -> exn
      end
    type 'a parse_state =
      'a Pre_sexp.parse_state = private {
      parse_pos : Parse_pos.t;
      mutable pstack : 'a;
      pbuf : Buffer.t;
    }
    type parse_error =
      Pre_sexp.parse_error = {
      location : string;
      err_msg : string;
      parse_state :
        [ `Annot of Annotated.stack parse_state
        | `Sexp of t list list parse_state ];
    }
    exception Parse_error of parse_error
    val parse :
      ?parse_pos:Parse_pos.t ->
      ?len:int -> string -> (string, t) parse_result
    val parse_bigstring :
      ?parse_pos:Parse_pos.t ->
      ?len:int -> bigstring -> (bigstring, t) parse_result
    val input_sexp : ?parse_pos:Parse_pos.t -> in_channel -> t
    val input_sexps :
      ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> t list
    val input_rev_sexps :
      ?parse_pos:Parse_pos.t -> ?buf:string -> in_channel -> t list
    val load_sexp : ?strict:bool -> ?buf:string -> string -> t
    val load_sexps : ?buf:string -> string -> t list
    val load_rev_sexps : ?buf:string -> string -> t list
    val load_sexp_conv :
      ?strict:bool -> ?buf:string -> string -> (t -> 'a) -> 'a Annotated.conv
    val load_sexp_conv_exn :
      ?strict:bool -> ?buf:string -> string -> (t -> 'a) -> 'a
    val load_sexps_conv :
      ?buf:string -> string -> (t -> 'a) -> 'a Annotated.conv list
    val load_sexps_conv_exn : ?buf:string -> string -> (t -> 'a) -> 'a list
    val output_hum : out_channel -> t -> unit
    val output_hum_indent : int -> out_channel -> t -> unit
    val output_mach : out_channel -> t -> unit
    val output : out_channel -> t -> unit
    val save_hum : ?perm:int -> string -> t -> unit
    val save_mach : ?perm:int -> string -> t -> unit
    val save : ?perm:int -> string -> t -> unit
    val save_sexps_hum : ?perm:int -> string -> t list -> unit
    val save_sexps_mach : ?perm:int -> string -> t list -> unit
    val save_sexps : ?perm:int -> string -> t list -> unit
    val pp_hum : Format.formatter -> t -> unit
    val pp_hum_indent : int -> Format.formatter -> t -> unit
    val pp_mach : Format.formatter -> t -> unit
    val pp : Format.formatter -> t -> unit
    module Of_string_conv_exn :
      sig
        type t = { exc : exn; sexp : Type.t; sub_sexp : Type.t; }
        exception E of t
      end
    val of_string : string -> t
    val of_string_conv : string -> (t -> 'a) -> 'a Annotated.conv
    val of_string_conv_exn : string -> (t -> 'a) -> 'a
    val of_bigstring : bigstring -> t
    val of_bigstring_conv : bigstring -> (t -> 'a) -> 'a Annotated.conv
    val of_bigstring_conv_exn : bigstring -> (t -> 'a) -> 'a
    val to_string_hum : ?indent:int -> t -> string
    val to_string_mach : t -> string
    val to_string : t -> string
    val to_buffer_hum : buf:Buffer.t -> ?indent:int -> t -> unit
    val to_buffer_mach : buf:Buffer.t -> t -> unit
    val to_buffer : buf:Buffer.t -> t -> unit
    val unit : t
    external sexp_of_t : t -> t = "%identity"
    external t_of_sexp : t -> t = "%identity"
    type found = [ `Found | `Pos of int * found ]
    type search_result = [ `Found | `Not_found | `Pos of int * found ]
    val search_physical : t -> contained:t -> search_result
    val subst_found : t -> subst:t -> found -> t
  end
