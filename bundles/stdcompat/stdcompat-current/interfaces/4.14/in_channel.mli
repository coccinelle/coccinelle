type t = in_channel
type open_flag = open_flag =
  | Open_rdonly 
  | Open_wronly 
  | Open_append 
  | Open_creat 
  | Open_trunc 
  | Open_excl 
  | Open_binary 
  | Open_text 
  | Open_nonblock 
val stdin : t
val open_bin : string -> t
val open_text : string -> t
val open_gen : open_flag list -> int -> string -> t
val with_open_bin : string -> (t -> 'a) -> 'a
val with_open_text : string -> (t -> 'a) -> 'a
val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64
val close : t -> unit
val close_noerr : t -> unit
val input_char : t -> char option
val input_byte : t -> int option
val input_line : t -> string option
val input : t -> bytes -> int -> int -> int
val really_input : t -> bytes -> int -> int -> unit option
val really_input_string : t -> int -> string option
val input_all : t -> string
val set_binary_mode : t -> bool -> unit
