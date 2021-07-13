val sgrep_mode2 : bool ref
val show_misc : bool ref
val show_transinfo : bool ref
val show_trying : bool ref
val track_iso_usage : bool ref
val worth_trying_opt : bool ref
type scanner = IdUtils | Glimpse | CocciGrep | GitGrep | PatchDiff | NoScanner
val scanner : scanner ref
val pyoutput : string ref
val ocamlc : string ref
val ocamlopt : string ref
val ocamldep : string ref
val ocamlfind : string ref
val patch : string option ref
val make_hrule : string option ref
val hrule_per_file : bool ref
val currentfile : string option ref
val currentfiles : string list ref
val current_element : string ref
val current_element_pos : ((int * int) * (int * int)) Lazy.t ref
val dir : string ref
val defined_virtual_rules : string list ref
val defined_virtual_env : (string * string) list ref
val set_defined_virtual_rules : string -> unit
type c_plus_plus = Off | On of int option (* release year of the version *)
val c_plus_plus : c_plus_plus ref
val set_c_plus_plus : string option -> unit
val ibm : bool ref
val include_headers : bool ref
val no_include_cache : bool ref
val parmap_cores      : int option ref
val parmap_chunk_size : int option ref
exception UnreadableFile of string
val cocci_attribute_names : string list ref
val add_cocci_attribute_names : string -> unit
