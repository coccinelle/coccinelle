val tuple_of_size_n_expected : string -> int -> Pre_sexp.t -> 'a
val stag_no_args : string -> Pre_sexp.t -> 'a
val stag_incorrect_n_args : string -> string -> Pre_sexp.t -> 'a
val stag_takes_args : string -> Pre_sexp.t -> 'a
val nested_list_invalid_sum : string -> Pre_sexp.t -> 'a
val empty_list_invalid_sum : string -> Pre_sexp.t -> 'a
val unexpected_stag : string -> Pre_sexp.t -> 'a
val record_only_pairs_expected : string -> Pre_sexp.t -> 'a
val record_superfluous_fields :
  what:string -> loc:string -> string list -> Pre_sexp.t -> 'a
val record_duplicate_fields : string -> string list -> Pre_sexp.t -> 'a
val record_extra_fields : string -> string list -> Pre_sexp.t -> 'a
val record_get_undefined_loop : string list -> (bool * string) list -> string
val record_undefined_elements :
  string -> Pre_sexp.t -> (bool * string) list -> 'a
val record_list_instead_atom : string -> Pre_sexp.t -> 'a
val record_poly_field_value : string -> Pre_sexp.t -> 'a
exception No_variant_match of string * Sexp.t
val no_variant_match : string -> Sexp.t -> 'a
val no_matching_variant_found : string -> Pre_sexp.t -> 'a
val ptag_no_args : string -> Pre_sexp.t -> 'a
val ptag_incorrect_n_args : string -> string -> Pre_sexp.t -> 'a
val nested_list_invalid_poly_var : string -> Pre_sexp.t -> 'a
val empty_list_invalid_poly_var : string -> Pre_sexp.t -> 'a
val silly_type : string -> Pre_sexp.t -> 'a
val empty_type : string -> Pre_sexp.t -> 'a
