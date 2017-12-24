val path : string ref
val macro_dir : string
val mk_macro_path : cocci_path:string -> string -> string
val std_h : string ref
val common_h : string ref
val cmdline_flags_macrofile : unit -> (string * Arg.spec * string) list
val cpp_i_opts : string list ref
val cpp_d_opts : string list ref
val cmdline_flags_cpp : unit -> (string * Arg.spec * string) list
val show_parsing_error : bool ref
val verbose_lexing : bool ref
val verbose_parsing : bool ref
val verbose_type : bool ref
val verbose_cfg : bool ref
val verbose_annotater : bool ref
val verbose_unparsing : bool ref
val verbose_visit : bool ref
val verbose_cpp_ast : bool ref
val verbose_includes : bool ref
val filter_msg : bool ref
val filter_msg_define_error : bool ref
val filter_define_error : bool ref
val filter_passed_level : int ref
val pretty_print_type_info : bool ref
val pretty_print_comment_info : bool ref
val pretty_print_typedef_value : bool ref
val show_flow_labels : bool ref
val cmdline_flags_verbose : unit -> (string * Arg.spec * string) list
val debug_lexer : bool ref
val debug_etdt : bool ref
val debug_typedef : bool ref
val debug_cpp : bool ref
val debug_cpp_ast : bool ref
val debug_unparsing : bool ref
val debug_cfg : bool ref
val cmdline_flags_debugging : unit -> (string * Arg.spec * string) list
val check_annotater : bool ref
val cmdline_flags_checks : unit -> (string * Arg.spec * string) list
val label_strategy_2 : bool ref
val cmdline_flags_algos : unit -> (string * Arg.spec * string) list
val cpp_directive_passing : bool ref
val ifdef_directive_passing : bool ref
val ifdef_to_if : bool ref
val disable_multi_pass : bool ref
val disable_add_typedef : bool ref
val if0_passing : bool ref
val add_typedef_root : bool ref
val exts_ITU : bool ref
val add : string list ref -> string -> unit
val defined : string list ref
val undefined : string list ref
val cmdline_flags_parsing_algos : unit -> (string * Arg.spec * string) list
val diff_lines : string option ref
val use_cache : bool ref
val cache_prefix : string option ref
val cache_limit : int option ref
val cmdline_flags_other : unit -> (string * Arg.spec * string) list
val int_thresholds : (int * int * string * string * string * string) option ref
val set_int_bits : int -> unit
val set_long_bits : int -> unit
type spacing = LINUX | SMPL
val spacing : spacing ref
val indent : int ref
val set_linux_spacing : 'a -> unit
val set_smpl_spacing : 'a -> unit
val max_width : int ref
val no_loops : bool ref
val no_gotos : bool ref
val keep_comments : bool ref
val parsing_header_for_types : bool ref
val force_kr : bool ref
val prevent_kr : bool ref
