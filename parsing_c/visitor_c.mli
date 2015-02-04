open Ast_c

type visitor_c = {
  kexpr      : (expression  -> unit)  * visitor_c -> expression  -> unit;
  kstatement : (statement   -> unit)  * visitor_c -> statement   -> unit;
  ktype      : (fullType    -> unit)  * visitor_c -> fullType    -> unit;

  kdecl      : (declaration -> unit)  * visitor_c -> declaration -> unit;
  konedecl   : (onedecl -> unit)      * visitor_c -> onedecl     -> unit;
  kparam : (parameterType -> unit)      * visitor_c -> parameterType -> unit;
  kdef       : (definition  -> unit)  * visitor_c -> definition  -> unit;
  kname      : (name  -> unit)        * visitor_c -> name        -> unit;

  kini       : (initialiser -> unit)  * visitor_c -> initialiser -> unit;
  kfield     : (field -> unit)        * visitor_c -> field       -> unit;

  kcppdirective: (cpp_directive -> unit) * visitor_c -> cpp_directive -> unit;
  kifdefdirective : (ifdef_directive -> unit) * visitor_c -> ifdef_directive -> unit;
  kdefineval :   (define_val -> unit)    * visitor_c -> define_val    -> unit;
  kstatementseq: (statement_sequencable   -> unit) * visitor_c -> statement_sequencable   -> unit;


  knode:
    (Control_flow_c.node -> unit) * visitor_c -> Control_flow_c.node -> unit;
  ktoplevel: (toplevel -> unit) * visitor_c -> toplevel -> unit;
  kfragment: (string_fragment -> unit) * visitor_c -> string_fragment -> unit;
  kformat: (string_format -> unit) * visitor_c -> string_format -> unit;

  kinfo      : (info        -> unit)  * visitor_c -> info        -> unit;
}

val default_visitor_c : visitor_c

val vk_expr      : visitor_c -> expression  -> unit
val vk_statement : visitor_c -> statement   -> unit
val vk_statement_sequencable : visitor_c -> statement_sequencable -> unit
val vk_type      : visitor_c -> fullType    -> unit
val vk_decl      : visitor_c -> declaration -> unit
val vk_decl_list : visitor_c -> declaration list -> unit
val vk_onedecl   : visitor_c -> onedecl -> unit
val vk_ini       : visitor_c -> initialiser -> unit
val vk_ini_list  : visitor_c -> initialiser wrap2 list -> unit
val vk_inis_splitted :
    visitor_c -> (initialiser, il) Common.either list -> unit
val vk_name      : visitor_c -> name -> unit
val vk_def       : visitor_c -> definition  -> unit
val vk_node      : visitor_c -> Control_flow_c.node -> unit
val vk_string_fragment : visitor_c -> string_fragment -> unit
val vk_string_fragments : visitor_c -> string_fragment list -> unit
val vk_string_fragments_splitted :
    visitor_c -> (string_fragment, il) Common.either list -> unit
val vk_string_format : visitor_c -> string_format -> unit
val vk_info      : visitor_c -> info -> unit
val vk_toplevel : visitor_c -> toplevel -> unit
val vk_program  : visitor_c -> program -> unit

val vk_argument : visitor_c -> argument -> unit
val vk_argument_list : visitor_c -> argument wrap2 list -> unit
val vk_args_splitted : visitor_c -> (argument, il) Common.either list -> unit
val vk_param : visitor_c -> parameterType -> unit
val vk_param_list : visitor_c -> parameterType wrap2 list -> unit
val vk_params_splitted :
  visitor_c -> (parameterType, il) Common.either list -> unit

val vk_struct_field : visitor_c -> field -> unit
val vk_struct_fields : visitor_c -> field list -> unit
val vk_struct_fieldkinds : visitor_c -> fieldkind wrap list -> unit
val vk_enum_fields : visitor_c -> enumType -> unit
val vk_enum_fields_splitted :
    visitor_c -> (oneEnumType, il) Common.either list -> unit

val vk_cst : visitor_c -> ((constant, string) Common.either wrap) -> unit

val vk_define_params_splitted :
  visitor_c -> (string Ast_c.wrap, il) Common.either list -> unit
val vk_pragmainfo : visitor_c -> pragmainfo -> unit
val vk_ident_list_splitted : visitor_c -> (name, il) Common.either list -> unit

val vk_exec_code_list_splitted :
    visitor_c -> (exec_code, il) Common.either list -> unit
val vk_attrs_splitted :
    visitor_c -> (attribute, il) Common.either list -> unit


(* ------------------------------------------------------------------------ *)
type 'a inout = 'a -> 'a
type visitor_c_s = {
  kexpr_s      : expression     inout * visitor_c_s -> expression     inout;
  kstatement_s : statement      inout * visitor_c_s -> statement      inout;
  ktype_s      : fullType       inout * visitor_c_s -> fullType       inout;

  kdecl_s      : declaration    inout * visitor_c_s -> declaration    inout;
  kdef_s       : definition     inout * visitor_c_s -> definition     inout;
  kname_s      : name           inout * visitor_c_s -> name           inout;

  kini_s       : initialiser    inout * visitor_c_s -> initialiser    inout;

  kcppdirective_s : (cpp_directive inout * visitor_c_s) -> cpp_directive inout;
  kifdefdirective_s : (ifdef_directive inout * visitor_c_s) -> ifdef_directive inout;
  kdefineval_s : (define_val inout * visitor_c_s) -> define_val inout;
  kstatementseq_s: (statement_sequencable inout * visitor_c_s) -> statement_sequencable inout;
  kstatementseq_list_s:
    (statement_sequencable list inout * visitor_c_s) -> statement_sequencable list inout;

  knode_s      :
    Control_flow_c.node inout * visitor_c_s -> Control_flow_c.node    inout;
  ktoplevel_s  : toplevel inout * visitor_c_s -> toplevel inout;

  kfragment_s  : string_fragment inout * visitor_c_s -> string_fragment inout;
  kformat_s    : string_format   inout * visitor_c_s -> string_format   inout;
  kinfo_s      : info            inout * visitor_c_s -> info            inout;
  }

val default_visitor_c_s : visitor_c_s

val vk_expr_s : visitor_c_s -> expression -> expression
val vk_argument_s : visitor_c_s -> argument -> argument
val vk_statement_s : visitor_c_s -> statement -> statement
val vk_statement_sequencable_s : visitor_c_s -> statement_sequencable -> statement_sequencable
val vk_type_s : visitor_c_s -> fullType -> fullType
val vk_decl_s : visitor_c_s -> declaration -> declaration
val vk_decl_list_s : visitor_c_s -> declaration list -> declaration list
val vk_ini_s : visitor_c_s -> initialiser -> initialiser

val vk_inis_splitted_s :
  visitor_c_s ->
  (initialiser, il) Common.either list ->
  (initialiser, il) Common.either list

val vk_def_s : visitor_c_s -> definition -> definition
val vk_name_s : visitor_c_s -> name -> name
val vk_toplevel_s : visitor_c_s -> toplevel -> toplevel
val vk_string_fragment_s : visitor_c_s -> string_fragment -> string_fragment
val vk_string_fragments_s :
    visitor_c_s -> string_fragment list -> string_fragment list
val vk_string_fragments_splitted_s :
    visitor_c_s ->
      (string_fragment, il) Common.either list ->
	(string_fragment, il) Common.either list
val vk_string_format_s : visitor_c_s -> string_format -> string_format
val vk_info_s : visitor_c_s -> info -> info
val vk_ii_s : visitor_c_s -> info list -> info list
val vk_node_s : visitor_c_s -> Control_flow_c.node -> Control_flow_c.node
val vk_program_s  : visitor_c_s -> program -> program

val vk_arguments_s : visitor_c_s -> argument wrap2 list -> argument wrap2 list

val vk_inis_s : visitor_c_s -> initialiser wrap2 list -> initialiser wrap2 list

val vk_args_splitted_s :
  visitor_c_s ->
  (argument, il) Common.either list ->
  (argument, il) Common.either list

val vk_params_s :
  visitor_c_s ->
  parameterType wrap2 list -> parameterType wrap2 list

val vk_params_splitted_s :
  visitor_c_s ->
  (parameterType, il) Common.either list ->
  (parameterType, il) Common.either list


val vk_param_s : visitor_c_s -> parameterType -> parameterType

val vk_define_params_splitted_s :
  visitor_c_s ->
  (string Ast_c.wrap, il) Common.either list ->
  (string Ast_c.wrap, il) Common.either list

val vk_pragmainfo_s : visitor_c_s -> pragmainfo -> pragmainfo
val vk_ident_list_splitted_s :
  visitor_c_s ->
  (name, il) Common.either list ->
  (name, il) Common.either list

val vk_enum_fields_s : visitor_c_s -> enumType -> enumType
val vk_enum_fields_splitted_s : visitor_c_s ->
  (oneEnumType, il) Common.either list ->
  (oneEnumType, il) Common.either list
val vk_struct_field_s : visitor_c_s -> field -> field
val vk_struct_fields_s : visitor_c_s -> field list -> field list

val vk_exec_code_list_splitted_s :
    visitor_c_s ->
      (exec_code, il) Common.either list ->
	(exec_code, il) Common.either list
val vk_attrs_splitted_s :
    visitor_c_s ->
      (attribute, il) Common.either list ->
	(attribute, il) Common.either list

val vk_cst_s : visitor_c_s -> ((constant, string) Common.either wrap) inout
