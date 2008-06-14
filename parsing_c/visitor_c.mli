open Ast_c

type visitor_c = {
  kexpr      : (expression  -> unit)  * visitor_c -> expression  -> unit;
  kstatement : (statement   -> unit)  * visitor_c -> statement   -> unit;
  ktype      : (fullType    -> unit)  * visitor_c -> fullType    -> unit;
  kdecl      : (declaration -> unit)  * visitor_c -> declaration -> unit;
  kdef       : (definition  -> unit)  * visitor_c -> definition  -> unit;
  kini       : (initialiser -> unit)  * visitor_c -> initialiser -> unit;
  kinfo      : (info        -> unit)  * visitor_c -> info        -> unit;
  knode      :      
    (Control_flow_c.node -> unit) * visitor_c -> Control_flow_c.node -> unit;
  ktoplevel: (toplevel -> unit) * visitor_c -> toplevel -> unit;
}

val default_visitor_c : visitor_c

val vk_expr      : visitor_c -> expression  -> unit
val vk_statement : visitor_c -> statement   -> unit
val vk_type      : visitor_c -> fullType    -> unit
val vk_decl      : visitor_c -> declaration -> unit
val vk_ini       : visitor_c -> initialiser -> unit
val vk_def       : visitor_c -> definition  -> unit
val vk_node      : visitor_c -> Control_flow_c.node -> unit
val vk_info      : visitor_c -> info -> unit
val vk_toplevel :   visitor_c -> toplevel -> unit

val vk_argument : visitor_c -> argument -> unit

val vk_args_splitted : visitor_c -> (argument, il) Common.either list -> unit
val vk_param : visitor_c -> parameterType -> unit
val vk_params_splitted : 
  visitor_c -> (parameterType, il) Common.either list -> unit

val vk_struct_fields : visitor_c -> field wrap list -> unit
val vk_struct_field : visitor_c -> fieldkind wrap list -> unit

val vk_cst : visitor_c -> ((constant, string) Common.either wrap) -> unit

val vk_define_params_splitted : 
  visitor_c -> (string Ast_c.wrap, il) Common.either list -> unit




type 'a inout = 'a -> 'a
type visitor_c_s = {
  kexpr_s      : expression     inout * visitor_c_s -> expression     inout;
  kstatement_s : statement      inout * visitor_c_s -> statement      inout;
  ktype_s      : fullType       inout * visitor_c_s -> fullType       inout;
  kini_s       : initialiser    inout * visitor_c_s -> initialiser    inout;
  kdecl_s      : declaration    inout * visitor_c_s -> declaration    inout;
  kdef_s       : definition     inout * visitor_c_s -> definition     inout;
  ktoplevel_s  : toplevel inout * visitor_c_s -> toplevel inout;
  knode_s      : 
    Control_flow_c.node inout * visitor_c_s -> Control_flow_c.node    inout;
  kdefineval_s : (define_val inout * visitor_c_s) -> define_val inout;
  kinfo_s      : info           inout * visitor_c_s -> info           inout;
  }

val default_visitor_c_s : visitor_c_s

val vk_expr_s : visitor_c_s -> expression -> expression
val vk_argument_s : visitor_c_s -> argument -> argument
val vk_statement_s : visitor_c_s -> statement -> statement
val vk_type_s : visitor_c_s -> fullType -> fullType
val vk_decl_s : visitor_c_s -> declaration -> declaration
val vk_ini_s : visitor_c_s -> initialiser -> initialiser
val vk_def_s : visitor_c_s -> definition -> definition
val vk_toplevel_s : visitor_c_s -> toplevel -> toplevel
val vk_info_s : visitor_c_s -> info -> info
val vk_node_s : visitor_c_s -> Control_flow_c.node -> Control_flow_c.node

val vk_arguments_s : 
  visitor_c_s -> 
  argument wrap2 list -> argument wrap2 list

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

val vk_struct_fields_s : visitor_c_s -> 
  field wrap list -> field wrap list

val vk_cst_s : visitor_c_s -> ((constant, string) Common.either wrap) inout 
