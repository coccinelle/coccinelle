val pr2 : string -> unit
val pr2_once : string -> unit
val strip_info_visitor : 'a -> Visitor_c.visitor_c_s
val al_expr : Ast_c.expression -> Ast_c.expression
val al_declaration : Ast_c.declaration -> Ast_c.declaration
val al_field : Ast_c.field -> Ast_c.field
val al_statement : Ast_c.statement -> Ast_c.statement
val al_statement_seq_list :
  Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
val al_type : Ast_c.fullType -> Ast_c.fullType
val al_init : Ast_c.initialiser -> Ast_c.initialiser
val al_inits :
  Ast_c.initialiser Ast_c.wrap2 list -> Ast_c.initialiser Ast_c.wrap2 list
val al_param : Ast_c.parameterType -> Ast_c.parameterType
val al_params :
  Ast_c.parameterType Ast_c.wrap2 list ->
  Ast_c.parameterType Ast_c.wrap2 list
val al_define_params :
  string Ast_c.wrap Ast_c.wrap2 list -> string Ast_c.wrap Ast_c.wrap2 list
val al_arguments :
  Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
val al_fields : Ast_c.field list -> Ast_c.field list
val al_name : Ast_c.name -> Ast_c.name
val al_string_format : Ast_c.string_format -> Ast_c.string_format
val al_attribute : Ast_c.attribute -> Ast_c.attribute
val al_attr_arg : Ast_c.attr_arg -> Ast_c.attr_arg
val al_string_fragments :
  Ast_c.string_fragment list -> Ast_c.string_fragment list
val al_node : Control_flow_c.node -> Control_flow_c.node
val al_program : Ast_c.toplevel list -> Ast_c.toplevel list
val al_ii : Ast_c.info list -> Ast_c.info list
val strip_inh_info_visitor : 'a -> Visitor_c.visitor_c_s
val al_inh_expr : Ast_c.expression -> Ast_c.expression
val al_inh_declaration : Ast_c.declaration -> Ast_c.declaration
val al_inh_field : Ast_c.field -> Ast_c.field
val al_inh_field_list : Ast_c.field list -> Ast_c.field list
val al_inh_statement : Ast_c.statement -> Ast_c.statement
val al_inh_statement_seq_list :
  Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
val al_inh_type : Ast_c.fullType -> Ast_c.fullType
val al_inh_init : Ast_c.initialiser -> Ast_c.initialiser
val al_inh_inits :
  Ast_c.initialiser Ast_c.wrap2 list -> Ast_c.initialiser Ast_c.wrap2 list
val al_inh_arguments :
  Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
val al_inh_string_format : Ast_c.string_format -> Ast_c.string_format
val al_inh_string_fragments :
  Ast_c.string_fragment list -> Ast_c.string_fragment list
val al_inh_attribute : Ast_c.attribute -> Ast_c.attribute
val al_inh_attr_arg : Ast_c.attr_arg -> Ast_c.attr_arg
val semi_strip_info_visitor : Visitor_c.visitor_c_s
val semi_al_expr : Ast_c.expression -> Ast_c.expression
val semi_al_declaration : Ast_c.declaration -> Ast_c.declaration
val semi_al_field : Ast_c.field -> Ast_c.field
val semi_al_fields : Ast_c.field list -> Ast_c.field list
val semi_al_statement : Ast_c.statement -> Ast_c.statement
val semi_al_statement_seq_list :
  Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
val semi_al_type : Ast_c.fullType -> Ast_c.fullType
val semi_al_init : Ast_c.initialiser -> Ast_c.initialiser
val semi_al_inits :
  Ast_c.initialiser Ast_c.wrap2 list -> Ast_c.initialiser Ast_c.wrap2 list
val semi_al_param : Ast_c.parameterType -> Ast_c.parameterType
val semi_al_params :
  Ast_c.parameterType Ast_c.wrap2 list ->
  Ast_c.parameterType Ast_c.wrap2 list
val semi_al_define_params :
  string Ast_c.wrap Ast_c.wrap2 list -> string Ast_c.wrap Ast_c.wrap2 list
val semi_al_arguments :
  Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
val semi_al_string_format : Ast_c.string_format -> Ast_c.string_format
val semi_al_string_fragments :
  Ast_c.string_fragment list -> Ast_c.string_fragment list
val semi_al_attribute : Ast_c.attribute -> Ast_c.attribute
val semi_al_attr_arg  : Ast_c.attr_arg -> Ast_c.attr_arg
val semi_al_program : Ast_c.toplevel list -> Ast_c.toplevel list
val real_strip_info_visitor : 'a -> Visitor_c.visitor_c_s
val real_al_expr : Ast_c.expression -> Ast_c.expression
val real_al_arguments :
  Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
val real_al_node : Control_flow_c.node -> Control_flow_c.node
val real_al_type : Ast_c.fullType -> Ast_c.fullType
val real_al_binop : Ast_c.binaryOp -> Ast_c.binaryOp
val real_al_assignop : Ast_c.assignOp -> Ast_c.assignOp
val real_al_decl : Ast_c.declaration -> Ast_c.declaration
val real_al_init : Ast_c.initialiser -> Ast_c.initialiser
val real_al_inits :
  Ast_c.initialiser Ast_c.wrap2 list -> Ast_c.initialiser Ast_c.wrap2 list
val real_al_statement : Ast_c.statement -> Ast_c.statement
val real_al_statement_seq_list :
  Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
val real_al_def : Ast_c.toplevel -> Ast_c.toplevel

val real_al_decl_with_comments : Ast_c.declaration -> Ast_c.declaration
val real_al_statement_with_comments : Ast_c.statement -> Ast_c.statement

val extract_info_visitor :
  (Visitor_c.visitor_c -> 'a -> 'b) -> 'a -> Ast_c.info list
val ii_of_def : Ast_c.definition -> Ast_c.info list
val ii_of_decl : Ast_c.declaration -> Ast_c.info list
val ii_of_field : Ast_c.field -> Ast_c.info list
val ii_of_node : Control_flow_c.node -> Ast_c.info list
val ii_of_expr : Ast_c.expression -> Ast_c.info list
val ii_of_assignOp : Ast_c.assignOp -> Ast_c.info list
val ii_of_binaryOp : Ast_c.binaryOp -> Ast_c.info list
val ii_of_stmt : Ast_c.statement -> Ast_c.info list
val ii_of_stmtseq : Ast_c.statement_sequencable -> Ast_c.info list
val ii_of_stmtseqlist : Ast_c.statement_sequencable list -> Ast_c.info list
val ii_of_args :
  (Ast_c.argument, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_type : Ast_c.fullType -> Ast_c.info list
val ii_of_ini : Ast_c.initialiser -> Ast_c.info list
val ii_of_inis :
  (Ast_c.initialiser, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_param : Ast_c.parameterType -> Ast_c.info list
val ii_of_params :
  (Ast_c.parameterType, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_enum_fields :
  (Ast_c.oneEnumType, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_struct_fields : Ast_c.field list -> Ast_c.info list
val ii_of_struct_fieldkinds :
  Ast_c.fieldkind Ast_c.wrap list -> Ast_c.info list
val ii_of_cst :
  (Ast_c.constant, string) Common.either Ast_c.wrap -> Ast_c.info list
val ii_of_fragments :
  (Ast_c.string_fragment, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_format : Ast_c.string_format -> Ast_c.info list
val ii_of_define_params :
  (string Ast_c.wrap, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_ident_list :
  (Ast_c.name, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_exec_code_list :
  (Ast_c.exec_code, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_attr : Ast_c.attribute -> Ast_c.info list
val ii_of_attr_arg : Ast_c.attr_arg -> Ast_c.info list
val ii_of_attrs :
  (Ast_c.attribute, Ast_c.il) Common.either list -> Ast_c.info list
val ii_of_toplevel : Ast_c.toplevel -> Ast_c.info list
val max_min_ii_by_pos : Ast_c.info list -> Ast_c.info * Ast_c.info
val max_min_ii_by_pos_filtered :
    (Ast_c.info -> bool) -> Ast_c.info list -> Ast_c.info * Ast_c.info
val info_to_fixpos : Ast_c.info -> Ast_cocci.fixpos
val max_min_by_pos : Ast_c.info list -> Ast_cocci.fixpos * Ast_cocci.fixpos
val lin_col_by_pos :
  Ast_c.info list ->
  Common.filename * string * Ast_c.posl * Ast_c.posl
val min_pinfo_of_node : Control_flow_c.node -> Common.parse_info
val range_of_origin_ii : Ast_c.info list -> (int * int) option
val names_of_parameters_in_def : Ast_c.definitionbis -> string list
val names_of_parameters_in_macro : (('a * 'b) * 'c) list -> 'a list
val stmt_elems_of_sequencable :
  Ast_c.statement_sequencable list -> Ast_c.statement list
