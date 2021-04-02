val contains_string_constant : bool ref
type info = Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt
type midinfo =
    Ast_cocci.meta_name * Ast0_cocci.constraints * Ast_cocci.seed * Ast0_cocci.pure * Data.clt
type cstrinfo = Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure * Data.clt
type assignOpinfo = Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure * Data.clt
type binaryOpinfo = Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure * Data.clt
type expinfo = Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure * Data.clt
type tyinfo = Ast_cocci.meta_name * Ast0_cocci.typeC list * Ast0_cocci.pure * Data.clt
type list_info =
    Ast_cocci.meta_name * Ast_cocci.list_len * Ast0_cocci.constraints * Ast0_cocci.pure * Data.clt
type typed_expinfo =
    Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure * Ast0_cocci.typeC list option *
    Data.clt
type typed_expinfo_bitfield =
    Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure * Ast0_cocci.typeC list option *
    Data.clt * Ast_cocci.list_len option
type pos_info =
    Ast_cocci.meta_name * Ast0_cocci.constraints * Ast_cocci.meta_collect * Data.clt
type com_info = Ast_cocci.meta_name * Ast0_cocci.constraints * Data.clt
val make_info :
  int ->
  int ->
  int ->
  int ->
  int ->
  (Ast_cocci.added_string * Ast0_cocci.position_info) list ->
  (Ast_cocci.added_string * Ast0_cocci.position_info) list ->
  bool -> string -> Ast0_cocci.info
val clt2info :
  'a * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * 'b * string ->
  Ast0_cocci.info
val drop_bef :
  'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j ->
  'a * 'b * 'c * 'd * 'e * 'f * 'k list * 'h * 'i * 'j
val drop_aft :
  'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j ->
  'a * 'b * 'c * 'd * 'e * 'f * 'g * 'k list * 'i * 'j
val get_aft : 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'h
val set_aft :
  'a ->
  'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k ->
  'b * 'c * 'd * 'e * 'f * 'g * 'h * 'a * 'j * 'k
val drop_pos :
  'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j ->
  'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'k list * 'j
val clt2mcode_ext :
  'a ->
  bool ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * 'b * string ->
  'a * Ast0_cocci.arity * Ast0_cocci.info * Ast0_cocci.mcodekind * 'b ref * int
val clt2mcode :
  'a ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * 'b * string ->
  'a * Ast0_cocci.arity * Ast0_cocci.info * Ast0_cocci.mcodekind * 'b ref * int
val id3name : 'a * 'b * 'c -> 'a
val id2name : 'a * 'b -> 'a
val id2clt : 'a * 'b -> 'b
val id2mcode :
  'a *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * 'b * string) ->
  'a * Ast0_cocci.arity * Ast0_cocci.info * Ast0_cocci.mcodekind * 'b ref * int
val sym2mcode :
  'a *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * 'b * string) ->
  'a * Ast0_cocci.arity * Ast0_cocci.info * Ast0_cocci.mcodekind * 'b ref * int
val mkdots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) *
  (Ast0_cocci.statement Ast0_cocci.dots, Ast0_cocci.statement) Ast0_cocci.whencode list ->
  Ast0_cocci.base_statement Ast0_cocci.wrap
val mkedots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) *
  (string Ast0_cocci.mcode * string Ast0_cocci.mcode * Ast0_cocci.expression) option ->
  Ast0_cocci.base_expression Ast0_cocci.wrap
val mkdpdots :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_define_param Ast0_cocci.wrap
val mkidots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) *
  (string Ast0_cocci.mcode * string Ast0_cocci.mcode * Ast0_cocci.initialiser) option ->
  Ast0_cocci.base_initialiser Ast0_cocci.wrap
val mkfdots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) *
  (string Ast0_cocci.mcode * string Ast0_cocci.mcode * Ast0_cocci.field) list option ->
  Ast0_cocci.base_field Ast0_cocci.wrap
val mkfdots_one :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) *
  (string Ast0_cocci.mcode * string Ast0_cocci.mcode * Ast0_cocci.field) option ->
  Ast0_cocci.base_field Ast0_cocci.wrap
val mkpdots :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_parameterTypeDef Ast0_cocci.wrap
val mkenumdots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) *
  (string Ast0_cocci.mcode * string Ast0_cocci.mcode * Ast0_cocci.enum_decl)
  option ->
  Ast0_cocci.base_enum_decl Ast0_cocci.wrap
val arith_op :
  Ast_cocci.arithOp ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.expression -> Ast0_cocci.base_expression Ast0_cocci.wrap
val logic_op :
  Ast_cocci.logicalOp ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.expression -> Ast0_cocci.base_expression Ast0_cocci.wrap
val make_cv :
  Ast_cocci.const_vol Ast0_cocci.mcode option -> Ast0_cocci.typeC -> Ast0_cocci.typeC
val make_attr:
  Ast0_cocci.attr_arg -> Ast0_cocci.attr
val make_gcc_attr:
  Data.clt -> Data.clt -> Data.clt -> Ast0_cocci.attr_arg ->
  Data.clt -> Data.clt -> Ast0_cocci.attr
val top_dots : 'a -> 'a Ast0_cocci.wrap
val pointerify :
  Ast0_cocci.typeC ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string)
  list -> Ast0_cocci.typeC
val ty_pointerify : Ast0_cocci.typeC -> 'a list -> Ast0_cocci.typeC
val arrayify :
  Ast0_cocci.typeC ->
  ((Data.line_type * int * int * int * int * int *
    (Ast_cocci.added_string * Ast0_cocci.position_info) list *
    (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
    string) *
   Ast0_cocci.expression option *
   (Data.line_type * int * int * int * int * int *
    (Ast_cocci.added_string * Ast0_cocci.position_info) list *
    (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
    string))
  list -> Ast0_cocci.typeC
val iso_adjust :
  ('a -> 'b) ->
  ('c -> 'b) -> 'a -> ('c, 'c) Common.either list -> 'b list list
val lookup : string -> string -> Ast_cocci.metavar
val meta_lookup : string -> string -> Ast_cocci.metavar -> Ast_cocci.metavar
val check_meta_tyopt : bool -> Ast_cocci.metavar -> unit
val check_meta : Ast_cocci.metavar -> unit
val check_inherited_constraint_without_type : 'a option * 'b -> 'a * 'b
val check_inherited_constraint :
  'a option * 'b -> ('a * 'b -> Ast_cocci.metavar) -> 'a * 'b
val create_metadec :
  'a ->
  'b ->
  ('a ->
   'c * 'd ->
   'b ->
   (Ast_cocci.metavar -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list) -> 'e list) ->
  ('c option * 'd) list -> 'c -> 'e list
val create_metadec_virt :
  'a ->
  'b ->
  ('a ->
   'c ->
   'b ->
   ('d -> ('e, 'd) Common.either list) -> (string * string) list -> 'f list) ->
  'c list -> 'g -> 'f list
val create_fresh_metadec :
  ('a * 'b ->
   (Ast_cocci.metavar -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list) ->
   'c -> 'd list) ->
  (('a option * 'b) * 'c) list -> 'a -> 'd list
val create_metadec_with_constraints :
  'a ->
  'b ->
  ('a ->
   'c * 'd ->
   'b ->
   (Ast_cocci.metavar -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list) ->
   'e -> 'f list) ->
  (('c option * 'd) * 'e) list -> 'c -> 'f list
val create_metadec_ty :
  'a ->
  'b ->
  ('a ->
   'c * 'd ->
   'b ->
   (Ast_cocci.metavar -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list) ->
   'e -> 'f list) ->
  (('c option * 'd) * 'e) list -> 'c -> 'f list
val create_len_metadec :
  'a ->
  'b ->
  (Ast_cocci.list_len ->
   'a ->
   string * 'c ->
   'b ->
   (Ast_cocci.metavar -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list) ->
   'd -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list) ->
  ((string option * string) * Ast0_cocci.constraints, int) Common.either ->
  ((string option * 'c) * 'd) list ->
  string -> (Ast_cocci.metavar, Ast_cocci.metavar) Common.either list
val str2inc : string -> Ast_cocci.inc_elem list
val meta_decl :
  Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) ->
  Ast0_cocci.base_declaration Ast0_cocci.wrap
val meta_field :
  Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) ->
  Ast0_cocci.base_field Ast0_cocci.wrap
val dolen :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast_cocci.list_len -> Ast0_cocci.listlen
val meta_field_list :
  Ast_cocci.meta_name * Ast_cocci.list_len * Ast0_cocci.constraints * Ast0_cocci.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) ->
  Ast0_cocci.base_field Ast0_cocci.wrap
val meta_stm :
  Ast_cocci.meta_name * Ast0_cocci.constraints * Ast0_cocci.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) ->
  Ast0_cocci.base_statement Ast0_cocci.wrap
val meta_stm_list :
  Ast_cocci.meta_name * Ast_cocci.list_len * Ast0_cocci.constraints * Ast0_cocci.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) ->
  Ast0_cocci.base_statement Ast0_cocci.wrap
val meta_dparam_list :
  Ast_cocci.meta_name * Ast_cocci.list_len * Ast0_cocci.constraints * Ast0_cocci.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list *
   (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
   string) ->
  Ast0_cocci.base_define_param Ast0_cocci.wrap
val exp_stm :
  Ast0_cocci.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val make_fake_mcode : 'a -> Ast0_cocci.info * Ast0_cocci.mcodekind * int
val ifthen :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.statement -> Ast0_cocci.base_statement Ast0_cocci.wrap
val ifthenelse :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.statement ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.statement -> Ast0_cocci.base_statement Ast0_cocci.wrap
val forloop :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.statement -> Ast0_cocci.base_statement Ast0_cocci.wrap
val forloop2 :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.declaration ->
  Ast0_cocci.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.statement -> Ast0_cocci.base_statement Ast0_cocci.wrap
val whileloop :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.statement -> Ast0_cocci.base_statement Ast0_cocci.wrap
val doloop :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.statement ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val iterator :
  Ast0_cocci.ident ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression Ast0_cocci.dots ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.statement -> Ast0_cocci.base_statement Ast0_cocci.wrap
val switch :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.declaration list ->
  Ast0_cocci.case_line list ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val ret_exp :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val ret :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val break :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val cont :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val label :
  Ast0_cocci.ident ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val goto :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.ident ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val seq :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string ->
  Ast0_cocci.statement Ast0_cocci.dots ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_statement Ast0_cocci.wrap
val check_rule_name : (string * 'a) option -> string option
val make_iso_rule_name_result : string -> Ast_cocci.rulename
val fix_dependencies : Ast0_cocci.dependency -> Ast_cocci.dependency
val make_cocci_rule_name_result :
  (string * 'a) option ->
  Ast0_cocci.dependency ->
  string list -> string list -> Ast_cocci.exists -> Ast_cocci.parser_kind -> Ast_cocci.rulename
val make_generated_rule_name_result :
  (string * 'a) option ->
  Ast0_cocci.dependency ->
  string list -> string list -> Ast_cocci.exists -> Ast_cocci.parser_kind -> Ast_cocci.rulename
val make_script_rule_name_result :
  string * 'a -> (string * 'b) option -> Ast0_cocci.dependency -> Ast_cocci.rulename
val make_initial_script_rule_name_result :
  string * 'a -> Ast0_cocci.dependency -> Ast_cocci.rulename
val make_final_script_rule_name_result :
  string * 'a -> Ast0_cocci.dependency -> Ast_cocci.rulename
val struct_initializer :
  Ast0_cocci.base_initialiser Ast0_cocci.wrap list Ast0_cocci.wrap -> bool
val drop_dot_commas :
  Ast0_cocci.base_initialiser Ast0_cocci.wrap list Ast0_cocci.wrap ->
  Ast0_cocci.base_initialiser Ast0_cocci.wrap list Ast0_cocci.wrap
type metavars =
    MFrag of (string Ast0_cocci.mcode -> Ast0_cocci.string_fragment)
  | MFmt of Ast0_cocci.string_format
val string_metavariables :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> metavars
val pct_split : string -> string list
val parse_middle :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast0_cocci.base_string_fragment Ast0_cocci.wrap list
val check_no_duplicates : Ast0_cocci.base_string_fragment Ast0_cocci.wrap list -> unit
val update_line :
  'a * int * int * int * int * int * 'b * 'c * 'd * 'e ->
  int -> 'a * int * int * int * int * int * 'b * 'c * 'd * 'e
val drop_minus_plus :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> int * Ast0_cocci.base_string_fragment Ast0_cocci.wrap list
val not_format_string :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast_cocci.isWchar -> Ast0_cocci.base_expression Ast0_cocci.wrap
val nometas : string -> bool
val parse_string :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list *
  (Ast_cocci.added_string * Ast0_cocci.position_info) list * Ast0_cocci.anything list *
  string -> Ast_cocci.isWchar -> Ast0_cocci.base_expression Ast0_cocci.wrap
val unfloatl : string -> string
val unfloatr : string -> string
val mk_script :
  'a * string ->
  Ast_cocci.script_position ->
  string * 'b ->
  Ast_cocci.meta_name list ->
  string list ->
  string ->
  (string * string * (Ast_cocci.meta_name * Ast_cocci.metavar) list *
  Ast_cocci.script_position * string)
