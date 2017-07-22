module Ast0 = Ast0_cocci
module Ast = Ast_cocci
val contains_string_constant : bool ref
type info = Ast.meta_name * Ast0.pure * Data.clt
type midinfo =
    Ast.meta_name * Ast0.constraints * Ast.seed * Ast0.pure * Data.clt
type cstrinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type assignOpinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type binaryOpinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type expinfo = Ast.meta_name * Ast0.constraints * Ast0.pure * Data.clt
type tyinfo = Ast.meta_name * Ast0.typeC list * Ast0.pure * Data.clt
type list_info =
    Ast.meta_name * Ast.list_len * Ast0.constraints * Ast0.pure * Data.clt
type typed_expinfo =
    Ast.meta_name * Ast0.constraints * Ast0.pure * Ast0.typeC list option *
    Data.clt
type typed_expinfo_bitfield =
    Ast.meta_name * Ast0.constraints * Ast0.pure * Ast0.typeC list option *
    Data.clt * Ast.list_len option
type pos_info =
    Ast.meta_name * Ast0.constraints * Ast.meta_collect * Data.clt
val make_info :
  int ->
  int ->
  int ->
  int ->
  int ->
  (Ast_cocci.added_string * Ast0.position_info) list ->
  (Ast_cocci.added_string * Ast0.position_info) list ->
  bool -> string -> Ast0.info
val clt2info :
  'a * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * 'b * string ->
  Ast0.info
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
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * 'b * string ->
  'a * Ast0.arity * Ast0.info * Ast0.mcodekind * 'b ref * int
val clt2mcode :
  'a ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * 'b * string ->
  'a * Ast0.arity * Ast0.info * Ast0.mcodekind * 'b ref * int
val id2name : 'a * 'b -> 'a
val id2clt : 'a * 'b -> 'b
val id2mcode :
  'a *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * 'b * string) ->
  'a * Ast0.arity * Ast0.info * Ast0.mcodekind * 'b ref * int
val sym2mcode :
  'a *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * 'b * string) ->
  'a * Ast0.arity * Ast0.info * Ast0.mcodekind * 'b ref * int
val mkdots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) *
  (Ast0.statement Ast0.dots, Ast0.statement) Ast0.whencode list ->
  Ast0.base_statement Ast0.wrap
val mkedots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) *
  (string Ast0.mcode * string Ast0.mcode * Ast0.expression) option ->
  Ast0.base_expression Ast0.wrap
val mkdpdots :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_define_param Ast0.wrap
val mkidots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) *
  (string Ast0.mcode * string Ast0.mcode * Ast0.initialiser) option ->
  Ast0.base_initialiser Ast0.wrap
val mkfdots :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) *
  (string Ast0.mcode * string Ast0.mcode * Ast0.field) list option ->
  Ast0.base_field Ast0.wrap
val mkfdots_one :
  string ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) *
  (string Ast0.mcode * string Ast0.mcode * Ast0.field) option ->
  Ast0.base_field Ast0.wrap
val mkpdots :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_parameterTypeDef Ast0.wrap
val arith_op :
  Ast_cocci.arithOp ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.expression -> Ast0.base_expression Ast0.wrap
val logic_op :
  Ast_cocci.logicalOp ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.expression -> Ast0.base_expression Ast0.wrap
val make_cv :
  Ast_cocci.const_vol Ast0.mcode option -> Ast0.typeC -> Ast0.typeC
val top_dots : 'a -> 'a Ast0.wrap
val pointerify :
  Ast0.typeC ->
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string)
  list -> Ast0.typeC
val ty_pointerify : Ast0.typeC -> 'a list -> Ast0.typeC
val arrayify :
  Ast0.typeC ->
  ((Data.line_type * int * int * int * int * int *
    (Ast_cocci.added_string * Ast0.position_info) list *
    (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
    string) *
   Ast0.expression option *
   (Data.line_type * int * int * int * int * int *
    (Ast_cocci.added_string * Ast0.position_info) list *
    (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
    string))
  list -> Ast0.typeC
val iso_adjust :
  ('a -> 'b) ->
  ('c -> 'b) -> 'a -> ('c, 'c) Common.either list -> 'b list list
val lookup : string -> string -> Ast.metavar
val meta_lookup : string -> string -> Ast.metavar -> Ast.metavar
val check_meta_tyopt : bool -> Ast.metavar -> unit
val check_meta : Ast.metavar -> unit
val check_inherited_constraint_without_type : 'a option * 'b -> 'a * 'b
val check_inherited_constraint :
  'a option * 'b -> ('a * 'b -> Ast.metavar) -> 'a * 'b
val create_metadec :
  'a ->
  'b ->
  ('a ->
   'c * 'd ->
   'b ->
   (Ast.metavar -> (Ast.metavar, Ast.metavar) Common.either list) -> 'e list) ->
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
   (Ast.metavar -> (Ast.metavar, Ast.metavar) Common.either list) ->
   'c -> 'd list) ->
  (('a option * 'b) * 'c) list -> 'a -> 'd list
val create_metadec_with_constraints :
  'a ->
  'b ->
  ('a ->
   'c * 'd ->
   'b ->
   (Ast.metavar -> (Ast.metavar, Ast.metavar) Common.either list) ->
   'e -> 'f list) ->
  (('c option * 'd) * 'e) list -> 'c -> 'f list
val create_metadec_ty :
  'a ->
  'b ->
  ('a ->
   'c * 'd ->
   'b ->
   (Ast.metavar -> (Ast.metavar, Ast.metavar) Common.either list) ->
   'e -> 'f list) ->
  (('c option * 'd) * 'e) list -> 'c -> 'f list
val create_len_metadec :
  'a ->
  'b ->
  (Ast.list_len ->
   'a ->
   string * 'c ->
   'b ->
   (Ast.metavar -> (Ast.metavar, Ast.metavar) Common.either list) ->
   'd -> (Ast.metavar, Ast.metavar) Common.either list) ->
  ((string option * string) * Ast0_cocci.constraints, int) Common.either ->
  ((string option * 'c) * 'd) list ->
  string -> (Ast.metavar, Ast.metavar) Common.either list
val str2inc : string -> Ast.inc_elem list
val meta_decl :
  Ast_cocci.meta_name * Ast0.constraints * Ast0.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) ->
  Ast0.base_declaration Ast0.wrap
val meta_field :
  Ast_cocci.meta_name * Ast0.constraints * Ast0.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) ->
  Ast0.base_field Ast0.wrap
val dolen :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast.list_len -> Ast0.listlen
val meta_field_list :
  Ast_cocci.meta_name * Ast.list_len * Ast0.constraints * Ast0.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) ->
  Ast0.base_field Ast0.wrap
val meta_stm :
  Ast_cocci.meta_name * Ast0.constraints * Ast0.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) ->
  Ast0.base_statement Ast0.wrap
val meta_stm_list :
  Ast_cocci.meta_name * Ast.list_len * Ast0.constraints * Ast0.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) ->
  Ast0.base_statement Ast0.wrap
val meta_dparam_list :
  Ast_cocci.meta_name * Ast.list_len * Ast0.constraints * Ast0.pure *
  (Data.line_type * int * int * int * int * int *
   (Ast_cocci.added_string * Ast0.position_info) list *
   (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
   string) ->
  Ast0.base_define_param Ast0.wrap
val exp_stm :
  Ast0.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val make_fake_mcode : 'a -> Ast0.info * Ast0.mcodekind * int
val ifthen :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.statement -> Ast0.base_statement Ast0.wrap
val ifthenelse :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.statement ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.statement -> Ast0.base_statement Ast0.wrap
val forloop :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.statement -> Ast0.base_statement Ast0.wrap
val forloop2 :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.declaration ->
  Ast0.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression option ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.statement -> Ast0.base_statement Ast0.wrap
val whileloop :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.statement -> Ast0.base_statement Ast0.wrap
val doloop :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.statement ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val iterator :
  Ast0.ident ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression Ast0.dots ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.statement -> Ast0.base_statement Ast0.wrap
val switch :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.declaration list ->
  Ast0.case_line list ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val ret_exp :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.expression ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val ret :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val break :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val cont :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val label :
  Ast0.ident ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val goto :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.ident ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val seq :
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string ->
  Ast0.statement Ast0.dots ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_statement Ast0.wrap
val check_rule_name : (string * 'a) option -> string option
val make_iso_rule_name_result : string -> Ast.rulename
val fix_dependencies : Ast0.dependency -> Ast.dependency
val make_cocci_rule_name_result :
  (string * 'a) option ->
  Ast0.dependency ->
  string list -> string list -> Ast.exists -> Ast.parser_kind -> Ast.rulename
val make_generated_rule_name_result :
  (string * 'a) option ->
  Ast0.dependency ->
  string list -> string list -> Ast.exists -> Ast.parser_kind -> Ast.rulename
val make_script_rule_name_result :
  string * 'a -> (string * 'b) option -> Ast0.dependency -> Ast.rulename
val make_initial_script_rule_name_result :
  string * 'a -> Ast0.dependency -> Ast.rulename
val make_final_script_rule_name_result :
  string * 'a -> Ast0.dependency -> Ast.rulename
val struct_initializer :
  Ast0.base_initialiser Ast0.wrap list Ast0.wrap -> bool
val drop_dot_commas :
  Ast0.base_initialiser Ast0.wrap list Ast0.wrap ->
  Ast0.base_initialiser Ast0.wrap list Ast0.wrap
type metavars =
    MFrag of (string Ast0.mcode -> Ast0.string_fragment)
  | MFmt of Ast0.string_format
val string_metavariables :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> metavars
val pct_split : string -> string list
val parse_middle :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_string_fragment Ast0.wrap list
val check_no_duplicates : Ast0.base_string_fragment Ast0.wrap list -> unit
val update_line :
  'a * int * int * int * int * int * 'b * 'c * 'd * 'e ->
  int -> 'a * int * int * int * int * int * 'b * 'c * 'd * 'e
val drop_minus_plus :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> int * Ast0.base_string_fragment Ast0.wrap list
val not_format_string :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_expression Ast0.wrap
val nometas : string -> bool
val parse_string :
  string ->
  Data.line_type * int * int * int * int * int *
  (Ast_cocci.added_string * Ast0.position_info) list *
  (Ast_cocci.added_string * Ast0.position_info) list * Ast0.anything list *
  string -> Ast0.base_expression Ast0.wrap
