(** Compatibility module for Pycaml users. *)

type pyobject = Py.Object.t

type pyobject_type =
  | TupleType
  | BytesType
  | UnicodeType
  | BoolType
  | IntType
  | FloatType
  | ListType
  | NoneType
  | CallableType
  | ModuleType
  | ClassType
  | TypeType
  | DictType
  | NullType
  | CamlpillType
  | OtherType
  | EitherStringType (* Signifies that either of BytesType or UnicodeType is allowed. *)
  | CamlpillSubtype of string (* Signifies that only the particular Camlpill variety is allowed. *)
  | AnyType                   (* Allow any python object. *)

type pyerror_type =
    Pyerr_Exception
  | Pyerr_ArithmeticError
  | Pyerr_LookupError
  | Pyerr_AssertionError
  | Pyerr_AttributeError
  | Pyerr_EOFError
  | Pyerr_EnvironmentError
  | Pyerr_FloatingPointError
  | Pyerr_IOError
  | Pyerr_ImportError
  | Pyerr_IndexError
  | Pyerr_KeyError
  | Pyerr_KeyboardInterrupt
  | Pyerr_MemoryError
  | Pyerr_NameError
  | Pyerr_NotImplementedError
  | Pyerr_OSError
  | Pyerr_OverflowError
  | Pyerr_ReferenceError
  | Pyerr_RuntimeError
  | Pyerr_SyntaxError
  | Pyerr_SystemExit
  | Pyerr_TypeError
  | Pyerr_ValueError
  | Pyerr_ZeroDivisionError

val pytype: pyobject -> pyobject_type

val pycaml_seterror: pyerror_type -> string -> unit

val pynull: unit -> pyobject
val pynone: unit -> pyobject
val py_true: unit -> pyobject
val py_false: unit -> pyobject

val py_initialize: unit -> unit
val py_finalize: unit -> unit
val pyerr_print: unit -> unit
val pyerr_clear: unit -> unit
val pyimport_cleanup: unit -> unit

(* Type2 *)
val py_exit: int -> unit
val pyerr_printex: int -> unit

(* Type3 *)
val py_setprogramname: string -> unit
val py_setpythonhome: string -> unit

(* Type4 *)
val py_isinitialized: unit -> int
val pyeval_getrestricted: unit -> int

(* Type5 *)
val pyrun_simplestring: string -> int
val pyimport_importfrozenmodule: string -> int

(* Test6 *)
val pyrun_anyfile: (int * string) -> int
val pyrun_simplefile: (int * string) -> int
val pyrun_interactiveone: (int * string) -> int
val pyrun_interactiveloop: (int * string) -> int
val py_fdisinteractive: (int * string) -> int

(* Type7 *)
val pyrun_anyfileex: (int * string * int) -> int
val pyrun_simplefileex: (int * string * int) -> int

(* Type8 *)
val py_getprogramname: unit -> string
val py_getpythonhome: unit -> string
val py_getprogramfullpath: unit -> string
val py_getprefix: unit -> string
val py_getexecprefix: unit -> string
val py_getpath: unit -> string
val py_getversion: unit -> string
val py_getplatform: unit -> string
val py_getcopyright: unit -> string
val py_getcompiler: unit -> string
val py_getbuildinfo: unit -> string

(* Type9 *)
val pyrun_string: (string * int * pyobject * pyobject) -> pyobject

(* Type10 *)
val pyrun_file: (int * string * int * pyobject * pyobject) -> pyobject

(* Type11 *)
val pyrun_fileex: (int * string * int * pyobject * pyobject * int) -> pyobject

(* Type12 *)
val py_compilestring: (string * string * int) -> pyobject

(* Type13 *)
val pyobject_print: (pyobject * int * int) -> int
val pytuple_getslice: (pyobject * int * int) -> pyobject
val pysequence_getslice: (pyobject * int * int) -> pyobject

(* Type14 *)
val pymethod_function: pyobject -> pyobject
val pymethod_self: pyobject -> pyobject
val pymethod_class: pyobject -> pyobject
val pymodule_getdict: pyobject -> pyobject
val pyunicode_asutf8string: pyobject -> pyobject
val pyunicode_asutf16string: pyobject -> pyobject
val pyunicode_asutf32string: pyobject -> pyobject
val pyobject_repr: pyobject -> pyobject
val pyimport_reloadmodule: pyobject -> pyobject
val pyimport_import: pyobject -> pyobject
val pyobject_str: pyobject -> pyobject
val pyobject_type: pyobject -> pyobject
val pyobject_unicode: pyobject -> pyobject
val pydict_keys: pyobject -> pyobject
val pydict_values: pyobject -> pyobject
val pydict_items: pyobject -> pyobject
val pydict_copy: pyobject -> pyobject
val pysequence_tuple: pyobject -> pyobject
val pysequence_list: pyobject -> pyobject
val pynumber_int: pyobject -> pyobject
val pynumber_long: pyobject -> pyobject
val pynumber_float: pyobject -> pyobject
val pynumber_negative: pyobject -> pyobject
val pynumber_positive: pyobject -> pyobject
val pynumber_absolute: pyobject -> pyobject
val pynumber_invert: pyobject -> pyobject
val pyiter_next: pyobject -> pyobject
val pyinstancemethod_new: pyobject -> pyobject

(* Type15 *)
val pyobject_richcompare: (pyobject * pyobject * int) -> pyobject

(* Type16 *)
val pydict_getitemstring: (pyobject * string) -> pyobject
val pyobject_getattrstring: (pyobject * string) -> pyobject
val pysequence_fast: (pyobject * string) -> pyobject
val pymapping_getitemstring: (pyobject * string) -> pyobject

(* Type17 *)
val pydict_getitem: pyobject * pyobject -> pyobject
val pyeval_callobject: pyobject * pyobject -> pyobject
val pyobject_getattr: pyobject * pyobject -> pyobject
val pyobject_getitem: pyobject * pyobject -> pyobject
val pynumber_add: pyobject * pyobject -> pyobject
val pynumber_subtract: pyobject * pyobject -> pyobject
val pynumber_multiply: pyobject * pyobject -> pyobject
val pynumber_divide: pyobject * pyobject -> pyobject
val pynumber_truedivide: pyobject * pyobject -> pyobject
val pynumber_floordivide: pyobject * pyobject -> pyobject
val pynumber_remainder: pyobject * pyobject -> pyobject
val pynumber_divmod: pyobject * pyobject -> pyobject
val pynumber_lshift: pyobject * pyobject -> pyobject
val pynumber_rshift: pyobject * pyobject -> pyobject
val pynumber_and: pyobject * pyobject -> pyobject
val pynumber_xor: pyobject * pyobject -> pyobject
val pynumber_or: pyobject * pyobject -> pyobject
val pynumber_inplaceadd: pyobject * pyobject -> pyobject
val pynumber_inplacesubtract: pyobject * pyobject -> pyobject
val pynumber_inplacemultiply: pyobject * pyobject -> pyobject
val pynumber_inplacetruedivide: pyobject * pyobject -> pyobject
val pynumber_inplacefloordivide: pyobject * pyobject -> pyobject
val pynumber_inplacedivide: pyobject * pyobject -> pyobject
val pynumber_inplaceremainder: pyobject * pyobject -> pyobject
val pynumber_inplacelshift: pyobject * pyobject -> pyobject
val pynumber_inplacershift: pyobject * pyobject -> pyobject
val pynumber_inplaceand: pyobject * pyobject -> pyobject
val pynumber_inplacexor: pyobject * pyobject -> pyobject
val pynumber_inplaceor: pyobject * pyobject -> pyobject
(*val pybytes_format: pyobject * pyobject -> pyobject*)
val pystring_format: pyobject * pyobject -> pyobject
val pyinstance_newraw: pyobject * pyobject -> pyobject
val pysequence_concat: pyobject * pyobject -> pyobject
val pysequence_inplaceconcat: pyobject * pyobject -> pyobject

(* Type18 *)
val pyobject_istrue: pyobject -> int
val pyobject_not: pyobject -> int
val pycallable_check: pyobject -> int
val pybytes_size: pyobject -> int
val pystring_size: pyobject -> int
val pydict_size: pyobject -> int
val pytuple_size: pyobject -> int
val pyerr_exceptionmatches: pyobject -> int
val pyobject_size: pyobject -> int
val pynumber_check: pyobject -> int
val pysequence_check: pyobject -> int
val pysequence_size: pyobject -> int
val pysequence_length: pyobject -> int
val pymapping_check: pyobject -> int
val pymapping_size: pyobject -> int
val pymapping_length: pyobject -> int
val pyiter_check: pyobject -> int
val pyunicode_getsize: pyobject -> int
val pyunicode_check: pyobject -> int
val pybytes_check: pyobject -> int

(* Type19 *)
val pyobject_hasattr: (pyobject * pyobject) -> int
val pyobject_delitem: (pyobject * pyobject) -> int
val pydict_delitem: (pyobject * pyobject) -> int
val pyerr_givenexceptionmatches: (pyobject * pyobject) -> int
val pysequence_count: (pyobject * pyobject) -> int
val pysequence_contains: (pyobject * pyobject) -> int
val pysequence_in: (pyobject * pyobject) -> int
val pysequence_index: (pyobject * pyobject) -> int
val pymapping_haskey: (pyobject * pyobject) -> int

val pyobject_compare: (pyobject * pyobject) -> int

(* Type20 *)
val pyobject_richcomparebool: (pyobject * pyobject * int) -> int

(* Type21 *)
val pyobject_setattrstring: (pyobject * string * pyobject) -> int
val pydict_setitemstring: (pyobject * string * pyobject) -> int
val pymapping_setitemstring: (pyobject * string * pyobject) -> int
val pymodule_addobject: (pyobject * string * pyobject) -> int

(* Type22 *)
val pymapping_haskeystring: (pyobject * string) -> int
val pyobject_hasattrstring: (pyobject * string) -> int
val pydict_delitemstring: (pyobject * string) -> int

(* Type23 -- currently not implemented.
val pynumber_coerce: (pyobject * pyobject) -> (pyobject * pyobject) option
val pynumber_coerceex: (pyobject * pyobject) -> (pyobject * pyobject) option
*)

(* Type24 *)
val pyobject_setattr: (pyobject * pyobject * pyobject) -> int
val pyobject_setitem: (pyobject * pyobject * pyobject) -> int
val pydict_setitem: (pyobject * pyobject * pyobject) -> int

(* Type25 *)
val pyobject_hash: pyobject -> int64
val pyint_aslong: pyobject -> int64

(* Type26 *)
val pybytes_asstring: pyobject -> string
(** Equivalent to {!pystring_asstring}. *)

val pystring_asstring: pyobject -> string
(** [pystring_asstring o] returns the string contained in the Python value [o].
Equivalent to {!Py.String.to_string}: the function raises an exception if [o]
is not a string, and, with Python 3, [o] can be either a Bytes or Unicode. *)

val pymodule_getfilename: pyobject -> string
(** Equivalent to {!Py.Module.get_filename}: raises an exception in case of
error. *)

val pymodule_getname: pyobject -> string
(** Equivalent to {!Py.Module.get_name}: raises an exception in case of
error. *)

(* Type28 *)
val pyimport_addmodule: string -> pyobject
val pybytes_fromstring: string -> pyobject
val pystring_fromstring: string -> pyobject
val pymodule_new: string -> pyobject
val pyimport_importmodule: string -> pyobject

(* Type29 *)
val pydict_new: unit -> pyobject
val pyerr_occurred: unit -> pyobject
val pyimport_getmoduledict: unit -> pyobject
val pyeval_getbuiltins: unit -> pyobject
val pyeval_getglobals: unit -> pyobject
val pyeval_getlocals: unit -> pyobject
(* val pyeval_getframe: unit -> pyobject *)

(* Type30 *)
val pydict_clear: pyobject -> unit
val pyerr_setnone: pyobject -> unit

(* Type31  -- currently not implemented.
val pydict_next: (pyobject * int) -> (pyobject * pyobject * int) option
*)

(* Type34 *)
val pyint_fromlong: int64 -> pyobject

(* Type35 *)
val pyint_getmax: unit -> int64
val pyimport_getmagicnumber: unit -> int64

(* Type36 *)
val pyfloat_fromdouble: float -> pyobject

(* Type37 *)
val pyfloat_asdouble: pyobject -> float

(* Type39 *)
val pytuple_new: int -> pyobject

(* Type40 *)
val pysequence_inplacerepeat: (pyobject * int) -> pyobject
val pytuple_getitem: (pyobject * int) -> pyobject
val pysequence_repeat: (pyobject * int) -> pyobject
val pysequence_getitem: (pyobject * int) -> pyobject

(* Type40b *)
val pysequence_delitem: (pyobject * int) -> int

(* Type41 *)
val pytuple_setitem: (pyobject * int * pyobject) -> int
val pysequence_setitem: (pyobject * int * pyobject) -> int

(* Type42 *)
val pyslice_new: (pyobject * pyobject * pyobject) -> pyobject
val pyclass_new: (pyobject * pyobject * pyobject) -> pyobject
val pyinstance_new: (pyobject * pyobject * pyobject) -> pyobject
val pymethod_new: (pyobject * pyobject * pyobject) -> pyobject
val pyeval_callobjectwithkeywords: (pyobject * pyobject * pyobject) -> pyobject
val pynumber_power: (pyobject * pyobject * pyobject) -> pyobject
val pynumber_inplacepower: (pyobject * pyobject * pyobject) -> pyobject

(* Type43 *)
(* val pyslice_getindices: (pyobject * int) -> (int * int * int) option *)

(* Type45 *)
val pyerr_setobject: (pyobject * pyobject) -> unit

(* Type46 *)
val pyerr_setstring: (pyobject * string) -> unit

(* Type47 *)
val pyerr_fetch: (pyobject * pyobject * pyobject) -> (pyobject * pyobject * pyobject)
(** Wrapper for
    {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Fetch} PyErr_Fetch}.
    The argument in ignored and [PyErr_NormalizeException] is implicitly
    called. *)

val pyerr_normalizeexception: (pyobject * pyobject * pyobject) -> (pyobject * pyobject * pyobject)
(** Implemented as the identity function. *)

(* Type48 *)
val pyerr_restore: (pyobject * pyobject * pyobject) -> unit

(* Type49 *)
val pyimport_execcodemodule: (string * pyobject) -> pyobject

(* Type50 *)
val pyimport_execcodemoduleex: (string * pyobject * string) -> pyobject

(* Type51 *)
val pyimport_importmoduleex: (string * pyobject * pyobject * pyobject) -> pyobject

(* Type52 *)
(*
val pybytes_asstringandsize: pyobject -> string
val pystring_asstringandsize: pyobject -> string
val pyobject_ascharbuffer: pyobject -> string
val pyobject_asreadbuffer: pyobject -> string
val pyobject_aswritebuffer: pyobject -> string
*)

(* Type53 *)
val pysequence_setslice: (pyobject * int * int * pyobject) -> int

(* Type54 *)
val pysequence_delslice: (pyobject * int * int) -> int

(* TypeUTF8Decoder *)
val pyunicode_decodeutf8: (string * string option) -> pyobject
val pyunicode_asencodedstring: (pyobject * string * string) -> pyobject

val make_pill_wrapping: string -> 'a -> ('a -> pyobject) * (pyobject -> 'a)

val py_is_true: pyobject -> bool

val pytuple_fromarray: pyobject array -> pyobject

val pytuple_fromsingle: pyobject -> pyobject

val pytuple_toarray: pyobject -> pyobject array

val register_ocamlpill_types: string array -> unit
(** For compatibility only. Does nothing. *)
