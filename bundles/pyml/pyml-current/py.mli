(** OCaml Interface for Python. *)

(** Call [initialize ()] first. *)

val initialize: ?interpreter:string -> unit -> unit
(** [initialize ~interpreter ()] finds and loads the Python library.
    This function should be called before any other functions, except
    if explicitely mentioned.
    The version of Python is determined by the output of the shell command
    [python --version].
    If an [interpreter] executable name is given, this executable is
    used in place of [python] in the previous command line.
    The library is searched by
    using [pkg-config] if available, by considering system paths, and
    in the directory [../lib] relatively to the directory where the
    [python] executable is. If the library has been statically linked
    with the executable, it will be used. *)

val finalize: unit -> unit
(** [finalize ()] unloads the library. No other functions except
    [initialize ()] should be called afterwards. *)

val is_initialized: unit -> bool
(** [is_initialized ()] returns [true] if the library is initialized
    ([initialize ()] has been called and [finalize ()] has not been
    called afterwards). *)

val version: unit -> string
(** [version ()] returns the version of the Python library. E.g. ["3.5.1"]. *)

val version_major: unit -> int
(** [version_major ()] returns the major number (the first component) of the
    version of the Python library, either [2] or [3]. *)

val version_minor: unit -> int
(** [version_minor ()] returns the minor number (the second component) of the
    version of the Python library. *)

type compare = Pytypes.compare = LT | LE | EQ | NE | GT | GE

(** General functions to handle Python values *)
module Object: sig
  type t = Pytypes.pyobject
  (** The type of a Python value. *)

  val del_attr: t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelAttr} PyObject_DelAttr} *)

  val del_attr_string: t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelAttrString} PyObject_DelAttrString} *)

  val del_item: t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelItem} PyObject_DelItem} *)

  val del_item_string: t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelItemString} PyObject_DelItemString} *)

  val get_attr: t -> t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetAttr} PyObject_GetAttr} *)

  val get_attr_string: t -> string -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetAttrString} PyObject_GetAttrString} *)

  val get_item: t -> t -> t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetItem} PyObject_GetItem} *)

  val find: t -> t -> t
  (** Equivalent to {!get_item} but raises a [Not_found] exception in
      case of failure. *)

  val get_item_string: t -> string -> t option
  (** [get_item_string o key] returns the element corresponding to the object
      [key] or [None] on failure. *)

  val find_string: t -> string -> t
  (** Equivalent to {!get_item_string} but raises a [Not_found] exception in
      case of failure. *)

  val get_iter: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetIter} PyObject_GetIter} *)

  val get_type: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetType} PyObject_GetType} *)

  val has_attr: t -> t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_HasAttr} PyObject_HasAttr} *)

  val has_attr_string: t -> string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_HasAttrString} PyObject_HasAttrString} *)

  val hash: t -> int64
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Hash} PyObject_Hash} *)

  val is_true: t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_IsTrue} PyObject_IsTrue} *)

  val not: t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Not} PyObject_Not} *)

  val print: t -> out_channel -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Print} PyObject_Print} *)

  val repr: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Repr} PyObject_Repr} *)

  val rich_compare: t -> t -> compare -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_RichCompare} PyObject_RichCompare} *)

  val rich_compare_bool: t -> t -> compare -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_RichCompareBool} PyObject_RichCompareBool} *)

  val set_attr: t -> t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetAttr} PyObject_SetAttr} *)

  val set_attr_string: t -> string -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetAttrString} PyObject_SetAttrString} *)

  val set_item: t -> t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetItem} PyObject_SetItem} *)

  val set_item_string: t -> string -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetItemString} PyObject_SetItemString} *)

  val str: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Str} PyObject_Str} *)

  val string_of_repr: t -> string
  (** [string_of_repr o] returns the string [repr o].
      We have
      [Py.Object.to_string o = Py.String.to_string (Py.Object.repr o)]. *)

  val to_string: t -> string
  (** [to_string o] returns the string [str o].
      We have
      [Py.Object.to_string o = Py.String.to_string (Py.Object.str o)]. *)

  val as_char_buffer: t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/objbuffer.html#c.PyObject_AsCharBuffer} PyObject_AsCharBuffer} *)

  val as_read_buffer: t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/objbuffer.html#c.PyObject_AsReadBuffer} PyObject_AsReadBuffer} *)

  val as_write_buffer: t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/objbuffer.html#c.PyObject_AsWriteBuffer} PyObject_AsWriteBuffer} *)

  val reference_count: t -> int
  (** [reference_count o] returns the number of references to the Python
      object [o]. *)
end

exception E of Object.t * Object.t
(** [E (errtype, errvalue)] is a Python error.
    [errtype] is the type of the exception.
    [errvalue] is the value. *)

val null: Object.t
(** The value [NULL] of the C Python API. [null] is useful for calling
    directly the functions of {!Pywrappers} module.
    The value should not appear when using the functions of the [Py] module. *)

val none: Object.t
(** The value [None] of Python. *)

val set_program_name: string -> unit
(** Sets the program name (by default, [Sys.argv.(0)]).
    The function can be called before [initialize ()] and the value is preserved
    from one initialization to the other. *)

val set_python_home: string -> unit
(** Sets the path of the Python home.
    The function can be called before [initialize ()] and the value is preserved
    from one initialization to the other. *)

val get_program_name: unit -> string
(** Gets the program name (by default, [Sys.argv.(0)]).
    The function can be called before [initialize ()]. *)

val get_python_home: unit -> string
(** Gets the path of the Python home.
    The function can be called before [initialize ()]. *)

val get_program_full_path: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetProgramFullPath} Py_GetProgramFullPath}. *)

val get_prefix: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetPrefix} Py_GetPrefix}. *)

val get_exec_prefix: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetExecPrefix} Py_GetExecPrefix}. *)

val get_path: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetPath} Py_GetPath}. *)

val get_version: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetVersion} Py_GetVersion}. *)

val get_platform: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetPlatform} Py_GetPlatform}. *)

val get_copyright: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetCopyright} Py_GetCopyright}. *)

val get_compiler: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetCompiler} Py_GetCompiler}. *)

val get_build_info: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetBuildInfo} Py_GetBuildInfo}. *)

(** Interface for Python values of type [Bool]. *)
module Bool: sig
  val t: Object.t
  (** The Python value [True]. *)

  val f: Object.t
  (** The Python value [False]. *)

  val check: Object.t -> bool
  (** [check v] returns [true] if [v = t] or [v = f]. *)

  val of_bool: bool -> Object.t
  (** [of_bool b] returns [t] if [b = true], and [f] if [b = false]. *)

  val to_bool: Object.t -> bool
  (** [to_bool b] returns [true] if [b = t], and [false] if [b = f].
      [Failure] is raised if [b] is neither [t] nor [f]. *)
end

(** Interface for Python values of type [Callable]. *)
module Callable: sig
  val check: Object.t -> bool
  (** [check v] returns [true] if [v] is callable.
      Wrapper for
      {{: https://docs.python.org/3/c-api/object.html#c.PyCallable_Check} PyCallable_Check}. *)

  val of_function: ?docstring:string -> (Object.t -> Object.t) -> Object.t
  (** [of_function f] returns a Python callable object that calls the function
      [f].
      Arguments are passed as a tuple.
      If [f] raises a Python exception
      ([Py.E (errtype, errvalue)] or [Py.Err (errtype, msg)]),
      this exception is raised as a Python exception
      (via {!Err.set_object} or {!Err.set_error} respectively).
      If [f] raises any other exception, this exception bypasses the Python
      interpreter. *)

  val of_function_array: ?docstring:string -> (Object.t array -> Object.t)
    -> Object.t
  (** Equivalent to {!of_function} but with an array of Python objects
      instead of a tuple for passing arguments. *)

  val to_function: Object.t -> Object.t -> Object.t
  (** [to_function c] returns a function [f] such that [f args] calls the
      Python callable [c] with the Python tuple [args] as arguments. *)

  val to_function_array: Object.t -> Object.t array -> Object.t
  (** Equivalent to {!to_function} but with an array of Python objects
      instead of a tuple for passing arguments. *)
end

(** Embedding of OCaml values in Python. *)
module Capsule: sig
  val check: Object.t -> bool
  (** [check v] returns [true] if [v] contains an OCaml value. *)

  val make: string -> ('a -> Object.t) * (Object.t -> 'a)
  (** For a given type ['a], [make s] returns a pair [(wrap, unwrap)].
      [wrap v] transforms the value [v] of type 'a to an opaque Python object.
      [unwrap w] transforms the opaque Python object [w] previously obtained
      with [wrap v] into the original OCaml value [v],
      such that [unwrap (wrap v) = v].
      [Failure _] is raised if a wrapper has already been generated for a type
      of the same name. *)

  val type_of: Object.t -> string
  (** [type_of w] returns the type string associated to the opaque Python
      object [w]. *)

  val is_valid: Object.t -> string -> bool
  (** Wrapper for
      {{: https://docs.python.org/3/c-api/capsule.html#c.PyCapsule_IsValid} PyCapsule_IsValid}.
      OCaml capsules have the name ["ocaml-capsule"].
      We have [check v = is_valid v "ocaml-capsule"]. *)

  val unsafe_wrap_value: 'a -> Object.t
  (** [unsafe_wrap_value v] transforms the value [v] to an opaque Python
      object. *)

  val unsafe_unwrap_value: Object.t -> 'a
  (** [unsafe_unwrap_value v] transforms the opaque Python object [w]
      previously obtained with [unsafe_wrap_value v] into the original OCaml
      value [v]. *)
end

(** Defining a new class type *)
module Class: sig
  val init: ?parents:Object.t -> ?fields:((string * Object.t) list) ->
      ?methods:((string * (Object.t -> Object.t)) list) ->
        Object.t -> Object.t
  (** [init ~parents ~fields ~methods classname] Returns a new class type.
      @param classname is a Python string.
      @param parents is a Python tuple for bases (default: [()]).
      @param fields is an associative list for field values (default : [[]]).
      @param methods is an associative list for method closures
      (default : [[]]). *)
end

(** Interface for Python values of type [Long]. *)
module Long: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python integer/long. *)

  val of_int64: int64 -> Object.t
  (** [of_int i] returns the Python long with the value [i].
      Wrapper for
      {{: https://docs.python.org/3/c-api/long.html#c.PyLong_FromLong} PyLong_FromLong}. *)

  val to_int64: Object.t -> int64
  (** [to_int o] takes a Python long [o] as arguments
      and returns the corresponding 64-bit integer value.
      A Python exception ([Py.E _]) is raised if [o] is not a long.
      Wrapper for
      {{: https://docs.python.org/3/c-api/long.html#c.PyLong_AsLong} PyLong_AsLong}. *)

  val of_int: int -> Object.t
  (** [of_int i] returns the Python long with the value [i].
      We have [of_int i = of_int64 (Int64.of_int i)]. *)

  val to_int: Object.t -> int
  (** [to_int o] takes a Python long [o] as arguments
      and returns the corresponding integer value.
      A Python exception ([Py.E _]) is raised if [o] is not a long.
      We have [to_int o = Int64.to_int (to_int 64 o)]. *)
end

(** Interface for Python values of type [Dict]. *)
module Dict: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python dictionary. *)

  val clear: Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Clear} PyDict_Clear} *)

  val copy: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Copy} PyDict_Copy} *)

  val create: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_New} PyDict_New} *)

  val del_item: Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_DelItem} PyDict_DelItem} *)

  val del_item_string: Object.t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_DelItemString} PyDict_DelItemString} *)

  val get_item: Object.t -> Object.t -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_GetItem} PyDict_GetItem} *)

  val find: Object.t -> Object.t -> Object.t
  (** [find p key] returns the object from Python dictionary [p] which has a key
      [key]. Equivalent to {!get_item} but [find] raises [Not_found] if the
      key [key] is not present. *)

  val get_item_string: Object.t -> string -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_GetItemString} PyDict_GetItemString} *)

  val find_string: Object.t -> string -> Object.t
  (** [find_string p key] returns the object from Python dictionary [p]
      which has a key [key]. Equivalent to {!get_item_string} but [find_string]
      raises [Not_found] if the key [key] is not present. *)

  val keys: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Keys} PyDict_Keys} *)

  val items: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Items} PyDict_Items} *)

  val set_item: Object.t -> Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_SetItem} PyDict_SetItem} *)

  val set_item_string: Object.t -> string -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_SetItemString} PyDict_SetItemString} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Size} PyDict_Size} *)

  val values: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Clear} PyDict_Str} *)

  val iter: (Object.t -> Object.t -> unit) -> Object.t -> unit
  (** [iter f dict] applies [f key value] for each pair [(key, value)]
      in the Python dictionary [dict]. *)

  val fold: (Object.t -> Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** [fold f dict v] returns [f key1 value1 (... (f keyn valuen dict))]
      where [(key1, value1)], ..., [(keyn, valuen)] are the bindings of
      the Python dictionary [dict]. *)

  val for_all: (Object.t -> Object.t -> bool) -> Object.t -> bool
  (** [for_all p dict] checks whether all the bindings [(key, value)] of the
      Python dictionary [dict] satisfy the predicate [p key value]. *)

  val exists: (Object.t -> Object.t -> bool) -> Object.t -> bool
  (** [for_all p dict] checks that there is at least one binding [(key, value)]
      among those of the Python dictionary [dict] that satisfies the predicate
      [p key value]. *)

  val bindings: Object.t -> (Object.t * Object.t) list
  (** [bindings o] returns all the pairs [(key, value)] in the Python dictionary
      [o]. *)

  val singleton: Object.t -> Object.t -> Object.t
  (** [singleton key value] returns the one-element Python dictionary that maps
      [key] to [value] *)

  val singleton_string: string -> Object.t -> Object.t
  (** [singleton key value] returns the one-element Python dictionary that maps
      [key] to [value] *)
end

module Err: sig
  type t =
      Exception
    | StandardError
    | ArithmeticError
    | LookupError
    | AssertionError
    | AttributeError
    | EOFError
    | EnvironmentError
    | FloatingPointError
    | IOError
    | ImportError
    | IndexError
    | KeyError
    | KeyboardInterrupt
    | MemoryError
    | NameError
    | NotImplementedError
    | OSError
    | OverflowError
    | ReferenceError
    | RuntimeError
    | SyntaxError
    | SystemExit
    | TypeError
    | ValueError
    | ZeroDivisionError

  val clear: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Clear} PyErr_Clear} *)

  val exception_matches: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_ExceptionMatches} PyErr_ExceptionMatches} *)

  val fetch: unit -> (Object.t * Object.t * Object.t) option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Fetch} PyErr_Fetch}.
   *)

  val fetched: unit -> (Object.t * Object.t * Object.t) option
  (** Exception fetched when {!Py.E} has been raised. *)

  val given_exception_matches: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_GivenExceptionMatches} PyErr_GivenExceptionMatches} *)

  val occurred: unit -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Occurred} PyErr_Occurred} *)

  val print: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Print} PyErr_Print} *)

  val print_ex: int -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_PrintEx} PyErr_PrintEx} *)

  val restore: Object.t -> Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Restore} PyErr_Restore} *)

  val set_error: t -> string -> unit
  (** [set_error e msg] calls [Py.Err.set_string e msg] with a predefined error type.
      In a closure/method/callback, it is recommended to raise a [Py.Err _] exception
      instead. *)

  val set_none: Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetNone} PyErr_SetNone} *)

  val set_string: Object.t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetString} PyErr_SetString} *)

  val set_object: Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetObject} PyErr_SetObject}.
      In a closure/method/callback, it is recommended to raise a [Py.E _] exception
      instead. *)
end

exception Err of Err.t * string
(** Represents an exception to be set with {!Err.set_error} in a callback. *)

module Eval: sig
  val call_object: Object.t -> Object.t -> Object.t
 (** See {{:https://docs.python.org/3.0/extending/extending.html} Extending Python with C or C++} *)

  val call_object_with_keywords: Object.t -> Object.t -> Object.t -> Object.t
 (** See {{:https://docs.python.org/3.0/extending/extending.html} Extending Python with C or C++} *)

  val get_builtins: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/reflection.html#c.PyEval_GetBuiltins} PyEval_GetBuiltins} *)

  val get_globals: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/reflection.html#c.PyEval_GetGlobals} PyEval_GetGlobals} *)

  val get_locals: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/reflection.html#c.PyEval_GetLocals} PyEval_GetLocals} *)
end

(** Interface for Python values of type [Float]. *)
module Float: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python float. *)

  val of_float: float -> Object.t
  (** [of_float f] returns the Python long with the value [f].
      Wrapper for
      {{:https://docs.python.org/3/c-api/float.html#c.PyFloat_AsDouble} PyFloat_AsDouble}. *)

  val to_float: Object.t -> float
  (** [to_float o] returns the floating-point vale stored in [o].
      A Python exception ([Py.E _]) is raised if [o] is not a float.
      Wrapper for
      {{:https://docs.python.org/3/c-api/float.html#c.PyFloat_FromDouble} PyFloat_FromDouble}. *)
end


(** Importing Modules *)
module Import: sig
  val cleanup: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_Cleanup} PyImport_Cleanup} *)

  val add_module: string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_AddModule} PyImport_AddModule} *)

  val exec_code_module: string -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ExecCodeModule} PyImport_ExecCodeModule} *)

  val exec_code_module_ex: string -> Object.t -> string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ExecCodeModuleEx} PyImport_ExecCodeModuleEx} *)

  val get_magic_number: unit -> int64
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_GetMagicNumber} PyImport_GetMagicNumber} *)

  val get_module_dict: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_GetModuleDict} PyImport_GetModuleDict} *)

  val import_frozen_module: string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportFrozenModule} PyImport_ImportFrozenModule} *)

  val import_module: string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModule} PyImport_ImportModule} *)

  val import_module_ex:
      string -> Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModuleEx} PyImport_ImportModuleEx} *)

  val import_module_level:
      string -> Object.t -> Object.t -> Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModuleLevel} PyImport_ImportModuleLevel} *)

  val reload_module: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ReloadModule} PyImport_ReloadModule} *)
end

(** Interface for Python values of type [Iter]. *)
module Iter: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is an iterator. *)

  val next: Object.t -> Object.t option
  (** [next i] returns the next value from the iteration [i].
      If there are no remaining values, returns [None].
      Wrapper for
      {{:https://docs.python.org/3/c-api/iter.html#c.PyIter_Next} PyIter_Next}. *)

  val iter: (Object.t -> unit) -> Object.t -> unit
  (** [iter f i] iteratively calls [f v] with all the remaining values of the
      iteration [i]. *)

  val to_list: Object.t -> Object.t list
  (** [to_list i] returns the list of all the remaining values from the
      iteration [i]. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** [to_list_map f i] returns the list of the results of [f] applied to all
      the remaining values from the iteration [i].
      [to_list_map f s] is equivalent to [List.map f (to_list s)] but is
      tail-recursive and [f] is applied to the elements of [s] in the reverse
      order. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** [fold_left f v i] returns [(f (...(f v i1)...) in)] where [i1], ..., [in]
      are the remaining values from the iteration [i]. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** [fold_right f i v] returns [(f i1 (...(f v in)...)] where [i1], ..., [in]
      are the remaining values from the iteration [i].
      This function is not tail-recursive. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** [for_all p i] checks if [p] holds for all the remaining values from the
      iteration [i]. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** [exists p i] checks if [p] holds for at least one of the remaining values
      from the iteration [i]. *)
end

(** Interface for Python values of type [List]. *)
module List: sig
  val check: Object.t -> bool
  (** [check v] returns [true] if [v] is a Python list. *)

  val create: int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/list.html#c.PyList_New} PyList_New} *)

  val get_item: Object.t -> int -> Object.t
  (** Equivalent to {!Sequence.get_item}. *)

  val get: Object.t -> int -> Object.t
  (** Equivalent to {!get_item}. *)

  val set_item: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!Sequence.set_item}. *)

  val set: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!set_item}. *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/list.html#c.PyList_Size} PyList_Size} *)

  val init: int -> (int -> Object.t) -> Object.t
  (** [init n f] returns the Python list [[f 0, f 1, ..., f (n - 1)]]. *)

  val of_array: Object.t array -> Object.t
  (** [of_array a] returns the Python list with the same elements as [a]. *)

  val of_array_map: ('a -> Object.t) -> 'a array -> Object.t
  (** [of_array_map f a] returns the Python list [(f a0, ..., f ak)] where
      [a0], ..., [ak] are the elements of [a]. *)

  val to_array: Object.t -> Object.t array
  (** Equivalent to {!Sequence.to_array}. *)

  val to_array_map: (Object.t -> 'a) -> Object.t -> 'a array
  (** Equivalent to {!Sequence.to_array_map}. *)

  val of_list: Object.t list -> Object.t
  (** [of_list l] returns the Python list with the same elements as [l]. *)

  val of_list_map: ('a -> Object.t) -> 'a list -> Object.t
  (** [of_list f l] returns the Python list [(f l1, ..., f ln)] where
      [l1], ..., [ln] are the elements of [l].
      [of_list_map f l] is equivalent to [of_list (List.map f l)] but is
      tail-recursive and [f] is applied to the elements of [l] in the reverse
      order. *)

  val to_list: Object.t -> Object.t list
  (** Equivalent to {!Sequence.to_list}. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** Equivalent to {!Sequence.to_list_map}. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** Equivalent to {!Sequence.fold_left}. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** Equivalent to {!Sequence.fold_right}. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.for_all}. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.exists}. *)

  val of_sequence: Object.t -> Object.t
  (** Equivalent to {!Sequence.list}. *)

  val singleton: Object.t -> Object.t
  (** [singleton o] returns the Python list [[o]]. *)
end

(** Interface for Python values with a [Mapping] interface. *)
module Mapping: sig
  val check: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_Check} PyMapping_Check} *)

  val get_item_string: Object.t -> string -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_GetItemString} PyMapping_GetItemString} *)

  val find_string: Object.t -> string -> Object.t
  (** Equivalent to {!get_item_string} but raises a [Not_found] exception in
      case of failure. *)

  val has_key: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_HasKey} PyMapping_HasKey} *)

  val has_key_string: Object.t -> string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_HasKeyString} PyMapping_HasKeyString} *)

  val length: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_Length} PyMapping_Length} *)

  val set_item_string: Object.t -> string -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_SetItemString} PyMapping_SetItemString} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_Size} PyMapping_Size} *)
end

(** Interface for Python values of type [Method]. *)
module Method: sig
  val create: Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/method.html#c.PyMethod_New} PyMethod_New} *)

  val get_function: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/method.html#c.PyMethod_GetFunction} PyMethod_GetFunction} *)

  val self: Object.t -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/method.html#c.PyMethod_Self} PyMethod_Self} *)
end

(** Interface for Python values of type [Module]. *)
module Module: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python module. *)

  val create: string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_New} PyModule_New} *)

  val get_dict: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_GetDict} PyModule_GetDict} *)

  val get_filename: Object.t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_GetFilename} PyModule_GetFilename} *)

  val get_name: Object.t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_GetName} PyModule_GetName} *)

  val get: Object.t -> string -> Object.t
  (** Equivalent to {!Object.get_attr_string}. *)

  val set: Object.t -> string -> Object.t -> unit
  (** Equivalent to {!Object.set_attr_string}. *)

  val remove: Object.t -> string -> unit
  (** Equivalent to {!Object.del_attr_string}. *)

  val main: unit -> Object.t
  (** Returns the [__main__] module.
      We have [Py.Module.main () = Py.Module.add_module "__main__"]. *)

  val sys: unit -> Object.t
  (** Returns the [sys] module.
      We have [Py.Module.sys () = Py.Module.import_module "sys"]. *)

  val builtins: unit -> Object.t
  (** Returns the [__builtins__] module.
      We have
[Py.Module.builtins () = Py.Module.find (Py.Module.main ()) "__builtins__"]. *)
end

(** Interface for Python values of type [Number]. *)
module Number: sig
  val absolute: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Absolute} PyNumber_Absolute} *)

  val add: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Add} PyNumber_Add} *)

  val number_and: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_And} PyNumber_And} *)

  val check: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Check} PyNumber_Check} *)

  val divmod: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Divmod} PyNumber_Divmod} *)

  val float: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Float} PyNumber_Float} *)

  val floor_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_FloorDivide} PyNumber_FloorDivide} *)

  val in_place_add: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceAdd} PyNumber_InPlaceAdd} *)

  val in_place_and: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceAnd} PyNumber_InPlaceAnd} *)

  val in_place_floor_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceFloorDivide} PyNumber_InPlaceFloorDivide} *)

  val in_place_lshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceLshift} PyNumber_InPlaceLshift} *)

  val in_place_multiply: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceMultiply} PyNumber_InPlaceMultiply} *)

  val in_place_or: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceOr} PyNumber_InPlaceOr} *)

  val in_place_power: Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlacePower} PyNumber_InPlacePower} *)

  val in_place_remainder: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceRemainder} PyNumber_InPlaceRemainder} *)

  val in_place_rshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceRshift} PyNumber_InPlaceRshift} *)

  val in_place_subtract: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceSubtract} PyNumber_InPlaceSubtract} *)

  val in_place_true_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceTrueDivide} PyNumber_InPlaceTrueDivide} *)

  val in_place_xor: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceXor} PyNumber_InPlaceXor} *)

  val invert: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Invert} PyNumber_Invert} *)

  val lshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Lshift} PyNumber_Lshift} *)

  val multiply: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Multiply} PyNumber_Multiply} *)

  val negative: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Negative} PyNumber_Negative} *)

  val number_or: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Or} PyNumber_Or} *)

  val positive: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Positive} PyNumber_Positive} *)

  val power: Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Power} PyNumber_Power} *)

  val remainder: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Remainder} PyNumber_Remainder} *)

  val rshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Rshift} PyNumber_Rshift} *)

  val subtract: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Subtract} PyNumber_Subtract} *)

  val true_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_TrueDivide} PyNumber_TrueDivide} *)

  val number_xor: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Xor} PyNumber_Xor} *)

  val check: Object.t -> bool
  (** [check v] returns [true] if [v] is a Python float or a Python
      integer/long. *)

  val to_float: Object.t -> float
  (** [to_float v] returns the floating-point value equal to the Python integer
      or Python float [v]. Raises a failure ([Failure _]) if [v] is neither a
      float nor an integer. *)
end

type input = Pytypes.input = Single | File | Eval

(** Interface for Python values of type [Run]. *)
module Run: sig
  val eval: ?start:input -> ?globals:Object.t -> ?locals:Object.t -> string
    -> Object.t
  (** [eval ~start ~globals ~locals e]
      evaluates the Python expression [e] and returns the computed value.
      We have
[Py.Run.eval ~start ~globals ~locals e = Py.Run.String e start globals locals].
      @param start is the initial input mode (default: [Eval]).
      @param globals is the global symbol directory
      (default: Module.get_dict (Module.main ())).
      @param locals is the local symbol directory (default: [Dict.create ()]).
   *)

  val load: ?start:input -> ?globals:Object.t -> ?locals:Object.t ->
    in_channel -> string -> Object.t
  (** [load ~start ~globals ~locals chan filename] loads the contents of the file
      opened in [chan].
      We have
[Py.Run.load ~start ~globals ~locals chan filename = Py.Run.file chan filename start globals locals].
      @param start is the initial input mode (default: [File]).
      @param globals is the global symbol directory
      (default: Module.get_dict (Module.main ())).
      @param locals is the local symbol directory (default: [Dict.create ()]). *)

  val interactive: unit -> unit
  (** Runs the interactive loop.
      We have [Py.Run.interactive () = Py.Run.interactive_loop stdin "<stdin>"].
   *)

  val ipython: unit -> unit
  (** Runs the IPython interactive loop. *)

  val any_file: in_channel -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_AnyFile} PyRun_AnyFile} *)

  val file: in_channel -> string -> input -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_File} PyRun_File} *)

  val interactive_one: in_channel -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_InteractiveOne} PyRun_InteractiveOne} *)

  val interactive_loop: in_channel -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_InteractiveLoop} PyRun_InteractiveLoop} *)

  val simple_file: in_channel -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_SimpleFile} PyRun_SimpleFile} *)

  val simple_string: string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_SimpleString} PyRun_SimpleString} *)

  val string: string -> input -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_String} PyRun_String} *)
end

(** Interface for Python values with a [Sequence] interface. *)
module Sequence: sig
  val check: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Check} PySequence_Check} *)

  val concat: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Concat} PySequence_Concat} *)

  val contains: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Contains} PySequence_Contains} *)

  val count: Object.t -> Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Count} PySequence_Count} *)

  val del_item: Object.t -> int -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_DelItem} PySequence_DelItem} *)

  val fast: Object.t -> string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Fast} PySequence_Fast} *)

  val get_item: Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_GetItem} PySequence_GetItem} *)

  val get: Object.t -> int -> Object.t
  (** Equivalent to {!get_item}. *)

  val get_slice: Object.t -> int -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_GetSlice} PySequence_GetSlice} *)

  val index: Object.t -> Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Index} PySequence_Index} *)

  val in_place_concat: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_InPlaceConcat} PySequence_InPlaceConcat} *)

  val in_place_repeat: Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_InPlaceRepeat} PySequence_InPlaceRepeat} *)

  val length: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Length} PySequence_Length} *)

  val list: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_List} PySequence_List} *)

  val repeat: Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Repeat} PySequence_Repeat} *)

  val set_item: Object.t -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_SetItem} PySequence_SetItem} *)

  val set: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!set_item}. *)

  val set_slice: Object.t -> int -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_SetSlice} PySequence_SetSlice} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Size} PySequence_Size} *)

  val tuple: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Tuple} PySequence_Tuple} *)

  val to_array: Object.t -> Object.t array
  (** [to_array s] returns the array with the same elements as the Python
      sequence [s]. *)

  val to_array_map: (Object.t -> 'a) -> Object.t -> 'a array
  (** [to_array_map f s] returns the array of the results of [f] applied to
      all the elements of the Python sequence [s]. *)

  val to_list: Object.t -> Object.t list
  (** [to_list s] returns the list with the same elements as the Python
      sequence [s]. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** [to_list_map f s] returns the list of the results of [f] applied to all
      the elements of the Python sequence [s].
      [to_list_map f s] is equivalent to [List.map f (to_list s)] but is
      tail-recursive and [f] is applied to the elements of [s] in the reverse
      order. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** [fold_left f v s] returns [(f (...(f v s1)...) sn)] where [s1], ..., [sn]
      are the elements of the Python sequence [s]. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** [fold_right f s v] returns [(f s1 (...(f v sn)...)] where [s1], ..., [sn]
      are the elements of the Python sequence [s].
      This function is tail-recursive. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** [for_all p s] checks if [p] holds for all the elements of the Python
      sequence [s]. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** [exists p s] checks if [p] holds for at least one of the elements of the
      Python sequence [s]. *)
end

type byteorder =
    LittleEndian
  | BigEndian

(** Interface for Python values of type [String], [Bytes] and [Unicode]. *)
module String: sig
  val check: Object.t -> bool
  (** [check o] returns [o] if [o] is a Python string
      (either [Bytes] or [Unicode] with Python 3). *)

  val format: Object.t -> Object.t -> Object.t
  (** [format fmt args] returns the formatted Python string from the string
      format [fmt] and the arguments [args].
      This is analogous to [fmt % args].
      With Python 2, if [fmt] is a String, wrapper for
      {{:https://docs.python.org/2/c-api/string.html#c.PyString_Format} PyString_Format}.
      With Python 3 or with Python 2 if [fmt] is Unicode, wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_Format} PyUnicode_Format}. *)

  val as_UTF8_string: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_AsUTF8String} PyUnicode_AsUTF8String} *)

  val decode_UTF8: ?errors:string -> ?size:int -> string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_DecodeUTF8} PyUnicode_DecodeUTF8}.
      If [size] is omitted, the length of the string is used by default. *)

  val decode_UTF16: ?errors:string -> ?size:int -> ?byteorder:byteorder
    -> string -> Object.t * byteorder
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_DecodeUTF16} PyUnicode_DecodeUTF16}.
      If [size] is omitted, the length of the string is used by default. *)

  val decode_UTF32: ?errors:string -> ?size:int -> ?byteorder:byteorder
    -> string -> Object.t * byteorder
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_DecodeUTF32} PyUnicode_DecodeUTF32}.
      If [size] is omitted, the length of the string is used by default. *)

  val length: Object.t -> int
  (** [length s] returns the length of the Python string [s].
      A failure ([Failure _]) is raised if [s] is neither a [Bytes] value
      nor a [Unicode] value.
      With Python 2,
      if [s] is a String, wrapper for
      {{:https://docs.python.org/2/c-api/string.html#c.PyString_Size} PyString_Size},
      and if [s] is Unicode, wrapper for
      {{:https://docs.python.org/2/c-api/unicode.html#c.PyUnicode_GetSize} PyUnicode_GetSize},
      With Python 3,
      if [s] is Bytes, wrapper for
      {{:https://docs.python.org/2/c-api/bytes.html#c.PyBytes_Size} PyBytes_Size},
      and if [s] is Unicode, wrapper for
      {{:https://docs.python.org/2/c-api/unicode.html#c.PyUnicode_GetLength} PyUnicode_GetLength}. *)

  val of_string: string -> Object.t
  (** [of_string s] returns the Python string with the value [s]. *)

  val to_string: Object.t -> string
  (** [to_string o] returns the string contained in the Python value [o].
      A failure ([Failure _]) is raised if [o] is neither a
      [String]/[Bytes] value nor a [Unicode] value. *)

  val of_unicode: ?size:int -> int array -> Object.t
  (** [of_unicode codepoints] returns the Python Unicode string with the
      codepoints [codepoints]. *)

  val to_unicode: Object.t -> int array
  (** [to_unicode s] returns the codepoints of the Python Unicode string
      [s]. *)
end

(** Interface for Python values of type [Tuple]. *)
module Tuple: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python tuple. *)

  val create: int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/tuple.html#c.PyTuple_New} PyTuple_New} *)

  val empty: Object.t
  (** The empty tuple [()]. *)

  val get_item: Object.t -> int -> Object.t
  (** Equivalent to {!Sequence.get_item}. *)

  val get: Object.t -> int -> Object.t
  (** Equivalent to {!get_item}. *)

  val set_item: Object.t -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PyTuple_SetItem} PyTuple_SetItem} *)

  val set: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!set_item}. *)

  val get_slice: Object.t -> int -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/tuple.html#c.PyTuple_GetSlice} PyTuple_GetSlice} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/tuple.html#c.PyTuple_Size} PyTuple_Size} *)

  val init: int -> (int -> Object.t) -> Object.t
  (** [init n f] returns the Python tuple [(f 0, f 1, ..., f (n - 1))]. *)

  val of_array: Object.t array -> Object.t
  (** [of_array a] returns the Python tuple with the same elements as [a]. *)

  val of_array_map: ('a -> Object.t) -> 'a array -> Object.t
  (** [of_array_map f a] returns the Python tuple [(f a0, ..., f ak)] where
      [a0], ..., [ak] are the elements of [a]. *)

  val of_list: Object.t list -> Object.t
  (** [of_list l] returns the Python tuple with the same elements as [l]. *)

  val of_list_map: ('a -> Object.t) -> 'a list -> Object.t
  (** [of_list f l] returns the Python tuple [(f l1, ..., f ln)] where
      [l1], ..., [ln] are the elements of [l].
      [of_list_map f l] is equivalent to [of_list (List.map f l)] but is
      tail-recursive. *)

  val to_array: Object.t -> Object.t array
  (** Equivalent to {!Sequence.to_array}. *)

  val to_array_map: (Object.t -> 'a) -> Object.t -> 'a array
  (** Equivalent to {!Sequence.to_array_map}. *)

  val to_list: Object.t -> Object.t list
  (** Equivalent to {!Sequence.to_list}. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** Equivalent to {!Sequence.to_list_map}. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** Equivalent to {!Sequence.fold_left}. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** Equivalent to {!Sequence.fold_right}. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.for_all}. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.exists}. *)

  val of_sequence: Object.t -> Object.t
  (** Equivalent to {!Sequence.tuple}. *)

  val of_tuple1: Object.t -> Object.t
  (** [of_tuple1 o0] returns the Python tuple [(o0)]. *)

  val of_tuple2: Object.t * Object.t -> Object.t
  (** [of_tuple4 (o0, o1)] returns the Python tuple [(o0, o1)]. *)

  val of_tuple3: Object.t * Object.t * Object.t -> Object.t
  (** [of_tuple4 (o0, o1, o2)] returns the Python tuple [(o0, o1, o2)]. *)

  val of_tuple4: Object.t * Object.t * Object.t * Object.t -> Object.t
  (** [of_tuple4 (o0, o1, o2, o3)] returns the Python tuple
      [(o0, o1, o2, o3)]. *)

  val of_tuple5:
    Object.t * Object.t * Object.t * Object.t * Object.t -> Object.t
  (** [of_tuple5 (o0, o1, o2, o3, o4)] returns the Python tuple
      [(o0, o1, o2, o3, o4)]. *)

  val to_tuple1: Object.t -> Object.t
  (** [to_tuple1 t] returns the value [Py.Tuple.get_item t 0]. *)

  val to_tuple2: Object.t -> Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1)]. *)

  val to_tuple3: Object.t -> Object.t * Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1, Py.Tuple.get_item t 2)]. *)

  val to_tuple4: Object.t -> Object.t * Object.t * Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1, Py.Tuple.get_item t 2, Py.Tuple.get_item t 3)]. *)

  val to_tuple5:
    Object.t -> Object.t * Object.t * Object.t * Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1, Py.Tuple.get_item t 2, Py.Tuple.get_item t 3,
      Py.Tuple.get_item t 4)]. *)

  val singleton: Object.t -> Object.t
  (** Equivalent to {!of_tuple1}. *)

  val to_singleton: Object.t -> Object.t
  (** Equivalent to {!to_tuple1}. *)

  val of_pair: Object.t * Object.t -> Object.t
  (** Equivalent to {!of_tuple2}. *)

  val to_pair: Object.t -> Object.t * Object.t
  (** Equivalent to {!to_tuple2}. *)
end

(** Introspection of Python types *)
module Type: sig
  type t =
      Unknown
    | Bool
    | Bytes
    | Callable
    | Capsule
    | Closure
    | Dict
    | Float
    | List
    | Long
    | Module
    | None
    | Null
    | Tuple
    | Type
    | Unicode
    | Iter
  (** Some types of Python values.
      [Bytes] covers both the [Str] values of Python 2
      and the [Bytes] values of Python 3.
      [Long] covers both the [Int] values of Python 2
      and the [Long] values of Python 3.
      [Capsule] corresponds to the values created with {!Py.Capsule}.
      [Closure] corresponds to the values created with {!Py.Callable}. *)

  val get: Object.t -> t
  (** [get o] returns the type of the Python value [o]. *)

  val is_subtype: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/type.html#c.PyType_IsSubtype} PyType_IsSubtype} *)

  val name: t -> string
  (** [name t] returns a string that represents the type [t]. *)

  val mismatch: string -> Object.t -> 'a
  (** [mismatch ty obj] raises a type mismatch [Failure _] that indicates that
      an object of type [ty] was expected, but [obj] was found. *)
end

val set_argv: string array -> unit
(** [set_argv argv] set Python's [sys.argv]. *)

val last_value: unit -> Object.t
(** [last_value ()] returns the last value that was computed in the
    toplevel.
    We have [Py.last_value = Py.Module.find (Py.Module.builtins ()) "_"]. *)

module Utils: sig
    (** This module declares utility functions that does not require Python to
        be initialized. *)

val try_finally: ('a -> 'b) -> 'a -> ('c -> unit) -> 'c -> 'b
(** [try_finally f arg finally finally_arg] calls [f arg], and returns the
    result of [f].
    [finally finally_arg] is always closed after [f] has been called, even if
    [f] raises an exception. *)

val read_and_close: in_channel -> ('a -> 'b) -> 'a -> 'b
(** [read_and_close channel f arg] calls [f arg], and returns the result of [f].
    [channel] is always closed after [f] has been called, even if [f] raises an
    exception.
    This is an utility function that does not require Python to be
    initialized. *)

val write_and_close: out_channel -> ('a -> 'b) -> 'a -> 'b
(** [write_and_close channel f arg] calls [f arg], and returns the result of
    [f].
    [channel] is always closed after [f] has been called, even if [f] raises an
    exception.
 *)
end
