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
  | AnyType

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

include Pywrappers.Pycaml

let make_pill_wrapping name instance = Py.Capsule.make name

let py_false () = Py.Bool.f

let py_finalize = Py.finalize

let py_initialize = Py.initialize

let py_is_true = Py.Object.is_true

let py_isinitialized () =
  if Py.is_initialized () then 1
  else 0

let py_setprogramname = Py.set_program_name

let py_setpythonhome = Py.set_python_home

let py_getprogramname = Py.get_program_name

let py_getpythonhome = Py.get_python_home

let py_getprogramfullpath = Py.get_program_full_path

let py_getprefix = Py.get_prefix

let py_getexecprefix = Py.get_exec_prefix

let py_getpath = Py.get_path

let py_true () = Py.Bool.t

let pycaml_seterror error msg =
  let error' =
    match error with
      Pyerr_Exception -> Py.Err.Exception
    | Pyerr_ArithmeticError -> Py.Err.ArithmeticError
    | Pyerr_LookupError -> Py.Err.LookupError
    | Pyerr_AssertionError -> Py.Err.AssertionError
    | Pyerr_AttributeError -> Py.Err.AttributeError
    | Pyerr_EOFError -> Py.Err.EOFError
    | Pyerr_EnvironmentError -> Py.Err.EnvironmentError
    | Pyerr_FloatingPointError -> Py.Err.FloatingPointError
    | Pyerr_IOError -> Py.Err.IOError
    | Pyerr_ImportError -> Py.Err.ImportError
    | Pyerr_IndexError -> Py.Err.IndexError
    | Pyerr_KeyError -> Py.Err.KeyError
    | Pyerr_KeyboardInterrupt -> Py.Err.KeyboardInterrupt
    | Pyerr_MemoryError -> Py.Err.MemoryError
    | Pyerr_NameError -> Py.Err.NameError
    | Pyerr_NotImplementedError -> Py.Err.NotImplementedError
    | Pyerr_OSError -> Py.Err.OSError
    | Pyerr_OverflowError -> Py.Err.OverflowError
    | Pyerr_ReferenceError -> Py.Err.ReferenceError
    | Pyerr_RuntimeError -> Py.Err.RuntimeError
    | Pyerr_SyntaxError -> Py.Err.SyntaxError
    | Pyerr_SystemExit -> Py.Err.SystemExit
    | Pyerr_TypeError -> Py.Err.TypeError
    | Pyerr_ValueError -> Py.Err.ValueError
    | Pyerr_ZeroDivisionError -> Py.Err.ZeroDivisionError in
  Py.Err.set_error error' msg

let int_of_bool b =
  if b then -1
  else 0

let pybytes_check v = int_of_bool (Py.Type.get v = Py.Type.Bytes)

let pybytes_asstring = Py.String.to_string

let pyiter_check v = int_of_bool (Py.Type.get v = Py.Type.Iter)

let pymodule_getfilename = Py.Module.get_filename

let pymodule_getname = Py.Module.get_name

let pyunicode_check v = int_of_bool (Py.Type.get v = Py.Type.Unicode)

let pyerr_fetch _ =
  match Py.Err.fetch () with
    None -> (Py.null, Py.null, Py.null)
  | Some e -> e

let pyerr_normalizeexception e = e

let pylist_toarray = Py.Sequence.to_array

let pyint_asint = Py.Long.to_int

let pyint_fromint = Py.Long.of_int

let pynone () = Py.none

let pynull () = Py.null

let pystring_asstring = Py.String.to_string

let pystring_fromstring = Py.String.of_string

let pytuple_fromarray = Py.Tuple.of_array

let pytuple_fromsingle = Py.Tuple.singleton

let pytuple_toarray = Py.Tuple.to_array

let pytype v =
  match Py.Type.get v with
    Py.Type.Unknown | Py.Type.Iter -> OtherType
  | Py.Type.Bool -> BoolType
  | Py.Type.Bytes -> BytesType
  | Py.Type.Callable -> CallableType
  | Py.Type.Capsule -> CamlpillType
  | Py.Type.Closure -> CallableType
  | Py.Type.Dict -> DictType
  | Py.Type.Float -> FloatType
  | Py.Type.List -> ListType
  | Py.Type.Long -> IntType
  | Py.Type.Module -> ModuleType
  | Py.Type.None -> NoneType
  | Py.Type.Null -> NullType
  | Py.Type.Tuple -> TupleType
  | Py.Type.Type -> TypeType
  | Py.Type.Unicode -> UnicodeType

let register_ocamlpill_types array = ()

let pyeval_callobject (func, arg) =
  Pywrappers.pyeval_callobjectwithkeywords func arg Py.null

let pyimport_importmoduleex (name, globals, locals, fromlist) =
  Pywrappers.pyimport_importmodulelevel name globals locals fromlist (-1)

let input_of_int input =
  match input with
    256 -> Py.Single
  | 257 -> Py.File
  | 258 -> Py.Eval
  | _ -> failwith "input_of_int"

let py_compilestring (str, filename, start) =
  if Py.version_major () <= 2 then
    Pywrappers.Python2.py_compilestringflags str filename (input_of_int start)
      None
  else
    Pywrappers.Python3.py_compilestringexflags str filename (input_of_int start)
      None (-1)

let pyrun_anyfile (fd, filename) =
  Pywrappers.pyrun_anyfileexflags fd filename 0 None

let pyrun_anyfileex (fd, filename, closeit) =
  Pywrappers.pyrun_anyfileexflags fd filename closeit None

let pyrun_file (fd, filename, start, globals, locals) =
  Pywrappers.pyrun_fileexflags fd filename (input_of_int start) globals locals 0
    None

let pyrun_fileex (fd, filename, start, globals, locals, closeit) =
  Pywrappers.pyrun_fileexflags fd filename (input_of_int start) globals locals
    closeit None

let pyrun_interactiveone (fd, filename) =
  Pywrappers.pyrun_interactiveoneflags fd filename None

let pyrun_interactiveloop (fd, filename) =
  Pywrappers.pyrun_interactiveloopflags fd filename None

let pyrun_simplefile (fd, filename) =
  Pywrappers.pyrun_simplefileexflags fd filename 0 None

let pyrun_simplefileex (fd, filename, closeit) =
  Pywrappers.pyrun_simplefileexflags fd filename closeit None

let pyrun_simplestring s = Pywrappers.pyrun_simplestringflags s None

let pyrun_string (s, start, globals, locals) =
  Pywrappers.pyrun_stringflags s (input_of_int start) globals locals None
