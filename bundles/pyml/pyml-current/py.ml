type pyobject = Pytypes.pyobject

type input = Pytypes.input = Single | File | Eval

type compare = Pytypes.compare = LT | LE | EQ | NE | GT | GE

type ucs = UCSNone | UCS2 | UCS4

external load_library: int -> string option -> unit = "py_load_library"
external finalize_library: unit -> unit = "py_finalize_library"
external pywrap_closure: string -> (pyobject -> pyobject) -> pyobject
    = "pywrap_closure"
external pynull: unit -> pyobject = "PyNull_wrapper"
external pynone: unit -> pyobject = "PyNone_wrapper"
external pytrue: unit -> pyobject = "PyTrue_wrapper"
external pyfalse: unit -> pyobject = "PyFalse_wrapper"
external pytuple_empty: unit -> pyobject = "PyTuple_Empty_wrapper"
external pyobject_callfunctionobjargs: pyobject -> (pyobject array) -> pyobject
    = "PyObject_CallFunctionObjArgs_wrapper"
external pyerr_fetch_internal: unit -> pyobject * pyobject * pyobject
    = "PyErr_Fetch_wrapper"
external pystring_asstringandsize: pyobject -> string option
    = "PyString_AsStringAndSize_wrapper"
external pyobject_ascharbuffer: pyobject -> string option
    = "PyObject_AsCharBuffer_wrapper"
external pyobject_asreadbuffer: pyobject -> string option
    = "PyObject_AsReadBuffer_wrapper"
external pyobject_aswritebuffer: pyobject -> string option
    = "PyObject_AsWriteBuffer_wrapper"

external ucs: unit -> ucs = "py_get_UCS"

external fd_of_descr: Unix.file_descr -> int = "%identity"

let initialized = ref false

let is_initialized () = !initialized

let assert_initialized () =
  if not !initialized then
    failwith "Py.assert_initialized: run 'Py.initialize ()' first"

let version_value = ref ""

let version_major_value = ref 0

let version_minor_value = ref 0

let program_name = ref Sys.argv.(0)

let set_program_name s =
  program_name := s;
  if !initialized then
    if !version_major_value <= 2 then
      Pywrappers.Python2.py_setprogramname s
    else
      Pywrappers.Python3.py_setprogramname s

let python_home = ref None

let set_python_home s =
  python_home := (Some s);
  if !initialized then
    if !version_major_value <= 2 then
      Pywrappers.Python2.py_setpythonhome s
    else
      Pywrappers.Python3.py_setpythonhome s

let substring_between string before after =
  String.sub string (before + 1) (after - before - 1)

let extract_version version_line =
  let before =
    try String.index version_line ' '
    with Not_found ->
      let msg =
        Printf.sprintf "Py.extract_version: cannot parse the version line '%s'"
          version_line in
      failwith msg in
  let after =
    try String.index_from version_line (succ before) ' '
    with Not_found -> String.length version_line in
  substring_between version_line before after

let extract_version_major_minor version =
  try
    let first_dot = String.index version '.' in
    let second_dot = String.index_from version (succ first_dot) '.' in
    let major = int_of_string (String.sub version 0 first_dot) in
    let minor =
      int_of_string (substring_between version first_dot second_dot) in
    (major, minor)
  with Not_found | Failure _ ->
    let msg =
      Printf.sprintf
        "Py.extract_version_major: unable to parse the version number '%s'"
        version in
    failwith msg

let run_command command read_stderr =
  let (input, output, error) =
    Unix.open_process_full command (Unix.environment ()) in
  let result =
    try
      input_line (if read_stderr then error else input)
    with _ ->
      begin
        try
          ignore (Unix.close_process_full (input, output, error))
        with _ ->
          ()
      end;
      let msg =
        Printf.sprintf "Py.run_command: unable to read the result of '%s'"
          command in
      failwith msg in
  if Unix.close_process_full (input, output, error) <> Unix.WEXITED 0 then
    begin
      let msg = Printf.sprintf "Py.run_command: unable to run '%s'" command in
      failwith msg;
    end;
  result

let suffix string from =
  String.sub string from (String.length string - from)

let rec split string ?(from=0) separator =
  match
    try Some (String.index_from string from separator)
    with Not_found -> None
  with
    None -> [suffix string from]
  | Some position ->
      let word = String.sub string from (position - from) in
      word :: split string ~from:(succ position) separator

let find_library_path version_major version_minor =
  let command =
    Printf.sprintf "pkg-config --libs python-%d.%d" version_major
      version_minor in
  match
    try Some (run_command command false)
    with Failure _ -> None
  with
    None ->
      let library_paths =
        try
          [Filename.concat (Filename.dirname (run_command "which python" false))
             "../lib"]
        with Failure _ -> [] in
      let library_filenames =
        [Printf.sprintf "python%d.%dm" version_major version_minor;
         Printf.sprintf "python%d.%d" version_major version_minor] in
      (library_paths, library_filenames)
  | Some words ->
      let word_list = split words ' ' in
      let unable_to_parse () =
        let msg = Printf.sprintf
        "Py.find_library_path: unable to parse the output of pkg-config '%s'"
            words in
        failwith msg in
      let parse_word (library_paths, library_filename) word =
        if String.length word > 2 then
          match String.sub word 0 2 with
            "-L" -> (suffix word 2 :: library_paths, library_filename)
          | "-l" ->
              if library_filename <> None then
                unable_to_parse ();
              (library_paths, Some (suffix word 2))
          | _ -> (library_paths, library_filename)
        else (library_paths, library_filename) in
      let (library_paths, library_filename) =
        List.fold_left parse_word ([], None) word_list in
      let library_filename =
        match library_filename with
          None -> unable_to_parse ()
        | Some library_filename -> library_filename in
      (library_paths, [library_filename])

let initialize_version_value python =
  let version_line = run_command (Printf.sprintf "%s --version" python) true in
  let version = extract_version version_line in
  let (version_major, version_minor) = extract_version_major_minor version in
  version_value := version;
  version_major_value := version_major;
  version_minor_value := version_minor

let find_library () =
  try
    load_library !version_major_value None
  with Failure _ ->
    let (library_paths, library_filenames) =
      find_library_path !version_major_value !version_minor_value in
    let expand_filenames filename =
      [Printf.sprintf "lib%s.so" filename;
       Printf.sprintf "lib%s.dylib" filename] in
    let library_filenames =
      List.concat (List.map expand_filenames library_filenames) in
    let expand_filepaths filename =
      filename ::
      List.map (fun path -> Filename.concat path filename) library_paths in
    let library_filenames =
      List.concat (List.map expand_filepaths library_filenames) in
    let rec try_load_library library_filenames =
      match library_filenames with
        [] -> failwith "Py.find_library: unable to find the Python library"
      | filename :: others ->
          begin
            try load_library !version_major_value (Some filename)
            with Failure _ -> try_load_library others
          end in
    try_load_library library_filenames

let initialize_library () =
  find_library ();
  set_program_name !program_name;
  begin
    match !python_home with
      None -> ()
    | Some s -> set_python_home s
  end

let initialize ?(interpreter = "python") () =
  if !initialized then
    failwith "Py.initialize: already initialized";
  initialize_version_value interpreter;
  initialize_library ();
  initialized := true

let finalize () =
  assert_initialized ();
  finalize_library ();
  initialized := false

let version () =
  assert_initialized ();
  !version_value

let version_major () =
  assert_initialized ();
  !version_major_value

let version_minor () =
  assert_initialized ();
  !version_minor_value

let null =
  pynull ()

let none =
  pynone ()

exception E of pyobject * pyobject

let fetched_exception = ref None

let python_exception () =
  let ptype, pvalue, ptraceback = pyerr_fetch_internal () in
  fetched_exception := Some (ptype, pvalue, ptraceback);
  raise (E (ptype, pvalue))

let check_not_null result =
  if result = null then
    python_exception ();
  result

let check_some s =
  match s with
    None -> python_exception ()
  | Some s -> s

let check_error () =
  if Pywrappers.pyerr_occurred () <> null then
    python_exception ()

let check_int result =
  if result = -1 then
    python_exception ()
  else
    result

let check_int64 result =
  if result = -1L then
    python_exception ()
  else
    result

let assert_int_success result =
  if result = -1 then
    python_exception ()

let bool_of_int i = check_int i <> 0

let get_program_name () =
  if !initialized then
    if !version_major_value <= 2 then
      Pywrappers.Python2.py_getprogramname ()
    else
      Pywrappers.Python3.py_getprogramname ()
  else
    !program_name

let get_python_home () =
  if !initialized then
    if !version_major_value <= 2 then
      Pywrappers.Python2.py_getpythonhome ()
    else
      Pywrappers.Python3.py_getpythonhome ()
  else
    match !python_home with
      None -> ""
    | Some s -> s

let get_program_full_path () =
  if version_major () <= 2 then
    Pywrappers.Python2.py_getprogramfullpath ()
  else
    Pywrappers.Python3.py_getprogramfullpath ()

let get_prefix () =
  if version_major () <= 2 then
    Pywrappers.Python2.py_getprogramfullpath ()
  else
    Pywrappers.Python3.py_getprogramfullpath ()

let get_exec_prefix () =
  if version_major () <= 2 then
    Pywrappers.Python2.py_getexecprefix ()
  else
    Pywrappers.Python3.py_getexecprefix ()

let get_path () =
  if version_major () <= 2 then
    Pywrappers.Python2.py_getpath ()
  else
    Pywrappers.Python3.py_getpath ()

let get_version = Pywrappers.py_getversion

let get_platform = Pywrappers.py_getplatform

let get_copyright = Pywrappers.py_getcopyright

let get_compiler = Pywrappers.py_getcompiler

let get_build_info = Pywrappers.py_getbuildinfo

let option result =
  if result = null then
    begin
      check_error ();
      None
    end
  else
    Some result

module Capsule = struct
  let is_valid v name  = Pywrappers.pycapsule_isvalid v name <> 0

  let check v = is_valid v "ocaml-capsule"

  let table = Hashtbl.create 17

  external unsafe_wrap_value: 'a -> pyobject = "pywrap_value"

  external unsafe_unwrap_value: pyobject -> 'a = "pyunwrap_value"

  let make name =
    try
      Hashtbl.find table name;
      failwith
        (Printf.sprintf "Py.Capsule.make: capsule of type %s already defined"
           name)
    with Not_found ->
      Hashtbl.add table name ();
      let wrap v = unsafe_wrap_value (name, v) in
      let unwrap x =
        let name', v = unsafe_unwrap_value x in
        if name <> name' then
          failwith
            (Printf.sprintf
               "Py.Capsule: capsule of type %s, but type %s expected"
               name' name);
        v in
      (wrap, unwrap)

  let type_of x = fst (unsafe_unwrap_value x)
end

module Eval = struct
  let call_object_with_keywords func arg keyword =
    check_not_null (Pywrappers.pyeval_callobjectwithkeywords func arg keyword)

  let call_object func arg =
    call_object_with_keywords func arg null

  let get_builtins () = check_not_null (Pywrappers.pyeval_getbuiltins ())

  let get_globals () = check_not_null (Pywrappers.pyeval_getglobals ())

  let get_locals () = check_not_null (Pywrappers.pyeval_getlocals ())
end

module Import = struct
  let cleanup = Pywrappers.pyimport_cleanup

  let add_module name = check_not_null (Pywrappers.pyimport_addmodule name)

  let exec_code_module name obj =
    check_not_null (Pywrappers.pyimport_execcodemodule name obj)

  let exec_code_module_ex name obj pathname =
    check_not_null (Pywrappers.pyimport_execcodemoduleex name obj pathname)

  let get_magic_number = Pywrappers.pyimport_getmagicnumber

  let get_module_dict () =
    check_not_null (Pywrappers.pyimport_getmoduledict ())

  let import_frozen_module name =
    bool_of_int (Pywrappers.pyimport_importfrozenmodule name)

  let import_module name =
    check_not_null (Pywrappers.pyimport_importmodule name)

  let import_module_level name globals locals fromlist level =
    check_not_null
      (Pywrappers.pyimport_importmodulelevel name globals locals fromlist level)

  let import_module_ex name globals locals fromlist =
    import_module_level name globals locals fromlist (-1)

  let reload_module obj =
    check_not_null (Pywrappers.pyimport_reloadmodule obj)
end

let object_repr obj = check_not_null (Pywrappers.pyobject_repr obj)

let as_UTF8_string s =
  let f =
    match ucs () with
      UCS2 -> Pywrappers.UCS2.pyunicodeucs2_asutf8string
    | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_asutf8string
    | UCSNone ->
        if !version_major_value >= 3 then
          Pywrappers.Python3.pyunicode_asutf8string
        else
          failwith "Py.as_UTF8_string: unavailable" in
  check_not_null (f s)

module Type = struct
  let none = None

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

  external get: pyobject -> t = "pytype"

  let is_subtype a b =
    bool_of_int (Pywrappers.pytype_issubtype a b)

  let name t =
    match t with
      Unknown -> "Unknown"
    | Bool -> "Bool"
    | Bytes -> "Bytes"
    | Callable -> "Callable"
    | Capsule -> "Capsule"
    | Closure -> "Closure"
    | Dict -> "Dict"
    | Float -> "Float"
    | List -> "List"
    | Long -> "Long"
    | Module -> "Module"
    | None -> "None"
    | Null -> "Null"
    | Tuple -> "Tuple"
    | Type -> "Type"
    | Unicode -> "Unicode"
    | Iter -> "Iter"

  let to_string s =
    match get s with
      Bytes -> Some (pystring_asstringandsize s)
    | Unicode -> Some (pystring_asstringandsize (as_UTF8_string s))
    | _ -> none

  let string_of_repr item =
    match to_string (object_repr item) with
      Some repr -> check_some repr
    | _ (* None *) -> failwith "Py.Object.string_of_repr"

  let mismatch t o =
    failwith
      (Printf.sprintf "Type mismatch: %s expected. Got: %s (%s)"
         t (name (get o)) (string_of_repr o))
end

module Iter = struct
  let check o = Type.get o = Type.Iter

  let next i = option (Pywrappers.pyiter_next i)

  let rec iter f i =
    match next i with
      None -> ()
    | Some item ->
        f item;
        iter f i

  let rec fold_left f v i =
    match next i with
      None -> v
    | Some item -> fold_left f (f v item) i

  let rec fold_right f i v =
    match next i with
      None -> v
    | Some item -> f item (fold_right f i v)

  let to_list i = List.rev (fold_left (fun list item -> item :: list) [] i)

  let to_list_map f i =
    List.rev (fold_left (fun list item -> f item :: list) [] i)

  let rec for_all p i =
    match next i with
      None -> true
    | Some item -> p item && for_all p i

  let rec exists p i =
    match next i with
      None -> false
    | Some item -> p item || exists p i
end

let find_exception option =
  match option with
    None -> raise Not_found
  | Some result -> result

module Mapping = struct
  let check v = Pywrappers.pymapping_check v <> 0

  let get_item_string mapping key =
    option (Pywrappers.pymapping_getitemstring mapping key)

  let find_string mapping key = find_exception (get_item_string mapping key)

  let has_key mapping key = Pywrappers.pymapping_haskey mapping key <> 0

  let has_key_string mapping key =
    Pywrappers.pymapping_haskeystring mapping key <> 0

  let length mapping = check_int (Pywrappers.pymapping_length mapping)

  let set_item_string mapping key value =
    assert_int_success (Pywrappers.pymapping_setitemstring mapping key value)

  let size mapping = check_int (Pywrappers.pymapping_size mapping)
end

module Method = struct
  let create func self cl =
    check_not_null (Pywrappers.pymethod_new func self cl)

  let get_function m = check_not_null (Pywrappers.pymethod_function m)

  let self m = option (Pywrappers.pymethod_self m)
end

module Bool = struct
  let t = pytrue ()

  let f = pyfalse ()

  let check v = v = t || v = f

  let of_bool b = if b then t else f

  let to_bool v =
    if v = t then true
    else if v = f then false
    else Type.mismatch "True or False" v
end

module Float = struct
  let check o = Type.get o = Type.Float

  let of_float = Pywrappers.pyfloat_fromdouble

  let to_float v =
    let result = Pywrappers.pyfloat_asdouble v in
    if result = -1.0 then
      check_error ();
    result
end

module Long = struct
  let check o = Type.get o = Type.Long

  let of_int64 v =
    check_not_null (Pywrappers.pylong_fromlong v)

  let to_int64 v =
    let result = Pywrappers.pylong_aslong v in
    check_error ();
    result

  let of_int v = of_int64 (Int64.of_int v)

  let to_int v = Int64.to_int (to_int64 v)
end

module Number = struct
  let absolute v = check_not_null (Pywrappers.pynumber_absolute v)

  let add v0 v1 = check_not_null (Pywrappers.pynumber_add v0 v1)

  let number_and v0 v1 = check_not_null (Pywrappers.pynumber_and v0 v1)

  let check v = Pywrappers.pynumber_check v <> 0

  let divmod v0 v1 = check_not_null (Pywrappers.pynumber_divmod v0 v1)

  let float v = check_not_null (Pywrappers.pynumber_float v)

  let floor_divide v0 v1 =
    check_not_null (Pywrappers.pynumber_floordivide v0 v1)

  let in_place_add v0 v1 = check_not_null (Pywrappers.pynumber_inplaceadd v0 v1)

  let in_place_and v0 v1 = check_not_null (Pywrappers.pynumber_inplaceand v0 v1)

  let in_place_floor_divide v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacefloordivide v0 v1)

  let in_place_lshift v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacelshift v0 v1)

  let in_place_multiply v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacemultiply v0 v1)

  let in_place_or v0 v1 =
    check_not_null (Pywrappers.pynumber_inplaceor v0 v1)

  let in_place_power v0 v1 v2 =
    check_not_null (Pywrappers.pynumber_inplacepower v0 v1 v2)

  let in_place_remainder v0 v1 =
    check_not_null (Pywrappers.pynumber_inplaceremainder v0 v1)

  let in_place_rshift v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacershift v0 v1)

  let in_place_subtract v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacesubtract v0 v1)

  let in_place_true_divide v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacetruedivide v0 v1)

  let in_place_xor v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacexor v0 v1)

  let invert v =
    check_not_null (Pywrappers.pynumber_invert v)

  let lshift v0 v1 =
    check_not_null (Pywrappers.pynumber_lshift v0 v1)

  let multiply v0 v1 =
    check_not_null (Pywrappers.pynumber_multiply v0 v1)

  let negative v =
    check_not_null (Pywrappers.pynumber_negative v)

  let number_or v0 v1 =
    check_not_null (Pywrappers.pynumber_or v0 v1)

  let positive v =
    check_not_null (Pywrappers.pynumber_positive v)

  let power v0 v1 v2 =
    check_not_null (Pywrappers.pynumber_power v0 v1 v2)

  let remainder v0 v1 =
    check_not_null (Pywrappers.pynumber_remainder v0 v1)

  let rshift v0 v1 =
    check_not_null (Pywrappers.pynumber_rshift v0 v1)

  let subtract v0 v1 =
    check_not_null (Pywrappers.pynumber_subtract v0 v1)

  let true_divide v0 v1 =
    check_not_null (Pywrappers.pynumber_truedivide v0 v1)

  let number_xor v0 v1 =
    check_not_null (Pywrappers.pynumber_xor v0 v1)

  let check v =
    match Type.get v with
      Type.Float
    | Type.Long -> true
    | _ -> false

  let to_float v =
    match Type.get v with
      Type.Float -> Float.to_float v
    | Type.Long -> Int64.to_float (Long.to_int64 v)
    | _ -> Type.mismatch "Long or Float" v
end

type byteorder =
    LittleEndian
  | BigEndian

module String = struct
  let check_bytes s =
    Type.get s = Type.Bytes

  let check_unicode s =
    Type.get s = Type.Unicode

  let check s =
    match Type.get s with
      Type.Bytes | Type.Unicode -> true
    | _ -> false

  let as_UTF8_string = as_UTF8_string

  let decode_UTF8 ?errors ?size s =
    let size' =
      match size with
        None -> String.length s
      | Some size' -> size' in
    let f =
      match ucs () with
        UCS2 -> Pywrappers.UCS2.pyunicodeucs2_decodeutf8
      | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_decodeutf8
      | UCSNone ->
          if !version_major_value >= 3 then
            Pywrappers.Python3.pyunicode_decodeutf8
          else
            failwith "Py.String.decode_UTF8: unavailable" in
    check_not_null (f s size' errors)

  let decode_UTF16_32 decode_ucs2 decode_ucs4 decode_python3 errors size
      byteorder s =
    let size' =
      match size with
        None -> String.length s
      | Some size' -> size' in
    let byteorder' =
      match byteorder with
        None -> 0
      | Some LittleEndian -> -1
      | Some BigEndian -> 1 in
    let byteorder_ref = ref byteorder' in
    let f =
      match ucs () with
        UCS2 -> decode_ucs2
      | UCS4 -> decode_ucs4
      | UCSNone ->
          if !version_major_value >= 3 then
            decode_python3
          else
            failwith "Py.String.decode_UTF16/32: unavailable" in
    let decoded_string = check_not_null (f s size' errors byteorder_ref) in
    let decoded_byteorder =
      match !byteorder_ref with
        -1 -> LittleEndian
      | 1 -> BigEndian
      | _ -> failwith "Py.String.decode_UTF16/32: unknown endianess value" in
    (decoded_string, decoded_byteorder)

  let decode_UTF16 ?errors ?size ?byteorder s =
    decode_UTF16_32 Pywrappers.UCS2.pyunicodeucs2_decodeutf16
      Pywrappers.UCS4.pyunicodeucs4_decodeutf16
      Pywrappers.Python3.pyunicode_decodeutf16 errors size byteorder s

  let decode_UTF32 ?errors ?size ?byteorder s =
    decode_UTF16_32 Pywrappers.UCS2.pyunicodeucs2_decodeutf32
      Pywrappers.UCS4.pyunicodeucs4_decodeutf32
      Pywrappers.Python3.pyunicode_decodeutf32 errors size byteorder s

  let of_unicode ?size int_array =
    let size' =
      match size with
        None -> Array.length int_array
      | Some size' -> size' in
    let f =
      match ucs () with
        UCS2 -> Pywrappers.UCS2.pyunicodeucs2_fromunicode
      | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_fromunicode
      | UCSNone ->
          if !version_major_value >= 3 then
            Pywrappers.Python3.pyunicode_fromkindanddata 4
          else
            failwith "Py.String.of_unicode: unavailable" in
    check_not_null (f int_array size')

  let to_unicode s =
    let f =
      match ucs () with
        UCS2 -> Pywrappers.UCS2.pyunicodeucs2_asunicode
      | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_asunicode
      | UCSNone ->
          if !version_major_value >= 3 then
            Pywrappers.Python3.pyunicode_asucs4copy
          else
            failwith "Py.String.to_unicode: unavailable" in
    check_some (f s)

  let string_type_mismatch obj = Type.mismatch "String or Unicode" obj

  let format fmt args =
    match Type.get fmt with
      Type.Unicode ->
        let f =
          match ucs () with
            UCS2 -> Pywrappers.UCS2.pyunicodeucs2_format
          | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_format
          | UCSNone ->
              if !version_major_value >= 3 then
                Pywrappers.Python3.pyunicode_format
              else
                failwith "Py.String.format: unavailable" in
        check_not_null (f fmt args)
    | Type.Bytes ->
        if !version_major_value >= 3 then
          failwith "No format on Bytes in Python 3"
        else
          check_not_null (Pywrappers.Python2.pystring_format fmt args)
    | _ -> string_type_mismatch fmt

  let length s =
    match Type.get s with
      Type.Unicode ->
        let f =
          match ucs () with
            UCS2 -> Pywrappers.UCS2.pyunicodeucs2_getsize
          | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_getsize
          | UCSNone ->
              if !version_major_value >= 3 then
                Pywrappers.Python3.pyunicode_getlength
              else
                failwith "Py.String.length: unavailable" in
        f s
    | Type.Bytes ->
        if !version_major_value >= 3 then
          Pywrappers.Python3.pybytes_size s
        else
          Pywrappers.Python2.pystring_size s
    | _ -> string_type_mismatch s

  let of_string s =
    let len = String.length s in
    if !version_major_value >= 3 then
      check_not_null (Pywrappers.Python3.pyunicode_fromstringandsize s len)
    else
      check_not_null (Pywrappers.Python2.pystring_fromstringandsize s len)

  let to_string s =
    match Type.to_string s with
      None -> string_type_mismatch s
    | Some s -> check_some s
end

module Err = struct
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

  let clear () =
    Pywrappers.pyerr_clear ();
    fetched_exception := None

  let exception_matches exc = Pywrappers.pyerr_exceptionmatches exc <> 0

  let fetch () =
    let (ptype, pvalue, ptraceback) = pyerr_fetch_internal () in
    if ptype = null then
      None
    else
      Some (ptype, pvalue, ptraceback)

  let fetched () = !fetched_exception

  let given_exception_matches given exc =
    Pywrappers.pyerr_givenexceptionmatches given exc <> 0

  let occurred () = option (Pywrappers.pyerr_occurred ())

  let print () =
    Pywrappers.pyerr_print ()

  let print_ex i =
    Pywrappers.pyerr_printex i

  let restore = Pywrappers.pyerr_restore

  let set_none = Pywrappers.pyerr_setnone

  let set_string = Pywrappers.pyerr_setstring

  let set_object = Pywrappers.pyerr_setobject

  let set_error error msg =
    let exc =
      match error with
        Exception -> Pywrappers.pyexc_exception ()
      | StandardError ->
          if !version_major_value <= 2 then
            Pywrappers.Python2.pyexc_standarderror ()
          else
            Pywrappers.pyexc_exception ()
      | ArithmeticError -> Pywrappers.pyexc_arithmeticerror ()
      | LookupError -> Pywrappers.pyexc_lookuperror ()
      | AssertionError -> Pywrappers.pyexc_assertionerror ()
      | AttributeError -> Pywrappers.pyexc_attributeerror ()
      | EOFError -> Pywrappers.pyexc_eoferror ()
      | EnvironmentError -> Pywrappers.pyexc_environmenterror ()
      | FloatingPointError -> Pywrappers.pyexc_floatingpointerror ()
      | IOError -> Pywrappers.pyexc_ioerror ()
      | ImportError -> Pywrappers.pyexc_importerror ()
      | IndexError -> Pywrappers.pyexc_indexerror ()
      | KeyError -> Pywrappers.pyexc_keyerror ()
      | KeyboardInterrupt -> Pywrappers.pyexc_keyboardinterrupt ()
      | MemoryError -> Pywrappers.pyexc_memoryerror ()
      | NameError -> Pywrappers.pyexc_nameerror ()
      | NotImplementedError -> Pywrappers.pyexc_notimplementederror ()
      | OSError -> Pywrappers.pyexc_oserror ()
      | OverflowError -> Pywrappers.pyexc_overflowerror ()
      | ReferenceError -> Pywrappers.pyexc_referenceerror ()
      | RuntimeError -> Pywrappers.pyexc_runtimeerror ()
      | SyntaxError -> Pywrappers.pyexc_syntaxerror ()
      | SystemExit -> Pywrappers.pyexc_systemerror ()
      | TypeError -> Pywrappers.pyexc_typeerror ()
      | ValueError -> Pywrappers.pyexc_valueerror ()
      | ZeroDivisionError -> Pywrappers.pyexc_zerodivisionerror () in
    set_object exc (String.of_string msg)
end

exception Err of Err.t * string

module Object = struct
  type t = Pytypes.pyobject

  let del_item obj item =
    assert_int_success (Pywrappers.pyobject_delitem obj item)

  let del_item_string obj item =
    assert_int_success (Pywrappers.pyobject_delitemstring obj item)

  let get_attr obj attr =
    check_not_null (Pywrappers.pyobject_getattr obj attr)

  let get_attr_string obj attr =
    check_not_null (Pywrappers.pyobject_getattrstring obj attr)

  let get_item obj key =
    option (Pywrappers.pyobject_getitem obj key)

  let find obj attr = find_exception (get_item obj attr)

  let get_item_string obj key = get_item obj (String.of_string key)

  let find_string obj attr = find_exception (get_item_string obj attr)

  let get_iter obj =
    check_not_null (Pywrappers.pyobject_getiter obj)

  let get_type obj =
    check_not_null (Pywrappers.pyobject_type obj)

  let has_attr obj attr =
    bool_of_int (Pywrappers.pyobject_hasattr obj attr)

  let has_attr_string obj attr =
    bool_of_int (Pywrappers.pyobject_hasattrstring obj attr)

  let hash obj = check_int64 (Pywrappers.pyobject_hash obj)

  let is_true obj = bool_of_int (Pywrappers.pyobject_istrue obj)

  let not obj = bool_of_int (Pywrappers.pyobject_istrue obj)

  let print obj out_channel =
    assert_int_success
      (Pywrappers.pyobject_print obj
         (fd_of_descr (Unix.descr_of_out_channel out_channel)) 1)

  let repr = object_repr

  let rich_compare a b cmp =
    check_not_null (Pywrappers.pyobject_richcompare a b cmp)

  let rich_compare_bool a b cmp =
    bool_of_int (Pywrappers.pyobject_richcomparebool a b cmp)

  let set_attr obj attr value =
    assert_int_success (Pywrappers.pyobject_setattr obj attr value)

  let set_attr_string obj attr value =
    assert_int_success (Pywrappers.pyobject_setattrstring obj attr value)

  let del_attr obj attr = set_attr obj attr null

  let del_attr_string obj attr = set_attr_string obj attr null

  let set_item obj key value =
    assert_int_success (Pywrappers.pyobject_setitem obj key value)

  let set_item_string obj key value = set_item obj (String.of_string key) value

  let str obj = check_not_null (Pywrappers.pyobject_str obj)

  let string_of_repr = Type.string_of_repr

  let to_string item = String.to_string (str item)

  let as_char_buffer obj = check_some (pyobject_ascharbuffer obj)

  let as_read_buffer obj = check_some (pyobject_asreadbuffer obj)

  let as_write_buffer obj = check_some (pyobject_aswritebuffer obj)

  external reference_count: pyobject -> int = "pyrefcount"
end

module Sequence = struct
  let check obj = bool_of_int (Pywrappers.pysequence_check obj)

  let concat s s' = check_not_null (Pywrappers.pysequence_concat s s')

  let contains s value = bool_of_int (Pywrappers.pysequence_contains s value)

  let count s value = check_int (Pywrappers.pysequence_count s value)

  let del_item s index =
    assert_int_success (Pywrappers.pysequence_delitem s index)

  let fast s msg = check_not_null (Pywrappers.pysequence_fast s msg)

  let get_item sequence index = Pywrappers.pysequence_getitem sequence index

  let get = get_item

  let get_slice s i0 i1 =
    check_not_null (Pywrappers.pysequence_getslice s i0 i1)

  let index s value = check_int (Pywrappers.pysequence_index s value)

  let in_place_concat s s' =
    check_not_null (Pywrappers.pysequence_inplaceconcat s s')

  let in_place_repeat s count =
    check_not_null (Pywrappers.pysequence_inplacerepeat s count)

  let length s = check_int (Pywrappers.pysequence_length s)

  let list sequence = check_not_null (Pywrappers.pysequence_list sequence)

  let repeat s count = check_not_null (Pywrappers.pysequence_repeat s count)

  let set_item s index value =
    assert_int_success (Pywrappers.pysequence_setitem s index value)

  let set = set_item

  let set_slice s i0 i1 value =
    assert_int_success (Pywrappers.pysequence_setslice s i0 i1 value)

  let size s = check_int (Pywrappers.pysequence_size s)

  let tuple sequence = check_not_null (Pywrappers.pysequence_tuple sequence)

  let to_array sequence = Array.init (size sequence) (get_item sequence)

  let to_array_map f sequence =
    Array.init (size sequence) (fun index -> f (get_item sequence index))

  let rec fold_right_upto f upto sequence v =
    if upto > 0 then
      let i = pred upto in
      fold_right_upto f i sequence (f (get_item sequence i) v)
    else
      v

  let fold_right f sequence v =
    fold_right_upto f (length sequence) sequence v

  let to_list sequence =
    fold_right (fun item list -> item :: list) sequence []

  let to_list_map f sequence =
    fold_right (fun item list -> f item :: list) sequence []

  let fold_left f v sequence =
    Iter.fold_left f v (Object.get_iter sequence)

  let for_all p sequence =
    Iter.for_all p (Object.get_iter sequence)

  let exists p sequence =
    Iter.exists p (Object.get_iter sequence)
end

module Tuple = struct
  include Sequence

  let check o = Type.get o = Type.Tuple

  let create size =
    check_not_null (Pywrappers.pytuple_new size)

  let empty = pytuple_empty ()

  let get_slice tuple i0 i1 =
    check_not_null (Pywrappers.pytuple_getslice tuple i0 i1)

  let size tuple = check_int (Pywrappers.pytuple_size tuple)

  let set_item s index value =
    assert_int_success (Pywrappers.pytuple_setitem s index value)

  let set = set_item

  let init size f =
    let result = create size in
    for index = 0 to size - 1 do
      set_item result index (f index)
    done;
    result

  let of_array array = init (Array.length array) (Array.get array)

  let of_array_map f array =
    init (Array.length array) (fun i -> f (Array.get array i))

  let of_list list = of_array (Array.of_list list)

  let of_list_map f list = of_array_map f (Array.of_list list)

  let of_sequence = Sequence.tuple

  let of_tuple1 v0 = init 1 (function _ -> v0)

  let of_tuple2 (v0, v1) = init 2 (function 0 -> v0 | _ -> v1)

  let of_tuple3 (v0, v1, v2) = init 3 (function 0 -> v0 | 1 -> v1 | _ -> v2)

  let of_tuple4 (v0, v1, v2, v3) =
    init 4 (function 0 -> v0 | 1 -> v1 | 2 -> v2 | _ -> v3)

  let of_tuple5 (v0, v1, v2, v3, v4) =
    init 5 (function 0 -> v0 | 1 -> v1 | 2 -> v2 | 3 -> v3 | _ -> v4)

  let to_tuple1 v = get_item v 0

  let to_tuple2 v = (get_item v 0, get_item v 1)

  let to_tuple3 v = (get_item v 0, get_item v 1, get_item v 2)

  let to_tuple4 v = (get_item v 0, get_item v 1, get_item v 2, get_item v 3)

  let to_tuple5 v =
    (get_item v 0, get_item v 1, get_item v 2, get_item v 3, get_item v 4)

  let singleton = of_tuple1

  let to_singleton = to_tuple1

  let of_pair = of_tuple2

  let to_pair = to_tuple2
end

module Callable = struct
  let check v = Pywrappers.pycallable_check v <> 0

  let of_function ?(docstring = "Anonymous closure") f =
    let f' parameter =
      try f parameter with
        E (errtype, errvalue) ->
         Err.set_object errtype errvalue;
         null
      | Err (errtype, msg) ->
         Err.set_error errtype msg;
         null in
    pywrap_closure docstring f'

  let of_function_array ?docstring f =
    of_function ?docstring (fun args -> f (Tuple.to_array args))

  let to_function c =
    if not (check c) then
      Type.mismatch "Callable" c;
    function args ->
      Eval.call_object c args

  let to_function_array c =
    let f = to_function c in
    (fun args -> f (Tuple.of_array args))
end

module Dict = struct
  let check o = Type.get o = Type.Dict

  let clear = Pywrappers.pydict_clear

  let copy v = check_not_null (Pywrappers.pydict_copy v)

  let create () =
    check_not_null (Pywrappers.pydict_new ())

  let del_item dict item =
    assert_int_success (Pywrappers.pydict_delitem dict item)

  let del_item_string dict name =
    assert_int_success (Pywrappers.pydict_delitemstring dict name)

  let get_item dict key =
    option (Pywrappers.pydict_getitem dict key)

  let find dict key = find_exception (get_item dict key)

  let get_item_string dict name =
    option (Pywrappers.pydict_getitemstring dict name)

  let find_string dict key = find_exception (get_item_string dict key)

  let keys dict = check_not_null (Pywrappers.pydict_keys dict)

  let items dict = check_not_null (Pywrappers.pydict_items dict)

  let set_item dict key value =
    assert_int_success (Pywrappers.pydict_setitem dict key value)

  let set_item_string dict name value =
    assert_int_success (Pywrappers.pydict_setitemstring dict name value)

  let size dict =
    let sz = Pywrappers.pydict_size dict in
    assert_int_success sz;
    sz

  let values dict =
    check_not_null (Pywrappers.pydict_values dict)

  let iter f dict =
    Iter.iter begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      f key value
    end (Object.get_iter (items dict))

  let fold f dict v =
    Iter.fold_left begin fun v pair ->
      let (key, value) = Tuple.to_pair pair in
      f key value v
    end v (Object.get_iter (items dict))

  let for_all p dict =
    Iter.for_all begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      p key value
    end (Object.get_iter (items dict))

  let exists p dict =
    Iter.exists begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      p key value
    end (Object.get_iter (items dict))

  let bindings dict =
    Iter.to_list_map Tuple.to_pair (Object.get_iter (items dict))

  let singleton key value =
    let result = create () in
    set_item result key value;
    result

  let singleton_string key value =
    let result = create () in
    set_item_string result key value;
    result
end

module Module = struct
  let check o = Type.get o = Type.Module

  let create name =
    check_not_null (Pywrappers.pymodule_new name)

  let get_dict m =
    check_not_null (Pywrappers.pymodule_getdict m)

  let get_filename m =
    check_some (Pywrappers.pymodule_getfilename m)

  let get_name m =
    check_some (Pywrappers.pymodule_getname m)

  let get = Object.get_attr_string

  let set = Object.set_attr_string

  let remove = Object.del_attr_string

  let main () = Import.add_module "__main__"

  let sys () = Import.import_module "sys"

  let builtins () = get (main ()) "__builtins__"
end

module Utils = struct
  let try_finally f arg finally finally_arg =
    try
      let result = f arg in
      finally finally_arg;
      result
    with e ->
      finally finally_arg;
      raise e

  let read_and_close channel f arg =
    try
      let result = f arg in
      close_in channel;
      result
    with e ->
      close_in_noerr channel;
      raise e

  let write_and_close channel f arg =
    try
      let result = f arg in
      close_out channel;
      result
    with e ->
      close_out_noerr channel;
      raise e
end

module Run = struct
  let any_file file filename =
    assert_int_success
      (Pywrappers.pyrun_anyfileexflags
         (fd_of_descr (Unix.descr_of_in_channel file)) filename 0 None)

  let file file filename start globals locals =
    let fd = fd_of_descr (Unix.descr_of_in_channel file) in
    check_not_null
      (Pywrappers.pyrun_fileexflags fd filename start globals locals 0 None)

  let interactive_one channel name =
    let fd = fd_of_descr (Unix.descr_of_in_channel channel) in
    assert_int_success (Pywrappers.pyrun_interactiveoneflags fd name None)

  let interactive_loop channel name =
    let fd = fd_of_descr (Unix.descr_of_in_channel channel) in
    assert_int_success (Pywrappers.pyrun_interactiveloopflags fd name None)

  let simple_file channel name =
    let fd = fd_of_descr (Unix.descr_of_in_channel channel) in
    assert_int_success (Pywrappers.pyrun_simplefileexflags fd name 0 None)

  let simple_string string =
    Pywrappers.pyrun_simplestringflags string None = 0

  let string s start globals locals =
    check_not_null
      (Pywrappers.pyrun_stringflags s start globals locals None)

  let eval ?(start = Eval) ?(globals = Module.get_dict (Module.main ()))
      ?(locals = Dict.create ()) s =
    string s start globals locals

  let load ?(start = File) ?(globals = Module.get_dict (Module.main ()))
      ?(locals = Dict.create ()) chan filename =
    file chan filename start globals locals

  let interactive () = interactive_loop stdin "<stdin>"

  let ipython () =
    ignore
      (eval ~start:File "
try:
  from IPython import embed
  embed()
except ImportError:
  from IPython.Shell import IPShellEmbed
  ipshell = IPShellEmbed(argv=[''])
  ipshell()
")
end

module Class = struct
  let init ?(parents = Tuple.empty) ?(fields = []) ?(methods = []) classname =
    let dict = Dict.create () in
    let add_field (name, value) = Dict.set_item_string dict name value in
    List.iter add_field fields;
    if version_major () >= 3 then
      let add_method (name, closure) =
        let closure' = check_not_null (Callable.of_function closure) in
        let m =
          check_not_null (Pywrappers.Python3.pyinstancemethod_new closure') in
        Dict.set_item_string dict name m in
      List.iter add_method methods;
      let ty = Pywrappers.pytype_type () in
      check_not_null
        (pyobject_callfunctionobjargs ty [| classname; parents; dict |])
    else
      let c =
        check_not_null
          (Pywrappers.Python2.pyclass_new parents dict classname) in
      let add_method (name, closure) =
        let closure' = check_not_null (Callable.of_function closure) in
        let m = check_not_null (Pywrappers.pymethod_new closure' null c) in
        Dict.set_item_string dict name m in
      List.iter add_method methods;
      c
end

module List = struct
  include Sequence

  let check v = Type.get v = Type.List

  let create size = check_not_null (Pywrappers.pylist_new size)

  let size list = check_int (Pywrappers.pylist_size list)

  let init size f =
    let result = create size in
    for index = 0 to size - 1 do
      set_item result index (f index)
    done;
    result

  let of_array array = init (Array.length array) (Array.get array)

  let of_array_map f array =
    init (Array.length array) (fun i -> f (Array.get array i))

  let of_list list = of_array (Array.of_list list)

  let of_list_map f list = of_array_map f (Array.of_list list)

  let of_sequence = Sequence.list

  let singleton v = init 1 (fun _ -> v)
end

let set_argv argv =
  Module.set (Module.sys ()) "argv" (List.of_array_map String.of_string argv)

let last_value () = Module.get (Module.builtins ()) "_"
