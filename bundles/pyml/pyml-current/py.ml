open Stdcompat

type pyobject = Pytypes.pyobject

type input = Pytypes.input = Single | File | Eval

type 'a file = 'a Pytypes.file = Filename of string | Channel of 'a

type compare = Pytypes.compare = LT | LE | EQ | NE | GT | GE

type ucs = UCSNone | UCS2 | UCS4

type closure =
    WithoutKeywords of (pyobject -> pyobject)
  | WithKeywords of (pyobject -> pyobject -> pyobject)

external load_library: string option -> bool option -> unit = "py_load_library"
external is_debug_build: unit -> bool = "py_is_debug_build"
external unsetenv: string -> unit = "py_unsetenv"
external finalize_library: unit -> unit = "py_finalize_library"
external pywrap_closure: string -> closure -> pyobject
    = "pyml_wrap_closure"
external pynull: unit -> pyobject = "PyNull_wrapper"
external pynone: unit -> pyobject = "PyNone_wrapper"
external pytrue: unit -> pyobject = "PyTrue_wrapper"
external pyfalse: unit -> pyobject = "PyFalse_wrapper"
external pytuple_empty: unit -> pyobject = "PyTuple_Empty_wrapper"
external pyobject_callfunctionobjargs: pyobject -> pyobject array -> pyobject
    = "PyObject_CallFunctionObjArgs_wrapper"
external pyobject_callmethodobjargs: pyobject -> pyobject -> pyobject array
  -> pyobject = "PyObject_CallMethodObjArgs_wrapper"
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
external pylong_fromstring: string -> int -> pyobject * int
    = "PyLong_FromString_wrapper"
external pycapsule_isvalid: Pytypes.pyobject -> string -> int
      = "Python27_PyCapsule_IsValid_wrapper"

external ucs: unit -> ucs = "py_get_UCS"

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

let pythonpaths = ref []

let set_python_home s =
  python_home := (Some s);
  if !initialized then
    if !version_major_value <= 2 then
      Pywrappers.Python2.py_setpythonhome s
    else
      Pywrappers.Python3.py_setpythonhome s

let add_python_path path =
  pythonpaths := path :: !pythonpaths

let extract_version version_line =
  let before =
    try String.index version_line ' '
    with Not_found ->
      let msg =
        Printf.sprintf "Py.extract_version: cannot parse the version line '%s'"
          version_line in
      failwith msg in
  Pyutils.split_left_on_char ~from:(succ before) ' ' version_line

let extract_version_major_minor version =
  try
    if String.length version >= 3 && (version.[1] = '.') then
      let major = int_of_string (String.sub version 0 1) in
      let minor = int_of_string (String.sub version 2 1) in
      (major, minor)
    else
      raise Exit
  with Exit | Failure _ ->
    let msg =
      Printf.sprintf
        "Py.extract_version_major_minor: unable to parse the version number '%s'"
        version in
    failwith msg

let run_command ?(input = "") command read_stderr =
  let (input_channel, output, error) =
    Unix.open_process_full command (Unix.environment ()) in
  let result =
    try
      output_string output input;
      close_out output;
      Pyutils.input_lines (if read_stderr then error else input_channel)
    with _ ->
      begin
        try
          ignore (Unix.close_process_full (input_channel, output, error))
        with _ ->
          ()
      end;
      let msg =
        Printf.sprintf "Py.run_command: unable to read the result of '%s'"
          command in
      failwith msg in
  if Unix.close_process_full
       (input_channel, output, error) <> Unix.WEXITED 0 then
    begin
      let msg = Printf.sprintf "Py.run_command: unable to run '%s'" command in
      failwith msg;
    end;
  result

let run_command_opt ?input command read_stderr =
  try Some (run_command ?input command read_stderr)
  with Failure _ -> None

let parent_dir filename =
  let dirname = Filename.dirname filename in
  Filename.concat dirname Filename.parent_dir_name

let has_putenv = ref false

let has_set_pythonpath = ref None

let init_pythonhome verbose pythonhome =
  pythonhome <> "" &&
    try
      ignore (Sys.getenv "PYTHONHOME");
      false
    with Not_found ->
      if verbose then
        begin
          Printf.eprintf "Temporary set PYTHONHOME=\"%s\".\n" pythonhome;
          flush stderr;
        end;
      Unix.putenv "PYTHONHOME" pythonhome;
      has_putenv := true;
      true

let uninit_pythonhome () =
  if !has_putenv then
    begin
      unsetenv "PYTHONHOME";
      has_putenv := false
    end

let uninit_pythonpath () =
  match !has_set_pythonpath with
    None -> ()
  | Some old_pythonpath ->
      begin
        has_set_pythonpath := None;
        match old_pythonpath with
          None -> unsetenv "PYTHONPATH"
        | Some old_pythonpath' -> Unix.putenv "PYTHONPATH" old_pythonpath'
      end

let ldd executable =
  let command = Printf.sprintf "ldd %s" executable in
  match run_command_opt command false with
    None -> []
  | Some lines ->
     let extract_line line =
       String.trim
         (Pyutils.split_left_on_char '('
            (Pyutils.split_right_on_char '>' line)) in
     List.map extract_line lines

let ldconfig () =
  match run_command_opt "ldconfig -p" false with
    None -> []
  | Some lines ->
     let extract_line line =
       String.trim (Pyutils.split_right_on_char '>' line) in
     List.map extract_line lines

let libpython_from_interpreter python_full_path =
  let lines = ldd python_full_path in
  let is_libpython line =
    let basename = Filename.basename line in
    Pyutils.has_prefix "libpython" basename in
  List.find_opt is_libpython lines

let libpython_from_ldconfig major minor =
  let lines = ldconfig () in
  let prefix =
    match major, minor with
      None, _ -> "libpython"
    | Some major', None -> Printf.sprintf "libpython%d" major'
    | Some major', Some minor' -> Printf.sprintf "libpython%d.%d" major' minor' in
  let is_libpython line =
    let basename = Filename.basename line in
    Pyutils.has_prefix prefix basename in
  List.find_opt is_libpython lines

let parse_python_list list =
  let length = String.length list in
  let buffer = Buffer.create 17 in
  let rec parse_item accu index =
    if index < length then
      match list.[index] with
        '\'' ->
        begin
          let item = Buffer.contents buffer in
          let accu = item :: accu in
          if index + 1 < length then
            match list.[index + 1] with
              ']' ->
              if index + 2 = length then
                Some (List.rev accu)
              else
                None
            | ',' ->
               if list.[index + 2] = ' ' && list.[index + 3] = '\'' then
                 begin
                   Buffer.clear buffer;
                   parse_item accu (index + 4)
                 end
               else
                 None
            | _ ->
               None
          else
            None
        end
      | '\\' ->
         if index + 1 < length then
           begin
             match list.[index + 1] with
               '\n' -> parse_item accu (index + 2)
             | '0' .. '9' ->
                if index + 3 < length then
                  begin
                    let octal_number = String.sub list (index + 1) 3 in
                    let c = char_of_int (Pyutils.int_of_octal octal_number) in
                    Buffer.add_char buffer c;
                    parse_item accu (index + 4)
                  end
                else
                  None
             | 'x' ->
                if index + 2 < length then
                  begin
                    let hexa_number = String.sub list (index + 1) 2 in
                    let c = char_of_int (Pyutils.int_of_hex hexa_number) in
                    Buffer.add_char buffer c;
                    parse_item accu (index + 3)
                  end
                else
                  None
             | c ->
                begin
                  match
                    try
                      let c' =
                        match c with
                          '\\' -> '\\'
                        | '\'' -> '\''
                        | '"' -> '"'
                        | 'a' -> '\007'
                        | 'b' -> '\b'
                        | 'f' -> '\012'
                        | 'n' -> '\n'
                        | 'r' -> '\r'
                        | 't' -> '\t'
                        | 'v' -> '\011'
                        | _ -> raise Not_found in
                      Some c'
                    with Not_found -> None
                  with
                    None -> None
                  | Some c' ->
                     Buffer.add_char buffer c';
                    parse_item accu (index + 2)
                end
           end
         else
           None
      | c ->
         Buffer.add_char buffer c;
         parse_item accu (index + 1)
    else
      None in
  if length >= 2 && list.[0] == '[' then
    match list.[1] with
      '\'' ->
        Buffer.clear buffer;
        parse_item [] 2
    | ']' when length = 2 -> Some []
    | _ -> None
  else
    None

let pythonpaths_from_interpreter python_full_path =
  let command = "\
import sys
print(sys.path)
" in
  match
    try run_command ~input:command python_full_path false
    with Failure _ -> []
  with
    [path_line] ->
      begin
        match parse_python_list path_line with
          None -> []
        | Some paths -> paths
      end
  | _ -> []

let concat_library_filenames library_paths library_filenames =
  let expand_filepaths filename =
    filename ::
    List.map (fun path -> Filename.concat path filename) library_paths in
  List.concat (List.map expand_filepaths library_filenames)

let libpython_from_pkg_config version_major version_minor =
  let command =
    Printf.sprintf "pkg-config --libs python-%d.%d" version_major
      version_minor in
  match run_command_opt command false with
    Some (words :: _) ->
      let word_list = String.split_on_char ' ' words in
      let unable_to_parse () =
        let msg = Printf.sprintf
        "Py.find_library_path: unable to parse the output of pkg-config '%s'"
            words in
        failwith msg in
      let parse_word (library_paths, library_filename) word =
        if String.length word > 2 then
          match String.sub word 0 2 with
            "-L" ->
              let word' = Pyutils.substring_between word 2 (String.length word) in
              (word' :: library_paths, library_filename)
          | "-l" ->
              let word' = Pyutils.substring_between word 2 (String.length word) in
              if library_filename <> None then
                unable_to_parse ();
              let library_filename =
                Printf.sprintf "lib%s%s" word' Pyml_arch.library_suffix in
              (library_paths, Some library_filename)
          | _ -> (library_paths, library_filename)
        else (library_paths, library_filename) in
      let (library_paths, library_filename) =
        List.fold_left parse_word ([], None) word_list in
      let library_filename =
        match library_filename with
          None -> unable_to_parse ()
        | Some library_filename -> library_filename in
      Some (concat_library_filenames library_paths [library_filename])
  | _ -> None

let libpython_from_pythonhome version_major version_minor python_full_path =
  let library_paths =
    match
      try Some (Sys.getenv "PYTHONHOME")
      with Not_found ->
        match python_full_path with
          None -> None
        | Some python_full_path -> Some (parent_dir python_full_path)
    with
      None -> failwith "Unable to find libpython!"
    | Some pythonhome ->
        let prefix = Pyutils.split_left_on_char ':' pythonhome in
        [Filename.concat prefix "lib"] in
  let library_filenames =
    List.map
      (fun format -> Printf.sprintf format version_major version_minor)
      Pyml_arch.library_patterns in
  concat_library_filenames library_paths library_filenames

let find_library_path version_major version_minor python_full_path =
  match Option.bind python_full_path libpython_from_interpreter with
    Some path -> [path]
  | None ->
      match libpython_from_ldconfig version_major version_minor with
        Some path -> [path]
      | None ->
          match version_major, version_minor with
            Some version_major, Some version_minor ->
              begin
                match libpython_from_pkg_config version_major version_minor with
                  Some paths -> paths
                | None ->
                    libpython_from_pythonhome version_major version_minor
                      python_full_path
              end
          | _ -> failwith "Cannot infer Python version"

let python_version_from_interpreter interpreter =
  let version_line =
    let python_version_cmd = Printf.sprintf "\"%s\" --version" interpreter in
    try List.hd (run_command python_version_cmd false)
    with Failure _ -> List.hd (run_command python_version_cmd true) in
  extract_version version_line

let library_filename = ref None

let load_library filename =
  library_filename := filename;
  load_library filename

let get_library_filename () = !library_filename

let find_library ~verbose ~version_major ~version_minor ~debug_build
    python_full_path =
  try
    load_library None debug_build
  with Failure _ ->
    let library_filenames =
      find_library_path version_major version_minor python_full_path in
    let errors = Buffer.create 17 in
    let rec try_load_library library_filenames =
      match library_filenames with
        [] ->
          let msg =
            Printf.sprintf
              "Py.find_library: unable to find the Python library%s"
              (Buffer.contents errors) in
          failwith msg
      | filename :: others ->
          begin
            let pythonhome_set =
              not (Filename.is_implicit filename) &&
                init_pythonhome verbose (parent_dir filename) in
            try
              if verbose then
                begin
                  Printf.eprintf "Trying to load \"%s\".\n" filename;
                  flush stderr;
                end;
              load_library (Some filename) debug_build;
            with Failure msg ->
              if pythonhome_set then
                uninit_pythonhome ();
              if verbose then
                begin
                  Printf.eprintf "Failed: \"%s\".\n" msg;
                  flush stderr;
                end;
              Printf.bprintf errors " [%s returned %s]" filename msg;
              try_load_library others
          end in
    try_load_library library_filenames

let initialize_library ~verbose ~version_major ~version_minor
    ~debug_build python_full_path =
  begin
    match !python_home with
      None -> ()
    | Some s -> ignore (init_pythonhome verbose s)
  end;
  find_library ~verbose ~version_major ~version_minor ~debug_build
    python_full_path;
  begin
    match python_full_path with
      None -> ()
    | Some python_full_path' ->
        let pythonhome =
          let dirname = Filename.dirname python_full_path' in
          if Filename.basename dirname = "bin" then
            Filename.concat dirname Filename.parent_dir_name
          else
            dirname in
        ignore (init_pythonhome verbose pythonhome);
  end;
  set_program_name !program_name;
  begin
    match !python_home with
      None -> ()
    | Some s -> set_python_home s
  end

let get_version = Pywrappers.py_getversion

let which program =
  let exe = Pyml_arch.ensure_executable_suffix program in
  let command = Printf.sprintf "%s \"%s\"" Pyml_arch.which exe in
  match run_command_opt command false with
    Some (path :: _) -> Some path
  | _ -> None

let find_interpreter interpreter version minor =
  match interpreter with
    Some interpreter' ->
      if String.contains interpreter' '/' then
        Some interpreter'
      else
        which interpreter'
  | None ->
      match
        Option.bind version
           (fun version' ->
             match
               Option.bind minor
                 (fun minor' ->
                   which (Printf.sprintf "python%d.%d" version' minor'))
             with
             | Some result -> Some result
             | None -> which (Printf.sprintf "python%d" version'))
      with
      | Some result -> Some result
      | None -> which "python"

let version_mismatch interpreter found expected =
  Printf.sprintf
    "Version mismatch: %s is version %s but version %s is expected"
    interpreter found expected

let build_version_string major minor =
  Printf.sprintf "%d.%d" major minor

let initialize ?library_name ?interpreter ?version ?minor ?(verbose = false)
    ?debug_build () =
  if !initialized then
    failwith "Py.initialize: already initialized";
  begin
    match library_name with
    | Some library_name ->
        load_library (Some library_name) debug_build;
    | None ->
  begin
    try
      let python_full_path = find_interpreter interpreter version minor in
      let interpreter_pythonpaths =
        match python_full_path with
          None -> []
        | Some python_full_path' ->
            pythonpaths_from_interpreter python_full_path' in
      let new_pythonpaths =
        List.rev_append !pythonpaths interpreter_pythonpaths in
      if new_pythonpaths <> [] then
        begin
          let former_pythonpath = Sys.getenv_opt "PYTHONPATH" in
          has_set_pythonpath := Some former_pythonpath;
          let all_paths =
            match former_pythonpath with
              None -> new_pythonpaths
            | Some former_pythonpath' ->
                former_pythonpath' :: new_pythonpaths in
          let pythonpath = String.concat Pyml_arch.path_separator all_paths in
          if verbose then
            begin
              Printf.eprintf "Temporary set PYTHONPATH=\"%s\".\n" pythonpath;
              flush stderr;
            end;
          Unix.putenv "PYTHONPATH" pythonpath
      end;
      let (version_major, version_minor) =
        match python_full_path with
          Some python_full_path' ->
            let version_string =
              python_version_from_interpreter python_full_path' in
            let (version_major, version_minor) =
              extract_version_major_minor version_string in
            begin
              match version with
                None -> ()
              | Some version_major' ->
                  if version_major <> version_major' then
                    failwith
                      (version_mismatch
                         python_full_path' (string_of_int version_major)
                         (string_of_int version_major'));
                  match minor with
                    None -> ()
                  | Some version_minor' ->
                      if version_minor <> version_minor' then
                        let expected =
                          build_version_string version_major version_minor in
                        let got =
                          build_version_string version_major' version_minor' in
                        failwith
                          (version_mismatch python_full_path' expected got);
            end;
            (Some version_major, Some version_minor)
        | _ -> version, minor in
      initialize_library ~verbose ~version_major ~version_minor ~debug_build
        python_full_path;
    with e ->
      uninit_pythonhome ();
      uninit_pythonpath ();
      raise e
  end;
  end;
  let version = get_version () in
  let (version_major, version_minor) =
    extract_version_major_minor version in
  version_value := version;
  version_major_value := version_major;
  version_minor_value := version_minor;
  initialized := true

let on_finalize_list = ref []

let on_finalize f = on_finalize_list := f :: !on_finalize_list

let finalize () =
  assert_initialized ();
  List.iter (fun f -> f ()) !on_finalize_list;
  finalize_library ();
  uninit_pythonhome ();
  uninit_pythonpath ();
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
  let is_valid v name  = pycapsule_isvalid v name <> 0

  let check v = is_valid v "ocaml-capsule"

  let table = Hashtbl.create 17

  let () = on_finalize (fun () -> Hashtbl.clear table)

  external unsafe_wrap_value: 'a -> pyobject = "pyml_wrap_value"

  external unsafe_unwrap_value: pyobject -> 'a = "pyml_unwrap_value"

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

let object_repr obj = check_not_null (Pywrappers.pyobject_repr obj)

module String_ = struct
  let as_UTF8_string s =
    let f =
      match ucs () with
        UCS2 -> Pywrappers.UCS2.pyunicodeucs2_asutf8string
      | UCS4 -> Pywrappers.UCS4.pyunicodeucs4_asutf8string
      | UCSNone ->
          if !version_major_value >= 3 then
            Pywrappers.Python3.pyunicode_asutf8string
          else
            failwith "String.as_UTF8_string: unavailable" in
    check_not_null (f s)

  let of_string s =
    let len = String.length s in
    if !version_major_value >= 3 then
      check_not_null (Pywrappers.Python3.pyunicode_fromstringandsize s len)
    else
      check_not_null (Pywrappers.Python2.pystring_fromstringandsize s len)

  let of_bytes s =
    of_string (Bytes.unsafe_to_string s)
end

module Tuple_ = struct
  let create size =
    check_not_null (Pywrappers.pytuple_new size)

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

  let of_list list = of_array (Array.of_list list)
end

let id x = x

module Dict_ = struct
  let create () =
    check_not_null (Pywrappers.pydict_new ())

  let set_item dict key value =
    assert_int_success (Pywrappers.pydict_setitem dict key value)

  let of_bindings_map fkey fvalue list =
    let result = create () in
    List.iter begin fun (key, value) ->
      set_item result (fkey key) (fvalue value);
    end list;
    result

  let of_bindings = of_bindings_map id id

  let of_bindings_string = of_bindings_map String_.of_string id
end

module Object_ = struct
  let call_function_obj_args callable args =
    check_not_null (pyobject_callfunctionobjargs callable args)
end

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
    | Int
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
    | Int -> "Int"
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
    | Unicode -> Some (pystring_asstringandsize (String_.as_UTF8_string s))
    | _ -> none

  let string_of_repr item =
    match to_string (object_repr item) with
      Some repr -> check_some repr
    | _ (* None *) -> failwith "Py.Object.string_of_repr"

  let mismatch t o =
    failwith
      (Printf.sprintf "Type mismatch: %s expected. Got: %s (%s)"
         t (name (get o)) (string_of_repr o))

  let create classname parents dict =
    let ty = Pywrappers.pytype_type () in
    let classname = String_.of_string classname in
    let parents = Tuple_.of_list parents in
    let dict = Dict_.of_bindings_string dict in
    Object_.call_function_obj_args ty [| classname; parents; dict |]
end

module Mapping = struct
  let check v = Pywrappers.pymapping_check v <> 0

  let get_item_string mapping key =
    option (Pywrappers.pymapping_getitemstring mapping key)

  let find_string mapping key =
    Pyutils.option_unwrap (get_item_string mapping key)

  let find_string_opt = get_item_string

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

type byteorder =
    LittleEndian
  | BigEndian

let string_length = String.length

module String__ = struct
  include String_

  let check_bytes s =
    Type.get s = Type.Bytes

  let check_unicode s =
    Type.get s = Type.Unicode

  let check s =
    match Type.get s with
      Type.Bytes | Type.Unicode -> true
    | _ -> false

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

  let to_string s =
    match Type.to_string s with
      None -> string_type_mismatch s
    | Some s -> check_some s

  let to_bytes s =
    Bytes.unsafe_of_string (to_string s)
end

module Bytes = struct
  include String__

  let of_string s =
    let len = String.length s in
    if !version_major_value >= 3 then
      check_not_null (Pywrappers.Python3.pybytes_fromstringandsize s len)
    else
      check_not_null (Pywrappers.Python2.pystring_fromstringandsize s len)

  let of_bytes s =
    of_string (Bytes.unsafe_to_string s)
end

module String = String__

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
  | StopIteration

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

  let restore_tuple (ptype, pvalue, ptraceback) =
    restore ptype pvalue ptraceback

  let restore_fetch () =
    match fetch () with
      Some tuple -> restore_tuple tuple
    | None -> failwith "restore_fetch"

  let restore_fetched () =
    match fetched () with
      Some tuple -> restore_tuple tuple
    | None -> failwith "restore_fetched"

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
      | ZeroDivisionError -> Pywrappers.pyexc_zerodivisionerror ()
      | StopIteration -> Pywrappers.pyexc_stopiteration () in
    set_object exc (String.of_string msg)
end

exception Err of Err.t * string

module Object = struct
  include Object_

  type t = Pytypes.pyobject

  let del_item obj item =
    assert_int_success (Pywrappers.pyobject_delitem obj item)

  let del_item_string obj item =
    assert_int_success (Pywrappers.pyobject_delitemstring obj item)

  let get_attr obj attr =
    option (Pywrappers.pyobject_getattr obj attr)

  let get_attr_string obj attr =
    option (Pywrappers.pyobject_getattrstring obj attr)

  let find_attr obj attr = Pyutils.option_unwrap (get_attr obj attr)

  let find_attr_opt = get_attr

  let find_attr_string obj attr =
    Pyutils.option_unwrap (get_attr_string obj attr)

  let find_attr_string_opt = get_attr_string

  let get_item obj key =
    option (Pywrappers.pyobject_getitem obj key)

  let find obj attr = Pyutils.option_unwrap (get_item obj attr)

  let find_opt = get_item

  let get_item_string obj key = get_item obj (String.of_string key)

  let find_string obj attr = Pyutils.option_unwrap (get_item_string obj attr)

  let find_string_opt = get_item_string

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
         (Pytypes.file_map Unix.descr_of_out_channel out_channel) 1)

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

  let repr_or_string repr v =
    if repr then string_of_repr v
    else to_string v

  let robust_to_string repr v =
    if !initialized then
      try
        try
          repr_or_string repr v
        with E (ty, value) ->
          repr_or_string (Pervasives.not repr) v
      with E (ty, value) ->
        Printf.sprintf "[ERROR] %s: %s" (to_string ty) (to_string value)
    else
      "<python value: run 'Py.initialize ()' to print it>"

  let format fmt v =
    Format.pp_print_string fmt (robust_to_string false v)

  let format_repr fmt v =
    Format.pp_print_string fmt (robust_to_string true v)

  let call_method_obj_args obj name args =
    check_not_null (pyobject_callmethodobjargs obj name args)

  let call_method obj name args =
    call_method_obj_args obj (String.of_string name) args

  let call callable args kw =
    check_not_null (Pywrappers.pyobject_call callable args kw)
end

let exception_printer exn =
  match exn with
    E (ty, value) when !initialized ->
      Some (
      Printf.sprintf "E (%s, %s)" (Object.to_string ty)
        (Object.to_string value))
  | _ -> None

let () = Printexc.register_printer exception_printer

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

  let from_string str base =
    let result = pylong_fromstring str base in
    ignore (check_not_null (fst result));
    result

  let of_string ?(base = 0) s =
    let value, len = from_string s base in
    if len <> string_length s then
      failwith "Py.Long.of_string";
    value

  let to_string = Object.to_string
end

module Int = struct
  let check o = Type.get o = Type.Long

  let of_int64 v =
    if version_major () >= 3 then
      Long.of_int64 v
    else
      check_not_null (Pywrappers.Python2.pyint_fromlong v)

  let to_int64 v =
    if version_major () >= 3 then
      Long.to_int64 v
    else
      let result = Pywrappers.Python2.pyint_aslong v in
      check_error ();
      result

  let of_int v = of_int64 (Int64.of_int v)

  let to_int v = Int64.to_int (to_int64 v)

  let of_string = Long.of_string

  let to_string = Long.to_string
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

  let in_place_power ?(modulo = none) v0 v1 =
    check_not_null (Pywrappers.pynumber_inplacepower v0 v1 modulo)

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

  let power ?(modulo = none) v0 v1 =
    check_not_null (Pywrappers.pynumber_power v0 v1 modulo)

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

  let of_int i = Int.of_int i

  let of_int64 i = Int.of_int64 i

  let of_float f = Float.of_float f

  let to_float v =
    match Type.get v with
      Type.Float -> Float.to_float v
    | Type.Long -> Int64.to_float (Long.to_int64 v)
    | _ -> Type.mismatch "Long or Float" v

  let ( + ) = add

  let ( - ) = subtract

  let ( * ) = multiply

  let ( / ) = true_divide

  let ( ** ) x y = power x y

  let ( land ) = number_and

  let ( lor ) = number_or

  let ( lxor ) = number_xor

  let ( lnot ) = invert

  let ( lsl ) = lshift

  let ( lsr ) = rshift

  let ( ~- ) = negative
end

module Iter_ = struct
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

(* From stdcompat *)
let vec_to_seq length get v =
  let length = length v in
  let rec aux i () =
    if i = length then Seq.Nil
    else
      let x = get v i in
      Seq.Cons (x, aux (i + 1)) in
  aux 0

let vec_to_seqi length get v =
  let length = length v in
  let rec aux i () =
    if i = length then Seq.Nil
    else
      let x = get v i in
      Seq.Cons ((i, x), aux (i + 1)) in
  aux 0

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

  let list sequence =
    check_not_null (Pywrappers.pysequence_list sequence)

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
    Iter_.fold_left f v (Object.get_iter sequence)

  let for_all p sequence =
    Iter_.for_all p (Object.get_iter sequence)

  let exists p sequence =
    Iter_.exists p (Object.get_iter sequence)

  let to_seq = vec_to_seq size get

  let to_seqi = vec_to_seqi size get
end

module Tuple = struct
  include Sequence

  include Tuple_

  let check o = Type.get o = Type.Tuple

  let empty = pytuple_empty ()

  let get_slice tuple i0 i1 =
    check_not_null (Pywrappers.pytuple_getslice tuple i0 i1)

  let size tuple = check_int (Pywrappers.pytuple_size tuple)

  let of_array_map f array =
    init (Array.length array) (fun i -> f (Array.get array i))

  let of_list_map f list = of_array_map f (Array.of_list list)

  let of_sequence = Sequence.tuple

  let of_seq s = of_array (Array.of_seq s)

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

module Dict = struct
  include Dict_

  let check o = Type.get o = Type.Dict

  let clear = Pywrappers.pydict_clear

  let copy v = check_not_null (Pywrappers.pydict_copy v)

  let del_item dict item =
    assert_int_success (Pywrappers.pydict_delitem dict item)

  let del_item_string dict name =
    assert_int_success (Pywrappers.pydict_delitemstring dict name)

  let get_item dict key =
    option (Pywrappers.pydict_getitem dict key)

  let find dict key = Pyutils.option_unwrap (get_item dict key)

  let find_opt = get_item

  let get_item_string dict name =
    option (Pywrappers.pydict_getitemstring dict name)

  let find_string dict key = Pyutils.option_unwrap (get_item_string dict key)

  let find_string_opt = get_item_string

  let keys dict = check_not_null (Pywrappers.pydict_keys dict)

  let items dict = check_not_null (Pywrappers.pydict_items dict)

  let set_item_string dict name value =
    assert_int_success (Pywrappers.pydict_setitemstring dict name value)

  let size dict =
    let sz = Pywrappers.pydict_size dict in
    assert_int_success sz;
    sz

  let values dict =
    check_not_null (Pywrappers.pydict_values dict)

  let iter f dict =
    Iter_.iter begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      f key value
    end (Object.get_iter (items dict))

  let fold f dict v =
    Iter_.fold_left begin fun v pair ->
      let (key, value) = Tuple.to_pair pair in
      f key value v
    end v (Object.get_iter (items dict))

  let for_all p dict =
    Iter_.for_all begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      p key value
    end (Object.get_iter (items dict))

  let exists p dict =
    Iter_.exists begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      p key value
    end (Object.get_iter (items dict))

  let to_bindings_map fkey fvalue dict =
    Iter_.to_list_map begin fun pair ->
      let (key, value) = Tuple.to_pair pair in
      (fkey key, fvalue value)
    end (Object.get_iter (items dict))

  let to_bindings = to_bindings_map id id

  let to_bindings_string = to_bindings_map String.to_string id

  let singleton key value = of_bindings [(key, value)]

  let singleton_string key value = of_bindings_string [(key, value)]
end

module Callable = struct
  let check v = Pywrappers.pycallable_check v <> 0

  let handle_errors f arg =
    try f arg with
      E (errtype, errvalue) ->
        Err.set_object errtype errvalue;
        null
    | Err (errtype, msg) ->
        Err.set_error errtype msg;
        null

  let of_function_as_tuple ?(docstring = "Anonymous closure") f =
    check_not_null (pywrap_closure docstring
      (WithoutKeywords (handle_errors f)))

  let of_function_as_tuple_and_dict ?(docstring = "Anonymous closure") f =
    check_not_null (pywrap_closure docstring
      (WithKeywords (fun args -> handle_errors (f args))))

  let of_function ?docstring f =
    of_function_as_tuple ?docstring (fun args -> f (Tuple.to_array args))

  let of_function_with_keywords ?docstring f =
    of_function_as_tuple_and_dict ?docstring
      (fun args dict -> f (Tuple.to_array args) dict)

  let to_function_as_tuple c =
    if not (check c) then
      Type.mismatch "Callable" c;
    function args ->
      Eval.call_object c args

  let to_function_as_tuple_and_dict c =
    if not (check c) then
      Type.mismatch "Callable" c;
    fun args keywords ->
      Eval.call_object_with_keywords c args keywords

  let to_function c =
    let f = to_function_as_tuple c in
    fun args -> f (Tuple.of_array args)

  let to_function_with_keywords c =
    let f = to_function_as_tuple_and_dict c in
    fun args keywords ->
      f (Tuple.of_array args) (Dict.of_bindings_string keywords)
end

module Import = struct
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

  let import_module_opt name =
    try
      Some (check_not_null (Pywrappers.pyimport_importmodule name))
    with E (e, msg)
        when
          let ty = Object.to_string e in
          ty = "<class 'ModuleNotFoundError'>" ||
            ty = "<type 'exceptions.ImportError'>" ->
      None

  let try_import_module = import_module_opt

  let import_module_level name globals locals fromlist level =
    check_not_null
      (Pywrappers.pyimport_importmodulelevel name globals locals fromlist level)

  let import_module_ex name globals locals fromlist =
    import_module_level name globals locals fromlist (-1)

  let reload_module obj =
    check_not_null (Pywrappers.pyimport_reloadmodule obj)
end

let import = Import.import_module

let import_opt = Import.import_module_opt

let option_map f o =
  match o with
  | None -> None
  | Some x -> Some (f x)

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

  let get = Object.find_attr_string

  let get_opt = Object.find_attr_string_opt

  let set = Object.set_attr_string

  let get_function m name = Callable.to_function (get m name)

  let get_function_opt m name = option_map Callable.to_function (get_opt m name)

  let get_function_with_keywords m name =
    Callable.to_function_with_keywords (get m name)

  let get_function_with_keywords_opt m name =
    option_map Callable.to_function_with_keywords (get_opt m name)

  let set_function m name f = set m name (Callable.of_function f)

  let set_function_with_keywords m name f =
    set m name (Callable.of_function_with_keywords f)

  let remove = Object.del_attr_string

  let main () = Import.add_module "__main__"

  let sys () = Import.import_module "sys"

  let builtins () = get (main ()) "__builtins__"
end

module Class = struct
  let init ?(parents = []) ?(fields = []) ?(methods = []) classname =
    if version_major () >= 3 then
      let methods = List.rev_map (fun (name, closure) ->
        (name, Pywrappers.Python3.pyinstancemethod_new closure)) methods in
      Type.create classname parents (List.rev_append methods fields)
    else
      let classname = String.of_string classname in
      let dict = Dict.of_bindings_string fields in
      let c =
        check_not_null
          (Pywrappers.Python2.pyclass_new (Tuple.of_list parents) dict classname) in
      let add_method (name, closure) =
        let m = check_not_null (Pywrappers.pymethod_new closure null c) in
        Dict.set_item_string dict name m in
      List.iter add_method methods;
      c
end

module Iter = struct
  include Iter_

  let create next =
    let next_name =
      if version_major () >= 3 then "__next__"
      else "next" in
    let next' _args =
      match next () with
        None -> raise (Err (Err.StopIteration, ""))
      | Some item -> item in
    let methods = [next_name, Callable.of_function next'] in
    Object.call_function_obj_args
      (Class.init ~methods "iterator") [| |]

  let of_seq s =
    let s = ref s in
    let next () =
      match !s () with
      | Seq.Nil -> None
      | Seq.Cons (head, tail) ->
          s := tail;
          Some head in
    create next

  let to_seq i =
    let rec seq lazy_next () =
      match Lazy.force lazy_next with
      | None -> Seq.Nil
      | Some item ->
          Seq.Cons (item, seq (lazy (next i))) in
    seq (lazy (next i))

  let unsafe_to_seq i =
    let rec seq () =
      match next i with
      | None -> Seq.Nil
      | Some item ->
          Seq.Cons (item, seq) in
    seq
end

module List = struct
  include Sequence

  let check v = Type.get v = Type.List

  let create size = check_not_null (Pywrappers.pylist_new size)

  let size list = check_int (Pywrappers.pylist_size list)

  let length = size

  let set_item list index value =
    assert_int_success (Pywrappers.pylist_setitem list index value)

  let set = set_item

  let init size f =
    let result = create size in
    for index = 0 to size - 1 do
      set result index (f index)
    done;
    result

  let of_array array = init (Array.length array) (Array.get array)

  let of_array_map f array =
    init (Array.length array) (fun i -> f (Array.get array i))

  let of_list list = of_array (Array.of_list list)

  let of_list_map f list = of_array_map f (Array.of_list list)

  let of_sequence = Sequence.list

  let singleton v = init 1 (fun _ -> v)

  let of_seq s = of_array (Array.of_seq s)
end

module Marshal = struct
  let version () =
    let marshal_module = Import.import_module "marshal" in
    Long.to_int (Module.get marshal_module "version")

  let read_object_from_file file =
    let fd = Pytypes.file_map Unix.descr_of_in_channel file in
    check_not_null (Pywrappers.pymarshal_readobjectfromfile fd)

  let load = read_object_from_file

  let read_last_object_from_file file =
    let fd = Pytypes.file_map Unix.descr_of_in_channel file in
    check_not_null (Pywrappers.pymarshal_readlastobjectfromfile fd)

  let read_object_from_string s len =
    check_not_null (Pywrappers.pymarshal_readobjectfromstring s len)

  let loads s = read_object_from_string s (string_length s)

  let write_object_to_file v file version =
    let fd = Pytypes.file_map Unix.descr_of_out_channel file in
    assert_int_success (Pywrappers.pymarshal_writeobjecttofile v fd version)

  let dump ?(version = version ()) v file =
    write_object_to_file v file version

  let write_object_to_string v version =
    check_not_null (Pywrappers.pymarshal_writeobjecttostring v version)

  let dumps ?(version = version ()) v =
    String.to_string (write_object_to_string v version)
end

module Array = struct
  let of_indexed_structure getter setter length =
    let methods =
      ["__len__",
       Callable.of_function_as_tuple (fun _tuple -> Int.of_int length);
       "__getitem__", Callable.of_function_as_tuple (fun tuple ->
         let (self, key) = Tuple.to_tuple2 tuple in
         getter (Long.to_int key));
       "__setitem__", Callable.of_function_as_tuple (fun tuple ->
         let (self, key, value) = Tuple.to_tuple3 tuple in
         setter (Long.to_int key) value;
         none);
       "__iter__", Callable.of_function_as_tuple (fun tuple ->
         let cursor = ref 0 in
         let next () =
           let index = !cursor in
           if index < length then
             begin
               cursor := succ index;
               Some (getter index)
             end
           else
             None in
         Iter.create next);
       "__repr__", Callable.of_function_as_tuple (fun tuple ->
         let (self) = Tuple.to_tuple1 tuple in
         Object.repr (Sequence.list self))] in
    Object.call_function_obj_args (Class.init ~methods "array") [| |]

  let of_array getter setter a =
    of_indexed_structure (fun i -> getter a.(i)) (fun i v -> a.(i) <- setter v)
      (Array.length a)

  type numpy_info = {
      numpy_api: Object.t;
      array_pickle: floatarray -> Object.t;
      array_unpickle: Object.t -> floatarray;
      pyarray_subtype: Object.t;
    }

  let numpy_info = ref None

  let () = on_finalize (fun () -> numpy_info := None)

  external get_pyarray_type: Object.t -> Object.t = "get_pyarray_type"

  external pyarray_of_floatarray: Object.t -> Object.t
    -> floatarray
    -> Object.t = "pyarray_of_floatarray_wrapper"

  let get_numpy_info () =
    match !numpy_info with
      Some info -> info
    | None ->
        let numpy_api =
          let numpy = Import.import_module "numpy.core.multiarray" in
          Object.find_attr_string numpy "_ARRAY_API" in
        let array_pickle, array_unpickle = Capsule.make "floatarray" in
        let pyarray_subtype =
          let pyarray_type = get_pyarray_type numpy_api in
          Type.create "ocamlarray" [pyarray_type] [("ocamlarray", none)] in
        let info =
          { numpy_api; array_pickle; array_unpickle;
            pyarray_subtype } in
        numpy_info := Some info;
        info

  let numpy_api () =
    (get_numpy_info ()).numpy_api

  let pyarray_type () =
    get_pyarray_type (numpy_api ())

  let numpy a =
    let info = get_numpy_info () in
    let result = pyarray_of_floatarray info.numpy_api info.pyarray_subtype a in
    let result = check_not_null result in
    Object.set_attr_string result "ocamlarray" (info.array_pickle a);
    result

  let numpy_get_array a =
    let info = get_numpy_info () in
    info.array_unpickle (Object.find_attr_string a "ocamlarray")
end

module Run = struct
  let any_file file filename =
    assert_int_success
      (Pywrappers.pyrun_anyfileexflags
         (Pytypes.file_map Unix.descr_of_in_channel file) filename 1 None)

  let file file filename start globals locals =
    let fd = Pytypes.file_map Unix.descr_of_in_channel file in
    check_not_null
      (Pywrappers.pyrun_fileexflags fd filename start globals locals 1 None)

  let interactive_one channel name =
    let fd = Channel (Unix.descr_of_in_channel channel) in
    assert_int_success (Pywrappers.pyrun_interactiveoneflags fd name None)

  let interactive_loop channel name =
    let fd = Channel (Unix.descr_of_in_channel channel) in
    assert_int_success (Pywrappers.pyrun_interactiveloopflags fd name None)

  let simple_file channel name =
    let fd = Pytypes.file_map Unix.descr_of_in_channel channel in
    assert_int_success (Pywrappers.pyrun_simplefileexflags fd name 1 None)

  let simple_string string =
    Pywrappers.pyrun_simplestringflags string None = 0

  let string s start globals locals =
    check_not_null
      (Pywrappers.pyrun_stringflags s start globals locals None)

  let eval ?(start = Eval) ?(globals = Module.get_dict (Module.main ()))
      ?(locals = globals) s =
    string s start globals locals

  let load ?(start = File) ?(globals = Module.get_dict (Module.main ()))
      ?(locals = globals) chan filename =
    file chan filename start globals locals

  let interactive () =
    interactive_loop stdin "<stdin>"

  let frame f arg =
    let m = Import.add_module "_pyml" in
    let result = ref None in
    let callback =
      (Callable.of_function (fun _ -> result := Some (f arg); pynone ())) in
    Module.set m "callback" callback;
    ignore (eval ~start:File "
from _pyml import callback
callback()
");
    match !result with
      None -> failwith "frame"
    | Some result -> result

(*
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
 *)

  let make_frame = frame

  let ipython ?(frame=true) () =
    let f () =
      let f =
        try
          Module.get_function (Import.import_module "IPython") "embed"
        with E _ ->
          let shell = Import.import_module "IPython.Shell" in
          let arg = [("argv", List.of_list [String.of_string ""])] in
          let f' =
            Module.get_function_with_keywords shell "IPShellEmbed" [| |] arg in
          Callable.to_function f' in
        ignore (f [| |]) in
    if frame then make_frame f ()
    else f ()
end

let set_argv argv =
  Module.set (Module.sys ()) "argv" (List.of_array_map String.of_string argv)

let last_value () = Module.get (Module.builtins ()) "_"
