let () =
  Pyml_tests_common.add_test
    ~title:"version"
    (fun () ->
      Printf.printf "Python version %s\n%!" (Py.version ());
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"library version"
    (fun () ->
      Printf.printf "Python library version %s\n%!" (Py.get_version ());
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"hello world"
    (fun () ->
      assert (Py.Run.simple_string "print('Hello world!')");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"class"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let value_obtained = ref None in
      let callback arg =
        value_obtained := Some (Py.String.to_string (Py.Tuple.get_item arg 1));
        Py.none in
      let c =
        Py.Class.init "myClass"
          ~methods:[("callback", Py.Callable.of_function_as_tuple callback)] in
      Py.Module.set m "myClass" c;
      assert (Py.Run.simple_string "
from test import myClass
myClass().callback('OK')
");
      assert (!value_obtained = Some "OK");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"empty tuple"
    (fun () ->
      assert (Py.Tuple.create 0 = Py.Tuple.empty);
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"make tuple"
    (fun () ->
      assert
        (Py.Tuple.to_singleton (Py.Tuple.singleton (Py.Long.of_int 0))
           = Py.Long.of_int 0);
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"module get/set/remove"
    (fun () ->
      let m = Py.Module.create "test" in
      Py.Module.set m "test" Py.none;
      assert (Py.Module.get m "test" = Py.none);
      Py.Module.remove m "test";
      begin
        try
          ignore (Py.Module.get m "test");
          Pyml_tests_common.Failed "Should have been removed"
        with Py.E _ -> Pyml_tests_common.Passed
      end)

let () =
  Pyml_tests_common.add_test
    ~title:"capsule"
    (fun () ->
      let (wrap, unwrap) = Py.Capsule.make "string" in
      let m = Py.Import.add_module "test" in
      let pywrap args =
        let s = Py.String.to_string args.(0) in
        wrap s in
      let pyunwrap args =
        let s = unwrap args.(0) in
        Py.String.of_string s in
      Py.Module.set_function m "wrap" pywrap;
      Py.Module.set_function m "unwrap" pyunwrap;
      assert (Py.Run.simple_string "
from test import wrap, unwrap
x = wrap('OK')
print('Capsule type: {0}'.format(x))
assert unwrap(x) == 'OK'
");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"exception"
    (fun () ->
      try
        let _ = Py.Run.eval ~start:Py.File "
raise Exception('Great')
" in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) ->
        assert (Py.Object.to_string value = "Great");
        Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"ocaml exception"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let mywrap _ =
        raise (Py.Err (Py.Err.Exception, "Great")) in
      Py.Module.set_function m "mywrap" mywrap;
      assert (Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
    raise Exception('No exception raised')
except Exception as err:
    assert str(err) == \"Great\"
");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"ocaml other exception"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let mywrap _ = raise Exit in
      Py.Module.set_function m "mywrap" mywrap;
      try
        assert (Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
except Exception as err:
    raise Exception('Should not be caught by Python')
");
        Pyml_tests_common.Failed "Uncaught exception"
      with Exit ->
        Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"run file with filename"
    (fun () ->
      let result = Pyutils.with_temp_file "print(\"Hello, world!\")"
        begin fun file channel ->
         Py.Run.load (Py.Filename file) "test.py"
        end in
      if result = Py.none then
        Pyml_tests_common.Passed
      else
        let result_str = Py.Object.to_string result in
        let msg = Printf.sprintf "Result None expected but got %s" result_str in
        Pyml_tests_common.Failed msg
    )

let () =
  Pyml_tests_common.add_test
    ~title:"run file with channel"
    (Pyml_tests_common.enable_only_on_unix
       (fun () ->
         let result = Pyutils.with_temp_file "print(\"Hello, world!\")"
           begin fun file channel ->
           Py.Run.load (Py.Channel channel) "test.py"
           end in
         if result = Py.none then
           Pyml_tests_common.Passed
         else
           let result_str = Py.Object.to_string result in
           let msg = Printf.sprintf "Result None expected but got %s" result_str in
           Pyml_tests_common.Failed msg
       )
    )

let () =
  Pyml_tests_common.add_test
    ~title:"boolean"
    (fun () ->
      try
        if not (Py.Bool.to_bool (Py.Run.eval "True")) then
          Pyml_tests_common.Failed "true is false"
        else if Py.Bool.to_bool (Py.Run.eval "False") then
          Pyml_tests_common.Failed "false is true"
        else
          Pyml_tests_common.Passed;
      with Py.E (_, value) ->
        Pyml_tests_common.Failed (Py.Object.to_string value))

let () =
  Pyml_tests_common.add_test
    ~title:"reinitialize"
    (fun () ->
      Py.finalize ();
      begin
        try
          assert (Py.Run.simple_string "not initialized");
          raise Exit
        with
          Failure _ -> ()
        | Exit -> failwith "Uncaught not initialized"
      end;
      let (version, minor) = !Pyml_tests_common.use_version in
      Py.initialize ~verbose:true ?version ?minor ();
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"string conversion error"
    (fun () ->
      try
        let _ = Py.String.to_string (Py.Long.of_int 0) in
        Pyml_tests_common.Failed "uncaught exception"
      with
        Py.E (_, value) ->
          Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value);
          Pyml_tests_common.Passed
      | Failure s ->
          Printf.printf "Caught failure: %s\n%!" s;
          Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"float conversion error"
    (fun () ->
      try
        let _ = Py.Float.to_float (Py.String.of_string "a") in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value);
        Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"long conversion error"
    (fun () ->
      try
        let _ = Py.Long.to_int (Py.String.of_string "a") in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value);
        Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"iterators"
    (fun () ->
      let iter = Py.Object.get_iter (Py.Run.eval "['a','b','c']") in
      let list = Py.Iter.to_list_map Py.String.to_string iter in
      assert (list = ["a"; "b"; "c"]);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Dict.iter"
    (fun () ->
      let dict = Py.Dict.create () in
      for i = 0 to 9 do
        Py.Dict.set_item_string dict (string_of_int i) (Py.Long.of_int i)
      done;
      let table = Array.make 10 None in
      Py.Dict.iter begin fun key value ->
        let index = Py.Long.to_int value in
        assert (table.(index) = None);
        table.(index) <- Some (Py.String.to_string key)
      end dict;
      Array.iteri begin fun i v ->
        match v with
          None -> failwith "None!"
        | Some v' -> assert (i = int_of_string v')
      end table;
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"unicode"
    (fun () ->
      let codepoints = [| 8203; 127; 83; 2384; 0; 12 |] in
      let python_string = Py.String.of_unicode codepoints in
      let ocaml_string = Py.String.to_string python_string in
      let python_string' = Py.String.decode_UTF8 ocaml_string in
      let codepoints' = Py.String.to_unicode python_string' in
      assert (codepoints = codepoints');
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"interactive loop"
    (Pyml_tests_common.enable_only_on_unix (fun () ->
      Pyutils.with_stdin_from_string "42"
        Py.Run.interactive ();
      assert (Py.Long.to_int (Py.last_value ()) = 42);
      Pyml_tests_common.Passed))

let () =
  Pyml_tests_common.add_test
    ~title:"IPython"
    (Pyml_tests_common.enable_only_on_unix
       (Py.Run.frame (Pyutils.with_stdin_from_string "exit" (fun () ->
         if Py.Import.try_import_module "IPython" = None then
           Pyml_tests_common.Disabled "IPython is not available"
         else
           begin
             Py.Run.ipython ~frame:false ();
             Pyml_tests_common.Passed
           end))))

let () =
  Pyml_tests_common.add_test
    ~title:"Marshal"
    (fun () ->
      let v = Py.Long.of_int 42 in
      let m = Py.Marshal.dumps v in
      let v' = Py.Marshal.loads m in
      assert (Py.Long.to_int v' = 42);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Py.List.of_list"
    (fun () ->
      let v = Py.List.of_list [Py.Long.of_int 42] in
      assert (Py.List.length v = 1);
      assert (Py.Long.to_int (Py.List.get v 0) = 42);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test ~title:"array"
    (fun () ->
      let array = [| 1; 2 |] in
      let a = Py.Array.of_array Py.Long.of_int Py.Long.to_int array in
      let m = Py.Import.add_module "test" in
      Py.Module.set m "array" a;
      assert (Py.Run.simple_string "
from test import array
assert len(array) == 2
assert array[0] == 1
assert array[1] == 2
array[0] = 42
array[1] = 43
copy = []
for x in array:
  copy.append(x)
assert copy == [42, 43]
");
      assert (array.(0) = 42);
      assert (array.(1) = 43);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test ~title:"numpy"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let array = Stdcompat.Array.Floatarray.create 2 in
          Stdcompat.Array.Floatarray.set array 0 1.;
          Stdcompat.Array.Floatarray.set array 1 2.;
          let a = Py.Array.numpy array in
          let m = Py.Import.add_module "test" in
          Py.Module.set m "array" a;
          assert (Py.Run.simple_string "
from test import array
assert len(array) == 2
assert array[0] == 1.
assert array[1] == 2.
array[0] = 42.
array[1] = 43.
");
          assert (Stdcompat.Array.Floatarray.get array 0 = 42.);
          assert (Stdcompat.Array.Floatarray.get array 1 = 43.);
          Pyml_tests_common.Passed
        end)

let () =
  if not !Sys.interactive then
    Pyml_tests_common.main ()
