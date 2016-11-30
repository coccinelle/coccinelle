let tests = Queue.create ()

let add_test ~title f =
  Queue.add (title, f) tests

let failed = ref false

let launch_test (title, f) =
  Printf.printf "Test '%s' ... %!" title;
  try
    f ();
    Printf.printf "passed\n%!"
  with
    Py.E (_, value) ->
    Printf.printf "raised a Python exception: %s\n%!"
        (Py.Object.to_string value);
    failed := true
  | e ->
    Printf.printf "raised an exception: %s\n%!" (Printexc.to_string e);
    failed := true

let rec launch_tests () =
  match
    try Some (Queue.pop tests)
    with Queue.Empty -> None
  with
    None -> ()
  | Some test ->
      launch_test test;
      launch_tests ()

let () =
  add_test
    ~title:"version"
    (fun () ->
      Printf.printf "Python version %s\n%!" (Py.version ())
    )

let () =
  add_test
    ~title:"library version"
    (fun () ->
      Printf.printf "Python library version %s\n%!" (Py.get_version ())
    )

let () =
  add_test
    ~title:"hello world"
    (fun () ->
      assert (Py.Run.simple_string "print('Hello world!')")
    )

let () =
  add_test
    ~title:"class"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let value_obtained = ref None in
      let callback arg =
        value_obtained := Some (Py.String.to_string (Py.Tuple.get_item arg 1));
        Py.none in
      let c =
        Py.Class.init (Py.String.of_string "myClass")
          ~methods:[("callback", callback)] in
      Py.Module.set m "myClass" c;
      assert (Py.Run.simple_string "
from test import myClass
myClass().callback('OK')
");
      assert (!value_obtained = Some "OK")
    )

let () =
  add_test
    ~title:"empty tuple"
    (fun () ->
      assert (Py.Tuple.create 0 = Py.Tuple.empty))

let () =
  add_test
    ~title:"make tuple"
    (fun () ->
      assert
        (Py.Tuple.to_singleton (Py.Tuple.singleton (Py.Long.of_int 0))
           = Py.Long.of_int 0))

let () =
  add_test
    ~title:"module get/set/remove"
    (fun () ->
      let m = Py.Module.create "test" in
      Py.Module.set m "test" Py.none;
      assert (Py.Module.get m "test" = Py.none);
      Py.Module.remove m "test";
      begin
        try
          ignore (Py.Module.get m "test");
          failwith "Should have been removed"
        with Py.E _ -> ()
      end)

let () =
  add_test
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
      Py.Module.set m "wrap" (Py.Callable.of_function_array pywrap);
      Py.Module.set m "unwrap" (Py.Callable.of_function_array pyunwrap);
      assert (Py.Run.simple_string "
from test import wrap, unwrap
x = wrap('OK')
print('Capsule type: {}'.format(x))
assert unwrap(x) == 'OK'
");
    )

let () =
  add_test
    ~title:"exception"
    (fun () ->
      try
        let _ = Py.Run.eval ~start:Py.File "
raise Exception('Great')
" in
        failwith "uncaught exception"
      with Py.E (_, value) ->
        assert (Py.Object.to_string value = "Great"))

let () =
  add_test
    ~title:"ocaml exception"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let f _ =
        raise (Py.Err (Py.Err.Exception, "Great")) in
      let mywrap = Py.Callable.of_function f in
      Py.Module.set m "mywrap" mywrap;
      assert (Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
    raise Exception('No exception raised')
except Exception as err:
    assert str(err) == \"Great\"
");
    )

let () =
  add_test
    ~title:"ocaml other exception"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let f _ = raise Exit in
      let mywrap = Py.Callable.of_function f in
      Py.Module.set m "mywrap" mywrap;
      try
        assert (Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
except Exception as err:
    raise Exception('Should not be caught by Python')
");
        failwith "Uncaught exception"
      with Exit ->
        ()
    )

let with_temp_file contents f =
  let (file, channel) = Filename.open_temp_file "test" ".py" in
  Py.Utils.try_finally begin fun () ->
    Py.Utils.write_and_close channel (output_string channel) contents;
    let channel = open_in file in
    Py.Utils.read_and_close channel f (file, channel)
  end ()
    Sys.remove file

let with_pipe f =
  let (read, write) = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr read
  and out_channel = Unix.out_channel_of_descr write in
  Py.Utils.try_finally (f in_channel) out_channel
    (fun () ->
      close_in in_channel;
      close_out out_channel) ()

let with_stdin_from channel f arg =
  let stdin_backup = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel channel) Unix.stdin;
  Py.Utils.try_finally
    f arg
    (Unix.dup2 stdin_backup) Unix.stdin

let with_stdin_from_string s f arg =
  with_pipe begin fun in_channel out_channel ->
    output_string out_channel s;
    close_out out_channel;
    with_stdin_from in_channel f arg
  end

let () =
  add_test
    ~title:"run file"
    (fun () ->
      let result = with_temp_file "print(\"Hello, world!\")"
        begin fun (file, channel) ->
         Py.Run.load channel "test.py"
        end in
      if result <> Py.none then
        let result_str = Py.Object.to_string result in
        let msg = Printf.sprintf "Result None expected but got %s" result_str in
        failwith msg
    )

let () =
  add_test
    ~title:"boolean"
    (fun () ->
      try
        if not (Py.Bool.to_bool (Py.Run.eval "True")) then
          failwith "true is false";
        if Py.Bool.to_bool (Py.Run.eval "False") then
          failwith "false is true";
      with Py.E (_, value) ->
        failwith (Py.Object.to_string value))

let () =
  add_test
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
      Py.initialize ()
    )

let () =
  add_test
    ~title:"string conversion error"
    (fun () ->
      try
        let _ = Py.String.to_string (Py.Long.of_int 0) in
        failwith "uncaught exception"
      with
        Py.E (_, value) ->
          Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value)
      | Failure s ->
          Printf.printf "Caught failure: %s\n%!" s)

let () =
  add_test
    ~title:"float conversion error"
    (fun () ->
      try
        let _ = Py.Float.to_float (Py.String.of_string "a") in
        failwith "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value))

let () =
  add_test
    ~title:"long conversion error"
    (fun () ->
      try
        let _ = Py.Long.to_int (Py.String.of_string "a") in
        failwith "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value))

let () =
  add_test
    ~title:"iterators"
    (fun () ->
      let iter = Py.Object.get_iter (Py.Run.eval "['a','b','c']") in
      let list = Py.Iter.to_list_map Py.String.to_string iter in
      assert (list = ["a"; "b"; "c"]))

let () =
  add_test
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
      end table)

let () =
  add_test
    ~title:"unicode"
    (fun () ->
      let codepoints = [| 8203; 127; 83; 2384; 0; 12 |] in
      let python_string = Py.String.of_unicode codepoints in
      let ocaml_string = Py.String.to_string python_string in
      let python_string' = Py.String.decode_UTF8 ocaml_string in
      let codepoints' = Py.String.to_unicode python_string' in
      assert (codepoints = codepoints'))

let () =
  add_test
    ~title:"interactive loop"
    (fun () ->
      with_stdin_from_string "42"
        Py.Run.interactive ();
      assert (Py.Long.to_int (Py.last_value ()) = 42))

let () =
  add_test
    ~title:"IPython"
    (fun () ->
      with_stdin_from_string "exit"
        Py.Run.ipython ())

let () =
  add_test
    ~title:"Marshal"
    (fun () ->
      let v = Py.Long.of_int 42 in
      let m = Py.Marshal.dumps v in
      let v' = Py.Marshal.loads m in
      assert (Py.Long.to_int v' = 42))

let () =
  add_test
    ~title:"Py.List.of_list"
    (fun () ->
      let v = Py.List.of_list [Py.Long.of_int 42] in
      assert (Py.List.length v = 1);
      assert (Py.Long.to_int (Py.List.get v 0) = 42))

let () =
  prerr_endline "Initializing library...";
  Py.initialize ();
  prerr_endline "Starting tests...";
  launch_tests ();
  if !failed then
    exit 1
