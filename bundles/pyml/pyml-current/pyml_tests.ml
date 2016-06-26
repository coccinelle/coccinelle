let tests = Queue.create ()

let add_test ~title f =
  Queue.add (title, f) tests

let failed = ref false

let launch_test (title, f) =
  Printf.printf "Test '%s' ... " title;
  flush stdout;
  try
    f ();
    Printf.printf "passed\n";
    flush stdout;
  with e ->
    Printf.printf "raised an exception %s\n" (Printexc.to_string e);
    flush stdout;
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
      Printf.printf "Python version %s\n" (Py.version ());
      flush stdout
    )

let () =
  add_test
    ~title:"hello world"
    (fun () ->
      Py.Run.simple_string "print('Hello world!')"
    )

let () =
  add_test
    ~title:"class"
    (fun () ->
      let m = Py.Module.create "test" in
      Py.Dict.set_item_string (Py.Import.get_module_dict ()) "test" m;
      let callback arg =
        Printf.printf "Callback gets %s\n"
          (Py.String.to_string (Py.Tuple.get_item arg 1));
        flush stdout;
        Py.none in
      let c =
        Py.Class.init (Py.String.of_string "myClass")
          (Py.Tuple.create 0) []
          [("callback", callback)] in
      Py.Dict.set_item_string (Py.Module.get_dict m) "myClass" c;
      Py.Run.simple_string "
from test import myClass
myClass().callback('OK')
";
    )

let () =
  add_test
    ~title:"capsule"
    (fun () ->
      let (wrap, unwrap) = Py.Capsule.make "string" in
      let m = Py.Module.create "test" in
      Py.Dict.set_item_string (Py.Import.get_module_dict ()) "test" m;
      let pywrap arg =
        let s = Py.String.to_string (Py.Tuple.get_item arg 0) in
        wrap s in
      let pyunwrap arg =
        let s = unwrap (Py.Tuple.get_item arg 0) in
        Py.String.of_string s in
      Py.Dict.set_item_string (Py.Module.get_dict m) "wrap"
        (Py.Wrap.closure pywrap);
      Py.Dict.set_item_string (Py.Module.get_dict m) "unwrap"
        (Py.Wrap.closure pyunwrap);
      Py.Run.simple_string "
from test import wrap, unwrap
x = wrap('OK')
print('Capsule type: {}'.format(x))
print('Capsule contents: {}'.format(unwrap(x)))
";
    )

let () =
  add_test
    ~title:"exception"
    (fun () ->
      let main = Py.Import.add_module "__main__" in
      let globals = Py.Module.get_dict main in
      let locals = Py.Dict.create () in
      try
        let _ = Py.Run.string "
raise Exception('Great')
" Py.File globals locals in
        failwith "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n" (Py.Object.to_string value);
        flush stdout)

let () =
  add_test
    ~title:"ocaml exception"
    (fun () ->
      let m = Py.Module.create "test" in
      Py.Dict.set_item_string (Py.Import.get_module_dict ()) "test" m;
      let f _ =
        prerr_endline
("Exception to string:" ^
Py.Object.to_string (Pywrappers.pyexc_exception ()));
        raise (Py.Err (Py.Err.Exception, "Great")) in
      let mywrap = Py.Wrap.closure f in
      Py.Dict.set_item_string (Py.Module.get_dict m) "mywrap" mywrap;
      Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
except Exception as err:
    print('Caught exception: ' + str(err))
";
    )

let () =
  add_test
    ~title:"ocaml other exception"
    (fun () ->
      let m = Py.Module.create "test" in
      Py.Dict.set_item_string (Py.Import.get_module_dict ()) "test" m;
      let f _ = raise Exit in
      let mywrap = Py.Wrap.closure f in
      Py.Dict.set_item_string (Py.Module.get_dict m) "mywrap" mywrap;
      try
        Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
except Exception as err:
    print('Caught exception: ' + err)
";
        failwith "Uncaught exception"
      with Exit ->
        ()
    )

let read_file filename f =
  let channel = open_in filename in
  try
    let result = f channel in
    close_in channel;
    result
  with e ->
    close_in_noerr channel;
    raise e

let () =
  add_test
    ~title:"run file"
    (fun () ->
      let main = Py.Import.add_module "__main__" in
      let globals = Py.Module.get_dict main in
      let locals = Py.Dict.create () in
      let result = read_file "test.py" (fun channel ->
        Py.Run.file channel "test.py" Py.File globals locals
      ) in
      if result <> Py.none then
        let result_str = Py.Object.to_string result in
        let msg = Printf.sprintf "Result None expected but got %s" result_str in
        failwith msg
    )

let () =
  add_test
    ~title:"reinitialize"
    (fun () ->
      Py.finalize ();
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
          Printf.printf "Caught exception: %s\n" (Py.Object.to_string value);
          flush stdout
      | Failure s ->
          Printf.printf "Caught failure: %s\n" s;
          flush stdout)

let () =
  add_test
    ~title:"float conversion error"
    (fun () ->
      try
        let _ = Py.Float.to_float (Py.String.of_string "a") in
        failwith "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n" (Py.Object.to_string value);
        flush stdout)

let () =
  add_test
    ~title:"long conversion error"
    (fun () ->
      try
        let _ = Py.Long.to_int (Py.String.of_string "a") in
        failwith "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n" (Py.Object.to_string value);
        flush stdout)

let () =
  add_test
    ~title:"iterators"
    (fun () ->
      try
        prerr_endline
          (String.concat ", "
             (List.map Py.String.to_string
                (Py.Iter.to_list
                   (Py.Object.get_iter (Py.Run.eval "['a','b','c']")))))
      with Py.E (_, value) ->
        failwith (Py.Object.to_string value))

let () =
  prerr_endline "Initializing library...";
  Py.initialize ();
  prerr_endline "Starting tests...";
  launch_tests ();
  if !failed then
    exit 1
