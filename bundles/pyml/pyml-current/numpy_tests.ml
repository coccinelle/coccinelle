let () =
  Pyml_tests_common.add_test ~title:"of_bigarray"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let array = [| 1.; 2. |] in
          let array1 =
            Bigarray.Array1.of_array (Bigarray.float64) (Bigarray.c_layout) array in
          let bigarray = Bigarray.genarray_of_array1 array1 in
          let a = Numpy.of_bigarray bigarray in
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
          assert (Bigarray.Array1.get array1 0 = 42.);
          assert (Bigarray.Array1.get array1 1 = 43.);
          Pyml_tests_common.Passed
        end)

let () =
  Pyml_tests_common.add_test ~title:"to_bigarray"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let m = Py.Import.add_module "test" in
          let callback arg =
            let bigarray =
              Numpy.to_bigarray Bigarray.nativeint Bigarray.c_layout arg.(0) in
            let array1 = Bigarray.array1_of_genarray bigarray in
            assert (Bigarray.Array1.get array1 0 = 0n);
            assert (Bigarray.Array1.get array1 1 = 1n);
            assert (Bigarray.Array1.get array1 2 = 2n);
            assert (Bigarray.Array1.get array1 3 = 3n);
            Py.none in
          Py.Module.set m "callback" (Py.Callable.of_function callback);
          assert (Py.Run.simple_string "
from test import callback
import numpy
callback(numpy.array([0,1,2,3]))
");
          Pyml_tests_common.Passed
        end)

let () =
  if not !Sys.interactive then
    Pyml_tests_common.main ()
