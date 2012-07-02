#use "topfind";;
#require "pycaml";;

open Pycaml;;


let _py_float_product =
  python_pre_interfaced_function
    ~doc:"Compute the Product of two floatingpoint numbers in OCaml"
    [|FloatType;FloatType|]
    (fun py_args ->
       let x = pyfloat_asdouble py_args.(0)
       and y = pyfloat_asdouble py_args.(1)
       in pyfloat_fromdouble ( x*.y))
in
  register_pre_functions_for_python
    [|("floatproduct", _py_float_product) |]
;;

Printf.printf "within python, try for example 'ocaml.floatproduct(3.0,4.0)'\n%!";;

ipython();; 



