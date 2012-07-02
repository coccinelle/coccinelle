(*

The OCamlPill: how to pass data from OCaml to Python and give it back to OCaml. 

Motivation (or What is this OCamlPill good for?):

For example, a simulation data structure (such as the finite element
mesh) could be set up in OCaml (because this is fast). We may then
want to be able to control the simulation from Python, and we would
need to pass this populated mesh data structure to another OCaml
module that solves an equation on that mesh.

This is exacly the situation where we wrap up that data structure
(with in OCaml) into -- what is called -- an "OCamlPill". This
essentially means that to Python it is a "pill" with unknown content
where as OCaml can unwrap it and use the content if the pill is given
back to it.

Here are some simple examples to demonstrate this.


*)


(* load the pycaml module *)
#use "topfind";;
#require "pycaml";;

(* and bring it into the current name space *)
open Pycaml;;

(* Register the 'type' of the OCaml pill we will use. For simplicity,
we just call it example. However, we assign this to a variable
'pysym_example' so that we can change the actual string later. *)
let pysym_type = "example";;
register_ocamlpill_types [|pysym_type|];;

(* We also need an 'example' datum to teach the wrapping and
unwrapping function about the type of our OCaml pill. *) 
let ocamlpill_sample_example = [|1;2;3;4|];;

(* Now generate a wrapper and unwrapper *)
let (ocamlpill_from_content, content_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper pysym_type ocamlpill_sample_example;;



(* Now we write the function that creates the data structure and gives it to Python as a OCamlPill:*)
let _py_create_pill =
  python_pre_interfaced_function 
    ~doc:"Create my pill in OCaml and retrieve it from OCaml (to Python).\nFunction takes three integers."
    [|IntType;IntType;IntType|]
    (fun py_args ->
      let x = pyint_asint py_args.(0) in
      let y = pyint_asint py_args.(1) in 
      let z = pyint_asint py_args.(2) in
      let my_data_structure = [|x;y;z;99|]
      in ocamlpill_from_content my_data_structure)
(* and register this function for Python (so it appears in the OCaml-module in Python) *)
in
  register_pre_functions_for_python
  [|("create_pill",_py_create_pill)|]
;;

(* Note: in Python, you can now create an OCamlpill using:

>>>a=ocaml.create_pill(1,3,4)

And then check its type using:
>>>print ocaml.sys_ocamlpill_type(a) 

*)


(* The next part of this example shows how to use this OCamlPill
again, i.e. we would like to be able to issue a command such as
>>>result=ocaml.do_stuff(a)
in Python, and we would like to write the content of the 'do_stuff'
function in OCaml.
*)

(* We write the code that does the work, i.e. which can be called
from Python with an OCamlpill as an argument. This function can work
with the content of the pill, and has to return a Python Object (which
here is just an int). *)
let _py_do_stuff_with_pill =
  python_pre_interfaced_function
    ~doc:"Takes an ocaml pill of type 'example' and returns the first integer in that pill"
    [|CamlpillType|]
    (fun py_args -> 
      let a = content_from_ocamlpill py_args.(0) in
	pyint_fromint a.(0)        (* This is 'do_stuff' *)
    )
  
in register_pre_functions_for_python [|"do_stuff",_py_do_stuff_with_pill|];; 


(* At this point we could start Python (using "python();;"). However,
we add another example below before we do this.

The function defined above, could be used (from Python) as follows:

First create an ocampill using:

>>>A=Ocaml.Create_pill(1,3,4)

And then check its type using:
>>>print ocaml.sys_ocamlpill_type(a) 

Finally, to feedback the 'pill' to OCaml, do something with it in OCaml, and 
return a new Python object (here the first integer of the three), do this:
>>>result=ocaml.do_stuff(a)

The integer variable results should contain the first integer stored in the Ocaml Array.

*)


(* What now follows is a slighly more complicated example, where we pass 
   an ocampill from Python to Ocaml AND another parameter (in this case
   an integer).
*)

(* This function just computes the sum of a given integer array. *)
let compute_sum data = Array.fold_left (fun a b -> a+b) 0 data;;

(* Now we write the code that does the work, i.e. which can be called
from Python with an OCamlpill as the first argument and an extra
integer to be added to the sum as the second. As before, we return a
Python Int object. *)

let _py_do_stuff_with_pill2 = python_pre_interfaced_function
    ~doc:"Takes an ocaml pill and an int, and returns the sum of all ints in the pill and the new int"
    [|CamlpillType;IntType|]
    (fun py_args -> 
      let a = content_from_ocamlpill py_args.(0) in
      let b = pyint_asint py_args.(1) in
      let c = (compute_sum a) + b in
	pyint_fromint c
    )
in register_pre_functions_for_python [|"compute_sum_offset",_py_do_stuff_with_pill2|];;

(* This can be used in Python as follows:

>>>a=ocaml.create_pill(1,3,4)
>>>b=ocaml.compute_sum_offset(a,10)

b should contain the sum of 1, 3, 4, 99 and 10.

*)



(* Finally, we call Python *)
ipython()
