(*
 * (C) arty 2002
 * This software is covered under the GNU lesser general public license
 *)
open Pycaml ;;

let colorsys = pyimport_importmodule "colorsys"
let dict = pymodule_getdict colorsys

let triplet = pytuple3 (pyfloat_fromdouble 1.0,
			pyfloat_fromdouble 0.5,
			pyfloat_fromdouble 0.2) ;;

let rgbtoyiq = pydict_getitemstring (dict,"rgb_to_yiq")
let triplet = pyeval_callobject (rgbtoyiq,triplet)

let _ = print_endline ((string_of_float
			  (pyfloat_asdouble (pytuple_getitem (triplet,0)))) ^ 
		       " " ^
		       (string_of_float
			  (pyfloat_asdouble (pytuple_getitem (triplet,1)))) ^
		       " " ^
		       (string_of_float
			  (pyfloat_asdouble (pytuple_getitem (triplet,2))))) ;;

let x = pywrap_closure 
    (fun x -> print_string (pystring_asstring (pytuple_getitem (x,0))) ; 
      pynone ())

let _ = pyeval_callobject 
    (x,pytuple_fromsingle (pystring_fromstring "hi there"))
