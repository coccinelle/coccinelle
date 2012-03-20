open Pycaml

let foo_bar_print = pywrap_closure
  (fun x -> pytuple_fromarray (pytuple_toarray x)) ;;
let sd = pyimport_getmoduledict () ;;
let mx = pymodule_new "CamlModule" ;;
let cd = pydict_new () ;;
let cx = pyclass_new (pynull (), cd, pystring_fromstring "CamlClass") ;;
let cmx = pymethod_new (foo_bar_print,(pynull ()),cx) ;;
let _ = pydict_setitemstring (cd, "CamlMethod", cmx) ;;
let _ = pydict_setitemstring (pymodule_getdict mx, "CamlClass", cx) ;;
let _ = pydict_setitemstring (sd, "CamlModule", mx) ;;
let _ = pyrun_simplestring
  ("from CamlModule import CamlClass\n" ^
   "x = CamlClass()\n" ^
   "for i in range(100000):\n" ^
   "  x.CamlMethod(1,2,3,4)\n")
