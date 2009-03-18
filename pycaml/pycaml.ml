(*
 * (C) arty 2002
 * This software is covered under the GNU lesser general public license
 *)

open Int64 ;;

type funcptr 
type pyobject 
type funcent = (funcptr * int * int)

type pymodule_func = { pyml_name : string ; 
		       pyml_func : (pyobject -> pyobject) ;
		       pyml_flags : int ;
		       pyml_doc : string }

type pyobject_type =
    TupleType
  | StringType
  | IntType
  | FloatType
  | ListType
  | NoneType
  | CallableType
  | ModuleType
  | ClassType
  | NullType
  | TypeType
  | OtherType
      
      (* Function list *)
      
external getfuncarray : unit -> funcent array = "pygetfuncarray"
let py_funcs = getfuncarray () 
external pytype : pyobject -> pyobject_type = "pytype"
    
external pyfuncall1 : (funcptr * int * int) -> unit -> unit = "pygencall"
external pyfuncall2 : (funcptr * int * int) -> int -> unit = "pygencall"
external pyfuncall3 : (funcptr * int * int) -> string -> unit = "pygencall"
external pyfuncall4 : (funcptr * int * int) -> unit -> int = "pygencall"
external pyfuncall5 : (funcptr * int * int) -> string -> int = "pygencall"
external pyfuncall6 : (funcptr * int * int) -> (int * string) -> int = "pygencall"
external pyfuncall7 : (funcptr * int * int) -> (int * string * int) -> int =
  "pygencall"
external pyfuncall8 : (funcptr * int * int) -> unit -> string = "pygencall"
external pyfuncall9 : (funcptr * int * int) -> (string * int * pyobject * pyobject) -> pyobject = "pygencall"
external pyfuncall10 : (funcptr * int * int) -> 
  (int * string * int * pyobject * pyobject) -> pyobject = "pygencall"
external pyfuncall11 : (funcptr * int * int) ->
  (int * string * int * pyobject * pyobject * int) -> pyobject = "pygencall"
external pyfuncall12 : (funcptr * int * int) ->
  (string * string * int) -> pyobject = "pygencall"
external pyfuncall13 : (funcptr * int * int) ->
  (pyobject * int * int) -> int = "pygencall"
external pyfuncall14 : (funcptr * int * int) -> pyobject -> pyobject = "pygencall"
external pyfuncall15 : (funcptr * int * int) -> (pyobject * pyobject * int) -> pyobject = "pygencall"
external pyfuncall16 : (funcptr * int * int) -> (pyobject * string) -> pyobject = "pygencall"
external pyfuncall17 : (funcptr * int * int) -> (pyobject * pyobject) -> pyobject = "pygencall"
external pyfuncall18 : (funcptr * int * int) -> pyobject -> int = "pygencall"
external pyfuncall19 : (funcptr * int * int) -> (pyobject * pyobject) -> int = "pygencall"
external pyfuncall20 : (funcptr * int * int) -> (pyobject * pyobject * int) -> int = "pygencall"
external pyfuncall21 : (funcptr * int * int) -> (pyobject * string * pyobject) -> int = "pygencall"
external pyfuncall22 : (funcptr * int * int) -> (pyobject * string) -> int = "pygencall"
external pyfuncall23 : (funcptr * int * int) -> (pyobject * pyobject) -> (pyobject * pyobject) option = "pygencall"
external pyfuncall24 : (funcptr * int * int) -> (pyobject * pyobject * pyobject) -> int = "pygencall"
external pyfuncall25 : (funcptr * int * int) -> pyobject -> int64 = "pygencall"
external pyfuncall26 : (funcptr * int * int) -> pyobject -> string = "pygencall"
external pyfuncall27 : (funcptr * int * int) -> (pyobject * pyobject) -> pyobject = "pygencall"
external pyfuncall28 : (funcptr * int * int) -> string -> pyobject = "pygencall"
external pyfuncall29 : (funcptr * int * int) -> unit -> pyobject = "pygencall"
external pyfuncall30 : (funcptr * int * int) -> pyobject -> unit = "pygencall"
external pyfuncall31 : (funcptr * int * int) -> (pyobject * int) -> 
  (pyobject * pyobject * int) option = "pygencall"
external pyfuncall34 : (funcptr * int * int) -> int64 -> pyobject = "pygencall"
external pyfuncall35 : (funcptr * int * int) -> unit -> int64 = "pygencall"
external pyfuncall36 : (funcptr * int * int) -> float -> pyobject = "pygencall"
external pyfuncall37 : (funcptr * int * int) -> pyobject -> float = "pygencall"
external pyfuncall39 : (funcptr * int * int) -> int -> pyobject = "pygencall"
external pyfuncall40 : (funcptr * int * int) -> (pyobject * int) -> 
  pyobject = "pygencall"
external pyfuncall41 : (funcptr * int * int) -> (pyobject * int * pyobject) ->
  int = "pygencall"
external pyfuncall42 : (funcptr * int * int) -> (pyobject * pyobject * pyobject) ->
  pyobject = "pygencall"
external pyfuncall43 : (funcptr * int * int) -> (pyobject * int) -> 
  (int * int * int) option = "pygencall"
external pyfuncall44 : (funcptr * int * int) -> (int * int * int * int) -> 
  pyobject = "pygencall"
external pyfuncall45 : (funcptr * int * int) -> (pyobject * pyobject) -> unit =
  "pygencall"
external pyfuncall46 : (funcptr * int * int) -> (pyobject * string) -> unit =
  "pygencall"
external pyfuncall47 : (funcptr * int * int) -> (pyobject * pyobject * pyobject) ->
  (pyobject * pyobject * pyobject) = "pygencall"
external pyfuncall48 : (funcptr * int * int) -> (pyobject * pyobject * pyobject) ->
  unit = "pygencall"
external pyfuncall49 : (funcptr * int * int) -> (pyobject * string) -> pyobject =
  "pygencall"
external pyfuncall50 : (funcptr * int * int) -> (string * pyobject * string) ->
  pyobject = "pygencall"
external pyfuncall51 : (funcptr * int * int) -> 
  (string * pyobject * pyobject * pyobject) -> pyobject = "pygencall"
external pyfuncall52 : (funcptr * int * int) -> pyobject -> string = "pygencall"
external pyfuncall53 : (funcptr * int * int) -> (pyobject * int * int * pyobject) -> int = "pygencall"
external pyfuncall54 : (funcptr * int * int) -> (pyobject * int * int) -> int =
  "pygencall"

let fmt1call func = pyfuncall1 func
let fmt2call func = pyfuncall2 func
let fmt3call func = pyfuncall3 func
let fmt4call func = pyfuncall4 func
let fmt5call func = pyfuncall5 func
let fmt6call func = pyfuncall6 func
let fmt7call func = pyfuncall7 func
let fmt8call func = pyfuncall8 func
let fmt9call func = pyfuncall9 func
let fmt10call func = pyfuncall10 func
let fmt11call func = pyfuncall11 func
let fmt12call func = pyfuncall12 func
let fmt13call func = pyfuncall13 func
let fmt14call func = pyfuncall14 func
let fmt15call func = pyfuncall15 func
let fmt16call func = pyfuncall16 func
let fmt17call func = pyfuncall17 func
let fmt18call func = pyfuncall18 func
let fmt19call func = pyfuncall19 func
let fmt20call func = pyfuncall20 func
let fmt21call func = pyfuncall21 func
let fmt22call func = pyfuncall22 func
let fmt23call func = pyfuncall23 func
let fmt24call func = pyfuncall24 func
let fmt25call func = pyfuncall25 func
let fmt26call func = pyfuncall26 func
let fmt27call func = pyfuncall27 func
let fmt28call func = pyfuncall28 func
let fmt29call func = pyfuncall29 func
let fmt30call func = pyfuncall30 func
let fmt31call func = pyfuncall31 func
      (* 32 *)
      (* 33 *)
let fmt34call func = pyfuncall34 func
let fmt35call func = pyfuncall35 func
let fmt36call func = pyfuncall36 func
let fmt37call func = pyfuncall37 func
      (* 38 *)
let fmt39call func = pyfuncall39 func
let fmt40call func = pyfuncall40 func
let fmt41call func = pyfuncall41 func
let fmt42call func = pyfuncall42 func
let fmt43call func = pyfuncall43 func
let fmt44call func = pyfuncall44 func
let fmt45call func = pyfuncall45 func
let fmt46call func = pyfuncall46 func
let fmt47call func = pyfuncall47 func
let fmt48call func = pyfuncall48 func
let fmt49call func = pyfuncall49 func
let fmt50call func = pyfuncall50 func
let fmt51call func = pyfuncall51 func
let fmt52call func = pyfuncall52 func
let fmt53call func = pyfuncall53 func
let fmt54call func = pyfuncall54 func
    
let py_cur_func_num = ref 0
let pnf () = let this_func = py_funcs.(!py_cur_func_num) in 
py_cur_func_num := !py_cur_func_num + 1 ; this_func
  
      (* 1 *)
let py_initialize = fmt1call (pnf ())
let py_finalize = fmt1call (pnf ())
let pyerr_print = fmt1call (pnf ())
      (* 2 *)
let py_exit = fmt2call (pnf ())
let pyerr_printex = fmt2call (pnf ())
      (* 3 *)
let py_setprogramname = fmt3call (pnf ())
let py_setpythonhome = fmt3call (pnf ())
      (* 4 *)
let py_isinitialized = fmt4call (pnf ())
      (* 5 *)
let pyrun_simplestring = fmt5call (pnf ())
      (* 6 *)
let pyrun_anyfile = fmt6call (pnf ())
let pyrun_simplefile = fmt6call (pnf ())
let pyrun_interactiveone = fmt6call (pnf ())
let pyrun_interactiveloop = fmt6call (pnf ())
let py_fdisinteractive = fmt6call (pnf ())
      (* 7 *)
let pyrun_anyfileex = fmt7call (pnf ())
let pyrun_simplefileex = fmt7call (pnf ())
      (* 8 *)
let py_getprogramname = fmt8call (pnf ())
let py_getpythonhome = fmt8call (pnf ())
let py_getprogramfullpath = fmt8call (pnf ())
let py_getprefix = fmt8call (pnf ())
let py_getexecprefix = fmt8call (pnf ())
let py_getpath = fmt8call (pnf ())
let py_getversion = fmt8call (pnf ())
let py_getplatform = fmt8call (pnf ())
let py_getcopyright = fmt8call (pnf ())
let py_getcompiler = fmt8call (pnf ())
let py_getbuildinfo = fmt8call (pnf ())
      (* 9 *)
let pyrun_string = fmt9call (pnf ())
      (* 10 *)
let pyrun_file = fmt10call (pnf ())
      (* 11 *)
let pyrun_fileex = fmt11call (pnf ())
      (* 12 *)
let py_compilestring = fmt12call (pnf ())
      (* 13 *)
let pyobject_print = fmt13call (pnf ())
      (* 14 *)
let pyobject_repr = fmt14call (pnf ())
let pyobject_str = fmt14call (pnf ())
let pyobject_unicode = fmt14call (pnf ())
(* 15 *)
let pyobject_richcompare = fmt15call (pnf ())
(* 16 *)
let pyobject_getattrstring = fmt16call (pnf ())
(* 17 *)
let pyobject_getattr = fmt17call (pnf ())
let pyobject_callobject = fmt17call (pnf ())
(* 18 *)
let pyobject_istrue = fmt18call (pnf ())
let pyobject_not = fmt18call (pnf ())
let pycallable_check = fmt18call (pnf ())
(* 19 *)
let pyobject_compare = fmt19call (pnf ())
let pyobject_hasattr = fmt19call (pnf ())
(* 20 *)
let pyobject_richcomparebool = fmt20call (pnf ())
(* 21 *)
let pyobject_setattrstring = fmt21call (pnf ())
(* 22 *)
let pyobject_hasattrstring = fmt22call (pnf ())
(* 23 *)
let pynumber_coerce = fmt23call (pnf ())
let pynumber_coerceex = fmt23call (pnf ())
(* 24 *)
let pyobject_setattr = fmt24call (pnf ())
(* 25 *)
let pyobject_hash = fmt25call (pnf ())
(* Strings *)
(* 18 *)
let pystring_size = fmt18call (pnf ())
(* 26 *)
let pystring_asstring = fmt26call (pnf ())
(* 27 *)
let pystring_concat = fmt27call (pnf ())
let pystring_concatanddel = fmt27call (pnf ())
(* 28 *)
let pystring_fromstring = fmt28call (pnf ())
(* 17 *)
let pystring_format = fmt17call (pnf ())

(* Dictionaries *)
(* 29 *)
let pydict_new = fmt29call (pnf ())
(* 17 *)
let pydict_getitem = fmt17call (pnf ())
(* 24 *)
let pydict_setitem = fmt24call (pnf ())
(* 19 *)
let pydict_delitem = fmt19call (pnf ())
(* 30 *)
let pydict_clear = fmt30call (pnf ())
(* 31 *)
let pydict_next = fmt31call (pnf ())
(* 14 *)
let pydict_keys = fmt14call (pnf ())
let pydict_values = fmt14call (pnf ())
let pydict_items = fmt14call (pnf ())
let pydict_copy = fmt14call (pnf ())
(* 18 *)
let pydict_size = fmt18call (pnf ())
(* 16 *)
let pydict_getitemstring = fmt16call (pnf ())
(* 22 *)
let pydict_delitemstring = fmt22call (pnf ())
(* 21 *)
let pydict_setitemstring = fmt21call (pnf ())

(* Integer *)
(* 34 *)
let pyint_fromlong = fmt34call (pnf ())
(* 25 *)
let pyint_aslong = fmt25call (pnf ())
(* 35 *)
let pyint_getmax = fmt35call (pnf ())
    
(* Float *)
(* 36 *)
let pyfloat_fromdouble = fmt36call (pnf ())
(* 37 *)
let pyfloat_asdouble = fmt37call (pnf ())

(* Modules *)
(* 28 *)
let pymodule_new = fmt28call (pnf ())
(* 14 *)
let pymodule_getdict = fmt14call (pnf ())
(* 26 *)
let pymodule_getname = fmt26call (pnf ())
let pymodule_getfilename = fmt26call (pnf ())

(* 39 *)
let pytuple_new = fmt39call (pnf ())
(* 18 *)
let pytuple_size = fmt18call (pnf ())
let pytuple_check = fmt18call (pnf ())
(* 40 *)
let pytuple_getitem = fmt40call (pnf ())
(* 41 *)
let pytuple_setitem = fmt41call (pnf ())
(* 13 *)
let pytuple_getslice = fmt13call (pnf ())

(* 42 *)
let pyslice_new = fmt42call (pnf ())
(* 43 *)
let pyslice_getindices = fmt43call (pnf ())
(* 44 *)
let pyrange_new = fmt44call (pnf ())

(* Error handling definitions *)

(* 30 *)
let pyerr_setnone = fmt30call (pnf ())
(* 45 *)
let pyerr_setobject = fmt45call (pnf ())
(* 46 *)
let pyerr_setstring = fmt46call (pnf ())
(* 29 *)
let pyerr_occurred = fmt29call (pnf ())
(* 1 *)
let pyerr_clear = fmt1call (pnf ())
(* 47 *)
let pyerr_fetch = fmt47call (pnf ())
(* 48 *)
let pyerr_restore = fmt48call (pnf ())

(* Error testing and normalization *)
(* 19 *)
let pyerr_givenexceptionmatches = fmt19call (pnf ())
(* 18 *)
let pyerr_exceptionmatches = fmt18call (pnf ())
(* 47 *)
let pyerr_normalizeexception = fmt47call (pnf ())

(* Classes *)
(* 42 *)
let pyclass_new = fmt42call (pnf ())
(* 42 *)
let pyinstance_new = fmt42call (pnf ())

(* 17 *)
let pyinstance_newraw = fmt17call (pnf ())
(* 42 *)
let pymethod_new = fmt42call (pnf ())
(* 14 *)
let pymethod_function = fmt14call (pnf ())
let pymethod_self = fmt14call (pnf ())
let pymethod_class = fmt14call (pnf ())

(* Module *)
(* 28 *)
let pymodule_new = fmt28call (pnf ())
(* 14 *)
let pymodule_getdict = fmt14call (pnf ())
(* 26 *)
let pymodule_getname = fmt26call (pnf ())
let pymodule_getfilename = fmt26call (pnf ())
(* 35 *)
let pyimport_getmagicnumber = fmt35call (pnf ())
(* 49 *)
let pyimport_execcodemodule = fmt49call (pnf ())
(* 50 *)
let pyimport_execcodemoduleex = fmt50call (pnf ())
(* 29 *)
let pyimport_getmoduledict = fmt29call (pnf ())
(* 28 *)
let pyimport_addmodule = fmt28call (pnf ())
let pyimport_importmodule = fmt28call (pnf ())
(* - disabled, see comment in pycaml_ml.c.  - RWMJ
(* 51 *)
let pyimport_importmoduleex = fmt51call (pnf ())
*)
(* 28 *)
let pyimport_import = fmt28call (pnf ())
(* 14 *)
let pyimport_reloadmodule = fmt14call (pnf ())
(* 1 *)
let pyimport_cleanup = fmt1call (pnf ())
(* 5 *)
let pyimport_importfrozenmodule = fmt5call (pnf ())
    
(* Interface to random parts in ceval.c *)
(* 42 *)
let pyeval_callobjectwithkeywords = fmt42call (pnf ())
(* 17 *)
let pyeval_callobject = fmt17call (pnf ())

(* 29 *)
let pyeval_getbuiltins = fmt29call (pnf ())
let pyeval_getglobals = fmt29call (pnf ())
let pyeval_getlocals = fmt29call (pnf ())
let pyeval_getframe = fmt29call (pnf ())
(* 4 *)
let pyeval_getrestricted = fmt4call (pnf ())

(* Abstract layer *)
(* 14 *)
let pyobject_type = fmt14call (pnf ())
(* 18 *)
let pyobject_size = fmt18call (pnf ())
(* 17 *)
let pyobject_getitem = fmt17call (pnf ())
(* 24 *)
let pyobject_setitem = fmt24call (pnf ())
(* 17 *)
let pyobject_delitem = fmt17call (pnf ())
(* 52 *)
let pyobject_ascharbuffer = fmt52call (pnf ())
let pyobject_asreadbuffer = fmt52call (pnf ())
let pyobject_aswritebuffer = fmt52call (pnf ())
(* 18 *)
let pynumber_check = fmt18call (pnf ())
(* 17 *)
let pynumber_add = fmt17call (pnf ())
let pynumber_subtract = fmt17call (pnf ())
let pynumber_multiply = fmt17call (pnf ())
let pynumber_divide = fmt17call (pnf ())
let pynumber_remainder = fmt17call (pnf ())
let pynumber_divmod = fmt17call (pnf ())
(* 42 *)
let pynumber_power = fmt42call (pnf ())
(* 14 *)
let pynumber_negative = fmt14call (pnf ())
let pynumber_positive = fmt14call (pnf ())
let pynumber_absolute = fmt14call (pnf ())
let pynumber_invert = fmt14call (pnf ())
(* 17 *)
let pynumber_lshift = fmt17call (pnf ())
let pynumber_rshift = fmt17call (pnf ())
let pynumber_and = fmt17call (pnf ())
let pynumber_xor = fmt17call (pnf ())
let pynumber_or = fmt17call (pnf ())
(* 14 *)
let pynumber_int = fmt14call (pnf ())
let pynumber_long = fmt14call (pnf ())
let pynumber_float = fmt14call (pnf ())
(* 17 *)
let pynumber_inplaceadd = fmt17call (pnf ())
let pynumber_inplacesubtract = fmt17call (pnf ())
let pynumber_inplacemultiply = fmt17call (pnf ())
let pynumber_inplacedivide = fmt17call (pnf ())
let pynumber_inplaceremainder = fmt17call (pnf ())
let pynumber_inplacelshift = fmt17call (pnf ())
let pynumber_inplacershift = fmt17call (pnf ())
let pynumber_inplaceand = fmt17call (pnf ())
let pynumber_inplacexor = fmt17call (pnf ())
let pynumber_inplaceor = fmt17call (pnf ())
(* 42 *)
let pynumber_inplacepower = fmt42call (pnf ())
(* 18 *)
let pysequence_check = fmt18call (pnf ())
let pysequence_size = fmt18call (pnf ())
let pysequence_length = fmt18call (pnf ())
(* 17 *)
let pysequence_concat = fmt17call (pnf ())
(* 40 *)
let pysequence_repeat = fmt40call (pnf ())
let pysequence_getitem = fmt40call (pnf ())
(* 13 *)
let pysequence_getslice = fmt13call (pnf ())
(* 41 *)
let pysequence_setitem = fmt41call (pnf ())
(* 20 *)
let pysequence_delitem = fmt20call (pnf ())
(* 53 *)
let pysequence_setslice = fmt53call (pnf ())
(* 54 *)
let pysequence_delslice = fmt54call (pnf ())
(* 14 *)
let pysequence_tuple = fmt14call (pnf ())
let pysequence_list = fmt14call (pnf ())
(* 16 *)
let pysequence_fast = fmt16call (pnf ())
(* 19 *)
let pysequence_count = fmt19call (pnf ())
let pysequence_contains = fmt19call (pnf ())
let pysequence_in = fmt19call (pnf ())
let pysequence_index = fmt19call (pnf ())
(* 17 *)
let pysequence_inplaceconcat = fmt17call (pnf ())
(* 22 *)
let pysequence_inplacerepeat = fmt22call (pnf ())
(* 18 *)
let pymapping_check = fmt18call (pnf ())
let pymapping_size = fmt18call (pnf ())
let pymapping_length = fmt18call (pnf ())
(* 16 *)
let pymapping_haskeystring = fmt16call (pnf ())
(* 19 *)
let pymapping_haskey = fmt19call (pnf ())
(* 16 *)
let pymapping_getitemstring = fmt16call (pnf ())
(* 41 *)
let pymapping_setitemstring = fmt41call (pnf ())

external pynull : unit -> pyobject = "pynull"

external pynone : unit -> pyobject = "pynone"
external pytrue : unit -> pyobject = "pytrue"
external pyfalse : unit -> pyobject = "pyfalse"

external pytuple_fromarray : pyobject array -> pyobject = "pytuple_fromarray"
let pytuple_fromsingle elt = pytuple_fromarray [| elt |] 
let pytuple_empty = pytuple_fromarray [| |] 
external pytuple2 : (pyobject * pyobject) -> pyobject = 
  "pytuple_fromarray"
external pytuple3 : (pyobject * pyobject * pyobject) -> pyobject =
  "pytuple_fromarray"
external pytuple4 : (pyobject * pyobject * pyobject * pyobject) ->
  pyobject = "pytuple_fromarray"
external pytuple5 : (pyobject * pyobject * pyobject * pyobject * pyobject) ->
  pyobject = "pytuple_fromarray"
external pytuple6 : (pyobject * pyobject * pyobject * pyobject * pyobject * pyobject) ->
  pyobject = "pytuple_fromarray"

let pyint_fromint i = pyint_fromlong (Int64.of_int i)
let pyint_asint obj = Int64.to_int (pyint_aslong obj)

external pytuple_toarray : pyobject -> pyobject array = "pytuple_toarray"

external pywrap_closure : (pyobject -> pyobject) -> pyobject = "pywrap_closure"
external pycaml_setargs : string -> unit = "pycaml_setargs"

external pymodule_initmodule : string -> pymodule_func array = "pymodule_initmodule"

external pywrap_value : 'a -> pyobject = "pywrapvalue" ;;

external pyunwrap_value : pyobject -> 'a = "pyunwrapvalue" ;;

let _ = py_initialize () ;;
