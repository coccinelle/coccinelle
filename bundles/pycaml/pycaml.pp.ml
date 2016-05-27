(*
 * (C) arty 2002

 This library is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as
 published by the Free Software Foundation; either version 2.1 of the
 License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 USA

 * Major Modifications (C) T. Fischbacher 2005,2006
 * 
 * NOTE: have to properly document the CamlType / ocamlpill mechanism!
   
 * More modifications are by Barry Schwartz.
 * Copyright (C) 2009 Barry Schwartz.

 * Note (T.F.): Should we incorporate auto-conversion of int to float?
 * At present, we do not do this. (B.S.): Maybe we should, or maybe we
 * shouldn't. But BytesType/UnicodeType/string conversion does seem
 * called for; it's getting to be a big problem in kompostilo.
 *)

type funcptr 
type pyobject 
type funcent = (funcptr * int * int * bool)

type pymodule_func = {
  pyml_name : string ; 
  pyml_func : (pyobject -> pyobject) ;
  pyml_flags : int ;
  pyml_doc : string;
}

type pyobject_type =
  | TupleType
  | BytesType
  | UnicodeType
  | BoolType
  | IntType
  | FloatType
  | ListType
  | NoneType
  | CallableType
  | ModuleType
  | ClassType
  | TypeType
  | DictType
  | NullType
  | CamlpillType
  | OtherType
  | EitherStringType (* Signifies that either of BytesType or UnicodeType is allowed. *)
  | CamlpillSubtype of string (* Signifies that only the particular Camlpill variety is allowed. *)
  | AnyType                   (* Allow any python object. *)

type pyerror_type =
  | Pyerr_Exception
  | Pyerr_StandardError
  | Pyerr_ArithmeticError
  | Pyerr_LookupError
  | Pyerr_AssertionError
  | Pyerr_AttributeError
  | Pyerr_EOFError
  | Pyerr_EnvironmentError
  | Pyerr_FloatingPointError
  | Pyerr_IOError
  | Pyerr_ImportError
  | Pyerr_IndexError
  | Pyerr_KeyError
  | Pyerr_KeyboardInterrupt
  | Pyerr_MemoryError
  | Pyerr_NameError
  | Pyerr_NotImplementedError
  | Pyerr_OSError
  | Pyerr_OverflowError
  | Pyerr_ReferenceError
  | Pyerr_RuntimeError
  | Pyerr_SyntaxError
  | Pyerr_SystemExit
  | Pyerr_TypeError
  | Pyerr_ValueError
  | Pyerr_ZeroDivisionError

exception Pycaml_exn of (pyerror_type * string)

      
(* Function list *)
      
external pytype : pyobject -> pyobject_type = "pytype"

external pycaml_seterror : pyerror_type -> string -> unit = "pycaml_seterror"

external pynull : unit -> pyobject = "pynull"
external pynone : unit -> pyobject = "pynone"
external py_true : unit -> pyobject = "py_true"
external py_false : unit -> pyobject = "py_false"

(*-----------------------------------------------------------------------*)

(* Type1 *)
external py_initialize : unit -> unit = "Py_Initialize_wrapper"
external py_finalize : unit -> unit = "Py_Finalize_wrapper"
external pyerr_print : unit -> unit = "PyErr_Print_wrapper"
external pyerr_clear : unit -> unit = "PyErr_Clear_wrapper"
external pyimport_cleanup : unit -> unit = "PyImport_Cleanup_wrapper"

(* Type2 *)
external py_exit : int -> unit = "Py_Exit_wrapper"
external pyerr_printex : int -> unit = "PyErr_PrintEx_wrapper"

(* Type3 *)
external py_setprogramname : string -> unit = "Py_SetProgramName_wrapper"
external py_setpythonhome : string -> unit = "Py_SetPythonHome_wrapper"

(* Type4 *)
external py_isinitialized : unit -> int = "Py_IsInitialized_wrapper"
#ifdef PYMAJOR2
external pyeval_getrestricted : unit -> int = "PyEval_GetRestricted_wrapper"
#endif

(* Type5 *)
external pyrun_simplestring : string -> int = "PyRun_SimpleString_wrapper"
external pyimport_importfrozenmodule : string -> int = "PyImport_ImportFrozenModule_wrapper"

(* Test6 *)
external pyrun_anyfile : (int * string) -> int = "PyRun_AnyFile_wrapper"
external pyrun_simplefile : (int * string) -> int = "PyRun_SimpleFile_wrapper"
external pyrun_interactiveone : (int * string) -> int = "PyRun_InteractiveOne_wrapper"
external pyrun_interactiveloop : (int * string) -> int = "PyRun_InteractiveLoop_wrapper"
external py_fdisinteractive : (int * string) -> int = "Py_FdIsInteractive_wrapper"

(* Type7 *)
external pyrun_anyfileex : (int * string * int) -> int = "PyRun_AnyFileEx_wrapper"
external pyrun_simplefileex : (int * string * int) -> int = "PyRun_SimpleFileEx_wrapper"

(* Type8 *)
external py_getprogramname : unit -> string = "Py_GetProgramName_wrapper"
external py_getpythonhome : unit -> string = "Py_GetPythonHome_wrapper"
external py_getprogramfullpath : unit -> string = "Py_GetProgramFullPath_wrapper"
external py_getprefix : unit -> string = "Py_GetPrefix_wrapper"
external py_getexecprefix : unit -> string = "Py_GetExecPrefix_wrapper"
external py_getpath : unit -> string = "Py_GetPath_wrapper"
external py_getversion : unit -> string = "Py_GetVersion_wrapper"
external py_getplatform : unit -> string = "Py_GetPlatform_wrapper"
external py_getcopyright : unit -> string = "Py_GetCopyright_wrapper"
external py_getcompiler : unit -> string = "Py_GetCompiler_wrapper"
external py_getbuildinfo : unit -> string = "Py_GetBuildInfo_wrapper"

(* Type9 *)
external pyrun_string : (string * int * pyobject * pyobject) -> pyobject = "PyRun_String_wrapper"

(* Type10 *)
external pyrun_file : (int * string * int * pyobject * pyobject) -> pyobject = "PyRun_File_wrapper"

(* Type11 *)
external pyrun_fileex : (int * string * int * pyobject * pyobject * int) -> pyobject = "PyRun_FileEx_wrapper"

(* Type12 *)
external py_compilestring : (string * string * int) -> pyobject = "Py_CompileString_wrapper"

(* Type13 *)
external pyobject_print : (pyobject * int * int) -> int = "PyObject_Print_wrapper"
external pytuple_getslice : (pyobject * int * int) -> int = "PyTuple_GetSlice_wrapper"
external pysequence_getslice : (pyobject * int * int) -> int = "PySequence_GetSlice_wrapper"

(* Type14 *)
external pymethod_function : pyobject -> pyobject = "PyMethod_Function_wrapper"
external pymethod_self : pyobject -> pyobject = "PyMethod_Self_wrapper"
#ifdef PYMAJOR2
external pymethod_class : pyobject -> pyobject = "PyMethod_Class_wrapper"
#endif
external pymodule_getdict : pyobject -> pyobject = "PyModule_GetDict_wrapper"
#ifdef PYMAJOR3
external pyunicode_asutf8string : pyobject -> pyobject = "PyUnicode_AsUTF8String_wrapper"
external pyunicode_asutf16string : pyobject -> pyobject = "PyUnicode_AsUTF16String_wrapper"
external pyunicode_asutf32string : pyobject -> pyobject = "PyUnicode_AsUTF32String_wrapper"
#endif
external pyobject_repr : pyobject -> pyobject = "PyObject_Repr_wrapper"
external pyimport_reloadmodule : pyobject -> pyobject = "PyImport_ReloadModule_wrapper"
external pyimport_import : pyobject -> pyobject = "PyImport_Import_wrapper"
external pyobject_str : pyobject -> pyobject = "PyObject_Str_wrapper"
external pyobject_type : pyobject -> pyobject = "PyObject_Type_wrapper"
external pyobject_unicode : pyobject -> pyobject = "PyObject_Unicode_wrapper"
external pydict_keys : pyobject -> pyobject = "PyDict_Keys_wrapper"
external pydict_values : pyobject -> pyobject = "PyDict_Values_wrapper"
external pydict_items : pyobject -> pyobject = "PyDict_Items_wrapper"
external pydict_copy : pyobject -> pyobject = "PyDict_Copy_wrapper"
external pysequence_tuple : pyobject -> pyobject = "PySequence_Tuple_wrapper"
external pysequence_list : pyobject -> pyobject = "PySequence_List_wrapper"
#ifdef PYMAJOR2
external pynumber_int : pyobject -> pyobject = "PyNumber_Int_wrapper"
#endif
external pynumber_long : pyobject -> pyobject = "PyNumber_Long_wrapper"
external pynumber_float : pyobject -> pyobject = "PyNumber_Float_wrapper"
external pynumber_negative : pyobject -> pyobject = "PyNumber_Negative_wrapper"
external pynumber_positive : pyobject -> pyobject = "PyNumber_Positive_wrapper"
external pynumber_absolute : pyobject -> pyobject = "PyNumber_Absolute_wrapper"
external pynumber_invert : pyobject -> pyobject = "PyNumber_Invert_wrapper"
external pyiter_next : pyobject -> pyobject = "PyIter_Next_wrapper"
#ifdef PYMAJOR3
external pyinstancemethod_new : pyobject -> pyobject = "PyInstanceMethod_New_wrapper"
#endif

(* Type15 *)
external pyobject_richcompare : (pyobject * pyobject * int) -> pyobject = "PyObject_RichCompare_wrapper"

(* Type16 *)
external pydict_getitemstring : (pyobject * string) -> pyobject = "PyDict_GetItemString_wrapper"
external pyobject_getattrstring : (pyobject * string) -> pyobject = "PyObject_GetAttrString_wrapper"
external pysequence_fast : (pyobject * string) -> pyobject = "PySequence_Fast_wrapper"
external pymapping_getitemstring : (pyobject * string) -> pyobject = "PyMapping_GetItemString_wrapper"

(* Type17 *)
external pydict_getitem : pyobject * pyobject -> pyobject = "PyDict_GetItem_wrapper"
external pyeval_callobject : pyobject * pyobject -> pyobject = "PyEval_CallObject_wrapper"
external pyobject_getattr : pyobject * pyobject -> pyobject = "PyObject_GetAttr_wrapper"
external pyobject_getitem : pyobject * pyobject -> pyobject = "PyObject_GetItem_wrapper"
external pynumber_add : pyobject * pyobject -> pyobject = "PyNumber_Add_wrapper"
external pynumber_subtract : pyobject * pyobject -> pyobject = "PyNumber_Subtract_wrapper"
external pynumber_multiply : pyobject * pyobject -> pyobject = "PyNumber_Multiply_wrapper"
#ifdef PYMAJOR2
external pynumber_divide : pyobject * pyobject -> pyobject = "PyNumber_Divide_wrapper"
#endif
external pynumber_truedivide : pyobject * pyobject -> pyobject = "PyNumber_TrueDivide_wrapper"
external pynumber_floordivide : pyobject * pyobject -> pyobject = "PyNumber_FloorDivide_wrapper"
external pynumber_remainder : pyobject * pyobject -> pyobject = "PyNumber_Remainder_wrapper"
external pynumber_divmod : pyobject * pyobject -> pyobject = "PyNumber_Divmod_wrapper"
external pynumber_lshift : pyobject * pyobject -> pyobject = "PyNumber_Lshift_wrapper"
external pynumber_rshift : pyobject * pyobject -> pyobject = "PyNumber_Rshift_wrapper"
external pynumber_and : pyobject * pyobject -> pyobject = "PyNumber_And_wrapper"
external pynumber_xor : pyobject * pyobject -> pyobject = "PyNumber_Xor_wrapper"
external pynumber_or : pyobject * pyobject -> pyobject = "PyNumber_Or_wrapper"
external pynumber_inplaceadd : pyobject * pyobject -> pyobject = "PyNumber_InPlaceAdd_wrapper"
external pynumber_inplacesubtract : pyobject * pyobject -> pyobject = "PyNumber_InPlaceSubtract_wrapper"
external pynumber_inplacemultiply : pyobject * pyobject -> pyobject = "PyNumber_InPlaceMultiply_wrapper"
external pynumber_inplacetruedivide : pyobject * pyobject -> pyobject = "PyNumber_InPlaceTrueDivide_wrapper"
external pynumber_inplacefloordivide : pyobject * pyobject -> pyobject = "PyNumber_InPlaceFloorDivide_wrapper"
#ifdef PYMAJOR2
external pynumber_inplacedivide : pyobject * pyobject -> pyobject = "PyNumber_InPlaceDivide_wrapper"
#endif
external pynumber_inplaceremainder : pyobject * pyobject -> pyobject = "PyNumber_InPlaceRemainder_wrapper"
external pynumber_inplacelshift : pyobject * pyobject -> pyobject = "PyNumber_InPlaceLshift_wrapper"
external pynumber_inplacershift : pyobject * pyobject -> pyobject = "PyNumber_InPlaceRshift_wrapper"
external pynumber_inplaceand : pyobject * pyobject -> pyobject = "PyNumber_InPlaceAnd_wrapper"
external pynumber_inplacexor : pyobject * pyobject -> pyobject = "PyNumber_InPlaceXor_wrapper"
external pynumber_inplaceor : pyobject * pyobject -> pyobject = "PyNumber_InPlaceOr_wrapper"
#ifdef PYMAJOR2
external pybytes_format : pyobject * pyobject -> pyobject = "PyBytes_Format_wrapper"
external pystring_format : pyobject * pyobject -> pyobject = "PyBytes_Format_wrapper" (* Legacy support *)
external pyinstance_newraw : pyobject * pyobject -> pyobject = "PyInstance_NewRaw_wrapper"
#endif
external pysequence_concat : pyobject * pyobject -> pyobject = "PySequence_Concat_wrapper"
external pysequence_inplaceconcat : pyobject * pyobject -> pyobject = "PySequence_InPlaceConcat_wrapper"

(* Type18 *)
external pyobject_istrue : pyobject -> int = "PyObject_IsTrue_wrapper"
external pyobject_not : pyobject -> int = "PyObject_Not_wrapper"
external pycallable_check : pyobject -> int = "PyCallable_Check_wrapper"
external pybytes_size : pyobject -> int = "PyBytes_Size_wrapper"
external pystring_size : pyobject -> int = "PyBytes_Size_wrapper" (* Legacy support *)
external pydict_size : pyobject -> int = "PyDict_Size_wrapper"
external pytuple_size : pyobject -> int = "PyTuple_Size_wrapper"
external pyerr_exceptionmatches : pyobject -> int = "PyErr_ExceptionMatches_wrapper"
external pyobject_size : pyobject -> int = "PyObject_Size_wrapper"
external pynumber_check : pyobject -> int = "PyNumber_Check_wrapper"
external pysequence_check : pyobject -> int = "PySequence_Check_wrapper"
external pysequence_size : pyobject -> int = "PySequence_Size_wrapper"
external pysequence_length : pyobject -> int = "PySequence_Length_wrapper"
external pymapping_check : pyobject -> int = "PyMapping_Check_wrapper"
external pymapping_size : pyobject -> int = "PyMapping_Size_wrapper"
external pymapping_length : pyobject -> int = "PyMapping_Length_wrapper"
external pyiter_check : pyobject -> int = "PyIter_Check_wrapper"
#ifdef PYMAJOR3
external pyunicode_getsize : pyobject -> int = "PyUnicode_GetSize_wrapper"
external pyunicode_check : pyobject -> int = "PyUnicode_Check_wrapper"
#endif
external pybytes_check : pyobject -> int = "PyBytes_Check_wrapper"

(* Type19 *)
external pyobject_hasattr : (pyobject * pyobject) -> int = "PyObject_HasAttr_wrapper"
external pyobject_delitem : (pyobject * pyobject) -> int = "PyObject_DelItem_wrapper"
external pydict_delitem : (pyobject * pyobject) -> int = "PyDict_DelItem_wrapper"
external pyerr_givenexceptionmatches : (pyobject * pyobject) -> int = "PyErr_GivenExceptionMatches_wrapper"
external pysequence_count : (pyobject * pyobject) -> int = "PySequence_Count_wrapper"
external pysequence_contains : (pyobject * pyobject) -> int = "PySequence_Contains_wrapper"
external pysequence_in : (pyobject * pyobject) -> int = "PySequence_In_wrapper"
external pysequence_index : (pyobject * pyobject) -> int = "PySequence_Index_wrapper"
external pymapping_haskey : (pyobject * pyobject) -> int = "PyMapping_HasKey_wrapper"

#ifdef PYCAML2
external pyobject_compare : (pyobject * pyobject) -> int = "PyObject_Compare_wrapper"
#endif

(* Type20 *)
external pyobject_richcomparebool : (pyobject * pyobject * int) -> int = "PyObject_RichCompareBool_wrapper"

(* Type21 *)
external pyobject_setattrstring : (pyobject * string * pyobject) -> int = "PyObject_SetAttrString_wrapper"
external pydict_setitemstring : (pyobject * string * pyobject) -> int = "PyDict_SetItemString_wrapper"
external pymapping_setitemstring : (pyobject * string * pyobject) -> int = "PyMapping_SetItemString_wrapper"
external pymodule_addobject : (pyobject * string * pyobject) -> int = "PyModule_AddObject_wrapper"

(* Type22 *)
external pymapping_haskeystring : (pyobject * string) -> int = "PyMapping_HasKeyString_wrapper"
external pyobject_hasattrstring : (pyobject * string) -> int = "PyObject_HasAttrString_wrapper"
external pydict_delitemstring : (pyobject * string) -> int = "PyDict_DelItemString_wrapper"

(* Type23 -- currently not implemented.
external pynumber_coerce : (pyobject * pyobject) -> (pyobject * pyobject) option = "PyNumber_Coerce_wrapper"
external pynumber_coerceex : (pyobject * pyobject) -> (pyobject * pyobject) option = "PyNumber_CoerceEx_wrapper"
*)

(* Type24 *)
external pyobject_setattr : (pyobject * pyobject * pyobject) -> int = "PyObject_SetAttr_wrapper"
external pyobject_setitem : (pyobject * pyobject * pyobject) -> int = "PyObject_SetItem_wrapper"
external pydict_setitem : (pyobject * pyobject * pyobject) -> int = "PyDict_SetItem_wrapper"

(* Type25 *)
external pyobject_hash : pyobject -> int64 = "PyObject_Hash_wrapper"
external pyint_aslong : pyobject -> int64 = "PyInt_AsLong_wrapper"

(* Type26 *)
external pybytes_asstring : pyobject -> string = "PyBytes_AsString_wrapper"
#ifdef PYMAJOR2
external pystring_asstring : pyobject -> string = "PyBytes_AsString_wrapper" (* Legacy support *)
#endif
external pymodule_getfilename : pyobject -> string = "PyModule_GetFilename_wrapper"
external pymodule_getname : pyobject -> string = "PyModule_GetName_wrapper"

(* Type28 *)
external pyimport_addmodule : string -> pyobject = "PyImport_AddModule_wrapper"
external pybytes_fromstring : string -> pyobject = "PyBytes_FromString_wrapper"
#ifdef PYMAJOR2
external pystring_fromstring : string -> pyobject = "PyBytes_FromString_wrapper" (* Legacy support *)
#else
external pystring_fromstring : string -> pyobject = "PyUnicode_FromString_wrapper" (* Legacy support *)
#endif
external pymodule_new : string -> pyobject = "PyModule_New_wrapper"
external pyimport_importmodule : string -> pyobject = "PyImport_ImportModule_wrapper"

(* Type29 *)
external pydict_new : unit -> pyobject = "PyDict_New_wrapper"
external pyerr_occurred : unit -> pyobject = "PyErr_Occurred_wrapper"
external pyimport_getmoduledict : unit -> pyobject = "PyImport_GetModuleDict_wrapper"
external pyeval_getbuiltins : unit -> pyobject = "PyEval_GetBuiltins_wrapper"
external pyeval_getglobals : unit -> pyobject = "PyEval_GetGlobals_wrapper"
external pyeval_getlocals : unit -> pyobject = "PyEval_GetLocals_wrapper"
(* external pyeval_getframe : unit -> pyobject = "PyEval_GetFrame_wrapper"  -- FIX: see note in stubs. *)

(* Type30 *)
external pydict_clear : pyobject -> unit = "PyDict_Clear_wrapper"
external pyerr_setnone : pyobject -> unit = "PyErr_SetNone_wrapper"

(* Type31  -- currently not implemented.
external pydict_next : (pyobject * int) -> (pyobject * pyobject * int) option = "PyDict_Next_wrapper"
*)

(* Type34 *)
external pyint_fromlong : int64 -> pyobject = "PyInt_FromLong_wrapper"

(* Type35 *)
#ifdef PYMAJOR2
external pyint_getmax : unit -> int64 = "PyInt_GetMax_wrapper"
#endif
external pyimport_getmagicnumber : unit -> int64 = "PyImport_GetMagicNumber_wrapper"

(* Type36 *)
external pyfloat_fromdouble : float -> pyobject = "PyFloat_FromDouble_wrapper"

(* Type37 *)
external pyfloat_asdouble : pyobject -> float = "PyFloat_AsDouble_wrapper"

(* Type39 *)
external pytuple_new : int -> pyobject = "PyTuple_New_wrapper"

(* Type40 *)
external pysequence_inplacerepeat : (pyobject * int) -> pyobject = "PySequence_InPlaceRepeat_wrapper"
external pytuple_getitem : (pyobject * int) -> pyobject = "PyTuple_GetItem_wrapper"
external pysequence_repeat : (pyobject * int) -> pyobject = "PySequence_Repeat_wrapper"
external pysequence_getitem : (pyobject * int) -> pyobject = "PySequence_GetItem_wrapper"

(* Type40b *)
external pysequence_delitem : (pyobject * pyobject * int) -> int = "PySequence_DelItem_wrapper"

(* Type41 *)
external pytuple_setitem : (pyobject * int * pyobject) -> int = "PyTuple_SetItem_wrapper"
external pysequence_setitem : (pyobject * int * pyobject) -> int = "PySequence_SetItem_wrapper"

(* Type42 *)
external pyslice_new : (pyobject * pyobject * pyobject) -> pyobject = "PySlice_New_wrapper"
external pyclass_new : (pyobject * pyobject * pyobject) -> pyobject = "PyClass_New_wrapper"
#ifdef PYMAJOR2
external pyinstance_new : (pyobject * pyobject * pyobject) -> pyobject = "PyInstance_New_wrapper"
#endif
external pymethod_new : (pyobject * pyobject * pyobject) -> pyobject = "PyMethod_New_wrapper"
external pyeval_callobjectwithkeywords : (pyobject * pyobject * pyobject) -> pyobject = "PyEval_CallObjectWithKeywords_wrapper"
external pynumber_power : (pyobject * pyobject * pyobject) -> pyobject = "PyNumber_Power_wrapper"
external pynumber_inplacepower : (pyobject * pyobject * pyobject) -> pyobject = "PyNumber_InPlacePower_wrapper"

(* Type43 *)
(* external pyslice_getindices : (pyobject * int) -> (int * int * int) option = "PySlice_GetIndices_wrapper" <-- Currently not supported *)

(* Type45 *)
external pyerr_setobject : (pyobject * pyobject) -> unit = "PyErr_SetObject_wrapper"

(* Type46 *)
external pyerr_setstring : (pyobject * string) -> unit = "PyErr_SetString_wrapper"

(* Type47 *)
external pyerr_fetch : (pyobject * pyobject * pyobject) -> (pyobject * pyobject * pyobject) = "PyErr_Fetch_wrapper"
external pyerr_normalizeexception : (pyobject * pyobject * pyobject) -> (pyobject * pyobject * pyobject) = "PyErr_NormalizeException_wrapper"

(* Type48 *)
external pyerr_restore : (pyobject * pyobject * pyobject) -> unit = "PyErr_Restore_wrapper"

(* Type49 *)
external pyimport_execcodemodule : (pyobject * string) -> pyobject = "PyImport_ExecCodeModule_wrapper"

(* Type50 *)
external pyimport_execcodemoduleex : (string * pyobject * string) -> pyobject = "PyImport_ExecCodeModuleEx_wrapper"

(* Type51 *)
external pyimport_importmoduleex : (string * pyobject * pyobject * pyobject) -> pyobject = "PyImport_ImportModuleEx_wrapper"

(* Type52 *)
external pybytes_asstringandsize : pyobject -> string = "PyBytes_AsStringAndSize_wrapper"
external pystring_asstringandsize : pyobject -> string = "PyBytes_AsStringAndSize_wrapper" (* Legacy support *)
external pyobject_ascharbuffer : pyobject -> string = "PyObject_AsCharBuffer_wrapper"
external pyobject_asreadbuffer : pyobject -> string = "PyObject_AsReadBuffer_wrapper"
external pyobject_aswritebuffer : pyobject -> string = "PyObject_AsWriteBuffer_wrapper"

(* Type53 *)
external pysequence_setslice : (pyobject * int * int * pyobject) -> int = "PySequence_SetSlice_wrapper"

(* Type54 *)
external pysequence_delslice : (pyobject * int * int) -> int = "PySequence_DelSlice_wrapper"

(* TypeUTF8Decoder *)
#ifdef PYMAJOR3
external pyunicode_decodeutf8 : (string * string option) -> pyobject = "PyUnicode_DecodeUTF8_wrapper"
external pyunicode_asencodedstring : (pyobject * string * string) -> pyobject = "PyUnicode_AsEncodedString_wrapper"
#endif

(*-----------------------------------------------------------------------*)

external pytuple_fromarray : pyobject array -> pyobject = "pytuple_fromarray"
let pytuple_fromsingle elt = pytuple_fromarray [| elt |] 
let pytuple_empty = pytuple_fromarray [| |] 
external pytuple2 : (pyobject * pyobject) -> pyobject = "pytuple_fromarray"
external pytuple3 : (pyobject * pyobject * pyobject) -> pyobject = "pytuple_fromarray"
external pytuple4 : (pyobject * pyobject * pyobject * pyobject) -> pyobject = "pytuple_fromarray"
external pytuple5 : (pyobject * pyobject * pyobject * pyobject * pyobject) -> pyobject = "pytuple_fromarray"

let pyint_fromint i = pyint_fromlong (Int64.of_int i)
let pyint_asint obj = Int64.to_int (pyint_aslong obj)
    
external pytuple_toarray : pyobject -> pyobject array = "pytuple_toarray"
    
external pywrap_closure : (pyobject -> pyobject) -> pyobject = "pywrap_closure"

external pywrap_value : 'a -> pyobject = "pywrapvalue"
external pywrap_value_pill : 'a -> pyobject = "pywrapvalue_pill"

external pyunwrap_value : pyobject -> 'a = "pyunwrapvalue"
(* ^ Note: this will just unwrap and not care about pills! *)

(* -- T.F. extensions -- *)

let py_repr x = pybytes_asstring (pyobject_repr x)

external pywrap_closure_docstring :
  string -> (pyobject -> pyobject) -> pyobject = "pywrap_closure_docstring"


external pylist_fromarray : pyobject array -> pyobject = "pylist_fromarray"
external pylist_toarray : pyobject -> pyobject array = "pylist_toarray"
external pylist_set: pyobject -> int -> pyobject -> unit = "pylist_set"
external pylist_get: pyobject -> int -> pyobject = "pylist_get"

external python_prompt: unit -> unit = "pycaml_prompt"

external pyrefcount: pyobject -> int = "pyrefcount"

let pycheck_not_null v =
  if v = pynull () then
    failwith "pycheck_not_null";
  v

let pycheck_int i =
  if i = -1 then
    failwith "pycheck_int"

let pyclass_init classname parents fields methods =
  let dict = pycheck_not_null (pydict_new ()) in
  let add_field (name, value) =
    pycheck_int (pydict_setitemstring (dict, name, value)) in
  List.iter add_field fields;
#ifdef PYMAJOR2
  let c = pycheck_not_null (pyclass_new (parents, dict, classname)) in
  let add_method (name, closure) =
    let m = pycheck_not_null (pymethod_new (pywrap_closure closure, pynull (), c)) in
    add_field (name, m) in
  List.iter add_method methods;
  c
#else
  let add_method (name, closure) =
    let m = pycheck_not_null (pyinstancemethod_new (pywrap_closure closure)) in
    add_field (name, m) in
  List.iter add_method methods;
  pycheck_not_null (pyclass_new (parents, dict, classname))
#endif

#ifdef PYMAJOR3
let pystring_asstring obj =
  if pyunicode_check obj != 0 then
    let bytes = pyunicode_asutf8string obj in
    if bytes = pynull () then
      failwith "pystring_asstring: encoding failure";
    pybytes_asstring bytes
  else if pybytes_check obj != 0 then
    pybytes_asstring obj
  else
    failwith "pystring_asstring: not a string"
#endif

let pystring_check obj =
#ifdef PYMAJOR2
  pybytes_check obj != 0
#else
  pyunicode_check obj != 0 || pybytes_check obj != 0
#endif

let _py_profile_hash = ((Hashtbl.create 100):(string, float array) Hashtbl.t)
let _py_profiling_active=ref false
(* The profiling hash and switch are strictly internal! *)

let py_activate_profiling () =
  let z = !_py_profiling_active in
  let () = _py_profiling_active:=true in
    z

let py_deactivate_profiling () =
  let z = !_py_profiling_active in
  let () = _py_profiling_active:=false in
    z


let py_profile_reset () = Hashtbl.clear _py_profile_hash

(* Needed below. Actually, we also have this in our "snippets" module,
   but pycaml should not depend on anything not in the standard ocaml
   distribution.
*)

let __hashtbl_arbitrary_element ht =
  let have_it = ref None in
  let () =
    try
      Hashtbl.iter
	(fun k v ->
	   begin
	     have_it := Some (k,v);
	     raise Not_found; (* abused as a dummy exception here *)
	   end)
	ht
    with
      | Not_found -> ()
  in
    !have_it

let __map_hashtbl_to_array ?sorter mapper ht =
  let nr_entries = Hashtbl.length ht in
    if nr_entries = 0 
    then [||]
    else
      let opt_kv = __hashtbl_arbitrary_element ht in
      let result =
	match opt_kv with
	  | None -> failwith "Impossible!"
	  | Some (k,v) -> Array.make nr_entries (mapper k v)
      in
      let ref_index = ref 0 in
      let () = Hashtbl.iter
	(fun k v ->
	   begin
	     result.(!ref_index) <- mapper k v;
	     ref_index := (1+ !ref_index);
	   end) ht
      in
	match sorter with
	  | Some s ->
	      let () = Array.sort s result in result
	  | _ -> result


let py_profile_report () =
  __map_hashtbl_to_array
    ~sorter:(fun (_,time_a,_) (_,time_b,_) -> compare time_b time_a) (* sort by time consumed *)
    (fun name time_and_calls -> (name,time_and_calls.(0),time_and_calls.(1)))
    _py_profile_hash


let pytype_name pt =
  match pt with
  | TupleType -> "Python-Tuple"
  | BytesType -> "Python-Bytes"
  | UnicodeType -> "Python-Unicode"
  | BoolType -> "Python-Bool"
  | IntType -> "Python-Int"
  | FloatType -> "Python-Float"
  | ListType -> "Python-List"
  | NoneType -> "Python-None"
  | CallableType -> "Python-Callable"
  | ModuleType -> "Python-Module"
  | ClassType -> "Python-Class"
  | NullType -> "Python-Null"
  | TypeType -> "Python-Type"
  | DictType -> "Python-Dict"
  | CamlpillType -> "Python-Camlpill"
  | OtherType -> "Python-Other"
  | EitherStringType -> "Python-EitherString"
  | CamlpillSubtype sym -> "Python-Camlpill-" ^ sym
  | AnyType -> "Python-Any"

let set_python_argv argv =
  let py_mod_sys_dict = pymodule_getdict (pyimport_importmodule "sys") in
  let _ = pydict_setitem(py_mod_sys_dict,pybytes_fromstring "argv",
			 pylist_fromarray (Array.map pybytes_fromstring argv))
  in ()

let python_eval str =
  pyrun_simplestring str

let python_load filename =
  ignore(python_eval (Printf.sprintf "execfile(\"%s\")" filename))

let python () =
  let () = python_prompt() in 0

let ipython () =
  pyrun_simplestring
(* This version starts ipython with its own name space -- one
   can't see any objects created with earlier pycaml.* commands.

   I have added a replacement below. (Delete this comment later if 
   this generally works. (HF 23/09/05)
 *)
(* "import IPython       
import sys
sys.argv=['/usr/local/bin/ipython']
IPython.Shell.start().mainloop()
";;*)
"from IPython.Shell import IPShellEmbed
ipshell = IPShellEmbed(argv=[''])
ipshell() # this call anywhere in your program will start IPython
";;


(* Note: we prepare Python in such a way that we have a module ocaml
   within which the OCaml side will publish its own functions.

   Convention: We will register some examples in ocaml.*
   These will have names that start with "example_".
*)

let _ = py_initialize () (* Note that this must happen that early... *)

let _py_sys_modules = pyimport_getmoduledict ()
let _py_mod_ocaml = pymodule_new "ocaml"
let _py_mod_ocaml_dict = pymodule_getdict _py_mod_ocaml

(* Get the last value that was computed in the interactive REPL *)
let python_last_value () =
  let main = pyimport_importmodule "__main__" in
  let main_dict = pymodule_getdict main in
  let builtins = pydict_getitem (main_dict, pybytes_fromstring "__builtins__") in
  let builtins_dict = pymodule_getdict builtins in
  let pyname_lastvalue = pybytes_fromstring "_" in
    pydict_getitem (builtins_dict, pyname_lastvalue)

let py_is_true x = pyobject_istrue x <> 0 (* pyobject_istrue has return type int - which is quite insane... *)

let register_for_python stuff =
  Array.iter
    (fun (python_name, value) ->
       ignore(pydict_setitemstring
		(_py_mod_ocaml_dict,
		 python_name, value)))
    stuff

let register_pre_functions_for_python stuff =
  Array.iter
    (fun (python_name, pre_fun) ->
       ignore(pydict_setitemstring
		(_py_mod_ocaml_dict,
		 python_name, pre_fun python_name)))
    stuff


let float_array_to_python farr =
  pylist_fromarray (Array.map pyfloat_fromdouble farr)

let int_array_to_python iarr =
  pylist_fromarray (Array.map pyint_fromint iarr)


let py_float_tensor ?(init=(fun _ -> 0.0)) index_ranges =
  let nr_indices = Array.length index_ranges in
  let v_indices = Array.make nr_indices 0 in
    if nr_indices = 0
    then (pyfloat_fromdouble (init v_indices), fun _ -> failwith "Cannot set rank-0 python float tensor!")
    else
      let rec build pos =
	let range = index_ranges.(pos) in
	  if pos = nr_indices-1
	  then
	    pylist_fromarray 
	      (Array.init range
		 (fun ix_here ->
		    let () = v_indices.(pos) <- ix_here in
		      pyfloat_fromdouble (init v_indices)))
	  else
	    pylist_fromarray
	      (Array.init range
		 (fun ix_here ->
		    let () = v_indices.(pos) <- ix_here in
		      build (1+pos)))
      in
      let structure = build 0
      in
      let setter indices value =
	let rec walk sub_structure pos =
	  if pos = nr_indices-1
	  then pylist_set sub_structure indices.(pos) value
	  else walk (pylist_get sub_structure indices.(pos)) (1+pos)
	in walk structure 0
      in (structure,setter)



let py_homogeneous_list_as_array
    ?error_label
    ?length
    type_name type_checker unwrapper
    py_obj_arr
    =
  let array_first_not_to_satisfy p arr =
    let len = Array.length arr in
    let rec walk n =
      if n = len then (-1)
      else if p (arr.(n)) then walk (n+1)
      else n
    in walk 0
  in
  let the_error_label =
    match error_label with
      | None -> ""
      | Some x -> Printf.sprintf "%s: " x
  in
    if pytype py_obj_arr <> ListType then 
      raise 
	    (Pycaml_exn
	       (Pyerr_TypeError,
	        Printf.sprintf "%sExpected list, got: %s (%s)" the_error_label
	          (pytype_name (pytype py_obj_arr))
	          (py_repr py_obj_arr)
	       ))
    else
      let obj_arr = pylist_toarray py_obj_arr in
        (* Doing the length check is slightly tricky... *)
      let () =
        match length with
	      | None -> ()
	      | Some len ->
	          if Array.length obj_arr <> len then
	            raise 
		          (Pycaml_exn
		             (Pyerr_TypeError,
		              Printf.sprintf "%sExpected list of length %d, got length: %d"
		                the_error_label
		                len (Array.length obj_arr)))
      in
      let first_bad = array_first_not_to_satisfy type_checker obj_arr in
        if first_bad <> (-1) then
	      raise 
            (Pycaml_exn
               (Pyerr_TypeError,
	            Printf.sprintf "%sExpected homogeneous list of %s. Entry %d is of type %s (%s)!"
		          the_error_label
		          type_name
		          (1 + first_bad)
		          (pytype_name (pytype obj_arr.(first_bad)))
		          (py_repr obj_arr.(first_bad))))
        else
	      Array.map unwrapper obj_arr



let py_float_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "float" (fun x -> pytype x = FloatType) pyfloat_asdouble arr

let py_int_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "int" (fun x -> pytype x = IntType) pyint_asint arr

let py_number_list_as_float_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "number" 
    (fun x -> let ty = pytype x in ty = FloatType || ty = IntType)
    (fun x -> if pytype x = FloatType then pyfloat_asdouble x else float_of_int (pyint_asint x))
    arr


let py_string_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "string" (fun x -> pytype x = BytesType) pybytes_asstring arr

let py_list_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "<Python List>" (fun x -> pytype x = ListType) (fun x -> x) arr

let py_list_list_as_array2 ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "<Python List>" (fun x -> pytype x = ListType) pylist_toarray arr


let py_float_list_list_as_array ?error_label ?length_outer ?length_inner arr =
  let arr_outer = py_list_list_as_array ?error_label ?length:length_outer arr in
  Array.map (py_float_list_as_array ?error_label ?length:length_inner) arr_outer

let py_number_list_list_as_float_array ?error_label ?length_outer ?length_inner arr =
  let arr_outer = py_list_list_as_array ?error_label ?length:length_outer arr in
  Array.map (py_number_list_as_float_array ?error_label ?length:length_inner) arr_outer


let py_int_list_list_as_array ?error_label ?length_outer ?length_inner arr =
  let arr_outer = py_list_list_as_array ?error_label ?length:length_outer arr in
  Array.map (py_int_list_as_array ?error_label ?length:length_inner) arr_outer

let py_string_list_list_as_array ?error_label ?length_outer ?length_inner arr =
  let arr_outer = py_list_list_as_array ?error_label ?length:length_outer arr in
  Array.map (py_string_list_as_array ?error_label ?length:length_inner) arr_outer


(* When registering an OCaml function for Python, we should include
   quite some extra run-time type checks. Rationale: Python users expect
   programming errors to produce error messages, not crash the system.

   So, not including these checks would violate the user's expectations.

   The function below helps us wrapping up an OCaml function for
   calling from python with arg conversion, typechecks, and optional
   extra checks for individual parameters. Furthermore, we make sure
   that OCaml exceptions are passed on properly to the Python level.
*)

(* Actually, this turned out to be not quite as useful as I hoped initially... *)
let _caml_debug_exceptions () =
  let ocamlrunparam =
    try
      Unix.getenv "OCAMLRUNPARAM"
    with
      | Not_found ->
          try
            Unix.getenv "CAMLRUNPARAM"
          with
            | Not_found -> ""
  in
  let pieces = Str.split (Str.regexp ",") ocamlrunparam in
    try 
      let _ = List.find (fun p -> p="b") pieces in
        true
    with
      | Not_found -> false

let type_mismatch_exception type_wanted type_here pos exn_name =
  Pycaml_exn
	(Pyerr_TypeError,
	 (Printf.sprintf "Argument %d: Type wanted: %s -- Type provided: %s%s."
		(pos + 1) (* Humans like to start counting at 1. *)
		(pytype_name type_wanted)
		(pytype_name type_here)
		exn_name))

#ifdef PYMAJOR2
let sym_match a b =
  a == b                      (* Note the == for physical equality. *)
#else
let sym_match a b =
  a = b
#endif

let pill_type_mismatch_exception ?position ?exn_name wanted gotten =
  let arg_no =
    match position with
      | None -> ""
      | Some p -> "Argument %d: "
  in
  let en =
    match exn_name with
      | None -> ""
      | Some n -> n
  in
  Pycaml_exn (Pyerr_TypeError, 
			  arg_no ^ (Printf.sprintf "Python-Ocaml Pill Type mismatch: wanted: '%s' - got: '%s'" wanted gotten) ^ en)

let ocamlpill_type_of pill =
  if pytype pill <> CamlpillType then
    (* may happen if we e.g. look at list entries *)
    raise (Pycaml_exn(Pyerr_TypeError,
			 Printf.sprintf "Expected OCaml pill - got: %s (%s)"
			   (pytype_name (pytype pill))
			   (py_repr pill)))
  else
    let (type_name, _) = pyunwrap_value pill in
      type_name

let check_pill_type ?position ?exn_name wanted pill =
  let gotten = ocamlpill_type_of pill in
    if not (sym_match gotten wanted) then
      raise (pill_type_mismatch_exception ?position:position ?exn_name:exn_name wanted gotten)

let unpythonizing_function
    ?name                        (* Will be used in error reporting *)
    ?(catch_weird_exceptions = true)
    ?extra_guards (* An array of functions mapping pyobject -> failure_string option *)
    ?(expect_tuple = false)
    wanted_types
    function_body =
  let exn_name =
    match name with
      | None -> ""
      | Some s -> Printf.sprintf " (%s)" s
  in
  let work_fun python_args =
    let body () =
      let () =
        if expect_tuple && pytype python_args <> TupleType then
		  (* ^ This should never happen! *)
          raise (Pycaml_exn(Pyerr_TypeError, Printf.sprintf "Weird situation: Non-Tuple function args encountered.%s" exn_name))
      in
      let nr_args_given =
        if expect_tuple then
          pytuple_size python_args
        else
          1
      in
      let nr_args_wanted = Array.length wanted_types in
      let () =
        if nr_args_given <> nr_args_wanted then
          raise (Pycaml_exn(Pyerr_IndexError,
				            (Printf.sprintf "Args given: %d Wanted: %d%s" nr_args_given nr_args_wanted exn_name)))
      in
      let arr_args =
        if expect_tuple then
          pytuple_toarray python_args
        else
          [| python_args |]
      in
      let rec check_types pos =
	    if pos = nr_args_given then
          function_body arr_args
	    else
	      let type_here = pytype arr_args.(pos) in
	      let type_wanted = wanted_types.(pos) in
	      let () =
            match type_wanted with
              | AnyType ->
                  ()
              | EitherStringType ->
                  if type_here <> UnicodeType && type_here <> BytesType then
                    raise (type_mismatch_exception type_wanted type_here pos exn_name)
              | CamlpillSubtype sym ->
                  check_pill_type ~position:pos ~exn_name:exn_name sym arr_args.(pos)
              | _ ->
                  if type_here <> type_wanted then
		            raise (type_mismatch_exception type_wanted type_here pos exn_name)
	      in
	        (* Okay, typecheck succeeded.  Now, if extra guards have
	           been provided, try those. *)
	        match extra_guards with
	          | None -> check_types (pos + 1)
	          | Some guards ->
		          let guard = guards.(pos) in
		          let guard_error = guard arr_args.(pos) in
		            match guard_error with
		              | None -> check_types (pos+1)
		              | Some msg ->
			              raise (Pycaml_exn
				                   (Pyerr_TypeError,
				                    (Printf.sprintf "Check for argument %d failed: %s%s"
				                       (pos + 1)
				                       msg
				                       exn_name)))
      in
	    check_types 0
    in
      body ()
  in
    work_fun

#ifdef PYMAJOR3
(* OCaml string encoded in UTF-8 --> Python 3 string type (= Python 2 unicode type) *)
let pythonize_string s =
  pyunicode_decodeutf8(s, None)

(* Python 3 string or bytes type --> OCaml string encoded in UTF-8 *)
let unpythonize_string =
  unpythonizing_function
    [| EitherStringType |]
    begin
      fun py_args ->
        let s = py_args.(0) in
        let t = pytype s in
          match t with
            | UnicodeType -> pybytes_asstringandsize (pyunicode_asutf8string s)
            | BytesType   -> pybytes_asstringandsize s
            | _ -> assert false
    end
#endif

(* FIX: Maybe rewrite this as an unpythonizing_function. *)
let python_interfaced_function
    ?name     (* Will be used in both profiling and error reporting *)
    ?(catch_weird_exceptions = true)
    ?doc
    ?extra_guards (* An array of functions mapping pyobject -> failure_string option *)
    wanted_types
    function_body =
  let wrapper =
    match doc with
      | None -> pywrap_closure
      | Some docstring -> (pywrap_closure_docstring docstring)
  in
  let exn_name =
    match name with
      | None -> ""
      | Some s -> Printf.sprintf " (%s)" s
  in
  let work_fun python_args =
    let body () =
      let () =
        if pytype python_args <> TupleType then
		  (* ^ This should never happen! *)
          raise (Pycaml_exn(Pyerr_TypeError, Printf.sprintf "Weird situation: Non-Tuple function args encountered.%s" exn_name))
      in
      let nr_args_given = pytuple_size python_args in
      let nr_args_wanted = Array.length wanted_types in
      let () =
        if nr_args_given <> nr_args_wanted then
          raise (Pycaml_exn(Pyerr_IndexError,
				            (Printf.sprintf "Args given: %d Wanted: %d%s" nr_args_given nr_args_wanted exn_name)))
      in
      let arr_args = pytuple_toarray python_args in
      let rec check_types pos =
	    if pos = nr_args_given then
          function_body arr_args
	    else
	      let type_here = pytype arr_args.(pos) in
	      let type_wanted = wanted_types.(pos) in
	      let () =
            match type_wanted with
              | AnyType ->
                  ()
              | EitherStringType ->
                  if type_here <> UnicodeType && type_here <> BytesType then
                    raise (type_mismatch_exception type_wanted type_here pos exn_name)
              | CamlpillSubtype sym ->
                  check_pill_type ~position:pos ~exn_name:exn_name sym arr_args.(pos)
              | _ ->
                  if type_here <> type_wanted then
		            raise (type_mismatch_exception type_wanted type_here pos exn_name)
	      in
	        (* Okay, typecheck succeeded.  Now, if extra guards have
	           been provided, try those. *)
	        match extra_guards with
	          | None -> check_types (pos + 1)
	          | Some guards ->
		          let guard = guards.(pos) in
		          let guard_error = guard arr_args.(pos) in
		            match guard_error with
		              | None -> check_types (pos+1)
		              | Some msg ->
			              raise (Pycaml_exn
				                   (Pyerr_TypeError,
				                    (Printf.sprintf "Check for argument %d failed: %s%s"
				                       (pos + 1)
				                       msg
				                       exn_name)))
      in
	    check_types 0
    in
      if _caml_debug_exceptions () then
        body () 
      else
	    try
	      body ()
	    with 
	      | Pycaml_exn (errtype, msg) ->
	          pycaml_seterror errtype (Printf.sprintf "%s%s" msg exn_name); pynull()
	      | Not_found ->
	          pycaml_seterror Pyerr_LookupError (Printf.sprintf "OCaml exception 'Not_found'%s" exn_name);
	          pynull()
	      | Division_by_zero ->
	          pycaml_seterror Pyerr_ZeroDivisionError (Printf.sprintf "OCaml exception 'Division_by_zero'%s" exn_name);
	          pynull()
	      | Failure s ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Failure: %s'%s" s exn_name);
	          pynull ()
	      | Invalid_argument s ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Invalid_argument: %s'%s" s exn_name);
	          pynull()
	      | Out_of_memory ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Out_of_memory'%s" exn_name);
	          pynull()
	      | Stack_overflow ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Stack_overflow'%s" exn_name);
	          pynull()
	      | Sys_error s ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Sys_error %s'%s" s exn_name);
	          pynull()
	      | End_of_file ->
	          pycaml_seterror Pyerr_IOError (Printf.sprintf "OCaml exception 'End_of_file'%s" exn_name);
	          pynull()
	      | Match_failure (filename,line,column) ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Match_failure file=%s line=%d(c. %d)'%s" filename line column exn_name);
	          pynull()
	      | Assert_failure (filename,line,column) ->
	          pycaml_seterror Pyerr_StandardError (Printf.sprintf "OCaml exception 'Assert_failure file=%s line=%d(c. %d)'%s" filename line column exn_name);
	          pynull()
		        
	      | something_else ->
	          if catch_weird_exceptions then
		        begin
		          pycaml_seterror 
		            Pyerr_StandardError 
		            (Printf.sprintf "OCaml weird low-level exception (not resolved any further)%s" exn_name);
		          pynull()
		        end
	          else raise something_else
  in
    match name with
      | None -> wrapper work_fun
      | Some pname ->
	      let profiling_work_fun args =
	        if not(!_py_profiling_active)
	        then work_fun args
	        else
	          let t0 = Unix.gettimeofday () in
	          let result = work_fun args in
	          let t1 = Unix.gettimeofday () in
	          let old_time_and_calls =
		        try Hashtbl.find _py_profile_hash pname
		        with | Not_found ->
		          let x = [|0.0;0.0|] in
		          let () = Hashtbl.add _py_profile_hash pname x in
		            x
	          in
		        begin
		          old_time_and_calls.(0) <- old_time_and_calls.(0) +.(t1-.t0);
		          old_time_and_calls.(1) <- old_time_and_calls.(1) +.1.0;
		          result
		        end
	      in
            wrapper profiling_work_fun

(* python_interfaced_function takes a name argument,
   and indeed it has to, because we want to be able to profile-register
   also anonymously generated functions.

   On the other hand, we eventually register many of these functions
   under some particular name in the "ocaml" python module. So, it
   would be nice to be able to just use the functions' name in the
   ocaml package as its profiling name. So, what we need is a way to
   "pre-register" a function, i.e.:
*)

let python_pre_interfaced_function
    ?(catch_weird_exceptions=true)
    ?doc
    ?extra_guards (* An array of functions mapping pyobject -> failure_string option *)
    wanted_types function_body =
  fun name ->
    python_interfaced_function
      ~name ?doc ?extra_guards wanted_types function_body


(* pywrap_value will wrap up OCaml values for Python in such a way
   that Python can store this in containers, pass it around, and
   eventually hand it back to OCaml.

   As python is not statically typed in an OCaml-compatible way, this
   means that Python may hand back stuff to OCaml which is not of the
   expected type, without OCaml noticing.

   Again, Python users expect to get errors - not crashes - from
   programs, so we have to guard against this. How? By implementing
   some own primitive dynamic type system for python-wrapped OCaml
   values.

   Note: the conventions here are TF's own. Pycaml users need not use
   them, but they might be well advised to do so nevertheless.

   Design decisions:

   * OCaml will only provide opaque values of a manageable small
   number of types to Python.

   * We want to be able to register new types for Python encapsulation
   at runtime.
   
   * We abuse OCaml strings as type tag symbols, which have been
   uniq'd through an identity hash map.  (B.S.: In the PyCapsule
   implementation, we use the capsule name field and don't uniq it.)

   * The only values which are wrapped up for python are of the structure
   (type_tag_string,ref value)

   The ref in the second tuple slot may be a paranoid spurious indirection,
   but will ensure that we will always have a non-immediate in the second
   slot.

   * Note: If we use this in conjunction with some other Python foreign-function
   extension, then it might well happen that a mixup of opaque objects from us
   and that other extension produces a crash. We MIGHT be able to fix this if we
   extend the Python interpreter at low level with a new type for our purposes,
   and do not use PyCObject anymore.

   * It might sound ridiculous, but I consider it a major problem that
   so far, I could not come up with a nice, catchy name for the
   concept of python values that wrap up python-opaque ocaml
   values. Evidently, it is very desirable to have one.

   After a few failed attempts, the least bad name I can come up with is "ocamlpill".

   (B.S.: Personally, I have always liked the name, and now the name
   is especially good, because an ocamlpill has become a sort of
   "capsule", PyCapsule.)

   * ocamlpill type names are unique and considered as global. It is desirable
   to provide a Python function that returns this name, so those names should not be 
   considered "for internal use only".

*)

(* Mapping name => Unique name. Type names are e.g. "Mesh.mesh". *)
let _known_ocamlpill_types = ((Hashtbl.create 10):((string, string) Hashtbl.t))

let _ocamlpill_type_sym ocamlpill_type_name =
  try 
    Hashtbl.find _known_ocamlpill_types ocamlpill_type_name
  with
    | Not_found ->
	failwith
	  (Printf.sprintf "Used ocamlpill_type '%s' without register_ocamlpill_type(\"%s\")"
	     ocamlpill_type_name ocamlpill_type_name)

let register_ocamlpill_types type_names =
  Array.iter
    (fun type_name ->
      if Hashtbl.mem _known_ocamlpill_types type_name
      then () (* -- already known *)
      else 
	Hashtbl.add _known_ocamlpill_types type_name type_name)
    type_names

let make_pill_wrapping ocamlpill_type_name prototypical_object =
  let ocamlpill_type_sym = _ocamlpill_type_sym ocamlpill_type_name in
  let wrapper x =
    pywrap_value_pill
      (ocamlpill_type_sym,
       if false then
         prototypical_object (* Type inference uses this object's type. *)
       else
         x)
  in
  let unwrapper py_value =
    let (ocamlpill_type_sym_provided, xval) = pyunwrap_value py_value in
      if not (sym_match ocamlpill_type_sym_provided ocamlpill_type_sym) then
	    raise (pill_type_mismatch_exception ocamlpill_type_sym ocamlpill_type_sym_provided)
      else
	    let _ =
	      if false then
            prototypical_object (* Type inference uses this object's type. *)
	      else
            xval
	    in
          xval
  in
    (wrapper, unwrapper)

let make_ocamlpill_wrapper_unwrapper = make_pill_wrapping

let ocamlpill_hard_unwrap pill =
  let (_, x) = pyunwrap_value pill in
    x


(* There are situations where we want to provide optional python
   arguments. For the low-level interface, we use the convention to use a
   0-element or 1-element list. In most cases, this low-level convention
   will be wrapped up at a higher level python-wise so that the user of
   a module just sees an optional argument.
   
   This is somewhat tricky: we want to treat optional string/float/int
   args on the same footing as pill args. How do we do this? By a bit
   of combinatorical magic! The proper abstraction is to use a
   continuation function which will receive the unwrapped optional value,
   and an applicator, which may be some ocamlpill_applicator_for_xyz,
   or something like (fun py_x f -> f (pyint_asint py_x)), or just
   evaluate_at.

   Problem here: pyint_asint should also do an extra python typecheck
   in the example above! Hence, we need certain special applicators
   for python types...
*)

let py_optionally unwrapper py_value =
  (* Usually, the optional thingy comes in as an argument,
     and is checked in python_interfaced_function.
     This is not necessarily so, as optional thingies may be
     part of larger data structures, so we better check the type *)
  if pytype py_value != ListType
  then
    raise
      (Pycaml_exn
	 (Pyerr_TypeError, 
	  Printf.sprintf "Expected optional argument to be provided as an empty or 1-element list. Got: %s (%s)"
	    (pytype_name (pytype py_value)) (py_repr py_value)))
  else
    let a = pylist_toarray py_value in
    if Array.length a > 1 then
      raise
	(Pycaml_exn
	   (Pyerr_TypeError, 
	    Printf.sprintf "Expected optional argument to be provided as an empty or 1-element list. Got: %d-element list (%s)."
	      (Array.length a) (py_repr py_value)))
    else
      if Array.length a = 0 then None else Some (unwrapper a.(0))

(* There are a few functions which we may want to use in conjunction with py_optionally to get
   optional integers, optional floats, etc., and not just optional pills
*)

let guarded_pyint_asint x =
  if pytype x <> IntType
  then raise
    (Pycaml_exn(Pyerr_TypeError,
		Printf.sprintf "Wanted: int, got: %s (%s)" (pytype_name (pytype x)) (py_repr x)))
  else pyint_asint x

let guarded_pyfloat_asfloat x =
  if pytype x <> FloatType
  then raise
    (Pycaml_exn(Pyerr_TypeError,
		Printf.sprintf "Wanted: float, got: %s (%s)" (pytype_name (pytype x)) (py_repr x)))
  else pyfloat_asdouble x

let guarded_pynumber_asfloat x =
  match pytype x with
    | FloatType -> pyfloat_asdouble x
    | IntType -> float_of_int (pyint_asint x)
    | _ ->
	raise
	  (Pycaml_exn(Pyerr_TypeError,
		      Printf.sprintf "Wanted: number, got: %s (%s)" (pytype_name (pytype x)) (py_repr x)))

let guarded_pybytes_asstring x =
  if pytype x <> BytesType
  then raise
    (Pycaml_exn(Pyerr_TypeError,
		Printf.sprintf "Wanted: string, got: %s (%s)" (pytype_name (pytype x)) (py_repr x)))
  else pybytes_asstring x

let guarded_pylist_toarray x =
  if pytype x <> ListType
  then raise
    (Pycaml_exn(Pyerr_TypeError,
		Printf.sprintf "Wanted: list, got: %s (%s)" (pytype_name (pytype x)) (py_repr x)))
  else pylist_toarray x

let guarded_pytuple_toarray x =
  if pytype x <> TupleType
  then raise
    (Pycaml_exn(Pyerr_TypeError,
		Printf.sprintf "Wanted: tuple, got: %s (%s)" (pytype_name (pytype x)) (py_repr x)))
  else pytuple_toarray x


let pycallable_asfun py =
  if pytype py <> CallableType 
  then
    raise
      (Pycaml_exn
	 (Pyerr_TypeError, 
	  Printf.sprintf "Expected Python Callable - Got: %s (%s)" (pytype_name (pytype py)) (py_repr py)))
  else
    fun (args:(pyobject array)) ->
      pyeval_callobject(py,pytuple_fromarray args)


(* Note: we do not subject the example/low-level python functions we provide here
   to profiling. Maybe we should.
*)

let _py_profiling =
  python_interfaced_function
    ~doc:"Control profiling of python<->ocaml functions. Usage:

ocaml.sys_profiling(\"on\") -> turn on profiling, return previous on/off status (on=true)
ocaml.sys_profiling(\"off\") -> turn off profiling, return previous on/off status (on=true)
ocaml.sys_profiling(\"clear\") -> clear internal profiling tables
ocaml.sys_profiling(\"report\") -> return profiling report

Report format: list of (name,time,nr_calls), sorted by decreasing total time. 
nr_calls is a floatingpoint number to overcome 32-bit integer limitations.
"
    [|BytesType|]
    (fun arr ->
       let s = pybytes_asstring arr.(0) in
	 match s with
	   | "on" ->
	       let z = py_activate_profiling() in
		 if z then py_true () else py_false ()
	   | "off" ->
	       let z = py_activate_profiling() in
		 if z then py_true () else py_false ()
	   | "clear" ->
	       let () = py_profile_reset() in pynone()
	   | "report" ->
	       let r = py_profile_report() in
		 pylist_fromarray
		   (Array.map 
		      (fun (name,time,calls) ->
			 pytuple3 (pybytes_fromstring name,
				   pyfloat_fromdouble time,
				   pyfloat_fromdouble calls))
		      r)
	   | _ -> pynone()
    )

(* We provide a function to python that allows to check the type
   of an opaque OCaml object.
*)

let _py_ocamlpill_type =
  python_interfaced_function [|CamlpillType|]
    (fun arr ->
       let (type_name, _) = pyunwrap_value arr.(0)
       in pybytes_fromstring type_name)

(* Note: using ocaml.sys_python() will return
   the last value computed interactively.
 *)
let _py_python =
  python_interfaced_function [||]
    (fun arr ->
      let _ = python() in
      python_last_value())

let _py_ipython =
  python_interfaced_function [||]
    (fun arr ->
      let _ = ipython() in
      python_last_value())

let _py_check_heap =
  python_interfaced_function
    [||]
    (fun arr -> let _ = Gc.full_major () in py_true ())


(* -- init -- *)

let _ =
  begin
    Callback.register_exception "ocaml_exn_pycaml" (Pycaml_exn (Pyerr_StandardError,""));
    ignore(pydict_setitemstring (_py_sys_modules, "ocaml", _py_mod_ocaml));
    ignore(python_eval "import ocaml");
    register_for_python
      [|
	("sys_profiling",_py_profiling);
	(* -- This one is very important -- *)
	("sys_ocamlpill_type",_py_ocamlpill_type);
	(* -- This may seem very strange, but actually comes in handy -- *)
	("sys_python",_py_python);
	("sys_ipython",_py_ipython);
	("sys_check_heap",_py_check_heap);
	("sys_refcount", 
	 pywrap_closure
	   (fun py_args ->
	      let args = pytuple_toarray py_args in
		pyint_fromint (pyrefcount args.(0) )));

	(* -- Examples below -- *)
	("example_test_interface", 
	 pywrap_closure
	   (fun _ ->
	      begin
		Printf.printf "This is printed by OCaml, called from Python!\n%!" ;
		pynone ()
	      end
	   ));
	("example_the_answer", pyint_fromint 42);
      |];
  end


(* --- Example Code --- *)

(* For demonstrative purposes, we include two functions
   readily wrapped up here.
*)

(* 
   Now, let's have fun with this:
 *)

let _py_make_powers =
  python_interfaced_function
    ~extra_guards:
    [|(fun py_len ->
	     let len = pyint_asint py_len in
	       if len < 0 then
             Some "Negative Length"
	       else
             None);
      (fun _ -> None); (* This check never fails *)
    |]
    [|IntType;FloatType|]
    (fun py_args ->
       let len = pyint_asint py_args.(0)
       and pow = pyfloat_asdouble py_args.(1) in
	     float_array_to_python
	       (Array.init len (fun n -> let nn = float_of_int (n+1) in nn**pow)))
(*
let
    _py_hypotenuse_2d =
  python_interfaced_function
    [|FloatType;FloatType|]
    (fun py_args ->
       let x = pyfloat_asdouble py_args.(0)
       and y = pyfloat_asdouble py_args.(1) in
         pyfloat_fromdouble (sqrt(x*.x+.y*.y)))
in
  register_for_python
    [|("example_make_powers", _py_make_powers);
      ("example_hypotenuse", _py_hypotenuse_2d);
    |]
*)
