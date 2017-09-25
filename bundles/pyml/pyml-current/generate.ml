type ty =
    PyObject of bool
  | PyCompilerFlags | String | WideString | Int | Int64 | Long | Size | IntPtr
  | Compare | Input | Unit | FileIn of bool | FileOut of bool | Double
  | StringOption | NeverReturn | UCS2 | UCS4 | UCS2Option | UCS4Option of bool

type arguments =
    Value
  | Deref
  | Fun of ty list

type wrapper = {
    symbol: string;
    arguments: arguments;
    result: ty;
  }

let wrappers =
  [{ symbol = "_Py_NoneStruct";
     arguments = Value;
     result = PyObject false; };
   { symbol = "_Py_TrueStruct";
     arguments = Value;
     result = PyObject false; };
   { symbol = "Py_Exit";
     arguments = Fun [Int];
     result = NeverReturn; };
   { symbol = "Py_GetVersion";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetPlatform";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetCopyright";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetCompiler";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetBuildInfo";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_FdIsInteractive";
     arguments = Fun [FileIn true; String];
     result = Int; };
   { symbol = "Py_Initialize";
     arguments = Fun [];
     result = Unit; };
   { symbol = "PyBool_Type";
     arguments = Value;
     result = PyObject false; };
   { symbol = "PyCallable_Check";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyCapsule_IsValid";
     arguments = Fun [PyObject false; String];
     result = Int; };
   { symbol = "PyDict_Clear";
     arguments = Fun [PyObject false];
     result = Unit; };
   { symbol = "PyDict_Copy";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyDict_DelItem";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyDict_DelItemString";
     arguments = Fun [PyObject false; String];
     result = Int; };
   { symbol = "PyDict_GetItem";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject false; };
   { symbol = "PyDict_GetItemString";
     arguments = Fun [PyObject false; String];
     result = PyObject false; };
   { symbol = "PyDict_Keys";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyDict_Items";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyDict_New";
     arguments = Fun [];
     result = PyObject true; };
   { symbol = "PyDict_SetItem";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyDict_SetItemString";
     arguments = Fun [PyObject false; String; PyObject false];
     result = Int; };
   { symbol = "PyDict_Size";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyDict_Values";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyErr_Clear";
     arguments = Fun [];
     result = Unit; };
   { symbol = "PyErr_ExceptionMatches";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyErr_GivenExceptionMatches";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyErr_Occurred";
     arguments = Fun [];
     result = PyObject false; };
   { symbol = "PyErr_Print";
     arguments = Fun [];
     result = Unit; };
   { symbol = "PyErr_Restore";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = Unit; };
   { symbol = "PyErr_PrintEx";
     arguments = Fun [Int];
     result = Unit; };
   { symbol = "PyErr_SetNone";
     arguments = Fun [PyObject false];
     result = Unit; };
   { symbol = "PyErr_SetString";
     arguments = Fun [PyObject false; String];
     result = Unit; };
   { symbol = "PyErr_SetObject";
     arguments = Fun [PyObject false; PyObject false];
     result = Unit; };
   { symbol = "PyEval_CallObjectWithKeywords";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyEval_GetBuiltins";
     arguments = Fun [];
     result = PyObject false; };
   { symbol = "PyEval_GetGlobals";
     arguments = Fun [];
     result = PyObject false; };
   { symbol = "PyEval_GetLocals";
     arguments = Fun [];
     result = PyObject false; };
   { symbol = "PyExc_BaseException";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_Exception";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_StopIteration";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_GeneratorExit";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_ArithmeticError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_LookupError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_AssertionError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_AttributeError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_BufferError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_EOFError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_FloatingPointError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_OSError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_ImportError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_IndexError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_KeyError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_KeyboardInterrupt";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_MemoryError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_NameError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_OverflowError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_RuntimeError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_NotImplementedError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_SyntaxError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_IndentationError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_TabError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_ReferenceError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_SystemError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_SystemExit";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_TypeError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UnboundLocalError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UnicodeError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UnicodeEncodeError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UnicodeDecodeError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UnicodeTranslateError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_ValueError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_ZeroDivisionError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_EnvironmentError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_IOError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_RecursionErrorInst";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_Warning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UserWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_DeprecationWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_PendingDeprecationWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_SyntaxWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_RuntimeWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_FutureWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_ImportWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_UnicodeWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyExc_BytesWarning";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyFloat_AsDouble";
     arguments = Fun [PyObject false];
     result = Double; };
   { symbol = "PyFloat_FromDouble";
     arguments = Fun [Double];
     result = PyObject true; };
   { symbol = "PyFloat_Type";
     arguments = Value;
     result = PyObject false; };
   { symbol = "PyImport_AddModule";
     arguments = Fun [String];
     result = PyObject false; };
   { symbol = "PyImport_Cleanup";
     arguments = Fun [];
     result = Unit; };
   { symbol = "PyImport_ExecCodeModule";
     arguments = Fun [String; PyObject false];
     result = PyObject true; };
   { symbol = "PyImport_ExecCodeModuleEx";
     arguments = Fun [String; PyObject false; String];
     result = PyObject true; };
   { symbol = "PyImport_GetMagicNumber";
     arguments = Fun [];
     result = Int64; };
   { symbol = "PyImport_GetModuleDict";
     arguments = Fun [];
     result = PyObject false; };
   { symbol = "PyImport_ImportFrozenModule";
     arguments = Fun [String];
     result = Int; };
   { symbol = "PyImport_Import";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyImport_ImportModule";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyImport_ImportModuleLevel";
     arguments = Fun [
       String; PyObject false; PyObject false; PyObject false; Int];
     result = PyObject true; };
   { symbol = "PyImport_ReloadModule";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyIter_Next";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyList_New";
     arguments = Fun [Size];
     result = PyObject true; };
   { symbol = "PyList_GetItem";
     arguments = Fun [PyObject false; Size];
     result = PyObject false; };
   { symbol = "PyList_SetItem";
     arguments = Fun [PyObject false; Int; PyObject true];
     result = Int; };
   { symbol = "PyList_Size";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyLong_AsLong";
     arguments = Fun [PyObject false];
     result = Int64; };
   { symbol = "PyLong_FromLong";
     arguments = Fun [Int64];
     result = PyObject true; };
   { symbol = "PyMapping_Check";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyMapping_GetItemString";
     arguments = Fun [PyObject false; String];
     result = PyObject true; };
   { symbol = "PyMapping_HasKey";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyMapping_HasKeyString";
     arguments = Fun [PyObject false; String];
     result = Int; };
   { symbol = "PyMapping_Length";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyMapping_SetItemString";
     arguments = Fun [PyObject false; String; PyObject false];
     result = Int; };
   { symbol = "PyMapping_Size";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyMarshal_ReadObjectFromFile";
     arguments = Fun [FileIn true];
     result = PyObject true; };
   { symbol = "PyMarshal_ReadLastObjectFromFile";
     arguments = Fun [FileIn true];
     result = PyObject true; };
   { symbol = "PyMarshal_ReadObjectFromString";
     arguments = Fun [String; Size];
     result = PyObject true; };
   { symbol = "PyMarshal_WriteObjectToFile";
     arguments = Fun [PyObject false; FileOut true; Int];
     result = Int; };
   { symbol = "PyMarshal_WriteObjectToString";
     arguments = Fun [PyObject false; Int];
     result = PyObject true; };
   { symbol = "PyMethod_Function";
     arguments = Fun [PyObject false];
     result = PyObject false; };
   { symbol = "PyMethod_New";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyMethod_Self";
     arguments = Fun [PyObject false];
     result = PyObject false; };
   { symbol = "PyModule_AddObject";
     arguments = Fun [PyObject false; String; PyObject false];
     result = Int; };
   { symbol = "PyModule_GetDict";
     arguments = Fun [PyObject false];
     result = PyObject false; };
   { symbol = "PyModule_GetFilename";
     arguments = Fun [PyObject false];
     result = StringOption; };
   { symbol = "PyModule_GetName";
     arguments = Fun [PyObject false];
     result = StringOption; };
   { symbol = "PyModule_New";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyModule_Type";
     arguments = Value;
     result = PyObject false; };
   { symbol = "PyNumber_Absolute";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Add";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_And";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Check";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyNumber_Divmod";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Float";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_FloorDivide";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceAdd";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceAnd";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceFloorDivide";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceLshift";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceMultiply";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceOr";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlacePower";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceRemainder";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceRshift";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceSubtract";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceTrueDivide";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceXor";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Invert";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Long";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Lshift";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Multiply";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Negative";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Or";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Positive";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Power";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Remainder";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Rshift";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Subtract";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_TrueDivide";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Xor";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_Call";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_DelItem";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyObject_DelItemString";
     arguments = Fun [PyObject false; String];
     result = Int; };
   { symbol = "PyObject_GetAttr";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_GetAttrString";
     arguments = Fun [PyObject false; String];
     result = PyObject true; };
   { symbol = "PyObject_GetItem";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_GetIter";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_HasAttr";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyObject_HasAttrString";
     arguments = Fun [PyObject false; String];
     result = Int; };
   { symbol = "PyObject_Hash";
     arguments = Fun [PyObject false];
     result = Int64; };
   { symbol = "PyObject_IsTrue";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyObject_Not";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyObject_Print";
     arguments = Fun [PyObject false; FileOut true; Int];
     result = Int; };
   { symbol = "PyObject_Repr";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_RichCompare";
     arguments = Fun [PyObject false; PyObject false; Compare];
     result = PyObject true; };
   { symbol = "PyObject_RichCompareBool";
     arguments = Fun [PyObject false; PyObject false; Compare];
     result = Int; };
   { symbol = "PyObject_SetAttr";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyObject_SetAttrString";
     arguments = Fun [PyObject false; String; PyObject false];
     result = Int; };
   { symbol = "PyObject_SetItem";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyObject_Size";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyObject_Str";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_Type";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyRun_AnyFileExFlags";
     arguments = Fun
       [FileIn false; String; Int; PyCompilerFlags];
     result = Int; };
   { symbol = "PyRun_FileExFlags";
     arguments = Fun
       [FileIn false; String; Input; PyObject false; PyObject false; Int;
        PyCompilerFlags];
     result = PyObject true; };
   { symbol = "PyRun_InteractiveOneFlags";
     arguments = Fun [FileIn true; String; PyCompilerFlags];
     result = Int; };
   { symbol = "PyRun_InteractiveLoopFlags";
     arguments = Fun [FileIn true; String; PyCompilerFlags];
     result = Int; };
   { symbol = "PyRun_SimpleFileExFlags";
     arguments = Fun
       [FileIn false; String; Int; PyCompilerFlags];
     result = Int; };
   { symbol = "PyRun_StringFlags";
     arguments = Fun
       [String; Input; PyObject false; PyObject false; PyCompilerFlags];
     result = PyObject true; };
   { symbol = "PyRun_SimpleStringFlags";
     arguments = Fun [String; PyCompilerFlags];
     result = Int; };
   { symbol = "PySequence_Check";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PySequence_Concat";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PySequence_Contains";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PySequence_Count";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PySequence_DelItem";
     arguments = Fun [PyObject false; Size];
     result = Int; };
   { symbol = "PySequence_DelSlice";
     arguments = Fun [PyObject false; Size; Size];
     result = Int; };
   { symbol = "PySequence_Fast";
     arguments = Fun [PyObject false; String];
     result = PyObject true; };
   { symbol = "PySequence_GetItem";
     arguments = Fun [PyObject false; Size];
     result = PyObject false; };
   { symbol = "PySequence_GetSlice";
     arguments = Fun [PyObject false; Size; Size];
     result = PyObject true; };
   { symbol = "PySequence_In";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PySequence_Index";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PySequence_InPlaceConcat";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PySequence_InPlaceRepeat";
     arguments = Fun [PyObject false; Size];
     result = PyObject true; };
   { symbol = "PySequence_Length";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PySequence_List";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PySequence_Repeat";
     arguments = Fun [PyObject false; Size];
     result = PyObject true; };
   { symbol = "PySequence_SetItem";
     arguments = Fun [PyObject false; Size; PyObject false];
     result = Int; };
   { symbol = "PySequence_SetSlice";
     arguments = Fun [PyObject false; Size; Size; PyObject false];
     result = Int; };
   { symbol = "PySequence_Size";
     arguments = Fun [PyObject false];
     result = Size; };
   { symbol = "PySequence_Tuple";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PySlice_New";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyTuple_GetItem";
     arguments = Fun [PyObject false; Size];
     result = PyObject false; };
   { symbol = "PyTuple_GetSlice";
     arguments = Fun [PyObject false; Size; Size];
     result = PyObject true; };
   { symbol = "PyTuple_New";
     arguments = Fun [Size];
     result = PyObject true; };
   { symbol = "PyTuple_SetItem";
     arguments = Fun [PyObject false; Size; PyObject true];
     result = Int; };
   { symbol = "PyTuple_Size";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyType_IsSubtype";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyType_Type";
     arguments = Value;
     result = PyObject false; };]

let wrappers_python2 =
  [{ symbol = "Py_GetProgramName";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetPythonHome";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetProgramFullPath";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetPrefix";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetExecPrefix";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_GetPath";
     arguments = Fun [];
     result = String; };
   { symbol = "Py_SetProgramName";
     arguments = Fun [String];
     result = Unit; };
   { symbol = "Py_SetPythonHome";
     arguments = Fun [String];
     result = Unit; };
   { symbol = "Py_CompileStringFlags";
     arguments = Fun [String; String; Input; PyCompilerFlags];
     result = PyObject true; };
   { symbol = "PyClass_New";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyExc_StandardError";
     arguments = Deref;
     result = PyObject false; };
   { symbol = "PyEval_GetRestricted";
     arguments = Fun [];
     result = Int; };
   { symbol = "PyInstance_New";
     arguments = Fun [PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyInstance_NewRaw";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyInt_AsLong";
     arguments = Fun [PyObject false];
     result = Int64; };
   { symbol = "PyInt_FromLong";
     arguments = Fun [Int64];
     result = PyObject true; };
   { symbol = "PyInt_GetMax";
     arguments = Fun [];
     result = Int64; };
   { symbol = "PyMethod_Class";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Divide";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_InPlaceDivide";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyNumber_Int";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyObject_Cmp";
     arguments = Fun [PyObject false; PyObject false; IntPtr];
     result = Int; };
   { symbol = "PyObject_Compare";
     arguments = Fun [PyObject false; PyObject false];
     result = Int; };
   { symbol = "PyObject_Unicode";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyString_AsString";
     arguments = Fun [PyObject false];
     result = StringOption; };
   { symbol = "PyString_Format";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyString_FromString";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyString_FromStringAndSize";
     arguments = Fun [String; Size];
     result = PyObject true; };
   { symbol = "PyString_Size";
     arguments = Fun [PyObject false];
     result = Size; };]

let wrappers_ucs2 =
  [{ symbol = "PyUnicodeUCS2_AsEncodedString";
     arguments = Fun [PyObject false; String; String];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_AsUTF8String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_AsUTF16String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_AsUTF32String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_DecodeUTF8";
     arguments = Fun [String; Size; StringOption];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_DecodeUTF16";
     arguments = Fun [String; Size; StringOption; IntPtr];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_DecodeUTF32";
     arguments = Fun [String; Size; StringOption; IntPtr];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_Format";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_FromString";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS2_GetSize";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyUnicodeUCS2_FromUnicode";
     arguments = Fun [UCS2; Size];
     result = PyObject false; };
   { symbol = "PyUnicodeUCS2_AsUnicode";
     arguments = Fun [PyObject false];
     result = UCS2Option; };]

let wrappers_ucs4 =
  [{ symbol = "PyUnicodeUCS4_AsEncodedString";
     arguments = Fun [PyObject false; String; String];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_AsUTF8String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_AsUTF16String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_AsUTF32String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_DecodeUTF8";
     arguments = Fun [String; Size; StringOption];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_DecodeUTF16";
     arguments = Fun [String; Size; StringOption; IntPtr];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_DecodeUTF32";
     arguments = Fun [String; Size; StringOption; IntPtr];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_Format";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_FromString";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyUnicodeUCS4_GetSize";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyUnicodeUCS4_FromUnicode";
     arguments = Fun [UCS4; Size];
     result = PyObject false; };
   { symbol = "PyUnicodeUCS4_AsUnicode";
     arguments = Fun [PyObject false];
     result = UCS4Option false; };]

let wrappers_python3 =
  [{ symbol = "Py_GetProgramName";
     arguments = Fun [];
     result = WideString; };
   { symbol = "Py_GetPythonHome";
     arguments = Fun [];
     result = WideString; };
   { symbol = "Py_GetProgramFullPath";
     arguments = Fun [];
     result = WideString; };
   { symbol = "Py_GetPrefix";
     arguments = Fun [];
     result = WideString; };
   { symbol = "Py_GetExecPrefix";
     arguments = Fun [];
     result = WideString; };
   { symbol = "Py_GetPath";
     arguments = Fun [];
     result = WideString; };
   { symbol = "Py_SetProgramName";
     arguments = Fun [WideString];
     result = Unit; };
   { symbol = "Py_SetPythonHome";
     arguments = Fun [WideString];
     result = Unit; };
   { symbol = "Py_CompileStringExFlags";
     arguments = Fun [String; String; Input; PyCompilerFlags; Int];
     result = PyObject true; };
   { symbol = "PyBytes_AsString";
     arguments = Fun [PyObject false];
     result = StringOption; };
   { symbol = "PyBytes_FromString";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyBytes_Size";
     arguments = Fun [PyObject false];
     result = Size; };
   { symbol = "PyImport_ExecCodeModuleObject";
     arguments = Fun [
       PyObject false; PyObject false; PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyImport_ExecCodeModuleWithPathnames";
     arguments = Fun [String; PyObject false; String; String];
     result = PyObject true; };
   { symbol = "PyImport_ImportModuleLevelObject";
     arguments = Fun [
       PyObject false; PyObject false; PyObject false; PyObject false; Int];
     result = PyObject true; };
   { symbol = "PyInstanceMethod_New";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicode_AsEncodedString";
     arguments = Fun [PyObject false; String; String];
     result = PyObject true; };
   { symbol = "PyUnicode_AsUTF8String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicode_AsUTF16String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicode_AsUTF32String";
     arguments = Fun [PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicode_DecodeUTF8";
     arguments = Fun [String; Size; StringOption];
     result = PyObject true; };
   { symbol = "PyUnicode_DecodeUTF16";
     arguments = Fun [String; Size; StringOption; IntPtr];
     result = PyObject true; };
   { symbol = "PyUnicode_DecodeUTF32";
     arguments = Fun [String; Size; StringOption; IntPtr];
     result = PyObject true; };
   { symbol = "PyUnicode_Format";
     arguments = Fun [PyObject false; PyObject false];
     result = PyObject true; };
   { symbol = "PyUnicode_FromString";
     arguments = Fun [String];
     result = PyObject true; };
   { symbol = "PyUnicode_FromStringAndSize";
     arguments = Fun [String; Size];
     result = PyObject true; };
   { symbol = "PyUnicode_GetLength";
     arguments = Fun [PyObject false];
     result = Size; };
   { symbol = "PyUnicode_GetSize";
     arguments = Fun [PyObject false];
     result = Int; };
   { symbol = "PyUnicode_FromKindAndData";
     arguments = Fun [Int; UCS4; Size];
     result = PyObject false; };
   { symbol = "PyUnicode_AsUCS4Copy";
     arguments = Fun [PyObject false];
     result = UCS4Option true; };]

let string_of_type_ml ty =
  match ty with
    PyObject _ -> "Pytypes.pyobject"
  | PyCompilerFlags -> "int ref option"
  | String | WideString -> "string"
  | Int | Long | Size -> "int"
  | FileIn _ | FileOut _ -> "Unix.file_descr Pytypes.file"
  | Int64 -> "int64"
  | IntPtr -> "int ref"
  | Compare -> "Pytypes.compare"
  | Unit -> "unit"
  | Input -> "Pytypes.input"
  | Double -> "float"
  | StringOption -> "string option"
  | NeverReturn -> "'a"
  | UCS2 | UCS4 -> "int array"
  | UCS2Option | UCS4Option _ -> "int array option"

let decapitalize prefix symbol =
  prefix ^ symbol

let wrapper_name prefix symbol =
  Printf.sprintf "%s%s_wrapper" prefix symbol

let bytecode_name prefix symbol =
  Printf.sprintf "%s%s_bytecode" prefix symbol

let native_name prefix symbol =
  Printf.sprintf "%s%s_native" prefix symbol

let print_external indent prefix channel wrapper =
  let symbol = wrapper.symbol in
  let symbol_lowercase = Pyml_compat.lowercase symbol in
  let arguments = wrapper.arguments in
  let ty_arguments =
    match arguments with
      Value | Deref | Fun [] -> "unit"
    | Fun arguments' ->
      String.concat " -> " (List.map string_of_type_ml arguments') in
  let ty_result = string_of_type_ml wrapper.result in
  let ty = ty_arguments ^ " -> " ^ ty_result in
  let decl_start = Printf.sprintf "%sexternal %s:" indent symbol_lowercase in
  let decl_middle = Printf.sprintf " %s" ty in
  let decl_end =
    match arguments with
      Fun arguments' when List.length arguments' > 5 ->
        Printf.sprintf " = \"%s\" \"%s\""
          (bytecode_name prefix symbol) (native_name prefix symbol)
    | _ ->
        Printf.sprintf " = \"%s\"" (wrapper_name prefix symbol) in
  if String.length decl_start + String.length decl_middle > 80 then
    Printf.fprintf channel "%s\n     %s\n     %s\n"
      decl_start decl_middle decl_end
  else if String.length decl_start + String.length decl_middle
      + String.length decl_end > 80 then
    Printf.fprintf channel "%s%s\n     %s\n" decl_start decl_middle decl_end
  else
    Printf.fprintf channel "%s%s%s\n" decl_start decl_middle decl_end

let print_externals indent prefix channel wrappers =
  List.iter (print_external indent prefix channel) wrappers

let print_pycaml indent prefix channel wrapper =
  let symbol = wrapper.symbol in
  let symbol_lowercase = Pyml_compat.lowercase symbol in
  let arguments = wrapper.arguments in
  let arguments_list =
    match arguments with
      Value | Deref -> []
    | Fun list -> Pyml_compat.mapi (fun i _ -> Printf.sprintf "arg%d" i) list in
  let arguments_tuple =
    match arguments with
      Value | Deref -> ""
    | Fun [] -> " ()"
    | Fun [_] -> " arg"
    | Fun list ->
        Printf.sprintf " (%s)" (String.concat ", " arguments_list) in
  let convert i ty =
    let arg = Printf.sprintf "arg%d" i in
    match ty with
      Compare -> Printf.sprintf "(Pytypes.compare_of_int %s)" arg
    | Input -> Printf.sprintf "(Pytypes.input_of_int %s)" arg
    | FileIn _ | FileOut _ ->
        Printf.sprintf "(Pytypes.Channel (Pyml_arch.fd_of_int %s))" arg
    | _ -> arg in
  let converted_arguments_list =
    match arguments with
      Value | Deref -> []
    | Fun list -> Pyml_compat.mapi convert list in
  let arguments_curryfied =
    match arguments with
      Value | Deref -> ""
    | Fun [] -> " ()"
    | Fun [_] -> " arg"
    | Fun list ->
        Printf.sprintf " %s" (String.concat " " converted_arguments_list) in
  Printf.fprintf channel "%slet %s%s = %s%s%s\n"
    indent symbol_lowercase arguments_tuple prefix symbol_lowercase
    arguments_curryfied

let print_pycamls indent prefix channel wrappers =
  List.iter (print_pycaml indent prefix channel) wrappers

let print_all_externals channel =
  Printf.fprintf channel "(** Low-level bindings. *)
(** The library has to be initialized via {!Py.initialize} first. *)

";
  print_externals "" "Python_" channel wrappers;
  Printf.fprintf channel "
(** Python 2 specific bindings. *)
module Python2 = struct\n";
  print_externals "  " "Python2_" channel wrappers_python2;
  Printf.fprintf channel "end
(** UCS2 specific bindings. *)
module UCS2 = struct\n";
  print_externals "  " "UCS2_" channel wrappers_ucs2;
  Printf.fprintf channel "end
(** UCS4 specific bindings. *)
module UCS4 = struct\n";
  print_externals "  " "UCS4_" channel wrappers_ucs4;
  Printf.fprintf channel "end
(** Python 3 specific bindings. *)
module Python3 = struct\n";
  print_externals "  " "Python3_" channel wrappers_python3;
  Printf.fprintf channel "end\n
(** Automatic wrappers for Pycaml_compat. *)
module Pycaml = struct\n";
  print_pycamls "  " "" channel wrappers;
  print_pycamls "  " "Python2." channel wrappers_python2;
  print_pycamls "  " "Python3." channel wrappers_python3;
  Printf.fprintf channel "end\n"

let print_dlsym indent prefix channel wrapper =
  let symbol = wrapper.symbol in
  let symbol_decapitalized = decapitalize prefix symbol in
  if wrapper.arguments = Deref then
    Printf.fprintf channel "%s%s = *(void **) resolve(\"%s\");\n"
      indent symbol_decapitalized symbol
  else if wrapper.arguments = Value then
    Printf.fprintf channel "%s%s = resolve(\"%s\");\n"
      indent symbol_decapitalized symbol
  else
    Printf.fprintf channel "%s%s = resolve(\"%s\");\n"
      indent symbol_decapitalized symbol

let print_dlsyms indent prefix channel wrappers =
  List.iter (print_dlsym indent prefix  channel) wrappers

let print_all_dlsyms channel =
  print_dlsyms "" "Python_" channel wrappers;
  Printf.fprintf channel "if (version_major <= 2) {";
  print_dlsyms "    " "Python2_" channel wrappers_python2;
  Printf.fprintf channel "}\nelse {\n";
  print_dlsyms "    " "Python3_" channel wrappers_python3;
  Printf.fprintf channel "}
switch (ucs) {
case UCS2:
";
  print_dlsyms "    " "UCS2_" channel wrappers_ucs2;
  Printf.fprintf channel "break;
case UCS4:
";
  print_dlsyms "    " "UCS4_" channel wrappers_ucs4;
  Printf.fprintf channel "break;
case UCS_NONE:
  break;
}\n"

let string_of_type_c ty =
  match ty with
    PyObject _ -> "PyObject *"
  | PyCompilerFlags -> "PyCompilerFlags *"
  | String | StringOption -> "char *"
  | WideString -> "wchar_t *"
  | Int -> "int"
  | Long | Int64 -> "long"
  | Size -> "Py_ssize_t"
  | IntPtr -> "int *"
  | Compare -> "int"
  | Unit | NeverReturn -> "void"
  | Input -> "int"
  | FileIn _ | FileOut _ -> "FILE *"
  | Double -> "double"
  | UCS2 | UCS2Option -> "int16_t *"
  | UCS4 | UCS4Option _ -> "int32_t *"

let print_declaration prefix channel wrapper =
  let symbol = wrapper.symbol in
  let symbol_decapitalized = decapitalize prefix symbol in
  let arguments = wrapper.arguments in
  let result = wrapper.result in
  let ty_result = string_of_type_c result in
  let ty_result =
    if String.sub ty_result (String.length ty_result - 1) 1 = "*" then
      ty_result
    else
      ty_result ^ " " in
  match arguments with
    Value | Deref ->
      Printf.fprintf channel "%s %s;\n" ty_result symbol_decapitalized
  | Fun arguments' ->
      let ty_arguments =
        if arguments' = [] then "void"
        else String.concat ", " (List.map string_of_type_c arguments') in
      Printf.fprintf channel "%s(*%s)(%s);\n"
        ty_result symbol_decapitalized ty_arguments

let print_declarations prefix channel wrappers =
  List.iter (print_declaration prefix channel) wrappers

let print_all_declarations channel =
  print_declarations "Python_" channel wrappers;
  Printf.fprintf channel "\n/* Python 2 */\n";
  print_declarations "Python2_" channel wrappers_python2;
  Printf.fprintf channel "\n/* UCS 2 */\n";
  print_declarations "UCS2_" channel wrappers_ucs2;
  Printf.fprintf channel "\n/* UCS 4 */\n";
  print_declarations "UCS4_" channel wrappers_ucs4;
  Printf.fprintf channel "\n/* Python 3 */\n";
  print_declarations "Python3_" channel wrappers_python3

let coercion_of_caml ty v =
  match ty with
    PyObject _ -> Printf.sprintf "pyunwrap(%s)" v
  | String -> Printf.sprintf "String_val(%s)" v
  | WideString -> Printf.sprintf "pyunwrap_wide_string(%s)" v
  | Int | Long | Size -> Printf.sprintf "Int_val(%s)" v
  | Int64 -> Printf.sprintf "Int64_val(%s)" v
  | IntPtr -> Printf.sprintf "pyunwrap_intref(%s)" v
  | PyCompilerFlags -> Printf.sprintf "pyunwrap_compilerflags(%s)" v
  | Compare -> Printf.sprintf "Int_val(%s)" v
  | Unit | NeverReturn | UCS2Option | UCS4Option _ -> assert false
  | Input -> Printf.sprintf "256 + Int_val(%s)" v
  | FileIn _ -> Printf.sprintf "open_file(%s, \"r\")" v
  | FileOut _ -> Printf.sprintf "open_file(%s, \"w\")" v
  | Double -> Printf.sprintf "Double_val(%s)" v
  | StringOption ->
      Printf.sprintf "Is_block(%s) ? String_val(Field(%s, 0)) : NULL" v v
  | UCS2 -> Printf.sprintf "pyunwrap_ucs2(%s)" v
  | UCS4 -> Printf.sprintf "pyunwrap_ucs4(%s)" v

let string_of_bool b =
  if b then "true"
  else "false"

let coercion_of_c ty =
  match ty with
    PyObject b ->
      Printf.sprintf "    CAMLreturn(pywrap(result, %s));" (string_of_bool b)
  | String -> Printf.sprintf "    CAMLreturn(caml_copy_string(result));"
  | StringOption ->
      Printf.sprintf "    CAMLreturn(pywrap_string_option(result));"
  | WideString -> Printf.sprintf "    CAMLreturn(pywrap_wide_string(result));"
  | Int | Long | Size | Compare ->
      Printf.sprintf "    CAMLreturn(Val_int(result));"
  | Int64 -> Printf.sprintf "    CAMLreturn(copy_int64(result));"
  | IntPtr -> Printf.sprintf "    CAMLreturn(pywrap_intref(result));"
  | PyCompilerFlags ->
      Printf.sprintf "    CAMLreturn(pywrap_compilerflags(result));"
  | Unit | NeverReturn -> Printf.sprintf "    CAMLreturn(Val_unit);"
  | Input | FileIn _ | FileOut _ | UCS2 | UCS4 -> assert false
  | Double -> Printf.sprintf "    CAMLreturn(caml_copy_double(result));"
  | UCS2Option -> Printf.sprintf "    CAMLreturn(pywrap_ucs2_option(result));"
  | UCS4Option free ->
      Printf.sprintf "    CAMLreturn(pywrap_ucs4_option_and_free(result, %s));"
        (string_of_bool free)

let space_if_not_starred s =
  if s.[String.length s - 1] = '*' then
    s
  else
    s ^ " "

let string_of_type_c_argument ty =
  match ty with
    IntPtr -> "int"
  | _ -> string_of_type_c ty

let rec seq a b =
  if a < b then a :: seq (succ a) b
  else []

let print_stub prefix assert_initialized channel wrapper =
  let symbol = wrapper.symbol in
  let arguments = wrapper.arguments in
  let need_bytecode =
    match wrapper.arguments with
      Value | Deref -> false
    | Fun arguments' -> List.length arguments' > 5 in
  let symbol_wrapper =
    if need_bytecode then native_name prefix symbol
    else wrapper_name prefix symbol in
  let symbol_decapitalized = decapitalize prefix symbol in
  let result = wrapper.result in
  let stub_arguments =
    match arguments with
      Value | Deref | Fun [] -> "value unit"
    | Fun arguments' ->
        let value_arg i _ = Printf.sprintf "value arg%d_ocaml" i in
        let stub_argument_list = Pyml_compat.mapi value_arg arguments' in
        String.concat ", " stub_argument_list in
  let arg_ocaml i = Printf.sprintf "arg%d_ocaml" i in
  let camlparam =
    match arguments with
      Value | Deref | Fun [] -> "    CAMLparam1(unit);"
    | Fun arguments' ->
        let result = Buffer.create 17 in
        let count = List.length arguments' in
        let camlparam s a b =
          Buffer.add_string result
            (Printf.sprintf "    %s%d(%s);\n" s (b - a)
               (String.concat ", " (List.map arg_ocaml (seq a b)))) in
        camlparam "CAMLparam" 0 (min count 5);
        for i = 1 to (count - 1) / 5 do
          camlparam "CAMLxparam" (i * 5) (min ((i + 1) * 5) count)
        done;
        Buffer.contents result in
  let destruct_argument i ty =
    let ty_c = string_of_type_c_argument ty in
    let coercion = coercion_of_caml ty (arg_ocaml i) in
    let inc =
      if ty = PyObject true then
        Printf.sprintf "\n    Py_INCREF(arg%d);" i
      else
        "" in
    Printf.sprintf "    %sarg%d = %s;%s"
      (space_if_not_starred ty_c) i coercion inc in
  let destruct_arguments =
    let destruct_argument_list =
      match arguments with
        Value | Deref -> []
      | Fun arguments' -> Pyml_compat.mapi destruct_argument arguments' in
    String.concat "\n" destruct_argument_list in
  let result_type_c = string_of_type_c result in
  let make_arg i ty =
    match ty with
      IntPtr -> Printf.sprintf "&arg%d" i
    | _ -> Printf.sprintf "arg%d" i in
  let callarg =
    match arguments with
      Value | Deref -> symbol_decapitalized
    | Fun arguments' ->
        let arg_c_list = Pyml_compat.mapi make_arg arguments' in
        let arg_c = String.concat ", " arg_c_list in
        Printf.sprintf "%s(%s)" symbol_decapitalized arg_c in
  let call =
    match result with
      Unit | NeverReturn -> Printf.sprintf "    %s;" callarg
    | _ ->
        Printf.sprintf "    %sresult = %s;"
          (space_if_not_starred result_type_c) callarg in
  let free_argument i ty =
    match ty with
      PyCompilerFlags | UCS2 | UCS4 -> Printf.sprintf "\n    free(arg%d);" i
    | FileIn true | FileOut true ->
        Printf.sprintf "\n    close_file(arg%d_ocaml, arg%d);" i i
    | _ -> "" in
  let free =
    match arguments with
      Value | Deref -> ""
    | Fun arguments' ->
        String.concat "" (Pyml_compat.mapi free_argument arguments') in
  let return = coercion_of_c result in
  Printf.fprintf channel "
CAMLprim value
%s(%s)
{
%s
    %s
%s
%s%s
%s
}
" symbol_wrapper stub_arguments camlparam assert_initialized destruct_arguments
    call free return;
  if need_bytecode then
    let arguments' =
      match arguments with
        Value | Deref -> assert false
      | Fun arguments' -> arguments' in
    Printf.fprintf channel "
CAMLprim value
%s(value *argv, int argn)
{
    return %s(%s);
}
" (bytecode_name prefix symbol) (native_name prefix symbol)
      (String.concat ", "
         (Pyml_compat.mapi (fun i _ -> Printf.sprintf "argv[%d]" i) arguments'))

let print_stubs prefix assert_initialized channel wrappers =
  List.iter (print_stub prefix assert_initialized channel) wrappers

let print_all_stubs channel =
  print_stubs "Python_" "assert_initialized();" channel wrappers;
  Printf.fprintf channel "\n/* Python 2 */\n";
  print_stubs "Python2_" "assert_python2();" channel wrappers_python2;
  Printf.fprintf channel "\n/* UCS 2 */\n";
  print_stubs "UCS2_" "assert_ucs2();" channel wrappers_ucs2;
  Printf.fprintf channel "\n/* UCS 4 */\n";
  print_stubs "UCS4_" "assert_ucs4();" channel wrappers_ucs4;
  Printf.fprintf channel "\n/* Python 3 */\n";
  print_stubs "Python3_" "assert_python3();" channel wrappers_python3

let write_file filename f =
  let channel = open_out filename in
  try
    let result = f channel in
    close_out channel;
    result
  with e ->
    close_out_noerr channel;
    raise e

let () =
  write_file "pywrappers.ml" print_all_externals;
  write_file "pyml_dlsyms.inc" print_all_dlsyms;
  write_file "pyml.h" print_all_declarations;
  write_file "pyml_wrappers.inc" print_all_stubs
