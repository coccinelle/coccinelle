#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <sys/param.h>
#include <string.h>
#include <dlfcn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* The following definitions are extracted and simplified from
#include <Python.h>
*/

typedef ssize_t Py_ssize_t;

typedef struct {
    Py_ssize_t ob_refcnt;
    struct _typeobject *ob_type;
} PyObject;

typedef struct {
    PyObject ob_base;
    Py_ssize_t ob_size;
} PyVarObject;

typedef void (*destructor)(PyObject *);

typedef struct _typeobject {
    PyVarObject ob_base;
    const char *tp_name;
    Py_ssize_t tp_basicsize, tp_itemsize;
    destructor tp_dealloc;
    void *tp_print;
    void *tp_getattr;
    void *tp_setattr;
    void *tp_as_async;
    void *tp_repr;
    void *tp_as_number;
    void *tp_as_sequence;
    void *tp_as_mapping;
    void *tp_hash;
    void *tp_call;
    void *tp_str;
    void *tp_getattro;
    void *tp_setattro;
    void *tp_as_buffer;
    unsigned long tp_flags;
    const char *tp_doc;
    void *tp_traverse;
    void *tp_clear;
    void *tp_richcompare;
    Py_ssize_t tp_weaklistoffset;
    void *tp_iter;
    void *tp_iternext;
} PyTypeObject;

typedef struct {
    int cf_flags;
} PyCompilerFlags;

#define Py_TPFLAGS_LONG_SUBCLASS        (1UL << 24)
#define Py_TPFLAGS_LIST_SUBCLASS        (1UL << 25)
#define Py_TPFLAGS_TUPLE_SUBCLASS       (1UL << 26)
#define Py_TPFLAGS_BYTES_SUBCLASS       (1UL << 27)
#define Py_TPFLAGS_UNICODE_SUBCLASS     (1UL << 28)
#define Py_TPFLAGS_DICT_SUBCLASS        (1UL << 29)
#define Py_TPFLAGS_BASE_EXC_SUBCLASS    (1UL << 30)
#define Py_TPFLAGS_TYPE_SUBCLASS        (1UL << 31)

#define Py_INCREF(op)                                            \
    (((PyObject *)(op))->ob_refcnt++)

#define Py_XINCREF(op)                                           \
    do {                                                         \
        PyObject *_py_xincref_tmp = (PyObject *)(op);            \
        if (_py_xincref_tmp != NULL)                             \
            Py_INCREF(_py_xincref_tmp);                          \
    } while (0)

#define Py_DECREF(op)                                            \
    do {                                                         \
        PyObject *_py_decref_tmp = (PyObject *)(op);             \
        if (--(_py_decref_tmp)->ob_refcnt == 0)                  \
            _py_decref_tmp->ob_type->tp_dealloc(_py_decref_tmp); \
    } while (0)

#define Py_LT 0
#define Py_LE 1
#define Py_EQ 2
#define Py_NE 3
#define Py_GT 4
#define Py_GE 5

typedef PyObject *(*PyCFunction)(PyObject *, PyObject *);

typedef struct PyMethodDef {
    const char *ml_name;
    PyCFunction ml_meth;
    int ml_flags;
    const char	*ml_doc;
} PyMethodDef;

typedef void (*PyCapsule_Destructor)(PyObject *);

static void *Python__PyObject_NextNotImplemented;

/* Global variables for the library */

static int version_major;

static void *library;

/* Functions that are special enough to deserved to be wrapped specifically */

/* Wrapped by pywrap_closure */
static PyObject *(*Python_PyCFunction_NewEx)
(PyMethodDef *, PyObject *, PyObject *);

/* Wrapped by closure and capsuble */
static void *(*Python_PyCapsule_New)
(void *, const char *, PyCapsule_Destructor);
static void *(*Python_PyCapsule_GetPointer)(PyObject *, const char *);

/* Hack for multi-arguments */
static PyObject *(*Python_PyObject_CallFunctionObjArgs)(PyObject *, ...);

/* Wrapped by PyErr_Fetch_wrapper */
static void (*Python_PyErr_Fetch)(PyObject **, PyObject **, PyObject **);
static void (*Python_PyErr_NormalizeException)
(PyObject **, PyObject **, PyObject **);

/* Resolved differently between Python 2 and Python 3 */
static PyObject *Python__Py_FalseStruct;

#include "pyml.h"

static void *getcustom( value v )
{
    return *((void **)Data_custom_val(v));
}

static void pydecref( value v )
{
    if (getcustom(v)) {
        Py_DECREF((PyObject *)getcustom(v));
    }
}

static int pycompare(value v1, value v2)
{
    int result;
    PyObject *o1 = getcustom(v1);
    PyObject *o2 = getcustom(v2);

    if (o1 && !o2)
        result = -1;
    else if (o2 && !o1)
        result = 1;
    else if (!o1 && !o2)
        result = 0;
    else if (version_major <= 2)
        Python2_PyObject_Cmp(o1, o2, &result);
    else if (1 == Python_PyObject_RichCompareBool(o1, o2, Py_EQ))
        result = 0;
    else if (1 == Python_PyObject_RichCompareBool(o1, o2, Py_LT))
        result = -1;
    else if (1 == Python_PyObject_RichCompareBool(o1, o2, Py_GT))
        result = 1;
    else
        result = -1;

    return result;
}

static long pyhash( value v )
{
    if (getcustom(v))
        return Python_PyObject_Hash((PyObject *)getcustom(v));
    else
        return 0L;
}

static unsigned long
pydeserialize(void *dst)
{
    return 0L;
}

struct custom_operations pyops =
{
    "PythonObject",
    pydecref,
    pycompare,
    pyhash,
    custom_serialize_default,
    pydeserialize
};

enum code {
    CODE_NULL,
    CODE_NONE,
    CODE_TRUE,
    CODE_FALSE
};

static void *
resolve(const char *symbol)
{
    void *result = dlsym(library, symbol);
    if (result == NULL) {
        fprintf(stderr, "Cannot resolve %s.\n", symbol);
        exit(EXIT_FAILURE);
    }
    return result;
}

static value
pywrap(PyObject *obj, bool steal)
{
    CAMLparam0();
    CAMLlocal1(v);
    if (obj == NULL) {
        CAMLreturn(Val_int(CODE_NULL));
    }
    if (obj == Python__Py_NoneStruct) {
        CAMLreturn(Val_int(CODE_NONE));
    }
    if (obj == Python__Py_TrueStruct) {
        CAMLreturn(Val_int(CODE_TRUE));
    }
    if (obj == Python__Py_FalseStruct) {
        CAMLreturn(Val_int(CODE_FALSE));
    }
    if (!steal) {
        Py_INCREF(obj);
    }
    v = caml_alloc_custom(&pyops, sizeof(PyObject *), 100, 30000000);
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

static PyObject *
pyunwrap(value v)
{
    if (Is_long(v))
        switch (Int_val(v)) {
        case CODE_NULL:
            return NULL;
        case CODE_NONE:
            return Python__Py_NoneStruct;
        case CODE_TRUE:
            return Python__Py_TrueStruct;
        case CODE_FALSE:
            return Python__Py_FalseStruct;
        }

    return *((PyObject **)Data_custom_val(v));
}

/*
static value
pywrap_compilerflags(PyCompilerFlags *flags)
{
    CAMLparam0();
    CAMLlocal2(ref, some);
    if (flags == NULL) {
        CAMLreturn(Val_int(0));
    }
    else {
        ref = caml_alloc(0, 1);
        Store_field(ref, 0, Val_int(flags->cf_flags));
        some = caml_alloc(0, 1);
        Store_field(some, 0, ref);
        CAMLreturn(some);
    }
}
*/

static PyCompilerFlags *
pyunwrap_compilerflags(value v)
{
    CAMLparam1(v);
    if (Is_block(v)) {
        PyCompilerFlags *flags = malloc(sizeof(PyCompilerFlags));
        flags->cf_flags = Int_val(Field(Field(v, 0), 0));
        CAMLreturnT(PyCompilerFlags *, flags);
    }
    else {
        CAMLreturnT(PyCompilerFlags *, NULL);
    }
}

/*
static value
pywrap_intref(int v)
{
    CAMLparam0();
    CAMLlocal1(ref);
    ref = caml_alloc(0, 1);
    Store_field(ref, 0, Val_int(v));
    CAMLreturn(ref);
}
*/

static int
pyunwrap_intref(value v)
{
    CAMLparam1(v);
    CAMLreturnT(int, Int_val(Field(v, 0)));
}

static PyObject *
pycall_callback(PyObject *obj, PyObject *args)
{
    CAMLparam0();
    CAMLlocal3(ml_out, ml_func, ml_args);
    PyObject *out;
    void *p = Python_PyCapsule_GetPointer(obj, "ocaml-closure");
    if (p == NULL)
        {
          Py_INCREF(Python__Py_NoneStruct);
          return Python__Py_NoneStruct;
        }
    ml_func = *(value *) p;
    ml_args = pywrap(args, false);
    ml_out = caml_callback(ml_func, ml_args);
    out = pyunwrap(ml_out);
    Py_XINCREF(out);
    CAMLreturnT(PyObject *, out);
}

static void
caml_destructor(PyObject *v, const char *capsule_name)
{
    value *valptr = (value *) Python_PyCapsule_GetPointer(v, capsule_name);
    caml_remove_global_root(valptr);
    free(valptr);
}

static void
camldestr_closure(PyObject *v)
{
    caml_destructor(v, "ocaml-closure");
}

static PyObject *
camlwrap_closure(value val, void *aux_str, int size)
{
    value *v = (value *) malloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v + sizeof(value), aux_str, size);
    caml_register_global_root(v);
    return Python_PyCapsule_New(v, "ocaml-closure", camldestr_closure);
}

static void
camldestr_capsule(PyObject *v)
{
    caml_destructor(v, "ocaml-capsule");
}

static PyObject *
camlwrap_capsule(value val, void *aux_str, int size)
{
    value *v = (value *) malloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v + sizeof(value), aux_str, size);
    caml_register_global_root(v);
    return Python_PyCapsule_New(v, "ocaml-capsule", camldestr_capsule);
}

static void *
caml_aux(PyObject *obj)
{
    value *v = (value *) Python_PyCapsule_GetPointer(obj, "ocaml-closure");
    return (void *) v + sizeof(value);
}

CAMLprim value
pywrap_closure(value closure)
{
    CAMLparam1(closure);
    PyMethodDef ml;
    PyObject *obj;
    PyMethodDef *ml_def;
    ml.ml_name = "anonymous_closure";
    ml.ml_meth = pycall_callback;
    ml.ml_flags = 1;
    ml.ml_doc = "Anonymous closure";
    obj = camlwrap_closure(closure, &ml, sizeof(ml));
    ml_def = (PyMethodDef *) caml_aux(obj);
    PyObject *f = Python_PyCFunction_NewEx(ml_def, obj, NULL);
    CAMLreturn(pywrap(f, true));
}

CAMLprim value
py_load_library(value version_major_ocaml, value filename_ocaml)
{
    CAMLparam2(version_major_ocaml, filename_ocaml);
    version_major = Int_val(version_major_ocaml);
    if (Is_block(filename_ocaml)) {
        char *filename = String_val(Field(filename_ocaml, 0));
        library = dlopen(filename, RTLD_LAZY);
        if (library == NULL) {
            failwith("Library not found");
            CAMLreturn(Val_unit);
        }
    }
    else {
        library = RTLD_DEFAULT;
    }
    Python_PyCFunction_NewEx = dlsym(library, "PyCFunction_NewEx");
    if (Python_PyCFunction_NewEx == NULL) {
        failwith("No Python symbol");
        CAMLreturn(Val_unit);
    }
    Python_PyCapsule_New = resolve("PyCapsule_New");
    Python_PyCapsule_GetPointer = resolve("PyCapsule_GetPointer");
    Python_PyObject_CallFunctionObjArgs = resolve("PyObject_CallFunctionObjArgs");
    Python_PyErr_Fetch = resolve("PyErr_Fetch");
    Python_PyErr_NormalizeException = resolve("PyErr_NormalizeException");
    Python__PyObject_NextNotImplemented = resolve("_PyObject_NextNotImplemented");
    if (version_major <= 2) {
        Python__Py_FalseStruct = resolve("_Py_ZeroStruct");
    }
    else {
        Python__Py_FalseStruct = resolve("_Py_FalseStruct");
    }
#include "pyml_dlsyms.inc"
    Python_Py_Initialize();
    CAMLreturn(Val_unit);
}

CAMLprim value
py_finalize_library(value unit)
{
    CAMLparam1(unit);
    if (library != RTLD_DEFAULT) {
        dlclose(library);
    }
    CAMLreturn(Val_unit);
}

CAMLprim value
PyNull_wrapper(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(CODE_NULL));
}

CAMLprim value
PyNone_wrapper(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(CODE_NONE));
}

CAMLprim value
PyTrue_wrapper(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(CODE_TRUE));
}

CAMLprim value
PyFalse_wrapper(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(CODE_FALSE));
}

enum pytype_labels {
    Unknown,
    Bool,
    Bytes,
    Callable,
    Capsule,
    Closure,
    Dict,
    Float,
    List,
    Long,
    Module,
    NoneType,
    Null,
    Tuple,
    Type,
    Unicode,
    Iter
};

CAMLprim value
pytype(value object_ocaml)
{
    CAMLparam1(object_ocaml);
    PyObject *object = pyunwrap(object_ocaml);
    if (object == NULL) {
        CAMLreturn(Val_int(Null));
    }
    unsigned long flags = object->ob_type->tp_flags;
    int result;
    if ((PyObject *) object->ob_type == Python_PyBool_Type) {
        result = Bool;
    }
    else if (flags & Py_TPFLAGS_BYTES_SUBCLASS) {
        result = Bytes;
    }
    else if (Python_PyCallable_Check(object)) {
        result = Callable;
    }
    else if (Python_PyCapsule_IsValid(object, "ocaml-capsule")) {
        result = Capsule;
    }
    else if (Python_PyCapsule_IsValid(object, "ocaml-closure")) {
        result = Closure;
    }
    else if (flags & Py_TPFLAGS_DICT_SUBCLASS) {
        result = Dict;
    }
    else if (
        (PyObject *) object->ob_type == Python_PyFloat_Type ||
        Python_PyType_IsSubtype(
            (PyObject *) object->ob_type, Python_PyFloat_Type)) {
        result = Float;
    }
    else if (flags & Py_TPFLAGS_LIST_SUBCLASS) {
        result = List;
    }
    else if (flags & Py_TPFLAGS_LONG_SUBCLASS) {
        result = Long;
    }
    else if (
        (PyObject *) object->ob_type == Python_PyModule_Type ||
        Python_PyType_IsSubtype(
            (PyObject *) object->ob_type, Python_PyModule_Type)) {
        result = Module;
    }
    else if (object == Python__Py_NoneStruct) {
        result = NoneType;
    }
    else if (flags & Py_TPFLAGS_TUPLE_SUBCLASS) {
        result = Tuple;
    }
    else if (flags & Py_TPFLAGS_TYPE_SUBCLASS) {
        result = Type;
    }
    else if (flags & Py_TPFLAGS_UNICODE_SUBCLASS) {
        result = Unicode;
    }
    else if (object->ob_type->tp_iternext != NULL &&
        object->ob_type->tp_iternext != &Python__PyObject_NextNotImplemented) {
        result = Iter;
    }
    else {
        result = Unknown;
    }
    CAMLreturn(Val_int(result));
}

CAMLprim value
PyObject_CallFunctionObjArgs_wrapper(
    value callable_ocaml, value arguments_ocaml)
{
    CAMLparam2(callable_ocaml, arguments_ocaml);
    PyObject *callable = pyunwrap(callable_ocaml);
    PyObject *result;
    int argument_count = Wosize_val(arguments_ocaml);
    switch (argument_count) {
    case 0:
        result = Python_PyObject_CallFunctionObjArgs(callable);
        break;
    case 1:
        result = Python_PyObject_CallFunctionObjArgs
            (callable,
             pyunwrap(Field(arguments_ocaml, 0)),
             NULL);
        break;
    case 2:
        result = Python_PyObject_CallFunctionObjArgs
            (callable,
             pyunwrap(Field(arguments_ocaml, 0)),
             pyunwrap(Field(arguments_ocaml, 1)),
             NULL);
        break;
    case 3:
        result = Python_PyObject_CallFunctionObjArgs
            (callable,
             pyunwrap(Field(arguments_ocaml, 0)),
             pyunwrap(Field(arguments_ocaml, 1)),
             pyunwrap(Field(arguments_ocaml, 2)),
             NULL);
        break;
    case 4:
        result = Python_PyObject_CallFunctionObjArgs
            (callable,
             pyunwrap(Field(arguments_ocaml, 0)),
             pyunwrap(Field(arguments_ocaml, 1)),
             pyunwrap(Field(arguments_ocaml, 2)),
             pyunwrap(Field(arguments_ocaml, 3)),
             NULL);
        break;
    case 5:
        result = Python_PyObject_CallFunctionObjArgs
            (callable,
             pyunwrap(Field(arguments_ocaml, 0)),
             pyunwrap(Field(arguments_ocaml, 1)),
             pyunwrap(Field(arguments_ocaml, 2)),
             pyunwrap(Field(arguments_ocaml, 3)),
             pyunwrap(Field(arguments_ocaml, 4)),
             NULL);
        break;
    default:
        fprintf(stderr,
                "PyObject_CallFunctionObjArgs_wrapper not implemented for more "
                "than 5 arguments\n");
        exit(EXIT_FAILURE);
    }

    CAMLreturn(pywrap(result, true));
}

CAMLprim value
pywrap_value(value name_ocaml, value v) {
    CAMLparam2(name_ocaml, v);
    CAMLlocal1(pair);
    pair = caml_alloc(2, 0);
    Store_field(pair, 0, name_ocaml);
    Store_field(pair, 1, v);
    PyObject *result = camlwrap_capsule(pair, NULL, 0);
    CAMLreturn(pywrap(result, true));
}

CAMLprim value
pyunwrap_value(value x_ocaml) {
    CAMLparam1(x_ocaml);
    CAMLlocal2(v, pair);
    PyObject *x = pyunwrap(x_ocaml);
    void *p = Python_PyCapsule_GetPointer(x, "ocaml-capsule");
    if (p == NULL) {
        fprintf(stderr, "pyunwrap_value: type mismatch");
        exit(EXIT_FAILURE);
    }
    v = *(value *) p;
    pair = caml_alloc(2, 0);
    Store_field(pair, 0, Field(v, 0));
    Store_field(pair, 1, Field(v, 1));
    CAMLreturn(pair);
}

CAMLprim value
PyErr_Fetch_wrapper(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    PyObject *excType, *excValue, *excTraceback;
    Python_PyErr_Fetch(&excType, &excValue, &excTraceback);
    Python_PyErr_NormalizeException(&excType, &excValue, &excTraceback);
    result = caml_alloc(3, 0);
    Store_field(result, 0, pywrap(excType, false));
    Store_field(result, 1, pywrap(excValue, false));
    Store_field(result, 2, pywrap(excTraceback, false));
    CAMLreturn(result);
}

CAMLprim value
pywrap_string_option(char *s) {
    CAMLparam0();
    CAMLlocal1(result);
    if (s == NULL) {
        CAMLreturn(Val_int(0));
    }
    else {
        result = caml_alloc(1, 0);
        Store_field(result, 0, caml_copy_string(s));
        CAMLreturn(result);
    }
}

static void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL) {
        fprintf(stderr, "Virtual memory exhausted\n");
        exit(1);
    }
    return p;
}

static value
pywrap_wide_string(wchar_t *ws) {
    CAMLparam0();
    CAMLlocal1(result);
    size_t n = wcstombs(NULL, ws, 0);
    if (n == (size_t) -1) {
        fprintf(stderr, "pywrap_wide_string failure.\n");
        exit(EXIT_FAILURE);
    }
    char *s = xmalloc((n + 1) * sizeof (char));
    wcstombs(s, ws, n);
    result = caml_copy_string(s);
    free(s);
    CAMLreturn(result);
}

static wchar_t *
pyunwrap_wide_string(value string_ocaml) {
    CAMLparam1(string_ocaml);
    char *s = String_val(string_ocaml);
    size_t n = mbstowcs(NULL, s, 0);
    if (n == (size_t) -1) {
        fprintf(stderr, "pyunwrap_wide_string failure.\n");
        exit(EXIT_FAILURE);
    }
    wchar_t *ws = xmalloc((n + 1) * sizeof (wchar_t));
    mbstowcs(ws, s, n);
    CAMLreturnT(wchar_t *, ws);
}

#include "pyml_wrappers.inc"
