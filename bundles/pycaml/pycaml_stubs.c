/*
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


 Heavy modifications (Bugfixes!) done by T.F.

 See

 http://mail.python.org/pipermail/doc-sig/2001-July/001956.html

 for a discussion of result value stealing and a list of functions where
 this becomes relevant.

 Further modifications by Barry Schwartz.
 
 */

#include <Python.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#if 3 <= PY_MAJOR_VERSION
#include <wchar.h>
#if 4 <= PY_MAJOR_VERSION || (PY_MAJOR_VERSION == 3 && 1 <= PY_MINOR_VERSION)
#define USE_PYCAPSULE 1
#else
#define USE_PYCAPSULE 0
#endif
#endif


static void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL) {
        fprintf(stderr, "Virtual memory exhausted\n");
        exit(1);
    }
    return p;
}

#if 3 <= PY_MAJOR_VERSION

static wchar_t *copy_to_wide_string(const char *s)
{
    size_t n;
    wchar_t *ws;

    ws = NULL;
    n = mbstowcs(NULL, s, 0) + 1;
    if (n != (size_t) -1) {
        ws = xmalloc(n * sizeof (wchar_t));
        mbstowcs(ws, s, n);
    }
    return ws;
}

static size_t multibyte_strlen(const wchar_t *ws)
{
    char buffer[MB_CUR_MAX];
    int i;
    size_t size;
    size_t n;
    mbstate_t ps;

    n = wcrtomb(NULL, L'\0', &ps); /* Initialize the parse state. */
    size = 0;
    i = 0;
    while (size != (size_t) -1 && ws[i] != L'\0') {
        n = wcrtomb(buffer, ws[i], &ps);
        if (n != (size_t) -1)
            size += n;
        else
            size = (size_t) -1;
        i++;
    }
    return size;
}

static char *copy_from_wide_string(const wchar_t *ws)
{
    char *s;
    int i;
    int j;
    size_t size;
    size_t n;
    mbstate_t ps;

    s = NULL;
    size = multibyte_strlen(ws);
    if (size != (size_t) -1) {
        s = xmalloc((size + 1) * sizeof (char));
        n = wcrtomb(NULL, L'\0', &ps); /* Initialize the parse state. */
        j = 0;
        i = 0;
        while (ws[i] != L'\0') {
            n = wcrtomb(s + j, ws[i], &ps);
            j += n;
            i++;
        }
        s[j] = '\0';
    }        
    return s;
}

#endif /* 3 <= PY_MAJOR_VERSION */

static void *getcustom( value v )
{
    return *((void **)Data_custom_val(v));
}

static void pydecref( value v )
{
    if( getcustom(v) ) { 
        /* printf("GC - pydecref obj 0x%08x to refcount=%d\nOBJ=",getcustom(v),((PyObject *)getcustom(v))->ob_refcnt-1);
           PyObject_Print((PyObject *)getcustom(v),stdout,0);
           printf("END OBJ\n");
           fflush(stdout);
        */
        Py_DECREF((PyObject *)getcustom(v));
    }
}

#if PY_MAJOR_VERSION <= 2

static int pycompare( value v1, value v2 )
{
    int result;

    if (getcustom(v1) && !getcustom(v2))
        result = -1;
    else if (getcustom(v2) && !getcustom(v1))
        result = 1;
    else if (!getcustom(v1) && !getcustom(v2))
        result = 0;
    else
        PyObject_Cmp((PyObject *)getcustom(v1),
                     (PyObject *)getcustom(v2), &result);
    return result;
}

#else /* PY_MAJOR_VERSION <= 2 */

static int pycompare(value v1, value v2)
{
    int result;

    if (getcustom(v1) && !getcustom(v2))
        result = -1;
    else if (getcustom(v2) && !getcustom(v1))
        result = 1;
    else if (!getcustom(v1) && !getcustom(v2))
        result = 0;
    else if (1 == PyObject_RichCompareBool((PyObject *) getcustom(v1),
                                      (PyObject *) getcustom(v2), Py_EQ))
        result = 0;
    else if (1 == PyObject_RichCompareBool((PyObject *) getcustom(v1),
                                           (PyObject *) getcustom(v2), Py_LT))
        result = -1;
    else if (1 == PyObject_RichCompareBool((PyObject *) getcustom(v1),
                                           (PyObject *) getcustom(v2), Py_GT))
        result = 1;
    else
        result = -1;        /* Is there a better value to put here? */

    return result;
}

#endif /* PY_MAJOR_VERSION <= 2 */

static long pyhash( value v )
{
    if (getcustom(v))
        return PyObject_Hash((PyObject *)getcustom(v));
    else
        return 0L;
}

static unsigned long pydeserialize( void *dst )
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

struct custom_operations fnops =
{
    "FuncPointer",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

static value pywrap(PyObject *obj)
{
    CAMLparam0();
    CAMLlocal1(v);

    if (obj == NULL)
        CAMLreturn(Val_int(0));

    if (obj == Py_None)
        CAMLreturn(Val_int(1));

    Py_INCREF(obj);

    v = caml_alloc_custom( &pyops, sizeof( PyObject * ), 100, 30000000 );
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

/* T.F.: we may want to pywrap in such a way that we steal the reference: */
static value pywrap_steal( PyObject *obj )
{
    CAMLparam0();
    CAMLlocal1(v);

    if (obj == NULL)
        CAMLreturn(Val_int(0));

    if (obj == Py_None)
        CAMLreturn(Val_int(1));

    v = caml_alloc_custom( &pyops, sizeof( PyObject * ), 100, 30000000 );
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

static PyObject *
pyunwrap( value v )
{
    if (Is_long(v))
        switch (Int_val(v)) {
        case 0:
            return NULL;
        case 1:
            return Py_None;
        }

    return *((PyObject **)Data_custom_val(v));
}

#if USE_PYCAPSULE

static void
caml_destructor(PyObject *v, const char *capsule_name)
{
    value *valptr = (value *) PyCapsule_GetPointer(v, capsule_name);
    caml_remove_global_root(valptr);
    free(valptr);
}

static void
camldestr(PyObject *v)
{
    caml_destructor(v, "caml-other");
}

static void
camldestr_pill(PyObject *v)
{
    caml_destructor(v, "caml-pill");
}

#else /* USE_PYCAPSULE */

static void
camldestr(void *v)
{
    value *valptr = (value *) v;
    /* printf("DDD camlwrap remove_global_root(0x%08x)\n",valptr);fflush(stdout); */
    caml_remove_global_root(valptr);
    free(v);
}

static void
camldestr_pill(void *v, void *unused_dummy_receiving_ocamlpill_token)
{
    value *valptr = (value *) v;
    /* printf("DDD camlwrap remove_global_root(0x%08x)\n",valptr);fflush(stdout); */
    caml_remove_global_root(valptr);
    free(v);
}

#endif /* USE_PYCAPSULE */

/* T.F. Extension: the pill token is a subtle hack:

   One problem is that, as it seems, there are
   some python objects around which would be regarded as
   being of OtherType in original PyCaml.

   As these are opaque, we do not really have a good way
   to discern them from OCaml pills, and passing such an
   opaque value (which cannot be investigated systematically)
   where an ocaml pill is expected is bound to result
   in crashes (which we want to avoid).

   How to get rid of this? We somehow have to be able to properly
   identify OCaml Pills and extend the python types with CamlType.

   We do this by using the PyCObject_Check to determine c-object type,
   and abuse the closure parameter that is passed on to the destructor
   (which can be queried independently, and actually is not used by our
   destructor) as a token designating OCaml pills.

   Barry Schwartz: PyCapsule has a name field that can be used for
   identification, in place of the PyCObject hack described above.

*/
#if !USE_PYCAPSULE
static const char *ocamlpill_token = "CAML";
#endif

static PyObject *
camlwrap(value val, void *aux_str, int size)
{
    value *v = (value *) xmalloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v + sizeof(value), aux_str, size);
    caml_register_global_root(v);
    /* printf("DDD camlwrap caml_register_global_root(0x%08x)\n",v);fflush(stdout); */
#if USE_PYCAPSULE
    return PyCapsule_New(v, "caml-other", camldestr);
#else
    return PyCObject_FromVoidPtr(v, camldestr);
#endif
}

static PyObject *
camlwrap_pill(value val, void *aux_str, int size)
{
    value *v = (value *) xmalloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v + sizeof(value), aux_str, size);
    caml_register_global_root(v);
#if USE_PYCAPSULE
    return PyCapsule_New(v, "caml-pill", camldestr_pill);
#else
    return PyCObject_FromVoidPtrAndDesc(v, (void*)ocamlpill_token, camldestr_pill);
#endif
}


static void *
caml_aux(PyObject *obj)
{
#if USE_PYCAPSULE
    value *v = (value *) PyCapsule_GetPointer(obj, "caml-other");
#else
    value *v = (value *) PyCObject_AsVoidPtr(obj);
#endif
    return (void *) v + sizeof(value);
}

/*
PyObject *pycall_callback_buggy( PyObject *obj, PyObject *args ) {
    value out;
    value *v;
    
    if( !PyCObject_Check(obj) ) {
	Py_INCREF(Py_None);
	return Py_None;
    }
    v = (value *)PyCObject_AsVoidPtr( obj );
    out = caml_callback(*v,pywrap(args));
    return pyunwrap(out);
}
*/

/* T.F.: - I think the definition above is flawed...
   Looking at the definitions of OCAML macros in memory.h,
   this is how I suppose it should work:
*/

PyObject *pycall_callback( PyObject *obj, PyObject *args )
{
    CAMLparam0();
    CAMLlocal3(ml_out, ml_func, ml_args);
    PyObject *out;

#if USE_PYCAPSULE
    void *p = PyCapsule_GetPointer(obj, "caml-other");
    if (p == NULL)
        {
          Py_INCREF(Py_None);
          return Py_None;
        }
    ml_func = * (value *) p;
#else
    if (!PyCObject_Check(obj))
        {
          Py_INCREF(Py_None);
          return Py_None;
        }

    ml_func = * (value *) PyCObject_AsVoidPtr(obj);
#endif
    ml_args = pywrap(args);
    ml_out = caml_callback(ml_func, ml_args);
    out = pyunwrap(ml_out);
    /* T.F.:
       The result which we have now is borrowed - most probably, 
       there is only one reference to it which says
       "I am reachable through the ML heap".
       We have to properly transfer ownership, and hence
       see that we own that reference:
    */
    Py_XINCREF(out);
    CAMLreturnT(PyObject *, out);
}

/*-----------------------------------------------------------------------*/

static FILE *make_FILE(int fd_int)
{
    int fd_duplicate;

    fd_duplicate = dup(fd_int);
    return fdopen(fd_duplicate, "r+");
}

/*-----------------------------------------------------------------------*/

value pynull(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(pywrap(0));
}

value pynone(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_None));
}

value py_true(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_True));
}

value py_false(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_False));
}

/*-----------------------------------------------------------------------*/

#define Type1(func)                                          \
    CAMLprim value func##_wrapper(value unit)                \
    {                                                        \
        CAMLparam1(unit);                                    \
        func();                                              \
        CAMLreturn(Val_unit);                                \
    }

Type1(Py_Initialize)
Type1(Py_Finalize)
Type1(PyErr_Print)
Type1(PyErr_Clear)
Type1(PyImport_Cleanup)

/*-----------------------------------------------------------------------*/

#define Type2(func)                                          \
    CAMLprim value func##_wrapper(value obj)                 \
    {                                                        \
        CAMLparam1(obj);                                     \
                                                             \
        func(Int_val(obj));                                  \
        CAMLreturn(Val_unit);                                \
    }

Type2(Py_Exit)
Type2(PyErr_PrintEx)

/*-----------------------------------------------------------------------*/

#if PY_MAJOR_VERSION <= 2
    
#define Type3(func)                                          \
    CAMLprim value func##_wrapper(value obj)                 \
    {                                                        \
        CAMLparam1(obj);                                     \
                                                             \
        func(String_val(obj));                               \
        CAMLreturn(Val_unit);                                \
    }

#else

#define Type3(func)                                          \
    CAMLprim value func##_wrapper(value obj)                 \
    {                                                        \
        CAMLparam1(obj);                                     \
                                                             \
        char *s = String_val(obj);                           \
        wchar_t *ws = copy_to_wide_string(s);                \
        func(ws);                                            \
        free(ws);                                            \
        CAMLreturn(Val_unit);                                \
    }

#endif

Type3(Py_SetProgramName)
Type3(Py_SetPythonHome)

/*-----------------------------------------------------------------------*/

#define Type4(func)                                          \
    CAMLprim value func##_wrapper(value unit)                \
    {                                                        \
        CAMLparam1(unit);                                    \
                                                             \
        int result = func();                                 \
        CAMLreturn(Val_int(result));                         \
    }

Type4(Py_IsInitialized)

#if PY_MAJOR_VERSION <= 2
Type4(PyEval_GetRestricted)
#endif

/*-----------------------------------------------------------------------*/

#define Type5(func)                                          \
    CAMLprim value func##_wrapper(value obj)                 \
    {                                                        \
        CAMLparam1(obj);                                     \
                                                             \
        int result = func(String_val(obj));                  \
        CAMLreturn(Val_int(result));                         \
    }

Type5(PyRun_SimpleString)
Type5(PyImport_ImportFrozenModule)

/*-----------------------------------------------------------------------*/

/* Perhaps these should take a wrapped (FILE*) as argument instead of
 * an integer file descriptor. */
    
#define Type6(func)                                          \
    CAMLprim value func##_wrapper(value py_args)             \
    {                                                        \
        CAMLparam1(py_args);                                 \
                                                             \
        FILE *f = make_FILE(Int_val(Field(py_args, 0)));     \
        int result = func(f, String_val(Field(py_args, 1))); \
        fclose(f);                                           \
        CAMLreturn(Val_int(result));                         \
    }

Type6(PyRun_AnyFile)
Type6(PyRun_SimpleFile)
Type6(PyRun_InteractiveOne)
Type6(PyRun_InteractiveLoop)
Type6(Py_FdIsInteractive)

/*-----------------------------------------------------------------------*/

/* Perhaps these should take a wrapped (FILE*) as argument instead of
 * an integer file descriptor. */
    
#define Type7(func)                                             \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        FILE *f = make_FILE(Int_val(Field(py_args, 0)));        \
        int result = func(f,                                    \
                          String_val(Field(py_args, 1)),        \
                          Int_val(Field(py_args, 2)));          \
        fclose(f);                                              \
        CAMLreturn(Val_int(result));                            \
    }

Type7(PyRun_AnyFileEx)
Type7(PyRun_SimpleFileEx)

/*-----------------------------------------------------------------------*/

#define Type8(func)                                          \
    CAMLprim value func##_wrapper(value unit)                \
    {                                                        \
        CAMLparam1(unit);                                    \
        CAMLreturn(caml_copy_string(func()));                \
    }

#if PY_MAJOR_VERSION <= 2
#define Type8a Type8
#else
#define Type8a(func)                                           \
    CAMLprim value func##_wrapper(value unit)                  \
    {                                                          \
        CAMLparam1(unit);                                      \
        CAMLlocal1(string);                                    \
        wchar_t *ws;                                           \
        char *s;                                               \
                                                               \
        ws = func();                                           \
        if (ws == NULL)                                        \
            string = pynull(Val_unit);                         \
        else                                                   \
            {                                                  \
                s = copy_from_wide_string(ws);                 \
                if (s == NULL)                                 \
                    string = pynull(Val_unit);                 \
                else                                           \
                    string = caml_copy_string(s);              \
            }                                                  \
        CAMLreturn(string);                                    \
    }
#endif

Type8(Py_GetVersion)
Type8(Py_GetPlatform)
Type8(Py_GetCopyright)
Type8(Py_GetCompiler)
Type8(Py_GetBuildInfo)

Type8a(Py_GetProgramName)
Type8a(Py_GetPythonHome)
Type8a(Py_GetProgramFullPath)
Type8a(Py_GetPrefix)
Type8a(Py_GetExecPrefix)
Type8a(Py_GetPath)

/*-----------------------------------------------------------------------*/    

#define Type9(func, wrap_obj)                                   \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        PyObject *new_obj = func(String_val(Field(py_args, 0)), \
                                 Int_val(Field(py_args, 1)),    \
                                 pyunwrap(Field(py_args, 2)),   \
                                 pyunwrap(Field(py_args, 3)));  \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

Type9(PyRun_String, pywrap_steal)

/*-----------------------------------------------------------------------*/

/* Perhaps these should take a wrapped (FILE*) as argument instead of
 * an integer file descriptor. */
    
#define Type10(func, wrap_obj)                                  \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        FILE *f = make_FILE(Int_val(Field(py_args, 0)));        \
        PyObject *new_obj = func(f,                             \
                                 String_val(Field(py_args, 1)), \
                                 Int_val(Field(py_args, 2)),    \
                                 pyunwrap(Field(py_args, 3)),   \
                                 pyunwrap(Field(py_args, 4)));  \
        fclose(f);                                              \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

Type10(PyRun_File, pywrap_steal)

/*-----------------------------------------------------------------------*/

/* Perhaps these should take a wrapped (FILE*) as argument instead of
 * an integer file descriptor. */
    
#define Type11(func, wrap_obj)                                  \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        FILE *f = make_FILE(Int_val(Field(py_args, 0)));        \
        PyObject *new_obj = func(f,                             \
                                 String_val(Field(py_args, 1)), \
                                 Int_val(Field(py_args, 2)),    \
                                 pyunwrap(Field(py_args, 3)),   \
                                 pyunwrap(Field(py_args, 4)),   \
                                 Int_val(Field(py_args, 5)));   \
        fclose(f);                                              \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

Type11(PyRun_FileEx, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type12(func, wrap_obj)                                  \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        PyObject *new_obj = func(String_val(Field(py_args, 0)), \
                                 String_val(Field(py_args, 1)), \
                                 Int_val(Field(py_args, 2)));   \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

Type12(Py_CompileString, pywrap_steal)

/*-----------------------------------------------------------------------*/

/* Perhaps these should take a wrapped (FILE*) as argument instead of
 * an integer file descriptor. */
    
#define Type13(func)                                        \
    CAMLprim value func##_wrapper(value py_args)            \
    {                                                       \
        CAMLparam1(py_args);                                \
                                                            \
        FILE *f = make_FILE(Int_val(Field(py_args, 1)));    \
        int result = func(pyunwrap(Field(py_args, 0)),      \
                          f,                                \
                          Int_val(Field(py_args, 2)));      \
        fclose(f);                                          \
        CAMLreturn(Val_int(result));                        \
    }

Type13(PyObject_Print)

/*-----------------------------------------------------------------------*/

#define Type_GetSlice(func, wrap_obj)                           \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),   \
                                 Int_val(Field(py_args, 1)),    \
                                 Int_val(Field(py_args, 2)));   \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

Type_GetSlice(PySequence_GetSlice, pywrap_steal)
Type_GetSlice(PyTuple_GetSlice, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type14(func, wrap_obj)                      \
    CAMLprim value func##_wrapper(value obj)        \
    {                                               \
        CAMLparam1(obj);                            \
                                                    \
        PyObject *new_obj = func(pyunwrap(obj));    \
        CAMLreturn(wrap_obj(new_obj));              \
    }

#if PY_MAJOR_VERSION <= 2
#define Type14a(func, substitute, wrap_obj) Type14(func, wrap_obj)
#else
#define Type14a(func, substitute, wrap_obj)             \
    CAMLprim value func##_wrapper(value obj)            \
    {                                                   \
        CAMLparam1(obj);                                \
                                                        \
        PyObject *new_obj = substitute(pyunwrap(obj));  \
        CAMLreturn(wrap_obj(new_obj));                  \
    }
#endif    

Type14(PyMethod_Function, pywrap)
Type14(PyMethod_Self, pywrap)
Type14(PyModule_GetDict, pywrap)

#if PY_MAJOR_VERSION <= 2
Type14(PyMethod_Class, pywrap)
#endif

#if PY_MAJOR_VERSION >= 3
Type14(PyUnicode_AsUTF8String, pywrap_steal)
Type14(PyUnicode_AsUTF16String, pywrap_steal)
Type14(PyUnicode_AsUTF32String, pywrap_steal)
#endif
Type14(PyObject_Repr, pywrap_steal)
Type14(PyImport_ReloadModule, pywrap_steal)
Type14(PyImport_Import, pywrap_steal)
Type14(PyObject_Str, pywrap_steal)
Type14(PyObject_Type, pywrap_steal)
Type14(PyDict_Keys, pywrap_steal)
Type14(PyDict_Values, pywrap_steal)
Type14(PyDict_Items, pywrap_steal)
Type14(PyDict_Copy, pywrap_steal)
Type14(PySequence_Tuple, pywrap_steal)
Type14(PySequence_List, pywrap_steal)
Type14(PyNumber_Long, pywrap_steal)
Type14(PyNumber_Float, pywrap_steal)
Type14(PyNumber_Negative, pywrap_steal)
Type14(PyNumber_Positive, pywrap_steal)
Type14(PyNumber_Absolute, pywrap_steal)
Type14(PyNumber_Invert, pywrap_steal)
Type14(PyIter_Next, pywrap_steal)
#if PY_MAJOR_VERSION >= 3
Type14(PyInstanceMethod_New, pywrap_steal);
#endif

Type14a(PyObject_Unicode, PyObject_Str, pywrap_steal)
Type14a(PyNumber_Int, PyNumber_Long, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type15(func, wrap_obj)                                      \
    CAMLprim value func##_wrapper(value py_args)                    \
    {                                                               \
        CAMLparam1(py_args);                                        \
                                                                    \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),       \
                                 pyunwrap(Field(py_args, 1)),       \
                                 Int_val(Field(py_args, 2)));       \
        CAMLreturn(wrap_obj(new_obj));                              \
    }

Type15(PyObject_RichCompare, pywrap_steal);

/*-----------------------------------------------------------------------*/

#define Type16(func, wrap_obj)                                      \
    CAMLprim value func##_wrapper(value py_args)                    \
    {                                                               \
        CAMLparam1(py_args);                                        \
                                                                    \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),       \
                                 String_val(Field(py_args, 1)));    \
        CAMLreturn(wrap_obj(new_obj));                              \
    }

Type16(PyDict_GetItemString, pywrap)

Type16(PyObject_GetAttrString, pywrap_steal)
Type16(PySequence_Fast, pywrap_steal)    
Type16(PyMapping_GetItemString, pywrap_steal)    

/*-----------------------------------------------------------------------*/

#define Type17(func, wrap_obj)                                  \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),   \
                                 pyunwrap(Field(py_args, 1)));  \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

Type17(PyDict_GetItem, pywrap)

Type17(PyEval_CallObject, pywrap_steal)

#if PY_MAJOR_VERSION <= 2
Type17(PyBytes_Format, pywrap_steal)
#endif

#if PY_MAJOR_VERSION <= 2
Type17(PyInstance_NewRaw, pywrap_steal)
#endif

Type17(PySequence_Concat, pywrap_steal)
Type17(PySequence_InPlaceConcat, pywrap_steal)

Type17(PyObject_GetAttr, pywrap_steal)
Type17(PyObject_GetItem, pywrap_steal)

Type17(PyNumber_Add, pywrap_steal)
Type17(PyNumber_Subtract, pywrap_steal)
Type17(PyNumber_Multiply, pywrap_steal)
Type17(PyNumber_Remainder, pywrap_steal)
Type17(PyNumber_Divmod, pywrap_steal)
Type17(PyNumber_TrueDivide, pywrap_steal)
Type17(PyNumber_FloorDivide, pywrap_steal)

#if PY_MAJOR_VERSION <= 2
Type17(PyNumber_Divide, pywrap_steal)
#endif

Type17(PyNumber_Lshift, pywrap_steal)
Type17(PyNumber_Rshift, pywrap_steal)
Type17(PyNumber_And, pywrap_steal)
Type17(PyNumber_Xor, pywrap_steal)
Type17(PyNumber_Or, pywrap_steal)

Type17(PyNumber_InPlaceAdd, pywrap_steal)
Type17(PyNumber_InPlaceSubtract, pywrap_steal)
Type17(PyNumber_InPlaceMultiply, pywrap_steal)
Type17(PyNumber_InPlaceTrueDivide, pywrap_steal)
Type17(PyNumber_InPlaceFloorDivide, pywrap_steal)
Type17(PyNumber_InPlaceRemainder, pywrap_steal)
Type17(PyNumber_InPlaceLshift, pywrap_steal)
Type17(PyNumber_InPlaceRshift, pywrap_steal)
Type17(PyNumber_InPlaceAnd, pywrap_steal)
Type17(PyNumber_InPlaceXor, pywrap_steal)
Type17(PyNumber_InPlaceOr, pywrap_steal)

#if PY_MAJOR_VERSION <= 2
Type17(PyNumber_InPlaceDivide, pywrap_steal)
#endif

/*-----------------------------------------------------------------------*/
    
#define Type18(func)                                            \
    CAMLprim value func##_wrapper(value obj)                    \
    {                                                           \
        CAMLparam1(obj);                                        \
                                                                \
        int result = func(pyunwrap(obj));                       \
        CAMLreturn(Val_int(result));                            \
     }

Type18(PyObject_IsTrue)
Type18(PyObject_Not)
Type18(PyCallable_Check)
Type18(PyBytes_Size)
Type18(PyDict_Size)
Type18(PyTuple_Size)
Type18(PyErr_ExceptionMatches)
Type18(PyObject_Size)
Type18(PyNumber_Check)
Type18(PySequence_Check)
Type18(PySequence_Size)
Type18(PySequence_Length)
Type18(PyMapping_Check)
Type18(PyMapping_Size)
Type18(PyMapping_Length)
Type18(PyIter_Check)
#if PY_MAJOR_VERSION >= 3
Type18(PyUnicode_GetSize)
Type18(PyUnicode_Check)
#endif
Type18(PyBytes_Check)


/*-----------------------------------------------------------------------*/

#define Type19(func)                                            \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        int result = func(pyunwrap(Field(py_args, 0)),          \
                          pyunwrap(Field(py_args, 1)));         \
        CAMLreturn(Val_int(result));                            \
    }

Type19(PyObject_HasAttr)
Type19(PyObject_DelItem)
Type19(PyDict_DelItem)
Type19(PyErr_GivenExceptionMatches)
Type19(PySequence_Count)
Type19(PySequence_Contains)
Type19(PySequence_In)
Type19(PySequence_Index)
Type19(PyMapping_HasKey)

#if PY_MAJOR_VERSION <= 2
Type19(PyObject_Compare)
#endif

/*-----------------------------------------------------------------------*/

#define Type20(func)                                                \
    CAMLprim value func##_wrapper(value py_args)                    \
    {                                                               \
        CAMLparam1(py_args);                                        \
                                                                    \
        int result = func(pyunwrap(Field(py_args, 0)),              \
                          pyunwrap(Field(py_args, 1)),              \
                          Int_val(Field(py_args, 2)));              \
        CAMLreturn(Val_int(result));                                \
    }

Type20(PyObject_RichCompareBool)

/*-----------------------------------------------------------------------*/

#define Type21(func)                                                \
    CAMLprim value func##_wrapper(value py_args)                    \
    {                                                               \
        CAMLparam1(py_args);                                        \
                                                                    \
        int result = func(pyunwrap(Field(py_args, 0)),              \
                          String_val(Field(py_args, 1)),            \
                          pyunwrap(Field(py_args, 2)));             \
        CAMLreturn(Val_int(result));                                \
    }

Type21(PyObject_SetAttrString)
Type21(PyDict_SetItemString)
Type21(PyMapping_SetItemString)
Type21(PyModule_AddObject)

/*-----------------------------------------------------------------------*/

#define Type22(func)                                                    \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
                                                                        \
        int result = func(pyunwrap(Field(py_args, 0)),                  \
                          String_val(Field(py_args, 1)));               \
        CAMLreturn(Val_int(result));                                    \
    }

Type22(PyMapping_HasKeyString)
Type22(PyObject_HasAttrString)
Type22(PyDict_DelItemString)

/*-----------------------------------------------------------------------*/

/*
  Type23 not implemented:
  PyNumber_Coerce
  PyNumber_CoerceEx
*/

/*-----------------------------------------------------------------------*/

#define Type24(func)                                    \
  CAMLprim value func##_wrapper(value py_args)          \
  {                                                     \
      CAMLparam1(py_args);                              \
                                                        \
      int result = func(pyunwrap(Field(py_args, 0)),    \
                        pyunwrap(Field(py_args, 1)),    \
                        pyunwrap(Field(py_args, 2)));   \
      CAMLreturn(Val_int(result));                      \
  }

Type24(PyObject_SetAttr)
Type24(PyObject_SetItem)
Type24(PyDict_SetItem)

/*-----------------------------------------------------------------------*/

#define Type25(func)                                        \
      CAMLprim value func##_wrapper(value obj)              \
      {                                                     \
          CAMLparam1(obj);                                  \
          CAMLreturn(copy_int64(func(pyunwrap(obj))));      \
      }

#if PY_MAJOR_VERSION <= 2
#define Type25a(func, substitute) Type25(func)
#else
#define Type25a(func, substitute)                               \
    CAMLprim value func##_wrapper(value obj)                    \
    {                                                           \
        CAMLparam1(obj);                                        \
        CAMLreturn(copy_int64(substitute(pyunwrap(obj))));      \
    }
#endif

Type25(PyObject_Hash)
Type25a(PyInt_AsLong, PyLong_AsLong)
 
/*-----------------------------------------------------------------------*/

#define Type26(func, byte_type)                             \
    CAMLprim value func##_wrapper(value obj)                \
    {                                                       \
        CAMLparam1(obj);                                    \
        CAMLlocal1(string);                                 \
                                                            \
        byte_type *s = func(pyunwrap(obj));                 \
        if (s == NULL)                                      \
            string = pynull(Val_unit);                      \
        else                                                \
            string = caml_copy_string(s);                   \
        CAMLreturn(string);                                 \
    }

Type26(PyBytes_AsString, char)
Type26(PyModule_GetName, const char)
Type26(PyModule_GetFilename, const char)

/*-----------------------------------------------------------------------*/

#define Type28(func, wrap_obj)                                      \
    CAMLprim value func##_wrapper(value obj)                        \
    {                                                               \
        CAMLparam1(obj);                                            \
                                                                    \
        PyObject *result = func(String_val(obj));                   \
        CAMLreturn(wrap_obj(result));                               \
    }

Type28(PyImport_AddModule, pywrap)

Type28(PyBytes_FromString, pywrap_steal)
#if PY_MAJOR_VERSION >= 3
Type28(PyUnicode_FromString, pywrap_steal)
#endif
Type28(PyModule_New, pywrap_steal)
Type28(PyImport_ImportModule, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type29(func, wrap_obj)                                  \
    CAMLprim value func##_wrapper(value unit)                   \
    {                                                           \
        CAMLparam1(unit);                                       \
                                                                \
        PyObject *result = func();                              \
        CAMLreturn(wrap_obj(result));                           \
    }

Type29(PyErr_Occurred, pywrap)
Type29(PyImport_GetModuleDict, pywrap)
Type29(PyEval_GetBuiltins, pywrap)
Type29(PyEval_GetGlobals, pywrap)
Type29(PyEval_GetLocals, pywrap)
/* Type29(PyEval_GetFrame, pywrap)  -- FIX: Should return wrapped (PyFrameObject*), not wrapped (Object*). */

Type29(PyDict_New, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type30(func)                                            \
    CAMLprim value func##_wrapper(value obj)                    \
    {                                                           \
        CAMLparam1(obj);                                        \
                                                                \
        func(pyunwrap(obj));                                    \
        CAMLreturn(Val_unit);                                   \
    }

Type30(PyDict_Clear)
Type30(PyErr_SetNone)

/*-----------------------------------------------------------------------*/

/*
  Type31 -- currently not implemented:
  PyDict_Next
*/

/*-----------------------------------------------------------------------*/

#define Type34(func, wrap_obj)                                          \
    CAMLprim value func##_wrapper(value obj)                            \
    {                                                                   \
        CAMLparam1(obj);                                                \
                                                                        \
        PyObject *new_obj = func(Int64_val(obj));                       \
        CAMLreturn(wrap_obj(new_obj));                                  \
    }

#if PY_MAJOR_VERSION <= 2

#define Type34a(func, substitute, wrap_obj) Type34(func, wrap_obj)

#else

#define Type34a(func, substitute, wrap_obj)                             \
    CAMLprim value func##_wrapper(value obj)                            \
    {                                                                   \
        CAMLparam1(obj);                                                \
                                                                        \
        PyObject *new_obj = substitute(Int64_val(obj));                 \
        CAMLreturn(wrap_obj(new_obj));                                  \
    }

#endif

Type34a(PyInt_FromLong, PyLong_FromLong, pywrap_steal)

/*-----------------------------------------------------------------------*/
        
#define Type35(func)                                                    \
    CAMLprim value func##_wrapper(value unit)                           \
    {                                                                   \
        CAMLparam1(unit);                                               \
        CAMLreturn(copy_int64(func()));                                 \
    }

#if PY_MAJOR_VERSION <= 2
Type35(PyInt_GetMax)
#endif

Type35(PyImport_GetMagicNumber)

/*-----------------------------------------------------------------------*/

#define Type36(func, wrap_obj)                                          \
    CAMLprim value func##_wrapper(value obj)                            \
    {                                                                   \
        CAMLparam1(obj);                                                \
                                                                        \
        PyObject *new_obj = func(Double_val(obj));                      \
        CAMLreturn(wrap_obj(new_obj));                                  \
    }

Type36(PyFloat_FromDouble, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type37(func)                                                    \
    CAMLprim value func##_wrapper(value obj)                            \
    {                                                                   \
        CAMLparam1(obj);                                                \
        CAMLreturn(copy_double(func(pyunwrap(obj))));                   \
    }

Type37(PyFloat_AsDouble)

/*-----------------------------------------------------------------------*/

#define Type39(func, wrap_obj)                                          \
    CAMLprim value func##_wrapper(value obj)                            \
    {                                                                   \
        CAMLparam1(obj);                                                \
                                                                        \
        PyObject *new_obj = func(Int_val(obj));                         \
        CAMLreturn(wrap_obj(new_obj));                                  \
    }

Type39(PyTuple_New, pywrap_steal);

/*-----------------------------------------------------------------------*/

#define Type40(func, wrap_obj)                                          \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
                                                                        \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),           \
                                 Int_val(Field(py_args, 1)));           \
        CAMLreturn(wrap_obj(new_obj));                                  \
    }

Type40(PyTuple_GetItem, pywrap)

Type40(PySequence_InPlaceRepeat, pywrap_steal)
Type40(PySequence_Repeat, pywrap_steal)
Type40(PySequence_GetItem, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type40b(func)                                                   \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
                                                                        \
        int result = func(pyunwrap(Field(py_args, 0)),                  \
                          Int_val(Field(py_args, 1)));                  \
        CAMLreturn(Val_int(result));                                    \
    }

Type40b(PySequence_DelItem)

/*-----------------------------------------------------------------------*/

/* |do_steal| here means: "do we steal the arg[2] reference (which is
   an OCaml reference)?"  If our function is stealing, we first have
   to get a reference. */

#define Type41(func, do_steal)                                          \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
                                                                        \
        PyObject *x = pyunwrap(Field(py_args, 2));                      \
        if (do_steal)                                                   \
            Py_INCREF(x);                                               \
        CAMLreturn(Val_int(func(pyunwrap(Field(py_args, 0)),            \
                                Int_val(Field(py_args, 1)),             \
                                x)));                                   \
    }


Type41(PySequence_SetItem, 0)

Type41(PyTuple_SetItem, 1)

/*-----------------------------------------------------------------------*/

#define Type42(func, wrap_obj)                                  \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),   \
                                 pyunwrap(Field(py_args, 1)),   \
                                 pyunwrap(Field(py_args, 2)));  \
        CAMLreturn(wrap_obj(new_obj));                          \
    }

#if PY_MAJOR_VERSION <= 2
#define Type42a Type42
#else
#define Type42a(func, wrap_obj)                                 \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        PyObject *new_obj = func(pyunwrap(Field(py_args, 0)),   \
                                 pyunwrap(Field(py_args, 2)));  \
        CAMLreturn(wrap_obj(new_obj));                          \
    }
#endif

Type42(PySlice_New, pywrap_steal)
Type42(PyEval_CallObjectWithKeywords, pywrap_steal)
Type42(PyNumber_Power, pywrap_steal)
Type42(PyNumber_InPlacePower, pywrap_steal)

#if PY_MAJOR_VERSION <= 2
Type42(PyClass_New, pywrap_steal)
Type42(PyInstance_New, pywrap_steal)
#else
/* Calls the builtin-function: type(name,bases,dict), with the
 * name of the class, tuples of parent names, and dictionary
 * with initializations of fields.
 */
CAMLprim value PyClass_New_wrapper(value py_args)
{
  CAMLparam1(py_args);
  PyObject *bases = pyunwrap(Field(py_args, 0));
  PyObject *dict = pyunwrap(Field(py_args, 1));
  PyObject *name = pyunwrap(Field(py_args, 2));
  PyObject *new_obj = 
    PyObject_CallFunctionObjArgs
    ( (PyObject *) &PyType_Type, name, bases, dict, NULL);
  CAMLreturn(pywrap_steal(new_obj));
}
#endif

Type42a(PyMethod_New, pywrap_steal)

/*-----------------------------------------------------------------------*/

/*
Type43(PySlice_GetIndices) <-- Not supported in this version.
*/

/*-----------------------------------------------------------------------*/

#define Type45(func)                                            \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        func(pyunwrap(Field(py_args, 0)),                       \
             pyunwrap(Field(py_args, 1)));                      \
        CAMLreturn(Val_unit);                                   \
    }

Type45(PyErr_SetObject)

/*-----------------------------------------------------------------------*/
        
#define Type46(func)                                            \
    CAMLprim value func##_wrapper(value py_args)                \
    {                                                           \
        CAMLparam1(py_args);                                    \
                                                                \
        func(pyunwrap(Field(py_args, 0)),                       \
             String_val(Field(py_args, 1)));                    \
        CAMLreturn(Val_unit);                                   \
    }

Type46(PyErr_SetString)

/*-----------------------------------------------------------------------*/
    
#define Type47(func, wrap_obj)                                          \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
        CAMLlocal1(result);                                             \
                                                                        \
        PyObject *obj1 = pyunwrap(Field(py_args, 0));                   \
        PyObject *obj2 = pyunwrap(Field(py_args, 1));                   \
        PyObject *obj3 = pyunwrap(Field(py_args, 2));                   \
        func(&obj1, &obj2, &obj3);                                      \
        result = caml_alloc_tuple(3);                                   \
        Store_field(result, 0, wrap_obj(obj1));                         \
        Store_field(result, 1, wrap_obj(obj2));                         \
        Store_field(result, 2, wrap_obj(obj3));                         \
        CAMLreturn(result);                                             \
    }

Type47(PyErr_Fetch, pywrap_steal)
Type47(PyErr_NormalizeException, pywrap_steal)

  /*-----------------------------------------------------------------------*/

#define Type48(func)                            \
  CAMLprim value func##_wrapper(value py_args)  \
  {                                             \
      CAMLparam1(py_args);                      \
                                                \
      func(pyunwrap(Field(py_args, 0)),         \
           pyunwrap(Field(py_args, 1)),         \
           pyunwrap(Field(py_args, 2)));        \
      CAMLreturn(Val_unit);                     \
  }

Type48(PyErr_Restore)

/*-----------------------------------------------------------------------*/


#define Type49(func, wrap_obj)                                      \
      CAMLprim value func##_wrapper(value py_args)                  \
      {                                                             \
          CAMLparam1(py_args);                                      \
                                                                    \
          PyObject *result = func(String_val(Field(py_args, 0)),    \
                                  pyunwrap(Field(py_args, 1)));     \
          CAMLreturn(wrap_obj(result));                             \
      }

Type49(PyImport_ExecCodeModule, pywrap_steal)      

/*-----------------------------------------------------------------------*/

#define Type50(func, wrap_obj)                                      \
      CAMLprim value func##_wrapper(value py_args)                  \
      {                                                             \
          CAMLparam1(py_args);                                      \
                                                                    \
          PyObject *result = func(String_val(Field(py_args, 0)),    \
                                  pyunwrap(Field(py_args, 1)),      \
                                  String_val(Field(py_args, 2)));   \
          CAMLreturn(wrap_obj(result));                             \
      }

Type50(PyImport_ExecCodeModuleEx, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type51(func, wrap_obj)                                      \
      CAMLprim value func##_wrapper(value py_args)                  \
      {                                                             \
          CAMLparam1(py_args);                                      \
                                                                    \
          PyObject *result = func(String_val(Field(py_args, 0)),    \
                                  pyunwrap(Field(py_args, 1)),      \
                                  pyunwrap(Field(py_args, 2)),      \
                                  pyunwrap(Field(py_args, 3)));     \
          CAMLreturn(wrap_obj(result));                             \
      }

Type51(PyImport_ImportModuleEx, pywrap_steal)

/*-----------------------------------------------------------------------*/

#define Type52(func, byte_type)                                 \
      CAMLprim value func##_wrapper(value obj)                  \
      {                                                         \
          CAMLparam1(obj);                                      \
          CAMLlocal1(string);                                   \
                                                                \
          int return_val;                                       \
          byte_type *buffer;                                    \
          Py_ssize_t length;                                    \
                                                                \
          return_val = func(pyunwrap(obj), &buffer, &length);   \
          if (return_val == -1) {                               \
              string = pynull(Val_unit);                        \
          } else {                                              \
              string = caml_alloc_string(length);               \
              memcpy(String_val(string), buffer, length);       \
          }                                                     \
          CAMLreturn(string);                                   \
      }

      Type52(PyBytes_AsStringAndSize, char)
      Type52(PyObject_AsCharBuffer, const char)
      Type52(PyObject_AsReadBuffer, const void)
      Type52(PyObject_AsWriteBuffer, void)

/*-----------------------------------------------------------------------*/

#define Type53(func)                                        \
      CAMLprim value func##_wrapper(value py_args)          \
      {                                                     \
          CAMLparam1(py_args);                              \
                                                            \
          int result = func(pyunwrap(Field(py_args, 0)),    \
                            Int_val(Field(py_args, 1)),     \
                            Int_val(Field(py_args, 2)),     \
                            pyunwrap(Field(py_args, 3)));   \
          CAMLreturn(Val_int(result));                      \
      }

      Type53(PySequence_SetSlice)

/*-----------------------------------------------------------------------*/

#define Type54(func)                                        \
      CAMLprim value func##_wrapper(value py_args)          \
      {                                                     \
          CAMLparam1(py_args);                              \
                                                            \
          int result = func(pyunwrap(Field(py_args, 0)),    \
                            Int_val(Field(py_args, 1)),     \
                            Int_val(Field(py_args, 2)));    \
          CAMLreturn(Val_int(result));                      \
      }

      Type54(PySequence_DelSlice)

/*-----------------------------------------------------------------------*/
#if PY_MAJOR_VERSION >= 3
#define TypeUTF8Decoder(func, wrap_obj)                                 \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
                                                                        \
        PyObject *result;                                               \
                                                                        \
        char *utf8 = String_val(Field(py_args, 0));                     \
        Py_ssize_t utf8_length = caml_string_length(Field(py_args, 0)); \
                                                                        \
        if (Field(py_args, 1) == Val_int(0))                            \
            result = func(utf8, utf8_length, NULL);                     \
        else                                                            \
            result = func(utf8, utf8_length,                            \
                          String_val(Field(Field(py_args, 1), 0)));     \
        CAMLreturn(wrap_obj(result));                                   \
    }

TypeUTF8Decoder(PyUnicode_DecodeUTF8, pywrap_steal)
#endif
/*-----------------------------------------------------------------------*/
#if 0
#define TypeUTF16Decoder(func, wrap_obj)                                \
    CAMLprim value func##_wrapper(value py_args)                        \
    {                                                                   \
        CAMLparam1(py_args);                                            \
                                                                        \
        PyObject *result;                                               \
        char *errors;                                                   \
                                                                        \
        char *s = String_val(Field(py_args, 0));                        \
        Py_ssize_t s_length = caml_string_length(Field(py_args, 0));    \
                                                                        \
        if (Field(py_args, 1) == Val_int(0))                            \
            errors = NULL;                                              \
        else                                                            \
            errors = String_val(Field(Field(py_args, 1), 0));           \
                                                                        \
        if (Field(py_args, 2) == Val_int(0))                            \
            result = func(s, s_length, errors, NULL);                   \
        else {                                                          \
            int byteorder = Int_val(Field(Field(py_args, 2), 0));       \
            result = func(s, s_length, errors, &byteorder);             \
        }                                                               \
                                                                        \
        CAMLreturn(wrap_obj(result));                                   \
    }

TypeUTF16Decoder(PyUnicode_DecodeUTF16, pywrap_steal)
TypeUTF16Decoder(PyUnicode_DecodeUTF32, pywrap_steal)

/*-----------------------------------------------------------------------*/

CAMLprim value PyUnicode_FromUnicode_wrapper(value closure, value length)
{
    CAMLparam2(closure, length);
    CAMLlocal2(index, val); /* We need named intermediate values for
                             * garbage collection. */

    Py_ssize_t len = Int_val(length);
    Py_ssize_t i;
    PyObject *result;

    result = PyUnicode_FromUnicode(NULL, len);
    if (result != NULL) {
        Py_UNICODE *p = PyUnicode_AS_UNICODE(result);
        for (i = 0; i < len; i++) {
            index = Val_int(i);
            val = caml_callback(closure, index);
            p[i] = Int_val(val) & 0x7FFFFFFF;
        }
    }

    CAMLreturn(pywrap_steal(result));
}

CAMLprim value PyUnicode_AsUnicode_wrapper(value uni)
{
    CAMLparam1(uni);
    CAMLlocal1(result);

    PyObject *py_uni = pyunwrap(uni);
    Py_UNICODE *p = PyUnicode_AsUnicode(py_uni);

    if (p != NULL) {
        Py_ssize_t len = PyUnicode_GET_SIZE(py_uni);
        Py_ssize_t i;

        result = caml_alloc(len, 0);
        for (i = 0; i < len; i++)
            Store_field(result, i, Val_int(p[i]));
    }

    CAMLreturn(result);
}
#endif

CAMLprim value PyUnicode_AsEncodedString_wrapper(value py_args)
{
    CAMLparam1(py_args);

    PyObject *result = PyUnicode_AsEncodedString(
        pyunwrap(Field(py_args, 0)),
        String_val(Field(py_args, 1)),
        String_val(Field(py_args, 2)));

    CAMLreturn(pywrap_steal(result));
}

/*-----------------------------------------------------------------------*/

/* Value -> Pyobject */

value pywrapvalue( value cb ) {
    CAMLparam1(cb);
    CAMLreturn(pywrap_steal(camlwrap(cb,NULL,0)));
    /* T.F.: camlwrap already gives us a reference. We steal that. */
}

/* For pills, we use an extension: */
value pywrapvalue_pill( value cb ) {
    CAMLparam1(cb);
    CAMLreturn(pywrap_steal(camlwrap_pill(cb,NULL,0)));
}


value pycaml_seterror(value ml_err,value ml_str)
{
  CAMLparam2(ml_err,ml_str);
  PyObject *err;
  int nr_err;

  nr_err=Int_val(ml_err);

  switch(nr_err) {
      case 0:
          err=PyExc_Exception;
          break;
      case 1:
#if PY_MAJOR_VERSION <= 2
          err=PyExc_StandardError;
          break;
#else
          /* PyExc_StandardError is obsolete. Maybe it would be better
           * to raise an OCaml exception. */
          err=PyExc_Exception;
          break;
#endif
      case 2:
          err=PyExc_ArithmeticError;
          break;
      case 3:
          err=PyExc_LookupError;
          break;
      case 4:
          err=PyExc_AssertionError;
          break;
      case 5:
          err=PyExc_AttributeError;
          break;
      case 6:
          err=PyExc_EOFError;
          break;
      case 7:
          err=PyExc_EnvironmentError;
          break;
      case 8:
          err=PyExc_FloatingPointError;
          break;
      case 9:
          err=PyExc_IOError;
          break;
      case 10:
          err=PyExc_ImportError;
          break;
      case 11:
          err=PyExc_IndexError;
          break;
      case 12:
          err=PyExc_KeyError;
          break;
      case 13:
          err=PyExc_KeyboardInterrupt;
          break;
      case 14:
          err=PyExc_MemoryError;
          break;
      case 15:
          err=PyExc_NameError;
          break;
      case 16:
          err=PyExc_NotImplementedError;
          break;
      case 17:
          err=PyExc_OSError;
          break;
      case 18:
          err=PyExc_OverflowError;
          break;
      case 19:
          err=PyExc_ReferenceError;
          break;
      case 20:
          err=PyExc_RuntimeError;
          break;
      case 21:
          err=PyExc_SyntaxError;
          break;
      case 22:
          err=PyExc_SystemExit;
          break;
      case 23:
          err=PyExc_TypeError;
          break;
      case 24:
          err=PyExc_ValueError;
          break;
      case 25:
          err=PyExc_ZeroDivisionError;
          break;
      default:
          /* Maybe it would be better here to raise an OCaml
           * exception. */
#if PY_MAJOR_VERSION <= 2
          err=PyExc_StandardError;
#else
          err=PyExc_Exception;
#endif
  }

  PyErr_SetString(err,String_val(ml_str));
  CAMLreturn(Val_unit);
}

value pyunwrapvalue(value cb)
{
    CAMLparam1(cb);
    value *v;
#if USE_PYCAPSULE
    v = (value *) PyCapsule_GetPointer(pyunwrap(cb), "caml-pill");
    if (v == NULL)
        v = (value *) PyCapsule_GetPointer(pyunwrap(cb), "caml-other");
#else
    v = (value *) PyCObject_AsVoidPtr(pyunwrap(cb));
#endif
    CAMLreturn(*v);
}

/* Create the function table */

typedef struct _python_func_table {
    void *func;
    int format;
    int steal_result;
    char *desc;
} python_func_table;

int PyRun_SimpleString_(const char *command)
{
    return PyRun_SimpleStringFlags(command, NULL);
}

int PyRun_SimpleFile_(FILE *fp, const char *filename)
{
    return PyRun_SimpleFileExFlags(fp, filename, 0, NULL);
}

int PyRun_AnyFile_(FILE *fp, const char *filename)
{
    return PyRun_AnyFileExFlags(fp, filename, 0, NULL);
}

int PyRun_InteractiveOne_(FILE *fp, const char *filename)
{
    return PyRun_InteractiveOneFlags(fp, filename, NULL);
}

int PyRun_InteractiveLoop_(FILE *fp, const char *filename)
{
    return PyRun_InteractiveLoopFlags(fp, filename, NULL);
}

int PyRun_AnyFileEx_(FILE *fp, const char *filename, int closeit)
{
    return PyRun_AnyFileExFlags(fp, filename, closeit, NULL);
}

int PyRun_SimpleFileEx_(FILE *fp, const char *filename, int closeit)
{
    return PyRun_SimpleFileExFlags(fp, filename, closeit, NULL);
}

PyObject* PyRun_String_(const char *str, int start, PyObject *globals, PyObject *locals)
{
    return PyRun_StringFlags(str, start, globals, locals, NULL);
}

PyObject* PyRun_File_(FILE *fp, const char *filename, int start, PyObject *globals, PyObject *locals)
{
    return PyRun_FileExFlags(fp, filename, start, globals, locals, 0, NULL);
}

PyObject* PyRun_FileEx_(FILE *fp, const char *filename, int start, PyObject *globals, PyObject *locals, int closeit)
{
    return PyRun_FileExFlags(fp, filename, start, globals, locals, closeit, NULL);
}

PyObject* Py_CompileString_(const char *str, const char *filename, int start)
{
    return Py_CompileStringFlags(str, filename, start, NULL);
}

PyObject* PyImport_ImportModuleEx_(char *name, PyObject *globals, PyObject *locals, PyObject *fromlist)
{
    return PyImport_ImportModuleLevel(name, globals, locals, fromlist, -1);
}

enum PycamlTypeLabels { 
    TupleType = 0,
    BytesType,
    UnicodeType,
    BoolType,
    IntType,
    FloatType,
    ListType,
    NoneType,
    CallableType,
    ModuleType,
    ClassType,
    TypeType,
    DictType,
    NullType,
    CamlpillType,
    OtherType,
    EitherStringType, /* Signifies that either of BytesType or UnicodeType is allowed. */
    CamlpillSubtype, /* Signifies that only the particular Camlpill variety is allowed. */
    AnyType          /* Allow any python object. */
};

value pytype( value obj ) {
    CAMLparam1(obj);
    PyObject *pobj = pyunwrap( obj );
    if( !pobj ) CAMLreturn(NullType);
    else if( PyTuple_Check( pobj ) ) CAMLreturn(Val_int(TupleType));
    else if( PyBytes_Check( pobj ) ) CAMLreturn(Val_int(BytesType));
    else if( PyUnicode_Check( pobj ) ) CAMLreturn(Val_int(UnicodeType));
    else if( PyBool_Check( pobj ) ) CAMLreturn(Val_int(BoolType));
#if PY_MAJOR_VERSION <= 2
    else if( PyInt_Check( pobj ) ) CAMLreturn(Val_int(IntType));
#else
    else if( PyLong_Check( pobj ) ) CAMLreturn(Val_int(IntType));
#endif
    else if( PyFloat_Check( pobj ) ) CAMLreturn(Val_int(FloatType));
    else if( PyList_Check( pobj ) ) CAMLreturn(Val_int(ListType));
    else if( pobj == Py_None ) CAMLreturn(Val_int(NoneType));
    else if( PyCallable_Check( pobj ) ) CAMLreturn(Val_int(CallableType));
    else if( PyModule_Check( pobj ) ) CAMLreturn(Val_int(ModuleType));
#if PY_MAJOR_VERSION <= 2
    else if( PyClass_Check( pobj ) ) CAMLreturn(Val_int(ClassType));
#endif
    else if( PyType_Check( pobj ) ) CAMLreturn(Val_int(TypeType));
    else if( PyDict_Check( pobj ) ) CAMLreturn(Val_int(DictType));
#if USE_PYCAPSULE
    else if (PyCapsule_IsValid(pobj, "caml-pill"))
        CAMLreturn(Val_int(CamlpillType));
#else /* USE_PYCAPSULE */
    else if (PyCObject_Check(pobj))
      {
          void *desc = PyCObject_GetDesc(pobj);
          if (desc == (void *) ocamlpill_token)
                  CAMLreturn(Val_int(CamlpillType));
          else
                  CAMLreturn(Val_int(OtherType));
      }
#endif /* USE_PYCAPSULE */
    else
        CAMLreturn(Val_int(OtherType));
}

value pytuple_fromarray( value array ) {
    CAMLparam1(array);
    PyObject *tuple = PyTuple_New(Wosize_val(array));
    int i;

    for( i = 0; i < Wosize_val(array); i++ )
      {
	PyObject *entry;
	entry=pyunwrap(Field(array,i));
	/* T.F.:
	   entry's reference count was increased by one because it is visible
	   from within OCaml (and OCaml's GC will take care of decreasing
	   this again upon finalization. But now, we do add another use to
	   entry. So, we have to increase its reference count manually:

	   Note that even if tuple contained some python value before
	   (which it does not), we do not have to Py_DECREF the
	   reference count on the old entry, as the PyTuple_SetItem/PyList_SetItem
	   does this of its own! Nice, isn't it?
	*/
	Py_INCREF(entry);
	PyTuple_SetItem(tuple,i,entry);
      }

    CAMLreturn(pywrap_steal(tuple));
}

value pytuple_toarray( value array ) {
    CAMLparam1(array);
    PyObject *obj = pyunwrap(array);
    int i;
    CAMLlocal1(rv);

    rv = caml_alloc_tuple(PySequence_Size(obj));
    /* XXX T.F.: actually, using caml_alloc_tuple to get an array is not overly aesthetic... */

    for (i = 0; i < PySequence_Size(obj); i++)
	Store_field(rv, i, pywrap_steal(PySequence_GetItem(obj, i)));

    CAMLreturn(rv);
}

value pywrap_closure( value closure ) {
    CAMLparam1(closure);
    PyMethodDef ml;
    PyObject *obj;
    PyMethodDef *ml_def;
    ml.ml_name = "anonymous_closure";
    ml.ml_meth = pycall_callback;
    ml.ml_flags = 1;
    ml.ml_doc = "Anonymous closure";
    obj = camlwrap(closure, &ml, sizeof(ml));
    ml_def = (PyMethodDef *) caml_aux(obj);
    CAMLreturn(pywrap_steal(PyCFunction_New(ml_def, obj)));
}

/*
value pymodule_initmodule( value name, value funclist ) { ... }
Removed by T.F., as this piece of code seemed quite buggy....
*/

/* -- T.F. Extensions -- */

/*
  In case of "You used it the wrong way" or "this should not happen",
  we want to be able to just bailout from python into ocaml with an
  exception.


  Note: this is not being used, as we decided to do error handling the
  other way round: python is in charge, ocaml lies "beneath the surface",
  so we effectively always bailout into python.
 */

#if 0
static int pycaml_raise_error(int type, char *message)
{
  CAMLlocal1(ex);
  ex=caml_alloc_tuple(3);
  Store_field(ex,0,Val_int(type));
  Store_field(ex,1,caml_copy_string(message));
  raise_with_arg(*caml_named_value("ocaml_exn_pycaml"),ex);
}
#endif

/* This is just an adjusted copy of pytuple_fromarray. */
value pylist_fromarray( value array ) {
    CAMLparam1(array);
    PyObject *list = PyList_New(Wosize_val(array));
    int i;

    for( i = 0; i < Wosize_val(array); i++ )
      {
	PyObject *entry;
	entry=pyunwrap(Field(array,i));
	/* T.F.: See pytuple_fromarray code comments! */
	Py_INCREF(entry);
	PyList_SetItem(list,i,entry);
      }
    CAMLreturn(pywrap_steal(list));
}

/* We also need it the other way round */
value pylist_toarray( value pylist ) {
    CAMLparam1(pylist);
    PyObject *obj = pyunwrap(pylist);
    int i,len;
    CAMLlocal1(rv);

    rv = caml_alloc_tuple( PySequence_Size(obj) );

    len=PySequence_Size(obj);

    for( i = 0; i < len; i++ )
	Store_field(rv,i,pywrap_steal(PySequence_GetItem(obj,i)));

    CAMLreturn(rv);
}

value pylist_set( value pylist, value index, value v ) {
  CAMLparam3(pylist,index,v);
  PyObject *list, *new_entry;
  
  list = pyunwrap(pylist);
  new_entry=pyunwrap(v);
  Py_INCREF(new_entry);
  PyList_SetItem(list,Int_val(index),new_entry);
  
  CAMLreturn(Val_unit);
}

value pylist_get( value pylist, value index) {
  CAMLparam2(pylist,index);
  PyObject *list = pyunwrap(pylist);
  
  /* T.F.: According to the Python docs, we own the reference produced by
     PySequence_GetItem. Hence, we have to steal that reference...
   */
  CAMLreturn(pywrap_steal(PySequence_GetItem(list,Int_val(index))));
}



/* It's nice to have this variant of pywrap_closure */
value pywrap_closure_docstring(value docstring, value closure) {
  CAMLparam2(docstring, closure);
  PyMethodDef ml;
  PyObject *obj;
  PyMethodDef *ml_def;
  ml.ml_name = "anonymous_closure";
  ml.ml_meth = pycall_callback;
  ml.ml_flags = 1;
  ml.ml_doc = String_val(docstring);
  obj = camlwrap(closure,&ml,sizeof(ml));
  ml_def = (PyMethodDef *) caml_aux(obj);
  CAMLreturn(pywrap_steal(PyCFunction_New(ml_def,obj)));
}

/* Using pyrun_interactiveloop the way it was in the original code
   may work on some systems, but then just by chance. We have to
   do this in a cleaner way:
 */
value pycaml_prompt(value ml_unit) {
  CAMLparam1(ml_unit);
  
  PyRun_InteractiveLoop(stdin,"<stdin>");

  CAMLreturn(Val_unit);
}


/* The function below is highly useful for debugging! */
value pyrefcount(value pyobj) {
  CAMLparam1(pyobj);
  PyObject *obj = pyunwrap(pyobj);
  
  CAMLreturn(Val_int(obj->ob_refcnt));
}
