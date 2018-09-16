#ifndef _PYML_STUBS_H_
#define _PYML_STUBS_H_
#include <stdbool.h>
#include <unistd.h>

/* The following definitions are extracted and simplified from
#include <Python.h>
*/

typedef ssize_t Py_ssize_t;

#define _PyObject_HEAD_EXTRA

#define PyObject_HEAD                   \
    _PyObject_HEAD_EXTRA                \
    Py_ssize_t ob_refcnt;               \
    PyObject *ob_type;

typedef void PyObject;

typedef struct {
    Py_ssize_t ob_refcnt;
    PyObject *ob_type;
} PyObjectDescr;

PyObjectDescr *pyobjectdescr(PyObject *obj);

typedef struct {
    PyObjectDescr ob_base;
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
    void *tp_methods;
    void *tp_members;
    void *tp_getset;
    void *tp_base;
    PyObject *tp_dict;
    void *tp_descr_get;
    void *tp_descr_set;
    Py_ssize_t tp_dictoffset;
    void *tp_init;
    void *tp_alloc;
    void *tp_new;
    void *tp_free;
    void *tp_is_gc;
    PyObject *tp_bases;
    PyObject *tp_mro;
    PyObject *tp_cache;
    PyObject *tp_subclasses;
    PyObject *tp_weaklist;
    void *tp_del;
    unsigned int tp_version_tag;
    void *tp_finalize;
/* #ifdef COUNT_ALLOCS */
    Py_ssize_t tp_allocs;
    Py_ssize_t tp_frees;
    Py_ssize_t tp_maxalloc;
    struct _typeobject *tp_prev;
    struct _typeobject *tp_next;
/* #endif */
} PyTypeObject;

void
pyml_assert_initialized();

void
pyml_assert_python2();

void
pyml_assert_ucs2();

void
pyml_assert_ucs4();

void
pyml_assert_python3();

value
pyml_wrap(PyObject *object, bool steal);

PyObject *
pyml_unwrap(value v);

/* Numpy */

/* from ndarraytypes.h */

enum NPY_TYPES {
    NPY_BOOL=0,
                    NPY_BYTE, NPY_UBYTE,
                    NPY_SHORT, NPY_USHORT,
                    NPY_INT, NPY_UINT,
                    NPY_LONG, NPY_ULONG,
                    NPY_LONGLONG, NPY_ULONGLONG,
                    NPY_FLOAT, NPY_DOUBLE, NPY_LONGDOUBLE,
                    NPY_CFLOAT, NPY_CDOUBLE, NPY_CLONGDOUBLE,
                    NPY_OBJECT=17,
                    NPY_STRING, NPY_UNICODE,
                    NPY_VOID,
                    /*
                     * New 1.6 types appended, may be integrated
                     * into the above in 2.0.
                     */
                    NPY_DATETIME, NPY_TIMEDELTA, NPY_HALF,

                    NPY_NTYPES,
                    NPY_NOTYPE,
                    NPY_CHAR,      /* special flag */
                    NPY_USERDEF=256,  /* leave room for characters */

                    /* The number of types not including the new 1.6 types */
                    NPY_NTYPES_ABI_COMPATIBLE=21
};

#define NPY_ARRAY_C_CONTIGUOUS    0x0001
#define NPY_ARRAY_F_CONTIGUOUS    0x0002
#define NPY_ARRAY_ALIGNED         0x0100
#define NPY_ARRAY_WRITEABLE       0x0400

#define NPY_ARRAY_BEHAVED      (NPY_ARRAY_ALIGNED | \
                                NPY_ARRAY_WRITEABLE)
#define NPY_ARRAY_CARRAY       (NPY_ARRAY_C_CONTIGUOUS | \
                                NPY_ARRAY_BEHAVED)
#define NPY_ARRAY_FARRAY       (NPY_ARRAY_F_CONTIGUOUS | \
                                NPY_ARRAY_BEHAVED)

/* From pyport.h */
typedef intptr_t        Py_intptr_t;

/* From npy_common.h */
typedef Py_intptr_t npy_intp;

void **
pyml_get_pyarray_api(PyObject *c_api);

#define Py_INCREF(op)                                            \
    ((pyobjectdescr(op))->ob_refcnt++)

#define Py_XINCREF(op)                                           \
    do {                                                         \
        PyObjectDescr *_py_xincref_tmp =                         \
            pyobjectdescr((PyObject *)(op));                     \
        if (_py_xincref_tmp != NULL)                             \
            Py_INCREF(_py_xincref_tmp);                          \
    } while (0)

#define Py_DECREF(op)                                            \
    do {                                                         \
        PyObjectDescr *_py_decref_tmp =                          \
            pyobjectdescr((PyObject *)(op));                     \
        if (--(_py_decref_tmp)->ob_refcnt == 0)                  \
            ((struct _typeobject *)                              \
             pyobjectdescr(_py_decref_tmp->ob_type))             \
                ->tp_dealloc(op);                                \
    } while (0)

#endif /* _PYML_STUBS_H_ */
