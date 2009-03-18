/*
 * (C) arty 2002
 * This software is covered under the GNU lesser general public license
 */

#include "Python.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include <unistd.h>

#define true 1
#define false 0

static void *getcustom( value v ) { return *((void **)Data_custom_val(v)); }

static void pydecref( value v ) {
    if( getcustom(v) ) { Py_DECREF((PyObject *)getcustom(v)); }
}

static int pycompare( value v1, value v2 ) {
    int result;
    if( getcustom(v1) && !getcustom(v2) ) return -1;
    if( getcustom(v2) && !getcustom(v1) ) return 1;
    if( !getcustom(v1) && !getcustom(v2) ) return 0;
    PyObject_Cmp((PyObject *)getcustom(v1),
		 (PyObject *)getcustom(v2),&result);
    return result;
}

static long pyhash( value v ) {
    if( getcustom(v) ) return PyObject_Hash((PyObject *)getcustom(v));
    else return 0;
}

static unsigned long pydeserialize( void *dst ) {
    return 0;
}

struct custom_operations pyops = {
    "PythonObject",
    pydecref,
    pycompare,
    pyhash,
    custom_serialize_default,
    pydeserialize
};

struct custom_operations fnops = {
    "FuncPointer",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

value pywrap( PyObject *obj ) {
    CAMLparam0();
    CAMLlocal1(v);
    if( obj )
	Py_INCREF(obj);
    v = alloc_custom( &pyops, sizeof( PyObject * ), 100, 100000 );
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

value funcwrap( void *obj ) {
    CAMLparam0();
    CAMLlocal1(v);
    v = alloc_custom( &fnops, sizeof( void * ), 100, 100000 );
    *((void **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

PyObject *pyunwrap( value v ) {
    return *((PyObject **)Data_custom_val(v));
}

static void camldestr( void *v ) {
    value *valptr = (value *)v;
    remove_global_root(valptr);
    free( v );
}

PyObject *camlwrap( value val, void *aux_str, int size ) {
    value *v = (value *)malloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v+sizeof(value),aux_str,size);
    register_global_root(v);
    return PyCObject_FromVoidPtr(v,camldestr);
}

void *caml_aux( PyObject *obj ) {
    value *v = (value *)PyCObject_AsVoidPtr( obj );
    return (void *)v+sizeof(value);
}

PyObject *pycall_callback( PyObject *obj, PyObject *args ) {
    value out;
    value *v;
    
    if( !PyCObject_Check(obj) ) {
	Py_INCREF(Py_None);
	return Py_None;
    }
    v = (value *)PyCObject_AsVoidPtr( obj );
    out = callback(*v,pywrap(args));
    return pyunwrap(out);
}

typedef void  (*type_1)( void );
typedef void  (*type_2)( int );
typedef void  (*type_3)( char * );
typedef int   (*type_4)( void );
typedef int   (*type_5)( char * );
typedef int   (*type_6)( FILE *, char * );
typedef int   (*type_7)( FILE *, char *, int );
typedef char *(*type_8)( void );
typedef PyObject* (*type_9)(char*, int, PyObject *, PyObject *);
typedef PyObject* (*type_10)(FILE *, char*, int, PyObject *, PyObject *);
typedef PyObject* (*type_11)(FILE *, char*, int, PyObject *, PyObject *, int );
typedef PyObject* (*type_12)(char*, char*, int);
typedef int (*type_13)(PyObject *, FILE *, int);
typedef PyObject * (*type_14)(PyObject *);
typedef PyObject * (*type_15)(PyObject *, PyObject *, int);
typedef PyObject * (*type_16)(PyObject *, char *);
typedef PyObject * (*type_17)(PyObject *, PyObject *);
typedef int (*type_18)(PyObject *);
typedef int (*type_19)(PyObject *, PyObject *);
typedef int (*type_20)(PyObject *, PyObject *, int);
typedef int (*type_21)(PyObject *, char *, PyObject *);
typedef int (*type_22)(PyObject *, char *);
typedef int (*type_23)(PyObject **, PyObject **);
typedef int (*type_24)(PyObject *, PyObject *, PyObject *);
typedef long (*type_25)(PyObject *);
typedef char *(*type_26)(PyObject *);
typedef void (*type_27)(PyObject **, PyObject *);
typedef PyObject *(*type_28)(char *);
typedef PyObject *(*type_29)(void);
typedef void (*type_30)(PyObject *);
typedef int (*type_31)(PyObject *, int *, PyObject **, PyObject **);
typedef PyObject *(*type_32)(char*, char**, int);
typedef PyObject *(*type_33)(Py_UNICODE*, int, int);
typedef PyObject *(*type_34)(long);
typedef long (*type_35)(void);
typedef PyObject *(*type_36)(double);
typedef double (*type_37)(PyObject *);
typedef PyObject *(*type_38)(PyObject*, char**);
typedef PyObject *(*type_39)(int size);
typedef PyObject *(*type_40)(PyObject *, int);
typedef int (*type_41)(PyObject *, int, PyObject *);
typedef PyObject *(*type_42)(PyObject* start, PyObject* stop, PyObject* step);
typedef int (*type_43)(PySliceObject *r, int length, int *start, int *stop, int *step);
typedef PyObject *(*type_44)(long, long, long, int);
typedef void (*type_45)(PyObject *, PyObject *);
typedef void (*type_46)(PyObject *, const char *);
typedef void (*type_47)(PyObject **, PyObject **, PyObject **);
typedef void (*type_48)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*type_49)(PyObject *, char *);
typedef PyObject *(*type_50)(char *,PyObject *,char *);
typedef PyObject *(*type_51)(char *,PyObject *,PyObject *,PyObject *);
typedef int (*type_52)(PyObject *obj,const char **buffer,int *buffer_len);
typedef int (*type_53)(PyObject *o, int i1, int i2, PyObject *v);
typedef int (*type_54)(PyObject *o, int i1, int i2);

value pygencall( value format, value arg ) {
    CAMLparam2(format,arg);
    CAMLlocal1(rv);
    FILE *f;
    int fd;
    int x;
    int ret_int;
    const char *rvs;
    int fmt = Int_val(Field(format,1));
    PyObject *ob1,*ob2,*ob3;
    void *func = getcustom(Field(format,0));
    int reflect = Int_val(Field(format,2));

    rv = Val_unit;

    switch( fmt ) {
    case 1:
	((type_1)func)();
	CAMLreturn(Val_unit);

    case 2:
	((type_2)func)(Int_val(arg));
	CAMLreturn(Val_unit);

    case 3:
	((type_3)func)(String_val(arg));
	CAMLreturn(Val_unit);

    case 4:
	CAMLreturn(Int_val(((type_4)func)()));

    case 5:
	CAMLreturn(Int_val(((type_5)func)
			   (String_val(arg))));

    case 6:
    case 7:
    case 10:
    case 11:
	fd = dup(Int_val(Field(arg,0)));
	f = fdopen(fd,"r+");
	switch( fmt ) {
	case 6:
	    rv = Val_int(((type_6)func)
			 (f,String_val(Field(arg,1))));
	    break;

	case 7:
	    rv = Val_int(((type_7)func)
			 (f,
			  String_val(Field(arg,1)),
			  Int_val(Field(arg,2))));
	    break;

	case 10:
	    rv = pywrap(((type_10)func)
			(f,
			 String_val(Field(arg,1)),
			 Int_val(Field(arg,2)),
			 pyunwrap(Field(arg,3)),
			 pyunwrap(Field(arg,4))));
	    break;
	    
	case 11:
	    rv = pywrap(((type_11)func)
			(f,
			 String_val(Field(arg,1)),
			 Int_val(Field(arg,2)),
			 pyunwrap(Field(arg,3)),
			 pyunwrap(Field(arg,4)),
			 Int_val(Field(arg,5))));
	    break;
	}
	
	fclose( f );
	CAMLreturn( rv );
	
    case 8:
	CAMLreturn(copy_string(((type_8)func)()));

    case 9:
	CAMLreturn(pywrap(((type_9)func)
			  (String_val(Field(arg,0)),
			   Int_val(Field(arg,1)),
			   pyunwrap(Field(arg,2)),
			   pyunwrap(Field(arg,3)))));

    case 12:
	CAMLreturn(pywrap(((type_12)func)
			  (String_val(Field(arg,0)),
			   String_val(Field(arg,1)),
			   Int_val(Field(arg,2)))));

    case 13:
	fd = dup(Int_val(Field(arg,1)));
	f = fdopen(fd,"r+");
	rv = Val_int(((type_13)func)
		     (pyunwrap(Field(arg,0)),
		      f,
		      Int_val(Field(arg,2))));
	fclose( f );
	break;

    case 14:
	CAMLreturn(pywrap(((type_14)func)(pyunwrap(arg))));

    case 15:
	CAMLreturn(pywrap(((type_15)func)
			  (pyunwrap(Field(arg,0)),
			   pyunwrap(Field(arg,1)),Int_val(Field(arg,2)))));
	
    case 16:
	CAMLreturn(pywrap(((type_16)func)
			  (pyunwrap(Field(arg,0)),
			   String_val(Field(arg,1)))));
	
    case 17:
	CAMLreturn(pywrap(((type_17)func)
			  (pyunwrap(Field(arg,0)),pyunwrap(Field(arg,1)))));
	
    case 18:
	CAMLreturn(Val_int(((type_18)func)(pyunwrap(arg))));
	
    case 19:
	CAMLreturn(Val_int(((type_19)func)
			   (pyunwrap(Field(arg,0)),
			    pyunwrap(Field(arg,1)))));

    case 20:
	CAMLreturn(Val_int(((type_20)func)
			   (pyunwrap(Field(arg,0)),
			    pyunwrap(Field(arg,1)),
			    Int_val(Field(arg,2)))));

    case 21:
	CAMLreturn(Val_int(((type_21)func)
			   (pyunwrap(Field(arg,0)),
			    String_val(Field(arg,1)),
			    pyunwrap(Field(arg,2)))));

    case 22:
	CAMLreturn(Val_int(((type_22)func)
			   (pyunwrap(Field(arg,0)),
			    String_val(Field(arg,1)))));

    case 23:
	ob1 = pyunwrap(Field(arg,0)); ob2 = pyunwrap(Field(arg,1));
	ret_int = ((type_23)func)( &ob1, &ob2 );
	if( ret_int == -1 ) CAMLreturn((value)1);
	else {
	    rv = alloc_tuple(2);
	    Field(rv,0) = pywrap(ob1);
	    Field(rv,1) = pywrap(ob2);
	    CAMLreturn(rv);
	}

    case 24:
	CAMLreturn(Int_val(((type_24)func)
			   (pyunwrap(Field(arg,0)),
			    pyunwrap(Field(arg,1)),
			    pyunwrap(Field(arg,2)))));

    case 25:
	CAMLreturn(copy_int64(((type_25)func)(pyunwrap(arg))));

    case 26:
	CAMLreturn(copy_string(((type_26)func)(pyunwrap(arg))));

    case 27:
	ob1 = pyunwrap(Field(arg,0));
	((type_27)func)(&ob1,pyunwrap(Field(arg,1)));
	CAMLreturn(pywrap(ob1));

    case 28:
	CAMLreturn(pywrap(((type_28)func)(String_val(arg))));

    case 29:
	CAMLreturn(pywrap(((type_29)func)()));

    case 30:
	((type_30)func)(pyunwrap(arg));
	CAMLreturn(Val_unit);

    case 31:
	x = Int_val(Field(arg,1));
	ret_int = ((type_31)func)
	    (pyunwrap(Field(arg,0)),
	     &x,
	     &ob1, &ob2);
	if( !ret_int ) CAMLreturn((value)1);
	else {
	    rv = alloc_tuple(3);
	    Field(rv,0) = pywrap(ob1);
	    Field(rv,1) = pywrap(ob2);
	    Field(rv,2) = Val_int(x);
	    CAMLreturn(rv);
	}

	/* case 32: string -> int */

	/* case 33: unicode ... need to do something coherent */

    case 34:
	CAMLreturn(pywrap(((type_34)func)(Int64_val(arg))));

    case 35:
	CAMLreturn(copy_int64(((type_35)func)()));

    case 36:
	CAMLreturn(pywrap(((type_36)func)(Double_val(arg))));

    case 37:
	CAMLreturn(copy_double(((type_37)func)
			       (pyunwrap(arg))));

	/* case 38: string -> float */

    case 39:
	CAMLreturn(pywrap(((type_39)func)(Int_val(arg))));
	
    case 40:
	CAMLreturn(pywrap(((type_40)func)
			  (pyunwrap(Field(arg,0)),
			   Int_val(Field(arg,1)))));

    case 41:
	CAMLreturn(Val_int(((type_41)func)
			   (pyunwrap(Field(arg,0)),
			    Int_val(Field(arg,1)),
			    pyunwrap(Field(arg,2)))));

    case 42:
	CAMLreturn(pywrap(((type_42)func)
			  (pyunwrap(Field(arg,0)),
			   pyunwrap(Field(arg,1)),
			   pyunwrap(Field(arg,2)))));

    case 43: {
	int start,end,step;

	ret_int = ((type_43)func)
	    ((PySliceObject *)pyunwrap(Field(arg,0)),
	     Int_val(Field(arg,1)),
	     &start, &end, &step);
	if( !ret_int ) CAMLreturn((value)1);
	else {
	    rv = alloc_tuple(3);
	    Field(rv,0) = start;
	    Field(rv,1) = end;
	    Field(rv,2) = step;
	    CAMLreturn(rv);
	}
    }

    case 44:
	CAMLreturn(pywrap(((type_44)func)
			  (Int_val(Field(arg,0)),
			   Int_val(Field(arg,1)),
			   Int_val(Field(arg,2)),
			   Int_val(Field(arg,3)))));

    case 45:
	((type_45)func)
	    (pyunwrap(Field(arg,0)),
	     pyunwrap(Field(arg,1)));
	CAMLreturn(Val_unit);
	
    case 46:
	((type_46)func)
	    (pyunwrap(Field(arg,0)),String_val(Field(arg,1)));
	CAMLreturn(Val_unit);
	
    case 47:
	ob1 = pyunwrap(Field(arg,0));
	ob2 = pyunwrap(Field(arg,1));
	ob3 = pyunwrap(Field(arg,2));
	((type_47)func)(&ob1,&ob2,&ob3);
	rv = alloc_tuple(3);
	Field(rv,0) = pywrap(ob1);
	Field(rv,1) = pywrap(ob2);
	Field(rv,2) = pywrap(ob3);
	CAMLreturn(rv);
	
    case 48:
	((type_48)func)
	    (pyunwrap(Field(arg,0)),
	     pyunwrap(Field(arg,1)),
	     pyunwrap(Field(arg,2)));
	CAMLreturn(Val_unit);

    case 49:
	CAMLreturn(pywrap(((type_49)func)
			  (pyunwrap(Field(arg,0)),
			   String_val(Field(arg,1)))));
	
    case 50:
	CAMLreturn(pywrap(((type_50)func)
			  (String_val(Field(arg,0)),
			   pyunwrap(Field(arg,1)),
			   String_val(Field(arg,2)))));

    case 51:
	CAMLreturn(pywrap(((type_51)func)
			  (String_val(Field(arg,0)),
			   pyunwrap(Field(arg,1)),
			   pyunwrap(Field(arg,2)),
			   pyunwrap(Field(arg,3)))));

    case 52:
	((type_52)func)(pyunwrap(arg),&rvs,&ret_int);
	rv = copy_string(rvs);
	CAMLreturn(rv);

    case 53:
	CAMLreturn(Val_int(((type_53)func)
			   (pyunwrap(Field(arg,0)),
			    Int_val(Field(arg,1)),
			    Int_val(Field(arg,2)),
			    pyunwrap(Field(arg,3)))));

    case 54:
	CAMLreturn(Val_int(((type_54)func)
			   (pyunwrap(Field(arg,0)),
			    Int_val(Field(arg,1)),
			    Int_val(Field(arg,2)))));
    }
    
    CAMLreturn(rv);
}

#ifdef DONT_COMPILE_THIS
/* 1 */
DL_IMPORT(void) Py_Initialize(void);
DL_IMPORT(void) Py_Finalize(void);
DL_IMPORT(void) PyErr_Print(void);
/* 2 */
DL_IMPORT(void) Py_Exit(int);
DL_IMPORT(void) PyErr_PrintEx(int);
/* 3 */
DL_IMPORT(void) Py_SetProgramName(char *);
DL_IMPORT(void) Py_SetPythonHome(char *);
/* 4 */
DL_IMPORT(int) Py_IsInitialized(void);
/* 5 */
DL_IMPORT(int) PyRun_SimpleString(char *);
/* 6 */
DL_IMPORT(int) PyRun_AnyFile(FILE *, char *);
DL_IMPORT(int) PyRun_SimpleFile(FILE *, char *);
DL_IMPORT(int) PyRun_InteractiveOne(FILE *, char *);
DL_IMPORT(int) PyRun_InteractiveLoop(FILE *, char *);
DL_IMPORT(int) Py_FdIsInteractive(FILE *, char *);
/* 7 */
DL_IMPORT(int) PyRun_AnyFileEx(FILE *, char *, int);
DL_IMPORT(int) PyRun_SimpleFileEx(FILE *, char *, int);
/* 8 */
DL_IMPORT(char*) Py_GetProgramName(void);
DL_IMPORT(char*) Py_GetPythonHome(void);
DL_IMPORT(char*) Py_GetProgramFullPath(void);
DL_IMPORT(char*) Py_GetPrefix(void);
DL_IMPORT(char*) Py_GetExecPrefix(void);
DL_IMPORT(char*) Py_GetPath(void);
DL_IMPORT(char*) Py_GetVersion(void);
DL_IMPORT(char*) Py_GetPlatform(void);
DL_IMPORT(char*) Py_GetCopyright(void);
DL_IMPORT(char*) Py_GetCompiler(void);
DL_IMPORT(char*) Py_GetBuildInfo(void);
/* 9 */
DL_IMPORT(PyObject*) PyRun_String(char*, int, PyObject *, PyObject *);
/* 10 */
DL_IMPORT(PyObject*) PyRun_File(FILE *, char*, int, PyObject *, PyObject *);
/* 11 */
DL_IMPORT(PyObject*) PyRun_FileEx(FILE *, char*, int, PyObject *, PyObject *, int );
/* 12 */
DL_IMPORT(PyObject*) Py_CompileString(char*, char*, int); /* FUNCTION 30 */

/* Generic operations on objects */
/* 13 */
extern DL_IMPORT(int) PyObject_Print(PyObject *, FILE *, int);
/* 14 */
extern DL_IMPORT(PyObject *) PyObject_Repr(PyObject *);
extern DL_IMPORT(PyObject *) PyObject_Str(PyObject *);
extern DL_IMPORT(PyObject *) PyObject_Unicode(PyObject *);
/* 15 */
extern DL_IMPORT(PyObject *) PyObject_RichCompare(PyObject *, PyObject *, int);
/* 16 */
extern DL_IMPORT(PyObject *) PyObject_GetAttrString(PyObject *, char *);
/* 17 */
extern DL_IMPORT(PyObject *) PyObject_GetAttr(PyObject *, PyObject *);
extern DL_IMPORT(PyObject *) PyObject_CallObject(PyObject *, PyObject *);
/* 18 */
extern DL_IMPORT(int) PyObject_IsTrue(PyObject *);
extern DL_IMPORT(int) PyObject_Not(PyObject *);
extern DL_IMPORT(int) PyCallable_Check(PyObject *);
/* 19 */
extern DL_IMPORT(int) PyObject_Compare(PyObject *, PyObject *);
extern DL_IMPORT(int) PyObject_HasAttr(PyObject *, PyObject *);
/* 20 */
extern DL_IMPORT(int) PyObject_RichCompareBool(PyObject *, PyObject *, int);
/* 21 */
extern DL_IMPORT(int) PyObject_SetAttrString(PyObject *, char *, PyObject *);
/* 22 */
extern DL_IMPORT(int) PyObject_HasAttrString(PyObject *, char *);
/* 23 */
extern DL_IMPORT(int) PyNumber_Coerce(PyObject **, PyObject **);
extern DL_IMPORT(int) PyNumber_CoerceEx(PyObject **, PyObject **);
/* 24 */
extern DL_IMPORT(int) PyObject_SetAttr(PyObject *, PyObject *, PyObject *);
/* 25 */
extern DL_IMPORT(long) PyObject_Hash(PyObject *);

/* Strings */
/* 18 */
extern DL_IMPORT(int) PyString_Size(PyObject *);
/* 26 */
extern DL_IMPORT(char *) PyString_AsString(PyObject *);
/* 27 */
extern DL_IMPORT(void) PyString_Concat(PyObject **, PyObject *);
extern DL_IMPORT(void) PyString_ConcatAndDel(PyObject **, PyObject *);
/* 28 */
extern DL_IMPORT(PyObject *) PyString_FromString(const char *);
/* 17 */
extern DL_IMPORT(PyObject *) PyString_Format(PyObject *, PyObject *);

/* Dictionaries */
/* 29 */
extern DL_IMPORT(PyObject *) PyDict_New(void);
/* 17 */
extern DL_IMPORT(PyObject *) PyDict_GetItem(PyObject *mp, PyObject *key);
/* 24 */
extern DL_IMPORT(int) PyDict_SetItem(PyObject *mp, PyObject *key, PyObject *item);
/* 19 */
extern DL_IMPORT(int) PyDict_DelItem(PyObject *mp, PyObject *key);
/* 30 */
extern DL_IMPORT(void) PyDict_Clear(PyObject *mp);
/* 31 */
extern DL_IMPORT(int) PyDict_Next
	(PyObject *mp, int *pos, PyObject **key, PyObject **value);
/* 14 */
extern DL_IMPORT(PyObject *) PyDict_Keys(PyObject *mp);
extern DL_IMPORT(PyObject *) PyDict_Values(PyObject *mp);
extern DL_IMPORT(PyObject *) PyDict_Items(PyObject *mp);
extern DL_IMPORT(PyObject *) PyDict_Copy(PyObject *mp);
/* 18 */
extern DL_IMPORT(int) PyDict_Size(PyObject *mp);
/* 16 */
extern DL_IMPORT(PyObject *) PyDict_GetItemString(PyObject *dp, char *key);
/* 22 */
extern DL_IMPORT(int) PyDict_DelItemString(PyObject *dp, char *key);
/* 21 */
extern DL_IMPORT(int) PyDict_SetItemString(PyObject *dp, char *key, PyObject *item);

/* Integer */
/* 32 */
extern DL_IMPORT(PyObject *) PyInt_FromString(char*, char**, int);
/* 33 */
extern DL_IMPORT(PyObject *) PyInt_FromUnicode(Py_UNICODE*, int, int);
/* 34 */
extern DL_IMPORT(PyObject *) PyInt_FromLong(long);
/* 25 */
extern DL_IMPORT(long) PyInt_AsLong(PyObject *);
/* 35 */
extern DL_IMPORT(long) PyInt_GetMax(void);
/* Long */
/* 34 */
extern DL_IMPORT(PyObject *) PyLong_FromLong(long);
/* 36 */
extern DL_IMPORT(PyObject *) PyLong_FromDouble(double);
/* 25 */
extern DL_IMPORT(long) PyLong_AsLong(PyObject *);

/* Float */
/* 36 */
extern DL_IMPORT(PyObject *) PyFloat_FromDouble(double);
/* 37 */
extern DL_IMPORT(double) PyFloat_AsDouble(PyObject *);

/* Modules */
/* 28 */
extern DL_IMPORT(PyObject *) PyModule_New(char *);
/* 14 */
extern DL_IMPORT(PyObject *) PyModule_GetDict(PyObject *);
/* 26 */
extern DL_IMPORT(char *) PyModule_GetName(PyObject *);
extern DL_IMPORT(char *) PyModule_GetFilename(PyObject *);

/* 39 */
extern DL_IMPORT(PyObject *) PyTuple_New(int size);
/* 18 */
extern DL_IMPORT(int) PyTuple_Size(PyObject *);
extern DL_IMPORT(int) PyTuple_Check(PyObject *);
/* 40 */
extern DL_IMPORT(PyObject *) PyTuple_GetItem(PyObject *, int);
/* 41 */
extern DL_IMPORT(int) PyTuple_SetItem(PyObject *, int, PyObject *);
/* 13 */
extern DL_IMPORT(PyObject *) PyTuple_GetSlice(PyObject *, int, int);

/* 42 */
DL_IMPORT(PyObject *) PySlice_New(PyObject* start, PyObject* stop, PyObject* step);
/* 43 */
DL_IMPORT(int) PySlice_GetIndices(PySliceObject *r, int length, int *start, int *stop, int *step);
/* 44 */
DL_IMPORT(PyObject *) PyRange_New(long, long, long, int);

/* Error handling definitions */

/* 30 */
DL_IMPORT(void) PyErr_SetNone(PyObject *);
/* 45 */
DL_IMPORT(void) PyErr_SetObject(PyObject *, PyObject *);
/* 46 */
DL_IMPORT(void) PyErr_SetString(PyObject *, const char *);
/* 29 */
DL_IMPORT(PyObject *) PyErr_Occurred(void);
/* 1 */
DL_IMPORT(void) PyErr_Clear(void);
/* 47 */
DL_IMPORT(void) PyErr_Fetch(PyObject **, PyObject **, PyObject **);
/* 48 */
DL_IMPORT(void) PyErr_Restore(PyObject *, PyObject *, PyObject *);

/* Error testing and normalization */
/* 19 */
DL_IMPORT(int) PyErr_GivenExceptionMatches(PyObject *, PyObject *);
/* 18 */
DL_IMPORT(int) PyErr_ExceptionMatches(PyObject *);
/* 47 */
DL_IMPORT(void) PyErr_NormalizeException(PyObject**, PyObject**, PyObject**);

/* Classes */
/* 42 */
extern DL_IMPORT(PyObject *) PyClass_New(PyObject *, PyObject *, PyObject *);
/* 42 */
extern DL_IMPORT(PyObject *) PyInstance_New(PyObject *, PyObject *,
                                            PyObject *);
/* 17 */
extern DL_IMPORT(PyObject *) PyInstance_NewRaw(PyObject *, PyObject *);
/* 42 */
extern DL_IMPORT(PyObject *) PyMethod_New(PyObject *, PyObject *, PyObject *);
/* 14 */
extern DL_IMPORT(PyObject *) PyMethod_Function(PyObject *);
extern DL_IMPORT(PyObject *) PyMethod_Self(PyObject *);
extern DL_IMPORT(PyObject *) PyMethod_Class(PyObject *);

/* Module */
/* 28 */
extern DL_IMPORT(PyObject *) PyModule_New(char *);
/* 14 */
extern DL_IMPORT(PyObject *) PyModule_GetDict(PyObject *);
/* 26 */
extern DL_IMPORT(char *) PyModule_GetName(PyObject *);
extern DL_IMPORT(char *) PyModule_GetFilename(PyObject *);
/* 35 */
DL_IMPORT(long) PyImport_GetMagicNumber(void);
/* 49 */
DL_IMPORT(PyObject *) PyImport_ExecCodeModule(char *name, PyObject *co);
/* 50 */
DL_IMPORT(PyObject *) PyImport_ExecCodeModuleEx(char *name, PyObject *co, char *pathname);
/* 29 */
DL_IMPORT(PyObject *) PyImport_GetModuleDict(void);
/* 28 */
DL_IMPORT(PyObject *) PyImport_AddModule(char *name);
DL_IMPORT(PyObject *) PyImport_ImportModule(char *name);
/* 51 */
DL_IMPORT(PyObject *) PyImport_ImportModuleEx(char *name, PyObject *globals, PyObject *locals, PyObject *fromlist);
/* 28 */
DL_IMPORT(PyObject *) PyImport_Import(PyObject *name);
/* 14 */
DL_IMPORT(PyObject *) PyImport_ReloadModule(PyObject *m);
/* 1 */
DL_IMPORT(void) PyImport_Cleanup(void);
/* 5 */
DL_IMPORT(int) PyImport_ImportFrozenModule(char *);

/* Interface to random parts in ceval.c */
/* 42 */
DL_IMPORT(PyObject *) PyEval_CallObjectWithKeywords(PyObject *, PyObject *, PyObject *);
/* 17 */
DL_IMPORT(PyObject *) PyEval_CallObject(PyObject *, PyObject *);

/* 29 */
DL_IMPORT(PyObject *) PyEval_GetBuiltins(void);
DL_IMPORT(PyObject *) PyEval_GetGlobals(void);
DL_IMPORT(PyObject *) PyEval_GetLocals(void);
DL_IMPORT(PyObject *) PyEval_GetFrame(void);
/* 4 */
DL_IMPORT(int) PyEval_GetRestricted(void);

/* Abstract layer */
/* 14 */
DL_IMPORT(PyObject *) PyObject_Type(PyObject *o);
/* 18 */
DL_IMPORT(int) PyObject_Size(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyObject_GetItem(PyObject *o, PyObject *key);
/* 24 */
DL_IMPORT(int) PyObject_SetItem(PyObject *o, PyObject *key, PyObject *v);
/* 17 */
DL_IMPORT(int) PyObject_DelItem(PyObject *o, PyObject *key);
/* 52 */
DL_IMPORT(int) PyObject_AsCharBuffer(PyObject *obj,const char **buffer,int *buffer_len);
DL_IMPORT(int) PyObject_AsReadBuffer(PyObject *obj,const void **buffer,int *buffer_len);
DL_IMPORT(int) PyObject_AsWriteBuffer(PyObject *obj,void **buffer,int *buffer_len);
/* 18 */
DL_IMPORT(int) PyNumber_Check(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyNumber_Add(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Subtract(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Multiply(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Divide(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Remainder(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Divmod(PyObject *o1, PyObject *o2);
/* 42 */
DL_IMPORT(PyObject *) PyNumber_Power(PyObject *o1, PyObject *o2,PyObject *o3);
/* 14 */
DL_IMPORT(PyObject *) PyNumber_Negative(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Positive(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Absolute(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Invert(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyNumber_Lshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Rshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_And(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Xor(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Or(PyObject *o1, PyObject *o2);
/* 14 */
DL_IMPORT(PyObject *) PyNumber_Int(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Long(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Float(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyNumber_InPlaceAdd(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceSubtract(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceMultiply(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceDivide(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceRemainder(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceLshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceRshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceAnd(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceXor(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceOr(PyObject *o1, PyObject *o2);
/* 42 */
DL_IMPORT(PyObject *) PyNumber_InPlacePower(PyObject *o1, PyObject *o2,PyObject *o3);
/* 18 */
DL_IMPORT(int) PySequence_Check(PyObject *o);
DL_IMPORT(int) PySequence_Size(PyObject *o);
DL_IMPORT(int) PySequence_Length(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PySequence_Concat(PyObject *o1, PyObject *o2);
/* 40 */
DL_IMPORT(PyObject *) PySequence_Repeat(PyObject *o, int count);
DL_IMPORT(PyObject *) PySequence_GetItem(PyObject *o, int i);
/* 13 */
DL_IMPORT(PyObject *) PySequence_GetSlice(PyObject *o, int i1, int i2);
/* 41 */
DL_IMPORT(int) PySequence_SetItem(PyObject *o, int i, PyObject *v);
/* 20 */
DL_IMPORT(int) PySequence_DelItem(PyObject *o, int i);
/* 53 */
DL_IMPORT(int) PySequence_SetSlice(PyObject *o, int i1, int i2, PyObject *v);
/* 54 */
DL_IMPORT(int) PySequence_DelSlice(PyObject *o, int i1, int i2);
/* 14 */
DL_IMPORT(PyObject *) PySequence_Tuple(PyObject *o);
DL_IMPORT(PyObject *) PySequence_List(PyObject *o);
/* 16 */
DL_IMPORT(PyObject *) PySequence_Fast(PyObject *o, const char* m);
/* 19 */
DL_IMPORT(int) PySequence_Count(PyObject *o, PyObject *value);
DL_IMPORT(int) PySequence_Contains(PyObject *o, PyObject *value);
DL_IMPORT(int) PySequence_In(PyObject *o, PyObject *value);
DL_IMPORT(int) PySequence_Index(PyObject *o, PyObject *value);
/* 17 */
DL_IMPORT(PyObject *) PySequence_InPlaceConcat(PyObject *o1, PyObject *o2);
/* 22 */
DL_IMPORT(PyObject *) PySequence_InPlaceRepeat(PyObject *o, int count);
/* 18 */
DL_IMPORT(int) PyMapping_Check(PyObject *o);
DL_IMPORT(int) PyMapping_Size(PyObject *o);
DL_IMPORT(int) PyMapping_Length(PyObject *o);
/* 16 */
DL_IMPORT(int) PyMapping_HasKeyString(PyObject *o, char *key);
/* 19 */
DL_IMPORT(int) PyMapping_HasKey(PyObject *o, PyObject *key);
/* 16 */
DL_IMPORT(PyObject *) PyMapping_GetItemString(PyObject *o, char *key);
/* 41 */
DL_IMPORT(int) PyMapping_SetItemString(PyObject *o, char *key, PyObject *value);


#ifdef  MAYBE_RUN
DL_IMPORT(void) init_exceptions(void);
DL_IMPORT(void) fini_exceptions(void);
DL_IMPORT(void) _PyImport_Init(void);
DL_IMPORT(void) _PyImport_Fini(void);
DL_IMPORT(void) PyMethod_Fini(void);
DL_IMPORT(void) PyFrame_Fini(void);
DL_IMPORT(void) PyCFunction_Fini(void);
DL_IMPORT(void) PyTuple_Fini(void);
DL_IMPORT(void) PyString_Fini(void);
DL_IMPORT(void) PyInt_Fini(void);
DL_IMPORT(void) PyFloat_Fini(void);
DL_IMPORT(PyObject*) _PyBuiltin_Init(void);
DL_IMPORT(PyObject*) _PySys_Init(void);
DL_IMPORT(struct symtable *) Py_SymtableString(char*, char*, int);
DL_IMPORT(struct _node *) PyParser_SimpleParseString(char*, int);
DL_IMPORT(struct _node *) PyParser_SimpleParseFile(FILE *, char*, int);
DL_IMPORT(int) Py_AtExit(void (*func)(void));
DL_IMPORT(void) Py_EndInterpreter(PyThreadState *);
DL_IMPORT(PyThreadState *) Py_NewInterpreter(void);
DL_IMPORT(int) (*PyOS_InputHook)(void);
DL_IMPORT(int) PyOS_CheckStack(void);
DL_IMPORT(char) *(*PyOS_ReadlineFunctionPointer)(char*);
DL_IMPORT(PyOS_sighandler_t) PyOS_getsig(int);
DL_IMPORT(PyOS_sighandler_t) PyOS_setsig(int, PyOS_sighandler_t);
DL_IMPORT(void) PyOS_FiniInterrupts(void);
DL_IMPORT(char*) PyOS_Readline(char*);
#endif//MAYBE_RUN
#endif//DONT_COMPILE_THIS

/* HST FIXUP */
#if (PY_MAJOR_VERSION >= 2 && PY_MINOR_VERSION > 4)
#undef PyRun_SimpleString
#define PyRun_SimpleString PyRun_SimpleString_redo
int PyRun_SimpleString_redo(const char* command) { return PyRun_SimpleStringFlags(command, NULL); }

#undef PyRun_AnyFile
#define PyRun_AnyFile PyRun_AnyFile_redo
int PyRun_AnyFile_redo(FILE* fp, const char* filename) { return PyRun_AnyFileExFlags(fp, filename, 0, NULL); }

#undef PyRun_SimpleFile
#define PyRun_SimpleFile PyRun_SimpleFile_redo
int PyRun_SimpleFile_redo(FILE* fp, const char* filename) { return PyRun_SimpleFileExFlags(fp, filename, 0, NULL); }

#undef PyRun_InteractiveOne
#define PyRun_InteractiveOne PyRun_InteractiveOne_redo
int PyRun_InteractiveOne_redo(FILE* fp, const char* filename) { return PyRun_InteractiveOneFlags(fp, filename, NULL); }

#undef PyRun_InteractiveLoop
#define PyRun_InteractiveLoop PyRun_InteractiveLoop_redo
int PyRun_InteractiveLoop_redo(FILE* fp, const char* filename) { return PyRun_InteractiveLoopFlags(fp, filename, NULL); }

#undef PyRun_AnyFileEx
#define PyRun_AnyFileEx PyRun_AnyFileEx_redo
int PyRun_AnyFileEx_redo(FILE* fp, const char* filename, int closeit) { return PyRun_AnyFileExFlags(fp, filename, closeit, NULL); }

#undef PyRun_SimpleFileEx
#define PyRun_SimpleFileEx PyRun_SimpleFileEx_redo
int PyRun_SimpleFileEx_redo(FILE* fp, const char* filename, int closeit) { return PyRun_SimpleFileExFlags(fp, filename, closeit, NULL); }

#undef PyRun_String
#define PyRun_String PyRun_String_redo
PyObject* PyRun_String_redo(const char* str, int start, PyObject* globals, PyObject* locals) { return PyRun_StringFlags(str, start, globals, locals, NULL); }

#undef PyRun_File
#define PyRun_File PyRun_File_redo
PyObject* PyRun_File_redo(FILE* fp, const char* filename, int start, PyObject* globals, PyObject* locals) { return PyRun_FileExFlags(fp, filename, start, globals, locals, 0, NULL); }

#undef PyRun_FileEx
#define PyRun_FileEx PyRun_FileEx_redo
PyObject* PyRun_FileEx_redo(FILE* fp, const char* filename, int start, PyObject* globals, PyObject* locals, int closeit) { return PyRun_FileExFlags(fp, filename, start, globals, locals, closeit, NULL); }

#undef Py_CompileString
#define Py_CompileString Py_CompileString_redo
PyObject* Py_CompileString_redo(const char* str, const char* filename, int start) { return Py_CompileStringFlags(str, filename, start, NULL); }

#undef PyRange_New
#define PyRange_New PyRange_New_redo
PyObject* PyRange_New_redo(PyObject* start, PyObject* stop, PyObject* step) { return PyObject_CallFunction((PyObject*)&PyRange_Type, "lll", start, stop, step); }
#endif /* PYTHON 2.4 */

#undef PyTuple_Check
int PyTuple_Check(PyObject* op) { return PyObject_TypeCheck(op, &PyTuple_Type); }
/* END HST FIXUP */

/* Value -> Pyobject */

value pywrapvalue( value cb ) {
    CAMLparam1(cb);
    CAMLreturn(pywrap(camlwrap(cb,NULL,0)));
}

value pyunwrapvalue( value cb ) {
    CAMLparam1(cb);
    value *v;
    v = (value *)PyCObject_AsVoidPtr( pyunwrap(cb) );
    CAMLreturn(*v);
}

/* Create the function table */

typedef struct _python_func_table {
    void *func;
    int format;
    char *desc;
} python_func_table;
    
python_func_table the_python_func_table[] = {
/* 1 */
    { (void *)Py_Initialize, 1, "Py_Initialize" },
    { (void *)Py_Finalize, 1, "Py_Finalize" },
    { (void *)PyErr_Print, 1, "PyErr_Print" },
/* 2 */
    { (void *)Py_Exit, 2, "Py_Exit" },
    { (void *)PyErr_PrintEx, 2, "PyErr_PrintEx" },
/* 3 */
    { (void *)Py_SetProgramName, 3, "Py_SetProgramName" },
    { (void *)Py_SetPythonHome, 3, "Py_SetPythonHome" },
/* 4 */
    { (void *)Py_IsInitialized, 4, "Py_IsInitialized" },
/* 5 */
    { (void *)PyRun_SimpleString, 5, "PyRun_SimpleString" },
/* 6 */
    { (void *)PyRun_AnyFile, 6, "PyRun_AnyFile" },
    { (void *)PyRun_SimpleFile, 6, "PyRun_SimpleFile" },
    { (void *)PyRun_InteractiveOne, 6, "PyRun_InteractiveOne" },
    { (void *)PyRun_InteractiveLoop, 6, "PyRun_InteractiveLoop" },
    { (void *)Py_FdIsInteractive, 6, "Py_FdIsInteractive" },
/* 7 */
    { (void *)PyRun_AnyFileEx, 7, "PyRun_AnyFileEx" },
    { (void *)PyRun_SimpleFileEx, 7, "PyRun_SimpleFileEx" },
/* 8 */
    { (void *)Py_GetProgramName, 8, "Py_GetProgramName" },
    { (void *)Py_GetPythonHome, 8, "Py_GetPythonHome" },
    { (void *)Py_GetProgramFullPath, 8, "Py_GetProgramFullPath" },
    { (void *)Py_GetPrefix, 8, "Py_GetPrefix" },
    { (void *)Py_GetExecPrefix, 8, "Py_GetExecPrefix" },
    { (void *)Py_GetPath, 8, "Py_GetPath" },
    { (void *)Py_GetVersion, 8, "Py_GetVersion" },
    { (void *)Py_GetPlatform, 8, "Py_GetPlatform" },
    { (void *)Py_GetCopyright, 8, "Py_GetCopyright" },
    { (void *)Py_GetCompiler, 8, "Py_GetCompiler" },
    { (void *)Py_GetBuildInfo, 8, "Py_GetBuildInfo" },
/* 9 */
    { (void *)PyRun_String, 9, "PyRun_String" },
/* 10 */
    { (void *)PyRun_File, 10, "PyRun_File" },
/* 11 */
    { (void *)PyRun_FileEx, 11, "PyRun_FileEx" },
/* 12 */
    { (void *)Py_CompileString, 12, "Py_CompileString" },

/* Object */
/* 13 */
    { (void *)PyObject_Print, 13, "PyObject_Print" },
/* 14 */
    { (void *)PyObject_Repr, 14, "PyObject_Repr" },
    { (void *)PyObject_Str, 14, "PyObject_Str" },
    { (void *)PyObject_Unicode, 14, "PyObject_Unicode" },
/* 15 */
    { (void *)PyObject_RichCompare, 15, "PyObject_RichCompare" },
/* 16 */
    { (void *)PyObject_GetAttrString, 16, "PyObject_GetAttrString" },
/* 17 */
    { (void *)PyObject_GetAttr, 17, "PyObject_GetAttr" },
    { (void *)PyObject_CallObject, 17, "PyObject_CallObject" },
/* 18 */
    { (void *)PyObject_IsTrue, 18, "PyObject_IsTrue" },
    { (void *)PyObject_Not, 18, "PyObject_Not" },
    { (void *)PyCallable_Check, 18, "PyCallable_Check" },
/* 19 */
    { (void *)PyObject_Compare, 19, "PyObject_Compare" },
    { (void *)PyObject_HasAttr, 19, "PyObject_HasAttr" },
/* 20 */
    { (void *)PyObject_RichCompareBool, 20, "PyObject_RichCompareBool" },
/* 21 */
    { (void *)PyObject_SetAttrString, 21, "PyObject_SetAttrString" },
/* 22 */
    { (void *)PyObject_HasAttrString, 22, "PyObject_HasAttrString" },
/* 23 */
    { (void *)PyNumber_Coerce, 23, "PyNumber_Coerce" },
    { (void *)PyNumber_CoerceEx, 23, "PyNumber_CoerceEx" },
/* 24 */
    { (void *)PyObject_SetAttr, 24, "PyObject_SetAttr" },
/* 25 */
    { (void *)PyObject_Hash, 25, "PyObject_Hash" },

/* Strings */
/* 18 */
    { (void *)PyString_Size, 18, "PyString_Size" },
/* 26 */
    { (void *)PyString_AsString, 26, "PyString_AsString" },
/* 27 */
    { (void *)PyString_Concat, 27, "PyString_Concat" },
    { (void *)PyString_ConcatAndDel, 27, "PyString_ConcatAndDel" },
/* 28 */
    { (void *)PyString_FromString, 28, "PyString_FromString" },
/* 17 */
    { (void *)PyString_Format, 17, "PyString_Format" },

/* Dictionaries */
/* 29 */
    { (void *)PyDict_New, 29, "PyDict_New" },
/* 17 */
    { (void *)PyDict_GetItem, 17, "PyDict_GetItem" },
/* 24 */
    { (void *)PyDict_SetItem, 24, "PyDict_SetItem" },
/* 19 */
    { (void *)PyDict_DelItem, 19, "PyDict_DelItem" },
/* 30 */
    { (void *)PyDict_Clear, 30, "PyDict_Clear" },
/* 31 */
    { (void *)PyDict_Next, 31, "PyDict_Next" },
/* 14 */
    { (void *)PyDict_Keys, 14, "PyDict_Keys" },
    { (void *)PyDict_Values, 14, "PyDict_Values" },
    { (void *)PyDict_Items, 14, "PyDict_Items" },
    { (void *)PyDict_Copy, 14, "PyDict_Copy" },
/* 18 */
    { (void *)PyDict_Size, 18, "PyDict_Size" },
/* 16 */
    { (void *)PyDict_GetItemString, 16, "PyDict_GetItemString" },
/* 22 */
    { (void *)PyDict_DelItemString, 22, "PyDict_DelItemString" },
/* 21 */
    { (void *)PyDict_SetItemString, 21, "PyDict_SetItemString" },

/* Integer */
/* 34 */
    { (void *)PyInt_FromLong, 34, "PyInt_FromLong" },
/* 25 */
    { (void *)PyInt_AsLong, 25, "PyInt_AsLong" },
/* 35 */
    { (void *)PyInt_GetMax, 35, "PyInt_GetMax" },

/* Float */
/* 36 */
    { (void *)PyFloat_FromDouble, 36, "PyFloat_FromDouble" },
/* 37 */
    { (void *)PyFloat_AsDouble, 37, "PyFloat_AsDouble" },

/* Modules */
/* 28 */
    { (void *)PyModule_New, 28, "PyModule_New" },
/* 14 */
    { (void *)PyModule_GetDict, 14, "PyModule_GetDict" },
/* 26 */
    { (void *)PyModule_GetName, 26, "PyModule_GetName" },
    { (void *)PyModule_GetFilename, 26, "PyModule_GetFilename" },

/* 39 */
    { (void *)PyTuple_New, 39, "PyTuple_New" },
/* 18 */
    { (void *)PyTuple_Size, 18, "PyTuple_Size" },
    { (void *)PyTuple_Check, 18, "PyTuple_Check" },
/* 40 */
    { (void *)PyTuple_GetItem, 40, "PyTuple_GetItem" },
/* 41 */
    { (void *)PyTuple_SetItem, 41, "PyTuple_SetItem" },
/* 13 */
    { (void *)PyTuple_GetSlice, 13, "PyTuple_GetSlice" },

/* 42 */
    { (void *)PySlice_New, 42, "PySlice_New" },
/* 43 */
    { (void *)PySlice_GetIndices, 43, "PySlice_GetIndices" },
/* 44 */
    { (void *)PyRange_New, 44, "PyRange_New" },

/* Error handling definitions */

/* 30 */
    { (void *)PyErr_SetNone, 30, "PyErr_SetNone" },
/* 45 */
    { (void *)PyErr_SetObject, 45, "PyErr_SetObject" },
/* 46 */
    { (void *)PyErr_SetString, 46, "PyErr_SetString" },
/* 29 */
    { (void *)PyErr_Occurred, 29, "PyErr_Occurred" },
/* 1 */
    { (void *)PyErr_Clear, 1, "PyErr_Clear" },
/* 47 */
    { (void *)PyErr_Fetch, 47, "PyErr_Fetch" },
/* 48 */
    { (void *)PyErr_Restore, 48, "PyErr_Restore" },

/* Error testing and normalization */
/* 19 */
    { (void *)PyErr_GivenExceptionMatches, 19, "PyErr_GivenExceptionMatches" },
/* 18 */
    { (void *)PyErr_ExceptionMatches, 18, "PyErr_ExceptionMatches" },
/* 47 */
    { (void *)PyErr_NormalizeException, 47, "PyErr_NormalizeException" },

/* Classes */
/* 42 */
    { (void *)PyClass_New, 42, "PyClass_New" },
/* 42 */
    { (void *)PyInstance_New, 42, "PyInstance_New" },

/* 17 */
{ (void *)PyInstance_NewRaw, 17, "PyInstance_NewRaw" },
/* 42 */
{ (void *)PyMethod_New, 42, "PyMethod_New" },
/* 14 */
{ (void *)PyMethod_Function, 14, "PyMethod_Function" },
{ (void *)PyMethod_Self, 14, "PyMethod_Self" },
{ (void *)PyMethod_Class, 14, "PyMethod_Class" },

/* Module */
/* 28 */
{ (void *)PyModule_New, 28, "PyModule_New" },
/* 14 */
{ (void *)PyModule_GetDict, 14, "PyModule_GetDict" },
/* 26 */
{ (void *)PyModule_GetName, 26, "PyModule_GetName" },
{ (void *)PyModule_GetFilename, 26, "PyModule_GetFilename" },
/* 35 */
{ (void *)PyImport_GetMagicNumber, 35, "PyImport_GetMagicNumber" },
/* 49 */
{ (void *)PyImport_ExecCodeModule, 49, "PyImport_ExecCodeModule" },
/* 50 */
{ (void *)PyImport_ExecCodeModuleEx, 50, "PyImport_ExecCodeModuleEx" },
/* 29 */
{ (void *)PyImport_GetModuleDict, 29, "PyImport_GetModuleDict" },
/* 28 */
{ (void *)PyImport_AddModule, 28, "PyImport_AddModule" },
{ (void *)PyImport_ImportModule, 28, "PyImport_ImportModule" },
/* 51 */
#if 0
    /* In Python 2.6, this because a #define so we cannot take
     * the address of the function.  - RWMJ.
     */
{ (void *)PyImport_ImportModuleEx, 51, "PyImport_ImportModuleEx" },
#endif
/* 28 */
{ (void *)PyImport_Import, 28, "PyImport_Import" },
/* 14 */
{ (void *)PyImport_ReloadModule, 14, "PyImport_ReloadModule" },
/* 1 */
{ (void *)PyImport_Cleanup, 1, "PyImport_Cleanup" },
/* 5 */
{ (void *)PyImport_ImportFrozenModule, 5, "PyImport_ImportFrozenModule" },

/* Interface to random parts in ceval.c */
/* 42 */
{ (void *)PyEval_CallObjectWithKeywords, 42, "PyEval_CallObjectWithKeywords" },
/* 17 */
{ (void *)PyEval_CallObject, 17, "PyEval_CallObject" },

/* 29 */
{ (void *)PyEval_GetBuiltins, 29, "PyEval_GetBuiltins" },
{ (void *)PyEval_GetGlobals, 29, "PyEval_GetGlobals" },
{ (void *)PyEval_GetLocals, 29, "PyEval_GetLocals" },
{ (void *)PyEval_GetFrame, 29, "PyEval_GetFrame" },
/* 4 */
{ (void *)PyEval_GetRestricted, 4, "PyEval_GetRestricted" },

/* Abstract layer */
/* 14 */
{ (void *)PyObject_Type, 14, "PyObject_Type" },
/* 18 */
{ (void *)PyObject_Size, 18, "PyObject_Size" },
/* 17 */
{ (void *)PyObject_GetItem, 17, "PyObject_GetItem" },
/* 24 */
{ (void *)PyObject_SetItem, 24, "PyObject_SetItem" },
/* 17 */
{ (void *)PyObject_DelItem, 17, "PyObject_DelItem" },
/* 52 */
{ (void *)PyObject_AsCharBuffer, 52, "PyObject_AsCharBuffer" },
{ (void *)PyObject_AsReadBuffer, 52, "PyObject_AsReadBuffer" },
{ (void *)PyObject_AsWriteBuffer, 52, "PyObject_AsWriteBuffer" },
/* 18 */
{ (void *)PyNumber_Check, 18, "PyNumber_Check" },
/* 17 */
{ (void *)PyNumber_Add, 17, "PyNumber_Add" },
{ (void *)PyNumber_Subtract, 17, "PyNumber_Subtract" },
{ (void *)PyNumber_Multiply, 17, "PyNumber_Multiply" },
{ (void *)PyNumber_Divide, 17, "PyNumber_Divide" },
{ (void *)PyNumber_Remainder, 17, "PyNumber_Remainder" },
{ (void *)PyNumber_Divmod, 17, "PyNumber_Divmod" },
/* 42 */
{ (void *)PyNumber_Power, 42, "PyNumber_Power" },
/* 14 */
{ (void *)PyNumber_Negative, 14, "PyNumber_Negative" },
{ (void *)PyNumber_Positive, 14, "PyNumber_Positive" },
{ (void *)PyNumber_Absolute, 14, "PyNumber_Absolute" },
{ (void *)PyNumber_Invert, 14, "PyNumber_Invert" },
/* 17 */
{ (void *)PyNumber_Lshift, 17, "PyNumber_Lshift" },
{ (void *)PyNumber_Rshift, 17, "PyNumber_Rshift" },
{ (void *)PyNumber_And, 17, "PyNumber_And" },
{ (void *)PyNumber_Xor, 17, "PyNumber_Xor" },
{ (void *)PyNumber_Or, 17, "PyNumber_Or" },
/* 14 */
{ (void *)PyNumber_Int, 14, "PyNumber_Int" },
{ (void *)PyNumber_Long, 14, "PyNumber_Long" },
{ (void *)PyNumber_Float, 14, "PyNumber_Float" },
/* 17 */
{ (void *)PyNumber_InPlaceAdd, 17, "PyNumber_InPlaceAdd" },
{ (void *)PyNumber_InPlaceSubtract, 17, "PyNumber_InPlaceSubtract" },
{ (void *)PyNumber_InPlaceMultiply, 17, "PyNumber_InPlaceMultiply" },
{ (void *)PyNumber_InPlaceDivide, 17, "PyNumber_InPlaceDivide" },
{ (void *)PyNumber_InPlaceRemainder, 17, "PyNumber_InPlaceRemainder" },
{ (void *)PyNumber_InPlaceLshift, 17, "PyNumber_InPlaceLshift" },
{ (void *)PyNumber_InPlaceRshift, 17, "PyNumber_InPlaceRshift" },
{ (void *)PyNumber_InPlaceAnd, 17, "PyNumber_InPlaceAnd" },
{ (void *)PyNumber_InPlaceXor, 17, "PyNumber_InPlaceXor" },
{ (void *)PyNumber_InPlaceOr, 17, "PyNumber_InPlaceOr" },
/* 42 */
{ (void *)PyNumber_InPlacePower, 42, "PyNumber_InPlacePower" },
/* 18 */
{ (void *)PySequence_Check, 18, "PySequence_Check" },
{ (void *)PySequence_Size, 18, "PySequence_Size" },
{ (void *)PySequence_Length, 18, "PySequence_Length" },
/* 17 */
{ (void *)PySequence_Concat, 17, "PySequence_Concat" },
/* 40 */
{ (void *)PySequence_Repeat, 40, "PySequence_Repeat" },
{ (void *)PySequence_GetItem, 40, "PySequence_GetItem" },
/* 13 */
{ (void *)PySequence_GetSlice, 13, "PySequence_GetSlice" },
/* 41 */
{ (void *)PySequence_SetItem, 41, "PySequence_SetItem" },
/* 20 */
{ (void *)PySequence_DelItem, 20, "PySequence_DelItem" },
/* 53 */
{ (void *)PySequence_SetSlice, 53, "PySequence_SetSlice" },
/* 54 */
{ (void *)PySequence_DelSlice, 54, "PySequence_DelSlice" },
/* 14 */
{ (void *)PySequence_Tuple, 14, "PySequence_Tuple" },
{ (void *)PySequence_List, 14, "PySequence_List" },
/* 16 */
{ (void *)PySequence_Fast, 16, "PySequence_Fast" },
/* 19 */
{ (void *)PySequence_Count, 19, "PySequence_Count" },
{ (void *)PySequence_Contains, 19, "PySequence_Contains" },
{ (void *)PySequence_In, 19, "PySequence_In" },
{ (void *)PySequence_Index, 19, "PySequence_Index" },
/* 17 */
{ (void *)PySequence_InPlaceConcat, 17, "PySequence_InPlaceConcat" },
/* 22 */
{ (void *)PySequence_InPlaceRepeat, 22, "PySequence_InPlaceRepeat" },
/* 18 */
{ (void *)PyMapping_Check, 18, "PyMapping_Check" },
{ (void *)PyMapping_Size, 18, "PyMapping_Size" },
{ (void *)PyMapping_Length, 18, "PyMapping_Length" },
/* 16 */
{ (void *)PyMapping_HasKeyString, 16, "PyMapping_HasKeyString" },
/* 19 */
{ (void *)PyMapping_HasKey, 19, "PyMapping_HasKey" },
/* 16 */
{ (void *)PyMapping_GetItemString, 16, "PyMapping_GetItemString" },
/* 41 */
{ (void *)PyMapping_SetItemString, 41, "PyMapping_SetItemString" },

/* End */
{ NULL, 0 }
};

value pygetfuncarray( value unit ) {
    CAMLparam1(unit);
    CAMLlocal2(retv,tuplev);
    int i;
    int total_funcs;

    for( i = 0; the_python_func_table[i].func; i++ ) ; total_funcs = i;

    retv = alloc(total_funcs,0);

    for( i = 0; i < total_funcs; i++ ) {
	tuplev = alloc_tuple( 3 );
	Store_field(tuplev,0,funcwrap((void *)the_python_func_table[i].func));
	Store_field(tuplev,1,Val_int(the_python_func_table[i].format));
	Store_field(tuplev,2,Val_int(i));
	Store_field(retv,i,tuplev);
    }

    CAMLreturn(retv);
}

enum PycamlTypeLabels { 
    TupleType = 0,
    StringType,
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
    OtherType
};

value pytype( value obj ) {
    CAMLparam1(obj);
    PyObject *pobj = pyunwrap( obj );
    if( !pobj ) CAMLreturn(NullType);
    else if( PyTuple_Check( pobj ) ) CAMLreturn(Val_int(TupleType));
    else if( PyString_Check( pobj ) ) CAMLreturn(Val_int(StringType));
    else if( PyInt_Check( pobj ) ) CAMLreturn(Val_int(IntType));
    else if( PyFloat_Check( pobj ) ) CAMLreturn(Val_int(FloatType));
    else if( PyList_Check( pobj ) ) CAMLreturn(Val_int(ListType));
    else if( pobj == Py_None ) CAMLreturn(Val_int(NoneType));
    else if( PyCallable_Check( pobj ) ) CAMLreturn(Val_int(CallableType));
    else if( PyModule_Check( pobj ) ) CAMLreturn(Val_int(ModuleType));
    else if( PyClass_Check( pobj ) ) CAMLreturn(Val_int(ClassType));
    else if( PyType_Check( pobj ) ) CAMLreturn(Val_int(TypeType));
    else if( PyDict_Check( pobj ) ) CAMLreturn(Val_int(DictType));
    else CAMLreturn(Val_int(OtherType));
}

value pynull( value unit ) {
    CAMLparam1(unit);
    CAMLreturn(pywrap(NULL));
}

value pynone( value unit ) {
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_None));
}

value pytuple_fromarray( value array ) {
    CAMLparam1(array);
    PyObject *tuple = PyTuple_New(Wosize_val(array));
    PyObject *elt;
    int i;
    int x;

    for( i = 0; i < Wosize_val(array); i++ ) {
	elt = pyunwrap(Field(array,i));
	Py_INCREF(elt); /* PyTuple_SetItem will steal a reference */
	x = PyTuple_SetItem(tuple,i,elt);
    }

    CAMLreturn(pywrap(tuple));
}

value pytuple_toarray( value array ) {
    CAMLparam1(array);
    PyObject *obj = pyunwrap(array);
    int i;
    CAMLlocal1(rv);

    rv = alloc_tuple( PySequence_Size(obj) );

    for( i = 0; i < PySequence_Size(obj); i++ )
	Store_field(rv,i,pywrap(PySequence_GetItem(obj,i)));

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
    obj = camlwrap(closure,&ml,sizeof(ml));
    ml_def = (PyMethodDef *)caml_aux(obj);
    CAMLreturn(pywrap(PyCFunction_New(ml_def,obj)));
}

value pymodule_initmodule( value name, value funclist ) {
    CAMLparam2(name,funclist);
    int i;
    PyMethodDef *methods = malloc( sizeof( PyMethodDef ) * 
				   Wosize_val(funclist) );
    CAMLlocal1(item);

    PyImport_AddModule( String_val(name) );

    for( i = 0; i < Wosize_val(funclist); i++ ) {
	item = Field(funclist,i);
	methods[i].ml_name = String_val(Field(item,0));
	methods[i].ml_meth = pywrap_closure(Field(item,1));
	methods[i].ml_flags = Int_val(Field(item,2));
	methods[i].ml_doc = String_val(Field(item,3));
    }

    Py_InitModule( String_val(name), methods );

    free( methods );

    CAMLreturn(Val_unit);
}

value pycaml_setargs(value argv) {
  CAMLparam1(argv);
  char* cargv[1];

  cargv[0] = String_val(argv);

  PySys_SetArgv(1, cargv);

  CAMLreturn0; 
}

value pytrue( value unit ) {
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_True));
}

value pyfalse(value unit) {
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_False));
}
