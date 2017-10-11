#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <unistd.h>
#ifdef __APPLE__
#include <libproc.h>
#endif
#include <errno.h>
#include <string.h>
#include <limits.h>

CAMLprim value
proc_pidpath_wrapper(value pid_ocaml)
{
    CAMLparam1(pid_ocaml);
#ifdef __APPLE__
    int pid = Int_val(pid_ocaml);
    int ret;
    char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
    ret = proc_pidpath(pid, pathbuf, sizeof(pathbuf));
    if (ret <= 0) {
        failwith(strerror(errno));
    }
#else
    char *pathbuf = "";
#endif
    CAMLreturn(caml_copy_string(pathbuf));
}


CAMLprim value
realpath_wrapper(value path_ocaml)
{
    CAMLparam1(path_ocaml);
    char pathbuf[PATH_MAX];
    char *path = String_val(path_ocaml);
    if (realpath(path, pathbuf) == NULL) {
        failwith(strerror(errno));
    }
    CAMLreturn(caml_copy_string(pathbuf));
}
