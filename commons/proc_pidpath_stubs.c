#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <unistd.h>
#include <libproc.h>
#include <errno.h>
#include <string.h>

CAMLprim value
proc_pidpath_wrapper(value pid_ocaml)
{
    CAMLparam1(pid_ocaml);
    int pid = Int_val(pid_ocaml);
    int ret;
    char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
    ret = proc_pidpath(pid, pathbuf, sizeof(pathbuf));
    if (ret <= 0) {
        failwith(strerror(errno));
    }
    return caml_copy_string(pathbuf);
}
