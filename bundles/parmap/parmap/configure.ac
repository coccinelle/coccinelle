AC_INIT(parmap, 1.0-rc7, roberto@dicosmo.org)

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
 AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
AC_SUBST(OCAMLFIND)
if test "$OCAMLFIND" = "no"; then
 AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_HEADER_STDC
AC_CHECK_HEADERS([sched.h])
AC_CHECK_DECLS([sched_setaffinity], [], [], [[
#define _GNU_SOURCE 1
#include <sched.h>
]])

AC_CHECK_HEADERS([mach/thread_policy.h])
AC_CHECK_DECLS([thread_policy_set], [], [], [[
#include <mach/mach_init.h>
#include <mach/thread_policy.h>
]])

AC_CONFIG_HEADERS([config.h])

if $OCAMLBUILD -use-ocamlfind >/dev/null 2>&1 ; then
  AC_MSG_RESULT(ocaml version $OCAMLVERSION. Use standard Makefile)
  AC_CONFIG_FILES([Makefile])
  AC_OUTPUT
else
  AC_MSG_RESULT(ocamlbuild does not exists or it does not support -use-ocamlfind)
  AC_CONFIG_FILES([Makefile_3.11])
  AC_OUTPUT
  mv Makefile_3.11 Makefile
fi


