AC_INIT([[parmap-coccinelle-bundle]], [1.2.5],
  [[Thierry.Martinez@inria.fr]], [[parmap-coccinelle-bundle]],
  [[https://github.com/coccinelle/coccinelle]])
AC_CHECK_FUNCS([sched_setaffinity thread_policy_set])
AC_CHECK_TOOL([OCAML], [ocaml], [])
AC_CHECK_TOOL([OCAMLC], [ocamlc.opt], [])
AS_IF([test "x$OCAMLC" = "x"],
  [AC_CHECK_TOOL([OCAMLC], [ocamlc], [])])
AC_MSG_CHECKING([OCaml version])
AC_SUBST([OCAMLVERSION],
  [[`$OCAMLC -v | sed -n 's/^.*version \(.*\)$/\1/p' | sed 's/\r//'`]])
AC_MSG_RESULT([$OCAMLVERSION])
AS_IF([test `printf "$OCAMLVERSION\n4.02.0" | sort | head -n1` = 4.02.0],
  [dnl
    AC_SUBST([BEGIN_FROM_4_02_0], [])
    AC_SUBST([END_FROM_4_02_0], [])
    AC_SUBST([BEGIN_BEFORE_4_02_0], [['(*']])
    AC_SUBST([END_BEFORE_4_02_0], [['*)']])],
  [dnl
    AC_SUBST([BEGIN_FROM_4_02_0], [['(*']])
    AC_SUBST([END_FROM_4_02_0], [['*)']])
    AC_SUBST([BEGIN_BEFORE_4_02_0], [])
    AC_SUBST([END_BEFORE_4_02_0], [])])
AS_IF([test `printf "$OCAMLVERSION\n4.06.0" | sort | head -n1` = 4.06.0],
  [dnl
    AC_SUBST([BEGIN_FROM_4_06_0], [])
    AC_SUBST([END_FROM_4_06_0], [])
    AC_SUBST([BEGIN_BEFORE_4_06_0], [['(*']])
    AC_SUBST([END_BEFORE_4_06_0], [['*)']])],
  [dnl
    AC_SUBST([BEGIN_FROM_4_06_0], [['(*']])
    AC_SUBST([END_FROM_4_06_0], [['*)']])
    AC_SUBST([BEGIN_BEFORE_4_06_0], [])
    AC_SUBST([END_BEFORE_4_06_0], [])])
AC_CONFIG_FILES([parmap/src/parmap_compat.ml:parmap_compat.ml.in])
AC_CONFIG_FILES([parmap/src/bytearray.ml:bytearray.ml.in])
AC_CONFIG_FILES([parmap/src/parmap.ml:parmap.ml.in])
AC_CONFIG_HEADERS([parmap/src/setcore_stubs.h:setcore_stubs.h.in])

AC_OUTPUT
