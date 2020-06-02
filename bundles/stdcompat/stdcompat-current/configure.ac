AC_INIT([[stdcompat]], m4_esyscmd([tr -d '\n' < VERSION]),
  [[Thierry.Martinez@inria.fr]], [[stdcompat]],
  [[https://github.com/thierry.martinez/stdcompat/]])
AM_INIT_AUTOMAKE([foreign])

AC_DEFUN([AC_OCAMLFIND_PKG],
  [AC_MSG_CHECKING([for ocamlfind package $2])
   AS_IF([$OCAMLFIND query $2 >/dev/null 2>&1],
     [AC_MSG_RESULT([yes])
      AC_SUBST([$1], [$2])],
     [AC_MSG_RESULT([no])
      AC_SUBST([$1], [])])])

AC_CHECK_TOOL([OCAML], [ocaml], [])
AC_CHECK_TOOL([OCAMLFIND], [ocamlfind], [])
AS_IF([test "x$OCAMLFIND" = "x"],
  [AC_CHECK_TOOL([OCAMLC], [ocamlc.opt], [])
   AS_IF([test "x$OCAMLC" = "x"],
     [AC_CHECK_TOOL([OCAMLC], [ocamlc], [])])
   AC_CHECK_TOOL([OCAMLOPT], [ocamlopt.opt], [])
   AS_IF([test "x$OCAMLOPT" = "x"],
     [AC_CHECK_TOOL([OCAMLOPT], [ocamlopt], [])])
   AC_CHECK_TOOL([OCAMLDOC], [ocamldoc.opt], [])
   AS_IF([test "x$OCAMLDOC" = "x"],
     [AC_CHECK_TOOL([OCAMLDOC], [ocamldoc], [])])
   AC_CHECK_TOOL([OCAMLDEP], [ocamldep.opt], [])
   AS_IF([test "x$OCAMLDEP" = "x"],
     [AC_CHECK_TOOL([OCAMLDEP], [ocamldep], [])])
   AC_CHECK_TOOL([OCAMLMKLIB], [ocamlmklib.opt], [])
   AS_IF([test "x$OCAMLMKLIB" = "x"],
     [AC_CHECK_TOOL([OCAMLMKLIB], [ocamlmklib], [])])
   AC_SUBST([RESULT_PKG], [])
   AC_SUBST([SEQ_PKG], [])
   AC_SUBST([UCHAR_PKG], [])],
  [AC_SUBST([OCAMLC], [["$OCAMLFIND ocamlc"]])
   AC_SUBST([OCAMLOPT], [["$OCAMLFIND ocamlopt"]])
   AC_SUBST([OCAMLDOC], [["$OCAMLFIND ocamldoc"]])
   AC_SUBST([OCAMLDEP], [["$OCAMLFIND ocamldep"]])
   AC_SUBST([OCAMLMKLIB], [["$OCAMLFIND ocamlmklib"]])
   AC_OCAMLFIND_PKG([RESULT_PKG], [result])
   AC_OCAMLFIND_PKG([SEQ_PKG], [seq])
   AC_OCAMLFIND_PKG([UCHAR_PKG], [uchar])])

AS_IF([test "x$OCAMLOPT" = "x"],
  [AS_IF([test "x$OCAMLC" = "x"],
     [AC_MSG_ERROR([[No OCaml compiler available]])],
     [AC_SUBST([OCAMLBEST], [["$OCAMLC"]])
      AC_SUBST([CMAX], [["cma"]])])],
  [AC_SUBST([OCAMLBEST], [["$OCAMLOPT"]])
   AC_SUBST([CMAX], [["cmxa"]])])

AS_IF([test "x$OCAMLDEP" = "x"],
  [AC_MSG_ERROR([[No ocamldep available]])])

AC_CONFIG_HEADERS([config.h])
AC_C_BIGENDIAN(
  [AC_SUBST([BIG_ENDIAN], [[true]])],
  [AC_SUBST([BIG_ENDIAN], [[false]])],
  [AC_MSG_ERROR([[Unknown endianness]])])

# ocamlc --version is not supported by OCaml <4.03
# \($\|\r\) is not supported by Mac OS sed

AC_MSG_CHECKING([OCaml version])
AC_SUBST([OCAMLVERSION],
  [[`$OCAMLBEST -v | sed -n 's/^.*version \(.*\)$/\1/p' | sed 's/\r//'`]])
AC_MSG_RESULT([$OCAMLVERSION])

AC_MSG_CHECKING([OCaml target OS type])

AS_IF([test `printf "$OCAMLVERSION\n3.12.0" | sort | head -n1` = 3.12.0],
  [AC_SUBST([target_os_type],
    [[`$OCAMLBEST -config | sed -n 's/^os_type: \(.*\)$/\1/p' | sed 's/\r//'`]])],
  [AC_SUBST([target_os_type],
    [[`echo 'Sys.os_type;;' | $OCAML | sed -n 's/^\# - : string = "\(.*\)"$/\1/p'`]])])
AC_MSG_RESULT([$target_os_type])

AC_ARG_ENABLE([magic],
            [AS_HELP_STRING([--enable-magic],
              [[use Obj.magic in redefinitions when possible (default)]]),
             AS_HELP_STRING([--disable-magic],
              [[use only pure redefinitions (without Obj.magic)]])],
            [],
            [AC_SUBST([enable_magic], [yes])])

AC_DEFUN([AC_DISABLE_BLOCK],
  [AC_SUBST([BEGIN_$1], [['(*']])
   AC_SUBST([END_$1], [['*)']])
   AC_SUBST([C_BEGIN_$1], [['/*']])
   AC_SUBST([C_END_$1], [['*/']])])

AC_DEFUN([AC_ENABLE_BLOCK],
  [AC_SUBST([BEGIN_$1], [])
   AC_SUBST([END_$1], [])
   AC_SUBST([C_BEGIN_$1], [])
   AC_SUBST([C_END_$1], [])])

AC_DEFUN([AC_WITH_BLOCK],
  [AC_ENABLE_BLOCK([WITH_$1])
   AC_DISABLE_BLOCK([WITHOUT_$1])])

AC_DEFUN([AC_WITHOUT_BLOCK],
  [AC_DISABLE_BLOCK([WITH_$1])
   AC_ENABLE_BLOCK([WITHOUT_$1])])

AC_DEFUN([AC_WITH_BLOCK_PKG],
  [AS_IF([test "x$$1_PKG" = "x"],
    [AC_WITHOUT_BLOCK($1_PKG)],
    [AC_WITH_BLOCK($1_PKG)])])

AC_DEFUN([AC_WITH_OCAML_VERSION],
  [AS_IF([test `printf "$OCAMLVERSION\n$2" | sort | head -n1` = $2],
    [AC_ENABLE_BLOCK([FROM_$1])
     AC_DISABLE_BLOCK([BEFORE_$1])],
    [AC_DISABLE_BLOCK([FROM_$1])
     AC_ENABLE_BLOCK([BEFORE_$1])])])

AS_IF([test "x$enable_magic" = "xno"],
  [AC_WITHOUT_BLOCK([MAGIC])],
  [AC_WITH_BLOCK([MAGIC])])

AC_WITH_BLOCK_PKG([RESULT])
AC_WITH_BLOCK_PKG([SEQ])
AC_WITH_BLOCK_PKG([UCHAR])

AC_WITH_OCAML_VERSION([3_07_0], [3.07])
AC_WITH_OCAML_VERSION([3_08_0], [3.08.0])
AC_WITH_OCAML_VERSION([3_09_0], [3.09.0])
AC_WITH_OCAML_VERSION([3_10_0], [3.10.0])
AC_WITH_OCAML_VERSION([3_11_0], [3.11.0])
AC_WITH_OCAML_VERSION([3_12_0], [3.12.0])
AC_WITH_OCAML_VERSION([4_00_0], [4.00.0])
AC_WITH_OCAML_VERSION([4_01_0], [4.01.0])
AC_WITH_OCAML_VERSION([4_02_0], [4.02.0])
AC_WITH_OCAML_VERSION([4_03_0], [4.03.0])
AC_WITH_OCAML_VERSION([4_04_0], [4.04.0])
AC_WITH_OCAML_VERSION([4_05_0], [4.05.0])
AC_WITH_OCAML_VERSION([4_06_0], [4.06.0])
AC_WITH_OCAML_VERSION([4_07_0], [4.07.0])
AC_WITH_OCAML_VERSION([4_07_1], [4.07.1])
AC_WITH_OCAML_VERSION([4_08_0], [4.08.0])
AC_WITH_OCAML_VERSION([4_09_0], [4.09.0])
AC_WITH_OCAML_VERSION([4_10_0], [4.10.0])

AS_IF([test "x$target_os_type" = "xWin32"],
  [AC_WITH_BLOCK([WIN32])],
  [AC_WITHOUT_BLOCK([WIN32])])

AS_IF([test "x$target_os_type" = "xCygwin"],
  [AC_WITH_BLOCK([CYGWIN])],
  [AC_WITHOUT_BLOCK([CYGWIN])])

AS_IF([test "x$target_os_type" = "xUnix"],
  [AC_WITH_BLOCK([UNIX])],
  [AC_WITHOUT_BLOCK([UNIX])])

AM_CONDITIONAL(OCAML_SUPPORTS_BYTECODE, [test "x$OCAMLC" != "x"])

AM_CONDITIONAL(OCAML_SUPPORTS_NATIVE, [test "x$OCAMLOPT" != "x"])

AM_CONDITIONAL(OCAML_SUPPORTS_SHARED,
  [test `printf "$OCAMLVERSION\n3.11.1" | sort | head -n1` = 3.11.1])

AS_IF([test "x$OCAML_SUPPORTS_SHARED_TRUE" = "x"],
  [AC_SUBST([SHARED_LIBS], [["stdcompat.cmxs"]])],
  [AC_SUBST([SHARED_LIBS], [[""]])])

AM_CONDITIONAL(OCAML_SUPPORTS_NO_ALIAS_DEPS,
  [test `printf "$OCAMLVERSION\n4.02.0" | sort | head -n1` = 4.02.0])

AM_CONDITIONAL(OCAML_SUPPORTS_BIN_ANNOT,
  [test `printf "$OCAMLVERSION\n4.00.0" | sort | head -n1` = 4.00.0])

AM_CONDITIONAL(OCAML_SUPPORTS_ALERT,
  [test `printf "$OCAMLVERSION\n4.08.0" | sort | head -n1` = 4.08.0])

AC_SUBST([INCLUDE_DEPEND],[['
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
include .depend
endif
endif
']])

AC_CONFIG_FILES([META Makefile])
AC_CONFIG_FILES([stdcompat__hashtbl_ext.mli])
AC_CONFIG_FILES([stdcompat__hashtbl_ext.ml])
AC_CONFIG_FILES([stdcompat__init.mli])
AC_CONFIG_FILES([stdcompat__init.ml:stdcompat__init.mli.in])
AC_CONFIG_FILES([stdcompat__root.mli])
AC_CONFIG_FILES([stdcompat__root.ml])
AC_CONFIG_FILES([stdcompat__tools.mli])
AC_CONFIG_FILES([stdcompat__tools.ml])
AC_CONFIG_FILES([stdcompat__native.ml_byte])
AC_CONFIG_FILES([stdcompat__native.ml_native])
AC_CONFIG_FILES([stdcompat_tests.ml])
AC_CONFIG_FILES([stdcompat.h])
AC_CONFIG_FILES([stdcompat__stubs.c])
AC_CONFIG_FILES([stdcompat__pervasives_s.mli])
AC_CONFIG_FILES([stdcompat__arg_s.mli])
AC_CONFIG_FILES([stdcompat__array_s.mli])
AC_CONFIG_FILES([stdcompat__arrayLabels_s.mli])
AC_CONFIG_FILES([stdcompat__bool_s.mli])
AC_CONFIG_FILES([stdcompat__buffer_s.mli])
AC_CONFIG_FILES([stdcompat__bytes_s.mli])
AC_CONFIG_FILES([stdcompat__bytesLabels_s.mli])
AC_CONFIG_FILES([stdcompat__char_s.mli])
AC_CONFIG_FILES([stdcompat__digest_s.mli])
AC_CONFIG_FILES([stdcompat__ephemeron_s.mli])
AC_CONFIG_FILES([stdcompat__filename_s.mli])
AC_CONFIG_FILES([stdcompat__float_s.mli])
AC_CONFIG_FILES([stdcompat__format_s.mli])
AC_CONFIG_FILES([stdcompat__fun_s.mli])
AC_CONFIG_FILES([stdcompat__hashtbl_s.mli])
AC_CONFIG_FILES([stdcompat__int_s.mli])
AC_CONFIG_FILES([stdcompat__int32_s.mli])
AC_CONFIG_FILES([stdcompat__int64_s.mli])
AC_CONFIG_FILES([stdcompat__lazy_s.mli])
AC_CONFIG_FILES([stdcompat__lexing_s.mli])
AC_CONFIG_FILES([stdcompat__list_s.mli])
AC_CONFIG_FILES([stdcompat__listLabels_s.mli])
AC_CONFIG_FILES([stdcompat__map_s.mli])
AC_CONFIG_FILES([stdcompat__moreLabels_s.mli])
AC_CONFIG_FILES([stdcompat__nativeint_s.mli])
AC_CONFIG_FILES([stdcompat__option_s.mli])
AC_CONFIG_FILES([stdcompat__printexc_s.mli])
AC_CONFIG_FILES([stdcompat__printf_s.mli])
AC_CONFIG_FILES([stdcompat__queue_s.mli])
AC_CONFIG_FILES([stdcompat__result_s.mli])
AC_CONFIG_FILES([stdcompat__seq_s.mli])
AC_CONFIG_FILES([stdcompat__set_s.mli])
AC_CONFIG_FILES([stdcompat__spacetime_s.mli])
AC_CONFIG_FILES([stdcompat__stack_s.mli])
AC_CONFIG_FILES([stdcompat__stream_s.mli])
AC_CONFIG_FILES([stdcompat__string_s.mli])
AC_CONFIG_FILES([stdcompat__stringLabels_s.mli])
AC_CONFIG_FILES([stdcompat__sys_s.mli])
AC_CONFIG_FILES([stdcompat__uchar_s.mli])
AC_CONFIG_FILES([stdcompat__weak_s.mli])
AC_CONFIG_FILES([stdcompat__stdlib_s.mli])
AC_CONFIG_FILES([stdcompat__pervasives_s.ml:stdcompat__pervasives_s.mli.in])
AC_CONFIG_FILES([stdcompat__arg_s.ml:stdcompat__arg_s.mli.in])
AC_CONFIG_FILES([stdcompat__array_s.ml:stdcompat__array_s.mli.in])
AC_CONFIG_FILES([stdcompat__arrayLabels_s.ml:stdcompat__arrayLabels_s.mli.in])
AC_CONFIG_FILES([stdcompat__bool_s.ml:stdcompat__bool_s.mli.in])
AC_CONFIG_FILES([stdcompat__buffer_s.ml:stdcompat__buffer_s.mli.in])
AC_CONFIG_FILES([stdcompat__bytes_s.ml:stdcompat__bytes_s.mli.in])
AC_CONFIG_FILES([stdcompat__bytesLabels_s.ml:stdcompat__bytesLabels_s.mli.in])
AC_CONFIG_FILES([stdcompat__char_s.ml:stdcompat__char_s.mli.in])
AC_CONFIG_FILES([stdcompat__digest_s.ml:stdcompat__digest_s.mli.in])
AC_CONFIG_FILES([stdcompat__ephemeron_s.ml:stdcompat__ephemeron_s.mli.in])
AC_CONFIG_FILES([stdcompat__filename_s.ml:stdcompat__filename_s.mli.in])
AC_CONFIG_FILES([stdcompat__float_s.ml:stdcompat__float_s.mli.in])
AC_CONFIG_FILES([stdcompat__format_s.ml:stdcompat__format_s.mli.in])
AC_CONFIG_FILES([stdcompat__fun_s.ml:stdcompat__fun_s.mli.in])
AC_CONFIG_FILES([stdcompat__hashtbl_s.ml:stdcompat__hashtbl_s.mli.in])
AC_CONFIG_FILES([stdcompat__int_s.ml:stdcompat__int_s.mli.in])
AC_CONFIG_FILES([stdcompat__int32_s.ml:stdcompat__int32_s.mli.in])
AC_CONFIG_FILES([stdcompat__int64_s.ml:stdcompat__int64_s.mli.in])
AC_CONFIG_FILES([stdcompat__lazy_s.ml:stdcompat__lazy_s.mli.in])
AC_CONFIG_FILES([stdcompat__lexing_s.ml:stdcompat__lexing_s.mli.in])
AC_CONFIG_FILES([stdcompat__list_s.ml:stdcompat__list_s.mli.in])
AC_CONFIG_FILES([stdcompat__listLabels_s.ml:stdcompat__listLabels_s.mli.in])
AC_CONFIG_FILES([stdcompat__map_s.ml:stdcompat__map_s.mli.in])
AC_CONFIG_FILES([stdcompat__moreLabels_s.ml:stdcompat__moreLabels_s.mli.in])
AC_CONFIG_FILES([stdcompat__nativeint_s.ml:stdcompat__nativeint_s.mli.in])
AC_CONFIG_FILES([stdcompat__option_s.ml:stdcompat__option_s.mli.in])
AC_CONFIG_FILES([stdcompat__printexc_s.ml:stdcompat__printexc_s.mli.in])
AC_CONFIG_FILES([stdcompat__printf_s.ml:stdcompat__printf_s.mli.in])
AC_CONFIG_FILES([stdcompat__queue_s.ml:stdcompat__queue_s.mli.in])
AC_CONFIG_FILES([stdcompat__result_s.ml:stdcompat__result_s.mli.in])
AC_CONFIG_FILES([stdcompat__seq_s.ml:stdcompat__seq_s.mli.in])
AC_CONFIG_FILES([stdcompat__set_s.ml:stdcompat__set_s.mli.in])
AC_CONFIG_FILES([stdcompat__spacetime_s.ml:stdcompat__spacetime_s.mli.in])
AC_CONFIG_FILES([stdcompat__stack_s.ml:stdcompat__stack_s.mli.in])
AC_CONFIG_FILES([stdcompat__stdlib_s.ml:stdcompat__stdlib_s.mli.in])
AC_CONFIG_FILES([stdcompat__stream_s.ml:stdcompat__stream_s.mli.in])
AC_CONFIG_FILES([stdcompat__string_s.ml:stdcompat__string_s.mli.in])
AC_CONFIG_FILES([stdcompat__stringLabels_s.ml:stdcompat__stringLabels_s.mli.in])
AC_CONFIG_FILES([stdcompat__sys_s.ml:stdcompat__sys_s.mli.in])
AC_CONFIG_FILES([stdcompat__uchar_s.ml:stdcompat__uchar_s.mli.in])
AC_CONFIG_FILES([stdcompat__weak_s.ml:stdcompat__weak_s.mli.in])
AC_CONFIG_FILES([stdcompat__pervasives.ml])
AC_CONFIG_FILES([stdcompat__arg.ml])
AC_CONFIG_FILES([stdcompat__array.ml])
AC_CONFIG_FILES([stdcompat__arrayLabels.ml])
AC_CONFIG_FILES([stdcompat__bool.ml])
AC_CONFIG_FILES([stdcompat__buffer.ml])
AC_CONFIG_FILES([stdcompat__bytes.ml])
AC_CONFIG_FILES([stdcompat__bytesLabels.ml])
AC_CONFIG_FILES([stdcompat__char.ml])
AC_CONFIG_FILES([stdcompat__digest.ml])
AC_CONFIG_FILES([stdcompat__ephemeron.ml])
AC_CONFIG_FILES([stdcompat__filename.ml])
AC_CONFIG_FILES([stdcompat__float.ml])
AC_CONFIG_FILES([stdcompat__format.ml])
AC_CONFIG_FILES([stdcompat__fun.ml])
AC_CONFIG_FILES([stdcompat__hashtbl.ml])
AC_CONFIG_FILES([stdcompat__int.ml])
AC_CONFIG_FILES([stdcompat__int32.ml])
AC_CONFIG_FILES([stdcompat__int64.ml])
AC_CONFIG_FILES([stdcompat__lazy.ml])
AC_CONFIG_FILES([stdcompat__lexing.ml])
AC_CONFIG_FILES([stdcompat__list.ml])
AC_CONFIG_FILES([stdcompat__listLabels.ml])
AC_CONFIG_FILES([stdcompat__map.ml])
AC_CONFIG_FILES([stdcompat__moreLabels.ml])
AC_CONFIG_FILES([stdcompat__nativeint.ml])
AC_CONFIG_FILES([stdcompat__option.ml])
AC_CONFIG_FILES([stdcompat__printexc.ml])
AC_CONFIG_FILES([stdcompat__printf.ml])
AC_CONFIG_FILES([stdcompat__queue.ml])
AC_CONFIG_FILES([stdcompat__result.ml])
AC_CONFIG_FILES([stdcompat__seq.ml])
AC_CONFIG_FILES([stdcompat__set.ml])
AC_CONFIG_FILES([stdcompat__spacetime.ml])
AC_CONFIG_FILES([stdcompat__stack.ml])
AC_CONFIG_FILES([stdcompat__stdlib.ml])
AC_CONFIG_FILES([stdcompat__stream.ml])
AC_CONFIG_FILES([stdcompat__string.ml])
AC_CONFIG_FILES([stdcompat__stringLabels.ml])
AC_CONFIG_FILES([stdcompat__sys.ml])
AC_CONFIG_FILES([stdcompat__uchar.ml])
AC_CONFIG_FILES([stdcompat__weak.ml])
AC_CONFIG_FILES([stdcompat__pervasives.mli])
AC_CONFIG_FILES([stdcompat__arg.mli])
AC_CONFIG_FILES([stdcompat__array.mli])
AC_CONFIG_FILES([stdcompat__arrayLabels.mli])
AC_CONFIG_FILES([stdcompat__bool.mli])
AC_CONFIG_FILES([stdcompat__buffer.mli])
AC_CONFIG_FILES([stdcompat__bytes.mli])
AC_CONFIG_FILES([stdcompat__bytesLabels.mli])
AC_CONFIG_FILES([stdcompat__char.mli])
AC_CONFIG_FILES([stdcompat__digest.mli])
AC_CONFIG_FILES([stdcompat__ephemeron.mli])
AC_CONFIG_FILES([stdcompat__filename.mli])
AC_CONFIG_FILES([stdcompat__float.mli])
AC_CONFIG_FILES([stdcompat__format.mli])
AC_CONFIG_FILES([stdcompat__fun.mli])
AC_CONFIG_FILES([stdcompat__hashtbl.mli])
AC_CONFIG_FILES([stdcompat__int.mli])
AC_CONFIG_FILES([stdcompat__int32.mli])
AC_CONFIG_FILES([stdcompat__int64.mli])
AC_CONFIG_FILES([stdcompat__lazy.mli])
AC_CONFIG_FILES([stdcompat__lexing.mli])
AC_CONFIG_FILES([stdcompat__list.mli])
AC_CONFIG_FILES([stdcompat__listLabels.mli])
AC_CONFIG_FILES([stdcompat__map.mli])
AC_CONFIG_FILES([stdcompat__moreLabels.mli])
AC_CONFIG_FILES([stdcompat__nativeint.mli])
AC_CONFIG_FILES([stdcompat__option.mli])
AC_CONFIG_FILES([stdcompat__printexc.mli])
AC_CONFIG_FILES([stdcompat__printf.mli])
AC_CONFIG_FILES([stdcompat__queue.mli])
AC_CONFIG_FILES([stdcompat__result.mli])
AC_CONFIG_FILES([stdcompat__seq.mli])
AC_CONFIG_FILES([stdcompat__set.mli])
AC_CONFIG_FILES([stdcompat__spacetime.mli])
AC_CONFIG_FILES([stdcompat__stack.mli])
AC_CONFIG_FILES([stdcompat__stdlib.mli])
AC_CONFIG_FILES([stdcompat__stream.mli])
AC_CONFIG_FILES([stdcompat__string.mli])
AC_CONFIG_FILES([stdcompat__stringLabels.mli])
AC_CONFIG_FILES([stdcompat__sys.mli])
AC_CONFIG_FILES([stdcompat__uchar.mli])
AC_CONFIG_FILES([stdcompat__weak.mli])
AC_CONFIG_FILES([stdcompat.mli])
AC_CONFIG_FILES([stdcompat.ml])

AC_OUTPUT

AC_MSG_NOTICE([OCaml version is $OCAMLVERSION])
AS_IF([test "x$enable_magic" = "xno"],
  [AC_MSG_NOTICE(
    [[Only pure redefinitions will be used ]]
    [[(--disable-magic: no use of Obj.magic).]])],
  [AC_MSG_NOTICE(
    [[Obj.magic will be used in redefinition when possible ]]
    [[(add --disable-magic for pure redefinitions).]])])
