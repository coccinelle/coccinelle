AC_INIT([[stdcompat]], [8], [[Thierry.Martinez@inria.fr]], [[stdcompat]],
  [[https://github.com/thierry.martinez/stdcompat/]])
AM_INIT_AUTOMAKE

AC_DEFUN([AC_OCAMLFIND_PKG],
  [AC_MSG_CHECKING([for ocamlfind package $2])
   AS_IF([$OCAMLFIND query $2 >/dev/null 2>&1],
     [AC_MSG_RESULT([yes])
      AC_SUBST([$1], [$2])],
     [AC_MSG_RESULT([no])
      AC_SUBST([$1], [])])])

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
   AC_SUBST([RESULT_PKG], [])
   AC_SUBST([SEQ_PKG], [])
   AC_SUBST([UCHAR_PKG], [])],
  [AC_SUBST([OCAMLC], [["$OCAMLFIND ocamlc"]])
   AC_SUBST([OCAMLOPT], [["$OCAMLFIND ocamlopt"]])
   AC_SUBST([OCAMLDOC], [["$OCAMLFIND ocamldoc"]])
   AC_SUBST([OCAMLDEP], [["$OCAMLFIND ocamldep"]])
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

AC_C_BIGENDIAN(
  [AC_SUBST([BIG_ENDIAN], [[true]])],
  [AC_SUBST([BIG_ENDIAN], [[false]])],
  [AC_MSG_ERROR([[Unknown endianness]])])

AC_MSG_CHECKING([OCaml version])
AC_SUBST([OCAMLVERSION],
  [[`$OCAMLBEST -v | sed -n 's/^.*version \(.*\)$/\1/p'`]])
AC_MSG_RESULT([$OCAMLVERSION])

AC_ARG_ENABLE([magic],
            [AS_HELP_STRING([--enable-magic],
              [[use Obj.magic in redefinitions when possible (default)]]),
             AS_HELP_STRING([--disable-magic],
              [[use only pure redefinitions (without Obj.magic)]])],
            [],
            [AC_SUBST([enable_magic], [yes])])

AC_DEFUN([AC_DISABLE_BLOCK],
  [AC_SUBST([BEGIN_$1], [['(*']])
   AC_SUBST([END_$1], [['*)']])])

AC_DEFUN([AC_ENABLE_BLOCK],
  [AC_SUBST([BEGIN_$1], [])
   AC_SUBST([END_$1], [])])

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

AM_CONDITIONAL(OCAML_SUPPORTS_BYTECODE, [test "x$OCAMLC" != "x"])

AM_CONDITIONAL(OCAML_SUPPORTS_NATIVE, [test "x$OCAMLOPT" != "x"])

AM_CONDITIONAL(OCAML_SUPPORTS_SHARED,
  [test `printf "$OCAMLVERSION\n3.11.1" | sort | head -n1` = 3.11.1])

AM_CONDITIONAL(OCAML_SUPPORTS_NO_ALIAS_DEPS,
  [test `printf "$OCAMLVERSION\n4.02.0" | sort | head -n1` = 4.02.0])

AC_SUBST([INCLUDE_DEPEND],[['
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
include .depend
endif
endif
']])

AC_CONFIG_FILES([META Makefile])
AC_CONFIG_FILES([
  stdcompat__hashtbl_ext.mli
  stdcompat__hashtbl_ext.ml
  stdcompat__init.mli
  stdcompat__init.ml
  stdcompat__root.mli
  stdcompat__root.ml
  stdcompat__tools.mli
  stdcompat__tools.ml
  stdcompat__native.ml_byte
  stdcompat__native.ml_native
  stdcompat_tests.ml
  stdcompat__pervasives_s.mli
  stdcompat__arg_s.mli
  stdcompat__array_s.mli
  stdcompat__arrayLabels_s.mli
  stdcompat__bool_s.mli
  stdcompat__buffer_s.mli
  stdcompat__bytes_s.mli
  stdcompat__bytesLabels_s.mli
  stdcompat__char_s.mli
  stdcompat__digest_s.mli
  stdcompat__ephemeron_s.mli
  stdcompat__filename_s.mli
  stdcompat__float_s.mli
  stdcompat__format_s.mli
  stdcompat__fun_s.mli
  stdcompat__hashtbl_s.mli
  stdcompat__int_s.mli
  stdcompat__int32_s.mli
  stdcompat__int64_s.mli
  stdcompat__lazy_s.mli
  stdcompat__lexing_s.mli
  stdcompat__list_s.mli
  stdcompat__listLabels_s.mli
  stdcompat__map_s.mli
  stdcompat__moreLabels_s.mli
  stdcompat__nativeint_s.mli
  stdcompat__option_s.mli
  stdcompat__printf_s.mli
  stdcompat__queue_s.mli
  stdcompat__result_s.mli
  stdcompat__seq_s.mli
  stdcompat__set_s.mli
  stdcompat__spacetime_s.mli
  stdcompat__stack_s.mli
  stdcompat__stream_s.mli
  stdcompat__string_s.mli
  stdcompat__stringLabels_s.mli
  stdcompat__sys_s.mli
  stdcompat__uchar_s.mli
  stdcompat__weak_s.mli
  stdcompat__stdlib_s.mli
  stdcompat__pervasives_s.ml:stdcompat__pervasives_s.mli.in
  stdcompat__arg_s.ml:stdcompat__arg_s.mli.in
  stdcompat__array_s.ml:stdcompat__array_s.mli.in
  stdcompat__arrayLabels_s.ml:stdcompat__arrayLabels_s.mli.in
  stdcompat__bool_s.ml:stdcompat__bool_s.mli.in
  stdcompat__buffer_s.ml:stdcompat__buffer_s.mli.in
  stdcompat__bytes_s.ml:stdcompat__bytes_s.mli.in
  stdcompat__bytesLabels_s.ml:stdcompat__bytesLabels_s.mli.in
  stdcompat__char_s.ml:stdcompat__char_s.mli.in
  stdcompat__digest_s.ml:stdcompat__digest_s.mli.in
  stdcompat__ephemeron_s.ml:stdcompat__ephemeron_s.mli.in
  stdcompat__filename_s.ml:stdcompat__filename_s.mli.in
  stdcompat__float_s.ml:stdcompat__float_s.mli.in
  stdcompat__format_s.ml:stdcompat__format_s.mli.in
  stdcompat__fun_s.ml:stdcompat__fun_s.mli.in
  stdcompat__hashtbl_s.ml:stdcompat__hashtbl_s.mli.in
  stdcompat__int_s.ml:stdcompat__int_s.mli.in
  stdcompat__int32_s.ml:stdcompat__int32_s.mli.in
  stdcompat__int64_s.ml:stdcompat__int64_s.mli.in
  stdcompat__lazy_s.ml:stdcompat__lazy_s.mli.in
  stdcompat__lexing_s.ml:stdcompat__lexing_s.mli.in
  stdcompat__list_s.ml:stdcompat__list_s.mli.in
  stdcompat__listLabels_s.ml:stdcompat__listLabels_s.mli.in
  stdcompat__map_s.ml:stdcompat__map_s.mli.in
  stdcompat__moreLabels_s.ml:stdcompat__moreLabels_s.mli.in
  stdcompat__nativeint_s.ml:stdcompat__nativeint_s.mli.in
  stdcompat__option_s.ml:stdcompat__option_s.mli.in
  stdcompat__printf_s.ml:stdcompat__printf_s.mli.in
  stdcompat__queue_s.ml:stdcompat__queue_s.mli.in
  stdcompat__result_s.ml:stdcompat__result_s.mli.in
  stdcompat__seq_s.ml:stdcompat__seq_s.mli.in
  stdcompat__set_s.ml:stdcompat__set_s.mli.in
  stdcompat__spacetime_s.ml:stdcompat__spacetime_s.mli.in
  stdcompat__stack_s.ml:stdcompat__stack_s.mli.in
  stdcompat__stdlib_s.ml:stdcompat__stdlib_s.mli.in
  stdcompat__stream_s.ml:stdcompat__stream_s.mli.in
  stdcompat__string_s.ml:stdcompat__string_s.mli.in
  stdcompat__stringLabels_s.ml:stdcompat__stringLabels_s.mli.in
  stdcompat__sys_s.ml:stdcompat__sys_s.mli.in
  stdcompat__uchar_s.ml:stdcompat__uchar_s.mli.in
  stdcompat__weak_s.ml:stdcompat__weak_s.mli.in
  stdcompat__pervasives.ml
  stdcompat__arg.ml
  stdcompat__array.ml
  stdcompat__arrayLabels.ml
  stdcompat__bool.ml
  stdcompat__buffer.ml
  stdcompat__bytes.ml
  stdcompat__bytesLabels.ml
  stdcompat__char.ml
  stdcompat__digest.ml
  stdcompat__ephemeron.ml
  stdcompat__filename.ml
  stdcompat__float.ml
  stdcompat__format.ml
  stdcompat__fun.ml
  stdcompat__hashtbl.ml
  stdcompat__int.ml
  stdcompat__int32.ml
  stdcompat__int64.ml
  stdcompat__lazy.ml
  stdcompat__lexing.ml
  stdcompat__list.ml
  stdcompat__listLabels.ml
  stdcompat__map.ml
  stdcompat__moreLabels.ml
  stdcompat__nativeint.ml
  stdcompat__option.ml
  stdcompat__printf.ml
  stdcompat__queue.ml
  stdcompat__result.ml
  stdcompat__seq.ml
  stdcompat__set.ml
  stdcompat__spacetime.ml
  stdcompat__stack.ml
  stdcompat__stdlib.ml
  stdcompat__stream.ml
  stdcompat__string.ml
  stdcompat__stringLabels.ml
  stdcompat__sys.ml
  stdcompat__uchar.ml
  stdcompat__weak.ml
  stdcompat__pervasives.mli
  stdcompat__arg.mli
  stdcompat__array.mli
  stdcompat__arrayLabels.mli
  stdcompat__bool.mli
  stdcompat__buffer.mli
  stdcompat__bytes.mli
  stdcompat__bytesLabels.mli
  stdcompat__char.mli
  stdcompat__digest.mli
  stdcompat__ephemeron.mli
  stdcompat__filename.mli
  stdcompat__float.mli
  stdcompat__format.mli
  stdcompat__fun.mli
  stdcompat__hashtbl.mli
  stdcompat__int.mli
  stdcompat__int32.mli
  stdcompat__int64.mli
  stdcompat__lazy.mli
  stdcompat__lexing.mli
  stdcompat__list.mli
  stdcompat__listLabels.mli
  stdcompat__map.mli
  stdcompat__moreLabels.mli
  stdcompat__nativeint.mli
  stdcompat__option.mli
  stdcompat__printf.mli
  stdcompat__queue.mli
  stdcompat__result.mli
  stdcompat__seq.mli
  stdcompat__set.mli
  stdcompat__spacetime.mli
  stdcompat__stack.mli
  stdcompat__stdlib.mli
  stdcompat__stream.mli
  stdcompat__string.mli
  stdcompat__stringLabels.mli
  stdcompat__sys.mli
  stdcompat__uchar.mli
  stdcompat__weak.mli
  stdcompat.mli
  stdcompat.ml])

AC_OUTPUT

AC_MSG_NOTICE([OCaml version is $OCAMLVERSION])
AS_IF([test "x$enable_magic" = "xno"],
  [AC_MSG_NOTICE(
    [[Only pure redefinitions will be used ]]
    [[(--disable-magic: no use of Obj.magic).]])],
  [AC_MSG_NOTICE(
    [[Obj.magic will be used in redefinition when possible ]]
    [[(add --disable-magic for pure redefinitions).]])])
