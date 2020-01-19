dnl  changes from the original for coccinelle:
dnl    replaced AC_CHECK_TOOL with AC_PATH_TOOL to obtain full paths
dnl    removed some of the AC_REQUIRE calls

dnl autoconf macros for OCaml
dnl
dnl Copyright © 2009      Richard W.M. Jones
dnl Copyright © 2009      Stefano Zacchiroli
dnl Copyright © 2000-2005 Olivier Andrieu
dnl Copyright © 2000-2005 Jean-Christophe Filliâtre
dnl Copyright © 2000-2005 Georges Mariano
dnl
dnl For documentation, please read the ocaml.m4 man page.

AC_DEFUN([AC_PROG_OCAML],
[dnl
  # checking for ocamlc
  AC_PATH_TOOL([OCAMLC],[ocamlc],[no])

  if test "$OCAMLC" != "no"; then
     OCAMLVERSION=`$OCAMLC -v | sed -n -e 's|.*version* *\(.*\)$|\1|p'`
     AC_MSG_RESULT([OCaml version is $OCAMLVERSION])
     # If OCAMLLIB is set, use it
     if test "$OCAMLLIB" = ""; then
        OCAMLLIB=`$OCAMLC -where 2>/dev/null || $OCAMLC -v|tail -1|cut -d ' ' -f 4`
     else
        AC_MSG_RESULT([OCAMLLIB previously set; preserving it.])
     fi
     AC_MSG_RESULT([OCaml library path is $OCAMLLIB])

     AC_SUBST([OCAMLVERSION])
     AC_SUBST([OCAMLLIB])

     # checking for ocamlopt
     AC_PATH_TOOL([OCAMLOPT],[ocamlopt],[no])
     OCAMLBEST=byte
     if test "$OCAMLOPT" = "no"; then
	AC_MSG_WARN([Cannot find ocamlopt; bytecode compilation only.])
     else
	TMPVERSION=`$OCAMLOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT([versions differs from ocamlc; ocamlopt discarded.])
	    OCAMLOPT=no
	else
	    OCAMLBEST=opt
	fi
     fi

     AC_SUBST([OCAMLBEST])

     # checking for ocamlc.opt
     AC_PATH_TOOL([OCAMLCDOTOPT],[ocamlc.opt],[no])
     if test "$OCAMLCDOTOPT" != "no"; then
	TMPVERSION=`$OCAMLCDOTOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT([versions differs from ocamlc; ocamlc.opt discarded.])
	else
	    OCAMLC=$OCAMLCDOTOPT
	fi
     fi

     # checking for ocamlopt.opt
     if test "$OCAMLOPT" != "no" ; then
	AC_PATH_TOOL([OCAMLOPTDOTOPT],[ocamlopt.opt],[no])
	if test "$OCAMLOPTDOTOPT" != "no"; then
	   TMPVERSION=`$OCAMLOPTDOTOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
	   if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	      AC_MSG_RESULT([version differs from ocamlc; ocamlopt.opt discarded.])
	   else
	      OCAMLOPT=$OCAMLOPTDOTOPT
	   fi
        fi
     fi

     AC_SUBST([OCAMLOPT])
  fi

  AC_SUBST([OCAMLC])

  # checking for ocaml toplevel
  AC_PATH_TOOL([OCAML],[ocaml],[no])

  # checking for ocamldep
  AC_PATH_TOOL([OCAMLDEP],[ocamldep],[no])

  # checking for ocamlmktop
  AC_PATH_TOOL([OCAMLMKTOP],[ocamlmktop],[no])

  # checking for ocamlmklib
  AC_PATH_TOOL([OCAMLMKLIB],[ocamlmklib],[no])

  # checking for ocamldoc
  AC_PATH_TOOL([OCAMLDOC],[ocamldoc],[no])
])


AC_DEFUN([AC_PROG_OCAMLLEX],
[dnl
  # checking for ocamllex
  AC_PATH_TOOL([OCAMLLEX],[ocamllex],[no])
  if test "$OCAMLLEX" != "no"; then
    AC_PATH_TOOL([OCAMLLEXDOTOPT],[ocamllex.opt],[no])
    if test "$OCAMLLEXDOTOPT" != "no"; then
	OCAMLLEX=$OCAMLLEXDOTOPT
    fi
  fi
  AC_SUBST([OCAMLLEX])
])

AC_DEFUN([AC_PROG_OCAMLYACC],
[dnl
  AC_PATH_TOOL([OCAMLYACC],[ocamlyacc],[no])
  AC_SUBST([OCAMLYACC])
])


AC_DEFUN([AC_PROG_FINDLIB],
[dnl
  dnl  AC_REQUIRE([AC_PROG_OCAML])dnl

  # checking for ocamlfind
  AC_PATH_TOOL([OCAMLFIND],[ocamlfind],[no])
  AC_SUBST([OCAMLFIND])
])


dnl Thanks to Jim Meyering for working this next bit out for us.
dnl XXX We should define AS_TR_SH if it's not defined already
dnl (eg. for old autoconf).
AC_DEFUN([AC_CHECK_OCAML_PKG],
[dnl
  dnl  AC_REQUIRE([AC_PROG_FINDLIB])dnl

  AC_MSG_CHECKING([for OCaml findlib package $1])

  unset found
  unset pkg
  found=no
  for pkg in $1 $2 ; do
    if $OCAMLFIND query $pkg >/dev/null 2>/dev/null; then
      AC_MSG_RESULT([found])
      AS_TR_SH([OCAML_PKG_$1])=$pkg
      found=yes
      break
    fi
  done
  if test "$found" = "no" ; then
    AC_MSG_RESULT([not found])
    AS_TR_SH([OCAML_PKG_$1])=no
  fi

  AC_SUBST(AS_TR_SH([OCAML_PKG_$1]))
])


AC_DEFUN([AC_CHECK_OCAML_MODULE],
[dnl
  AC_MSG_CHECKING([for OCaml module $2])

  cat > conftest.ml <<EOF
open $3
EOF
  unset found
  for $1 in $$1 $4 ; do
    if $OCAMLC -c -I "$$1" conftest.ml >&5 2>&5 ; then
      found=yes
      break
    fi
  done

  if test "$found" ; then
    AC_MSG_RESULT([$$1])
  else
    AC_MSG_RESULT([not found])
    $1=no
  fi
  AC_SUBST([$1])
])


dnl XXX Cross-compiling
AC_DEFUN([AC_CHECK_OCAML_WORD_SIZE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([for OCaml compiler word size])
  cat > conftest.ml <<EOF
  print_endline (string_of_int Sys.word_size)
  EOF
  OCAML_WORD_SIZE=`$OCAML conftest.ml`
  AC_MSG_RESULT([$OCAML_WORD_SIZE])
  AC_SUBST([OCAML_WORD_SIZE])
])

AC_DEFUN([AC_CHECK_OCAML_OS_TYPE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([OCaml Sys.os_type])

  cat > conftest.ml <<EOF
  print_string(Sys.os_type);;
EOF

  OCAML_OS_TYPE=`$OCAML conftest.ml`
  AC_MSG_RESULT([$OCAML_OS_TYPE])
  AC_SUBST([OCAML_OS_TYPE])
])
