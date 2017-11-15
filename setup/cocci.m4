dnl
dnl  autoconf helper macros for coccinelle
dnl


dnl  check if the ocaml version is recent enough
dnl    $1: the variable to assign
dnl    $2: version to test against
AC_DEFUN([AC_CHECK_OCAMLVERSION],
[dnl
  AS_UNSET([versioncheck])
  AC_MSG_CHECKING([that the OCaml version is at least $2])
  AS_VERSION_COMPARE([$OCAMLVERSION],[$2],[versioncheck=no],[versioncheck=yes],[versioncheck=yes])
  AC_MSG_RESULT([$versioncheck])
  AC_SUBST([$1], [$versioncheck])
])


dnl  using ocamlfind to store into $1 the location where
dnl  a package $2 resides.
AC_DEFUN([AC_COCCI_OCAMLPKGDIR],
[dnl
  AC_SUBST(AS_TR_SH([PATH_$1]),[`$OCAMLFIND query $1 2>/dev/null`])
])


dnl  stores the result of checking for the
dnl  ocaml package given as $2 in the
dnl  variable given as $1
AC_DEFUN([AC_COCCI_OCAMLPKG],
[dnl
  AC_CHECK_OCAML_PKG([$1])
  AS_IF([test "x$[]AS_TR_SH([OCAML_PKG_$1])" != xno],[AC_COCCI_OCAMLPKGDIR([$1])])
])

dnl  requires that the ocaml package is installed.
dnl  it is assumed that this package is part of the
dnl  ocaml installation.
AC_DEFUN([AC_REQ_COCCI_STDPKG],
[dnl
  AC_COCCI_OCAMLPKG([$1])
  AS_IF([test "x$[]AS_TR_SH([OCAML_PKG_$1])" = xno],
  [dnl
    AC_MSG_ERROR([package $1 is required. It should be part of your ocaml installation.])
  ])
])


dnl  defines the COCCI_OCAML_EXTERNAL variable to point to the directory
dnl  with extra ocaml packages
AC_DEFUN([AC_COCCI_SET_EXTERNAL_DIR],
[dnl
  AC_ARG_VAR(COCCI_OCAML_EXTERNAL, [path to extra ocaml packages (default: $1)])
  AC_SUBST([COCCI_OCAML_EXTERNAL],["$1"])
  AC_MSG_NOTICE([coccinelle may use external ocaml libraries in $COCCI_OCAML_EXTERNAL])
])


dnl  handle optional packages for which coccinelle may have
dnl  local versions.
dnl
dnl  Note: this macro sets additional variables for use with
dnl  'Makefile.config'.
dnl
dnl  variables:
dnl    enable_$1: either 'yes', 'local', or 'no'
AC_DEFUN([AC_CHECK_COCCI_EXTPKG],
[dnl
  AC_MSG_NOTICE([configuring package $1])
  AC_ARG_ENABLE([$1], AS_HELP_STRING([--enable-$1], [enable global package $1 (yes,no) (default: auto)]))

  dnl  try and find a globally installed version
  dnl  if not, enable_$1 will be "no"
  AS_IF([test "x$[]AS_TR_SH([enable_$1])" != xno],
  [dnl
    AC_COCCI_OCAMLPKG([$1])
    AC_SUBST(AS_TR_SH([GLOBAL_$1]),[$[]AS_TR_SH([OCAML_PKG_$1])])

    AS_IF([test "x$[]AS_TR_SH([GLOBAL_$1])" != xno],
    [dnl  when the package is available
      AC_SUBST(AS_TR_SH([enable_$1]),[yes])
    ],
    [dnl  when the package is not available
      AS_IF([test "x$[]AS_TR_SH([enable_$1])" = xyes],
      [dnl  when explicitly requested the global version
        AC_MSG_ERROR([OCaml package $1 is not available but requested explicitly])
      ])
      AC_MSG_NOTICE([OCaml package $1 is not available])
      AC_SUBST(AS_TR_SH([enable_$1]),[no])
    ])
  ])

  dnl  check for a local package
  AS_IF([test "x$AS_TR_SH([enable_$1])" = xno],
  [dnl
    AS_UNSET([pkgdir])
    pkgdir="$COCCI_OCAML_EXTERNAL/$1/"
    AC_MSG_CHECKING([for a bundled substitute of $1])
    AS_IF([test -d "$pkgdir"],
    [dnl
      AC_MSG_RESULT([yes])
      AC_MSG_NOTICE([using bundled substitute for $1 in $pkgdir])
      AC_SUBST(AS_TR_SH([enable_$1]), [local])
      AC_SUBST(AS_TR_SH([PATH_$1]), ["$pkgdir"])
    ],
    [AC_MSG_RESULT([not available])])
  ])

  dnl  additional handling
  AS_IF([test "x$[]AS_TR_SH([enable_$1])" != xno],
  [dnl
    AC_SUBST(AS_TR_SH([FEATURE_$1]),[1])
    AC_SUBST(AS_TR_SH([FLAGS_$1]),['$([]AS_TR_SH([FLAGS_$1]))'])
    AC_SUBST(AS_TR_SH([OPTFLAGS_$1]),['$([]AS_TR_SH([OPTFLAGS_$1]))'])

    dnl  distinguish global/local
    AS_IF([test "x$[]AS_TR_SH([enable_$1])" = xlocal],
    [dnl
      AC_SUBST(AS_TR_SH([LOCALLIB_$1]),[1])
      AC_SUBST(AS_TR_SH([MODULES_$1]),['$(AS_TR_SH([LOCAL_$1]))'])
      AC_SUBST(AS_TR_SH([MODULESOPT_$1]),['$(AS_TR_SH([LOCALOPT_$1]))'])

      dnl check if the local directory has a Makefile
      AS_IF([test -f "$[]AS_TR_SH([PATH_$1])/Makefile"],
      [dnl
        AC_SUBST(AS_TR_SH([MAKE_$1]),[$[]AS_TR_SH([PATH_$1])])
      ],
      [dnl
        AC_SUBST(AS_TR_SH([MAKE_$1]),[ ])
      ])
    ],
    [dnl
      AC_SUBST(AS_TR_SH([MODULES_$1]),['$(AS_TR_SH([GLOBAL_$1]))'])
      AC_SUBST(AS_TR_SH([MODULESOPT_$1]),['$(AS_TR_SH([GLOBALOPT_$1]))'])
    ])
  ])
])


dnl  initializes the defaults substitutions for
dnl  configuration variables of packages
AC_DEFUN([AC_COCCI_INIT_PKG_EMPTY],
[dnl
  AC_SUBST(AS_TR_SH([FEATURE_$1]), [0])
  AC_SUBST(AS_TR_SH([LOCALLIB_$1]), [0])
  AC_SUBST(AS_TR_SH([FLAGS_$1]), [ ])
  AC_SUBST(AS_TR_SH([MODULES_$1]), [ ])
  AC_SUBST(AS_TR_SH([MODULESOPT_$1]), [ ])
  AC_SUBST(AS_TR_SH([PATH_$1]), [ ])
])


dnl  version of AC_CHECK_COCCI_EXTPKG that fails with an
dnl  error if the package could not be found and no local
dnl  substitute is available.
AC_DEFUN([AC_REQ_COCCI_EXTPKG],
[dnl
  AC_CHECK_COCCI_EXTPKG([$1])
  AS_IF([test "x$[]AS_TR_SH([enable_$1])" = xno],
  [dnl
    AC_MSG_ERROR([OCaml package $1 is required. Please make sure it is available.])
  ])
])


dnl  determine python version
AC_ARG_VAR([PYVER], [python version])
AC_DEFUN([AC_COCCI_PYVER],
[dnl
  AS_IF([test -z "$PYVER"],
  [dnl  PYVER not set before, determine it

    dnl  first try the generic "python" executable or what the user configured
    dnl  as commandline parameter
    AC_COCCI_TOOL([PYTHON],[python],[])

    dnl  some fall-back alternatives in case the above did not find anything
    AS_IF([test "x$PYTHON" = xno -a -z "$with_python"],
    [dnl
      AC_PATH_PROGS([PYTHON],[python python3 python3.2 python3.1 python2 python2.7 python2.6 python2.5])
      AS_IF([test -z "$PYTHON"],[AC_SUBST([PYTHON],[no])])
    ])

    AS_IF([test "x$PYTHON" = xno -a -n "$with_python" -a "x$with_python" != xyes],
    [dnl  python interpreter not found, but perhaps it was a version
      AC_MSG_NOTICE([$with_python is not a found as tool, therefore interpreted as version])
      AC_SUBST([PYVER],["$with_python"])
    ])

    AS_IF([test "x$PYTHON" != xno],
    [dnl  python interpereter found
      AC_MSG_CHECKING([python version])
      PYVER=`$PYTHON -c "import sys; print(sys.version[[:3]])"`
      AS_IF([test -n "$PYVER"],[AC_MSG_RESULT([$PYVER found])],[AC_MSG_RESULT([failed])])
      AC_SUBST([PYVER])
    ])
  ],
  [dnl  PYVER set before
    AC_MSG_NOTICE([python version assumed to be $PYVER])
  ])

  dnl  determine major version of pyver
  AC_SUBST([PYVER_MAJOR],[${PYVER%%.*}])
  AC_MSG_NOTICE([python major version: $PYVER_MAJOR])
])


dnl  determine version date (RTC format)
AC_DEFUN([AC_COCCI_CONFVERSION],
[dnl
  AC_SUBST([CONFVERSION])
  AC_MSG_NOTICE([determining version suffix])

  AS_IF([test -z "$CONFVERSION" -a -d "./.git"],
  [dnl  git administration found
    AC_MSG_NOTICE([building a version from a git repository])
    AC_PATH_TOOL([GIT],[git])
    AS_IF([test -n "$GIT"],
    [dnl  ask git
      CONFVERSION=`$GIT log -1 --date-order --date=rfc --pretty="format:%cd"`
    ])
  ])

  AS_IF([test -z "$CONFVERSION"],
  [dnl  otherwise, take the changelog date
    AC_PATH_TOOL([DATE],[date])
    AS_IF([test -n "$DATE"],
    [dnl
      CONFVERSION=`$DATE -u -r changes.txt "+%a, %d %b %Y %H:%M:%S %z"`
    ])
  ])

  AS_IF([test -z "$CONFVERSION"],
  [dnl  fallback
    CONFVERSION=unknown
  ])

  AC_MSG_NOTICE([version suffix set to $CONFVERSION])
])


dnl  find a tool, with specialized macros for certain cases
dnl  $1: name of the variable
dnl  $2: name of the tool
AC_DEFUN([AC_COCCI_FINDTOOL],
[dnl
  AS_IF([test "x$2" = xpkg-config -a "x$1" = xPKG_CONFIG],
  [dnl  specialized macro for pkg-config (from pkg-config m4 macros)
    PKG_PROG_PKG_CONFIG
  ], [test "x$2" = xocamllex -a "x$1" = xOCAMLLEX],
  [dnl  specialized macro for ocamllex (from ocaml.m4)
    AC_PROG_OCAMLLEX
  ], [test "x$2" = xocamlyacc -a "x$1" = xOCAMLYACC],
  [dnl  specialized macro for ocamlyacc (from ocaml.m4)
    AC_PROG_OCAMLYACC
  ],
  [dnl  generic macro
    AC_PATH_TOOL([$1], [$2])
  ])
])

dnl  find/override a particular tool
dnl  $1: var name
dnl  $2: prog name
dnl  $3: path to substitute (or empty)
AC_DEFUN([AC_COCCI_TOOL],
[dnl
  AC_ARG_VAR([$1], [path to $2])
  AC_ARG_WITH([$2], [AS_HELP_STRING([--with-$2], [whether/which $2 to use (default: auto)])])
  AC_SUBST([with_$1],["$with_[]AS_TR_SH([$2])"])  dnl sets with_$1

  dnl  explicit tool or command given
  AS_IF([test -n "$with_[]$1" -a "x$with_[]$1" != xno -a "x$with_[]$1" != xyes],
  [dnl  custom $with_$1 given
    AC_SUBST([$1], ["$with_[]$1"])
  ],
  [dnl  otherwise, use the default command name
    AC_SUBST([$1], ["$2"])
  ])

  dnl  searches for the tool (result either empty or 'no' if not found)
  AS_IF([test "x$with_$1" = xno],
  [dnl  disabled
    AC_MSG_NOTICE([$2 is disabled explicitly])
    AC_SUBST([$1], [no])
  ],
  [dnl  find the tool
    AC_COCCI_FINDTOOL([$1],[$2])
  ])

  AS_IF([test -z "[$]$1" -o "x[$]$1" = xno],
  [dnl  command not found
    AS_IF([test "x$with_$1" = xyes],
    [dnl  abort if a command was given explicitly
      AC_MSG_ERROR([--with=$2 is given explicitly but not found])
    ])

    AS_IF([test -n "$3"],
    [dnl  try substitute
      AC_MSG_NOTICE([$2 not found. Trying substitute $3.])
      AC_SUBST([$1],["$3"])
      AC_COCCI_FINDTOOL([$1],[$2])
      AC_SUBST([SUBSTITUTED_$1], [yes])
    ])
  ])

  dnl  $1 will always be defined at the exit of this macro
  AS_IF([test -z "[$]$1"],[AC_SUBST([$1],[no])])
])


dnl  defines a $1_CMD with $1 if set to a tool, otherwise
dnl  takes $2
AC_DEFUN([AC_COCCI_RUNTIME_CMD],
[dnl
  AC_ARG_VAR([RUNTIME_$1_CMD], [path to $2])
  AC_ARG_WITH([runtime-$2], [AS_HELP_STRING([--with-runtime-$2], [override the runtime cmd for $2])])

  AS_IF([test -z "$RUNTIME_$1_CMD"],
  [dnl  variable not yet set
    AS_IF([test "x$with_runtime_[]AS_TR_SH([$2])" = xno],
    [dnl  with_runtime_$2 set to no: use configured with_$2
      AC_SUBST([RUNTIME_$1_CMD],[$][$1])
    ], [test -n "$with_runtime_[]AS_TR_SH([$2])" -a "x$with_runtime_[]AS_TR_SH([$2])" != xyes],
    [dnl  explicit with_runtime_$2 parameter given: use that as default
      AC_SUBST([RUNTIME_$1_CMD],["$with_runtime_[]AS_TR_SH([$2])"])
    ],
    [dnl  otherwise, use $2
      AC_SUBST([RUNTIME_$1_CMD],["$2"])
    ])
  ])
])


AC_DEFUN([AC_COCCI_YESNO],
[dnl
  AC_PATH_TOOL([YES], [yes])
  AS_IF([test -n "$YES" -a "x$YES" != xno],
  [dnl
    AC_SUBST([YES_N_CMD],["$YES n"])
  ],
  [dnl
    AC_SUBST([YES_N_CMD],["echo -e 'n\nn'"])
  ])
])
