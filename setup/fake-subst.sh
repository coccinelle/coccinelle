#! /bin/sh -e

set -e

# This is a program that gives scripted replies
# to queries that are normally handled by programs
# such as pkg-config and ocamlfind. This program
# can serve as a poor substitute.

# the replies file contains sets of three lines. The
# first line is a pattern for grep. Grep matches
# this pattern against our command line. If fails,
# we skip to the next set of three lines.
# The second line is a replacement pattern for sed (or empty),
# which rewrites our command line, and stores the result
# in $MATCH. The third line is evaluated and may use
# $MATCH. If the execution succeeds, the script exists
# with a zero exit code. Otherwise it repeats the process with
# the next lines in the file.

# the replies file assumes that the
# libpcre and python libraries are installed, and
# that none of the optional ocaml libraries are
# installed.

if test -n "${ZSH_VERSION}"; then
  setopt BASH_REMATCH
fi

if test -z "${BASH_SOURCE}"; then
  BASH_SOURCE=$0
fi


#
# Before we actually try to substitute a given
# command, we first try to execute the original command 
#

# Check if the given program exists.
if command -v "$1" > /dev/null; then
  # exits the script if the command succeeds.
  $@ && exit $?
fi


#
# Trying a substitute
#

cmdline="$@"
scriptdir=$(dirname "${BASH_SOURCE}")
responsefile="${scriptdir}/replies.txt"

# learning mode
# echo "$cmdline" >> /tmp/queries.txt

# some helper functions callable from the replacement macros
ocamllibdir() {
  ocamlc -where
}

# outputs with what prefix 'python' was configured
pythonprefix() {
  python -c "import sys; print(sys.prefix)"
}

pythonversion() {
  python -c "import sys; print('%d.%d' % (sys.version_info[0], sys.version_info[1]))"
}

pythonexists() {
  local version=$1
  local prefix="$(pythonprefix)"
  test $? = 0

  if test -z "$version"; then
    version="$(pythonversion)"
  fi

  if test ! -f "${prefix}/include/python${version}/Python.h"; then
    echo "error: ${prefix}/include/python${version}/Python.h not found (a development version of python is not installed?)" 1>&2
    false
  fi
}

# outputs the "include" cflags for python
pythoncflags() {
  local version=$1
  local prefix="$(pythonprefix)"
  test $? = 0

  echo "-I${prefix}/include/python${version}"
}

# outputs the "linker" flags for python
pythonlibs() {
  local version=$1
  local prefix="$(pythonprefix)"
  test $? = 0

  echo "-L${prefix}/lib -lpython${version}"
}

# succeeds only if "/usr/include/pcre.h" exists
checkpcre() {
  test -f /usr/include/pcre.h
}

# interate through pattern-response pairs
found=
response=
while read pattern
do
  # empty lines preceeding pattern
  if test -z "${pattern}"; then
    continue
  fi

  # response may be empty
  read replace
  read response

  if echo "${cmdline}" | grep -qE "${pattern}"; then
    found=1
    break
  fi
done < "${responsefile}"

if test -n "${found}"; then
  MATCH=no
  if test -n "${replace}"; then
    MATCH="$(echo "$cmdline" | sed -E "${replace}")"
  fi

  if test -n "${response}"; then
    (eval "R=\"${response}\""; test $? = 0; if test -n "${R}"; then echo "${R}"; fi)
    test $? = 0
  fi

  exit 0
else
  # fallback case
  echo "fake-subst.sh: no substitution for: ${cmdline}. Running the 
original." 1>&2
  exec $@
fi
