#!/bin/sh -e

# This is a program that gives scripted replies
# to queries that are normally handled by programs
# such as pkg-config and ocamlfind. This program
# can serve as a poor substitute.

# the replies file contains sets of two lines. The
# first line is an extended regex to match against
# the command line. The second line is the reply to
# print to stdout. Variables ${BASH_REMATCH[i]} may be
# used to match against capture groups.
# (note: that would then only work when the interpreter
# is bash)

# the replies file assumes that the
# libpcre and python libraries are installed, and
# that none of the optional ocaml libraries are
# installed.

cmdline="$@"
scriptdir=$(dirname "$BASH_SOURCE")
responsefile="$scriptdir/replies.txt"

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
  if test -z "$pattern"; then
    continue
  fi

  # response may be empty
  read response

  if [[ $cmdline =~ $pattern ]]; then
    found=1 
    break
  fi
done < $responsefile

if test -n "$found"; then
  if test -n "$response"; then
    (eval "R=\"$response\""; test $? = 0; echo $R)
    test $? = 0
  fi

  exit 0
else
  # fallback case
  echo "fake-subst.sh: no substitution for: $cmdline" 1>&2
  exit 1
fi
