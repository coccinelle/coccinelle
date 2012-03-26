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
function ocamllibdir {
  ocamlc -where
}

# outputs with what prefix 'python' was configured
function pythonprefix {
  python -c "import sys; print(sys.prefix)" &2>/dev/null || echo "/usr"
}

# outputs the "include" cflags for python
function pythoncflags {
  local version=$1
  local prefix="$(pythonprefix)"

  echo "-I${prefix}/include/python${version}"
}

# outputs the "linker" flags for python
function pythonlibs {
  local version=$1
  local prefix="$(pythonprefix)"

  echo "-L${prefix}/lib -lpython${version}"
}

# succeeds only if "/usr/include/pcre.h" exists
function checkpcre {
  test -f /usr/include/pcre.h
}

# interate through pattern-response pairs
while read pattern
do
  # empty lines preceeding pattern
  if test -z "$pattern"; then
    continue
  fi

  # response may be empty
  read response

  if [[ $cmdline =~ $pattern ]]; then
    if test ! -z "$response"; then
      (eval "R=\"$response\""; echo $R)
    fi

    exit 0
  fi
done < $responsefile

# fallback case
echo "fake-subst.sh: no substitution for: $cmdline" 1>&2
exit 1
