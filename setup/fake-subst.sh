#!/bin/sh -e

# This is a program that gives scripted replies
# to queries that are normally handled by programs
# such as pkg-config and ocamlfind. This program
# can serve as a poor substitute.

# the replies file contains sets of two lines. The
# first line is an extended regex to match against
# the command line. The second line is the reply to
# print to stdout. Variables $BASH_REMATCH[] may be
# used to match against capture groups.

cmdline="$@"
scriptdir=$(dirname "$BASH_SOURCE")
responsefile="$scriptdir/replies.txt"

# some helper functions callable from the replacement macros
function ocamllibdir {
  echo "$(dirname $(which ocaml))/../lib/ocaml"
}

# debugging
# echo "$cmdline" >> /tmp/queries.txt

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
      eval echo $response
    fi

    exit 0
  fi
done < $responsefile

# fallback case
echo "fake-subst.sh: no substitution for: $cmdline" 1>&2
exit 1
