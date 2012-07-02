#! /bin/sh -e

set -e

# If you don't have menhir installed, but you do have the generated files, then
# this script fakes menhir by providing the generated files as the result of the
# menhir invocation.

STATE=0
base=
reqml=
reqmli=

for arg in "$@"; do
  if test "x$STATE" = x0 -a "x$arg" = "x--base" -o "x$arg" = "x-b"; then
    STATE=1
  elif test "x$STATE" = x1; then
    base="$arg"
    STATE=0
  else
    filename="$arg"
    basename="${filename%.*}"
    extension="${filename##*.}"

    # assumes that all commandline parameters ending in .mly are files to be processed by menhir
    if test "x$extension" = xmly; then
      reqml="${basename}.ml"
      reqmli="${basename}.mli"

      # do we have a preprocessed ml file?
      if test ! -f "$reqml"; then
        echo "error: the file $reqml is needed, which requires preprocessing by menhir to obtain it from ${filename}. However, menhir is not enabled." 1>&2
	exit 1
      fi

      # do we have a preprocessed mli file?
      if test ! -f "$reqmli"; then
        echo "error: the files ${basename}.mli is needed, which requires preprocessing by menhir to obtain it from ${filename}. However, menhir is not enabled." 1>&2
	exit 1
      fi

      # is the preprocessed file not older than the original source?
      if test "$reqml" -ot "$filename" -o "$reqmli" -ot "$filename"; then
        echo "error: ${basename}.ml(i) is older than $filename, which requires preprocessing by menhir to update them from ${filename}. However, menhir is not enabled." 1>&2
        exit 1
      fi

      if test -z "$base"; then
        base="$basename"
      fi
    fi
  fi
done

if test -n "$reqml" -a -n "$reqmli" -a -n "$base"; then
  if test "$reqml" != "${base}.ml"; then
    cp -f "$reqml" "${base}.ml"
  fi

  if test "$reqmli" != "${base}.mli"; then
    cp -f "$reqmli" "${base}.mli"
  fi

  touch "${base}.ml"
  touch "${base}.mli"
  exit 0
else
  echo "error: do not know how to handle: $@" 1>&2
  exit 1
fi
