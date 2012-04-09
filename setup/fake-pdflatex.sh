#!/bin/sh -e

# If you don't have pdflatex installed, but do have the generated pdf files, then
# this script provides those pdf files as a substitute for the pdflatex invocation.

for arg in "$@"; do
  basename="${arg%.*}"
  extension="${arg##*.}"

  if test "x$extension" = xtex; then
    if test -f "${basename}.pdf"; then
      echo "fake-pdflatex.sh: ${basename}.pdf provided as substitute for: $@"
      touch "${basename}.pdf"
      exit 1
    fi
  fi
done

echo "error: pdflatex has not been configured, therefore refusing to execute: $@" 1>&2
exit 1
