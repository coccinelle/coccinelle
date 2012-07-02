#! /bin/sh -e

set -e

# If you don't have pdflatex installed, but do have the generated pdf files, then
# this script provides those pdf files as a substitute for the pdflatex invocation.

for arg in "$@"; do
  base="${arg%.*}"
  ext="${arg##*.}"

  if test "x$ext" = xtex; then
    if test -f "${base}.pdf"; then
      echo "fake-pdflatex.sh: ${base}.pdf provided as substitute for: $@"
      touch "${base}.pdf"
      exit 1
    fi
  fi
done

echo "error: pdflatex has not been configured, therefore refusing to execute: $@" 1>&2
exit 1
