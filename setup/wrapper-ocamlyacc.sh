#! /bin/sh -e

set -e

# this command acts as a replacement for ocamlyacc that selectively
# runs menhir instead for some files.

# $1: path to menhir
# $2: path to ocamlyacc

MENHIR="$1"
OCAMLYACC="$2"
shift 2

domenhir=
case "$@" in
  *parsing_cocci_menhir.mly*)
    domenhir=1
  ;;
esac

if test -n "$domenhir"; then
  exec $OCAMLC "$@"
else
  exec $OCAMLYACC "$@"
fi
