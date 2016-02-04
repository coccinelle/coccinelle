#! /bin/sh -e

set -e

# this command acts as a replacement for ocamlcp to selectively turn
# off profiling for some files.

# $1: path to ocamlc
# $2: path to ocamlprof

OCAMLC="$1"
OCAMLPROF="$2"
shift 2

noprofile=
case "$@" in

  # skip profiling of files that are already preprocessed.
  *-pp\ *)
    noprofile=1
  ;;

  # uses "include"
  *regexp.ml*)
    noprofile=1
  ;;
  *pycocci.ml*)
    noprofile=1
  ;;
  *prepare_ocamlcocci.ml*)
    noprofile=1
  ;;
esac

if test -n "$noprofile"; then
  exec $OCAMLC "$@"
else
  exec $OCAMLC -pp "$OCAMLPROF -instrument -m a" "$@"
fi
