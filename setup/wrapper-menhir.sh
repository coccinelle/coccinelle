#! /bin/sh -e

set -e

# This wrapper is a work-around to fool ocamlbuild to
# use menhir but actually using ocamlyacc for some files.
# Since this wrapper makes some assumptions about how
# ocamlbuild works, it may break in the future.
# In particular: the paths to the files may change.

# $1: path to menhir
# $2: path to ocamlyacc

OCAMLYACC="$1"
MENHIR="$2"
shift 2

case "$@" in

# use ocamlyacc for parsing_c/parsing_c.mly
  *raw-depend*parsing_c/parser_c.mly*)
    # echo "Notice: using ocamlyacc instead of menhir: skipping apriori dependency generation" 1>&2
  ;;
  *parsing_c/parser_c.mly*)
    # echo "Notice: invoking ocamlyacc instead of menhir." 1>&2
    exec $OCAMLYACC parsing_c/parser_c.mly
  ;;

# execute with menhir
  *)
    exec $MENHIR "$@"
  ;;
esac
