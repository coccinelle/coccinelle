#! /bin/sh -e

set -e

# wrapper around ocaml-findlib that logs its
# invocations to ocamlfind.log

echo "ocamlfind $@" >> ocamlfind.log
exec ocamlfind "$@"
