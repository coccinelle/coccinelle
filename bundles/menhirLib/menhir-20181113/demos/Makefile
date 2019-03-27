# These demos assume that Menhir has been installed and is available
# under the name "menhir". This can be overridden by setting MENHIR.

# The following demos require menhirLib:
#   calc-incremental
#   calc-inspection

# The following demos require menhirSdk and OCaml >= 4.03:
#   generate-printers

# The demos whose name ends in -dune require dune 1.4.
# If dune is absent, attempting to build these demos
# succeeds without actually doing anything.

DEMOS := \
  calc \
  calc-alias \
  calc-two \
  calc-param \
  calc-incremental \
  calc-inspection \
  generate-printers \
  calc-dune \
  calc-new-syntax-dune \
  calc-ast-dune \
  calc-param-dune \
  calc-two-dune \
  calc-incremental-dune \
  calc-inspection-dune \
  generate-printers-dune \

.PHONY: all clean test

all clean test::
	@ for i in $(DEMOS) ; do \
	  $(MAKE) --no-print-directory -C $$i $@ ; \
	done

clean::
	/bin/rm -f *~ .*~
