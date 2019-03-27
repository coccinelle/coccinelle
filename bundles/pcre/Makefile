# This Makefile serves as a wrapper to bundle the
# pcre module without modifications.

MODULES=pcre
OBJS=pcre
INTERFACES=pcre
C_OBJS=pcre
ARCHIVE=pcre
LIBRARY=pcre
C_LIBS=pcre
SRC_DIR=pcre-ocaml/src

OCAMLFLAGS+=-I ../stdcompat

EXTRACFLAGS=$(PCRE_CFLAGS:%=-ccopt %) $(PCRE_LIBS:%=-cclib %)
EXTRALNKFLAGS=$(PCRE_LIBS:%=-ldopt %)

include ../Makefile.bundles

.PHONY: distclean
distclean:
