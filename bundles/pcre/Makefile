# This Makefile serves as a wrapper to bundle the
# pcre module without modifications.

PCRE_VERSION=7.2.3

bytes=$(shell \
	if [ "$$OCAMLVERSION" "<" 4.02.0 ]; then \
	  echo bytes; \
	fi \
)

PACKAGE_DIR=mmottl-pcre-ocaml-258b72c
TARBALL=mmottl-pcre-ocaml-v7.2.3-0-g258b72c.tar.gz
MODULES=pcre_compat pcre bytes
OBJS=$(bytes) pcre_compat pcre
INTERFACES=pcre
C_OBJS=pcre
ARCHIVE=pcre
LIBRARY=pcre
SRC_DIR=$(PACKAGE_DIR)/lib

EXTRACFLAGS=$(PCRE_CFLAGS:%=-ccopt %) $(PCRE_LIBS:%=-cclib %)
EXTRALNKFLAGS=$(PCRE_LIBS:%=-ldopt %)

include ../Makefile.bundles

distclean: clean
	rm -rf $(PACKAGE_DIR)
	rm -f .prepare

.prepare: $(MARKER)
	if [ "$$OCAMLVERSION" "<" 4.02.0 ]; then \
	  cp $(SRC_DIR)/pcre_compat312.ml $(SRC_DIR)/pcre_compat.ml; \
	  cp bytes_compat.ml $(SRC_DIR)/bytes.ml; \
	else \
	  cp $(SRC_DIR)/pcre_compat402.ml $(SRC_DIR)/pcre_compat.ml; \
	fi
	touch $@

# Seems to be needed, but why? Should be deduced from .depend!
$(SRC_DIR)/pcre.cmo: $(SRC_DIR)/pcre_compat.cmo
$(SRC_DIR)/pcre.cmx: $(SRC_DIR)/pcre_compat.cmx
