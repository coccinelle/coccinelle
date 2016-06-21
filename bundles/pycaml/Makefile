PACKAGE_DIR=chemoelectric-pycaml-8614105
TARBALL=$(PACKAGE_DIR).tar.gz
MODULES=pycaml
OBJS=pycaml
C_OBJS=pycaml
ARCHIVE=pycaml
LIBRARY=pycaml
SIDEPRODUCTS=pycaml.cmi
SRC_DIR=$(PACKAGE_DIR)

# note: the extra linker flags are needed so that the produced shared
# library gets linked against libpython.
EXTRACFLAGS=$(PYTHON_CFLAGS:%=-ccopt %) $(PYTHON_LIBS:%=-cclib %)
EXTRALNKFLAGS=$(PYTHON_LIBS:%=-ldopt %)

export PYMAJOR=$(shell echo ${PYTHON_VERSION} | sed -e 's/\..*//')

include ../Makefile.bundles

.prepare: $(MARKER)
	rm -f "$(SRC_DIR)/pycaml.mli"
	$(CC) -E -w -D PYMAJOR$(PYMAJOR) -xc pycaml.pp.ml >$(SRC_DIR)/pycaml.ml
	cp pycaml_stubs.c $(SRC_DIR)/pycaml_stubs.c
	touch $@

$(PACKAGE_DIR)/pycaml.cmi $(PACKAGE_DIR)/pycaml.cmo: $(PACKAGE_DIR)/pycaml.ml
	$(OCAMLC_CMD) -c $<
