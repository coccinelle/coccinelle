# This Makefile serves as a wrapper to bundle the
# pyml module without modifications.

PYML_VERSION=20160621

PACKAGE_DIR=pyml-20160621
TARBALL=pyml-20160621.tar.gz
MODULES=pyml_compat pytypes pywrappers py pycaml_compat
OBJS=pytypes pywrappers py pycaml_compat
INTERFACES=pywrappers py pycaml_compat
C_OBJS=pyml
ARCHIVE=pyml
LIBRARY=pyml
SRC_DIR=$(PACKAGE_DIR)

PYML_COMPAT=$(shell \
	if [ "$(OCAMLVERSION)" "<" 4.03.0 ]; then \
	  echo pyml_compat312.ml; \
	else \
	  echo pyml_compat403.ml; \
	fi \
)

include ../Makefile.bundles

.prepare: $(MARKER) $(SRC_DIR)/generate
	if [ "$$OCAMLVERSION" "<" 4.03.0 ]; then \
	  cp $(SRC_DIR)/pyml_compat312.ml $(SRC_DIR)/pyml_compat.ml; \
	else \
	  cp $(SRC_DIR)/pyml_compat403.ml $(SRC_DIR)/pyml_compat.ml; \
	fi
	cd $(SRC_DIR) && ./generate
	touch $@

$(SRC_DIR)/pyml_compat.mli: $(MARKER)
	touch $@

$(SRC_DIR)/$(PYML_COMPAT): $(MARKER)
	touch $@

$(SRC_DIR)/generate.ml: $(MARKER)
	touch $@

$(SRC_DIR)/pyml_compat.cmx: $(SRC_DIR)/pyml_compat.ml $(SRC_DIR)/pyml_compat.cmi
	$(OCAMLOPT_CMD) -c $< -o $@

$(SRC_DIR)/pyml_compat.cmo: $(SRC_DIR)/pyml_compat.ml $(SRC_DIR)/pyml_compat.cmi
	$(OCAMLC_CMD) -c $< -o $@

$(SRC_DIR)/generate: $(SRC_DIR)/pyml_compat.cmx $(SRC_DIR)/generate.cmx
	$(OCAMLOPT) $^ -o $@

$(SRC_DIR)/generate.cmx: $(SRC_DIR)/generate.ml $(SRC_DIR)/pyml_compat.cmx
	$(OCAMLOPT_CMD) -c $< -o $@

$(SRC_DIR)/pyml_compat.ml: $(SRC_DIR)/$(PYML_COMPAT)
	cp $< $@
