# This Makefile serves as a wrapper to bundle the
# pcre module without modifications.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
-include ../../Makefile.local
endif

PCRE_VERSION=7.2.3
PCRE_ARCHIVE=mmottl-pcre-ocaml-v7.2.3-0-g258b72c.tar.gz
PCRE_DIR=mmottl-pcre-ocaml-258b72c
PCRE_LIB=$(PCRE_DIR)/lib
PCRE_MARKER=$(PCRE_DIR)/.marker

all: $(PCRE_MARKER)
	@$(MAKE) all-build
all-build: $(PCRE_MARKER) pcre.cma
all.opt: $(PCRE_MARKER)
	@$(MAKE) all-opt-build
all-opt-build: $(PCRE_MARKER) pcre.cmxa

# prepares the source bundle for building.
.prepare: $(PCRE_MARKER)
	touch .prepare

clean:
	rm -f dllpcre_stubs.so libpcre_stubs.a pcre_stubs.o \
		pcre.cma pcre.cmi pcre.cmo \
		pcre.a pcre.cmx pcre.cmxa pcre.o pcre.annot
	- $(MAKE) -C $(PCRE_DIR) clean

distclean: clean
	rm -rf $(PCRE_DIR)
	rm -f .prepare

depend: $(PCRE_MARKER)

.PHONY: all all.opt clean distclean depend all-build all-opt-build

# prepare the pcre directory
$(PCRE_MARKER): $(PCRE_ARCHIVE)
	$(TAR) xfvz $<
	touch $@

$(PCRE_ARCHIVE):
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

pcre.cma pcre.cmxa:
	cd $(PCRE_DIR); ./configure
	unset OCAMLLIB && $(MAKE) -C $(PCRE_DIR)
	cp $(PCRE_DIR)/_build/lib/* .

include ../../Makefile.common
