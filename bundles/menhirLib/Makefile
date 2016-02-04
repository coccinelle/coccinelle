# Bundles the original menhirLib package without modifications.
# This Makefile integrates its build process with coccinelle.

# the build process is conventional build of the ml files, with
# the exception that all modules are packed as a submodule of
# a master module "MenhirLib" instead of an archive file.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

VERSION=20120123
MENHIRDIR=menhir-$(VERSION)
MENHIRLIB=$(MENHIRDIR)/src
MENHIRMARKER=$(MENHIRLIB)/.marker

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) -I $(MENHIRLIB)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) -I $(MENHIRLIB)
OCAMLDEP_CMD=$(OCAMLDEP) -I $(MENHIRLIB)

all: .depend
	@$(MAKE) all-build
all-build: .depend menhirLib.cma META

all.opt: .depend
	@$(MAKE) all-opt-build
all-opt-build: .depend menhirLib.cmxa META

clean:
	rm -f menhirLib.cma menhirLib.cmxa META \
		$(MENHIRLIB)/*.cm[ioxa] \
		$(MENHIRLIB)/*.o \
		$(MENHIRLIB)/engineTypes.mli $(MENHIRLIB)/tableFormat.mli \
		menhirLib.cm[ioxa] menhirLib.[oa] \
		.depend

distclean: clean
	rm -rf $(MENHIRDIR)

$(MENHIRMARKER): $(MENHIRDIR).tar.gz
	$(TAR) xfvz $<
	touch $@

$(MENHIRDIR).tar.gz:
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

.PHONEY: all all.opt all-build all-opt-build clean distclean depend depend-build

# Sources needed for coccinelle
MODULES=infiniteArray packedIntArray rowDisplacement engineTypes \
		engine tableFormat tableInterpreter convert

MLI=$(MODULES:%=$(MENHIRLIB)/%.mli)
SRC=$(MODULES:%=$(MENHIRLIB)/%.ml)

# Link
menhirLib.cma: .depend $(MLI:.mli=.cmi) $(SRC:.ml=.cmo)
	$(OCAMLC_CMD) -pack -o menhirLib.cmo $(SRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o menhirLib.cma menhirLib.cmo

menhirLib.cmxa: .depend $(MLI:.mli=.cmi) $(SRC:.ml=.cmx)
	$(OCAMLOPT) -pack -o menhirLib.cmx $(SRC:.ml=.cmx)
	$(OCAMLOPT) -a -o menhirLib.cmxa menhirLib.cmx

# Generic rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PRECIOUS: $(MLI)

.ml.mli:
	$(OCAMLC_CMD) -for-pack MenhirLib -i $< > $@

.mli.cmi:
	$(OCAMLC_CMD) -for-pack MenhirLib -c $<

.ml.cmo:
	$(OCAMLC_CMD) -for-pack MenhirLib -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -for-pack MenhirLib -c $<

META: $(MENHIRLIB)/META
	cp $< $@

# the 'depend' target
.depend depend: $(MENHIRMARKER)
	@$(MAKE) depend-build

depend-build: $(MENHIRMARKER) $(SRC) $(MLI)
	$(OCAMLDEP_CMD) $(MENHIRLIB)/*.mli $(MENHIRLIB)/*.ml > .depend

# extra dependencies that are needed to generate mli files
# that are needed for ocamldep
$(MENHIRLIB)/tableFormat.mli: $(MENHIRLIB)/packedIntArray.cmi $(MENHIRLIB)/engineTypes.cmi

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),depend-build)
include .depend
endif
endif
endif

include ../../Makefile.common
