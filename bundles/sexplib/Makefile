# This Makefile serves as a wrapper to bundle the
# sexplib package without modifications.

# This is a standard build with:
#  * all files marked with -for-pack Sexplib
#  * required packages unix num bigarray
#  * parser.mly requires ocamlyacc
#  * lexer.mll requires ocamllex
#  * pre_sexp.ml: use cpp -traditional -undef
#      (but all other files can be processed with this one as well)

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

VERSION=7.0.5
SEXPDIR=sexplib-$(VERSION)
SEXPLIB=$(SEXPDIR)/lib
SEXPMARKER=$(SEXPDIR)/.marker

PP_CMD=$(CPP) -x c -w -traditional -undef
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) -I $(SEXPLIB) -pp "${PP_CMD}"
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) -I $(SEXPLIB) -pp "${PP_CMD}"
OCAMLDEP_CMD=$(OCAMLDEP) -I $(SEXPLIB) -pp "${PP_CMD}"

all: .depend
	@$(MAKE) all-build
all-build: .depend $(SEXPMARKER) sexplib.cma META

all.opt: .depend
	@$(MAKE) all-opt-build
all-opt-build: .depend $(SEXPMARKER) sexplib.cmxa META

# prepares the source bundle for building.
.prepare: $(SEXPMARKER)
	touch .prepare

clean:
	rm -f sexplib.cma sexplib.cmxa META \
		$(SEXPLIB)/*.cm[ioxa] \
		$(SEXPLIB)/*.o \
		$(SEXPLIB)/parser.ml $(SEXPLIB)/parser.mli \
		$(SEXPLIB)/lexer.ml $(SEXPLIB)/lexer.mli \
		sexplib.cm[ioxa] sexplib.[oa] \
		.depend

distclean: clean
	rm -rf $(SEXPDIR)
	rm -f .prepare

$(SEXPMARKER): $(SEXPDIR).tar.gz sexp-3.10-compat.patch
	$(TAR) xfvz $<
	$(PATCH) -d $(SEXPDIR) -p1 < ./sexp-3.10-compat.patch
	touch $@

$(SEXPDIR).tar.gz:
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

.PHONY: all all.opt clean distclean depend depend-build all-build all-opt-build


# Sources needed for coccinelle
MODULES=type parser lexer pre_sexp sexp_intf \
		sexp path conv conv_error

MLI=$(MODULES:%=$(SEXPLIB)/%.mli)
SRC=$(MODULES:%=$(SEXPLIB)/%.ml)

# Link (produces the .cmi file as well)
sexplib.cma: .depend $(MLI:.mli=.cmi) $(SRC:.ml=.cmo)
	$(OCAMLC_CMD) -pack -o sexplib.cmo $(SRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o sexplib.cma sexplib.cmo

sexplib.cmxa: .depend $(MLI:.mli=.cmi) $(SRC:.ml=.cmx)
	$(OCAMLOPT) -pack -o sexplib.cmx $(SRC:.ml=.cmx)
	$(OCAMLOPT) -a -o sexplib.cmxa sexplib.cmx

# Generic rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll
.PRECIOUS: $(MLI) $(SRC)

.mly.ml:
	$(OCAMLYACC) $< > $@

.mll.ml:
	$(OCAMLLEX) -o $@ $<

.ml.mli:
	$(OCAMLC_CMD) -for-pack Sexplib -i $< > $@

.mli.cmi:
	$(OCAMLC_CMD) -for-pack Sexplib -c $<

.ml.cmo:
	$(OCAMLC_CMD) -for-pack Sexplib -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -for-pack Sexplib -c $<

META: $(SEXPLIB)/META
	cp $< $@

# the 'depend' target
.depend depend: $(SEXPMARKER)
	@$(MAKE) depend-build

depend-build: $(SEXPMARKER) $(SRC) $(MLI)
	$(OCAMLDEP_CMD) $(SEXPLIB)/*.mli $(SEXPLIB)/*.ml > .depend

# extra dependencies that are needed to generate mli files which
# are needed for ocamldep
$(SEXPLIB)/lexer.mli: $(SEXPLIB)/parser.cmi
$(SEXPLIB)/parser.mli: $(SEXPLIB)/type.cmi
$(SEXPLIB)/sexp_intf.mli: $(SEXPLIB)/pre_sexp.cmi
$(SEXPLIB)/conv.mli: $(SEXPLIB)/sexp.cmi
$(SEXPLIB)/sexp.mli: $(SEXPLIB)/sexp_intf.cmi
$(SEXPLIB)/pre_sexp.mli: $(SEXPLIB)/lexer.cmi
$(SEXPLIB)/conv_error.mli: $(SEXPLIB)/conv.cmi

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),depend-build)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif
endif

include ../../Makefile.common
