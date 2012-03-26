# This Makefile serves as a wrapper to bundle the
# sexplib package without modifications.

# This is a standard build with:
#  * all files marked with -for-pack Sexplib
#  * required packages unix num bigarray
#  * parser.mly requires ocamlyacc
#  * lexer.mll requires ocamllex
#  * pre_sexp.ml: use cpp -traditional -undef
#      (but all other files can be processed with this one as well)

include ../../Makefile.config

VERSION=7.0.5
SEXPDIR=sexplib-$(VERSION)
SEXPLIB=$(SEXPDIR)/lib
SEXPMARKER=$(SEXPDIR)/.marker

PP_CMD=$(CPP) -x c -w -traditional -undef
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) -I $(SEXPLIB) -thread -custom -pp "${PP_CMD}"
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) -I $(SEXPLIB) -thread -pp "${PP_CMD}"
OCAMLDEP_CMD=$(OCAMLDEP) -I $(SEXPLIB) -pp "${PP_CMD}"

all: sexplib.cma META
all.opt: sexplib.cmxa META

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

$(SEXPMARKER): $(SEXPDIR).tar.gz sexp-3.10-compat.patch
	$(TAR) xfvz $<
	$(PATCH) -d $(SEXPDIR) -p1 < ./sexp-3.10-compat.patch
	touch $@

$(SEXPDIR).tar.gz:
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

.PHONEY: all all.opt clean distclean depend


# Sources needed for coccinelle
MODULES=type parser lexer pre_sexp sexp_intf \
		sexp path conv conv_error

MLI=$(MODULES:%=$(SEXPLIB)/%.mli)
SRC=$(MODULES:%=$(SEXPLIB)/%.ml)

# Link
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
	$(OCAMLC_CMD) -i $< > $@

.mli.cmi:
	$(OCAMLC_CMD) -for-pack Sexplib -c $<

.ml.cmo:
	$(OCAMLC_CMD) -for-pack Sexplib -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -for-pack Sexplib -c $<

META: $(SEXPLIB)/META
	cp $< $@

# the 'depend' target
.depend depend: $(SEXPMARKER) $(SRC)
	$(OCAMLDEP_CMD) $(SEXPLIB)/*.mli $(SEXPLIB)/*.ml > .depend

-include .depend
