# Bundles the original menhirLib package without modifications.
# This Makefile integrates its build process with coccinelle.

# the build process is conventional build of the ml files, with
# the exception that all modules are packed as a submodule of
# a master module "MenhirLib" instead of an archive file.

include ../../Makefile.config

VERSION=20120123
MENHIRLIB=menhir-$(VERSION)/src

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) -I $(MENHIRLIB) -thread -custom
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) -I $(MENHIRLIB) -thread
OCAMLDEP_CMD=$(OCAMLDEP) -I $(MENHIRLIB)

all: menhirLib.cma META
all.opt: menhirLib.cmxa META

clean:
	rm -f menhirLib.cma menhirLib.cmxa META \
		$(MENHIRLIB)/*.cm[ioxa] \
		$(MENHIRLIB)/*.o \
		$(MENHIRLIB)/engineTypes.mli $(MENHIRLIB)/tableFormat.mli \
		menhirLib.cm[ioxa] menhirLib.[oa]

distclean: clean

.PHONEY: all all.opt clean distclean depend

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
	$(OCAMLC_CMD) -i $< > $@

.mli.cmi:
	$(OCAMLC_CMD) -for-pack MenhirLib -c $<

.ml.cmo:
	$(OCAMLC_CMD) -for-pack MenhirLib -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -for-pack MenhirLib -c $<

META: $(MENHIRLIB)/META
	cp $< $@

# the 'depend' target
.depend depend:
	$(OCAMLDEP_CMD) $(MENHIRLIB)/*.mli $(MENHIRLIB)/*.ml > .depend

-include .depend
