include ../../Makefile.config

PYCAMLFILE=pycaml_0.82.orig.tar.gz
DEBIANFILE=pycaml_0.82-14.debian.tar.gz

PYCAMLDIR=pycaml
PYCAMLMARKER=$(PYCAMLDIR)/.marker
DEBIANDIR=debian
DEBIANMARKER=$(DEBIANDIR)/.marker

SHARE_FLAGS=-shared -fpic
OCAMLC_CMD=$(OCAMLC) -thread -ccopt "${PYTHON_CFLAGS}" -cclib "${PYTHON_LIBS}" -cc "${CC} ${CFLAGS} ${SHARE_FLAGS}" -I $(PYCAMLDIR)
OCAMLOPT_CMD=$(OCAMLOPT) -thread -ccopt "${PYTHON_CFLAGS}" -cclib "${PYTHON_LIBS}" -I $(PYCAMLDIR)
OCAMLMKLIB_CMD=$(OCAMLMKLIB)


all: depend pycaml.cma

$(PYCAMLDIR)/pycaml.mli: $(PYCAMLDIR)/pycaml.ml
	$(OCAMLC_CMD) -i $< > $(PYCAMLDIR)/pycaml.mli

$(PYCAMLDIR)/pycaml.cmo: $(PYCAMLDIR)/pycaml.ml $(PYCAMLDIR)/pycaml.cmi
	$(OCAMLC_CMD) -c $< -o $@

$(PYCAMLDIR)/pycaml.cmi: $(PYCAMLDIR)/pycaml.mli
	$(OCAMLC_CMD) -c $< -o $@

# local copy of the interface
pycaml.cmi: $(PYCAMLDIR)/pycaml.cmi
	cp $< $@

pycaml_ml.o: $(PYCAMLDIR)/pycaml_ml.c
	$(OCAMLC_CMD) $< -o $@

dllpycaml_stubs.so: pycaml_ml.o
	$(OCAMLMKLIB_CMD) -o pycaml_stubs $<

pycaml.cma: dllpycaml_stubs.so pycaml.cmi $(PYCAMLDIR)/pycaml.cmo
	$(OCAMLC_CMD) -a -custom -linkall -o pycaml.cma pycaml.cmo -dllib -lpycaml_stubs

all.opt: pycaml.cmxa

$(PYCAMLDIR)/pycaml.cmx: $(PYCAMLDIR)/pycaml.ml $(PYCAMLDIR)/pycaml.cmi
	$(OCAMLOPT_CMD) -c $<

pycaml.cmxa: dllpycaml_stubs.so $(PYCAMLDIR)/pycaml.cmx
	$(OCAMLOPT_CMD) -a -o pycaml.cmxa pycaml.cmx -cclib -lpycaml_stubs

clean:
	rm -f $(PYCAMLDIR)/pycaml.mli pycaml.cm[aixo] $(PYCAMLDIR)/pycaml.cm[aixo] \
		pycaml.[ao] libpycaml_stubs.a dllpycaml_stubs.o dllpycaml_stubs.so \
		pycaml_ml.o pycaml.cmxa

distclean: clean
	rm -rf $(PYCAMLDIR) $(DEBIANDIR)


# construct the directories
depend: $(PYCAMLMARKER) $(DEBIANMARKER)

$(PYCAMLMARKER): $(PYCAMLFILE) $(DEBIANMARKER)
	$(TAR) xfvz $<
	set -e; for PATCH in $(shell cat debian/patches/series); do $(PATCH) -d "${PYCAMLDIR}" -p1 < "./debian/patches/$${PATCH}"; done
	touch $@

$(PYCAMLFILE):
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

$(DEBIANMARKER): $(DEBIANFILE)
	$(TAR) xfvz $<
	touch $@

$(DEBIANFILE):
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false
