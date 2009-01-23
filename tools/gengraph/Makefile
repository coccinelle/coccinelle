TARGET = gengraph

SOURCES = graph.ml gengraph.ml
SUBDIRS = commons exists bugs diff org
MENHIR_PATH=../../menhirlib
DEPDIRS = $(MENHIR_PATH)

# Automatic computation
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
INCLUDES=$(SUBDIRS:%=-I %)
LIBS = $(SUBDIRS:%=%.cma)
EXEC = $(TARGET)
DEP = $(SUBDIRS) $(DEPDIRS)

MENHIR=$(MENHIR_PATH)/menhirLib.cmo
MENHIRO=$(MENHIR_PATH)/menhirLib.cmx

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) #$(INCLUDES)

.PHONY: all all.opt test clean depend $(DEP)

all: .config .depend $(EXEC)
all.opt: $(EXEC).opt

test: all
	make -C $@

test-pdf: $(EXEC)
	make -C test $@

test-org: $(EXEC)
	make -C test $@

view: test-pdf
	make -C test $@

#########################
## Dependencies        ##
#########################
exists: $(MENHIR_PATH) commons
bugs: $(MENHIR_PATH) commons
orgs: $(MENHIR_PATH) commons diff
diff: commons
#########################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) -o $@ str.cma $(MENHIR) $^

$(LIBS): $(DEP)
$(OBJS): $(DEP)
$(DEP):
	$(MAKE) -C $@

clean:
	make -C commons $@
	make -C exists $@
	make -C bugs $@
	make -C org $@
	make -C diff $@
	make -C test $@
	rm -f $(TARGET)
	rm -f *.cm[aiox] *.o *.annot
	rm -f *~ .*~

depend:
	make -C commons $@
	make -C exists $@
	make -C bugs $@
	make -C org $@
	make -C diff $@
	$(OCAMLDEP) *.mli *.ml > .depend

.depend: depend

-include .depend

.config:
	@echo "PDFVIEWER=gv" >> .config
	@echo "#PDFVIEWER=xpdf" >> .config
	@echo "#PDFVIEWER=evince" >> .config
	@echo "#PDFVIEWER=kpdf" >> .config
	@echo "#PDFVIEWER=acroread" >> .config
	@echo "A default .config file have been generated."
	@echo "You may want to edit this file."
