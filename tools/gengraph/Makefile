#PDFVIEWER=gv
#PDFVIEWER=xpdf
#PDFVIEWER=evince
PDFVIEWER=kpdf
#PDFVIEWER=acroread
TARGET = gengraph

SOURCES = graph.ml gengraph.ml
SUBDIRS = commons exists bugs

# Automatic computation
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
INCLUDES=$(SUBDIRS:%=-I %)
LIBS = $(SUBDIRS:%=%.cma)
EXEC = $(TARGET)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)

.PHONY: all all.opt test clean depend subdirs $(SUBDIRS)

all: .depend $(EXEC)
all.opt: $(EXEC).opt

test: all
	make -C $@

view: test
	$(PDFVIEWER) test/test.pdf

exists: commons
bugs: commons

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) -o $@ $^

$(LIBS): $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	make -C commons $@
	make -C exists $@
	make -C bugs $@
	make -C test $@
	rm -f $(TARGET)
	rm -f *.cm[aiox] *.o *.annot
	rm -f *~ .*~

depend:
	make -C commons $@
	make -C exists $@
	make -C bugs $@
	$(OCAMLDEP) *.mli *.ml > .depend

.depend: depend

-include .depend
