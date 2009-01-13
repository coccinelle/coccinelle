TARGET = commons

SOURCES = misc.ml
INTFS = ast.mli
INCLUDES =

# Automatic computation
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)

all: $(LIB)
all.opt: $(OPTLIB)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: depend clean

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(LIB): $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(TARGET)
	rm -f *.cm[aiox] *.o *.annot
	rm -f *~ .*~

depend: .depend

.depend: $(INTFS) $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
