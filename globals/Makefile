
-include ../Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=globals

SRC= flag.ml config.ml iteration.ml regexp.ml

LIBS=
INCLUDEDIRS= ../commons  $(PCREDIR)

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

OBJS= $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma

all.opt: $(TARGET).cmxa

$(TARGET).cma: $(LIBS) $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

##############################################################################
# Developer rules
##############################################################################
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#

distclean::
	rm -f .depend

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend




