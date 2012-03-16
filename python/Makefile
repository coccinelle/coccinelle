#############################################################################
# Configuration section
#############################################################################
-include ../Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=coccipython

SOURCES= pycocci_aux.ml $(PYCOCCI_FILE) pycocci.ml

INCLUDEDIRS_LOCAL = ../commons ../commons/ocamlextra ../globals \
		    ../parsing_c ../parsing_cocci

INCLUDEDIRS = $(INCLUDEDIRS_LOCAL) $(PYCAMLDIR)

SYSLIBS = str.cma unix.cma pycaml.cma
LIBS=../commons/commons.cma ../globals/globals.cma

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)
INCLUDES_DEPEND=$(INCLUDEDIRS_LOCAL:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES_DEPEND)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

##############################################################################
# Top rules
##############################################################################

EXEC=$(TARGET).byte
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

CTLEXEC=$(CTLTARGET)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

CTLOBJS = $(CTLSOURCES:.ml=.cmo)
CTLOPTOBJS = $(CTLOBJS:.cmo=.cmx)

all: $(LIB)

all.opt: $(OPTLIB)

ctl: $(CTLEXEC)


$(LIB): $(OBJS)
	$(OCAMLC_CMD) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(LIB)


$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $(OPTLIB) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(OCAMLC_CMD) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo

$(CTLEXEC): $(CTLOBJS) $(LIBS)
	$(OCAMLC_CMD) -o $(CTLEXEC) $(SYSLIBS) $(LIBS) $(CTLOBJS)


clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a)
	rm -f $(TARGET)
	rm -f $(TARGET).byte
	rm -f $(CTLTARGET)

distclean::
	$(MAKE) -C coccilib $@
	rm -f .depend
	rm -f pycocci.ml

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<

.mli.cmi:
	$(OCAMLC_CMD) -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -c $<


# clean rule for others files
clean::
	rm -f *.cm[iox] *.o  *.annot
	rm -f *~ .*~ #*#

beforedepend:

depend: beforedepend
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

.depend:
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

-include .depend
