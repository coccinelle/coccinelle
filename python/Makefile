# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr


#############################################################################
# Configuration section
#############################################################################
ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

##############################################################################
# Variables
##############################################################################
TARGET=coccipython

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

SOURCES= pycocci_aux.ml $(PYCOCCI_FILE) pycocci.ml

INCLUDEDIRS_LOCAL = ../commons ../commons/ocamlextra ../globals \
		    ../parsing_c ../parsing_cocci ../ocaml

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

all.opt:
	@$(MAKE) $(OPTLIB) BUILD_OPT=yes

ctl: $(CTLEXEC)


$(LIB): $(OBJS)
	$(OCAMLC_CMD) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(TARGET).cma $(TARGET).a $(TARGET).o $(TARGET).cmxa


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
	rm -f .depend
	rm -f coccipython.cmxa coccipython.a

distclean::
	$(MAKE) -C coccilib $@
	rm -f pycocci.ml
	rm -f *.cm[ioxa] *.o  *.annot *.a

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

.PHONY: depend
.depend depend:
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include .depend
endif
endif

include ../Makefile.common
