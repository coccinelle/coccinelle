# Copyright 2012-2015, Inria
# Julia Lawall, Gilles Muller
# Copyright 2010-2011, INRIA, University of Copenhagen
# Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
# Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
# Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
# This file is part of Coccinelle.
#
# Coccinelle is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Coccinelle is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Coccinelle under other licenses.



#############################################################################
# Configuration section
#############################################################################
ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

##############################################################################
# Variables
##############################################################################
TARGET=coccipython

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

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

ifneq ($(FEATURE_OCAMLBUILD),yes)
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
else
all:
	cd .. && $(OCAMLBUILD) python/python.cma
all.opt:
	cd .. && $(OCAMLBUILD) python/python.cmxa
clean::
	cd .. && $(OCAMLBUILD) -clean
endif

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
	rm -f *.cm[ioxa] *.o  *.annot

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
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif

include ../Makefile.common
