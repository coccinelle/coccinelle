# Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
# Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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
-include ../Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=coccipython

SOURCES= pycocci_aux.ml pycocci.ml 

INCLUDEDIRS = ../commons ../commons/ocamlextra ../globals ../pycaml \
	      ../parsing_c ../parsing_cocci

SYSLIBS = str.cma unix.cma
LIBS=../commons/commons.cma ../globals/globals.cma 

#     ../ctl/ctl.cma \
#     ../parsing_c/c_parser.cma ../parsing_cocci/cocci_parser.cma 
#pycaml/pycaml.cma 


##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)


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


#all: $(EXEC) $(LIB)
all: $(LIB)

all.opt: $(OPTLIB)

ctl: $(CTLEXEC)


$(LIB): $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(LIB)


$(OPTLIB): $(OPTOBJS) 
	$(OCAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo

$(CTLEXEC): $(CTLOBJS) $(LIBS)
	$(OCAMLC) -o $(CTLEXEC) $(SYSLIBS) $(LIBS) $(CTLOBJS)


clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	
	rm -f $(TARGET) rm -f $(TARGET).byte
	rm -f $(CTLTARGET)


#pycocci.ml: ../pycaml/pycaml.ml ../pycaml/pycaml_ml.c
#pycocci_aux.ml:  ../pycaml/pycaml.ml ../pycaml/pycaml_ml.c

rmlinks:
	rm -f pycocci.ml pycocci_aux.ml

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<


# clean rule for others files
clean::
	rm -f *.cm[iox] *.o  *.annot
	rm -f *~ .*~ #*# 

beforedepend:

depend: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

.depend: 
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
