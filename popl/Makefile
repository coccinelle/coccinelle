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


#note: if you add a file (a .mli or .ml), dont forget to do a   make depend

TARGET = popl

SRC = ast_popl.ml asttopopl.ml insert_quantifiers.ml insert_befaft.ml \
pretty_print_popl.ml popltoctl.ml popl.ml flag_popl.ml

SYSLIBS=str.cma unix.cma
LIBS=../commons/commons.cma ../globals/globals.cma

INCLUDE_PATH = -I ../commons -I ../globals \
	       -I ../ctl -I ../parsing_c -I ../parsing_cocci -I ../engine

#The Caml compilers.
#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
CAMLC =ocamlc$(OPTBIN) -dtypes -g 
CAMLOPT=ocamlopt$(OPTBIN)   $(OPTFLAGS)
CAMLLEX = ocamllex$(OPTBIN)
CAMLYACC= ocamlyacc
CAMLDEP = ocamldep$(OPTBIN) 
CAMLMKTOP=ocamlmktop -g -custom



LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(LIB)
all.opt: $(OPTLIB)

$(TARGET).top: $(LIB)
	$(CAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

$(LIB):  $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(LIB) $(TARGET).top


$(OPTLIB): $(OPTOBJS)
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

# clean rule for LIB.opt
clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(INCLUDE_PATH) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDE_PATH) -c $<

.ml.cmx:
	$(CAMLOPT) $(INCLUDE_PATH) -c $<




# clean rule for others files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*# 

depend: 
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

#clean::
#	rm -f .depend

.depend:
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

-include .depend
