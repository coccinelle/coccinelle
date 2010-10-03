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


##############################################################################
# Variables
##############################################################################
#TARGET=matcher
TARGET=cocciengine
CTLTARGET=engine

SRC= flag_matcher.ml lib_engine.ml pretty_print_engine.ml \
      check_exhaustive_pattern.ml \
      check_reachability.ml \
      c_vs_c.ml isomorphisms_c_c.ml \
      cocci_vs_c.ml pattern_c.ml sgrep.ml transformation_c.ml  \
      asttomember.ml asttoctl2.ml ctltotex.ml \
      postprocess_transinfo.ml ctlcocci_integration.ml

#c_vs_c.ml
#SRC= flag_matcher.ml \
#  c_vs_c.ml cocci_vs_c.ml \
#  lib_engine.ml \
#  pattern_c.ml transformation_c.ml 

#LIBS=../commons/commons.cma ../parsing_c/parsing_c.cma
#INCLUDES= -I ../commons -I ../parsing_c
INCLUDES = -I ../commons -I ../commons/ocamlextra -I ../globals \
	       -I ../ctl -I ../parsing_cocci -I ../parsing_c 
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../ctl/ctl.cma ../parsing_c/parsing_c.cma ../parsing_cocci/cocci_parser.cma

SYSLIBS= str.cma unix.cma 


# just to test asttoctl
# CTLSOURCES = lib_engine.ml pretty_print_engine.ml asttoctl.ml ctltotex.ml \
#	main.ml

##############################################################################
# Generic variables
##############################################################################

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g -dtypes

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex$(OPTBIN) #-ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN) $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)


OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma
all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top



##############################################################################
# Pad's rules
##############################################################################

##############################################################################
# Generic rules
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
clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend
