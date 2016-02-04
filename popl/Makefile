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


#note: if you add a file (a .mli or .ml), dont forget to do a   make depend

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

TARGET = popl

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

SRC = ast_popl.ml asttopopl.ml insert_quantifiers.ml insert_befaft.ml \
pretty_print_popl.ml popltoctl.ml popl.ml flag_popl.ml

SYSLIBS=str.cma unix.cma
LIBS=../commons/commons.cma ../globals/globals.cma

INCLUDE_PATH = -I ../commons -I ../globals \
	       -I ../ctl -I ../parsing_c -I ../parsing_cocci -I ../engine

#The Caml compilers.
#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS)
OCAMLDEP_CMD=$(OCAMLDEP)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom

LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(LIB)
all.opt:
	@$(MAKE) $(OPTLIB) BUILD_OPT=yes

$(TARGET).top: $(LIB)
	$(OCAMLMKTOP_CMD) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

$(LIB):  $(OBJS)
	$(OCAMLC_CMD) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(LIB) $(TARGET).top


$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $(OPTLIB) $(OPTOBJS)

# clean rule for LIB.opt
clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) $(INCLUDE_PATH) -c $<

.mli.cmi:
	$(OCAMLC_CMD) $(INCLUDE_PATH) -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) $(INCLUDE_PATH) -c $<


# clean rule for others files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*#
	rm -f .depend

distclean: clean

.PHONY: depend
.depend depend:
	$(OCAMLDEP_CMD) $(INCLUDE_PATH) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif

include ../Makefile.common
