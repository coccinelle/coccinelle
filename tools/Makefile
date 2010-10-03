# Copyright 2005-2010, Ecole des Mines de Nantes, University of Copenhagen
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


-include ../Makefile.config

PROGS=spp gitgrep split_patch extract_c_and_res #generate_dependencies

all: $(SUBDIRS) $(PROGS)

# for recursive make
all.opt: $(PROGS)

gitgrep: gitgrep.ml
	ocamlopt -o gitgrep str.cmxa gitgrep.ml

gitsort: gitsort.ml
	ocamlopt -o gitsort str.cmxa gitsort.ml

isoprof: process_isoprofile.ml
	ocamlc -g -o isoprof str.cma process_isoprofile.ml

alloc_free: alloc_free.ml
	ocamlopt -o alloc_free str.cmxa alloc_free.ml

# more flexible version of alloc_free
bridge: bridge.ml
	ocamlopt -o bridge str.cmxa bridge.ml

install_bridge: bridge
	cp -f bridge /usr/local/bin

.PHONY: $(SUBDIRS)


SYSLIBS = str.cma unix.cma bigarray.cma

INCLUDE=-I ../commons -I ../extra -I  ../parsing_c
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../parsing_c/parsing_c.cma ../extra/extra.cma

OCAMLC=ocamlc$(OPTBIN) -g -dtypes   $(INCLUDE)
OCAMLOPT=ocamlopt$(OPTBIN)   $(INCLUDE) $(OPTFLAGS)
#OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLLEX=ocamllex$(OPTBIN)
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN) $(INCLUDE)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDE)

spp: spp.ml
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+
#	ocamlc -I ../commons/ -o spp str.cma unix.cma bigarray.cma  ../commons/common.cmo spp.ml

split_patch: split_patch.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+

extract_c_and_res: extract_c_and_res.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+

generate_dependencies: generate_dependencies.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

clean:
	rm -f $(PROGS)
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

beforedepend::

depend:: beforedepend
	ocamldep $(INCLUDE) *.mli *.ml    > .depend

distclean::
	rm -f .depend

-include .depend
