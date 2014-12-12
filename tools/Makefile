# Copyright 2012-2014, INRIA
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


ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

SYSLIBS = str.cma unix.cma bigarray.cma

INCLUDE=-I ../commons -I ../extra -I  ../parsing_c
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../parsing_c/parsing_c.cma ../extra/extra.cma

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDE)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDE)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDE)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDE)
OCAMLYACC_CMD=$(OCAMLYACC) -v


PROGS=spp gitgrep splitpatch extract_c_and_res \
	gitsort alloc_free bridge \
	process_isoprofile
	#generate_dependencies

ifneq ($(FEATURE_OCAMLBUILD),yes)
all: $(PROGS)

all.opt:
	@$(MAKE) $(PROGS) BUILD_OPT=yes

gitgrep: gitgrep.ml
	$(OCAMLOPT) -o gitgrep str.cmxa gitgrep.ml

gitsort: gitsort.ml
	$(OCAMLOPT) -o gitsort str.cmxa gitsort.ml

process_isoprofile: process_isoprofile.ml
	$(OCAMLC) -g -o process_isoprofile str.cma process_isoprofile.ml

alloc_free: alloc_free.ml
	$(OCAMLOPT) -o alloc_free str.cmxa alloc_free.ml

# more flexible version of alloc_free
bridge: bridge.ml
	$(OCAMLOPT) -o bridge str.cmxa dumper.mli dumper.ml bridge.ml

install_bridge: bridge
	cp -f bridge /usr/local/bin

spp: spp.ml
	$(OCAMLC_CMD) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS) $+

splitpatch: splitpatch.cmo
	$(OCAMLC_CMD) -o $@ $(SYSLIBS) $(INCLUDE) $+

extract_c_and_res: extract_c_and_res.cmo
	$(OCAMLC_CMD) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+

generate_dependencies: generate_dependencies.cmo
	$(OCAMLC_CMD) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+
else
.PHONY: $(PROGS) $(PROGS:%.opt) isoprof

all: $(PROGS)
all.opt: $(PROGS:%=%.opt)

$(PROGS):
	cd .. && $(OCAMLBUILD) tools/$@.byte
	cp ../_build/tools/$@.byte $@

$(PROGS:%=%.opt):
	cd .. && $(OCAMLBUILD) tools/$(@:%.opt=%).native
	cp ../_build/tools/$(@:%.opt=%).native $(@:%.opt=%)

clean::
	cd .. && $(OCAMLBUILD) -clean
	rm -f $(PROGS)
endif

isoprof: process_isoprofile
	cp process_isoprofile isoprof


.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

clean::
	rm -f $(PROGS) isoprof
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f .depend

distclean: clean

.PHONY: depend
.depend depend: beforedepend
	ocamldep $(INCLUDE) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include .depend
endif
endif

include ../Makefile.common
