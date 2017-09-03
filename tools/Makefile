# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
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

isoprof: process_isoprofile
	cp process_isoprofile isoprof


.SUFFIXES: .ml .mli .cmo .cmi .cmx

ifneq ($(strip $(abs_top_srcdir)),)
include $(abs_top_srcdir)/common_rules.make
endif

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

distclean: clean
	rm -f $(PROGS) isoprof
	rm -f .depend

.PHONY: depend
.depend depend: beforedepend
	ocamldep $(INCLUDE) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include .depend
endif
endif

include ../Makefile.common
