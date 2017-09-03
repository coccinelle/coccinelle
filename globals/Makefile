# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

##############################################################################
# Variables
##############################################################################
TARGET=globals

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

SRC_without_mli::=config.ml flag.ml $(REGEXP_FILE) regexp.ml
SRC_with_mli::=iteration.ml

LIBS=
INCLUDEDIRS= ../commons $(PCREDIR)

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS::=$(SRC_without_mli:.ml=.cmo) $(SRC_with_mli:.ml=.cmo)
OPTOBJS::=$(SRC_without_mli:.ml=.cmx) $(SRC_with_mli:.ml=.cmx)

##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma

all.opt:
	@$(MAKE) $(TARGET).cmxa BUILD_OPT=yes

$(TARGET).cma: $(LIBS) $(OBJS)
	$(OCAMLC_CMD) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $(TARGET).cmxa $(OPTOBJS)

##############################################################################
# Developer rules
##############################################################################
.SUFFIXES: .ml .mli .cmo .cmi .cmx

ifneq ($(strip $(abs_top_srcdir)),)
include $(abs_top_srcdir)/common_rules.make
include $(abs_top_srcdir)/rule_pair.make

iteration.cmo: iteration.ml iteration.cmi
	$(OCAMLC_CMD) -c $<

iteration.cmx: iteration.ml iteration.cmi
	$(OCAMLOPT_CMD) -c $<
endif

clean:
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend

distclean: clean

.PHONY: depend
.depend depend:
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include .depend
endif
endif

include ../Makefile.common
