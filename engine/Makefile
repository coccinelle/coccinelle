# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

##############################################################################
# Variables
##############################################################################
#TARGET=matcher
ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

TARGET::=engine
TARGET_ALIAS::=cocciengine

SRC= flag_matcher.ml lib_engine.ml pretty_print_engine.ml \
      check_exhaustive_pattern.ml \
      check_reachability.ml \
      c_vs_c.ml isomorphisms_c_c.ml \
      cocci_vs_c.ml pattern_c.ml transformation_c.ml  \
      asttomember.ml asttoctl2.ml ctltotex.ml \
      postprocess_transinfo.ml ctlcocci_integration.ml

INCLUDES = -I ../commons -I ../commons/ocamlextra -I ../globals \
		-I ../ctl -I ../parsing_cocci -I ../parsing_c -I ../ocaml \
		-I ../python
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../ctl/ctl.cma ../parsing_c/parsing_c.cma \
     ../parsing_cocci/cocci_parser.cma \
     ../ocaml/cocciocaml.cma

SYSLIBS= str.cma unix.cma


# just to test asttoctl
# CTLSOURCES = lib_engine.ml pretty_print_engine.ml asttoctl.ml ctltotex.ml \
#	main.ml

##############################################################################
# Generic variables
##############################################################################

#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET_ALIAS).cma
all.opt:
	@$(MAKE) $(TARGET_ALIAS).cmxa BUILD_OPT=yes

$(TARGET_ALIAS).cma: $(OBJS)
	$(OCAMLC_CMD) -a -o $(TARGET_ALIAS).cma $(OBJS)

$(TARGET_ALIAS).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD) -a -o $(TARGET_ALIAS).cmxa $(OPTOBJS)

$(TARGET_ALIAS).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP_CMD) -o $(TARGET_ALIAS).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET_ALIAS).top

##############################################################################
# Pad's rules
##############################################################################

##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

ifneq ($(strip $(abs_top_srcdir)),)
include $(abs_top_srcdir)/modules_with_mli.make
endif

clean::
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
