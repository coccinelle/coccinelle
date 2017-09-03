# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

TARGET=ctl

SRC_without_mli::=ast_ctl.ml flag_ctl.ml
SRC_with_mli::=ctl_engine.ml pretty_print_ctl.ml wrapper_ctl.ml

SYSLIBS=str.cma unix.cma
LIBS=../commons/commons.cma ../globals/globals.cma

INCLUDES=-I ../commons -I ../commons/ocamlextra -I ../globals


#The Caml compilers.
#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

working_order::=flag_ctl ast_ctl pretty_print_ctl ctl_engine wrapper_ctl
OBJS::=$(addsuffix .cmo,$(working_order))
OPTOBJS::=$(addsuffix .cmx,$(working_order))

all: $(LIB)
all.opt:
	@$(MAKE) $(OPTLIB) BUILD_OPT=yes

$(TARGET).top: $(LIB) test_ctl.cmo
	$(OCAMLMKTOP_CMD) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS) test_ctl.cmo

$(LIB):  $(OBJS)
	$(OCAMLC_CMD) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $(OPTLIB) $(OPTOBJS)

clean::
	rm -f $(LIB)  $(OPTLIB) $(LIB:.cma=.a) $(TARGET).top


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

ifneq ($(strip $(abs_top_srcdir)),)
include $(abs_top_srcdir)/common_rules.make
include $(abs_top_srcdir)/rule_pair.make

pretty_print_ctl.cmo: pretty_print_ctl.ml pretty_print_ctl.cmi \
../commons/common.cmi
	$(OCAMLC_CMD) -c $<

pretty_print_ctl.cmx: pretty_print_ctl.ml pretty_print_ctl.cmi \
../commons/common.cmi
	$(OCAMLOPT_CMD) -c $<
endif

# clean rule for other files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*#
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
