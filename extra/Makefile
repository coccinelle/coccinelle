ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

TARGET=extra

SOURCES = classic_patch.ml kbuild.ml maintainers.ml

SYSLIBS = str.cma unix.cma

LIBS=../commons/commons.cma ../parsing_c/parsing_c.cma ../globals/globals.cma
INCLUDES= -I ../commons -I ../globals -I  ../parsing_cocci -I  ../parsing_c

#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

all: $(TARGET).cma

all.opt:
	@$(MAKE) $(TARGET).cmxa BUILD_OPT=yes

$(TARGET).byte: $(OBJS) $(LIBS)
	$(OCAMLC_CMD) -o $(TARGET).byte $(SYSLIBS) $(LIBS) $(OBJS)

$(TARGET).opt: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD) -o $(TARGET).opt   $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa)  $(OPTOBJS)

$(TARGET).cma: $(OBJS)
	$(OCAMLC_CMD) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP_CMD) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).byte $(TARGET).opt rm -f $(TARGET).top


.SUFFIXES: .ml .mli .cmo .cmi .cmx

ifneq ($(strip $(abs_top_srcdir)),)
include $(abs_top_srcdir)/common_rules.make

$(OBJS): %.cmo: %.ml %.cmi
	$(OCAMLC_CMD) -c $<

$(OPTOBJS): %.cmx: %.ml %.cmi
	$(OCAMLOPT_CMD) -c $<
endif

clean::
	rm -f .depend
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#

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
