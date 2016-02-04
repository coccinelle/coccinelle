ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
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
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDESDEP)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

ifneq ($(FEATURE_OCAMLBUILD),yes)
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
else
all:
	cd .. && $(OCAMLBUILD) extra/extra.cma

all.opt:
	cd .. && $(OCAMLBUILD) extra/extra.cmxa

clean::
	cd .. && $(OCAMLBUILD) -clean
endif


.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

.ml.mldepend:
	$(OCAMLC_CMD) -i $<

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
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif

include ../Makefile.common
