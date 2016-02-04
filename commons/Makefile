##############################################################################
# Variables
##############################################################################

# The main library
ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

TARGET=commons

# note: if you add a file (a .mli or .ml), dont forget to redo a 'make depend'
MYSRC=	commands.ml                              \
	common.ml common_extra.ml                \
	interfaces.ml objet.ml                   \
	ocollection.ml                           \
	seti.ml                                  \
	oset.ml oassoc.ml osequence.ml ograph.ml \
	ocollection/oseti.ml                     \
	ocollection/oseth.ml                     \
	ocollection/osetb.ml                     \
	ocollection/osetpt.ml                    \
	ocollection/oassocb.ml			 \
	ocollection/oassoch.ml			 \
	ocollection/oassoc_buffer.ml		 \
	ocollection/oassoc_cache.ml		 \
	ocollection/oassocid.ml			 \
	oarray.ml				 \
	ocollection/ograph2way.ml		 \
	ograph_simple.ml ograph_extended.ml	 \
	glimpse.ml parser_combinators.ml

MLI_FILES=\
  common.mli \
  oarray.mli \
  oassoc.mli \
  objet.mli \
  ocollection.mli \
  ograph_extended.mli \
  ograph.mli \
  ograph_simple.mli \
  osequence.mli \
  oset.mli \
  parser_combinators.mli

ifeq ($(COMPILE_EMBEDDED_BYTES_MODULE),yes)
	MYSRC+=bytes.ml
	MLI_FILES+=bytes.mli
endif

# src from other authors, got from the web or caml hump
SRC=ocamlextra/dumper.ml
SRC+=ocamlextra/ANSITerminal.ml
SRC+=ocamlextra/setb.ml ocamlextra/mapb.ml # defunctorized version of standard set/map
SRC+=ocamlextra/setPt.ml
SRC+=$(MYSRC)
SRC+=ocamlextra/enum.ml ocamlextra/dynArray.ml
SRC+=ocamlextra/suffix_tree.ml ocamlextra/suffix_tree_ext.ml

SYSLIBS=str.cma bigarray.cma unix.cma

INCLUDEDIRS=ocamlextra ocollection
SUBDIRS=ocamlextra ocollection

#-----------------------------------------------------------------------------
# Other common (thin wrapper) libraries
#-----------------------------------------------------------------------------

#format: XXXSRC, XXXINCLUDE, XXXSYSLIBS

#gdbm
MYGDBMSRC=ocollection/oassocdbm.ml
GDBMSYSLIBS=dbm.cma

#berkeley db (ocamlbdb)
MYBDBSRC=ocollection/oassocbdb.ml ocollection/oassocbdb_string.ml
BDBINCLUDES=-I ../ocamlbdb
BDBSYSLIBS=bdb.cma


#lablgtk (ocamlgtk)
MYGUISRC=gui.ml
GUIINCLUDES=-I +lablgtk2 -I +lablgtksourceview -I ../ocamlgtk/src
GUISYSLIBS=lablgtk.cma lablgtksourceview.cma

#pycaml (ocamlpython)
MYPYSRC=python.ml
PYINCLUDES=-I ../ocamlpython -I ../../ocamlpython
PYSYSLIBS=python.cma

#ocamlmpi
MYMPISRC=distribution.ml
MPIINCLUDES=-I ../ocamlmpi -I ../../ocamlmpi -I +ocamlmpi
MPISYSLIBS=mpi.cma

#binprot
MYBINSRC=bin_common.ml
BININCLUDES=-I ../ocamltarzan/lib-binprot -I ../../ocamltarzan/lib-binprot

#-----------------------------------------------------------------------------
# Other stuff
#-----------------------------------------------------------------------------

#backtrace
MYBACKTRACESRC=backtrace.ml
BACKTRACEINCLUDES=-I $(shell $(OCAMLC) -where)


##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# This flag can also be used in subdirectories so don't change its name here.
# For profiling use:  -p -inline 0
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

# The OPTBIN variable is here to allow to use ocamlc.opt instead of
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var
# or setenv it to ".opt" in your startup script.
OPTBIN ?= #.opt

# The OCaml tools.
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

##############################################################################
# Top rules
##############################################################################
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


ifneq ($(FEATURE_OCAMLBUILD),yes)
all: $(LIB)
all.opt:
	@$(MAKE) $(OPTLIB) BUILD_OPT=yes
opt: all.opt
top: $(TARGET).top

$(LIB): $(OBJS)
	$(OCAMLC_CMD) -a -o $@ $^

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT_CMD) -a -o $@ $^

$(TARGET).top: $(OBJS)
	$(OCAMLMKTOP_CMD) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET).top
else
all:
	cd .. && $(OCAMLBUILD) commons/commons.cma
all.opt:
	cd .. && $(OCAMLBUILD) commons/commons.cmxa
clean::
	cd .. && $(OCAMLBUILD) -clean
endif

##############################################################################
# Other commons libs target
##############################################################################

all_libs: gdbm bdb gui mpi backtrace

#-----------------------------------------------------------------------------
gdbm: commons_gdbm.cma
gdbm.opt: commons_gdbm.cmxa

commons_gdbm.cma: $(MYGDBMSRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o $@ $^

commons_gdbm.cmxa: $(MYGDBMSRC:.ml=.cmx)
	$(OCAMLOPT_CMD) -a -o $@ $^


#-----------------------------------------------------------------------------
bdb:
	$(MAKE) INCLUDESEXTRA="$(BDBINCLUDES)" commons_bdb.cma
bdb.opt:
	$(MAKE) INCLUDESEXTRA="$(BDBINCLUDES)" commons_bdb.cmxa

commons_bdb.cma: $(MYBDBSRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o $@ $^

commons_bdb.cmxa: $(MYBDBSRC:.ml=.cmx)
	$(OCAMLOPT_CMD) -a -o $@ $^



#-----------------------------------------------------------------------------
gui:
	$(MAKE) INCLUDESEXTRA="$(GUIINCLUDES)" commons_gui.cma
gui.opt:
	$(MAKE) INCLUDESEXTRA="$(GUIINCLUDES)" commons_gui.cmxa

commons_gui.cma: $(MYGUISRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o $@ $^

commons_gui.cmxa: $(MYGUISRC:.ml=.cmx)
	$(OCAMLOPT_CMD) -a -o $@ $^



#-----------------------------------------------------------------------------
mpi:
	$(MAKE) INCLUDESEXTRA="$(MPIINCLUDES)" commons_mpi.cma
mpi.opt:
	$(MAKE) INCLUDESEXTRA="$(MPIINCLUDES)" commons_mpi.cmxa

commons_mpi.cma: $(MYMPISRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o $@ $^

commons_mpi.cmxa: $(MYMPISRC:.ml=.cmx)
	$(OCAMLOPT_CMD) -a -o $@ $^

#alias
distribution: mpi
distribution.opt: mpi.opt



#-----------------------------------------------------------------------------
python:
	$(MAKE) INCLUDESEXTRA="$(PYINCLUDES)" commons_python.cma
python.opt:
	$(MAKE) INCLUDESEXTRA="$(PYINCLUDES)" commons_python.cmxa


commons_python.cma: $(MYPYSRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o $@ $^

commons_python.cmxa: $(MYPYSRC:.ml=.cmx)
	$(OCAMLOPT_CMD) -a -o $@ $^

#-----------------------------------------------------------------------------
backtrace: commons_backtrace.cma
backtrace.opt: commons_backtrace.cmxa

backtrace_c.o: backtrace_c.c
	$(CC) $(BACKTRACEINCLUDES) -c $^

commons_backtrace.cma: $(MYBACKTRACESRC:.ml=.cmo) backtrace_c.o
	$(OCAMLMKLIB) -o commons_backtrace $^

commons_backtrace.cmxa: $(MYBACKTRACESRC:.ml=.cmx) backtrace_c.o
	$(OCAMLMKLIB) -o commons_backtrace  $^

clean::
	rm -f dllcommons_backtrace.so



#-----------------------------------------------------------------------------
binprot:
	$(MAKE) INCLUDESEXTRA="$(BININCLUDES)" commons_bin.cma
binprot.opt:
	$(MAKE) INCLUDESEXTRA="$(BININCLUDES)" commons_bin.cmxa

commons_bin.cma: $(MYBINSRC:.ml=.cmo)
	$(OCAMLC_CMD) -a -o $@ $^

commons_bin.cmxa: $(MYBINSRC:.ml=.cmx)
	$(OCAMLOPT_CMD) -a -o $@ $^

##############################################################################
# The global "features" lib wrapper
##############################################################################

features: commons_features.cma
features.opt: commons_features.cmxa

commons_features.cma: features.cmo
	$(OCAMLC_CMD) -a -o $@ $^

commons_features.cmxa: features.cmx
	$(OCAMLOPT_CMD) -a -o $@ $^


##############################################################################
# Developer rules
##############################################################################

tags:
	otags -no-mli-tags -r  .

clean::
	rm -f gmon.out

forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt

dependencygraph:
	ocamldep *.mli *.ml > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps

dependencygraph2:
	find  -name "*.ml" |grep -v "scripts" | xargs ocamldep -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl -I extra > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps


##############################################################################
# Generic rules
##############################################################################
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD)  -c $<
.mli.cmi:
	$(OCAMLC_CMD)  -c $<
.ml.cmx:
	$(OCAMLOPT_CMD)  -c $<

clean::
	rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot
	rm -f *~ .*~ #*#

clean::
	for i in $(SUBDIRS); do (cd $$i; \
        rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot *~ .*~ ; \
        cd ..; ) \
	done
	rm -f .depend

distclean: clean
	rm -f commands.ml

.PHONY: depend
.depend depend:
ifeq ($(COMPILE_EMBEDDED_BYTES_MODULE),yes)
	$(OCAMLDEP_CMD) $(MLI_FILES) $(MYSRC)  > .depend
else
	$(OCAMLDEP_CMD) $(MLI_FILES) $(MYSRC) | sed 's/bytes\.cm[a-z]\{1,\}//g' > .depend
endif
	for i in $(SUBDIRS); do $(OCAMLDEP_CMD) $$i/*.ml $$i/*.mli >> .depend; done

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif

include ../Makefile.common
