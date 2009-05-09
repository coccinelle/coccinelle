##############################################################################
# Variables
##############################################################################

# The main library
-include ../Makefile.config

TARGET=commons

# note: if you add a file (a .mli or .ml), dont forget to redo a 'make depend'
MYSRC=common.ml common_extra.ml \
      interfaces.ml objet.ml \
      ocollection.ml \
      seti.ml \
      oset.ml oassoc.ml osequence.ml ograph.ml \
      ocollection/oseti.ml ocollection/oseth.ml ocollection/osetb.ml ocollection/osetpt.ml \
      ocollection/oassocb.ml ocollection/oassoch.ml ocollection/oassoc_buffer.ml ocollection/oassoc_cache.ml ocollection/oassocid.ml \
      oarray.ml \
      ocollection/ograph2way.ml ograph_simple.ml ograph_extended.ml \
      ofullcommon.ml \
      glimpse.ml parser_combinators.ml

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

#pcre 
#REGEXPINCLUDES=-I +pcre
MYREGEXPSRC=regexp.ml
REGEXPINCLUDES=-I ../ocamlpcre/lib  -I ../../ocamlpcre/lib

#sexplib 
MYSEXPSRC=sexp_common.ml
SEXPINCLUDES=-I ../ocamlsexp -I ../ocamltarzan/lib-sexp -I ../../ocamltarzan/lib-sexp
#binprot
MYBINSRC=bin_common.ml
BININCLUDES=-I ../ocamltarzan/lib-binprot -I ../../ocamltarzan/lib-binprot

#-----------------------------------------------------------------------------
# Other stuff
#-----------------------------------------------------------------------------

#backtrace
MYBACKTRACESRC=backtrace.ml
BACKTRACEINCLUDES=-I $(shell ocamlc -where)


##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# This flag can also be used in subdirectories so don't change its name here.
# For profiling use:  -p -inline 0
OPTFLAGS=
#-thread

# The OPTBIN variable is here to allow to use ocamlc.opt instead of 
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var 
# or setenv it to ".opt" in your startup script.
OPTBIN ?= #.opt

OCAMLCFLAGS ?= -g -dtypes

# The OCaml tools.
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX = ocamllex$(OPTBIN)
OCAMLYACC= ocamlyacc -v
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

# if need C code
OCAMLMKLIB=ocamlmklib
CC=gcc

##############################################################################
# Top rules
##############################################################################
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


all: $(LIB)
all.opt: $(OPTLIB)
opt: all.opt
top: $(TARGET).top

$(LIB): $(OBJS)
	$(OCAMLC) -a -o $@ $^

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT) -a -o $@ $^

$(TARGET).top: $(OBJS)
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET).top

##############################################################################
# Other commons libs target
##############################################################################

all_libs: gdbm bdb gui mpi regexp backtrace

#-----------------------------------------------------------------------------
gdbm: commons_gdbm.cma
gdbm.opt: commons_gdbm.cmxa

commons_gdbm.cma: $(MYGDBMSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_gdbm.cmxa: $(MYGDBMSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


#-----------------------------------------------------------------------------
bdb:
	$(MAKE) INCLUDESEXTRA="$(BDBINCLUDES)" commons_bdb.cma
bdb.opt:
	$(MAKE) INCLUDESEXTRA="$(BDBINCLUDES)" commons_bdb.cmxa

commons_bdb.cma: $(MYBDBSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_bdb.cmxa: $(MYBDBSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^



#-----------------------------------------------------------------------------
gui:
	$(MAKE) INCLUDESEXTRA="$(GUIINCLUDES)" commons_gui.cma
gui.opt:
	$(MAKE) INCLUDESEXTRA="$(GUIINCLUDES)" commons_gui.cmxa

commons_gui.cma: $(MYGUISRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_gui.cmxa: $(MYGUISRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^



#-----------------------------------------------------------------------------
mpi:
	$(MAKE) INCLUDESEXTRA="$(MPIINCLUDES)" commons_mpi.cma
mpi.opt:
	$(MAKE) INCLUDESEXTRA="$(MPIINCLUDES)" commons_mpi.cmxa

commons_mpi.cma: $(MYMPISRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_mpi.cmxa: $(MYMPISRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

#alias
distribution: mpi
distribution.opt: mpi.opt



#-----------------------------------------------------------------------------
python:
	$(MAKE) INCLUDESEXTRA="$(PYINCLUDES)" commons_python.cma
python.opt:
	$(MAKE) INCLUDESEXTRA="$(PYINCLUDES)" commons_python.cmxa


commons_python.cma: $(MYPYSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_python.cmxa: $(MYPYSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


#-----------------------------------------------------------------------------
regexp:
	$(MAKE) INCLUDESEXTRA="$(REGEXPINCLUDES)" commons_regexp.cma
regexp.opt:
	$(MAKE) INCLUDESEXTRA="$(REGEXPINCLUDES)" commons_regexp.cmxa

commons_regexp.cma: $(MYREGEXPSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_regexp.cmxa: $(MYREGEXPSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


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
sexp:
	$(MAKE) INCLUDESEXTRA="$(SEXPINCLUDES)" commons_sexp.cma
sexp.opt:
	$(MAKE) INCLUDESEXTRA="$(SEXPINCLUDES)" commons_sexp.cmxa

commons_sexp.cma: $(MYSEXPSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_sexp.cmxa: $(MYSEXPSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

binprot:
	$(MAKE) INCLUDESEXTRA="$(BININCLUDES)" commons_bin.cma
binprot.opt:
	$(MAKE) INCLUDESEXTRA="$(BININCLUDES)" commons_bin.cmxa

commons_bin.cma: $(MYBINSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_bin.cmxa: $(MYBINSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

##############################################################################
# The global "features" lib wrapper
##############################################################################

features: commons_features.cma
features.opt: commons_features.cmxa

commons_features.cma: features.cmo
	$(OCAMLC) -a -o $@ $^

commons_features.cmxa: features.cmx
	$(OCAMLOPT) -a -o $@ $^


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
	$(OCAMLC)  -c $<
.mli.cmi:
	$(OCAMLC)  -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

clean::
	rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot
	rm -f *~ .*~ #*#

clean::
	for i in $(SUBDIRS); do (cd $$i; \
        rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot *~ .*~ ; \
        cd ..; ) \
	done

depend:
	$(OCAMLDEP) *.mli *.ml  > .depend
	for i in $(SUBDIRS); do $(OCAMLDEP) $$i/*.ml $$i/*.mli >> .depend; done

distclean::
	rm -f .depend

-include .depend
