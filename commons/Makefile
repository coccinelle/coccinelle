##############################################################################
# Variables
##############################################################################
TARGET=commons

# note: if you add a file (a .mli or .ml), dont forget to redo a 'make depend'
MYSRC=commonop.ml common.ml \
      seti.ml \
      objet.ml ocollection.ml \
      oset.ml oassoc.ml osequence.ml ograph.ml \
      oseti.ml oseth.ml osetb.ml osetpt.ml \
      oassocb.ml oassoch.ml oassoc_buffer.ml oassocid.ml \
      oarray.ml \
      ograph2way.ml ograph_extended.ml \
      fullcommon.ml 

# src from other authors, got from the web or caml hump
SRC=dumper.ml 
SRC+=ANSITerminal.ml 
SRC+=setb.ml mapb.ml # defunctorized version of standard set/map
SRC+=setPt.ml
SRC+=$(MYSRC)
SRC+=enum.ml dynArray.ml
SRC+=suffix_tree.ml suffix_tree_ext.ml 

SYSLIBS=str.cma unix.cma   


# other common libraries

#gdbm
MYGDBMSRC=oassocdbm.ml
GDBMSYSLIBS=dbm.cma

#berkeley db
BDBINCLUDES=-I ../ocamlbdb
MYBDBSRC=oassocbdb.ml
BDBSYSLIBS=bdb.cma

##############################################################################
# Generic variables
##############################################################################
# for warning:  -w A 
OCAMLCFLAGS=-g -dtypes

# This flag can also be used in subdirectories so don't change its name here.
# For profiling use:  -p -inline 0
OPTFLAGS=

# The OPTBIN variable is here to allow to use ocamlc.opt instead of 
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var 
# or setenv it to ".opt" in your startup script.
OPTBIN= #.opt

# The OCaml tools.
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS)
OCAMLLEX = ocamllex$(OPTBIN)
OCAMLYACC= ocamlyacc -v
OCAMLDEP = ocamldep
OCAMLMKTOP=ocamlmktop -g -custom


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

gdbm: commons_gdbm.cma
gdbm.opt: commons_gdbm.cmxa

bdb: 
	$(MAKE) INCLUDES="$(BDBINCLUDES)" commons_bdb.cma
bdb.opt: 
	$(MAKE) INCLUDES="$(BDBINCLUDES)" commons_bdb.cmxa

commons_gdbm.cma: $(MYGDBMSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_gdbm.cmxa: $(MYGDBMSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

commons_bdb.cma: $(MYBDBSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_bdb.cmxa: $(MYBDBSRC:.ml=.cmx)
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
	$(OCAMLC) $(INCLUDES) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDES) -c $<
.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c $<

clean::
	rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot
	rm -f *~ .*~ #*# 

depend: 
	$(OCAMLDEP) *.mli *.ml > .depend

distclean::
	rm -f .depend

-include .depend
