#note: if you add a file (a .mli or .ml), dont forget to do a   make depend

SRC = dumper.ml suffix_tree.ml \
	commonop.ml common.ml \
	setb.ml mapb.ml  setPt.ml seti.ml \
	ocollection.ml \
	oset.ml oassoc.ml osequence.ml oarray.ml ograph.ml \
	oseti.ml oseth.ml osetb.ml osetpt.ml \
	oassocb.ml oassoch.ml oassoc_buffer.ml oassocid.ml \
	ograph2way.ml ograph_extended.ml \
	fullcommon.ml 



TARGET = commons

# The Caml compilers.
CAMLC =ocamlc -g $(PP)
CAMLOPT=ocamlopt  $(PP) -inline 0
CAMLLEX = ocamllex
CAMLYACC= ocamlyacc
CAMLDEP = ocamldep $(PP)

LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(LIB)
all.opt: $(OPTLIB)


$(LIB):  $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

# clean rule for LIB
clean::
	rm -f $(LIB)


$(OPTLIB): $(OPTOBJS)
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

# clean rule for LIB.opt
clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(INCLUDE_PATH) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDE_PATH) -c $<

.ml.cmx:
	$(CAMLOPT) $(INCLUDE_PATH) -c $<

# clean rule for others files
clean::
	rm -f *.cm[iox] *.o 
	rm -f *~ .*~ #*# 

depend: 
	$(CAMLDEP) *.mli *.ml > .depend

#clean::
#	rm -f .depend

.depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
