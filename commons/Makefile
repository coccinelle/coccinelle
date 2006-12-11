#note: if you add a file (a .mli or .ml), dont forget to do a   make depend

TARGET = commons

SRC = dumper.ml suffix_tree.ml \
      commonop.ml common.ml \
      setb.ml mapb.ml  setPt.ml seti.ml \
      ocollection.ml \
      oset.ml oassoc.ml osequence.ml oarray.ml ograph.ml \
      oseti.ml oseth.ml osetb.ml osetpt.ml \
      oassocb.ml oassoch.ml oassoc_buffer.ml oassocid.ml \
      ograph2way.ml ograph_extended.ml \
      fullcommon.ml 

SYSLIBS=str.cma unix.cma

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OPTFLAGS=

# The Caml compilers.
CAMLC =ocamlc$(OPTBIN) -g -dtypes $(PP)
CAMLOPT=ocamlopt$(OPTBIN)  $(PP) $(OPTFLAGS)
CAMLLEX = ocamllex$(OPTBIN)
CAMLYACC= ocamlyacc
CAMLDEP = ocamldep$(OPTBIN) $(PP)
CAMLMKTOP=ocamlmktop -g -custom


LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(LIB)
all.opt: $(OPTLIB)

$(TARGET).top: $(LIB)
	$(CAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(OBJS)


$(LIB):  $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(OPTOBJS)
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

clean::
	rm -f $(LIB)

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
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*# 

depend: 
	$(CAMLDEP) *.mli *.ml > .depend

#clean::
#	rm -f .depend

-include .depend
