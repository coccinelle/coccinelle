#note: if you add a file (a .mli or .ml), dont forget to do a   make depend

TARGET = ctl

SRC = ast_ctl.ml ctl_engine.ml wrapper_ctl.ml

SYSLIBS=str.cma unix.cma
LIBS=../commons/commons.cma

ADDONSPATH = -I ../commons



OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

#The Caml compilers.
#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
CAMLC =ocamlc -g $(ADDONSPATH)
CAMLOPT=ocamlopt  $(ADDONSPATH) 
CAMLLEX = ocamllex
CAMLYACC= ocamlyacc
CAMLDEP = ocamldep $(ADDONSPATH)
CAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)


LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(LIB)
all.opt: $(OPTLIB)

$(TARGET).top: $(LIB) test_ctl.cmo
	$(CAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS) test_ctl.cmo

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
