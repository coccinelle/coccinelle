#note: if you add a file (a .mli or .ml), dont forget to do a   make depend

TARGET = ctl

SRC = flag_ctl.ml ast_ctl.ml pretty_print_ctl.ml ctl_engine.ml wrapper_ctl.ml

SYSLIBS=str.cma unix.cma
LIBS=../commons/commons.cma

ADDONSPATH = -I ../commons


#The Caml compilers.
#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
CAMLC =ocamlc$(OPTBIN) -dtypes -g $(ADDONSPATH)
CAMLOPT=ocamlopt$(OPTBIN)  $(ADDONSPATH) $(OPTFLAGS)
CAMLLEX = ocamllex$(OPTBIN)
CAMLYACC= ocamlyacc
CAMLDEP = ocamldep$(OPTBIN) $(ADDONSPATH)
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

clean::
	rm -f $(LIB) $(TARGET).top


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
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*# 

depend: 
	$(CAMLDEP) *.mli *.ml > .depend

#clean::
#	rm -f .depend

.depend:
	$(CAMLDEP) *.mli *.ml > .depend

-include .depend
