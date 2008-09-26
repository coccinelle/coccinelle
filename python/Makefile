#############################################################################
# Configuration section
#############################################################################
-include ../Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=coccipython

SOURCES= pycocci_aux.ml pycocci.ml 

INCLUDEDIRS = ../commons ../commons/ocamlextra ../globals ../pycaml \
	      ../parsing_c ../parsing_cocci

SYSLIBS = str.cma unix.cma
LIBS=../commons/commons.cma ../globals/globals.cma 

#     ../ctl/ctl.cma \
#     ../parsing_c/c_parser.cma ../parsing_cocci/cocci_parser.cma 
#pycaml/pycaml.cma 


##############################################################################
# Generic variables
##############################################################################

INCLUDE_PATH=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# The Caml compilers.
CAMLC =ocamlc$(OPTBIN) -g -dtypes 
CAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS)
CAMLLEX = ocamllex$(OPTBIN)
CAMLYACC= ocamlyacc -v
CAMLDEP = ocamldep$(OPTBIN)


##############################################################################
# Top rules
##############################################################################

EXEC=$(TARGET).byte
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

CTLEXEC=$(CTLTARGET)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

CTLOBJS = $(CTLSOURCES:.ml=.cmo)
CTLOPTOBJS = $(CTLOBJS:.cmo=.cmx)


#all: $(EXEC) $(LIB)
all: $(LIB)

all.opt: $(OPTLIB)

ctl: $(CTLEXEC)


$(LIB): $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

clean::
	rm -f $(LIB)


$(OPTLIB): $(OPTOBJS) 
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(CAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo

$(CTLEXEC): $(CTLOBJS) $(LIBS)
	$(CAMLC) -o $(CTLEXEC) $(SYSLIBS) $(LIBS) $(CTLOBJS)


clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	
	rm -f $(TARGET) rm -f $(TARGET).byte
	rm -f $(CTLTARGET)


#pycocci.ml: ../pycaml/pycaml.ml ../pycaml/pycaml_ml.c
#pycocci_aux.ml:  ../pycaml/pycaml.ml ../pycaml/pycaml_ml.c


##############################################################################
# Generic ocaml rules
##############################################################################

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
	rm -f *.cm[iox] *.o  *.annot
	rm -f *~ .*~ #*# 

beforedepend:

depend: beforedepend
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

.depend: 
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

-include .depend
