# Don't forget to first do a 'make links' to be able to compile the standalone cocci parser

TARGET=cocciengine

SOURCES = isomorphisms.ml isomorphisms_c_c.ml pattern.ml transformation.ml ast0toctl.ml ctltotex.ml ctlcocci_integration.ml

INCLUDE_PATH = -I ../commons -I ../ctl -I .. -I ../parsing_c -I ../parsing_cocci

SYSLIBS = str.cma unix.cma
LIBS=../commons/commons.cma ../ctl/ctl.cma ../parsing_c/c_parser.cma ../parsing_cocci/cocci_parser.cma 


# The Caml compilers.
CAMLC =ocamlc -g
CAMLOPT = ocamlopt
CAMLLEX = ocamllex
CAMLYACC= ocamlyacc -v
CAMLDEP = ocamldep

EXEC=$(TARGET).byte
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)


all: $(EXEC) $(LIB)

all.opt: $(OPTLIB)


$(LIB): $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

# clean rule for LIB
clean::
	rm -f $(LIB)


$(OPTLIB): $(OPTOBJS) 
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(CAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo


# clean rule for LIB.opt
clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	
	rm -f $(TARGET)



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

beforedepend:

depend: beforedepend
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

.depend: 
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

include .depend
