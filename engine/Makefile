# Don't forget to first do a 'make links' to be able to compile the standalone cocci parser

SOURCES = ast_ctl.ml ast0toctl.ml ctltotex.ml
TARGET=engine

# The Caml compilers.
CAMLC =ocamlc -g
CAMLOPT = ocamlopt
CAMLLEX = ocamllex
CAMLYACC= ocamlyacc -v
CAMLDEP = ocamldep
EXEC=$(TARGET)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

GENERATED=
OBJS = ast_ctl.cmo ast0toctl.cmo ctltotex.cmo 
OPTOBJS = $(OBJS:.cmo=.cmx)

INCLUDE_PATH = -I ../commons -I ../parsing_cocci

#all: $(LIB)
all: $(EXEC) $(LIB)

all.opt: $(OPTLIB)


$(LIB): $(GENERATED) $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

# clean rule for LIB
clean::
	rm -f $(LIB)


$(OPTLIB): $(GENERATED) $(OPTOBJS) 
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

LIBS=../commons/commons.cma ../parsing_cocci/cocci_parser.cma
SYSLIBS = str.cma unix.cma

$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(CAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo


# clean rule for LIB.opt
clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	
	rm -f $(TARGET)


links: 
	ln -s ../ast_cocci.ml
#	ln -s ../commons

dellinks:
	rm -f ast_cocci.ml


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

depend: $(GENERATED)
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

include .depend
