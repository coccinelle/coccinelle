# Don't forget to first do a 'make links' to be able to compile the standalone cocci parser

SOURCES = ast_cocci.ml ast0_cocci.ml ast0toast.ml top_level.ml check_meta.ml \
unparse_cocci.ml arity.ml \
plus.ml merge.ml semantic_cocci.ml data.ml \
lexer_cocci.mll parser_cocci.mly parse_cocci.ml 
TARGET=parser

# The Caml compilers.
CAMLC =ocamlc -g
CAMLOPT = ocamlopt
CAMLLEX = ocamllex
CAMLYACC= ocamlyacc -v
CAMLDEP = ocamldep
EXEC=$(TARGET)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

GENERATED= parser_cocci.ml parser_cocci.mli lexer_cocci.ml
OBJS = ast_cocci.cmo ast0_cocci.cmo ast0toast.cmo top_level.cmo \
check_meta.cmo unparse_cocci.cmo arity.cmo \
plus.cmo merge.cmo semantic_cocci.cmo data.cmo lexer_cocci.cmo \
parser_cocci.cmo parse_cocci.cmo 
OPTOBJS = $(OBJS:.cmo=.cmx)

INCLUDE_PATH = -I ../commons

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

LIBS=../commons/commons.cma
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

lexer_cocci.ml :	lexer_cocci.mll
	$(CAMLLEX) lexer_cocci.mll

parser_cocci.ml parser_cocci.mli : parser_cocci.mly
	$(CAMLYACC) parser_cocci.mly

clean::
	rm -f $(GENERATED)

# clean rule for others files
clean::
	rm -f *.cm[iox] *.o 
	rm -f *~ .*~ #*# 

depend: $(GENERATED)
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

#clean::
#	rm -f .depend

#.depend: $(GENERATED)
#	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

include .depend

lexer_cocci.ml: lexer_cocci.mll
parser_cocci.ml: parser_cocci.mly lexer_cocci.mll
parser_cocci.mli: parser_cocci.mly lexer_cocci.mll
