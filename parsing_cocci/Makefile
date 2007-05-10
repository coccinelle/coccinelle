TARGET=cocci_parser

LEXER_SOURCES = lexer_cocci.mll
PARSER_SOURCES = parser_cocci_menhir.mly
SOURCES = flag_parsing_cocci.ml type_cocci.ml ast_cocci.ml ast0_cocci.ml \
pretty_print_cocci.ml unparse_ast0.ml \
visitor_ast.ml visitor_ast0.ml compute_lines.ml \
iso_pattern.ml single_statement.ml \
ast0toast.ml check_meta.ml top_level.ml type_infer.ml \
unitary_ast0.ml arity.ml index.ml context_neg.ml \
insert_plus.ml function_prototypes.ml \
unify_ast.ml semantic_cocci.ml data.ml free_vars.ml parse_aux.ml \
$(LEXER_SOURCES:.mll=.ml) $(PARSER_SOURCES:.mly=.ml) get_constants.ml \
parse_cocci.ml

LIBS=../commons/commons.cma
SYSLIBS = str.cma unix.cma

INCLUDE_PATH = -I ../commons


# The Caml compilers.
CAMLC =ocamlc$(OPTBIN) -g
CAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS)
CAMLLEX = ocamllex$(OPTBIN)
CAMLYACC= menhir
CAMLDEP = ocamldep$(OPTBIN)
EXEC=$(TARGET).byte
EXEC=$(TARGET)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

GENERATED= $(LEXER_SOURCES:.mll=.ml) \
	   $(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli)
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)


all: $(LIB)
local: $(EXEC)

all.opt: $(OPTLIB)


$(LIB): $(GENERATED) $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

# clean rule for LIB
clean::
	rm -f $(LIB)


$(OPTLIB): $(GENERATED) $(OPTOBJS) 
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

$(LEXER_SOURCES:.mll=.ml) :	$(LEXER_SOURCES)
	$(CAMLLEX) $(LEXER_SOURCES)

$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	$(CAMLYACC) $(PARSER_SOURCES)

clean::
	rm -f $(GENERATED)

# clean rule for others files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*# 

depend: $(GENERATED)
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

.depend:
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

-include .depend

lexer_cocci.ml: lexer_cocci.mll
parser_cocci_menhir.ml: parser_cocci_menhir.mly lexer_cocci.mll
parser_cocci_menhir.mli: parser_cocci_menhir.mly lexer_cocci.mll
