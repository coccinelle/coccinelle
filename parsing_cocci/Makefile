TARGET=cocci_parser

LEXER_SOURCES = lexer_cocci.mll
PARSER_SOURCES = parser_cocci_menhir.mly
SOURCES = type_cocci.ml ast_cocci.ml ast0_cocci.ml \
visitor_ast.ml visitor_ast0.ml \
ast0toast.ml check_meta.ml pretty_print_cocci.ml unparse_ast0.ml top_level.ml \
type_infer.ml \
arity.ml compute_lines.ml index.ml context_neg.ml insert_plus.ml \
iso_pattern.ml unify_ast.ml semantic_cocci.ml data.ml free_vars.ml \
single_statement.ml \
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
	rm -f *.cm[iox] *.o 
	rm -f *~ .*~ #*# 

depend: $(GENERATED)
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

.depend:
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

-include .depend

lexer_cocci.ml: lexer_cocci.mll
parser_cocci_menhir.ml: parser_cocci_menhir.mly lexer_cocci.mll
parser_cocci_menhir.mli: parser_cocci_menhir.mly lexer_cocci.mll
