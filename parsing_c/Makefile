##############################################################################
# Variables
##############################################################################
TARGET=parsing_c


# - type_cocci.ml ast_cocci.ml  # + unparse_hrule 
SRC= flag_parsing_c.ml parsing_stat.ml \
 ast_c.ml control_flow_c.ml \
 visitor_c.ml lib_parsing_c.ml \
 ast_to_flow.ml \
 pretty_print_c.ml \
 semantic_c.ml lexer_parser.ml parser_c.ml lexer_c.ml \
 token_helpers.ml parsing_hacks.ml \
 unparse_cocci.ml unparse_c.ml unparse_hrule.ml  \
 parse_c.ml \
 cpp_ast_c.ml \
 compare_c.ml type_annoter_c.ml \
 test_parsing_c.ml



# ast_cocci.ml and unparse_cocci.ml should be deleted in the futur
# to make parsing_c really independent of coccinelle. 
# control_flow_c have also coccinelle dependencies.
# old: parsing_c now depends on cocci_parser because in addition to decorate 
# the token in Ast_c with some parse info, we now also make some place to
# welcome some mcodekind of Ast_cocci.
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../parsing_cocci/cocci_parser.cma

INCLUDES= -I ../commons -I ../globals -I  ../parsing_cocci 

#LIBS=../commons/commons.cma
#INCLUDES= -I ../commons
SYSLIBS= str.cma unix.cma 

##############################################################################
# Generic variables
##############################################################################

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g -dtypes

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex$(OPTBIN) #-ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN) $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)


OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma
all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top



lexer_c.ml: lexer_c.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_c.ml
beforedepend:: lexer_c.ml


parser_c.ml parser_c.mli: parser_c.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_c.ml parser_c.mli parser_c.output
beforedepend:: parser_c.ml parser_c.mli


##############################################################################
# Pad's rules
##############################################################################
# visitor_c.ml lib_parsing_c.ml \
# type_annoter_c.ml  \
# statistics_c.ml  \
# pretty_print_c.ml unparse_c.ml \
# test_parsing_c.ml
#toreput: compare_c.ml ast_to_flow.ml

COREPARSING= flag_parsing_c.ml parsing_stat.ml \
 ast_cocci.ml \
 ast_c.ml control_flow_c.ml \
 semantic_c.ml lexer_parser.ml parser_c.mly lexer_c.mll \
 token_helpers.ml parsing_hacks.ml parse_c.ml \

locparsing:
	wc -l $(COREPARSING)

locindiv:
	wc -l lexer_c.mll
	wc -l parser_c.mly
	wc -l parsing_hacks.ml
	wc -l ast_c.ml
	wc -l parse_c.ml


##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.mldepend: 
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend
