ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

##############################################################################
# Variables
##############################################################################
TARGET=parsing_c

# - type_cocci.ml ast_cocci.ml  # + unparse_hrule
SRC= token_annot.ml flag_parsing_c.ml parsing_stat.ml \
 token_c.ml ast_c.ml control_flow_c.ml \
 visitor_c.ml lib_parsing_c.ml \
 control_flow_c_build.ml \
 pretty_print_c.ml \
 semantic_c.ml lexer_parser.ml parser_c.ml lexer_c.ml \
 parse_string_c.ml token_helpers.ml token_views_c.ml \
 cpp_token_c.ml  \
 parsing_hacks.ml \
 cpp_analysis_c.ml \
 unparse_cocci.ml unparse_c.ml unparse_hrule.ml  \
 parsing_recovery_c.ml parsing_consistency_c.ml \
 danger.ml parse_c.ml type_c.ml \
 cpp_ast_c.ml \
 type_annoter_c.ml comment_annotater_c.ml \
 compare_c.ml \
 test_parsing_c.ml



# ast_cocci.ml and unparse_cocci.ml should be deleted in the future
# to make parsing_c really independent of coccinelle.
# control_flow_c have also coccinelle dependencies.
# old: parsing_c now depends on cocci_parser because in addition to decorate
# the token in Ast_c with some parse info, we now also make some place to
# welcome some mcodekind of Ast_cocci.
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../parsing_cocci/cocci_parser.cma

INCLUDESDEP= -I ../commons -I ../commons/ocamlextra -I ../commons/ocollection \
  -I ../globals -I  ../parsing_cocci

INCLUDES=$(INCLUDESDEP) $(TARZANINCLUDE)

SYSLIBS= str.cma unix.cma num.cma

##############################################################################
# Generic variables
##############################################################################

#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDESDEP)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma
all.opt:
	@$(MAKE) $(TARGET).cmxa BUILD_OPT=yes

$(TARGET).cma: $(OBJS)
	$(OCAMLC_CMD) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP_CMD) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top

lexer_c.ml: lexer_c.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_c.ml

parser_c.ml parser_c.mli: parser_c.mly
	$(OCAMLYACC) -v $<
clean::
	rm -f parser_c.ml parser_c.mli parser_c.output


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
# Literate Programming rules
##############################################################################


##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend

distclean: clean

.depend depend: lexer_c.ml parser_c.ml parser_c.mli
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include .depend
endif
endif

include ../Makefile.common
