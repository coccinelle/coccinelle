# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

TARGET=cocci_parser

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

LEXER_SOURCES = lexer_cocci.mll
CLI_LEXER_SOURCES = lexer_cli.mll
SCRIPT_LEXER_SOURCES = lexer_script.mll
PARSER_SOURCES = parser_cocci_menhir.mly
SOURCES = flag_parsing_cocci.ml ast_cocci.ml ast0_cocci.ml \
pretty_print_cocci.ml visitor_ast0_types.ml \
visitor_ast.ml visitor_ast0.ml ast0toast.ml unparse_ast0.ml \
compute_lines.ml comm_assoc.ml \
iso_pattern.ml iso_compile.ml single_statement.ml simple_assignments.ml \
get_metas.ml stmtlist.ml check_meta.ml top_level.ml \
type_infer.ml test_exps.ml unitary_ast0.ml arity.ml index.ml context_neg.ml \
adjust_pragmas.ml insert_plus.ml function_prototypes.ml \
unify_ast.ml semantic_cocci.ml data.ml free_vars.ml safe_for_multi_decls.ml \
parse_printf.ml parse_aux.ml disjdistr.ml \
$(LEXER_SOURCES:.mll=.ml) $(PARSER_SOURCES:.mly=.ml) \
$(CLI_LEXER_SOURCES:.mll=.ml) $(SCRIPT_LEXER_SOURCES:.mll=.ml) \
cocci_grep.ml dpll.ml get_constants2.ml id_utils.ml git_grep.ml adjacency.ml \
commas_on_lists.ml parse_cocci.ml command_line.ml

LIBS=../commons/commons.cma ../globals/globals.cma
SYSLIBS = str.cma unix.cma

INCLUDES = -I ../commons \
	   -I ../commons/ocamlextra \
	   -I ../globals \
	   -I $(MENHIRDIR)

MENHIRMOD=menhirLib.cmo
MENHIROMOD=menhirLib.cmx

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
EXEC=$(TARGET).byte
EXEC=$(TARGET)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

GENERATED= $(LEXER_SOURCES:.mll=.ml) \
	   $(CLI_LEXER_SOURCES:.mll=.ml) $(SCRIPT_LEXER_SOURCES:.mll=.ml) \
	   $(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli)
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)


all: $(LIB)
local: $(EXEC)

all.opt:
	@$(MAKE) $(OPTLIB) BUILD_OPT=yes

$(LIB): $(GENERATED) $(OBJS)
	$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES) -I $(MENHIRDIR) -a -o $(LIB) $(MENHIRMOD) $(OBJS)


$(OPTLIB): $(GENERATED) $(OPTOBJS)
	$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES) -I $(MENHIRDIR) -a -o $(OPTLIB) $(MENHIROMOD) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo

clean::
	rm -f $(LIB)
	rm -f $(OPTLIB) $(LIB:.cma=.a)
	rm -f $(TARGET)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES) -c $<

$(LEXER_SOURCES:.mll=.ml) :	$(LEXER_SOURCES)
	$(OCAMLLEX) $(LEXER_SOURCES)

$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	$(MENHIR) --ocamlc "${OCAMLC}" --ocamldep "${OCAMLDEP}" --table --base parser_cocci_menhir $(PARSER_SOURCES)

$(CLI_LEXER_SOURCES:.mll=.ml): $(CLI_LEXER_SOURCES)
	$(OCAMLLEX) $(CLI_LEXER_SOURCES)

$(SCRIPT_LEXER_SOURCES:.mll=.ml): $(SCRIPT_LEXER_SOURCES)
	$(OCAMLLEX) $(SCRIPT_LEXER_SOURCES)

distclean:: clean
	if test -z "${KEEP_GENERATED}"; then \
		@echo cleaning generated parsers and lexers; \
		rm -f $(GENERATED); fi

# clean rule for others files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*#
	rm -f .depend


.PHONY: depend
.depend depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend

lexer_cocci.ml: lexer_cocci.mll
lexer_script.ml: lexer_script.mll
lexer_cli.ml: lexer_cli.mll
parser_cocci_menhir.ml: parser_cocci_menhir.mly lexer_cocci.mll
parser_cocci_menhir.mli: parser_cocci_menhir.mly lexer_cocci.mll


ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include .depend
endif
endif

include ../Makefile.common
