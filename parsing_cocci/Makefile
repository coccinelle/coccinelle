# Copyright 2012-2014, INRIA
# Julia Lawall, Gilles Muller
# Copyright 2010-2011, INRIA, University of Copenhagen
# Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
# Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
# Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
# This file is part of Coccinelle.
#
# Coccinelle is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Coccinelle is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Coccinelle under other licenses.


ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

TARGET=cocci_parser

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

LEXER_SOURCES = lexer_cocci.mll
CLI_LEXER_SOURCES = lexer_cli.mll
SCRIPT_LEXER_SOURCES = lexer_script.mll
PARSER_SOURCES = parser_cocci_menhir.mly
SOURCES = flag_parsing_cocci.ml type_cocci.ml ast_cocci.ml ast0_cocci.ml \
pretty_print_cocci.ml unparse_ast0.ml visitor_ast0_types.ml \
visitor_ast.ml visitor_ast0.ml compute_lines.ml comm_assoc.ml \
iso_pattern.ml iso_compile.ml single_statement.ml simple_assignments.ml \
get_metas.ml ast0toast.ml check_meta.ml top_level.ml type_infer.ml \
test_exps.ml unitary_ast0.ml arity.ml index.ml context_neg.ml \
adjust_pragmas.ml insert_plus.ml function_prototypes.ml \
unify_ast.ml semantic_cocci.ml data.ml free_vars.ml safe_for_multi_decls.ml \
parse_printf.ml parse_aux.ml disjdistr.ml \
$(LEXER_SOURCES:.mll=.ml) $(PARSER_SOURCES:.mly=.ml) \
$(CLI_LEXER_SOURCES:.mll=.ml) $(SCRIPT_LEXER_SOURCES:.mll=.ml) \
cocci_grep.ml dpll.ml get_constants2.ml id_utils.ml adjacency.ml \
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


ifneq ($(FEATURE_OCAMLBUILD),yes)
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
else
all:
	cd .. && $(OCAMLBUILD) parsing_cocci/parsing_cocci.cma

all.opt:
	cd .. && $(OCAMLBUILD) parsing_cocci/parsing_cocci.cmxa

$(EXEC):
	cd .. && $(OCAMLBUILD) parsing_cocci/main.byte
	cp ../_build/parsing_cocci/main.byte $(EXEC)

clean::
	cd .. && $(OCAMLBUILD) -clean
	rm -f $(TARGET) $(TARGET).byte $(TARGET).native
endif

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

ifneq ($(FEATURE_OCAMLBUILD),yes)
$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	$(MENHIR) --ocamlc "${OCAMLC}" --ocamldep "${OCAMLDEP}" --table --base parser_cocci_menhir $(PARSER_SOURCES)
else
$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	cd .. && $(OCAMLBUILD) parsing_cocci/$@
	cp ../_build/parsing_cocci/$@ $@
endif

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


.PHONEY: depend
.depend depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend

lexer_cocci.ml: lexer_cocci.mll
lexer_script.ml: lexer_script.mll
lexer_cli.ml: lexer_cli.mll
parser_cocci_menhir.ml: parser_cocci_menhir.mly lexer_cocci.mll
parser_cocci_menhir.mli: parser_cocci_menhir.mly lexer_cocci.mll


ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(FEATURE_OCAMLBUILD),yes)
-include .depend
endif
endif
endif

include ../Makefile.common
