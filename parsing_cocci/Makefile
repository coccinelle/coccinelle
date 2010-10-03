# Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
# Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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


TARGET=cocci_parser

LEXER_SOURCES = lexer_cocci.mll
SCRIPT_LEXER_SOURCES = lexer_script.mll
PARSER_SOURCES = parser_cocci_menhir.mly
SOURCES = flag_parsing_cocci.ml type_cocci.ml ast_cocci.ml ast0_cocci.ml \
pretty_print_cocci.ml unparse_ast0.ml \
visitor_ast.ml visitor_ast0.ml compute_lines.ml comm_assoc.ml \
iso_pattern.ml iso_compile.ml single_statement.ml simple_assignments.ml \
ast0toast.ml check_meta.ml top_level.ml type_infer.ml test_exps.ml \
unitary_ast0.ml arity.ml index.ml context_neg.ml \
insert_plus.ml function_prototypes.ml \
unify_ast.ml semantic_cocci.ml data.ml free_vars.ml parse_aux.ml disjdistr.ml \
$(LEXER_SOURCES:.mll=.ml) $(PARSER_SOURCES:.mly=.ml) \
$(SCRIPT_LEXER_SOURCES:.mll=.ml) \
get_constants.ml get_constants2.ml parse_cocci.ml

LIBS=../commons/commons.cma ../globals/globals.cma 
SYSLIBS = str.cma unix.cma

#MENHIR_PATH=$(shell ocamlfind query menhirLib)
MENHIR_PATH=../menhirlib

INCLUDES = -I ../commons -I ../commons/ocamlextra -I ../globals \
-I $(MENHIR_PATH)

MENHIR=$(MENHIR_PATH)/menhirLib.cmo
MENHIRO=$(MENHIR_PATH)/menhirLib.cmx


# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX = ocamllex$(OPTBIN)
OCAMLYACC= menhir --table
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)
EXEC=$(TARGET).byte
EXEC=$(TARGET)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

GENERATED= $(LEXER_SOURCES:.mll=.ml) $(SCRIPT_LEXER_SOURCES:.mll=.ml) \
	   $(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli)
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)


all: $(LIB)
local: $(EXEC)

all.opt: $(OPTLIB)


$(LIB): $(GENERATED) $(OBJS)
	$(OCAMLC) -I $(MENHIR_PATH) -a -o $(LIB) $(MENHIR) $(OBJS)


$(OPTLIB): $(GENERATED) $(OPTOBJS) 
	$(OCAMLOPT) -I $(MENHIR_PATH) -a -o $(OPTLIB) $(MENHIRO) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo




clean::
	rm -f $(LIB)
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	
	rm -f $(TARGET)




.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(LEXER_SOURCES:.mll=.ml) :	$(LEXER_SOURCES)
	$(OCAMLLEX) $(LEXER_SOURCES)

$(PARSER_SOURCES:.mly=.ml) $(PARSER_SOURCES:.mly=.mli) : $(PARSER_SOURCES)
	$(OCAMLYACC) $(PARSER_SOURCES)

$(SCRIPT_LEXER_SOURCES:.mll=.ml): $(SCRIPT_LEXER_SOURCES)
	$(OCAMLLEX) $(SCRIPT_LEXER_SOURCES)

clean::
	rm -f $(GENERATED)

# clean rule for others files
clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ #*# 

depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend

.depend:
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend

lexer_cocci.ml: lexer_cocci.mll
parser_cocci_menhir.ml: parser_cocci_menhir.mly lexer_cocci.mll
parser_cocci_menhir.mli: parser_cocci_menhir.mly lexer_cocci.mll
lexer_script.ml: lexer_script.mll
