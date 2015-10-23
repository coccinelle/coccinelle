# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

# path to coccinelle source
COCCIDIR=../../..
SCRIPTDIR=../scripts

# do not raise error if config does not exist; handle in $(CONFIG) rule below
-include $(COCCIDIR)/Makefile.config

# where to install the program
INSTALLDIR=$(DESTDIR)$(LIBDIR)/spgen

##############################################################################
# Variables
##############################################################################

TARGET=spgen

CONFIG= $(COCCIDIR)/Makefile.config

SYSLIBS= str.cma unix.cma bigarray.cma nums.cma

LIBS= $(PCREDIR)/pcre.cma \
	$(DYNLINKDIR)/dynlink.cma \
	$(COCCIDIR)/commons/commons.cma \
	$(COCCIDIR)/globals/globals.cma \
	$(COCCIDIR)/parsing_cocci/cocci_parser.cma

INCLUDEDIRS= $(COCCIDIR)/commons $(COCCIDIR)/commons/ocamlextra \
     $(COCCIDIR)/globals $(COCCIDIR)/parsing_cocci $(PCREDIR)

INCLUDES=$(INCLUDEDIRS:%=-I %)

# lexer source
LEXER_SRC= spgen_lexer.mll

# compiled lexers
GENERATED= $(LEXER_SRC:.mll=.ml)

# all source OCaml files that have interfaces (.mli)
SRC= globals.ml ast_tostring.ml detect_patch.ml meta_variable.ml \
 snapshot.ml user_input.ml position_generator.ml disj_generator.ml \
 rule_body.ml rule_header.ml context_rule.ml script_rule.ml \
 file_transform.ml spgen_interactive.ml spgen_config.ml spgen.ml spgen_test.ml

# all source OCaml interface files (ocamlfind??)
SRC_INTERFACE= $(SRC:.ml=.mli)

# all source OCaml files (except for the generated lexer)
FULL_SRC= $(SRC_INTERFACE) $(SRC) main.ml

OCAMLCFLAGS ?= -g
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
CFLAGS = -cflags '$(INCLUDES)'
LFLAGS = -lflags '$(SYSLIBS) $(LIBS)'

##############################################################################
# Top rules
##############################################################################

all: $(TARGET)

# currently no support for opt
all.opt: $(TARGET)

$(TARGET): $(CONFIG) $(LIBS) $(GENERATED) $(FULL_SRC)
	$(OCAMLC_CMD) -custom -o $(TARGET) $(SYSLIBS) $(LIBS) $(GENERATED) $(FULL_SRC)

.PHONY: clean distclean

clean:
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend
	rm -f $(GENERATED)

distclean:
	rm -f $(TARGET)

$(CONFIG) $(LIBS):
	@echo "\n\n\tYou need to compile Coccinelle first.\n\n"
	@false

$(SCRIPTDIR)/spgen.sh:
	@$(ECHO) -e "\n\n\tYou need to run ./configure from the Coccinelle directory first.\n\n"
	@false

install: $(TARGET) $(SCRIPTDIR)/spgen.sh uninstall-legacy
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET)
	$(MKDIR_P) $(INSTALLDIR)
	$(INSTALL_PROGRAM) $(TARGET) $(INSTALLDIR)/$(TARGET)
	$(INSTALL_PROGRAM) $(SCRIPTDIR)/spgen.sh $(DESTDIR)$(BINDIR)/$(TARGET)

uninstall-legacy:
	# legacy cleanup, from when the tool was called sgen
	rm -f $(DESTDIR)$(BINDIR)/sgen $(SCRIPTDIR)/sgen.sh
	rm -rf $(DESTDIR)$(LIBDIR)/sgen

uninstall: uninstall-legacy
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET)
	rm -f $(INSTALLDIR)/$(TARGET)
	rmdir $(INSTALLDIR)

$(LEXER_SRC:.mll=.ml) :	$(LEXER_SRC)
	$(OCAMLLEX) $(LEXER_SRC)

.PHONY: depend
.depend depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend
