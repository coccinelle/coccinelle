# path to coccinelle source
COCCIDIR=../../..
SCRIPTDIR=../scripts

# do not raise error if config does not exist; handle in $(CONFIG) rule below
-include $(COCCIDIR)/Makefile.config

# where to install the program
INSTALLDIR=$(DESTDIR)$(SHAREDIR)/sgen

##############################################################################
# Variables
##############################################################################

TARGET=sgen

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
LEXER_SRC= sgen_lexer.mll

# compiled lexers
GENERATED= $(LEXER_SRC:.mll=.ml)

# all source OCaml files that have interfaces (.mli)
SRC= globals.ml ast_tostring.ml detect_patch.ml meta_variable.ml \
 snapshot.ml user_input.ml position_generator.ml disj_generator.ml \
 rule_body.ml rule_header.ml context_rule.ml script_rule.ml \
 file_transform.ml sgen_interactive.ml sgen_config.ml sgen.ml sgen_test.ml

# all source OCaml interface files (ocamlfind??)
SRC_INTERFACE= $(SRC:.ml=.mli)

# all source OCaml files (except for the generated lexer)
FULL_SRC= $(SRC_INTERFACE) $(SRC) main.ml

OCAMLCFLAGS ?= -g
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLBUILD=/usr/bin/ocamlbuild
CFLAGS = -cflags '$(INCLUDES)'
LFLAGS = -lflags '$(SYSLIBS) $(LIBS)'

##############################################################################
# Top rules
##############################################################################

all: $(TARGET)

$(TARGET): $(CONFIG) $(LIBS) $(GENERATED) $(FULL_SRC)
	@$(ECHO) -e "\n\n\tCompiling sgen ...\n\n"
	$(OCAMLC_CMD) -custom -o $(TARGET) $(SYSLIBS) $(LIBS) $(GENERATED) $(FULL_SRC)
	@$(ECHO) -e "\n\n\tsgen can now be installed via 'make install'.\n\n"

clean:
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f $(TARGET)
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend
	rm -f $(GENERATED)

$(CONFIG) $(LIBS):
	@echo "\n\n\tYou need to compile Coccinelle first.\n\n"
	@false

install: $(TARGET)
	# this is a small hack to avoid having to reconfigure the installation
	# directory path (and to avoid having the cocci config depend on sgen)
	if !([ -e $(SCRIPTDIR)/sgen.sh ]); then \
	  sed "\_^"PREFIX"=_ s_=.*_="$(prefix)"_" $(SCRIPTDIR)/sgen.sh.in > \
	  $(SCRIPTDIR)/sgen.sh; \
	fi;

	$(MKDIR_P) $(INSTALLDIR)
	$(INSTALL_PROGRAM) $(TARGET) $(INSTALLDIR)/$(TARGET)
	$(INSTALL_PROGRAM) $(SCRIPTDIR)/sgen.sh $(DESTDIR)$(BINDIR)/sgen
	@$(ECHO) -e "\n\n\tsgen installed: DONE!!!\n\n"

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET)
	rm -f $(INSTALLDIR)/$(TARGET)
	rm -f $(SCRIPTDIR)/sgen.sh
	rmdir $(INSTALLDIR)
	@$(ECHO) -e "\n\n\tsgen uninstalled: DONE!!!\n\n"

$(LEXER_SRC:.mll=.ml) :	$(LEXER_SRC)
	$(OCAMLLEX) $(LEXER_SRC)

.depend depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend
