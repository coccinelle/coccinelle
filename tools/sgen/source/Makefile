# path to coccinelle source
COCCIDIR=../../..
SCRIPTDIR=../scripts

include $(COCCIDIR)/Makefile.config

# where to install the program
INSTALLDIR=$(DESTDIR)$(SHAREDIR)/sgen

##############################################################################
# Variables
##############################################################################

TARGET=sgen

SYSLIBS= str.cma unix.cma bigarray.cma nums.cma $(PCREDIR)/pcre.cma \
	$(DYNLINKDIR)/dynlink.cma
LIBS= $(COCCIDIR)/commons/commons.cma \
     $(COCCIDIR)/globals/globals.cma \
     $(COCCIDIR)/parsing_cocci/cocci_parser.cma
INCLUDEDIRS= $(COCCIDIR)/commons $(COCCIDIR)/commons/ocamlextra \
     $(COCCIDIR)/globals $(COCCIDIR)/parsing_cocci
INCLUDES=$(INCLUDEDIRS:%=-I %)

# lexers
LEXER_SRC= sgen_lexer.mll

# compiled lexers
GENERATED= $(LEXER_SRC:.mll=.ml)

# all source OCaml files that have interfaces (.mli)
SRC= globals.ml ast_tostring.ml detect_patch.ml meta_variable.ml \
 generator_types.ml position_generator.ml disj_generator.ml rule_body.ml \
 rule_header.ml context_rule.ml script_rule.ml user_input.ml file_transform.ml \
 sgen_interactive.ml sgen_config.ml

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

ifneq ($(FEATURE_OCAMLBUILD),yes)
all: $(TARGET)

$(TARGET): $(COCCIDIR)/Makefile.config $(LIBS) $(GENERATED) $(FULL_SRC)
	$(ECHO) -e "\n\tCompiling sgen ...\n"
	$(OCAMLC_CMD) -o $(TARGET) $(SYSLIBS) $(LIBS) $(GENERATED) $(FULL_SRC)
	@$(ECHO) -e "\n\n\tsgen can now be installed via 'make install'\n\n"

clean:
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f $(TARGET)
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend
	rm -f $(GENERATED)

else
all: $(TARGET)

$(TARGET): $(COCCIDIR)/Makefile.config $(LIBS) $(FULL_SRC)
	$(ECHO) -e "\n\tCompiling sgen ...\n"
	$(OCAMLBUILD) $(CFLAGS) $(LFLAGS) main.byte;
	cp _build/main.byte $(TARGET);
	@$(ECHO) -e "\n\n\tsgen can now be installed via 'make install'\n\n"

clean:
	rm *~; rm $(TARGET); $(OCAMLBUILD) -clean
endif

$(LIBS):
	$(ECHO) -e "\n\tCompiling Coccinelle ...\n"
	cd $(COCCIDIR);	make spatch

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
	rmdir $(INSTALLDIR)
	@$(ECHO) -e "\n\n\tsgen uninstalled: DONE!!!\n\n"

$(LEXER_SRC:.mll=.ml) :	$(LEXER_SRC)
	$(OCAMLLEX) $(LEXER_SRC)

.depend depend: $(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend
