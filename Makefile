#############################################################################
# Configuration section
#############################################################################

include Makefile.libs
-include Makefile.config
-include /etc/lsb-release

VERSION=$(shell cat ./version)
CCVERSION=$(shell cat scripts/coccicheck/README |grep "Coccicheck version" |perl -p -e 's/.*version (.*)[ ]*/$$1/;')
PKGVERSION=$(shell dpkg-parsechangelog -ldebian/changelog.$(DISTRIB_CODENAME) 2> /dev/null \
	 | sed -n 's/^Version: \(.*\)/\1/p' )

##############################################################################
# Variables
##############################################################################

TARGET=spatch
PRJNAME=coccinelle

LEXER_SOURCES =
SRC=flag_cocci.ml cocci.ml testing.ml test.ml $(LEXER_SOURCES:.mll=.ml) main.ml

ifeq ($(FEATURE_PYTHON),1)
	PYCMA=pycaml.cma
	PYTHON_INSTALL_TARGET=install-python
else
	PYCMA=
	PYTHON_INSTALL_TARGET=
endif

ifeq ("$(DYNLINK)","no")
	DYNLINK=
else
	DYNLINK=dynlink.cma
endif

ifdef PCREDIR
	PCRELIB=pcre.cma
else
	PCRELIB=
	PCREDIR=
endif

SEXPSYSCMA=bigarray.cma nums.cma

SYSLIBS=str.cma unix.cma $(SEXPSYSCMA) $(DYNLINK) $(PCRELIB) # threads.cma
LIBS=commons/commons.cma \
     commons/commons_sexp.cma \
     globals/globals.cma \
     ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/parsing_c.cma \
     engine/cocciengine.cma popl09/popl.cma \
     extra/extra.cma python/coccipython.cma ocaml/cocciocaml.cma

# used for depend: and a little for rec & rec.opt
MAKESUBDIRS=$(MAKELIBS) commons \
 globals ctl parsing_cocci parsing_c \
 engine popl09 extra python ocaml

# used for clean:
# It is like MAKESUBDIRS but also
# force cleaning of local library copies
CLEANSUBDIRS=commons \
 globals ctl parsing_cocci parsing_c \
 engine popl09 extra python ocaml \
 $(CLEANLIBS)

INCLUDEDIRSDEP=commons commons/ocamlextra \
 globals ctl \
 parsing_cocci parsing_c engine popl09 extra python ocaml \
 $(DEPLIBS)

INCLUDEDIRS=$(INCLUDEDIRSDEP) $(PCREDIR) $(INCLIBS)

EXTRALINKS=
LINKFLAGS=$(EXTRALINKS:%=-cclib -l%)

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

EXEC=$(TARGET)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLCFLAGS=

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
# To enable backtrace support for native code, you need to put -g in OPTFLAGS
# to also link with -g.
OPTFLAGS= -g

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(LINKFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(LINKFLAGS) $(INCLUDES)
OCAMLYACC_CMD=$(OCAMLYACC) -v
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDEDIRSDEP:%=-I %)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

# can also be set via 'make static'
CFLAGS=-pie -fPIE -fpic -fPIC -static
STATICCFLAGS=$(CFLAGS:%=-ccopt %)
STATIC= # $(STATICCFLAGS)

# can also be unset via 'make purebytecode'
BYTECODE_STATIC=-custom

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt byte opt top clean distclean configure
.PHONY:: $(MAKESUBDIRS) $(MAKESUBDIRS:%=%.opt) subdirs subdirs.opt

all: Makefile.config byte preinstall

opt all.opt: opt-compil preinstall

world: preinstall
	$(MAKE) byte
	$(MAKE) opt-compil

byte: .depend version.ml
	$(MAKE) subdirs
	$(MAKE) $(EXEC)

opt-compil: .depend version.ml
	$(MAKE) subdirs.opt
	$(MAKE) $(EXEC).opt

top: $(EXEC).top

subdirs:
	+for D in $(MAKESUBDIRS); do $(MAKE) $$D || exit 1 ; done
	$(MAKE) -C commons sexp OCAMLCFLAGS="$(OCAMLCFLAGS)"

subdirs.opt:
	+for D in $(MAKESUBDIRS); do $(MAKE) $$D.opt || exit 1 ; done
	$(MAKE) -C commons sexp.opt OPTFLAGS="$(OPTFLAGS)"

$(MAKESUBDIRS):
	$(MAKE) -C $@ OCAMLCFLAGS="$(OCAMLCFLAGS)" all

$(MAKESUBDIRS:%=%.opt):
	$(MAKE) -C $(@:%.opt=%) OPTFLAGS="$(OPTFLAGS)" all.opt

#dependencies:
# commons:
# globals:
# menhirLib:
# parsing_cocci: commons globals menhirLib
# parsing_c:parsing_cocci
# ctl:globals commons
# engine: parsing_cocci parsing_c ctl
# popl09:engine
# extra: parsing_cocci parsing_c ctl
# pycaml:
# python:pycaml parsing_cocci parsing_c

clean::
	set -e; for i in $(CLEANSUBDIRS); do $(MAKE) -C $$i $@; done
	$(MAKE) -C demos/spp $@

$(LIBS): $(MAKESUBDIRS)
$(LIBS:.cma=.cmxa): $(MAKESUBDIRS:%=%.opt)
$(LNKLIBS) : $(MAKESUBDIRS)
$(LNKOPTLIBS) : $(MAKESUBDIRS:%=%.opt)

$(OBJS):$(LIBS) $(LNKLIBS)
$(OPTOBJS):$(LIBS:.cma=.cmxa) $(LNKOPTLIBS)

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC_CMD) $(BYTECODE_STATIC) -o $@ $(FLAGSLIBS) $(SYSLIBS) $(LNKLIBS) $^

$(EXEC).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT_CMD) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $(FLAGSLIBS) $(OPTLNKLIBS) $^

$(EXEC).top: $(LIBS) $(OBJS) $(LNKLIBS)
	$(OCAMLMKTOP_CMD) -custom -o $@ $(SYSLIBS) $(FLAGSLIBS) $(LNKLIBS) $^

clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

.PHONY:: tools configure

configure:
	./configure

Makefile.config:
	@echo "Makefile.config is missing. Run ./configure to generate it."
	exit 1

tools: $(LIBS) $(LNKLIBS)
	$(MAKE) -C tools

distclean::
	if [ -d tools ] ; then $(MAKE) -C tools distclean ; fi

static:
	rm -f spatch.opt spatch
	$(MAKE) STATIC="$(STATICCFLAGS)" spatch.opt
	cp spatch.opt spatch

purebytecode:
	rm -f spatch.opt spatch
	$(MAKE) BYTECODE_STATIC="" spatch
	perl -p -i -e 's/^#!.*/#!\/usr\/bin\/ocamlrun/' spatch

##############################################################################
# Build version information
##############################################################################

version.ml:
	./scripts/genversion.sh > version.ml

##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

docs:
	$(MAKE) -C docs
	if [ -x "$(TARGET)" -o -x "$(TARGET).opt" ]; \
		then $(MAKE) -C ocaml doc; fi

clean::
	$(MAKE) -C docs clean
	$(MAKE) -C ocaml cleandoc

distclean::
	$(MAKE) -C docs distclean

##############################################################################
# Pre-Install (customization of spatch frontend script)
##############################################################################

preinstall: docs/spatch.1 scripts/spatch scripts/spatch.opt scripts/spatch.byte

docs/spatch.1: Makefile.config
	$(MAKE) -C docs spatch.1

# user will use spatch to run spatch.opt (native)
scripts/spatch: Makefile.config scripts/spatch.sh
	cp scripts/spatch.sh scripts/spatch

# user will use spatch to run spatch (bytecode)
scripts/spatch.byte: Makefile.config scripts/spatch.sh
	sed "s|\.opt||" scripts/spatch.sh > scripts/spatch.byte

# user will use spatch.opt to run spatch.opt (native)
scripts/spatch.opt: Makefile.config scripts/spatch.sh
	cp scripts/spatch.sh scripts/spatch.opt

clean::
	rm -f scripts/spatch scripts/spatch.byte scripts/spatch.opt

##############################################################################
# Install
##############################################################################

# don't remove DESTDIR, it can be set by package build system like ebuild
# for staged installation.
install-common:
	$(MKDIR_P) $(DESTDIR)$(BINDIR)
	$(MKDIR_P) $(DESTDIR)$(LIBDIR)
	$(MKDIR_P) $(DESTDIR)$(SHAREDIR)/ocaml
	$(MKDIR_P) $(DESTDIR)$(SHAREDIR)/commons
	$(MKDIR_P) $(DESTDIR)$(SHAREDIR)/globals
	$(MKDIR_P) $(DESTDIR)$(SHAREDIR)/parsing_c
	$(INSTALL_DATA) standard.h $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) standard.iso $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) ocaml/coccilib.cmi $(DESTDIR)$(SHAREDIR)/ocaml/
	$(INSTALL_DATA) parsing_c/*.cmi $(DESTDIR)$(SHAREDIR)/parsing_c/
	$(INSTALL_DATA) commons/*.cmi $(DESTDIR)$(SHAREDIR)/commons/
	$(INSTALL_DATA) globals/iteration.cmi $(DESTDIR)$(SHAREDIR)/globals/

install-man:
	@echo "Installing manuals in: ${DESTDIR}${MANDIR}"
	$(MKDIR_P) $(DESTDIR)$(MANDIR)/man1
	$(MKDIR_P) $(DESTDIR)$(MANDIR)/man3
	$(INSTALL_DATA) docs/spatch.1 $(DESTDIR)$(MANDIR)/man1/
	$(INSTALL_DATA) docs/Coccilib.3cocci $(DESTDIR)$(MANDIR)/man3/

install-bash:
	@echo "Installing bash completion in: ${DESTDIR}${BASH_COMPLETION_DIR}"
	$(MKDIR_P) $(DESTDIR)$(BASH_COMPLETION_DIR)
	$(INSTALL_DATA) scripts/spatch.bash_completion \
		$(DESTDIR)$(BASH_COMPLETION_DIR)/spatch

install-tools:
	@echo "Installing tools in: ${DESTDIR}${BINDIR}"
	$(MKDIR_P) $(DESTDIR)$(BINDIR)
	$(INSTALL_PROGRAM) tools/splitpatch \
		$(DESTDIR)$(BINDIR)/splitpatch
	$(INSTALL_PROGRAM) tools/cocci-send-email.perl \
		$(DESTDIR)$(BINDIR)/cocci-send-email.perl

install-python:
	@echo "Installing python support in: ${DESTDIR}${SHAREDIR}/python"
	$(MKDIR_P) $(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/*.py \
		$(DESTDIR)$(SHAREDIR)/python/coccilib
	$(INSTALL_DATA) python/coccilib/coccigui/*.py \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/coccigui/pygui.glade \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/coccigui/pygui.gladep \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	if [ -n "$LOCAL_pycaml" ]; then \
		$(INSTALL_LIB) external/pycaml/dllpycaml_stubs.so \
			$(DESTDIR)$(SHAREDIR)/python ; fi

install: install-man install-common $(PYTHON_TARGET)
	@if test -x spatch -a ! -x spatch.opt ; then \
		$(MAKE) install-byte;fi
	@if test ! -x spatch -a -x spatch.opt ; then \
		$(MAKE) install-def; $(MAKE) install-opt;fi
	@if test -x spatch -a -x spatch.opt ; then \
		$(MAKE) install-byte; $(MAKE) install-opt;fi
	@if test ! -x spatch -a ! -x spatch.opt ; then \
		echo "\n\n\t==> Run 'make', 'make opt', or both first. <==\n\n";fi
	@echo ""
	@echo "\tYou can also install spatch by copying the program spatch"
	@echo "\t(available in this directory) anywhere you want and"
	@echo "\tgive it the right options to find its configuration files."
	@echo ""

# user will use spatch to run spatch.opt (native)
install-def:
	$(INSTALL_PROGRAM) spatch.opt $(DESTDIR)$(SHAREDIR)
	$(INSTALL_PROGRAM) scripts/spatch $(DESTDIR)$(BINDIR)/spatch

# user will use spatch to run spatch (bytecode)
install-byte:
	$(INSTALL_PROGRAM) spatch $(DESTDIR)$(SHAREDIR)
	$(INSTALL_PROGRAM) scripts/spatch.byte $(DESTDIR)$(BINDIR)/spatch

# user will use spatch.opt to run spatch.opt (native)
install-opt:
	$(INSTALL_PROGRAM) spatch.opt $(DESTDIR)$(SHAREDIR)
	$(INSTALL_PROGRAM) scripts/spatch.opt $(DESTDIR)$(BINDIR)/spatch.opt

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/spatch
	rm -f $(DESTDIR)$(BINDIR)/spatch.opt
	rm -f $(DESTDIR)$(LIBDIR)/dllpycaml_stubs.so
	rm -f $(DESTDIR)$(SHAREDIR)/spatch
	rm -f $(DESTDIR)$(SHAREDIR)/spatch.opt
	rm -f $(DESTDIR)$(SHAREDIR)/standard.h
	rm -f $(DESTDIR)$(SHAREDIR)/standard.iso
	rm -f $(DESTDIR)$(SHAREDIR)/ocaml/coccilib.cmi
	rm -f $(DESTDIR)$(SHAREDIR)/parsing_c/*.cmi
	rm -f $(DESTDIR)$(SHAREDIR)/commons/*.cmi
	rm -f $(DESTDIR)$(SHAREDIR)/globals/*.cmi
	rm -f $(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui/*
	rm -f $(DESTDIR)$(SHAREDIR)/python/coccilib/*.py
	rmdir --ignore-fail-on-non-empty -p \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	rmdir $(DESTDIR)$(SHAREDIR)/globals
	rmdir $(DESTDIR)$(SHAREDIR)/commons
	rmdir $(DESTDIR)$(SHAREDIR)/parsing_c
	rmdir $(DESTDIR)$(SHAREDIR)/ocaml
	rmdir $(DESTDIR)$(SHAREDIR)
	rm -f $(DESTDIR)$(MANDIR)/man1/spatch.1
	rm -f $(DESTDIR)$(MANDIR)/man3/Coccilib.3cocci

uninstall-bash:
	rm -f $(DESTDIR)$(BASH_COMPLETION_DIR)/spatch
	rmdir --ignore-fail-on-non-empty -p \
		$(DESTDIR)$(BASH_COMPLETION_DIR)

uninstall-tools:
	rm -f $(DESTDIR)$(BINDIR)/splitpatch
	rm -f $(DESTDIR)$(BINDIR)/cocci-send-email.perl

version:
	@echo "spatch     $(VERSION)"
	@echo "spatch     $(PKGVERSION) ($(DISTRIB_ID))"
	@echo "coccicheck $(CCVERSION)"


##############################################################################
# Deb package (for Ubuntu) and release rules
##############################################################################

include Makefile.release

##############################################################################
# Developer rules
##############################################################################

-include Makefile.dev

test: $(TARGET)
	./$(TARGET) -testall

testparsing:
	./$(TARGET) -D standard.h -parse_c -dir tests/

check:
	no | $(MAKE) test


# -inline 0  to see all the functions in the profile.
# Can also use the profile framework in commons/ and run your program
# with -profile.
forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt

clean::
	rm -f gmon.out

tags:
	otags -no-mli-tags -r  .

dependencygraph:
	find . -name "*.ml" |grep -v "scripts" | xargs $(OCAMLDEP) -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl09 -I extra > /tmp/dependfull.depend
	ocamldot -lr /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps
	ps2pdf /tmp/dependfull.ps /tmp/dependfull.pdf

##############################################################################
# Misc rules
##############################################################################

# each member of the project can have its own test.ml. this file is
# not under CVS.
test.ml:
	echo "let foo_ctl () = failwith \"there is no foo_ctl formula\"" \
	  > test.ml

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

.ml.mldepend:
	$(OCAMLC_CMD) -i $<

$(LEXER_SOURCES:.mll=.ml): $(LEXER_SOURCES)
	$(OCAMLLEX) $(LEXER_SOURCES)

clean::
	rm -f .depend
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ *.exe #*#

distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f test.ml
	rm -f TAGS
	rm -f tests/SCORE_actual.sexp
	rm -f tests/SCORE_best_of_both.sexp
	find . -name ".#*1.*" | xargs rm -f

.PHONEY: depend
.depend depend: test.ml version
	@echo constructing '.depend'
	$(OCAMLDEP_CMD) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

-include .depend

##############################################################################
# configure-related
##############################################################################

distclean::
	@echo "cleaning configured files"
	rm -f Makefile.config
	rm -rf autom4te.cache
	rm -f config.status
	rm -f config.log
	rm -f version.ml
	rm -f globals/config.ml
	rm -f globals/regexp.ml python/pycocci.ml ocaml/prepare_ocamlcocci.ml
	rm -f scripts/spatch.sh
	@echo "run 'configure' again prior to building coccinelle"

clean::
	@echo "run 'make depend' prior to building after cleaning"
