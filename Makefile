# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

#############################################################################
# Configuration section
#############################################################################

include Makefile.libs

# 'distclean' does not require configure to have run, and should also
# clean all the bundled directories. Hence, a special case.
ifeq ($(MAKECMDGOALS),distclean)
MAKELIBS:=$(dir $(wildcard ./bundles/*/Makefile))
else
ifneq ($(MAKECMDGOALS),configure)
-include Makefile.config
-include Makefile.local
endif
endif

# We inherit the version information *after* the user has run
# ./configure but in the absence of that the build system has
# a few dependencies on the version information, we need a default
# setting then prior to the user running ./configure, this provides
# that, but will only be set if the user hasn't already run ./configure
#
# The only thing we can do is assume that the user relying on this
# variable then was going to make a release, this means we don't
# tell them that the tree is dirty.
VERSION?=$(shell MAKE_COCCI_RELEASE="y" ./version.sh)

CCVERSION=$(shell cat scripts/coccicheck/README | egrep -o '[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+' | head -n1)

##############################################################################
# Variables
##############################################################################

TARGET=spatch
PRJNAME=coccinelle
ML_FILES=flag_cocci.ml cocci.ml testing.ml $(LEXER_SOURCES:.mll=.ml) \
read_options.ml main.ml
MLI_FILES=cocci.mli testing.mli

ifeq ($(FEATURE_PYTHON),1)
	PYTHON_INSTALL_TARGET=install-python
else
	PYTHON_INSTALL_TARGET=
endif

SYSLIBS=str.cma unix.cma bigarray.cma nums.cma
LIBS=commons/commons.cma \
     globals/globals.cma \
     ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/parsing_c.cma \
     ocaml/cocciocaml.cma engine/cocciengine.cma popl09/popl.cma \
     extra/extra.cma python/coccipython.cma

MAKESUBDIRS=commons \
 globals ctl parsing_cocci parsing_c \
 ocaml engine popl09 extra python tools/spgen

CLEANSUBDIRS=commons \
 globals ctl parsing_cocci parsing_c \
 ocaml engine popl09 extra python docs \
 $(MAKELIBS) tools/spgen

INCLUDEDIRSDEP=commons commons/ocamlextra \
 globals ctl \
 parsing_cocci parsing_c ocaml engine popl09 extra python \
 $(MAKELIBS)

INCLUDEDIRS=$(INCLUDEDIRSDEP) $(PCREDIR) $(PARMAPDIR) $(INCLIBS)

##############################################################################
# Generic variables
##############################################################################

# sort to remove duplicates
INCLUDESET=$(sort $(INCLUDEDIRS))
INCLUDES=$(INCLUDESET:%=-I %)

OBJS=    $(ML_FILES:.ml=.cmo)
OPTOBJS= $(ML_FILES:.ml=.cmx)

EXEC=$(TARGET)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLYACC_CMD=$(OCAMLYACC) -v
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDEDIRSDEP:%=-I %)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

# these are unused at the moment (todo: remove)
EXTRA_CFLAGS=   # -static -pie -fpie -fPIE -static-libgcc
EXTRA_OCAML_CFLAGS=$(EXTRA_CFLAGS:%=-ccopt %)

# 'make purebytecode' unsets this definition
BYTECODE_EXTRA=-custom $(EXTRA_OCAML_FLAGS)

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt byte opt top clean distclean opt-compil
.PHONY:: $(MAKELIBS:%=%.all) $(MAKESUBDIRS:%=%.all) $(MAKELIBS:%=%.opt) $(MAKESUBDIRS:%=%.opt) subdirs.all subdirs.opt
.PHONY:: byte-only opt-only pure-byte tools
.PHONY:: copy-stubs install-stubs install install-man install-python install-common

# All make targets that are expected to be an entry point have a dependency on
# 'Makefile.config' to ensure that if Makefile.config is not present, an error
# message is printed first before any other actions are executed.
# In addition, the targets that actually build something have a dependency on
# '.depend' and 'version.ml'.

# dispatches to either 'all-dev' or 'all-release'
all: Makefile.config
	@$(MAKE) .depend
	$(MAKE) $(TARGET_ALL)

# make "all" comes in three flavours
world: Makefile.config version.ml
	@$(ECHO) "Building both versions of spatch"
	$(MAKE) .depend
	$(MAKE) byte
	$(MAKE) opt-compil
	$(MAKE) preinstall
	$(MAKE) docs
	@$(ECHO) -e "\n\tcoccinelle can now be installed via 'make install'"

# note: the 'all-dev' target excludes the documentation
all-dev: Makefile.config version.ml
	@$(MAKE) .depend
	@$(ECHO) "Building $(TARGET_SPATCH)"
	$(MAKE) $(TARGET_SPATCH)
	@$(MAKE) preinstall
	@$(ECHO) -e "\n\tcoccinelle can now be installed via 'make install'"

all-release: Makefile.config version.ml
	@$(ECHO) "Building $(TARGET_SPATCH)"
	$(MAKE) .depend
	$(MAKE) $(TARGET_SPATCH)
	$(MAKE) preinstall
	$(MAKE) docs
	@$(ECHO) -e "\n\tcoccinelle can now be installed via 'make install'"

all.opt: Makefile.config
	@$(MAKE) .depend
	$(MAKE) opt-only
	$(MAKE) preinstall

byte: Makefile.config version.ml
	@$(MAKE) .depend
	@$(MAKE) subdirs.all
	@$(MAKE) $(EXEC)
	@$(ECHO) "The compilation of $(EXEC) finished."
	@$(ECHO) "$(EXEC) can be installed or used."

opt-compil: Makefile.config version.ml
	$(MAKE) .depend
	$(MAKE) subdirs.opt BUILD_OPT=yes
	$(MAKE) $(EXEC).opt BUILD_OPT=yes
	@$(ECHO) "The compilation of $(EXEC).opt finished."
	@$(ECHO) "$(EXEC).opt can be installed or used."

top: $(EXEC).top

# the .cmi file of coccilib
# aliases for "byte" and "opt-compil"
opt opt-only: Makefile.config opt-compil
byte-only: Makefile.config byte

subdirs.all:
	@+for D in $(MAKELIBS) $(MAKESUBDIRS); do $(MAKE) $$D.all || exit 1 ; done

subdirs.opt:
	@+for D in $(MAKELIBS) $(MAKESUBDIRS); do $(MAKE) $$D.opt || exit 1 ; done

$(MAKELIBS:%=%.all) $(MAKESUBDIRS:%=%.all):
	@$(MAKE) -C $(@:%.all=%) all

$(MAKELIBS:%=%.opt) $(MAKESUBDIRS:%=%.opt):
	@$(MAKE) -C $(@:%.opt=%) all.opt

# This make target prepares the bundled software for building.
# Note that running 'make' in these subdirectories will
# automatically prepare the bundled software.
.PHONY:: prepare-bundles
prepare-bundles: $(MAKELIBS:%=%/.prepare)

$(MAKELIBS:%=%/.prepare):
	$(ECHO) $@
	@$(MAKE) -C $(@:%.prepare=%) .prepare

#dependencies:
# commons:
# globals:
# menhirLib:
# parsing_cocci: commons globals menhirLib
# parsing_c:parsing_cocci
# ctl:globals commonsg
# engine: parsing_cocci parsing_c ctl
# popl09:engine
# extra: parsing_cocci parsing_c ctl
# pycaml:
# python:pycaml parsing_cocci parsing_c

clean:: Makefile.config
	@set -e; for i in $(CLEANSUBDIRS); do $(MAKE) -C $$i $@; done
	@$(MAKE) -C demos/spp $@

$(LIBS): $(MAKELIBS:%=%.all) $(MAKESUBDIRS:%=%.all)
$(LIBS:.cma=.cmxa): $(MAKELIBS:%=%.opt) $(MAKESUBDIRS:%=%.opt)
$(LNKLIBS) : $(MAKELIBS:%=%.all) $(MAKESUBDIRS:%=%.all)
$(LNKOPTLIBS) : $(MAKELIBS:%=%.opt) $(MAKESUBDIRS:%=%.opt)

$(OBJS):$(LIBS)
$(OPTOBJS):$(LIBS:.cma=.cmxa)

$(EXEC): $(LNKLIBS) $(LIBS) $(OBJS)
	$(OCAMLC_CMD) $(BYTECODE_EXTRA) $(FLAGSLIBS) -o $@ $(SYSLIBS) $^

$(EXEC).opt: $(OPTLNKLIBS) $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT_CMD) $(OPTFLAGSLIBS) -o $@ $(SYSLIBS:.cma=.cmxa) $^

$(EXEC).top: $(LNKLIBS) $(LIBS) $(OBJS)
	$(OCAMLMKTOP_CMD) -custom -o $@ $(SYSLIBS) $(FLAGSLIBS) $^

clean distclean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

./configure: ./configure.ac
	@echo Please run ./autogen to update configure
	@false

# the dependencies on Makefile.config should give a hint to the programmer that
# configure should be run again
Makefile.config: Makefile.config.in configure.ac
	@$(ECHO) "Makefile.config needs to be (re)build. Run  ./autogen and ./configure $(CONFIGURE_FLAGS) to generate it."
	@false

tools: $(LIBS) $(LNKLIBS)
	$(MAKE) -C tools

distclean::
	@if [ -d tools ] ; then $(MAKE) -C tools distclean ; fi

# it seems impossible to pass "-static" unless all dependent
# libraries are also available as static archives.
# set $(STATIC) to -static if you have such libraries.
static:
	rm -f spatch.opt spatch
	$(MAKE) $(STATIC) opt-only
	cp spatch.opt spatch

# creates a portable version of spatch, which, however, may
# be dependent on non-portably dynamic libraries. You
# may need the stubs, see 'copy-stubs'.
purebytecode:
	rm -f spatch.opt spatch
	$(MAKE) BYTECODE_EXTRA="" byte-only
	sed -i '1 s,^#!.*$$,#!/usr/bin/ocamlrun,g' spatch

# copies the stubs libraries (if any) to the root directory
copy-stubs:
	@if test -f ./bundles/pycaml/dllpycaml_stubs.so; then \
		cp -fv ./bundles/pycaml/dllpycaml_stubs.so .; fi
	@if test -f ./bundles/pcre/dllpcre_stubs.so; then \
		cp -fv ./bundles/pcre/dllpcre_stubs.so .; fi

##############################################################################
# Build version information
##############################################################################

version.ml:
	@$(ECHO) "version.ml is missing. Run ./configure to generate it. Run ./autogen first if ./configure does not exist."
	@false

##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

docs:
	@$(MAKE) -C docs || ($(ECHO) "Warning: ignored the failed construction of the manual" 1>&2)
	@$(MAKE) docs -C tools/spgen/documentation
	@if test "x$(FEATURE_OCAML)" = x1; then \
		if test -f ./parsing_c/ast_c.cmo -o -f ./parsing_c/ast_c.cmx; then \
			$(MAKE) -C ocaml doc; \
		else $(ECHO) "Note: to obtain coccilib documenation, it is required to build 'spatch' first so that ./parsing_c/ast_c.cm* gets build."; \
		fi fi
	@$(ECHO) "Finished building manuals"

clean:: Makefile.config
#	$(MAKE) -C docs clean
	$(MAKE) -C ocaml cleandoc
	$(MAKE) clean -C tools/spgen/documentation

##############################################################################
# Pre-Install (customization of spatch frontend script)
##############################################################################

preinstall: docs/spatch.1 scripts/spatch scripts/spatch.opt scripts/spatch.byte

docs/spatch.1: Makefile.config
	$(MAKE) -C docs spatch.1

# user will use spatch to run spatch.opt (native)
scripts/spatch: Makefile.config scripts/spatch.sh
	cp scripts/spatch.sh scripts/spatch
	chmod +x scripts/spatch

# user will use spatch to run spatch (bytecode)
scripts/spatch.byte: Makefile.config scripts/spatch.sh
	cp scripts/spatch.sh scripts/spatch.byte
	chmod +x scripts/spatch.byte

# user will use spatch.opt to run spatch.opt (native)
scripts/spatch.opt: Makefile.config scripts/spatch.sh
	cp scripts/spatch.sh scripts/spatch.opt
	chmod +x scripts/spatch.opt

distclean::
	rm -f scripts/spatch scripts/spatch.byte scripts/spatch.opt

##############################################################################
# Install
##############################################################################

# don't remove DESTDIR, it can be set by package build system like ebuild
# for staged installation.
install-common: ocaml/coccilib.cmi
	$(MKDIR_P) $(DESTDIR)$(BINDIR)
	$(MKDIR_P) $(DESTDIR)$(LIBDIR)
	$(MKDIR_P) $(DESTDIR)$(LIBDIR)/ocaml
	$(INSTALL_DATA) standard.h $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) standard.iso $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) ocaml/*.cmi $(DESTDIR)$(LIBDIR)/ocaml/

install-man:
	@$(ECHO) "Installing manuals in: ${DESTDIR}${MANDIR}"
	$(MKDIR_P) $(DESTDIR)$(MANDIR)/man1
	$(MKDIR_P) $(DESTDIR)$(MANDIR)/man3
	$(INSTALL_DATA) docs/spatch.1 $(DESTDIR)$(MANDIR)/man1/
	$(INSTALL_DATA) docs/pycocci.1 $(DESTDIR)$(MANDIR)/man1/
	$(INSTALL_DATA) docs/spgen.1 $(DESTDIR)$(MANDIR)/man1/
	$(INSTALL_DATA) docs/Coccilib.3cocci $(DESTDIR)$(MANDIR)/man3/

install-bash:
	@$(ECHO) "Installing bash completion in: ${DESTDIR}${BASH_COMPLETION_DIR}"
	$(MKDIR_P) $(DESTDIR)$(BASH_COMPLETION_DIR)
	$(INSTALL_DATA) scripts/spatch.bash_completion \
		$(DESTDIR)$(BASH_COMPLETION_DIR)/spatch

install-tools:
	@$(ECHO) "Installing tools in: ${DESTDIR}${BINDIR}"
	$(MKDIR_P) $(DESTDIR)$(BINDIR)
	$(INSTALL_PROGRAM) tools/splitpatch \
		$(DESTDIR)$(BINDIR)/splitpatch
	$(INSTALL_PROGRAM) tools/cocci-send-email.perl \
		$(DESTDIR)$(BINDIR)/cocci-send-email.perl

install-python:
	@$(ECHO) "Installing python support in: ${DESTDIR}${LIBDIR}/python"
	$(MKDIR_P) $(DESTDIR)$(LIBDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/*.py \
		$(DESTDIR)$(LIBDIR)/python/coccilib
	$(INSTALL_DATA) python/coccilib/coccigui/*.py \
		$(DESTDIR)$(LIBDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/coccigui/pygui.glade \
		$(DESTDIR)$(LIBDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/coccigui/pygui.gladep \
		$(DESTDIR)$(LIBDIR)/python/coccilib/coccigui

install-stubs:
	$(MKDIR_P) $(DESTDIR)$(LIBDIR)
	@if test -f ./bundles/pycaml/dllpycaml_stubs.so; then \
		cp -fv ./bundles/pycaml/dllpycaml_stubs.so $(DESTDIR)$(LIBDIR); fi
	@if test -f ./bundles/pcre/dllpcre_stubs.so; then \
		cp -fv ./bundles/pcre/dllpcre_stubs.so $(DESTDIR)$(LIBDIR); fi

install: install-common install-man install-stubs $(PYTHON_INSTALL_TARGET)
	rm -f $(DESTDIR)$(LIBDIR)/spatch
	rm -f $(DESTDIR)$(LIBDIR)/spatch.opt
	rm -f $(DESTDIR)$(BINDIR)/pycocci
	@$(MAKE) install -C tools/spgen
	$(INSTALL_PROGRAM) tools/pycocci $(DESTDIR)$(BINDIR)
	@if test -x spatch -o -x spatch.opt; then \
		$(MAKE) install-def;fi
	@if test -x spatch ; then \
		$(MAKE) install-byte; fi
	@if test -x spatch.opt ; then \
		$(MAKE) install-opt;fi
	@if test ! -x spatch -a ! -x spatch.opt ; then \
		$(ECHO) -e "\n\n\t==> Run 'make', 'make opt', or both first. <==\n\n";fi
	@$(ECHO) -e "\n\tYou can also install spatch by copying the program spatch"
	@$(ECHO) -e "\t(available in this directory) anywhere you want and"
	@$(ECHO) -e "\tgive it the right options to find its configuration files.\n"

#
# Installation of spatch and spatch.opt and their wrappers
#

# user will use spatch to run one of the binaries
install-def:
	$(INSTALL_PROGRAM) scripts/spatch $(DESTDIR)$(BINDIR)/spatch

# user will use spatch.byte to run spatch (bytecode)
install-byte:
	$(INSTALL_PROGRAM) spatch $(DESTDIR)$(LIBDIR)
	$(INSTALL_PROGRAM) scripts/spatch.byte $(DESTDIR)$(BINDIR)/spatch.byte

# user will use spatch.opt to run spatch.opt (native)
install-opt:
	$(INSTALL_PROGRAM) spatch.opt $(DESTDIR)$(LIBDIR)
	$(INSTALL_PROGRAM) scripts/spatch.opt $(DESTDIR)$(BINDIR)/spatch.opt

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/spatch
	rm -f $(DESTDIR)$(BINDIR)/spatch.opt
	rm -f $(DESTDIR)$(BINDIR)/spatch.byte
	rm -f $(DESTDIR)$(LIBDIR)/dllpycaml_stubs.so
	rm -f $(DESTDIR)$(LIBDIR)/dllpcre_stubs.so
	rm -f $(DESTDIR)$(LIBDIR)/spatch
	rm -f $(DESTDIR)$(LIBDIR)/spatch.opt
	rm -f $(DESTDIR)$(LIBDIR)/standard.h
	rm -f $(DESTDIR)$(LIBDIR)/standard.iso
	rm -f $(DESTDIR)$(LIBDIR)/ocaml/*.cmi
	rm -f $(DESTDIR)$(LIBDIR)/python/coccilib/coccigui/*
	rm -f $(DESTDIR)$(LIBDIR)/python/coccilib/*.py
	@$(MAKE) uninstall -C tools/spgen/source
	rmdir --ignore-fail-on-non-empty -p \
		$(DESTDIR)$(LIBDIR)/python/coccilib/coccigui
	rmdir --ignore-fail-on-non-empty $(DESTDIR)$(LIBDIR)/ocaml
	rmdir $(DESTDIR)$(LIBDIR)
	rm -f $(DESTDIR)$(MANDIR)/man1/spatch.1
	rm -f $(DESTDIR)$(MANDIR)/man3/Coccilib.3cocci
	rm -f $(DESTDIR)$(MANDIR)/man1/spgen.1

uninstall-bash:
	rm -f $(DESTDIR)$(BASH_COMPLETION_DIR)/spatch
	rmdir --ignore-fail-on-non-empty -p \
		$(DESTDIR)$(BASH_COMPLETION_DIR)

uninstall-tools:
	rm -f $(DESTDIR)$(BINDIR)/splitpatch
	rm -f $(DESTDIR)$(BINDIR)/cocci-send-email.perl

version:
	@$(ECHO) "spatch     $(VERSION)"
	@$(ECHO) "coccicheck $(CCVERSION)"


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

# the check target runs:
# * some feature tests (depending on what is enabled)
# * the test suite
check: scripts/spatch
	@$(ECHO) "Testing if spatch works on hello world..."
	@COCCINELLE_HOME="$$(pwd)" ./scripts/spatch --sp-file demos/hello/hello-smpl.cocci demos/hello/helloworld.c --very-quiet | grep -q '+  printf("world, hello!");'
	@$(ECHO) "Testing if spatch works with regexes..."
	@COCCINELLE_HOME="$$(pwd)" ./scripts/spatch --sp-file demos/hello/hello-regexp.cocci demos/hello/helloworld.c --very-quiet | grep -q '+  printf("world, hello!");'
	@if test "x${FEATURE_OCAML}" = x1 -a -z "${NO_OCAMLFIND}"; then \
		$(ECHO) "Testing if spatch works with ocaml scripts..."; \
		COCCINELLE_HOME="$$(pwd)" ./scripts/spatch --sp-file demos/hello/hello-ocaml.cocci demos/hello/helloworld.c --very-quiet | grep -q 'Hello at: 2'; fi
	@if test "x${FEATURE_PYTHON}" = x1; then \
		$(ECHO) "Testing if spatch works with python scripts..."; \
		COCCINELLE_HOME="$$(pwd)" ./scripts/spatch --sp-file demos/hello/hello-python.cocci demos/hello/helloworld.c --very-quiet | grep -q 'Hello at: 2'; fi
	@$(ECHO) running the test suite
	COCCINELLE_HOME="$$(pwd)" ./scripts/spatch --testall --no-update-score-file

pycocci-check:
	COCCINELLE_HOME="$$(pwd)" ./scripts/pycocci-check.sh

# -inline 0  to see all the functions in the profile.
# Can also use the profile framework in commons/ and run your program
# with -profile.
forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 ${EXTRA_OCAML_FLAGS}" opt

clean distclean::
	rm -f gmon.out ocamlprof.dump

tags:
	otags -no-mli-tags -r  .

##############################################################################
# Misc rules
##############################################################################

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

clean distclean::
	rm -f .depend
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ *.exe #*#

distclean::
	set -e; for i in $(CLEANSUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f TAGS *.native *.byte *.d.native *.p.byte
	if test -z "${KEEP_GENERATED}"; then \
		rm -f tests/SCORE_actual.sexp tests/SCORE_best_of_both.sexp; fi
	find . -name ".#*1.*" | xargs rm -f
	rm -f $(EXEC) $(EXEC).opt $(EXEC).top
	rm -f setup/Makefile

# using 'touch' to prevent infinite recursion with 'make depend'
.PHONY:: depend
.depend: Makefile.config version
	@touch .depend
	@$(MAKE) depend

depend: Makefile.config version
	@$(ECHO) "Constructing '.depend'"
	@rm -f .depend
	set -e; for i in $(MAKELIBS) $(MAKESUBDIRS); do echo $$i; $(MAKE) -C $$i depend; done
	$(OCAMLDEP_CMD) $(MLI_FILES) $(ML_FILES) > .depend

##############################################################################
# configure-related
##############################################################################

distclean::
	@echo "Cleaning configured files"
	if test -z "${KEEP_CONFIG}"; then rm -f Makefile.config; fi
	rm -rf autom4te.cache
	rm -f config.status
	rm -f config.log
	if test -z "${KEEP_GENERATED}"; then \
		rm -f version.ml; fi
	rm -f globals/config.ml
	rm -f globals/regexp.ml python/pycocci.ml ocaml/prepare_ocamlcocci.ml
	rm -f scripts/spatch.sh
	rm -f aclocal.m4
	for i in `find . -name '*.in'`; do rm -f `echo $$i | sed "s/\.in$$//"`; done
	@echo "Run './configure' again prior to building coccinelle."
	@echo "If ./configure does not exist, run ./autogen first."


# don't include depend for those actions that either don't need
# depend or that call 'make .depend' explicitly.
# TODO: find a nicer way to express this
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),configure)
ifneq ($(MAKECMDGOALS),prerelease)
ifneq ($(MAKECMDGOALS),release)
ifneq ($(MAKECMDGOALS),package)
ifneq ($(MAKECMDGOALS),all-release)
ifneq ($(MAKECMDGOALS),all-dev)
ifneq ($(MAKECMDGOALS),all)
ifneq ($(MAKECMDGOALS),.depend)
ifneq ($(MAKECMDGOALS),depend)
-include .depend
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif

include Makefile.common
