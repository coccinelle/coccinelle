
#############################################################################
# Configuration section
#############################################################################

-include Makefile.config
-include /etc/lsb-release

VERSION=$(shell cat globals/config.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')
CCVERSION=$(shell cat scripts/coccicheck/README |grep "Coccicheck version" |perl -p -e 's/.*version (.*)[ ]*/$$1/;')
PKGVERSION=$(shell dpkg-parsechangelog -ldebian/changelog.$(DISTRIB_CODENAME) 2> /dev/null \
	 | sed -n 's/^Version: \(.*\)/\1/p' )

##############################################################################
# Variables
##############################################################################
TARGET=spatch
PRJNAME=coccinelle

SRC=flag_cocci.ml cocci.ml testing.ml test.ml main.ml

ifeq ($(FEATURE_PYTHON),1)
PYCMA=pycaml.cma
# the following is essential for Coccinelle to compile under gentoo (weird)
#OPTLIBFLAGS=-cclib dllpycaml_stubs.so
else
PYCMA=
endif
OPTLIBFLAGS=

ifeq ("$(SEXPDIR)","ocamlsexp")
SEXPLIB=sexplib.cmo
OPTSEXPLIB=sexplib.cmx
else
SEXPLIB=sexplib.cma
OPTSEXPLIB=sexplib.cmxa
endif

SEXPSYSCMA=bigarray.cma nums.cma

SYSLIBS=str.cma unix.cma $(SEXPSYSCMA) $(PYCMA) dynlink.cma # threads.cma
LIBS=commons/commons.cma \
     commons/commons_sexp.cma \
     globals/globals.cma \
     ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/parsing_c.cma \
     engine/cocciengine.cma popl09/popl.cma \
     extra/extra.cma python/coccipython.cma ocaml/cocciocaml.cma

# Should we use the local version of pycaml
ifeq ("$(PYCAMLDIR)","pycaml")
LOCALPYCAML=pycaml
else
LOCALPYCAML=
endif

# Should we use the local version of menhirLib
ifeq ("$(MENHIRDIR)","menhirlib")
LOCALMENHIR=menhirlib
else
LOCALMENHIR=
endif

# Should we use the local version of ocamlsexp
ifeq ("$(SEXPDIR)","ocamlsexp")
LOCALSEXP=ocamlsexp
else
LOCALSEXP=
endif

# used for depend: and a little for rec & rec.opt
MAKESUBDIRS=$(LOCALPYCAML) $(LOCALSEXP) commons \
 globals $(LOCALMENHIR) ctl parsing_cocci parsing_c \
 engine popl09 extra python ocaml

# used for clean:
# It is like MAKESUBDIRS but also
# force cleaning of local library copies
CLEANSUBDIRS=pycaml ocamlsexp commons \
 globals menhirlib ctl parsing_cocci parsing_c \
 engine popl09 extra python ocaml

INCLUDEDIRSDEP=commons commons/ocamlextra $(LOCALSEXP) \
 globals $(LOCALMENHIR) $(LOCALPYCAML) ctl \
 parsing_cocci parsing_c engine popl09 extra python ocaml

INCLUDEDIRS=$(INCLUDEDIRSDEP) $(SEXPDIR) $(MENHIRDIR) $(PYCAMLDIR)

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

OCAMLCFLAGS= -g -dtypes # -w A

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
# To enable backtrace support for native code, you need to put -g in OPTFLAGS
# to also link with -g, but even in 3.11 the backtrace support seems buggy so
# not worth it.
OPTFLAGS=

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDEDIRSDEP:%=-I %)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

# can also be set via 'make static'
STATIC= #-ccopt -static

# can also be unset via 'make purebytecode'
BYTECODE_STATIC=-custom

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt byte opt top clean distclean configure
.PHONY:: $(MAKESUBDIRS) $(MAKESUBDIRS:%=%.opt) subdirs subdirs.opt

all: Makefile.config byte preinstall

opt: all.opt
all.opt: opt-compil preinstall

world: preinstall
	$(MAKE) byte
	$(MAKE) opt-compil

byte: .depend
	$(MAKE) subdirs
	$(MAKE) $(EXEC)

opt-compil: .depend
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
# menhirlib:
# parsing_cocci: commons globals menhirlib
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

$(OBJS):$(LIBS)
$(OPTOBJS):$(LIBS:.cma=.cmxa)

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $(SEXPLIB) $^

$(EXEC).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $(OPTSEXPLIB) $(OPTLIBFLAGS)  $^

$(EXEC).top: $(LIBS) $(OBJS)
	$(OCAMLMKTOP) -custom -o $@ $(SYSLIBS) $(SEXPLIB) $^

clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

.PHONY:: tools configure

configure:
	./configure

Makefile.config:
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1

tools:
	$(MAKE) -C tools

clean::
	if [ -d tools ] ; then $(MAKE) -C tools clean ; fi

static:
	rm -f spatch.opt spatch
	$(MAKE) STATIC="-ccopt -static" spatch.opt
	cp spatch.opt spatch

purebytecode:
	rm -f spatch.opt spatch
	$(MAKE) BYTECODE_STATIC="" spatch
	perl -p -i -e 's/^#!.*/#!\/usr\/bin\/ocamlrun/' spatch


##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

docs:
	make -C docs

clean::
	make -C docs clean

distclean::
	make -C docs distclean

##############################################################################
# Pre-Install (customization of spatch frontend script)
##############################################################################

preinstall: docs/spatch.1 scripts/spatch scripts/spatch.opt scripts/spatch.byte

docs/spatch.1: Makefile.config
	$(MAKE) -C docs spatch.1

# user will use spatch to run spatch.opt (native)
scripts/spatch: Makefile.config
	cp scripts/spatch.sh scripts/spatch.tmp2
	sed "s|SHAREDIR|$(SHAREDIR)|g" scripts/spatch.tmp2 > scripts/spatch.tmp
	sed "s|LIBDIR|$(LIBDIR)|g" scripts/spatch.tmp > scripts/spatch
	rm -f scripts/spatch.tmp2 scripts/spatch.tmp

# user will use spatch to run spatch (bytecode)
scripts/spatch.byte: Makefile.config
	cp scripts/spatch.sh scripts/spatch.byte.tmp3
	sed "s|\.opt||" scripts/spatch.byte.tmp3 > scripts/spatch.byte.tmp2
	sed "s|SHAREDIR|$(SHAREDIR)|g" scripts/spatch.byte.tmp2 \
		> scripts/spatch.byte.tmp
	sed "s|LIBDIR|$(LIBDIR)|g" scripts/spatch.byte.tmp \
		> scripts/spatch.byte
	rm -f   scripts/spatch.byte.tmp3 \
		scripts/spatch.byte.tmp2 \
		scripts/spatch.byte.tmp

# user will use spatch.opt to run spatch.opt (native)
scripts/spatch.opt: Makefile.config
	cp scripts/spatch.sh scripts/spatch.opt.tmp2
	sed "s|SHAREDIR|$(SHAREDIR)|g" scripts/spatch.opt.tmp2 \
		> scripts/spatch.opt.tmp
	sed "s|LIBDIR|$(LIBDIR)|g" scripts/spatch.opt.tmp \
		> scripts/spatch.opt
	rm -f scripts/spatch.opt.tmp scripts/spatch.opt.tmp2

clean::
	rm -f scripts/spatch scripts/spatch.byte scripts/spatch.opt

##############################################################################
# Install
##############################################################################

# don't remove DESTDIR, it can be set by package build system like ebuild
# for staged installation.
install-common:
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(LIBDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)/ocaml
	mkdir -p $(DESTDIR)$(MANDIR)/man1
	$(INSTALL_DATA) standard.h $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) standard.iso $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) ocaml/coccilib.cmi $(DESTDIR)$(SHAREDIR)/ocaml/
	$(INSTALL_DATA) docs/spatch.1 $(DESTDIR)$(MANDIR)/man1/
	@if [ $(FEATURE_PYTHON) -eq 1 ]; then $(MAKE) install-python; fi
	@if [ -d $(BASH_COMPLETION_DIR) ]; then $(MAKE) install-bash; fi

install-bash:
	mkdir -p $(DESTDIR)$(BASH_COMPLETION_DIR)
	$(INSTALL_DATA) scripts/spatch.bash_completion \
		$(DESTDIR)$(BASH_COMPLETION_DIR)/spatch

install-python:
	mkdir -p $(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/*.py \
		$(DESTDIR)$(SHAREDIR)/python/coccilib
	$(INSTALL_DATA) python/coccilib/coccigui/*.py \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/coccigui/pygui.glade \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	$(INSTALL_DATA) python/coccilib/coccigui/pygui.gladep \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	if [ -f pycaml/dllpycaml_stubs.so ]; then \
		$(INSTALL_LIB) pycaml/dllpycaml_stubs.so $(DESTDIR)$(LIBDIR) ; fi

install: install-common
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
	rm -f $(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui/*
	rm -f $(DESTDIR)$(SHAREDIR)/python/coccilib/*.py
	rmdir --ignore-fail-on-non-empty -p \
		$(DESTDIR)$(SHAREDIR)/python/coccilib/coccigui
	rm -f $(DESTDIR)$(MANDIR)/man1/spatch.1

uninstall-bash:
	rm -f $(DESTDIR)$(BASH_COMPLETION_DIR)/spatch
	rmdir --ignore-fail-on-non-empty -p \
		$(DESTDIR)$(BASH_COMPLETION_DIR)

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
	find  -name "*.ml" |grep -v "scripts" | xargs ocamldep -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl09 -I extra > /tmp/dependfull.depend
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

beforedepend:: test.ml


#INC=$(dir $(shell which ocaml))
#INCX=$(INC:/=)
#INCY=$(dir $(INCX))
#INCZ=$(INCY:/=)/lib/ocaml
#
#prim.o: prim.c
#	gcc -c -o prim.o -I $(INCZ) prim.c


##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)    -c $<
.mli.cmi:
	$(OCAMLC)    -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[iox] *.o *.annot
	rm -f *~ .*~ *.exe #*#

distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f python/pycocci.ml
	rm -f python/pycocci_aux.ml
	rm -f globals/config.ml
	rm -f TAGS
	rm -f tests/SCORE_actual.sexp
	rm -f tests/SCORE_best_of_both.sexp
	find -name ".#*1.*" | xargs rm -f

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done

.depend::
	@if [ ! -f .depend ] ; then $(MAKE) depend ; fi

-include .depend
