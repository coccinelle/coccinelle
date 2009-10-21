#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

VERSION=$(shell cat globals/config.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')
CCVERSION=$(shell cat scripts/coccicheck/README |grep "Coccicheck version" |perl -p -e 's/.*version (.*)[ ]*/$$1/;')

##############################################################################
# Variables
##############################################################################
TARGET=spatch

SRC=flag_cocci.ml cocci.ml testing.ml test.ml main.ml

ifeq ($(FEATURE_PYTHON),1)
PYCMA=pycaml/pycaml.cma
PYDIR=pycaml
PYLIB=dllpycaml_stubs.so
# the following is essential for Coccinelle to compile under gentoo (weird)
OPTLIBFLAGS=-cclib dllpycaml_stubs.so
else
PYCMA=
PYDIR=
PYLIB=
OPTLIBFLAGS=
endif

SEXPSYSCMA=bigarray.cma nums.cma

SYSLIBS=str.cma unix.cma $(SEXPSYSCMA)
LIBS=commons/commons.cma \
     ocamlsexp/sexplib1.cma commons/commons_sexp.cma \
     globals/globals.cma \
     ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/parsing_c.cma \
     engine/cocciengine.cma popl09/popl.cma \
     extra/extra.cma $(PYCMA) python/coccipython.cma

#used for clean: and depend: and a little for rec & rec.opt
MAKESUBDIRS=commons ocamlsexp \
 globals menhirlib $(PYDIR) ctl parsing_cocci parsing_c \
 engine popl09 extra python
INCLUDEDIRS=commons commons/ocamlextra ocamlsexp \
 globals menhirlib $(PYDIR) ctl \
 parsing_cocci parsing_c engine popl09 extra python

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
# the following is essential for Coccinelle to compile under gentoo
# but is now defined above in this file
#OPTLIBFLAGS=-cclib dllpycaml_stubs.so

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
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
	$(MAKE) -C commons OCAMLCFLAGS="$(OCAMLCFLAGS)"
	$(MAKE) -C ocamlsexp OCAMLCFLAGS="$(OCAMLCFLAGS)"
	$(MAKE) -C commons sexp OCAMLCFLAGS="$(OCAMLCFLAGS)"
	+for D in $(MAKESUBDIRS); do $(MAKE) $$D || exit 1 ; done

subdirs.opt:
	$(MAKE) -C commons all.opt OCAMLCFLAGS="$(OCAMLCFLAGS)"
	$(MAKE) -C ocamlsexp all.opt OCAMLCFLAGS="$(OCAMLCFLAGS)"
	$(MAKE) -C commons sexp.opt OCAMLCFLAGS="$(OCAMLCFLAGS)"
	+for D in $(MAKESUBDIRS); do $(MAKE) $$D.opt || exit 1 ; done

$(MAKESUBDIRS):
	$(MAKE) -C $@ OCAMLCFLAGS="$(OCAMLCFLAGS)" all

$(MAKESUBDIRS:%=%.opt):
	$(MAKE) -C $(@:%.opt=%) OCAMLCFLAGS="$(OCAMLCFLAGS)" all.opt

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
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done

$(LIBS): $(MAKESUBDIRS)
$(LIBS:.cma=.cmxa): $(MAKESUBDIRS:%=%.opt)

$(OBJS):$(LIBS)
$(OPTOBJS):$(LIBS:.cma=.cmxa)

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS)  $^

$(EXEC).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS)
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $(OPTLIBFLAGS)  $^

$(EXEC).top: $(LIBS) $(OBJS)
	$(OCAMLMKTOP) -custom -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

clean::
	rm -f dllpycaml_stubs.so


.PHONY:: tools configure

configure:
	./configure

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1

tools:
	$(MAKE) -C tools

clean::
	$(MAKE) -C tools clean

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

preinstall: scripts/spatch scripts/spatch.opt scripts/spatch.byte

# user will use spatch to run spatch.opt (native)
scripts/spatch:
	cp scripts/spatch.sh scripts/spatch.tmp2
	sed "s|SHAREDIR|$(SHAREDIR)|g" scripts/spatch.tmp2 > scripts/spatch.tmp
	sed "s|LIBDIR|$(LIBDIR)|g" scripts/spatch.tmp > scripts/spatch
	rm -f scripts/spatch.tmp2 scripts/spatch.tmp

# user will use spatch to run spatch (bytecode)
scripts/spatch.byte:
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
scripts/spatch.opt:
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
	mkdir -p $(DESTDIR)$(SHAREDIR)
	mkdir -p $(DESTDIR)$(MANDIR)/man1
	$(INSTALL_DATA) standard.h $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) standard.iso $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) docs/spatch.1 $(DESTDIR)$(MANDIR)/man1/
	@if [ $(FEATURE_PYTHON) -eq 1 ]; then $(MAKE) install-python; fi

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
	$(INSTALL_LIB) dllpycaml_stubs.so $(DESTDIR)$(LIBDIR)

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
	rm -f $(DESTDIR)$(SHAREDIR)/standard.h
	rm -f $(DESTDIR)$(SHAREDIR)/standard.iso
	rm -rf $(DESTDIR)$(SHAREDIR)/python/coccilib
	rm -f $(DESTDIR)$(MANDIR)/man1/spatch.1

version:
	@echo "spatch     $(VERSION)"
	@echo "coccicheck $(CCVERSION)"


##############################################################################
# Package rules
##############################################################################

PACKAGE=coccinelle-$(VERSION)
CCPACKAGE=coccicheck-$(CCVERSION)

BINSRC=spatch env.sh env.csh standard.h standard.iso \
       *.txt \
       docs/manual/manual.pdf docs/manual/options.pdf docs/manual/main_grammar.pdf docs/spatch.1 \
       docs/manual/cocci-python.txt \
       demos/foo.* demos/simple.*
#      $(PYLIB) python/coccilib/ demos/printloc.*
BINSRC2=$(BINSRC:%=$(PACKAGE)/%)

TMP=/tmp
OCAMLVERSION=$(shell ocaml -version |perl -p -e 's/.*version (.*)/$$1/;')

# Procedure to do first time:
#  cd ~/release
#  cvs checkout coccinelle
#  cd coccinelle
#  cvs update -d -P
#  touch **/*
#  make licensify
#  remember to comment the -g -dtypes in this Makefile
#  You can also remove a few things, for instance I removed in this
#   Makefile things related to popl/ and popl09/

# Procedure to do each time:
#  cvs update
#  make sure that ocaml is the distribution ocaml of /usr/bin, not ~pad/...
#  modify globals/config.ml.in
#  cd globals/; cvs commit -m"new version"  (do not commit from the root!)
#  ./configure --without-python
#  can make; ./spatch -testall to check and update the SCORE_expected.sexp
#  make package
#  make website
# Check that run an ocaml in /usr/bin

# To test you can try compile and run spatch from different instances
# like my ~/coccinelle, ~/release/coccinelle, and the /tmp/coccinelle-0.X
# downloaded from the website.

# For 'make srctar' it must done from a clean
# repo such as ~/release/coccinelle. It must also be a repo where
# the scripts/licensify has been run at least once.
# For the 'make bintar' I can do it from my original repo.


package:
	$(MAKE) srctar
	./configure --without-python
	$(MAKE) docs
	$(MAKE) bintar
	$(MAKE) bytecodetar
	$(MAKE) staticbintar
	$(MAKE) coccicheck


# I currently pre-generate the parser so the user does not have to
# install menhir on his machine. We could also do a few cleanups.
# You may have first to do a 'make licensify'.
#
# update: make docs generates pdf but also some ugly .log files, so
# make clean is there to remove them while not removing the pdf
# (only distclean remove the pdfs).
srctar:
	make distclean
	make docs
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP)/$(PACKAGE); cd parsing_cocci/; make parser_cocci_menhir.ml
	cd $(TMP); tar cvfz $(PACKAGE).tgz --exclude-vcs $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)


bintar: all
	rm -f $(TMP)/$(PACKAGE)
	ln -s `pwd` $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE)-bin-x86.tgz --exclude-vcs $(BINSRC2)
	rm -f $(TMP)/$(PACKAGE)

staticbintar: all.opt
	rm -f $(TMP)/$(PACKAGE)
	ln -s `pwd` $(TMP)/$(PACKAGE)
	make static
	cd $(TMP); tar cvfz $(PACKAGE)-bin-x86-static.tgz --exclude-vcs $(BINSRC2)
	rm -f $(TMP)/$(PACKAGE)

# add ocaml version in name ?
bytecodetar: all
	rm -f $(TMP)/$(PACKAGE)
	ln -s `pwd` $(TMP)/$(PACKAGE)
	make purebytecode
	cd $(TMP); tar cvfz $(PACKAGE)-bin-bytecode-$(OCAMLVERSION).tgz --exclude-vcs $(BINSRC2)
	rm -f $(TMP)/$(PACKAGE)

coccicheck:
	cp -a `pwd`/scripts/coccicheck $(TMP)/$(CCPACKAGE)
	tar cvfz $(TMP)/$(CCPACKAGE).tgz -C $(TMP) --exclude-vcs $(CCPACKAGE)
	rm -rf $(TMP)/$(CCPACKAGE)

clean::
	rm -f $(TMP)/$(PACKAGE)
	rm -f $(TMP)/$(PACKAGE)-bin-x86.tgz
	rm -f $(TMP)/$(PACKAGE)-bin-x86-static.tgz
	rm -f $(TMP)/$(PACKAGE)-bin-bytecode-$(OCAMLVERSION).tgz
	rm -f $(TMP)/$(CCPACKAGE).tgz

TOLICENSIFY=ctl engine parsing_cocci popl popl09 python scripts tools
licensify:
	ocaml tools/licensify.ml
	set -e; for i in $(TOLICENSIFY); do cd $$i; ocaml ../tools/licensify.ml; cd ..; done

# When checking out the source from diku sometimes I have some "X in the future"
# error messages.
fixdates:
	echo do 'touch **/*.*'

#fixCVS:
#	cvs update -d -P
#	echo do 'rm -rf **/CVS'

ocamlversion:
	@echo $(OCAMLVERSION)


##############################################################################
# Pad specific rules
##############################################################################

#TOP=/home/pad/mobile/project-coccinelle
WEBSITE=/home/pad/mobile/homepage/software/project-coccinelle

website:
	cp $(TMP)/$(PACKAGE).tgz                $(WEBSITE)
	cp $(TMP)/$(PACKAGE)-bin-x86.tgz        $(WEBSITE)
	cp $(TMP)/$(PACKAGE)-bin-x86-static.tgz $(WEBSITE)
	cp $(TMP)/$(PACKAGE)-bin-bytecode-$(OCAMLVERSION).tgz   $(WEBSITE)
	rm -f $(WEBSITE)/LATEST* $(WEBSITE)/coccinelle-latest.tgz
	cp changes.txt $(WEBSITE)/changes-$(VERSION).txt
	cd $(WEBSITE); touch LATEST_IS_$(VERSION); ln -s $(PACKAGE).tgz coccinelle-latest.tgz
	cp readme.txt $(WEBSITE)


#TXT=$(wildcard *.txt)
syncwiki:
#	unison ~/public_html/wiki/wiki-LFS/data/pages/ docs/wiki/
#	set -e; for i in $(TXT); do unison $$i docs/wiki/$$i; done

darcsweb:
#	@echo pull from ~/public_html/darcs/c-coccinelle and c-commons and lib-xxx

DARCSFORESTS=commons ocamlsexp \
 parsing_c parsing_cocci engine

update_darcs:
	darcs pull
	set -e; for i in $(DARCSFORESTS); do cd $$i; darcs pull; cd ..; done

#darcs diff -u
diff_darcs:
	set -e; for i in $(DARCSFORESTS); do cd $$i; darcs diff -u; cd ..; done

##############################################################################
# Git Developer rules
##############################################################################
gitupdate:
	git cvsimport -d :ext:topps:/var/cvs/cocci  coccinelle

##############################################################################
# Developer rules
##############################################################################

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
	rm -f python/coccilib/output.py
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
