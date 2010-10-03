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


#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

VERSION=$(shell cat globals/config.ml |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

##############################################################################
# Variables
##############################################################################
TARGET=spatch

SRC=flag_cocci.ml cocci.ml testing.ml test.ml main.ml


ifeq ($(FEATURE_PYTHON),1)
PYCMA=pycaml/pycaml.cma
PYDIR=pycaml
PYLIB=dllpycaml_stubs.so
# the following is essential for Coccinelle to compile under gentoo (wierd)
OPTLIBFLAGS=-cclib dllpycaml_stubs.so
else
PYCMA=
PYDIR=
PYLIB=
OPTLIBFLAGS=
endif


SYSLIBS=str.cma unix.cma
LIBS=commons/commons.cma globals/globals.cma\
     ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/parsing_c.cma \
     engine/cocciengine.cma popl09/popl.cma \
     extra/extra.cma $(PYCMA) python/coccipython.cma

MAKESUBDIRS=commons globals menhirlib $(PYDIR) ctl parsing_cocci parsing_c \
 engine popl09 extra python
INCLUDEDIRS=commons commons/ocamlextra globals menhirlib $(PYDIR) ctl \
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

OCAMLCFLAGS= #-g -dtypes # -w A

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=
# the following is essential for Coccinelle to compile under gentoo
# but is now defined above in this file
#OPTLIBFLAGS=-cclib dllpycaml_stubs.so

# the OPTBIN variable is here to allow to use ocamlc.opt instead of 
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var 
# or setenv it to ".opt" in your startup script.
OPTBIN= #.opt

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
eclipse: depend all
configure:
	./configure

all: rec $(EXEC)
opt: rec.opt $(EXEC).opt
all.opt: opt

rec:
	set -e; for i in $(MAKESUBDIRS); \
	do $(MAKE) -C $$i OCAMLCFLAGS="$(OCAMLCFLAGS)" all; done 
rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done 
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


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


.PHONY: tools all configure

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


##############################################################################
# Install
##############################################################################

# don't remove DESTDIR, it can be set by package build system like ebuild
install: all
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(LIBDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)
	cp spatch $(DESTDIR)$(BINDIR)	
	cp -f dllpycaml_stubs.so $(DESTDIR)$(LIBDIR)	
	cp standard.h $(DESTDIR)$(SHAREDIR)
	cp standard.iso $(DESTDIR)$(SHAREDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)/python
	cp -a python/coccilib $(DESTDIR)$(SHAREDIR)/python
	@echo ""
	@echo "You can also install spatch by copying the program spatch"
	@echo "(available in this directory) anywhere you want and"
	@echo "give it the right options to find its configuration files."

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/spatch
	rm -f $(DESTDIR)$(LIBDIR)/dllpycaml_stubs.so
	rm -f $(DESTDIR)$(SHAREDIR)/standard.h
	rm -f $(DESTDIR)$(SHAREDIR)/standard.iso
	rm -rf $(DESTDIR)$(SHAREDIR)/python/coccilib



version:
	@echo $(VERSION)


##############################################################################
# Package rules
##############################################################################

PACKAGE=coccinelle-$(VERSION)

BINSRC=spatch env.sh env.csh standard.h standard.iso \
       *.txt docs/* \
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

# Procedure to do each time:
#  cvs update
#  ./configure --without-python
#  make package
#  make website
# Check also that run an ocaml in /usr/bin

# To test you can try compile and run spatch from different instances 
# like my ~/coccinelle, ~/release/coccinelle, and the /tmp/coccinelle-0.X 
# downloaded from the website. 

# For 'make srctar' it must done from a clean
# repo such as ~/release/coccinelle. It must also be a repo where 
# the scripts/licensify has been run at least once. 
# For the 'make bintar' I can do it from my original repo.


package: 
	make srctar 
	make bintar 
	make staticbintar 
	make bytecodetar

# I currently pre-generate the parser so the user does not have to 
# install menhir on his machine. I also do a few cleanups like 'rm todo_pos'.
# You may have first to do a 'make licensify'.
srctar:
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP)/$(PACKAGE); cd parsing_cocci/; make parser_cocci_menhir.ml
	cd $(TMP)/$(PACKAGE); rm todo_pos
	cd $(TMP); tar cvfz $(PACKAGE).tgz  --exclude=CVS  $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)


bintar: all
	rm -f $(TMP)/$(PACKAGE)
	ln -s `pwd` $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE)-bin-x86.tgz --exclude=CVS $(BINSRC2)
	rm -f $(TMP)/$(PACKAGE)

staticbintar: all.opt
	rm -f $(TMP)/$(PACKAGE)
	ln -s `pwd` $(TMP)/$(PACKAGE)
	make static
	cd $(TMP); tar cvfz $(PACKAGE)-bin-x86-static.tgz --exclude=CVS $(BINSRC2)
	rm -f $(TMP)/$(PACKAGE)

# add ocaml version in name ?
bytecodetar: all
	rm -f $(TMP)/$(PACKAGE)
	ln -s `pwd` $(TMP)/$(PACKAGE)
	make purebytecode
	cd $(TMP); tar cvfz $(PACKAGE)-bin-bytecode-$(OCAMLVERSION).tgz --exclude=CVS $(BINSRC2)
	rm -f $(TMP)/$(PACKAGE)

clean::
	rm -f $(PACKAGE) 
	rm -f $(PACKAGE)-bin-x86.tgz 
	rm -f $(PACKAGE)-bin-x86-static.tgz 
	rm -f $(PACKAGE)-bin-bytecode-$(OCAMLVERSION).tgz



TOLICENSIFY=ctl engine parsing_cocci popl popl09 python
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


#TXT=$(wildcard *.txt)
syncwiki:
#	unison ~/public_html/wiki/wiki-LFS/data/pages/ docs/wiki/
#	set -e; for i in $(TXT); do unison $$i docs/wiki/$$i; done 

darcsweb:
#	@echo pull from ~/public_html/darcs/c-coccinelle and c-commons and lib-xxx


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
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps

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

clean::
	rm -f *~ .*~ *.exe #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

-include .depend
