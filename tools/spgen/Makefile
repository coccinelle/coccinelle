# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

COCCIDIR=../..
-include $(COCCIDIR)/Makefile.config

all:
	@$(ECHO) -e "\n\n\tCompiling spgen ...\n\n"
	make -C source all
	@$(ECHO) -e "\n\n\tspgen can now be installed via 'make install'.\n\n"

# also compile documentation; this is not done by default
world: docs all

clean:
	make -C source clean
	make -C documentation clean

install:
	make -C source install
	@$(ECHO) -e "\n\n\tspgen installed: DONE!!!\n\n"

uninstall:
	make -C source uninstall
	@$(ECHO) -e "\n\n\tspgen uninstalled: DONE!!!\n\n"

docs:
	make -C documentation docs
