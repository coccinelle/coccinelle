# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

COCCIDIR=../..
-include $(COCCIDIR)/Makefile.config
-include $(COCCIDIR)/Makefile.local

all:
	@$(ECHO) -e "\n\n\tCompiling spgen ...\n\n"
	$(MAKE) -C source all
	@$(ECHO) -e "\n\n\tspgen can now be installed via 'make install'.\n\n"

all.opt:
	@$(ECHO) -e "\n\n\tCompiling spgen in native code...\n\n"
	$(MAKE) -C source all.opt
	@$(ECHO) -e "\n\n\tThe native code version of spgen can now be installed via 'make install'.\n\n"

# also compile documentation; this is not done by default
world: docs all

clean:
	$(MAKE) -C source clean

	$(MAKE) -C documentation clean

distclean: clean

install:
	$(MAKE) -C source install
	@$(ECHO) -e "\n\n\tspgen installed: DONE!!!\n\n"

uninstall:
	$(MAKE) -C source uninstall
	@$(ECHO) -e "\n\n\tspgen uninstalled: DONE!!!\n\n"

docs:
	$(MAKE) -C documentation docs

depend:
	$(MAKE) -C source depend
