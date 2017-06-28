# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

include $(abs_top_srcdir)/common_rules.make

x_modules_with_mli_source_dir::=$(abs_top_srcdir)/$(TARGET)/
x_modules_with_mli_sources::=$(wildcard $(x_modules_with_mli_source_dir)*.mli)

ifeq ($(x_modules_with_mli_sources),)
$(error Required interface descriptions were not found in this directory: $(x_modules_with_mli_source_dir))
else
modules_with_mli::=$(basename $(subst $(x_modules_with_mli_source_dir),,$(x_modules_with_mli_sources)))

ifeq ($(MAKE_RESTARTS),)
$(info $(words $(modules_with_mli)) modules with mli files)
endif

include $(abs_top_srcdir)/rule_pair2.make
endif
