# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

define rule_pair =
x_rule_pair_m::=$$(basename $(1))

ifeq ($$(use_separate_build_directory),)
x_rule_pair_i::=
else
x_rule_pair_i::=$$(abs_top_builddir)/$$(TARGET)/
endif

x_rule_pair_i+=$$(x_rule_pair_m).cmi

$$(x_rule_pair_m).cmo: $(1) $$(x_rule_pair_i)
	$$(OCAMLC_CMD) -c $$<

$$(x_rule_pair_m).cmx: $(1) $$(x_rule_pair_i)
	$$(OCAMLOPT_CMD) -c $$<
endef

$(foreach x,$(SRC_with_mli),$(eval $(call rule_pair,$(x))))
