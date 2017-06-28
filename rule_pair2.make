# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

$(addsuffix .cmo,$(modules_with_mli)): %.cmo: %.ml %.cmi
	$(OCAMLC_CMD) -c $<

$(addsuffix .cmx,$(modules_with_mli)): %.cmx: %.ml %.cmi
	$(OCAMLOPT_CMD) -c $<
