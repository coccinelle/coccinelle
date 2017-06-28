# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

.mli.cmi:
	$(OCAMLC_CMD) -c $<

.ml.cmo:
	$(OCAMLC_CMD) -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -c $<

.ml.mldepend:
	$(OCAMLC_CMD) -i $<
