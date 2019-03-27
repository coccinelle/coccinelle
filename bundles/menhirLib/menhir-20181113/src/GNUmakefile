# This is a development Makefile, which is not shipped.

.PHONY: clean

-include Makefile

# ----------------------------------------------------------------------------
# When developing, the standard library path is the current directory.
# ocamlfind is assumed.

installation.ml:
	echo "let libdir = \""`pwd`"/\"" > $@
	echo "let ocamlfind = true" >> $@

# ----------------------------------------------------------------------------
# Pretty-printing the MenhirLib.Convert API.

clean::
	rm -f *.mli.html

# ----------------------------------------------------------------------------
# Remove the files *.old and *.new that I use to log output during debugging.

clean::
	rm -f *.old *.new
