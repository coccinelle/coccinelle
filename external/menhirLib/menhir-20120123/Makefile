# This is the main Makefile that is shipped as part of the source package.
# Keep in mind that the hierarchy that is shipped is not identical to the
# hierarchy within the svn repository: some sub-directories are not shipped;
# the documentation is pre-built.

# The hierarchy that is shipped includes:
#   demos
#   menhir.1
#   manual.pdf
#   src
#   Makefile (this one)

# ----------------------------------------------------------------------------

# By default, we attempt to use ocamlfind (if present in the PATH), but it it
# is possible to prevent that externally by setting USE_OCAMLFIND to false.

ifndef USE_OCAMLFIND
  USE_OCAMLFIND = ocamlfind ocamlc -v >/dev/null 2>&1
endif

# ----------------------------------------------------------------------------

# A few settings differ on Windows versus Unix.

include Makefile.arch

# ----------------------------------------------------------------------------
# Installation paths.

# TEMPORARY GODIVA and Linux do not agree on the standard paths...

ifndef PREFIX
  $(error Please define PREFIX)
endif

bindir          := ${PREFIX}/bin
docdir		:= ${PREFIX}/share/doc/menhir
libdir	        := ${PREFIX}/share/menhir
mandir          := ${PREFIX}/share/man/man1
MANS            := menhir.1
DOCS            := manual.pdf demos
MLYLIB          := src/standard.mly

# -------------------------------------------------------------------------

# Building menhirLib.

ifeq ($(TARGET),byte)
MENHIRLIB       := menhirLib.cmi menhirLib.cmo
else
MENHIRLIB       := menhirLib.cmi menhirLib.cmo menhirLib.cmx menhirLib.$(OBJ)
endif

# ----------------------------------------------------------------------------
# Compilation.

# Installation time settings are recorded within src/installation.ml.
# This file is recreated every time so as to avoid becoming stale.

.PHONY: all install uninstall

all:
	rm -f src/installation.ml
	echo "let libdir = \"${libdir}\"" > src/installation.ml
	if $(USE_OCAMLFIND) ; then \
	  echo "let ocamlfind = true" >> src/installation.ml ; \
	else \
	  echo "let ocamlfind = false" >> src/installation.ml ; \
	fi
	$(MAKE) $(MFLAGS) -C src -f Makefile
	$(MAKE) $(MFLAGS) -C src -f Makefile $(MENHIRLIB)

# ----------------------------------------------------------------------------
# Installation.

install: all
	mkdir -p $(bindir)
	mkdir -p $(libdir)
	mkdir -p $(docdir)
	mkdir -p $(mandir)
	install src/$(MENHIREXE) $(bindir)
	install -m 644 $(MLYLIB) $(libdir)
	cp -r $(DOCS) $(docdir)
	cp -r $(MANS) $(mandir)
	@cd src && if $(USE_OCAMLFIND) ; then \
	  echo Installing MenhirLib via ocamlfind. ; \
	  ocamlfind install menhirLib META $(MENHIRLIB) ; \
	else \
	  echo Installing MenhirLib manually. ; \
	  install -m 644 $(MENHIRLIB) $(libdir) ; \
	fi

uninstall:
	rm -rf $(bindir)/$(MENHIREXE)
	rm -rf $(libdir)
	rm -rf $(docdir)
	rm -rf $(mandir)/$(MANS)
	@if $(USE_OCAMLFIND) ; then \
	  echo Un-installing MenhirLib via ocamlfind. ; \
	  ocamlfind remove menhirLib ; \
	fi
