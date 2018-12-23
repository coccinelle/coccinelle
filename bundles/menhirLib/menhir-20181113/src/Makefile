# ----------------------------------------------------------------------------
# Choose a target.

ifndef TARGET
  TARGET := native
endif

# Build .cmxs in case of native compilation.
ifneq ($(TARGET),byte)
MENHIRLIBCMXS := menhirLib.cmxs
endif

# ----------------------------------------------------------------------------
# Ocamlbuild tool and settings.

OCAMLBUILD := ocamlbuild -classic-display -j 0
ifeq ($(TARGET),byte)
  OCAMLBUILD := $(OCAMLBUILD) -byte-plugin
endif

# ----------------------------------------------------------------------------
# For everyday development.
# Typing "make" will perform just stage 1. This is enough to ensure that
# the source code is correct.

.PHONY: everyday
everyday: installation.ml stage1

# ----------------------------------------------------------------------------
# Building Menhir from scratch (a.k.a. bootstrapping).

.PHONY: bootstrap
bootstrap:
	@ $(MAKE) stage1
	@ $(MAKE) stage2
	@ $(MAKE) stage3

# ----------------------------------------------------------------------------
# Stage 1.
# Build Menhir using ocamlyacc.

.PHONY: stage1
stage1:
	@ $(OCAMLBUILD) -build-dir _stage1 \
	    $(MENHIRLIBCMXS) menhir.$(TARGET)

# ----------------------------------------------------------------------------
# Updating the auto-generated comments in parserMessages.messages.
# We use Menhir (from stage 1).

.PHONY: update
update: stage1
	cp parserMessages.messages parserMessages.messages.bak
	_stage1/menhir.$(TARGET) $(BASEFLAGS) --stdlib . \
	  --update-errors parserMessages.messages.bak \
	  fancy-parser.mly \
	  > parserMessages.messages
	rm parserMessages.messages.bak

# ----------------------------------------------------------------------------
# Stripping away the auto-generated comments in parserMessages.messages.

.PHONY: strip
strip:
	sed -e "/^##/d" -i.bak parserMessages.messages
	rm parserMessages.messages.bak

# ----------------------------------------------------------------------------
# Stage 2.
# Build Menhir using Menhir (from stage 1).

# Do not use . to refer to the current directory, because ocamlbuild
# descends into another directory when executing commands.
# Do not use $(shell pwd) either, because this assumes we are running
# on a Unix platform, and can fail on Windows.
# So, use .., which works fine if ocamlbuild has effectively descended
# into a subdirectory.
SRC   := ..

BASEFLAGS := \
  -lg 1 -la 1 -lc 1 \
  --canonical \
  --table \
  --strict \
  --fixed-exception \

FLAGS := \
  $(BASEFLAGS) \
  -v \
  --stdlib $(SRC) \

.PHONY: stage2
stage2:
	@ $(OCAMLBUILD) -build-dir _stage2 -tag fancy_parser \
	    -use-menhir -menhir "$(SRC)/_stage1/menhir.$(TARGET) $(FLAGS)" \
	    $(MENHIRLIBCMXS) menhir.$(TARGET)

# ----------------------------------------------------------------------------
# Stage 3 (optional).
# Re-generate Menhir's parser using Menhir (from stage 2) and check that it
# is identical to the stage 2 parser.

.PHONY: stage3
stage3:
	@ $(OCAMLBUILD) -build-dir _stage3 -tag fancy_parser \
	    -use-menhir -menhir "$(SRC)/_stage2/menhir.$(TARGET) $(FLAGS)" \
	    parser.ml parser.mli
	@ for i in parser.ml parser.mli ; do \
	    if ! diff _stage2/$$i _stage3/$$i 2>&1 >/dev/null ; then \
	      echo "Bootstrap FAILED: $$i did not reach a fixed point."; exit 1 ; \
	    fi ; \
	  done; \
	  echo "Bootstrap successful."

# ----------------------------------------------------------------------------

# The ocamlbuild targets that should be used to build menhirSdk.

MENHIRSDK       := menhirSdk.cmo
ifneq ($(TARGET),byte)
MENHIRSDK       := $(MENHIRSDK) menhirSdk.cmx menhirSdk.cmxs
endif

# ----------------------------------------------------------------------------
# Building menhirSdk.

.PHONY: sdk
sdk:
	@ $(OCAMLBUILD) \
	    -build-dir _sdk \
	    -tag sdk \
	    $(MENHIRSDK)

# ----------------------------------------------------------------------------
# Cleaning up.

.PHONY: clean
clean::
	rm -rf _stage1 _stage2 _stage3 _sdk
