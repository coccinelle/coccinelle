SP=rule21.cocci

# backend.c contain the E, generic.c contain some collateral effect, but no
# CE sites, and so both are not included in the sources list
# char/agp/intel-agp.c contain some collateral effects in additon to CE

SOURCES = ali-agp.c \
          amd-k7-agp.c \
          amd-k8-agp.c \
          hp-agp.c \
          i460-agp.c \
          i7x05-agp.c \
          intel-agp.c \
          sis-agp.c \
          sworks-agp.c \
          via-agp.c \
          via-kt400.c

include ../generic_makefile
