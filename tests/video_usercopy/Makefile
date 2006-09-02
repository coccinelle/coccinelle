SPATCH=../../spatch
SP=aztech.cocci

SOURCES = aztech_ver10_micropcm20-radio.c \
          aztech_ver11_aimslab.c \
          aztech_ver12_aztech.c \
          aztech_ver13_cadet.c \
          aztech_ver14_gemtek.c \
          aztech_ver15_gemtek-pci.c \
          aztech_ver16_maestro.c \
          aztech_ver17_maxiradio.c \
          aztech_ver18_rtrack2.c \
          aztech_ver19_sf16fmi.c \
          aztech_ver20_terratec.c \
          aztech_ver21_trust.c \
          aztech_ver22_typhoon.c \
          aztech_ver23_zoltrix.c

OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@
	time $(SPATCH) -compare_with_expected -cocci_file $(SP) $<
	test -e $(SPATCH) && \
	(diff -b -q /tmp/output.c $(<:.c=.res) && touch $@) || echo failed
