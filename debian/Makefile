include /etc/lsb-release

all:
	debuild -S

karmic lucid maverik:
	cp changelog.$@ changelog
	$(MAKE)

binary:
	cp changelog.$(DISTRIB_CODENAME) changelog
	debuild -uc -us
