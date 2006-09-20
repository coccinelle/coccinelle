SP=check_region.cocci

SOURCES = case1.c case2.c case3.c case4.c

include ../../generic_makefile

CRARGS=


cr: $(SPATCH) check_region1.cocci check_region2.cocci check_region3.cocci \
check_region4.cocci check_region5.cocci check_region6.cocci \
check_region7.cocci check_region8.cocci
	find drivers -name "*.c" -exec sh run_one {} \;

cd: $(SPATCH) check_region1.cocci check_region2.cocci check_region3.cocci \
check_region4.cocci check_region5.cocci check_region6.cocci \
check_region7.cocci check_region8.cocci
	find drivers/cdrom -name "*.c" -exec sh run_one {} \;
