SP=check_region.cocci

SOURCES = case1.c case2.c case3.c case4.c

include ../../generic_makefile

CRARGS=ARGS=-iso_file ../../standard.iso -save_output_file -cocci_file $(SP) \
-no_show_ctl_text -no_show_transinfo -no_parse_error_msg -no_show_misc


cr: $(SPATCH) $(SP)
	find drivers -name "*.c" -exec $(SPATCH) $(CRARGS) {} \;
