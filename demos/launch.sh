#! /bin/sh

ARGS="-no_show_ctl_text -no_show_transinfo -no_parse_error_msg -no_show_misc -no_type_error_msg";

for i in *.c
do
 ../spatch $ARGS -iso_file ../standard.iso -cocci_file ${i}occi $i
done
