#! /bin/sh

find linux -name "*\.c" -exec ../../spatch.opt -cocci_file intr4.cocci {} \
-no_show_ctl_text -no_show_transinfo -no_parse_error_msg -no_show_misc \
-sgrep -save_output_file \;

find linux -name "*\.cocci_res" -exec ./doit {} \; -print > err_found

# doit contains cat -n $1 | grep '/\*<\*/'
