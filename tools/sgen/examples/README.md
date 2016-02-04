Examples
==============

Each example has four corresponding files:

 - name.c: C source file that returns matches/patches for the corresponding cocci file. Can be tested with

    	spatch --sp-file name.cocci name.c

 - name.cocci: simple, unhardened Coccinelle script.
 - name.config: sgen configuration file for specifying preface and rule information.
 - name_.cocci: expected output when running sgen on the unhardened Coccinelle script with the config file. Should be a valid, hardened Coccinelle script. Can be tested on the C file with e.g.

    	spatch --sp-file name\_.cocci name.c -D report --no-show-diff

