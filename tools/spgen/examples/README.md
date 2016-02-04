Examples
==============

Each example has four corresponding files:

 - name.c: C source file that returns matches/patches for the corresponding cocci file. Can be tested with

    	spatch --sp-file name.cocci name.c

 - name.cocci: simple, unhardened Coccinelle script.
 - name.config: spgen configuration file for specifying preface and rule information.
 - name_.cocci: expected output when running spgen on the unhardened Coccinelle script with the config file. Should be a valid, hardened Coccinelle script. Can be tested on the C file with e.g.

    	spatch --sp-file name_.cocci name.c -D report --no-show-diff

Examples:

 - addvoid: minimal patch example
 - context: context example (all others are patches)
 - disj: example of using spgen on a rule with pattern-matching disjunctions
 - inherited: example of metavariable inheritance and usage for printing modes
