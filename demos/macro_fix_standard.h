// use this file with the -macro_file option of spatch.
// ex: ./spatch -macro_file demos/macro_fix_standard.h -parse_c demos/macro_parsing_problem.c 

#define MALLOC(A) malloc(a);

