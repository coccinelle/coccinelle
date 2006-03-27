i read this in in standard C manuel:

Compatible and Composite Types                                                                                             
   In many contexts, the translator must test whether two types are compatible, which occurs when one of the following     
   conditions is met:                                                                                                      
     * Both types are the same.                                                                                            
...
     * Both are structure, union, or enumeration types that are declared in different translation units with the same      
       member names. Structure members are declared in the same order. Structure and union members whose names match are   
       declared with compatible types. Enumeration constants whose names match have the same values.                       


so i check with gcc different exemple, and gcc dont respect the rule, so why ?


ex1:

enum t { RED = 1, BLUE} i;

int f() {
  extern enum t { RED = 2} i; /* must raise an error but gcc dont */
}


ex2:

struct { int i;} i;
int f() {
   extern struct { int i;} i; /* must not raise an error but gcc do */	
}
