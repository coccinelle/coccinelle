#include <stdlib.h>

#define MACRO(value, a, b)      int a, b = value
#define MACRO_2(value, a, b)    int a = value; int b = value;

int main(int argc, char **argv) {

  MACRO(1, i, j);  
  /* comment */    
  MACRO(2, k, l);

  return 0;
}

