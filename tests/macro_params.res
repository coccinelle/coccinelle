#include <stdlib.h>

#define MACRO(value, a, b)      int a, b = value 
#define MACRO_2(value, a, b)    int a = value; int b = value;

int main(int argc, char **argv) {

  MACRO_2(argc, i, j);
  MACRO_2(argc, k, l);

  return 0;
}

