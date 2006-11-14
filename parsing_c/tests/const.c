#include <stdlib.h> 

int main() { 

  const char *t1;
  char * const t2;

  t1 = malloc(10);
  t2 = malloc(10); // interdit
  *t1 = 'a'; // interdit
  *t2 = 'a';

  

}
