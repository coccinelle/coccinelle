#include <stdio.h>
#include <stdlib.h>
#include "includes-bug.h"

typedef int t2;

int main()
{
  fprintf(stderr, "Size of type t: %lu\n", sizeof(t));
  return 0;
}
