#include "includes-bug.h"
int foo()
{
  fprintf(stderr, "Size of type t: %lu\n", sizeof(t));
  return 0;
}
