#include "caml/mlvalues.h"

static int x = 0;

CAMLprim value c_counter(value nothing) {
  return Val_long(x++);
}
