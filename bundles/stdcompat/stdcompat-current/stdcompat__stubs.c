#include <caml/alloc.h>
#include <string.h>


value
caml_alloc_initialized_string(mlsize_t len, const char *p)
{
  value result = caml_alloc_string(len);
  memcpy((char *)String_val(result), p, len);
  return result;
}

