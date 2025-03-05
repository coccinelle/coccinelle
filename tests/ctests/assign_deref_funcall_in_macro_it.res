#define for_one(ignored) for (int i = 0; i == 0; ++i)
int *id(int *ptr)
{
  return ptr;
}

int main(void) {
  int x = 1;
  int *ptr = &x;
  for_one(1)
    *id(ptr) = 0;
  return x;
}
