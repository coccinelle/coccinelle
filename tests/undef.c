#define foo 12
#undef foo

#define foo 12
int main () {
#undef foo
  return;
}

int main () {
#define foo 12
  return;
}
#undef foo



