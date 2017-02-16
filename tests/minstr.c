int main() {
  struct foo x = {};
  struct foo x = {0};
  struct foo x = {0, 1, 2};
  struct foo x = {.a = 0, .b = 1, .c = 2,};

  struct foo m = {.a = 5, .c = 2, .d = 2, .b = 3,};

  struct foo y = {};
  struct foo y = {0};
  struct foo y = {0, 1, 2};
  struct foo y = {.a = 3, .b = 1, .c = 2,};
  struct foo y = {.a = 3,};\
}
