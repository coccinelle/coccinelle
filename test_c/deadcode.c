int f(int i) {
  return i;
  i++;
  return i;
}

int main() {

  int i;
  i = 0;
  f(i);
  return i;
  i = 2;

}
