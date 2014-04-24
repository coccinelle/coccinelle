int main () {
  if (x) {
    blah();
    goto end;
  }
  else blah();
  later();
end:
  xxx();
end2:
  return 12;
}
