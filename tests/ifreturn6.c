int main () {
  if (x) {
    blah();
    goto end;
  }
  else goto end2;
end:
  xxx();
end2:
  return 12;
}
