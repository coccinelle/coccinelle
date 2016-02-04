int main () {
  for (i = 0; i < IVTV_VBI_FRAMES; i++) {
    a = itv[i];
    kfree(itv[i]);
  }
  print("foo",itv[i]);
  print("foo",itv[i]);
  a = itv[i];
  itv[i]=12;
  a = itv[i];
}

int bad () {
  kfree(itv[i]);
  print("foo",itv[i]);
  print("foo",itv[i]);
  a = NULL;
  itv[i]=12;
  a = itv[i];
}
