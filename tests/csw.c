int main() {
  switch (x) {
  case XYZ: 
    link->state &= ~DEV_PRESENT;
    if (link->state & DEV_CONFIG) {
      bluecard_close(info);
      bluecard_release(link);
    }
    break;
  case MID: mid(); break;
  case FOO: bar(); break;
  }
}

