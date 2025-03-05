int main () {
  if (x) {
    blah();
    {
      return 1;
    }
  }
  return 12;
}

int main () {
  if (a) {
    blah();
    {
      if (x) {
        blah();
        {
          return 1;
        }
      }
      b();
    }
  }
  return 12;
}

int main () {
  if (a) {
    blah();
    {
      if (b) {
        blah();
        {
          if (x) {
            return 1;
          }
          else {
            return 1;
          }
        }
      }
      b();
    }
  }
  return 12;
}
