// indentation algorithm is not perfect here, because it doesn't know how
// much to outdent.  But it gives the illusion of working.

int main() {
  if (x)
    if (x)
      if (x)
        if (x)
          if (x)
            if (x)
              blah();
  foo();
}
