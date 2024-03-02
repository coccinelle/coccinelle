@one disable all@
identifier foo;
@@

foo(...) {
  <+...
  {
  ...
  return;
  }
  ...+>
}

@two depends on one@
@@

- yyy();
