int foo(int i) {

  if(1) {
    {
      x = 3;
      foo();
      z = 4;
      foo();
  }
  foo();
  } // we dont want that it add both foo on the } and on the endif
    // (note: but need correct endif accrochage)
  foo();

}
