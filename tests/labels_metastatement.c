int foo(int i) {

  if(1) {
    x = 3;
    z = 4;
  } // we don't want that it add both foo on the } and on the endif
    // (note: but need correct endif accrochage)

}
