/* simple example */
static void f() {

  if(g(h(3))) {
    foo();
    x = 1;
  } else {
    bar();
    x = 2;
    goto error;
    //x = 3;  //  must generate exception DeadCode
  }
  foobar();
  foobar();
  while(1) {
    foo1();
    
  }
  if(1) {
    return 3;
  }

out:
  foo2();

error:
  foo3();
  goto last; // would generate exception DeadCode too (with first (buggy) version of deadcode detection)

last:
  foo4();
  foo5(TOTO, "toto\n");

  return; // was returning deadcode with first (buggy) version

}


void main(int o) {
  f();
}
