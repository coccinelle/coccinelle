/* simple example */
static void f() {

  /*
  while(1) {
    while1();
  insidewhile: 
    insidewhile();
  }
  goto insidewhile;
  */


  switch(3) {
  case 1: switch1();
  case 2: switch2(); break;
  case 3: switch3(); break;
  case 4: switch4(); break;
    
  default: 
    switchdefault(); 
    //goto error;
  }

  while(1) {
    foowhile();
    if(1) { break; }
    foowhile2();
    
  }
  /*


  for(i=1; i<3; i++) {
    foofor();
    if(1) { 
      break; 
      //i++; // must generate exn DeadCode
    }
  }

  for(i=1; i<3; i++) {
    foofor();
    if (1) { continue; }
    foofor2();
  }


  */
  /*
  if(g(h(3))) {
    fooif();
    x = 1;
  } else {
    fooelse();
    x = 2;
    goto error;
    //x = 3;  //  must generate exception DeadCode
  }
  foobar();
  foobar();
  */

  /*
  while(1) {
    foowhile();
  }
  */


  if(1) {
    return 3;
  }
  
  /*
  do {
    foodowhile();
    goto out;
   
  } while(1);
  */

  /*
out:
  fooout();

error:
  fooerror();
  goto last; // would generate exception DeadCode too (with first (buggy) version of deadcode detection)

last:
  foolast();
  foolast(TOTO, "toto\n");

  return; // was returning deadcode with first (buggy) version
  */

}


void main(int o) {
  f();
}
