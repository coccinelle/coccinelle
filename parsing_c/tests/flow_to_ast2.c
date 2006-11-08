void main () {

  for(;;) {
    i++;
  }

  switch(3) {
  case 1: switch1(); //break; //CONFIG
  case 2: switch2(); break;
  case 3: switch3(); i++; break;
  case 4: switch4(); break;
    
  default: 
    switchdefault(); 
    //goto error;
    // return
    break;
  }
  //  return 4; // CONFIG

}
