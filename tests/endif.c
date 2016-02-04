void f(int i) {

  x = 1;
  if(1) x = 3;
  
  x = 1;
  while(1) x = 3;

  x = 1;
  do x = 3; while(1);

  x = 1;
  for(1;1;1) x = 3;

  x = 1;
  for(1;1;1) { x = 3; }

  // switch(1) {
  // case 0: x = 3;
  // default: x = 3;
  // }

}
