
int classA(int i) {
  if(classA()) { x=0; }

  UnitTestEntry("A1");
  if(MethodA1()) { }
  if(MethodA2()) { }
  UnitTestEntry("A3");
  if(MethodA3()) { }
  if(MethodA4()) { }

}

int classB(int i) {
  if(classB()) { x=0; }

  if(MethodB1()) { }
  UnitTestEntry("B2");
  if(MethodB2()) { }
  if(MethodB3()) { }
  UnitTestEntry("B4");
  if(MethodB4()) { }

  
}

int lastfunction(int i) {
}
