/*
 * If still have an edge from the startif to endif (AfterNode), 
 * with a if-then-and-else, then rene will see this edge, 
 * and that means that the ctl engine will see this direct path from
 * startif to endif as a valid execution path. So on this program,
 * CTL will reject the formula f(X) ... g(X) because
 * when we take the direct path (which should not exist I repeat), 
 * we can't find a later g(1).
 */
void main() {

  f(1);
  
  if(1) {
    g(1);
  } else {
    g(1);
  }

  g(1);  // if  add this then the CTL even with the direct path will this time
  //  accept, but we cheat.
}
