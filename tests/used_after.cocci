@ rule1 @
identifier x;
//local function func;
identifier func;
@@

  x.proc_info = &func;



@@
// type T;
identifier rule1.func;
@@

- int func(int i) {
+ int func(int i, char j) {
  ...    
 }

