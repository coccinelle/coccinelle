@ rule1 @
identifier C, i;
@@
 int C(int i) {
  ...
  if(C()) { ... }
  ...
+ if(UnitTest()) {
+   int c;
+   C();
+  }
 }


@@
identifier TestMethod, i;
expression name; 
statement S1,S2;
identifier rule1.C;
// if put identifier name; then cocci does not help
// to say that there is a partial match :( 
@@

// int C(int i) {
//  ...
//  UnitTestEntry(name);
//  if(TestMethod()) { ... }
//  ...
//  if(UnitTest()) {
//   int c;
//   ...
//+  c = C();
//+  Console.WriteLine("invoking test", name);
//+  c.TestMethod();
//  }
// }


//  int C(int i) {
//   <...  UnitTestEntry(...);  ...>
//   UnitTestEntry(name);
//   if(TestMethod()) { ... }
//   <...  UnitTestEntry(...);  ...>
//   }

// or simply (does not work )

//  int C(int i) {
//   <...  
//   UnitTestEntry(name);
//   if(TestMethod()) { ... }
//   ...>
//   }

// or

 int C(int i) {
//  <...  UnitTestEntry(name); if(TestMethod()) { ... }  ...>
//  <...  UnitTestEntry(...); if(...) { ... }  ...>
    <... S1 ...>
        UnitTestEntry(name); if(TestMethod()) { ... }
    <... S2 ...>
//  <...  UnitTestEntry(...); if(...) { ... }  ...>
//  <...  UnitTestEntry(name); if(TestMethod()) { ... }  ...>
  }

@@
identifier rule1.C, i, c;
@@
 int C(int i) {
  ...
  if(UnitTest()) {
   struct foo c;
   ...
+  c = C();
+  Console.WriteLine("invoking test", name);
+  c.TestMethod();
  }
 }
