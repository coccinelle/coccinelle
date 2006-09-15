@@
identifier fn1, fn2, fn3;
expression A;
@@

  fn1(...) {
-   foo(A); 
//+   bidon(A, fn1);

  }

  fn2(...) {
-   bar(A);
//+   bidon(A, fn1, fn2);
  }

  fn3(...) {
//+   bidon(A, fn1, fn2);
    fn1(...);
    fn2(...);
    // bug need: ...  (partial match didn't help that much)
    ...
  }

//@@
//@@
//- bidon(...);

// @@
// @@
// (
// - fn1(A,1)
// |
// - fn2(A,1)
// )




// @@
// identifier fn1; 
// expression A;
// @@
//   fn1(...) {
//    foo(A);
//   }
// 
// @@
// identifier fn2;
// @@
// 
//   fn2(...) {
//    bar(A);
//   }
// 
// 
// @@
// identifier fn3;
// //identifier fn1, fn2;
// @@
// 
//   fn3(...) {
//     fn1(...);
//     fn2(...);
//     ...
//   }
// 
// @@
// //expression A;
// @@
//   fn1(...) {
// -   foo(A);
//   }
// 
// @@
// @@
// 
//   fn2(...) {
// -   bar(A);
//   }

